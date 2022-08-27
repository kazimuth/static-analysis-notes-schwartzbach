//! An algebraic implementation of pretty printing.
//! Unlike Wadler's original paper, we implement this using an n-ary algebra,
//! adopting the principle that a concatenation can be flattened into a containing concatenation
//! if that is the only place it is used.

use std::{borrow::Cow, hash::Hash, ops::Deref, rc::Rc};

use ahash::HashMap;
thread_local! {
    static EMPTY: Doc<'static> = Doc(Rc::new(DocBody::Empty));
    static NEWLINE: Doc<'static> = Doc(Rc::new(DocBody::Newline));
    static SPACE: Doc<'static> = Doc(Rc::new(DocBody::Text(" ".into())));
}

/// A Doc represents a set of possible layouts for a given string of text.
#[derive(Clone)]
pub struct Doc<'a>(Rc<DocBody<'a>>);

enum DocBody<'a> {
    /// {}
    Empty,
    /// {concat(p_1, ..., p_n) for p_1 in d_1, ..., p_n in d_n}
    Concat(Vec<Doc<'a>>),
    /// {indent(i, p) for p in d}
    Indent { indent: u32, contents: Doc<'a> },
    /// {s}
    Text(Cow<'a, str>),
    /// {"\n"}
    Newline,
    /// Either the document, or the document with all newlines flattened
    Group(Doc<'a>),
}

impl<'a> Doc<'a> {
    /// `""`
    pub fn empty() -> Doc<'static> {
        EMPTY.with(|empty| empty.clone())
    }

    /// `"\n"` or `" "` depending on available space
    pub fn newline() -> Doc<'static> {
        NEWLINE.with(|newline| newline.clone())
    }

    /// `" "`
    pub fn space() -> Doc<'static> {
        SPACE.with(|space| space.clone())
    }

    /// Concatenates docs without spaces
    pub fn concat(docs: impl IntoIterator<Item = Doc<'a>>) -> Doc<'a> {
        let mut output = vec![];
        // work through the rest
        for doc in docs {
            if doc.unwrappable() && doc.is_concat() {
                output.append(&mut doc.unwrap_concat());
                continue;
            }
            if let DocBody::Empty = *doc.0 {
                continue;
            }
            output.push(doc);
        }
        if (output.len() == 0) {
            Doc::empty()
        } else {
            Doc::new(DocBody::Concat(output))
        }
    }

    /// Raw text inclusion
    pub fn text(text: impl Into<Cow<'a, str>>) -> Doc<'a> {
        Doc(Rc::new(DocBody::Text(text.into())))
    }

    /// Indent the text in here following newlines
    /// If the front of the doc does not start with a newline, the first line will not be indented
    pub fn indent(doc: Doc<'a>, indent: u32) -> Doc<'a> {
        match &*doc.0 {
            DocBody::Indent {
                indent: old_indent,
                contents,
            } => Doc::new(DocBody::Indent {
                indent: indent + old_indent,
                contents: contents.clone(),
            }),
            _ => Doc::new(DocBody::Indent {
                indent,
                contents: doc,
            }),
        }
    }

    /// Try to pack this hunk of the document onto a single line
    pub fn group(doc: Doc<'a>) -> Doc<'a> {
        if let DocBody::Group(_) = &*doc.0 {
            doc
        } else {
            Doc(Rc::new(DocBody::Group(doc)))
        }
    }

    /*
    rep :: [(Int, DOC)] -> DOC
    rep z = fold (<>) nil [ nest i x | (i,x) <- z ]

    // -- spec --
    be w k z = best w k (rep z)

    // -- impl --
    // start
    best w k x = be w k [(0,x)]
    // skip nil
    be w k [] = Nil
    be w k ((i,NIL):z) = be w k z
    // unpack concats
    be w k ((i,x :<> y):z) = be w k ((i,x):(i,y):z)
    // indenting subtracts from available space
    be w k ((i,NEST j x):z) = be w k ((i+j,x):z)
    // text and newlines operate on the output stream,
    // UNLESS these terms are left partly symbolic while
    // evaluating lengths
    be w k ((i,TEXT s):z) = s ‘Text‘ be w (k + length s) z
    be w k ((i,LINE):z) = i ‘Line‘ be w i z

    // the next two definitions are the core of the algorithm
    be w k ((i,x :<|> y):z) = better w k (be w k ((i,x):z))
                                            (be w k ((i,y):z))
    better w k x y = if fits (w-k) x then x else y
    // recall: group x = flatten x :<|> x
    // so basically:
    // - if the group can fit on the line, use it
    // - otherwise, ungroup and continue

    fits w x | w < 0 = False
    fits w Nil = True
    fits w (s ‘Text‘ x) = fits (w - length s) x
    fits w (i ‘Line‘ x) = True
    */

    pub fn layout(self, target_width: u32) -> String {
        // dw, this isn't recursive
        self.validate();

        // we continually pull from the front of the document.
        // when we pluck a group, we check if it fits on the current line.
        let mut grouped_sizes: HashMap<Doc, u32> = HashMap::default();

        // O(nodes) ~ n log n, since DP
        fn compute_grouped_sizes<'a>(memo: &mut HashMap<Doc<'a>, u32>, doc: &Doc<'a>) -> u32 {
            if let Some(size) = memo.get(doc) {
                *size
            } else {
                let result = match &*doc.0 {
                    DocBody::Empty => 0,
                    DocBody::Concat(args) => args
                        .iter()
                        .map(|doc| compute_grouped_sizes(memo, doc))
                        .sum(),
                    DocBody::Indent { indent, contents } => compute_grouped_sizes(memo, doc),
                    DocBody::Text(text) => text.len() as u32,
                    DocBody::Newline => 1 as u32, // newlines converted to " "
                    DocBody::Group(doc) => compute_grouped_sizes(memo, doc),
                };
                memo.insert(doc.clone(), result);
                result
            }
        }

        compute_grouped_sizes(&mut grouped_sizes, &self);

        /*
            ...hmmm...
            think: explode groups until you find one that fits.
            so you want a stack of cursors like

                         input
            __________________________________
            | [oo|     1         |---------] | level 0
            |    [o|  2      |---]           | level 1
            |      [ooo| 3 |-]               | level 2
            |          [4--]                 | level 3
            |___________X____________________|
                         leaves

            ~ `(current: Doc, levels: [ (current_indent: u32, next: &[Doc]) ])`

            (all levels separated by chains of unary doc nodes)
            (we only enter a node if we know it'll be possible to fit)

            invariant:  levels[max][0] is next node

            ...ehh, this is still probably easier to implement functionally...
        */

        let mut output = String::new();

        struct Level<'a> {
            currently_grouped: bool,
            current_indent: u32,
            remaining: &'a [Doc<'a>],
        }

        let mut levels: Vec<Level<'a>> = vec![];
        let mut current: &Doc = &self;
        let mut current_indent: i32 = 0;
        let mut remaining: i32 = target_width as i32;

        loop {
            /* invariant: levels[max][0] is next node after current */
            let max = levels.len() - 1;

            match &*current.0 {
                // special control flow
                DocBody::Concat(vec) => {
                    panic!();
                    continue;
                }
                DocBody::Group(doc) => {
                    let grouped_size = grouped_sizes[doc];
                    if remaining - grouped_size as i32 > 0 {
                        remaining.
                    }
                    panic!();
                    continue;
                }
                // remaining cases all have same ending control flow
                DocBody::Indent { indent, contents } => {
                    current_indent += *indent;
                    current = contents;
                }
                DocBody::Text(text) => {
                    output.push_str(&text);
                }
                DocBody::Newline => {
                    output.push('\n');
                    for i in 0..current_indent {
                        output.push(' ');
                    }
                    // i smell off-by-one-errors
                    remaining = target_width - current_indent;
                }
                DocBody::Empty => todo!(),
            }
        }

        panic!()
    }

    // ideally these would accept a pattern but alas, such things are not possible in rust
    fn unwrappable(&self) -> bool {
        Rc::strong_count(&self.0) == 1
    }
    fn is_concat(&self) -> bool {
        match *self.0 {
            DocBody::Concat(_) => true,
            _ => false,
        }
    }
    fn unwrap_concat(self) -> Vec<Doc<'a>> {
        match Rc::try_unwrap(self.0) {
            Ok(DocBody::Concat(vec)) => vec,
            _ => panic!("unwrap failed"),
        }
    }
    fn new(body: DocBody<'a>) -> Doc<'a> {
        Doc(Rc::new(body))
    }
    fn ptr_usize(&self) -> usize {
        Rc::as_ptr(&self.0) as usize
    }

    fn validate(&self) {
        match &*self.0 {
            DocBody::Concat(vec) => assert!(vec.len() > 0),
            _ => (),
        }

        // TODO loop check ig
    }
}

/// hash and compare by pointer
impl<'a> PartialEq for Doc<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr_usize() == other.ptr_usize()
    }
}
impl<'a> Eq for Doc<'a> {}
impl<'a> Hash for Doc<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ptr_usize().hash(state);
    }
}
