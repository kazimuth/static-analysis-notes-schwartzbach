//! An algebraic implementation of pretty printing.
//! Basic idea: stick as many newlines and indents in as you like, but also throw in
//! information allowing optimistic grouping: if it'll fit within your margin,
//! you can tag stuff like
//! ```
//! a(
//!    b,
//!    c,
//!    d
//! )
//! ```
//! to be flattened to
//! ```
//! a(b, c, d)
//! ```
//! if there's room.
//!
//! (keep in mind the representation of indents and newlines is sparse,
//! so none of that stuff is written to a buffer until )
//!
//! (TODO: add no-space newlines).
//! TODO: reorganize data structures: flat token stream + separate tables for grouping
//! lattice and newline info
//! for each group, store flattened length + possible lengths to newlines
//! (length to first newline after?)

use std::{
    borrow::Cow,
    fmt::Debug,
    hash::Hash,
    ops::{Add, Deref},
    rc::Rc,
    thread::current,
};

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
// custom debugs to minimize noise
impl<'a> Debug for Doc<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
impl<'a> Debug for DocBody<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DocBody::Empty => f.debug_tuple("").finish(),
            DocBody::Concat(entries) => f.debug_list().entries(entries.iter()).finish(),
            DocBody::Indent { indent, contents } => todo!(),
            DocBody::Text(str) => str.fmt(f),
            DocBody::Newline => f.debug_struct("Newline").finish(),
            DocBody::Group(body) => f.debug_tuple("Group").field(body).finish(),
        }
    }
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
                // FIXME we oughtta be recursively flattening here
                // (n.b. if anybody's ever hassling you about monads,
                //  remember that "monad" just means "recursively
                //  flattenable data structure")
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
        let text = text.into();
        assert!(!text.contains('\n'));
        Doc(Rc::new(DocBody::Text(text)))
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

    pub fn layout(&self, target_width: u32) -> String {
        // dw, this isn't recursive
        self.validate();
        // use i32s everywhere to handle running over remaining space
        let target_width = target_width as i32;

        // we continually pull from the front of the document.
        // when we pluck a group, we check if it fits on the current line.
        let mut metadata: HashMap<Doc, Metadata> = HashMap::default();

        // O(nodes)

        compute_metadata(&mut metadata, &self);

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

            ~ `(current: Doc, levels: [ (current_indent: i32, next: &[Doc]) ])`

            (all levels separated by chains of unary doc nodes)
            (we only enter a node if we know it'll be possible to fit)

            invariant:  levels[max][0] is next node

            ...ehh, this is still probably easier to implement functionally...
        */

        // despite passing around a mutable pointer to th
        struct Context<'a> {
            output: &'a mut String,
            target_width: i32,
            metadata: &'a HashMap<Doc<'a>, Metadata>,
        }

        fn process(
            context: &mut Context,
            doc: &Doc,
            next_doc: Option<&Doc>,
            current_indent: i32,
            remaining_space: i32,
            in_group: bool,
        ) -> i32 {
            assert!(current_indent >= 0);
            match &*doc.0 {
                DocBody::Empty => (),
                DocBody::Concat(subdocs) => {
                    let mut remaining_space = remaining_space;

                    // this is STILL wrong. too local!
                    let peekable = subdocs.iter().cloned().peekable();

                    for subdoc in peekable {
                        remaining_space = process(
                            context,
                            &subdoc,
                            peekable.peek().or(next_doc),
                            current_indent,
                            remaining_space,
                            in_group,
                        );
                    }
                    remaining_space
                }
                DocBody::Indent { indent, contents } => process(
                    context,
                    contents,
                    next_doc,
                    current_indent + *indent as i32,
                    remaining_space,
                    in_group,
                ),
                DocBody::Text(text) => {
                    context.output.push_str(&text);
                    remaining_space - text.len() as i32
                }
                DocBody::Newline => {
                    if in_group {
                        context.output.push(' ');
                        remaining_space - 1
                    } else {
                        context.output.push('\n');
                        for _ in 0..current_indent {
                            context.output.push(' ')
                        }
                        context.target_width - current_indent
                    }
                }
                DocBody::Group(doc) => {
                    let next_doc_to_first_newline = match next_doc {
                        Some(next_doc) => match context.metadata[next_doc].to_first_newline {
                            Some(to_first_newline) => to_first_newline,
                            _ => 0,
                        },
                        _ => 0,
                    };

                    let space_if_grouped =
                        context.metadata[doc].grouped_size + next_doc_to_first_newline;

                    let nested_in_group = in_group || remaining_space - space_if_grouped >= 0;

                    // FIXME we ought to look deeper here to see if there's any room past to fit the stuff at the end of this group (peek up to a newline)
                    /* if !in_group && nested_in_group {
                        println!(
                            "grouping: {:?} ({} < {})",
                            subdoc, context.grouped_sizes[subdoc], remaining_space
                        );
                    } */
                    process(
                        context,
                        doc,
                        next_doc,
                        current_indent,
                        remaining_space,
                        nested_in_group,
                    )
                }
            }
        }

        let mut output = String::new();

        process(
            &mut Context {
                grouped_sizes: &grouped_sizes,
                output: &mut output,
                target_width,
            },
            &self,
            0,
            target_width,
            false,
        );

        output
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
    fn is_empty(&self) -> bool {
        match *self.0 {
            DocBody::Empty => true,
            _ => false,
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

#[derive(Clone, Copy)]
/// Metadata about a document node.
struct Metadata {
    /// Size with all newlines flattened to spaces
    grouped_size: i32,
    /// None if does not contain a newline
    to_first_newline: Option<i32>,
}
/// Compute metadata about a document.
fn compute_metadata<'a>(memo: &mut HashMap<Doc<'a>, Metadata>, doc: &Doc<'a>) -> Metadata {
    if let Some(metadata) = memo.get(doc) {
        return *metadata;
    }

    let metadata = match &*doc.0 {
        DocBody::Empty => Metadata {
            grouped_size: 0,
            to_first_newline: None,
        },
        DocBody::Concat(args) => {
            let mut grouped_size = 0;
            let mut to_first_newline = None;
            let mut so_far = 0;

            for doc in args {
                let metadata = compute_metadata(memo, doc);
                grouped_size += metadata.grouped_size;

                match (to_first_newline, metadata.to_first_newline) {
                    // note: if no newlines contained, grouped_size = size
                    (None, None) => so_far += metadata.grouped_size,
                    (None, Some(dist)) => to_first_newline = Some(so_far + dist),
                    _ => (),
                }
            }

            Metadata {
                grouped_size,
                to_first_newline,
            }
        }
        DocBody::Indent { indent, contents } => compute_metadata(memo, contents),
        DocBody::Text(text) => Metadata {
            grouped_size: text.len() as i32,
            to_first_newline: None,
        },
        DocBody::Newline => Metadata {
            grouped_size: 1, // newlines converted to " "
            to_first_newline: Some(0),
        },
        DocBody::Group(doc) => compute_metadata(memo, doc),
    };

    memo.insert(doc.clone(), metadata);
    //println!("{:?}: {}", doc, result);
    metadata
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
/* due to cleverness this is O(n) for n additions usually */
impl<'a> Add for Doc<'a> {
    type Output = Doc<'a>;

    fn add(self, rhs: Self) -> Self::Output {
        if rhs.is_empty() {
            return self;
        }
        if self.is_empty() {
            return rhs;
        } else if self.is_concat() && self.unwrappable() {
            let mut output = self.unwrap_concat();
            output.push(rhs);
            Doc(Rc::new(DocBody::Concat(output)))
        } else {
            Doc(Rc::new(DocBody::Concat(vec![self, rhs])))
        }
    }
}
impl<'a> Add<&'a str> for Doc<'a> {
    type Output = Doc<'a>;

    fn add(self, rhs: &'a str) -> Self::Output {
        self + Doc::text(rhs)
    }
}
impl<'a> Add<String> for Doc<'a> {
    type Output = Doc<'a>;

    fn add(self, rhs: String) -> Self::Output {
        self + Doc::text(rhs)
    }
}

#[cfg(test)]
mod tests {
    use super::Doc;

    #[test]
    fn paper_example_1() {
        let doc = Doc::group(
            Doc::group(
                Doc::group(
                    Doc::group(Doc::empty() + "hello" + Doc::newline() + "a")
                        + Doc::newline()
                        + "b",
                ) + Doc::newline()
                    + "c",
            ) + Doc::newline()
                + "d",
        );
        //println!("{:#?}", doc);
        assert_eq!(doc.layout(13), "hello a b c d");
        assert_eq!(doc.layout(12), "hello a b c\nd");
        assert_eq!(doc.layout(10), "hello a b\nc\nd");
        assert_eq!(doc.layout(8), "hello a\nb\nc\nd");
        assert_eq!(doc.layout(6), "hello\na\nb\nc\nd");
        /*
        group (
        group (
        group (
        group (text "hello" <> line <> text "a")
        <> line <> text "b")
        <> line <> text "c")
        <> line <> text "d")

        This has the following possible layouts:
        hello a b c hello a b hello a hello

        c b a
        c b
        c */
    }
}
