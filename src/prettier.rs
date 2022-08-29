//! An algebraic implementation of pretty printing.
//!
//! Basic idea: stick as many newlines and indents in as you like, but also throw in
//! information allowing optimistic grouping: if it'll fit within your margin,
//! you can tag stuff like
//! ```ignore
//! a(
//!    b,
//!    c,
//!    d
//! )
//! ```
//! to be flattened to
//! ```ignore
//! a(b, c, d)
//! ```
//! if there's room.
//!
//! The logic is:
//!
//! - for each grouping:
//!     - will this group, and the text after it until the next newline, fit in available margin?
//!         - yes: group it
//!         - no: don't group it
//!
//! This means that text outside a group can still overflow margins, and if the indent goes past the margin,
//! everything will be ungrouped. Works well enough usually, though.
//!
//! Actual usage:
//! ```
//! # use sans::prettier::Doc;
//! let doc = Doc::group(Doc::Empty + "function(" + Doc::indent(4,
//!     Doc::NewlineOrEmpty + "a," +
//!     Doc::NewlineOrSpace + "b," +
//!     Doc::NewlineOrSpace + "c"
//! ) + Doc::NewlineOrEmpty + ")");
//!
//! let prepared = doc.prepare();
//!
//! assert_eq!(&prepared.layout(80), "function(a, b, c)");
//! assert_eq!(&prepared.layout(9),
//! "function(
//!     a,
//!     b,
//!     c
//! )");
//! ```
//!
//! FIXME: there's no a priori reason you couldn't have a non-treelike lattice of
//! groups, but that would require a different interface.

use std::{
    borrow::Cow,
    collections::hash_map::Entry,
    fmt::Debug,
    hash::Hash,
    ops::{Add, Deref},
    rc::Rc,
    thread::current,
};

use ahash::HashMap;

/// A Doc represents a set of possible layouts for a given string of text.
#[derive(Clone, Debug)]
pub enum Doc<'a> {
    /// `{""}`
    Empty,
    /// `{"\n" | " "}`
    NewlineOrSpace,
    /// `{"\n" | ""}
    NewlineOrEmpty,
    /// `{concat(p_1, ..., p_n) for p_1 in d_1, ..., p_n in d_n}`
    Concat(Vec<Doc<'a>>),
    /// `{indent(i, p) for p in d}`
    Indent { indent: u32, doc: Box<Doc<'a>> },
    /// `{s}`
    Text(Cow<'a, str>),
    /// Either the contained document, or the contained document with all of its newlines converted to spaces
    Group(Box<Doc<'a>>),
}

impl<'a> Doc<'a> {
    /// Make a document flattenable.
    pub fn group(subdoc: Doc<'a>) -> Doc<'a> {
        if let Doc::Group(_) = &subdoc {
            subdoc
        } else {
            Doc::Group(Box::new(subdoc))
        }
    }

    /// Indent a document.
    /// This affects all newlines contained within the document,
    /// causing them to be followed by `indent` spaces.
    pub fn indent(indent: u32, mut doc: Doc<'a>) -> Doc<'a> {
        if let Doc::Indent {
            indent: old_indent,
            doc,
        } = doc
        {
            Doc::Indent {
                indent: old_indent + indent,
                doc,
            }
        } else {
            Doc::Indent {
                indent,
                doc: Box::new(doc),
            }
        }
    }

    /// Convert some sort of string into a document
    pub fn text(text: impl Into<Cow<'a, str>>) -> Doc<'a> {
        let text = text.into();
        assert!(!text.contains('\n'), "Doc must not contain bare newlines");
        Doc::Text(text)
    }

    /// A pair of brackets with an indent:
    /// ```ignore
    /// {start}\n
    /// {indent}{middle}\n
    /// {end}
    /// ```
    /// or
    /// ```ignore
    /// {start}{middle}{end}
    /// ```
    pub fn bracket(
        indent: u32,
        start: impl Into<Cow<'a, str>>,
        middle: Doc<'a>,
        end: impl Into<Cow<'a, str>>,
    ) -> Doc<'a> {
        Doc::group(
            Doc::text(start)
                + Doc::indent(indent, Doc::NewlineOrEmpty + middle)
                + Doc::NewlineOrEmpty
                + Doc::text(end),
        )
    }

    /// Prepare a doc for layout.
    pub fn prepare(self) -> PreppedDoc<'a> {
        let mut result = PreppedDoc {
            token_stream: Default::default(),
            groups: Default::default(),
        };

        let mut group_ends: HashMap<u32, u32> = HashMap::default();

        // This goes in REVERSE ORDER, and returns a modified distance to the next newline.
        fn prepare_rec<'a>(
            result: &mut PreppedDoc<'a>,
            doc: Doc<'a>,
            indent: u32,
            to_next_newline: u32,
        ) -> u32 {
            match doc {
                Doc::Empty => to_next_newline,
                Doc::NewlineOrSpace => {
                    result.token_stream.push(Token::Newline {
                        indent,
                        or_space: true,
                    });
                    0 // we are at a newline!
                }
                Doc::NewlineOrEmpty => {
                    result.token_stream.push(Token::Newline {
                        indent,
                        or_space: false,
                    });
                    0 // we are at a newline!
                }
                Doc::Text(text) => {
                    // FIXME this does not handle grapheme clusters
                    let len_chars = text.chars().count() as u32;
                    result.token_stream.push(Token::Text { text, len_chars });
                    to_next_newline + len_chars
                }
                Doc::Indent {
                    indent: new_indent,
                    doc,
                } => prepare_rec(result, *doc, indent + new_indent, to_next_newline),
                Doc::Concat(docs) => {
                    let mut to_next_newline = to_next_newline;
                    for doc in docs.into_iter().rev() {
                        to_next_newline = prepare_rec(result, doc, indent, to_next_newline)
                    }
                    to_next_newline
                }
                Doc::Group(doc) => {
                    let end_index = result.token_stream.len() as u32;
                    // distance from the end of the group to the next newline
                    // computing this is why we iterate backwards
                    let to_next_newline = prepare_rec(result, *doc, indent, to_next_newline);
                    let start_index = result.token_stream.len() as u32 - 1;

                    let len_chars: u32 = result.token_stream
                        [end_index as usize..start_index as usize]
                        .iter()
                        .map(|token| match token {
                            Token::Text { len_chars, .. } => *len_chars,
                            // keep in mind this is the length in chars *if grouped*
                            Token::Newline { or_space, .. } => {
                                if *or_space {
                                    1
                                } else {
                                    0
                                }
                            }
                        })
                        .sum();

                    result
                        .groups
                        .entry(start_index as u32)
                        .or_insert_with(Vec::new)
                        .push(Group {
                            end_index,
                            columns_needed: len_chars + to_next_newline,
                        });
                    to_next_newline
                }
            }
        }

        // treat end as a newline since this is actually the most permissive option
        prepare_rec(&mut result, self, 0, 0);
        result
    }

    fn is_empty(&self) -> bool {
        match *self {
            Doc::Empty => true,
            _ => false,
        }
    }
}

/* due to cleverness this is O(n) for n additions usually */
impl<'a> Add for Doc<'a> {
    type Output = Doc<'a>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (lhs, Doc::Empty) => lhs,
            (Doc::Empty, rhs) => rhs,
            (Doc::Concat(mut lefts), Doc::Concat(mut rights)) => {
                lefts.append(&mut rights);
                Doc::Concat(lefts)
            }
            // + associates left to right so this is the fast path
            (Doc::Concat(mut docs), rhs) => {
                docs.push(rhs);
                Doc::Concat(docs)
            }
            (lhs, Doc::Concat(mut docs)) => {
                docs.insert(0, lhs);

                Doc::Concat(docs)
            }
            (lhs, rhs) => Doc::Concat(vec![lhs, rhs]),
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

#[derive(Debug)]
/// A single token of text
enum Token<'a> {
    /// len_chars is separate because we don't want length bytes, we want at least estimate of length in columns
    Text { text: Cow<'a, str>, len_chars: u32 },
    /// Indent comes *after* the newline
    Newline { indent: u32, or_space: bool },
}

#[derive(Debug)]
struct Group {
    /// Index in the token stream
    /// Keep in mind this will come *after* the start because the token stream is reversed
    end_index: u32,
    /// Estimated space needed to flatten this group (including the contents of the group, and text after the group until the next newline)
    columns_needed: u32,
}

#[derive(Debug)]
/// A document prepared for rendering
pub struct PreppedDoc<'a> {
    /// A stream of tokens, stored in *reverse order*
    token_stream: Vec<Token<'a>>,
    /// A list of groups, mapped (starting token index) -> (ending token index)
    /// The starting token is the first token contained in the group, and similarly for ending
    /// Ending indices are sorted by distance away, farthest first
    /// FIXME use a vec and merge instead
    groups: HashMap<u32, Vec<Group>>,
}

impl<'a> PreppedDoc<'a> {
    pub fn layout(&self, available_columns: u32) -> String {
        // used signed ints to allow overflow
        let available_columns = available_columns as i32;
        let mut result = String::new();

        // *inclusive*; once we hit this index, we're out of the group
        let mut group_end_index: Option<usize> = None;

        let mut remaining = available_columns;

        // Token stream is *reversed*, so this iterates in *forward order* in the output
        for i in (0..self.token_stream.len()).rev() {
            let token = &self.token_stream[i];

            // whether or not our group state changes, if we start grouped, we should render
            // newlines as spaces
            let current_iter_grouped = group_end_index.is_some();

            // check if we should end the group
            match group_end_index {
                Some(j) if j == i => group_end_index = None,
                _ => (),
            }

            if let None = group_end_index {
                // check if we should start a new grouping
                let groups_starting_here: &[Group] =
                    self.groups.get(&(i as u32)).map(|v| &v[..]).unwrap_or(&[]);

                if groups_starting_here.len() > 0 {
                    let check = tracing::debug_span!("group_check", remaining = remaining,);

                    // reversed order because that's naturally the order
                    // where larger groups come before smaller
                    for group in groups_starting_here.into_iter().rev() {
                        if (group.columns_needed as i32) <= remaining {
                            tracing::debug!(
                                group.columns_needed = group.columns_needed,
                                group.end_index = group.end_index,
                                "group_select",
                            );

                            group_end_index = Some(group.end_index as usize);
                            break;
                        }
                    }
                }
            }

            match (token, current_iter_grouped) {
                (Token::Text { text, len_chars }, _) => {
                    result.push_str(&text);
                    remaining -= *len_chars as i32;

                    tracing::debug!(remaining = remaining, text = &**text, "append");
                }
                (Token::Newline { or_space, .. }, true) => {
                    if *or_space {
                        result.push(' ');
                        remaining -= 1;

                        tracing::debug!(remaining = remaining, text = " ", "append");
                    }
                }
                (Token::Newline { indent, .. }, false) => {
                    result.push('\n');
                    for _ in 0..*indent {
                        result.push(' ')
                    }
                    remaining = available_columns - *indent as i32;

                    tracing::debug!(remaining = remaining, indent = *indent, "newline");
                }
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::Doc;

    #[test]
    fn paper_example_1() {
        tracing_subscriber::fmt::try_init();
        let doc = Doc::group(
            Doc::group(
                Doc::group(
                    Doc::group(Doc::Empty + "hello" + Doc::NewlineOrSpace + "a")
                        + Doc::NewlineOrSpace
                        + "b",
                ) + Doc::NewlineOrSpace
                    + "c",
            ) + Doc::NewlineOrSpace
                + "d",
        );

        println!("{:#?}", doc);
        let prepared = doc.prepare();
        println!("{:#?}", prepared);

        // note: this
        assert_eq!(prepared.layout(13), "hello a b c d");
        assert_eq!(prepared.layout(12), "hello a b c\nd");
        assert_eq!(prepared.layout(10), "hello a b\nc\nd");
        assert_eq!(prepared.layout(8), "hello a\nb\nc\nd");
        assert_eq!(prepared.layout(6), "hello\na\nb\nc\nd");
    }

    #[test]
    fn paper_example_2() {
        use itertools::Itertools;
        use Doc::*;

        fn att(att: (&'static str, &'static str)) -> Doc<'static> {
            Empty + att.0 + "=\"" + att.1 + "\""
        }

        fn elt(
            name: &'static str,
            atts: &'static [(&'static str, &'static str)],
            body: Doc<'static>,
        ) -> Doc<'static> {
            let atts = if atts.len() > 0 {
                Doc::indent(
                    2,
                    NewlineOrSpace
                        + atts
                            .iter()
                            .cloned()
                            .map(att)
                            .intersperse(NewlineOrSpace)
                            .fold(Empty, |a, b| a + b),
                )
            } else {
                Empty
            };
            if let Empty = body {
                NewlineOrEmpty
                    + Doc::group(Empty + "<" + name + atts + NewlineOrEmpty + "/>")
                    + NewlineOrEmpty
            } else {
                NewlineOrEmpty
                    + Doc::group(
                        Doc::group(Empty + "<" + name + atts + NewlineOrEmpty + ">")
                            + Doc::indent(2, NewlineOrEmpty + body)
                            + NewlineOrEmpty
                            + "</"
                            + name
                            + ">",
                    )
                    + NewlineOrEmpty
            }
        }

        let doc = elt(
            "p",
            &[("color", "red"), ("font", "Times"), ("size", "10")],
            Doc::group(
                Empty + "Here is some " + elt("em", &[], Doc::text("emphasized")) + " text.",
            ) + NewlineOrSpace
                + Doc::group(
                    Empty
                        + "Here is a "
                        + elt("a", &[("href", "http://www.eg.com")], Doc::text("link"))
                        + " elsewhere.",
                ),
        );

        let prepared = doc.prepare();

        let correct_80 = "
<p color=\"red\" font=\"Times\" size=\"10\">
  Here is some <em>emphasized</em> text.
  Here is a <a href=\"http://www.eg.com\">link</a> elsewhere.
</p>
";
        let correct_35 = "
<p
  color=\"red\"
  font=\"Times\"
  size=\"10\"
>
  Here is some 
  <em>emphasized</em>
   text.
  Here is a 
  <a href=\"http://www.eg.com\">
    link
  </a>
   elsewhere.
</p>
";
        println!("{}", prepared.layout(80));
        println!("{}", prepared.layout(35));

        assert_eq!(prepared.layout(80), correct_80);
        assert_eq!(prepared.layout(35), correct_35);
    }
}
