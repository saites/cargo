//! A small module giving you a simple container that allows
//! easy and cheap replacement of parts of its content,
//! with the ability to commit or rollback pending changes.
//!
//! Create a new [`Data`] struct with some initial set of code,
//! then record changes with [`Data::replace_range`],
//! which will validate that the changes do not conflict with one another.
//! At any time, you can "checkpoint" the current changes with [`Data::commit`]
//! or roll them back (perhaps due to a conflict) with [`Data::restore`].
//! When you're done, use [`Data::to_vec`] or [`Data::to_string`]
//! to merge the original data with the changes.
//!
//! # Notes
//!
//! The [`Data::to_vec`] and [`Data::to_string`] methods include uncommitted changes, if present.
//! The reason for including uncommitted changes is that typically, once you're calling those,
//! you're done with edits and will be dropping the [`Data`] struct in a moment.
//! In this case, requiring an extra call to `commit` would be unnecessary work.
//! Of course, there's no harm in calling `commit`---it's just not strictly necessary.
//!
//! Put another way, the main point of `commit` is to checkpoint a set of known-good changes
//! before applying additional sets of as-of-yet unvalidated changes.
//! If no future changes are expected, you aren't _required_ to pay the cost of `commit`.
//! If you want to discard uncommitted changes, simply call [`Data::restore`] first.

use std::ops::Range;

use crate::error::Error;

/// Data that should replace a particular range of the original.
#[derive(Clone)]
struct Span<'a> {
    /// Span of the parent data to be replaced, inclusive of the start, exclusive of the end.
    range: Range<usize>,
    /// New data to insert at the `start` position of the `original` data.
    data: &'a [u8],
    /// Whether this data is committed or provisional.
    committed: bool,
}

impl<'a> std::fmt::Debug for Span<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let state = if self.is_insert() {
            "inserted"
        } else {
            "replaced"
        };

        let committed = if self.committed {
            "committed"
        } else {
            "uncommitted"
        };

        write!(
            f,
            "({}, {}: {state}, {committed})",
            self.range.start, self.range.end
        )
    }
}

impl<'a> Span<'a> {
    fn new(range: Range<usize>, data: &'a [u8]) -> Self {
        Self {
            range,
            data,
            committed: false,
        }
    }

    /// Returns `true` if and only if this is a "pure" insertion,
    /// i.e. does not remove any existing data.
    ///
    /// The insertion point is the `start` position of the range.
    fn is_insert(&self) -> bool {
        self.range.start == self.range.end
    }
}

impl<'a> PartialEq for Span<'a> {
    /// Returns `true` if and only if this `Span` and `other` have the same range and data,
    /// regardless of `committed` status.
    fn eq(&self, other: &Self) -> bool {
        self.range == other.range && self.data == other.data
    }
}

/// A container that allows easily replacing chunks of its data.
#[derive(Debug, Clone, Default)]
pub struct Data<'a> {
    /// Original data.
    original: &'a [u8],
    /// [`Span`]s covering the full range of the original data.
    /// Important: it's expected that the underlying implementation maintains this in order,
    /// sorted ascending by start position.
    parts: Vec<Span<'a>>,
}

impl<'a> Data<'a> {
    /// Create a new data container from a slice of bytes
    pub fn new<T>(data: &'a T) -> Self
    where
        T: AsRef<[u8]> + ?Sized,
    {
        Data {
            original: data.as_ref(),
            parts: vec![],
        }
    }

    /// Commit the current changes.
    pub fn commit(&mut self) {
        self.parts.iter_mut().for_each(|span| span.committed = true);
    }

    /// Discard uncommitted changes.
    pub fn restore(&mut self) {
        self.parts.retain(|parts| parts.committed);
    }

    /// Merge the original data with changes, **including** uncommitted changes.
    ///
    /// See the module-level documentation for more information on why uncommitted changes are included.
    pub fn to_vec(&self) -> Vec<u8> {
        let mut prev_end = 0;
        let mut s = self.parts.iter().fold(Vec::new(), |mut acc, span| {
            // Hedge against potential implementation errors.
            debug_assert!(
                prev_end <= span.range.start,
                "expected parts in sorted order"
            );

            acc.extend_from_slice(&self.original[prev_end..span.range.start]);
            acc.extend_from_slice(span.data);
            prev_end = span.range.end;
            acc
        });

        // Append remaining data, if any.
        s.extend_from_slice(&self.original[prev_end..]);
        s
    }

    /// Merge the original data with changes, **including** uncommitted changes,
    /// and validate that the result is a valid UTF-8 string.
    ///
    /// See the module-level documentation for more information on why uncommitted changes are included.
    pub fn to_string(&self) -> Result<String, Error> {
        Ok(String::from_utf8(self.to_vec())?)
    }

    /// Record a provisional change.
    ///
    /// If committed, the original data in the given `range` will be replaced by the given data.
    /// If there already exist changes for data in the given range (committed or not),
    /// this method will return an error.
    /// It will also return an error if the beginning of the range comes before its end,
    /// or if the range is outside that of the original data.
    pub fn replace_range<T>(&mut self, range: Range<usize>, data: &'a T) -> Result<(), Error>
    where
        T: AsRef<[u8]> + ?Sized,
    {
        if range.start > range.end {
            return Err(Error::InvalidRange(range));
        }

        if range.end > self.original.len() {
            return Err(Error::DataLengthExceeded(range, self.original.len()));
        }

        // Keep sorted by start position, or by end position if the start position is the same,
        // which has the effect of keeping a pure insertion ahead of a replacement.
        // That limits the kinds of conflicts that can happen, simplifying the checks below.
        let ins_point = self.parts.partition_point(|span| {
            span.range.start < range.start
                || (span.range.start == range.start && span.range.end < range.end)
        });

        let incoming = Span::new(range, data.as_ref());

        // Reject if the change starts before the previous one ends.
        if let Some(before) = ins_point.checked_sub(1).and_then(|i| self.parts.get(i)) {
            if incoming.range.start < before.range.end {
                return Err(Error::AlreadyReplaced {
                    is_identical: incoming == *before,
                    range: incoming.range,
                });
            }
        }

        // Reject if the change ends after the next one starts,
        // or if this is an insert and there's already an insert there.
        if let Some(after) = self.parts.get(ins_point) {
            if incoming.range.end > after.range.start || incoming.range == after.range {
                return Err(Error::AlreadyReplaced {
                    is_identical: incoming == *after,
                    range: incoming.range,
                });
            }
        }

        self.parts.insert(ins_point, incoming);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    #[test]
    fn insert_at_beginning() {
        let mut d = Data::new("foo bar baz");
        d.replace_range(0..0, "oh no ").unwrap();
        assert_eq!("oh no foo bar baz", &d.to_string().unwrap());
    }

    #[test]
    fn insert_at_end() {
        let mut d = Data::new("foo bar baz");
        d.replace_range(11..11, " oh no").unwrap();
        assert_eq!("foo bar baz oh no", &d.to_string().unwrap());
    }

    #[test]
    fn replace_some_stuff() {
        let mut d = Data::new("foo bar baz");
        d.replace_range(4..7, "lol").unwrap();
        assert_eq!("foo lol baz", &d.to_string().unwrap());
    }

    #[test]
    fn replace_a_single_char() {
        let mut d = Data::new("let y = true;");
        d.replace_range(4..5, "mut y").unwrap();
        assert_eq!("let mut y = true;", &d.to_string().unwrap());
    }

    #[test]
    fn replace_multiple_lines() {
        let mut d = Data::new("lorem\nipsum\ndolor");

        d.replace_range(6..11, "lol").unwrap();
        assert_eq!("lorem\nlol\ndolor", &d.to_string().unwrap());

        d.replace_range(12..17, "lol").unwrap();
        assert_eq!("lorem\nlol\nlol", &d.to_string().unwrap());
    }

    #[test]
    fn replace_multiple_lines_with_insert_only() {
        let mut d = Data::new("foo!");

        d.replace_range(3..3, "bar").unwrap();
        assert_eq!("foobar!", &d.to_string().unwrap());

        d.replace_range(0..3, "baz").unwrap();
        assert_eq!("bazbar!", &d.to_string().unwrap());

        d.replace_range(3..4, "?").unwrap();
        assert_eq!("bazbar?", &d.to_string().unwrap());
    }

    #[test]
    fn replace_invalid_range() {
        let mut d = Data::new("foo!");

        assert!(d.replace_range(2..1, "bar").is_err());
        assert!(d.replace_range(0..3, "bar").is_ok());
    }

    #[test]
    fn empty_to_string_roundtrip() {
        let s = "";
        assert_eq!(s, &Data::new(s).to_string().unwrap());
    }

    #[test]
    fn replace_same_range_diff_data() {
        let mut d = Data::new("foo bar baz");

        d.replace_range(4..7, "lol").unwrap();
        assert_eq!("foo lol baz", &d.to_string().unwrap());

        assert!(matches!(
            d.replace_range(4..7, "lol2").unwrap_err(),
            Error::AlreadyReplaced {
                is_identical: false,
                ..
            },
        ));
    }

    #[test]
    fn replace_same_range_same_data() {
        let mut d = Data::new("foo bar baz");

        d.replace_range(4..7, "lol").unwrap();
        assert_eq!("foo lol baz", &d.to_string().unwrap());

        assert!(matches!(
            d.replace_range(4..7, "lol").unwrap_err(),
            Error::AlreadyReplaced {
                is_identical: true,
                ..
            },
        ));
    }

    #[test]
    fn broken_replacements() {
        let mut d = Data::new("foo");
        assert!(matches!(
            d.replace_range(4..8, "lol").unwrap_err(),
            Error::DataLengthExceeded(std::ops::Range { start: 4, end: 8 }, 3),
        ));
    }

    #[test]
    fn insert_same_twice() {
        let mut d = Data::new("foo");
        d.replace_range(1..1, "b").unwrap();
        assert_eq!("fboo", &d.to_string().unwrap());
        assert!(matches!(
            d.replace_range(1..1, "b").unwrap_err(),
            Error::AlreadyReplaced {
                is_identical: true,
                ..
            },
        ));
        assert_eq!("fboo", &d.to_string().unwrap());
    }

    #[test]
    fn commit_restore() {
        let mut d = Data::new(", ");
        assert_eq!(", ", &d.to_string().unwrap());

        d.replace_range(2..2, "world").unwrap();
        d.replace_range(0..0, "hello").unwrap();
        assert_eq!("hello, world", &d.to_string().unwrap());

        d.restore();
        assert_eq!(", ", &d.to_string().unwrap());

        d.commit();
        assert_eq!(", ", &d.to_string().unwrap());

        d.replace_range(2..2, "world").unwrap();
        assert_eq!(", world", &d.to_string().unwrap());
        d.commit();
        assert_eq!(", world", &d.to_string().unwrap());
        d.restore();
        assert_eq!(", world", &d.to_string().unwrap());

        d.replace_range(0..0, "hello").unwrap();
        assert_eq!("hello, world", &d.to_string().unwrap());
        d.commit();
        assert_eq!("hello, world", &d.to_string().unwrap());
        d.restore();
        assert_eq!("hello, world", &d.to_string().unwrap());
    }

    proptest! {
        #[test]
        fn new_to_string_roundtrip(ref s in "\\PC*") {
            assert_eq!(s, &Data::new(s).to_string().unwrap());
        }

        #[test]
        fn replace_random_chunks(
            ref data in "\\PC*",
            ref replacements in prop::collection::vec(
                (any::<::std::ops::Range<usize>>(), any::<Vec<u8>>()),
                1..100,
            )
        ) {
            let mut d = Data::new(data);
            for &(ref range, ref bytes) in replacements {
                let _ = d.replace_range(range.clone(), bytes);
            }
        }
    }
}
