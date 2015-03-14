/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use ::FromCss;
use ::cssparser::{Parser, Token};

pub mod discrete;
pub mod range;

#[derive(Copy, Debug, PartialEq, Eq)]
pub enum Range<T> {
    Eq(T),
    Lt(T),
    Le(T),
    Gt(T),
    Ge(T),

    Interval(T, bool, T, bool)
}

impl<T> FromCss for Range<T> where T: FromCss<Err=()> {
    type Err = ();

    // Handles the following forms:
    // * <value> ['=' | '<' | '<=' | '>' | '>=' ]
    // * [':' | '=' | '<' | '<=' | '>' | '>=' ] <value>
    fn from_css(input: &mut Parser) -> Result<Range<T>, ()> {
        macro_rules! range {
            ($value:expr, match op {
                $($eq:pat),* => Eq,
                '<'  => $lt:ident,
                "<=" => $le:ident,
                '>'  => $gt:ident,
                ">=" => $ge:ident
            }) => {{
                let c = match try!(input.next()) {
                    Token::Delim(c) => c,
                    Token::Colon => ':',
                    _ => return Err(())
                };

                match c {
                    $($eq)|* => Ok(Range::Eq($value)),
                    '<' => range!($value, '=' => $le, "<>" => $lt),
                    '>' => range!($value, '=' => $ge, "<>" => $gt),
                    _ => Err(())
                }
            }};
            ($value:expr, '=' => $eq:ident, "<>" => $strict:ident) => {{
                let eq = input.try(|input| {
                    if let Ok(Token::Delim('=')) = input.next_including_whitespace_and_comments() {
                        Ok(())
                    } else {
                        Err(())
                    }
                });

                if eq.is_ok() {
                    Ok(Range::$eq($value))
                } else {
                    Ok(Range::$strict($value))
                }
            }}
        }

        if let Ok(value) = input.try(<T as FromCss>::from_css) {
            range!(value, match op {
                '='  => Eq,
                '<'  => Gt,
                "<=" => Ge,
                '>'  => Lt,
                ">=" => Le
            })
        } else {
            range!(try!(<T as FromCss>::from_css(input)), match op {
                '=',':' => Eq,
                '<'     => Lt,
                "<="    => Le,
                '>'     => Gt,
                ">="    => Ge
            })
        }
    }
}

impl<T> Range<T> {
    /// Combine two Range::{Lt,Le,Gt,Ge} into a single Range::Interval
    pub fn interval(a: Range<T>, b: Range<T>) -> Option<Range<T>> {
        match a {
            Range::Lt(end) => match b {
                Range::Gt(start) => Some(Range::Interval(start, false, end, false)),
                Range::Ge(start) => Some(Range::Interval(start, true, end, false)),
                _ => None
            },
            Range::Le(end) => match b {
                Range::Gt(start) => Some(Range::Interval(start, false, end, true)),
                Range::Ge(start) => Some(Range::Interval(start, true, end, true)),
                _ => None
            },
            Range::Gt(start) => match b {
                Range::Lt(end) => Some(Range::Interval(start, false, end, false)),
                Range::Le(end) => Some(Range::Interval(start, false, end, true)),
                _ => None
            },
            Range::Ge(start) => match b {
                Range::Lt(end) => Some(Range::Interval(start, true, end, false)),
                Range::Le(end) => Some(Range::Interval(start, true, end, true)),
                _ => None
            },

            _ => None
        }
    }

    pub fn map<B, F>(&self, f: F) -> Range<B>
        where F: Fn(&T) -> B
    {
        match *self {
            Range::Eq(ref value) => Range::Eq(f(value)),
            Range::Lt(ref value) => Range::Lt(f(value)),
            Range::Le(ref value) => Range::Le(f(value)),
            Range::Gt(ref value) => Range::Gt(f(value)),
            Range::Ge(ref value) => Range::Ge(f(value)),

            Range::Interval(ref start, start_closed,
                            ref end, end_closed) =>
                Range::Interval(f(start), start_closed,
                                f(end), end_closed)
        }
    }
}

impl<T> Range<T> where T: PartialOrd {
    fn cmp<V>(&self, value: V) -> bool
        where T: PartialOrd<V>, V: PartialOrd
    {
        match *self {
            Range::Eq(ref specified) => *specified == value,
            Range::Lt(ref specified) => *specified >  value,
            Range::Le(ref specified) => *specified >= value,
            Range::Gt(ref specified) => *specified <  value,
            Range::Ge(ref specified) => *specified <= value,

            // [start, end]
            Range::Interval(ref start, true, ref end, true) =>
                *start <= value && *end >= value,
            // [start, end)
            Range::Interval(ref start, true, ref end, false) =>
                *start <= value && *end >  value,
            // (start, end]
            Range::Interval(ref start, false, ref end, true) =>
                *start <  value && *end >= value,
            // (start, end)
            Range::Interval(ref start, false, ref end, false) =>
                *start <  value && *end >  value
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ::FromCss;
    use ::cssparser::Parser;

    #[test]
    fn range_from_css() {
        macro_rules! assert_range_from_css_eq {
            ($css:expr, $variant:ident($value:expr)) => {
                assert_eq!(FromCss::from_css(&mut Parser::new($css)),
                           Ok(Range::$variant($value)));
            };
            ($css:expr, Err) => {
                assert!(<Range<i32> as FromCss>::from_css(&mut Parser::new($css)).is_err())
            }
        }

        assert_range_from_css_eq!(" = 0", Eq(0));
        assert_range_from_css_eq!(" : 0", Eq(0));
        assert_range_from_css_eq!("<  0", Lt(0));
        assert_range_from_css_eq!("<= 0", Le(0));
        assert_range_from_css_eq!(">  0", Gt(0));
        assert_range_from_css_eq!(">= 0", Ge(0));

        assert_range_from_css_eq!("0  =", Eq(0));
        assert_range_from_css_eq!("0  :", Err);
        assert_range_from_css_eq!("0 < ", Gt(0));
        assert_range_from_css_eq!("0 <=", Ge(0));
        assert_range_from_css_eq!("0 > ", Lt(0));
        assert_range_from_css_eq!("0 >=", Le(0));
    }

    #[test]
    fn range_cmp() {
        macro_rules! assert_range_cmp_eq {
            (value_op: $variant:expr, '<' => $lesser:expr, '=' => $equal:expr, '>' => $greater:expr) => {{
                let value_first: Range<i32> = FromCss::from_css(&mut Parser::new(concat!("0 ", $variant))).unwrap();
                assert!(value_first.cmp(-1) == $lesser,  "0 {} -1 != {}", $variant, $lesser);
                assert!(value_first.cmp(0)  == $equal,   "0 {} 0 != {}", $variant, $equal);
                assert!(value_first.cmp(1)  == $greater, "0 {} 1 != {}", $variant, $greater);
            }};
            (op_value: $variant:expr, '<' => $lesser:expr, '=' => $equal:expr, '>' => $greater:expr) => {{
                let op_first: Range<i32> = FromCss::from_css(&mut Parser::new(concat!($variant, " 0"))).unwrap();
                assert!(op_first.cmp(-1) == $greater, "-1 {} 0 != {}", $variant, $greater);
                assert!(op_first.cmp(0)  == $equal,   "0 {} 0 != {}", $variant, $equal);
                assert!(op_first.cmp(1)  == $lesser,  "1 {} 0 != {}", $variant, $lesser);
            }}
        }

        assert_range_cmp_eq!(value_op: "=",  '<' => false, '=' => true,  '>' => false);
        assert_range_cmp_eq!(value_op: "<",  '<' => false, '=' => false, '>' => true);
        assert_range_cmp_eq!(value_op: "<=", '<' => false, '=' => true,  '>' => true);
        assert_range_cmp_eq!(value_op: ">",  '<' => true,  '=' => false, '>' => false);
        assert_range_cmp_eq!(value_op: ">=", '<' => true,  '=' => true,  '>' => false);

        assert_range_cmp_eq!(op_value: "=",  '<' => false, '=' => true,  '>' => false);
        assert_range_cmp_eq!(op_value: ":",  '<' => false, '=' => true,  '>' => false);
        assert_range_cmp_eq!(op_value: "<",  '<' => false, '=' => false, '>' => true);
        assert_range_cmp_eq!(op_value: "<=", '<' => false, '=' => true,  '>' => true);
        assert_range_cmp_eq!(op_value: ">",  '<' => true,  '=' => false, '>' => false);
        assert_range_cmp_eq!(op_value: ">=", '<' => true,  '=' => true,  '>' => false);
    }

    #[test]
    fn range_to_interval() {
        assert_eq!(Range::interval(Range::Gt(0), Range::Lt(2)),
                   Some(Range::Interval(0, false, 2, false)));
        assert_eq!(Range::interval(Range::Ge(0), Range::Lt(2)),
                   Some(Range::Interval(0, true, 2, false)));
        assert_eq!(Range::interval(Range::Gt(0), Range::Le(2)),
                   Some(Range::Interval(0, false, 2, true)));
        assert_eq!(Range::interval(Range::Ge(0), Range::Le(2)),
                   Some(Range::Interval(0, true, 2, true)));

        assert_eq!(Range::interval(Range::Lt(2), Range::Gt(0)),
                   Some(Range::Interval(0, false, 2, false)));
        assert_eq!(Range::interval(Range::Le(2), Range::Gt(0)),
                   Some(Range::Interval(0, false, 2, true)));
        assert_eq!(Range::interval(Range::Lt(2), Range::Ge(0)),
                   Some(Range::Interval(0, true, 2, false)));
        assert_eq!(Range::interval(Range::Le(2), Range::Ge(0)),
                   Some(Range::Interval(0, true, 2, true)));
    }
}
