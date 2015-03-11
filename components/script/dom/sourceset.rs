use util::str::DOMString;

use std::str::FromStr;

mod parse {
    use util::str::parse_unsigned_integer;

    use std::borrow::ToOwned;
    use std::iter::Enumerate;
    use std::ops::Range;
    use std::str::Chars;

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct Error {
        span: Range<usize>,
        kind: ErrorKind
    }

    impl Error {
        pub fn at(i: usize, kind: ErrorKind) -> Error {
            Error { span: i .. i+1, kind: kind }
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum ErrorKind {
        UnexpectedComma,
        Descriptor(DescriptorErrorKind)
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub enum DescriptorErrorKind {
        UnsupportedType(char),
        InvalidValue,
        Duplicate(Range<usize>),
        Incompatible(Range<usize>),
        HeightRequiresWidth
    }

    #[derive(Debug, PartialEq, Eq)]
    struct ImageDescriptor<'a> {
        span: Range<usize>,
        value: &'a str
    }

    #[derive(Debug, PartialEq, Eq)]
    struct ImageSource<'a> {
        url: &'a str,
        descriptors: Vec<ImageDescriptor<'a>>
    }

    pub struct SourceSetParser<'a> {
        input: &'a str,
        iterator: Enumerate<Chars<'a>>,

        candidates: Vec<super::ImageSource>,
        parse_errors: Vec<Error>
    }

    fn is_html_whitespace(c: char) -> bool {
        match c {
            ' ' | '\t' | '\x0a' | '\x0c' | '\x0d' => true,
            _ => false
        }
    }

    impl<'a> SourceSetParser<'a> {
        pub fn new(input: &'a str) -> SourceSetParser<'a> {
            // 1. thru 3.
            SourceSetParser {
                input: input,
                iterator: input.chars().enumerate(),

                candidates: vec![],
                parse_errors: vec![]
            }
        }

        fn parse_error(&mut self, error: Error) {
            self.parse_errors.push(error);
        }

        fn char_at(&self, i: usize) -> char {
            self.input.char_at(i)
        }

        fn find<P>(&mut self, predicate: P) -> Option<(usize, char)>
            where P: FnMut(&(usize, char)) -> bool
        {
            self.iterator.find(predicate)
        }

        /// Parse a 'srcset' attribute as specified by
        /// https://www.whatwg.org/html/#parse-a-srcset-attribute
        pub fn parse(mut self) -> Result<super::SourceSet, (super::SourceSet, Vec<Error>)> {
            // 4. thru 8.
            while let Some(candidate) = self.parse_url() {
                // 9. thru 15.
                match candidate.to_image_source() {
                    Ok(image_source) => self.candidates.push(image_source),
                    Err((Some(image_source), mut parse_errors)) => {
                        self.candidates.push(image_source);
                        self.parse_errors.append(&mut parse_errors);
                    }
                    Err((None, ref mut parse_errors)) =>
                        self.parse_errors.append(parse_errors)
                }
            }

            let source_set = super::SourceSet { sources: self.candidates };
            if self.parse_errors.is_empty() {
                Ok(source_set)
            } else {
                Err((source_set, self.parse_errors))
            }
        }

        fn parse_url(&mut self) -> Option<ImageSource<'a>> {
            // 4. Splitting loop: Collect a sequence of characters that are space
            // characters or U+002C COMMA characters. If any U+002C COMMA characters
            // were collected, that is a parse error
            let mut unexpected_comma = None;
            let start = self.find(|t| {
                match t.1 {
                    c if is_html_whitespace(c) => false,
                    ',' => {
                        if unexpected_comma.is_none() {
                            unexpected_comma = Some(Error::at(t.0, ErrorKind::UnexpectedComma))
                        }
                        false
                    }
                    _ => true
                }
            });

            // we only track the first unexpected comma seen
            if let Some(error) = unexpected_comma {
                self.parse_errors.push(error);
            }

            match start {
                // 5. If position is past the end of input, return candidates and abort
                // these steps
                None => None,
                // 6. Collect a sequence of characters that are not space characters,
                // and let that be url
                Some((start, _)) => {
                    let mut url = match self.find(|t| is_html_whitespace(t.1)) {
                        Some((end, _)) => start..end,
                        None => start..self.input.len()
                    };

                    // 8.
                    let descriptors = if self.maybe_parse_no_descriptor(&mut url) {
                        vec![]
                    } else {
                        self.parse_descriptors()
                    };

                    Some(ImageSource {
                        url: &self.input[url],
                        descriptors: descriptors
                    })
                }
            }
        }

        fn maybe_parse_no_descriptor(&mut self, url: &mut Range<usize>) -> bool {
            if self.char_at(url.end - 1) != ',' {
                return false;
            }

            // 8.1.1. Remove all trailing U+002C COMMA characters from url.
            // If this removed more than one character, that is a parse error.
            let mut truncate_by = 1;
            while self.char_at(url.end - 1 - truncate_by) == ',' {
                truncate_by += 1;
            }

            url.end -= truncate_by;
            if truncate_by > 1 {
                self.parse_error(Error::at(url.end + 1, ErrorKind::UnexpectedComma));
            }

            true
        }

        fn parse_descriptors(&mut self) -> Vec<ImageDescriptor<'a>> {
            enum State {
                InDescriptor,
                InParens,
                AfterDescriptor,
            }

            // 7. Let descriptors be a new empty list.
            let mut descriptors = vec![];

            // 8.2.1. Descriptor tokeniser: Skip whitespace
            let mut start = match self.find(|t| !is_html_whitespace(t.1)) {
                Some((start, _)) => start,
                None => return descriptors
            };
            // 8.2.3. Let state be in descriptor.
            let mut state = State::InDescriptor;

            macro_rules! push_descriptor {
                ($range:expr) => {
                    if $range.end > $range.start {
                        descriptors.push(ImageDescriptor {
                            span: $range,
                            value: &self.input[$range]
                        });
                    }
                };
                ($start:expr => to_eof) => {
                    push_descriptor!($start .. self.input.len())
                }
            }

            // Step 8.2.4
            loop {
                match state {
                    State::InDescriptor => {
                        fn is_html_whitespace_comma_or_lparen(t: &(usize, char)) -> bool {
                            match t.1 {
                                c if is_html_whitespace(c) => true,
                                ',' => true,
                                '(' => true,
                                _ => false
                            }
                        }
                        match self.find(is_html_whitespace_comma_or_lparen) {
                            Some((end, c)) if is_html_whitespace(c) => {
                                push_descriptor!(start .. end);
                                state = State::AfterDescriptor;
                                continue;
                            }
                            Some((end, c)) if c == ',' => {
                                push_descriptor!(start .. end);
                                break;
                            }
                            Some((_, c)) if c == '(' => state = State::InParens,
                            Some(_) => unreachable!(),
                            None => {
                                push_descriptor!(start => to_eof);
                                break;
                            }
                        }
                    }
                    State::InParens => {
                        if self.find(|t| t.1 == ')').is_some() {
                            state = State::InDescriptor;
                        } else {
                            push_descriptor!(start => to_eof);
                            break;
                        }
                    }
                    State::AfterDescriptor => {
                        if let Some((i, _)) = self.find(|t| !is_html_whitespace(t.1)) {
                            start = i;
                            state = State::InDescriptor;
                        } else {
                            break;
                        }
                    }
                }
            }

            descriptors
        }
    }

    impl<'a> ImageDescriptor<'a> {
        fn digits(&self) -> &'a str {
            &self.value[..self.value.len() - 1]
        }

        pub fn to_u32(&self) -> Option<u32> {
            parse_unsigned_integer(self.digits().chars())
        }

        pub fn to_f64(&self) -> Option<f64> {
            //TODO: parse_floating_point_number(self.digits().chars())
            self.digits().parse::<f64>().ok()
        }

        pub fn suffix(&self) -> char {
            self.value.char_at(self.value.len() - 1)
        }
    }

    impl<'a> ImageSource<'a> {
        pub fn to_image_source(self) -> Result<super::ImageSource, (Option<super::ImageSource>, Vec<Error>)> {
            // 9. thru 12.
            let mut error = false;
            let mut parse_errors = vec![];
            let mut width: Option<(Range<usize>, super::ImageDescriptor)> = None;
            let mut density: Option<(Range<usize>, super::ImageDescriptor)> = None;
            let mut height: Option<(Range<usize>, u32)> = None;

            macro_rules! ekd {
                ($type_:ident) => {
                    ErrorKind::Descriptor(DescriptorErrorKind::$type_)
                };
                ($type_:ident, $($arg:expr),+) => {
                    ErrorKind::Descriptor(DescriptorErrorKind::$type_($($arg),+))
                }
            }

            macro_rules! clone_span {
                ($descriptor:ident) => {
                    $descriptor.as_ref().unwrap().0.clone()
                }
            }

            macro_rules! add_error_at {
                ($type_:ident, $span:expr) => {{
                    parse_errors.push(Error {
                        span: $span,
                        kind: ekd!($type_)
                    });
                    error = true;
                }};
                ($type_:ident, $span:expr, $($arg:expr),+) => {{
                    parse_errors.push(Error {
                        span: $span,
                        kind: ekd!($type_, $($arg),+)
                    });
                    error = true;
                }}
            }

            // 13. For each descriptor in descriptors, run the appropriate set of steps
            // from the following list:
            for descriptor in self.descriptors {
                macro_rules! add_error {
                    (InvalidValue) => {
                        add_error_at!(InvalidValue,
                                      descriptor.span.start .. descriptor.span.end - 1);
                    };
                    ($type_:ident, $($arg:expr),+) => {
                        add_error_at!($type_, descriptor.span.clone(), $($arg),+);
                    };
                    ($type_:ident) => {
                        add_error_at!($type_, descriptor.span.clone());
                    }
                }

                match descriptor.suffix() {
                    'w' => {
                        if let Some(value) = descriptor.to_u32() {
                            // TODO: add support for sizes attribute
                            add_error!(UnsupportedType, 'w');

                            if width.is_some() {
                                add_error!(Duplicate, clone_span!(width));
                            } else if density.is_some() {
                                add_error!(Incompatible, clone_span!(density));
                            } else if value == 0 {
                                add_error!(InvalidValue);
                            } else {
                                width = Some((descriptor.span.clone(),
                                              super::ImageDescriptor::Width(value)));
                            }
                        } else {
                            add_error!(InvalidValue);
                        }
                    }
                    'x' => {
                        if let Some(value) = descriptor.to_f64() {
                            if density.is_some() {
                                add_error!(Duplicate, clone_span!(density));
                            } else if width.is_some() || height.is_some() {
                                let first_at = match (width.as_ref(), height.as_ref()) {
                                    (Some(&(ref width, _)), Some(&(ref height, _))) =>
                                        if width.start < height.start {
                                            width.clone()
                                        } else {
                                            height.clone()
                                        },
                                    (Some(&(ref width, _)), None) => width.clone(),
                                    (None, Some(&(ref height, _))) => height.clone(),
                                    (None, None) => unreachable!()
                                };
                                add_error!(Incompatible, first_at);
                            } else if value < 0. {
                                add_error!(InvalidValue);
                            } else {
                                density = Some((descriptor.span.clone(),
                                                super::ImageDescriptor::PixelDensity(value)))
                            }
                        } else {
                            add_error!(InvalidValue);
                        }
                    }
                    'h' => {
                        if let Some(value) = descriptor.to_u32() {
                            // this is *not* a fail to construct image source error
                            parse_errors.push(Error {
                                span: descriptor.span.clone(),
                                kind: ekd!(UnsupportedType, 'h')
                            });

                            if height.is_some() {
                                add_error!(Duplicate, clone_span!(height));
                            } else if density.is_some() {
                                add_error!(Incompatible, clone_span!(density));
                            } else if value == 0 {
                                add_error!(InvalidValue);
                            } else {
                                height = Some((descriptor.span.clone(),
                                               value));
                            }
                        } else {
                            add_error!(InvalidValue);
                        }
                    }
                    s => add_error!(UnsupportedType, s)
                };
            }

            // 14.
            if height.is_some() && width.is_none() {
                add_error_at!(HeightRequiresWidth, clone_span!(height));
            }

            // 15.
            if !error {
                let source = super::ImageSource {
                    url: self.url.to_owned(),
                    descriptor: width.or(density).map(|t| t.1)
                };

                if parse_errors.is_empty() {
                    Ok(source)
                } else {
                    Err((Some(source), parse_errors))
                }
            } else {
                Err((None, parse_errors))
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::super::*;
        use super::ImageSource as CandidateImageSource;
        use super::ImageDescriptor as CandidateImageDescriptor;
        use super::{Error, ErrorKind, DescriptorErrorKind};

        use std::borrow::ToOwned;

        macro_rules! cid {
            ($span:expr, $value:expr) => {
                CandidateImageDescriptor { span: $span, value: $value }
            }
        }

        macro_rules! pde {
            ($type_:ident, $span:expr) => {
                Error {
                    span: $span,
                    kind: ErrorKind::Descriptor(DescriptorErrorKind::$type_)
                }
            };
            ($type_:ident, $span:expr, $($arg:expr),+) => {
                Error {
                    span: $span,
                    kind: ErrorKind::Descriptor(DescriptorErrorKind::$type_($($arg),+))
                }
            }
        }

        #[test]
        fn to_image_source() {
            // valid
            println!("valid");
            let input = CandidateImageSource { url: "test.png", descriptors: vec![] };
            assert_eq!(input.to_image_source(),
                       Ok(ImageSource { url: "test.png".to_owned(),
                                        descriptor: None
                       }));

            let input = CandidateImageSource { url: "test.png", descriptors: vec![cid!(0..2, "1x")] };
            assert_eq!(input.to_image_source(),
                       Ok(ImageSource { url: "test.png".to_owned(),
                                        descriptor: Some(ImageDescriptor::PixelDensity(1.))
                       }));

            // invalid
            // width
            println!("invalid: width");
            let input = CandidateImageSource { url: "test.png",
                                               descriptors: vec![cid!(0..2, "0w"),
                                                                 cid!(2..4, "zw")] };
            assert_eq!(input.to_image_source(),
                       Err((None, vec![pde!(UnsupportedType, 0..2, 'w'),
                                       pde!(InvalidValue, 0..1),
                                       pde!(InvalidValue, 2..3)])));

            let input = CandidateImageSource { url: "test.png",
                                               descriptors: vec![cid!(0..2, "1w"),
                                                                 cid!(2..4, "2w")] };
            assert_eq!(input.to_image_source(),
                       Err((None, vec![pde!(UnsupportedType, 0..2, 'w'),
                                       pde!(UnsupportedType, 2..4, 'w'),
                                       pde!(Duplicate, 2..4, 0..2)])));

            let input = CandidateImageSource { url: "test.png",
                                               descriptors: vec![cid!(0..2, "1x"),
                                                                 cid!(2..4, "2w")] };
            assert_eq!(input.to_image_source(),
                       Err((None, vec![pde!(UnsupportedType, 2..4, 'w'),
                                       pde!(Incompatible, 2..4, 0..2)])));

            // pixel density
            println!("invalid: pixel density");
            let input = CandidateImageSource { url: "test.png",
                                               descriptors: vec![cid!(0..3, "-1x"),
                                                                 cid!(3..5, "zx")] };
            assert_eq!(input.to_image_source(),
                       Err((None, vec![pde!(InvalidValue, 0..2),
                                       pde!(InvalidValue, 3..4)])));

            let input = CandidateImageSource { url: "test.png",
                                               descriptors: vec![cid!(0..2, "1x"),
                                                                 cid!(2..4, "2x")] };
            assert_eq!(input.to_image_source(),
                       Err((None, vec![pde!(Duplicate, 2..4, 0..2)])));

            let input = CandidateImageSource { url: "test.png",
                                               descriptors: vec![cid!(0..2, "1w"),
                                                                 cid!(2..4, "2x")] };
            assert_eq!(input.to_image_source(),
                       Err((None, vec![pde!(UnsupportedType, 0..2, 'w'),
                                       pde!(Incompatible, 2..4, 0..2)])));

            let input = CandidateImageSource { url: "test.png",
                                               descriptors: vec![cid!(0..2, "1h"),
                                                                 cid!(2..4, "2x")] };
            assert_eq!(input.to_image_source(),
                       Err((None, vec![pde!(UnsupportedType, 0..2, 'h'),
                                       pde!(Incompatible, 2..4, 0..2),
                                       pde!(HeightRequiresWidth, 0..2)])));

            // height
            println!("invalid: height");
            let input = CandidateImageSource { url: "test.png",
                                               descriptors: vec![cid!(0..2, "0h"),
                                                                 cid!(2..4, "zh")] };
            assert_eq!(input.to_image_source(),
                       Err((None, vec![pde!(UnsupportedType, 0..2, 'h'),
                                       pde!(InvalidValue, 0..1),
                                       pde!(InvalidValue, 2..3)])));

            let input = CandidateImageSource { url: "test.png",
                                               descriptors: vec![cid!(0..2, "1h"),
                                                                 cid!(2..4, "2h")] };
            assert_eq!(input.to_image_source(),
                       Err((None, vec![pde!(UnsupportedType, 0..2, 'h'),
                                       pde!(UnsupportedType, 2..4, 'h'),
                                       pde!(Duplicate, 2..4, 0..2),
                                       pde!(HeightRequiresWidth, 0..2)])));

            let input = CandidateImageSource { url: "test.png",
                                               descriptors: vec![cid!(0..2, "1x"),
                                                                 cid!(2..4, "2h")] };
            assert_eq!(input.to_image_source(),
                       Err((None, vec![pde!(UnsupportedType, 2..4, 'h'),
                                       pde!(Incompatible, 2..4, 0..2)])));

            // unknown
            println!("invalid: unknown");
            let input = CandidateImageSource { url: "test.png",
                                               descriptors: vec![cid!(0..2, "1u")] };
            assert_eq!(input.to_image_source(),
                       Err((None, vec![pde!(UnsupportedType, 0..2, 'u')])));
        }

        #[test]
        fn parse() {
            // valid
            println!("valid");
            assert_eq!("".parse::<SourceSet>().unwrap(),
                       SourceSet { sources: vec![]});

            assert_eq!(" foo.png, bar.png 2x ".parse::<SourceSet>().unwrap(),
                       SourceSet { sources: vec![
                           ImageSource { url: "foo.png".to_owned(),
                                         descriptor: None },
                           ImageSource { url: "bar.png".to_owned(),
                                         descriptor: Some(ImageDescriptor::PixelDensity(2.)) }]});

            // valid with errors
            println!("valid w/ errors");
            assert_eq!("foo.png 2u, bar.png 1x, baz.png 2x".parse::<SourceSet>().unwrap_err(),
                       (SourceSet { sources: vec![
                           ImageSource { url: "bar.png".to_owned(),
                                         descriptor: Some(ImageDescriptor::PixelDensity(1.)) },
                           ImageSource { url: "baz.png".to_owned(),
                                         descriptor: Some(ImageDescriptor::PixelDensity(2.)) }]},
                        vec![pde!(UnsupportedType, 8..10, 'u')]));

            // invalid
            println!("invalid");
            assert_eq!(",test.png,,,".parse::<SourceSet>().unwrap_err(),
                       (SourceSet { sources: vec![
                           ImageSource { url: "test.png".to_owned(),
                                         descriptor: None }] },
                        vec![Error{ span: 0..1, kind: ErrorKind::UnexpectedComma },
                             Error{ span: 10..11, kind: ErrorKind::UnexpectedComma }]));
        }
    }
}

#[derive(Copy, Debug, PartialEq)]
pub enum ImageDescriptor {
    Width(u32),
    PixelDensity(f64)
}

#[derive(Debug, PartialEq)]
pub struct ImageSource {
    url: DOMString,
    descriptor: Option<ImageDescriptor>,
}

#[derive(Debug, PartialEq)]
pub struct SourceSet {
    pub sources: Vec<ImageSource>,
}

impl FromStr for SourceSet {
    type Err = (SourceSet, Vec<parse::Error>);

    /// Parse a 'srcset' attribute as specified by
    /// https://www.whatwg.org/html/#parse-a-srcset-attribute
    fn from_str(s: &str) -> Result<SourceSet, (SourceSet, Vec<parse::Error>)> {
        parse::SourceSetParser::new(s).parse()
    }
}
