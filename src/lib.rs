
use std::rc;
use std::sync;
use std::error;
use std::fmt;

macro_rules! assert_parse_ok {
    ($parser:expr, $input:expr, $expected:expr $(,)*) => {{
        let input: String = ($input).into();
        assert_eq!(
            ::apply(&input, ::Options::default(), $parser),
            Ok($expected)
        );
        drop(input);
    }}
}

macro_rules! assert_parse_no_match {
    ($parser:expr, $input:expr $(,)*) => {{
        let input: String = ($input).into();
        assert_eq!(
            ::apply(&input, ::Options::default(), $parser),
            Err(::ApplyError::NoMatch)
        );
        drop(input);
    }}
}

macro_rules! assert_parse_fail {
    ($parser:expr, $input:expr, $value:expr, ($($l:tt)+) $(,)*) => {{
        let input: String = ($input).into();
        assert_eq!(
            ::apply(&input, ::Options::default(), $parser),
            Err(::ApplyError::Fail(::Error::new(::Location::new($($l)+), $value)))
        );
        drop(input);
    }}
}

macro_rules! assert_parse_partial {
    ($parser:expr, $input:expr, $value:expr, $rest:expr, ($($l:tt)+) $(,)*) => {{
        let input: String = ($input).into();
        assert_eq!(
            ::apply(&input, ::Options::default(), $parser),
            Err(::ApplyError::Unparsed($value, ::Location::new($($l)+), $rest))
        );
        drop(input);
    }}
}

pub mod util;
pub mod parse;

pub fn apply<'src, P>(input: &'src str, options: Options, parser: P)
-> Result<P::Output, ApplyError<'src, P::Output, P::Error>>
where P: Parser<'src> {
    let depth_limit = options.depth_limit;
    let ctx = Context {
        options: options,
        depth: 1,
    };
    let input = Input {
        rest: input,
        location: Location {
            line: 1,
            column: 1,
            offset: 0,
        },
    };
    match parser.parse(input, ctx) {
        Ok((value, input)) => match input.unpack_if_nonempty() {
            None => Ok(value),
            Some((location, rest)) => Err(ApplyError::Unparsed(value, location, rest)),
        },
        Err(Fail::NoMatch) =>
            Err(ApplyError::NoMatch),
        Err(Fail::DepthLimit(location)) =>
            Err(ApplyError::DepthLimit(location, depth_limit.unwrap())),
        Err(Fail::Error(error)) =>
            Err(ApplyError::Fail(error)),
        Err(Fail::ZeroLengthRepeat(location)) =>
            Err(ApplyError::ZeroLengthRepeat(location)),
    }
}

#[derive( Debug, Clone )]
pub struct Context {
    options: Options,
    depth: usize,
}

impl Context {

    pub fn descend<'src, E>(&self, input: &Input<'src>) -> Result<Context, Fail<E>> {
        let new_depth = self.depth + 1;
        if let Some(limit) = self.options.depth_limit {
            if new_depth > limit {
                return Err(Fail::DepthLimit(input.location));
            }
        }
        Ok(Context {
            options: self.options.clone(),
            depth: new_depth,
        })
    }
}

#[derive( Debug, Clone )]
pub struct Options {
    pub depth_limit: Option<usize>,
}

impl Default for Options {

    fn default() -> Options {
        Options {
            depth_limit: Some(64),
        }
    }
}

#[derive( Debug, Clone, PartialEq, Eq )]
pub enum ApplyError<'src, T, E> {
    NoMatch,
    Fail(Error<E>),
    DepthLimit(Location, usize),
    Unparsed(T, Location, &'src str),
    ZeroLengthRepeat(Location),
}

pub type Outcome<'src, O, E> = Result<(O, Input<'src>), Fail<E>>;

pub trait Parser<'src> {

    type Output;
    type Error;

    fn parse(&self, input: Input<'src>, ctx: Context)
    -> Outcome<'src, Self::Output, Self::Error>;
}

impl<'src, P> Parser<'src> for sync::Arc<P> where P: Parser<'src> {

    type Output = P::Output;
    type Error = P::Error;

    fn parse(&self, input: Input<'src>, ctx: Context)
    -> Outcome<'src, Self::Output, Self::Error> {
        (**self).parse(input, ctx)
    }
}

impl<'src, P> Parser<'src> for rc::Rc<P> where P: Parser<'src> {

    type Output = P::Output;
    type Error = P::Error;

    fn parse(&self, input: Input<'src>, ctx: Context)
    -> Outcome<'src, Self::Output, Self::Error> {
        (**self).parse(input, ctx)
    }
}

impl<'src, P> Parser<'src> for Box<P> where P: Parser<'src> {

    type Output = P::Output;
    type Error = P::Error;

    fn parse(&self, input: Input<'src>, ctx: Context)
    -> Outcome<'src, Self::Output, Self::Error> {
        (**self).parse(input, ctx)
    }
}

impl<'src, 'p, P> Parser<'src> for &'p P where P: Parser<'src> {

    type Output = P::Output;
    type Error = P::Error;

    fn parse(&self, input: Input<'src>, ctx: Context)
    -> Outcome<'src, Self::Output, Self::Error> {
        (**self).parse(input, ctx)
    }
}

#[derive( Debug, Clone, PartialEq, Eq )]
pub enum Fail<E> {
    NoMatch,
    Error(Error<E>),
    DepthLimit(Location),
    ZeroLengthRepeat(Location),
}

#[derive( Debug, Clone, Copy, PartialEq, Eq )]
pub struct Location {
    line: usize,
    column: usize,
    offset: usize,
}

impl Location {

    pub fn new(line: usize, column: usize, offset: usize) -> Location {
        Location {
            line: line,
            column: column,
            offset: offset,
        }
    }

    pub fn line(&self) -> usize { self.line }
    pub fn column(&self) -> usize { self.column }
    pub fn offset(&self) -> usize { self.offset }

    fn advance(&self, parsed: &str) -> Location {
        let nl_count = parsed
            .chars()
            .filter(|c| *c == '\n')
            .count();
        let last_line = parsed
            .rfind('\n')
            .map(|nl_pos| &parsed[(nl_pos+1)..])
            .unwrap_or(&parsed);
        let last_line_chars = last_line
            .chars()
            .count();
        Location {
            offset: self.offset + parsed.len(),
            line: self.line + nl_count,
            column: last_line_chars + if nl_count > 0 { 1 } else { self.column },
        }
    }
}

#[derive( Debug, Clone )]
pub struct Input<'src> {
    rest: &'src str,
    location: Location,
}

impl<'src> Input<'src> {

    pub fn location(&self) -> Location { self.location }

    pub fn consume_len_via<F>(self, find_len: F) -> Option<(&'src str, Input<'src>)>
    where F: FnOnce(&'src str) -> Option<usize> {
        let len = match find_len(self.rest) {
            None => return None,
            Some(len) => len,
        };
        let consumed = &self.rest[..len];
        Some((consumed, self.advance(len)))
    }

    pub fn consume_char(self) -> Option<(char, Input<'src>)> {
        let chr = match self.rest.chars().nth(0) {
            None => return None,
            Some(chr) => chr,
        };
        Some((chr, self.advance(chr.len_utf8())))
    }

    fn advance(self, len: usize) -> Input<'src> {
        let consumed = &self.rest[..len];
        let new_rest = &self.rest[len..];
        let new_location = self.location.advance(consumed);
        Input {
            rest: new_rest,
            location: new_location,
        }
    }

    fn unpack_if_nonempty(self) -> Option<(Location, &'src str)> {
        if self.rest.len() > 0 {
            Some((self.location, self.rest))
        }
        else {
            None
        }
    }
}

#[derive( Debug, Clone, PartialEq, Eq )]
pub struct Error<E> {
    location: Location,
    value: E,
    cause: Option<Box<Error<E>>>,
}

impl<E> Error<E> {

    pub fn new(location: Location, value: E) -> Error<E> {
        Error {
            location: location,
            value: value,
            cause: None,
        }
    }

    pub fn new_with_cause(location: Location, value: E, cause: Error<E>) -> Error<E> {
        Error {
            location: location,
            value: value,
            cause: Some(Box::new(cause)),
        }
    }

    pub fn location(&self) -> Location { self.location }

    pub fn value(&self) -> &E { &self.value }

    pub fn cause(&self) -> Option<&Error<E>> {
        match self.cause {
            Some(ref error) => Some(&*error),
            None => None,
        }
    }
}

impl<E> fmt::Display for Error<E> where E: fmt::Display {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.value, fmt)
    }
}

impl<E> error::Error for Error<E> where E: error::Error {

    fn description(&self) -> &str {
        self.value.description()
    }

    fn cause(&self) -> Option<&error::Error> {
        match self.cause {
            Some(ref error) => Some(&*error),
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use parse;

    #[test]
    fn apply() {

        #[derive( Debug, Clone, PartialEq, Eq )]
        struct ErrMarker;

        let parser = parse::pair(
            parse::exact_str("foo"),
            parse::require(parse::exact_str("bar"), || ErrMarker),
        );

        assert_parse_ok!(&parser, "foobar", ("foo", "bar"));
        assert_parse_no_match!(&parser, "qux");
        assert_parse_fail!(&parser, "fooqux", ErrMarker, (1, 4, 3));
    }

    #[test]
    fn pointers() {
        use std::rc::Rc;
        use std::sync::Arc;

        let plain = parse::no_custom_error(parse::exact_str("foo"));
        assert_eq!(::apply("foo", ::Options::default(), &plain), Ok("foo"));

        let boxed = Box::new(parse::no_custom_error(parse::exact_str("foo")));
        assert_eq!(::apply("foo", ::Options::default(), boxed), Ok("foo"));

        let rced = Rc::new(parse::no_custom_error(parse::exact_str("foo")));
        assert_eq!(::apply("foo", ::Options::default(), rced), Ok("foo"));

        let arced = Arc::new(parse::no_custom_error(parse::exact_str("foo")));
        assert_eq!(::apply("foo", ::Options::default(), arced), Ok("foo"));
    }

    #[test]
    fn location() {
        use super::Location as L;

        let loc = L::new(1, 1, 0).advance("foo");
        assert_eq!(loc, L::new(1, 4, 3));

        let loc = loc.advance(" \t");
        assert_eq!(loc, L::new(1, 6, 5));

        let loc = loc.advance("\n");
        assert_eq!(loc, L::new(2, 1, 6));

        let loc = loc.advance("a\nb\nc");
        assert_eq!(loc, L::new(4, 2, 11));
    }
}
