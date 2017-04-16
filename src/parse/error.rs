
use util;

pub fn no_custom_error<'src, P>(parser: P) -> P where P: ::Parser<'src, Error=()> {
    parser
}

pub fn require<'src, P, F>(parser: P, error_gen: F) -> Require<P, F>
where
    P: ::Parser<'src>,
    F: Fn() -> P::Error,
{
    Require {
        parser: parser,
        error_gen: error_gen,
    }
}

#[derive( Debug, Clone )]
pub struct Require<P, F> {
    parser: P,
    error_gen: F,
}

impl<'src, P, F> ::Parser<'src> for Require<P, F>
where
    P: ::Parser<'src>,
    F: Fn() -> P::Error,
{
    type Output = P::Output;
    type Error = P::Error;

    fn parse(&self, input: ::Input<'src>, ctx: ::Context)
    -> ::Outcome<'src, Self::Output, Self::Error> {
        let location = input.location();
        match self.parser.parse(input, ctx) {
            Err(::Fail::NoMatch) => Err(util::fail(location, (self.error_gen)())),
            other => other,
        }
    }
}

pub fn restrict<'src, P, F>(parser: P, test: F) -> Restrict<P, F>
where
    P: ::Parser<'src>,
    F: Fn(&P::Output) -> bool,
{
    Restrict {
        parser: parser,
        test: test,
    }
}

#[derive( Debug, Clone )]
pub struct Restrict<P, F> {
    parser: P,
    test: F,
}

impl<'src, P, F> ::Parser<'src> for Restrict<P, F>
where
    P: ::Parser<'src>,
    F: Fn(&P::Output) -> bool,
{
    type Output = P::Output;
    type Error = P::Error;

    fn parse(&self, input: ::Input<'src>, ctx: ::Context)
    -> ::Outcome<'src, Self::Output, Self::Error> {
        let (res, input) = self.parser.parse(input, ctx)?;
        if (self.test)(&res) {
            Ok((res, input))
        }
        else {
            Err(::Fail::NoMatch)
        }
    }
}

pub fn verify<'src, P, F>(parser: P, test: F) -> Verify<P, F>
where
    P: ::Parser<'src>,
    F: Fn(&P::Output) -> Option<P::Error>,
{
    Verify {
        parser: parser,
        test: test,
    }
}

#[derive( Debug, Clone )]
pub struct Verify<P, F> {
    parser: P,
    test: F,
}

impl<'src, P, F> ::Parser<'src> for Verify<P, F>
where
    P: ::Parser<'src>,
    F: Fn(&P::Output) -> Option<P::Error>,
{
    type Output = P::Output;
    type Error = P::Error;

    fn parse(&self, input: ::Input<'src>, ctx: ::Context)
    -> ::Outcome<'src, Self::Output, Self::Error> {
        let location = input.location();
        let (res, input) = self.parser.parse(input, ctx)?;
        if let Some(error) = (self.test)(&res) {
            Err(::Fail::Error(::Error::new(location, error)))
        }
        else {
            Ok((res, input))
        }
    }
}

pub fn error_context<'src, P, F>(parser: P, error_gen: F) -> ErrorContext<P, F>
where
    P: ::Parser<'src>,
    F: Fn() -> P::Error,
{
    ErrorContext {
        parser: parser,
        error_gen: error_gen,
    }
}

#[derive( Debug, Clone )]
pub struct ErrorContext<P, F> {
    parser: P,
    error_gen: F,
}

impl<'src, P, F> ::Parser<'src> for ErrorContext<P, F>
where
    P: ::Parser<'src>,
    F: Fn() -> P::Error,
{
    type Output = P::Output;
    type Error = P::Error;

    fn parse(&self, input: ::Input<'src>, ctx: ::Context)
    -> ::Outcome<'src, Self::Output, Self::Error> {
        let location = input.location();
        match self.parser.parse(input, ctx) {
            Err(::Fail::Error(error)) => Err(::Fail::Error(::Error::new_with_cause(
                location,
                (self.error_gen)(),
                error,
            ))),
            other => other,
        }
    }
}

#[cfg(test)]
mod tests {
    use parse;

    #[derive( Debug, Clone, PartialEq, Eq )]
    struct ErrMarker;

    #[test]
    fn error_context() {

        #[derive( Debug, Clone, PartialEq, Eq )]
        enum ErrStack {
            Original,
            Context,
        }

        let parser = parse::error_context(
            parse::pair(
                parse::exact_str("foo"),
                parse::require(parse::exact_str("bar"), || ErrStack::Original),
            ),
            || ErrStack::Context,
        );
        assert_parse_ok!(&parser, "foobar", ("foo", "bar"));
        assert_parse_no_match!(&parser, "");

        assert_eq!(
            ::apply("fooqux", ::Options::default(), &parser),
            Err(::ApplyError::Fail(::Error::new_with_cause(
                ::Location::new(1, 1, 0),
                ErrStack::Context,
                ::Error::new(
                    ::Location::new(1, 4, 3),
                    ErrStack::Original,
                ),
            )))
        );
    }

    #[test]
    fn verify() {
        let parser = parse::verify(
            parse::any_char(),
            |&c| if c == 'X' { Some(ErrMarker) } else { None },
        );
        assert_parse_ok!(&parser, "Y", 'Y');
        assert_parse_no_match!(&parser, "");
        assert_parse_fail!(&parser, "X", ErrMarker, (1, 1, 0));
    }

    #[test]
    fn restrict() {
        let parser = parse::no_custom_error(parse::restrict(
            parse::any_char(),
            |&c| c != 'X',
        ));
        assert_parse_ok!(&parser, "Y", 'Y');
        assert_parse_no_match!(&parser, "X");
        assert_parse_no_match!(&parser, "");
    }

    #[test]
    fn require() {
        let parser = parse::pair(
            parse::exact_str("foo"),
            parse::require(parse::exact_str("bar"), || ErrMarker),
        );
        assert_parse_ok!(&parser, "foobar", ("foo", "bar"));
        assert_parse_no_match!(&parser, "foXbar");
        assert_parse_fail!(&parser, "foobaX", ErrMarker, (1, 4, 3));
        assert_parse_fail!(&parser, "foo", ErrMarker, (1, 4, 3));
    }
}
