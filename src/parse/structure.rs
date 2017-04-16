
use std::marker;

pub fn delimited<'src, PL, PC, PR>(parser_l: PL, parser_c: PC, parser_r: PR)
-> Delimited<PL, PC, PR>
where
    PL: ::Parser<'src>,
    PC: ::Parser<'src, Error=PL::Error>,
    PR: ::Parser<'src, Error=PL::Error>,
{
    Delimited {
        parser_l: parser_l,
        parser_c: parser_c,
        parser_r: parser_r,
    }
}

#[derive( Debug, Clone )]
pub struct Delimited<PL, PC, PR> {
    parser_l: PL,
    parser_c: PC,
    parser_r: PR,
}

impl<'src, PL, PC, PR> ::Parser<'src> for Delimited<PL, PC, PR> 
where
    PL: ::Parser<'src>,
    PC: ::Parser<'src, Error=PL::Error>,
    PR: ::Parser<'src, Error=PL::Error>,
{
    type Output = PC::Output;
    type Error = PC::Error;

    fn parse(&self, input: ::Input<'src>, ctx: ::Context)
    -> ::Outcome<'src, Self::Output, Self::Error> {
        let (_, input) = self.parser_l.parse(input, ctx.clone())?;
        let (res, input) = self.parser_c.parse(input, ctx.clone())?;
        let (_, input) = self.parser_r.parse(input, ctx)?;
        Ok((res, input))
    }
}

pub fn pair<'src, PL, PR>(parser_l: PL, parser_r: PR)
-> Pair<PL, PR>
where
    PL: ::Parser<'src>,
    PR: ::Parser<'src, Error=PL::Error>,
{
    Pair {
        parser_l: parser_l,
        parser_r: parser_r,
    }
}

#[derive( Debug, Clone )]
pub struct Pair<PL, PR> {
    parser_l: PL,
    parser_r: PR,
}

impl<'src, PL, PR> ::Parser<'src> for Pair<PL, PR> 
where
    PL: ::Parser<'src>,
    PR: ::Parser<'src, Error=PL::Error>,
{
    type Output = (PL::Output, PR::Output);
    type Error = PL::Error;

    fn parse(&self, input: ::Input<'src>, ctx: ::Context)
    -> ::Outcome<'src, Self::Output, Self::Error> {
        let (res_l, input) = self.parser_l.parse(input, ctx.clone())?;
        let (res_r, input) = self.parser_r.parse(input, ctx)?;
        Ok(((res_l, res_r), input))
    }
}

pub fn either<'src, P>(parser: P) -> Either<P>
where P: ::Parser<'src> {
    Either { parser: parser }
}

#[derive( Debug, Clone )]
pub struct Either<P> {
    parser: P,
}

impl<'src, P> Either<P> where P: ::Parser<'src> {

    pub fn or<PO>(self, other: PO) -> EitherOr<Self, PO> {
        EitherOr {
            parser1: self,
            parser2: other,
        }
    }
}

impl<'src, P> ::Parser<'src> for Either<P>
where P: ::Parser<'src> {

    type Output = P::Output;
    type Error = P::Error;

    fn parse(&self, input: ::Input<'src>, ctx: ::Context)
    -> ::Outcome<'src, Self::Output, Self::Error> {
        self.parser.parse(input, ctx)
    }
}

#[derive( Debug, Clone )]
pub struct EitherOr<P1, P2> {
    parser1: P1,
    parser2: P2,
}

impl<'src, P1, P2> EitherOr<P1, P2>
where
    P1: ::Parser<'src>,
    P2: ::Parser<'src, Output=P1::Output, Error=P1::Error>,
{
    pub fn or<PO>(self, other: PO) -> EitherOr<Self, PO> {
        EitherOr {
            parser1: self,
            parser2: other,
        }
    }
}

impl<'src, P1, P2> ::Parser<'src> for EitherOr<P1, P2>
where
    P1: ::Parser<'src>,
    P2: ::Parser<'src, Output=P1::Output, Error=P1::Error>,
{
    type Output = P1::Output;
    type Error = P1::Error;

    fn parse(&self, input: ::Input<'src>, ctx: ::Context)
    -> ::Outcome<'src, Self::Output, Self::Error> {
        match self.parser1.parse(input.clone(), ctx.clone()) {
            Err(::Fail::NoMatch) => (),
            other => return other,
        }
        self.parser2.parse(input, ctx)
    }
}

pub fn optional<'src, P>(parser: P) -> Optional<P>
where P: ::Parser<'src> {
    Optional { parser: parser }
}

#[derive( Debug, Clone )]
pub struct Optional<P> {
    parser: P,
}

impl<'src, P> ::Parser<'src> for Optional<P>
where P: ::Parser<'src> {
    
    type Output = Option<P::Output>;
    type Error = P::Error;

    fn parse(&self, input: ::Input<'src>, ctx: ::Context)
    -> ::Outcome<'src, Self::Output, Self::Error> {
        match self.parser.parse(input.clone(), ctx) {
            Ok((value, input)) => Ok((Some(value), input)),
            Err(::Fail::NoMatch) => Ok((None, input)),
            Err(other) => Err(other),
        }
    }
}

pub fn current_location<E>() -> CurrentLocation<E> {
    CurrentLocation {
        _error: marker::PhantomData,
    }
}

#[derive( Debug, Clone )]
pub struct CurrentLocation<E> {
    _error: marker::PhantomData<E>,
}

impl<'src, E> ::Parser<'src> for CurrentLocation<E> {
    
    type Output = ::Location;
    type Error = E;

    fn parse(&self, input: ::Input<'src>, _ctx: ::Context)
    -> ::Outcome<'src, Self::Output, Self::Error> {
        let location = input.location();
        Ok((location, input))
    }
}

#[cfg(test)]
mod tests {
    use parse;

    #[derive( Debug, Clone, PartialEq, Eq )]
    pub struct ErrMarker;

    #[derive( Debug, Clone, PartialEq, Eq )]
    pub enum MultiErrMarker {
        Mark1,
        Mark2,
    }

    #[test]
    fn current_location() {
        let parser = parse::no_custom_error(parse::pair(
            parse::exact_str("foo"),
            parse::current_location(),
        ));
        assert_parse_ok!(&parser, "foo", ("foo", ::Location::new(1, 4, 3)));
        assert_parse_no_match!(&parser, "");
    }

    #[test]
    fn optional() {

        let parser = parse::no_custom_error(parse::optional(parse::exact_str("foo")));
        assert_parse_ok!(&parser, "foo", Some("foo"));
        assert_parse_ok!(&parser, "", None);

        let parser = parse::pair(
            parse::optional(parse::exact_str("foo")),
            parse::require(parse::exact_str("bar"), || ErrMarker),
        );
        assert_parse_ok!(&parser, "foobar", (Some("foo"), "bar"));
        assert_parse_ok!(&parser, "bar", (None, "bar"));
        assert_parse_fail!(&parser, "qux", ErrMarker, (1, 1, 0));
    }

    #[test]
    fn either() {

        let parser = parse::either(parse::pair(
            parse::exact_str("foo"),
            parse::require(parse::exact_str("bar"), || ErrMarker),
        ));
        assert_parse_ok!(&parser, "foobar", ("foo", "bar"));
        assert_parse_no_match!(&parser, "qux");
        assert_parse_fail!(&parser, "fooqux", ErrMarker, (1, 4, 3));

        let parser =
            parse::either(parse::pair(
                parse::exact_str("foo"),
                parse::require(parse::exact_str("bar"), || MultiErrMarker::Mark1),
            ))
            .or(parse::pair(
                parse::exact_str("baz"),
                parse::require(parse::exact_str("qux"), || MultiErrMarker::Mark2),
            ));
        assert_parse_ok!(&parser, "foobar", ("foo", "bar"));
        assert_parse_ok!(&parser, "bazqux", ("baz", "qux"));
        assert_parse_no_match!(&parser, "fnord");
        assert_parse_fail!(&parser, "fooqux", MultiErrMarker::Mark1, (1, 4, 3));
        assert_parse_fail!(&parser, "bazbar", MultiErrMarker::Mark2, (1, 4, 3));
    }

    #[test]
    fn pair() {
        let parser = parse::no_custom_error(parse::pair(
            parse::exact_str("foo"),
            parse::exact_str("bar"),
        ));
        assert_parse_ok!(&parser, "foobar", ("foo", "bar"));
        assert_parse_no_match!(&parser, "foXbar");
        assert_parse_no_match!(&parser, "foobaX");
    }

    #[test]
    fn delimited() {
        let parser = parse::no_custom_error(parse::delimited(
            parse::exact_str("("),
            parse::exact_str("foo"),
            parse::exact_str(")"),
        ));
        assert_parse_ok!(&parser, "(foo)", "foo");
        assert_parse_no_match!(&parser, "X");
        assert_parse_no_match!(&parser, "(");
        assert_parse_no_match!(&parser, "(X");
        assert_parse_no_match!(&parser, "(foo");
        assert_parse_no_match!(&parser, "(fooX");
    }
}
