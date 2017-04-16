
#[derive( Debug, Clone )]
pub struct DepthCheck<P> {
    parser: P,
}

pub fn depth_check<'src, P>(parser: P) -> DepthCheck<P>
where P: ::Parser<'src> {
    DepthCheck { parser: parser }
}

impl<'src, P> ::Parser<'src> for DepthCheck<P>
where P: ::Parser<'src> {

    type Output = P::Output;
    type Error = P::Error;

    fn parse(&self, input: ::Input<'src>, ctx: ::Context)
    -> ::Outcome<'src, Self::Output, Self::Error> {
        let deeper_ctx = ctx.descend(&input)?;
        self.parser.parse(input, deeper_ctx)
    }
}

#[cfg(test)]
mod tests {
    use parse;

    #[test]
    fn depth_check() {
        let parser = parse::no_custom_error(
            parse::depth_check(parse::depth_check(parse::depth_check(
                parse::exact_str("foo"),
            ))),
        );
        assert_eq!(
            ::apply("foo", ::Options { depth_limit: None }, &parser),
            Ok("foo")
        );
        assert_eq!(
            ::apply("foo", ::Options { depth_limit: Some(10) }, &parser),
            Ok("foo")
        );
        assert_eq!(
            ::apply("foo", ::Options { depth_limit: Some(2) }, &parser),
            Err(::ApplyError::DepthLimit(::Location::new(1, 1, 0), 2))
        );
        assert_eq!(
            ::apply("bar", ::Options { depth_limit: Some(2) }, &parser),
            Err(::ApplyError::DepthLimit(::Location::new(1, 1, 0), 2))
        );
    }
}
