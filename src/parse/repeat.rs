
#[derive( Debug, Clone, Copy )]
enum Minimum {
    Zero,
    One,
}

pub fn repeat0<'src, P>(parser: P) -> RepeatMin<P>
where P: ::Parser<'src> {
    RepeatMin {
        parser: parser,
        min: 0,
    }
}

pub fn repeat1<'src, P>(parser: P) -> RepeatMin<P>
where P: ::Parser<'src> {
    RepeatMin {
        parser: parser,
        min: 1,
    }
}

#[derive( Debug, Clone )]
pub struct RepeatMin<P> {
    parser: P,
    min: usize,
}

impl<'src, P> ::Parser<'src> for RepeatMin<P>
where P: ::Parser<'src> {

    type Output = Vec<P::Output>;
    type Error = P::Error;

    fn parse(&self, mut input: ::Input<'src>, ctx: ::Context)
    -> ::Outcome<'src, Self::Output, Self::Error> {
        let mut items = Vec::new();
        loop {
            let (item, next_input) = match self.parser.parse(input.clone(), ctx.clone()) {
                Ok(res) => res,
                Err(::Fail::NoMatch) => break,
                Err(other) => return Err(other),
            };
            if input.location().offset() == next_input.location().offset() {
                return Err(::Fail::ZeroLengthRepeat(input.location()));
            }
            items.push(item);
            input = next_input;
        }
        if items.len() >= self.min {
            Ok((items, input))
        }
        else {
            Err(::Fail::NoMatch)
        }
    }
}

pub fn fold0<'src, T, P, I, F>(parser: P, init: I, fold: F) -> FoldMin<P, I, F>
where
    P: ::Parser<'src>,
    I: Fn() -> T,
    F: Fn(T, P::Output) -> T,
{
    FoldMin {
        parser: parser,
        init: init,
        fold: fold,
        min: Minimum::Zero,
    }
}

#[derive( Debug, Clone )]
pub struct FoldMin<P, I, F> {
    parser: P,
    init: I,
    fold: F,
    min: Minimum,
}

impl<'src, T, P, I, F> ::Parser<'src> for FoldMin<P, I, F>
where
    P: ::Parser<'src>,
    I: Fn() -> T,
    F: Fn(T, P::Output) -> T,
{
    type Output = T;
    type Error = P::Error;

    fn parse(&self, mut input: ::Input<'src>, ctx: ::Context)
    -> ::Outcome<'src, Self::Output, Self::Error> {
        let mut value = None;
        loop {
            let (item, next_input) = match self.parser.parse(input.clone(), ctx.clone()) {
                Ok(res) => res,
                Err(::Fail::NoMatch) => break,
                Err(other) => return Err(other),
            };
            if input.location().offset() == next_input.location().offset() {
                return Err(::Fail::ZeroLengthRepeat(input.location()));
            }
            value = Some((self.fold)(value.unwrap_or_else(&self.init), item));
            input = next_input;
        }
        match self.min {
            Minimum::Zero => Ok((value.unwrap_or_else(&self.init), input)),
            Minimum::One => match value {
                Some(value) => Ok((value, input)),
                None => Err(::Fail::NoMatch),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use parse;

    #[derive( Debug, Clone, PartialEq, Eq )]
    pub struct ErrMarker;

    #[test]
    fn fold0() {
        let parser = parse::no_custom_error(parse::fold0(
            parse::exact_str("3"),
            || (),
            |_, _| (),
        ));
        let content: String = "335".into();
        let _ = ::apply(&content, ::Options::default(), parser);


        /*let parser = parse::no_custom_error(parse::fold0(
            parse::either(parse::exact_str("3")).or(parse::exact_str("5")),
            move || 1000_i32,
            move |value, new| value + new.parse::<i32>().unwrap(),
        ));*/
//        assert_eq!(
//            result,
 //           Ok(1011)
 //       );
//        assert_parse_ok!(&parser, "335", ());
    }

    #[test]
    fn repeat1() {

        let parser = parse::repeat1(parse::pair(
            parse::exact_str("foo"),
            parse::require(parse::exact_str("bar"), || ErrMarker),
        ));
        assert_parse_ok!(&parser, "foobarfoobarfoobar", vec![
            ("foo", "bar"),
            ("foo", "bar"),
            ("foo", "bar"),
        ]);
        assert_parse_ok!(&parser, "foobar", vec![("foo", "bar")]);
        assert_parse_no_match!(&parser, "");
        assert_parse_fail!(&parser, "fooqux", ErrMarker, (1, 4, 3));
        assert_parse_partial!(&parser, "foobarqux", vec![("foo", "bar")], "qux", (1, 7, 6));

        let parser = parse::no_custom_error(parse::repeat1(parse::exact_str("")));
        assert_eq!(
            ::apply("foo", ::Options::default(), &parser),
            Err(::ApplyError::ZeroLengthRepeat(::Location::new(1, 1, 0)))
        );
    }

    #[test]
    fn repeat0() {

        let parser = parse::repeat0(parse::pair(
            parse::exact_str("foo"),
            parse::require(parse::exact_str("bar"), || ErrMarker),
        ));
        assert_parse_ok!(&parser, "foobarfoobarfoobar", vec![
            ("foo", "bar"),
            ("foo", "bar"),
            ("foo", "bar"),
        ]);
        assert_parse_ok!(&parser, "foobar", vec![("foo", "bar")]);
        assert_parse_ok!(&parser, "", vec![]);
        assert_parse_fail!(&parser, "fooqux", ErrMarker, (1, 4, 3));
        assert_parse_partial!(&parser, "qux", vec![], "qux", (1, 1, 0));

        let parser = parse::no_custom_error(parse::repeat0(parse::exact_str("")));
        assert_eq!(
            ::apply("foo", ::Options::default(), &parser),
            Err(::ApplyError::ZeroLengthRepeat(::Location::new(1, 1, 0)))
        );
    }
}
