
use std::marker;
use std::str;

use util;

pub fn exact_str<'cmp, E>(content: &'cmp str) -> ExactStr<'cmp, E> {
    ExactStr {
        content: content,
        _error: marker::PhantomData,
    }
}

#[derive( Debug, Clone )]
pub struct ExactStr<'cmp, E> {
    content: &'cmp str,
    _error: marker::PhantomData<E>,
}

impl<'cmp, 'src, E> ::Parser<'src> for ExactStr<'cmp, E> {

    type Output = &'src str;
    type Error = E;

    fn parse(&self, input: ::Input<'src>, _ctx: ::Context)
    -> ::Outcome<'src, Self::Output, Self::Error> {
        util::option_to_outcome(
            input.consume_len_via(|rest| {
                if rest.starts_with(self.content) {
                    Some(self.content.len())
                }
                else {
                    None
                }
            }),
            |res| res,
        )
    }
}

pub fn any_char<E>() -> AnyChar<E> {
    AnyChar {
        _error: marker::PhantomData,
    }
}

#[derive( Debug, Clone )]
pub struct AnyChar<E> {
    _error: marker::PhantomData<E>,
}

impl<'src, E> ::Parser<'src> for AnyChar<E> {

    type Output = char;
    type Error = E;

    fn parse(&self, input: ::Input<'src>, _ctx: ::Context)
    -> ::Outcome<'src, Self::Output, Self::Error> {
        util::option_to_outcome(
            input.consume_char(),
            |res| res,
        )
    }
}

#[cfg(test)]
mod tests {
    use parse;

    #[test]
    fn any_char() {
        let parser = parse::no_custom_error(parse::any_char());
        assert_parse_ok!(&parser, "x", 'x');
        assert_parse_no_match!(&parser, "");
    }

    #[test]
    fn exact_str() {
        let parser = parse::no_custom_error(parse::exact_str("foo"));
        assert_parse_ok!(&parser, "foo", "foo");
        assert_parse_no_match!(&parser, "foX");
    }
}

