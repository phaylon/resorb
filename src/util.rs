
pub fn option_to_outcome<'src, T, U, E, F>(opt: Option<T>, mapper: F)
-> ::Outcome<'src, U, E>
where F: FnOnce(T) -> (U, ::Input<'src>) {
    match opt {
        None => Err(::Fail::NoMatch),
        Some(value) => Ok(mapper(value)),
    }
}

pub fn fail<E>(location: ::Location, error: E) -> ::Fail<E> {
    ::Fail::Error(::Error::new(location, error))
}
