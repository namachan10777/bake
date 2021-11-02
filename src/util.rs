pub fn inside_out<T, E>(x: Result<Option<T>, E>) -> Option<Result<T, E>> {
    match x {
        Ok(Some(x)) => Some(Ok(x)),
        Ok(None) => None,
        Err(e) => Some(Err(e)),
    }
}
