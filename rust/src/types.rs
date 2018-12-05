use failure::Error;
use std::fmt::Display;

#[derive(Clone)]
pub struct DayProg<T: Display + Sized> {
    pub name: &'static str,
    pub run: fn(&str) -> Result<T, Error>,
}
