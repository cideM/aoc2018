use failure::Error;
use std::fmt::Display;

#[derive(Clone)]
pub struct DayProg<'a, T: Display + ?Sized> {
    pub name: &'static str,
    pub run: fn(&'a str) -> Result<&'a T, Error>,
}
