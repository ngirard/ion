#[macro_use]
extern crate shrinkwraprs;

mod math;
mod modification;
pub mod types;

pub use self::{
    math::{EuclDiv, OpError, Pow},
    modification::Modifications,
};
use itertools::Itertools;
use std::fmt;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ValueKind {
    Alias,
    Array,
    BTreeMap,
    Function,
    HashMap,
    Str,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Value<T> {
    Alias(types::Alias),
    Array(types::Array<T>),
    BTreeMap(types::BTreeMap<T>),
    Function(T),
    HashMap(types::HashMap<T>),
    None,
    Str(types::Str),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ValueRef<'a, T> {
    Alias(types::AliasRef<'a>),
    Array(&'a types::ArrayRef<T>),
    BTreeMap(&'a types::BTreeMap<T>),
    Function(&'a T),
    HashMap(&'a types::HashMap<T>),
    None,
    Str(&'a types::StrRef),
}

impl<'a, T: Clone> ValueRef<'a, T> {
    pub fn to_owned(self) -> Value<T> {
        match self {
            ValueRef::Alias(value) => Value::Alias(types::Alias((*value).into())),
            ValueRef::Array(value) => Value::Array(value.to_owned()),
            ValueRef::BTreeMap(value) => Value::BTreeMap(value.clone()),
            ValueRef::Function(value) => Value::Function(value.clone()),
            ValueRef::HashMap(value) => Value::HashMap(value.to_owned()),
            ValueRef::Str(value) => Value::Str(value.into()),
            ValueRef::None => Value::None,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum ValueRefMut<'a, T> {
    Alias(types::AliasRefMut<'a>),
    Array(&'a mut types::ArrayRef<T>),
    BTreeMap(&'a mut types::BTreeMap<T>),
    Function(&'a mut T),
    HashMap(&'a mut types::HashMap<T>),
    None,
    Str(&'a mut types::StrRef),
}

impl<'a, T: Clone> ValueRefMut<'a, T> {
    pub fn to_owned(self) -> Value<T> {
        match self {
            ValueRefMut::Alias(value) => Value::Alias(types::Alias((&*value.0).into())),
            ValueRefMut::Array(value) => Value::Array(value.to_owned()),
            ValueRefMut::BTreeMap(value) => Value::BTreeMap(value.clone()),
            ValueRefMut::Function(value) => Value::Function(value.clone()),
            ValueRefMut::HashMap(value) => Value::HashMap(value.to_owned()),
            ValueRefMut::Str(value) => Value::Str((&*value).into()),
            ValueRefMut::None => Value::None,
        }
    }
}

// this one’s only special because of the lifetime parameter
impl<'a, T> From<&'a str> for Value<T> {
    fn from(string: &'a str) -> Self { Value::Str(string.into()) }
}

macro_rules! value_from_type {
    ($arg:ident: $from:ty => $variant:ident($inner:expr)) => {
        impl<T> From<$from> for Value<T> {
            fn from($arg: $from) -> Self { Value::$variant($inner) }
        }
    };
}

value_from_type!(string: types::Str => Str(string));
value_from_type!(string: String => Str(string.into()));
value_from_type!(alias: types::Alias => Alias(alias));
value_from_type!(array: types::Array<T> => Array(array));
value_from_type!(hmap: types::HashMap<T> => HashMap(hmap));
value_from_type!(bmap: types::BTreeMap<T> => BTreeMap(bmap));

impl<T> fmt::Display for Value<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Value::Str(ref str_) => write!(f, "{}", str_),
            Value::Alias(ref alias) => write!(f, "{}", **alias),
            Value::Array(ref array) => write!(f, "{}", array.iter().format(" ")),
            Value::HashMap(ref map) => write!(f, "{}", map.values().format(" ")),
            Value::BTreeMap(ref map) => write!(f, "{}", map.values().format(" ")),
            _ => write!(f, ""),
        }
    }
}

#[cfg(test)]
mod trait_test;
