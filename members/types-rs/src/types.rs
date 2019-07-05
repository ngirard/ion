use super::Value;
use hashbrown::HashMap as HashbrownMap;
use small;
use std::{
    collections::BTreeMap as StdBTreeMap,
    iter::FromIterator,
};

pub type Array<T> = Vec<Value<T>>;
pub type ArrayRef<T> = [Value<T>];
pub type HashMap<T> = HashbrownMap<Str, Value<T>>;
pub type BTreeMap<T> = StdBTreeMap<Str, Value<T>>;
pub type Str = small::String;
pub type StrRef = str;

#[derive(Clone, Debug, PartialEq, Hash, Eq, Default, Shrinkwrap)]
pub struct Alias(pub Str);

#[derive(Clone, Debug, PartialEq, Hash, Eq, Default, Shrinkwrap)]
pub struct AliasRef<'a>(pub &'a StrRef);

#[derive(Debug, PartialEq, Hash, Eq, Default, Shrinkwrap)]
pub struct AliasRefMut<'a>(pub &'a mut StrRef);

impl<'a> From<&'a mut Alias> for AliasRefMut<'a> {
    fn from(alias: &'a mut Alias) -> Self { AliasRefMut(alias.0.as_mut_str()) }
}

impl<'a> From<&'a Alias> for AliasRef<'a> {
    fn from(alias: &'a Alias) -> Self { AliasRef(&**alias) }
}

impl<T> FromIterator<Value<T>> for Value<T> {
    fn from_iter<I: IntoIterator<Item = Value<T>>>(items: I) -> Self {
        Value::Array(items.into_iter().collect())
    }
}

/// Construct a new Array containing the given arguments
///
/// `array!` acts like the standard library's `vec!` macro, and can be thought
/// of as a shorthand for:
/// ```ignore,rust
/// Array::from_vec(vec![...])
/// ```
/// Additionally it will call `Into::into` on each of its members so that one
/// can pass in any type with some `To<SmallString>` implementation; they will
/// automatically be converted to owned SmallStrings.
/// ```ignore,rust
/// let verbose = Array::from_vec(vec![
///     "foo".into(),
///     "bar".into(),
///     "baz".into(),
///     "zar".into(),
///     "doz".into(),
/// ]);
/// let concise = array!["foo", "bar", "baz", "zar", "doz"];
/// assert_eq!(verbose, concise);
/// ```
#[macro_export]
macro_rules! array [
    ( $($x:expr), *) => ({
        let mut _arr = crate::types::Array::new();
        $(_arr.push($x.into());)*
        _arr
    })
];
