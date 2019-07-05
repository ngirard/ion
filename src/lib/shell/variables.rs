use super::{colors::Colors, flow_control::Function};
use crate::{
    expansion,
    shell::IonError,
    types::{self, Array},
};
use froggy::{Pointer, Storage};
use nix::unistd::{geteuid, gethostname, getpid, getuid};
use scopes::{Namespace, Scope, Scopes};
use std::{env, rc::Rc};
use types_rs::array;
use unicode_segmentation::UnicodeSegmentation;
use xdg::BaseDirectories;

/// Contain a dynamically-typed variable value
pub use types_rs::{Value, ValueKind, ValueRef, ValueRefMut};

/// A pointer to a variable value.
///
/// When this type is dropped, the pointer's value in the `Storage` becomes vacant.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VariablePointer<T> {
    /// A pointer to an alias.
    Alias(Pointer<types::Alias>),
    /// A pointer to an array.
    Array(Pointer<types::Array<T>>),
    /// A pointer to a BTreeMap.
    BTreeMap(Pointer<types::BTreeMap<T>>),
    /// A pointer to a hash map.
    HashMap(Pointer<types::HashMap<T>>),
    /// A pointer to a function.
    Function(Pointer<T>),
    /// A pointer to a string.
    Str(Pointer<types::Str>),
}

/// A structure containing dynamically-typed values organised in scopes
pub struct Variables<'a> {
    cgs:  CompGraphSystem<Rc<Function<'a>>>,
    scopes: Scopes<types::Str, VariablePointer<Rc<Function<'a>>>>,
}

struct CompGraphSystem<T> {
    aliases:    Storage<types::Alias>,
    arrays:     Storage<types::Array<T>>,
    btree_maps: Storage<types::BTreeMap<T>>,
    functions:  Storage<T>,
    hash_maps:  Storage<types::HashMap<T>>,
    strings:    Storage<types::Str>,
}

impl<T: Default> CompGraphSystem<T> {
    fn new() -> Self {
        Self {
            aliases:    Storage::with_capacity(32),
            arrays:     Storage::with_capacity(32),
            btree_maps: Storage::with_capacity(32),
            functions:  Storage::with_capacity(32),
            hash_maps:  Storage::with_capacity(32),
            strings:    Storage::with_capacity(32),
        }
    }

    fn get<'a>(&'a self, ptr: &VariablePointer<T>) -> ValueRef<'a, T> {
        match ptr {
            VariablePointer::Str(ptr) => ValueRef::Str(&self.strings[ptr]),
            VariablePointer::Array(ptr) => ValueRef::Array(&self.arrays[ptr]),
            VariablePointer::BTreeMap(ptr) => ValueRef::BTreeMap(&self.btree_maps[ptr]),
            VariablePointer::HashMap(ptr) => ValueRef::HashMap(&self.hash_maps[ptr]),
            VariablePointer::Function(ptr) => ValueRef::Function(&self.functions[ptr]),
            VariablePointer::Alias(ptr) => ValueRef::Alias((&self.aliases[ptr]).into()),
        }
    }

    fn get_mut<'a>(&'a mut self, ptr: &VariablePointer<T>) -> ValueRefMut<'a, T> {
        match ptr {
            VariablePointer::Str(ptr) => ValueRefMut::Str(&mut self.strings[ptr]),
            VariablePointer::Array(ptr) => ValueRefMut::Array(&mut self.arrays[ptr]),
            VariablePointer::BTreeMap(ptr) => ValueRefMut::BTreeMap(&mut self.btree_maps[ptr]),
            VariablePointer::HashMap(ptr) => ValueRefMut::HashMap(&mut self.hash_maps[ptr]),
            VariablePointer::Function(ptr) => ValueRefMut::Function(&mut self.functions[ptr]),
            VariablePointer::Alias(ptr) => ValueRefMut::Alias((&mut self.aliases[ptr]).into()),
        }
    }

    fn remove(&mut self, ptr: &VariablePointer<T>) -> Value<T> {
        use std::mem::swap;
        match ptr {
            VariablePointer::Str(ptr) => {
                let mut temp = types::Str::default();
                swap(&mut temp, &mut self.strings[ptr]);
                Value::Str(temp)
            },
            VariablePointer::Array(ptr) => {
                let mut temp = types::Array::default();
                swap(&mut temp, &mut self.arrays[ptr]);
                Value::Array(temp)
            },
            VariablePointer::BTreeMap(ptr) => {
                let mut temp = types::BTreeMap::default();
                swap(&mut temp, &mut self.btree_maps[ptr]);
                Value::BTreeMap(temp)
            },
            VariablePointer::HashMap(ptr) => {
                let mut temp = types::HashMap::default();
                swap(&mut temp, &mut self.hash_maps[ptr]);
                Value::HashMap(temp)
            },
            VariablePointer::Function(ptr) => {
                let mut temp = T::default();
                swap(&mut temp, &mut self.functions[ptr]);
                Value::Function(temp)
            },
            VariablePointer::Alias(ptr) => {
                let mut temp = types::Alias::default();
                swap(&mut temp, &mut self.aliases[ptr]);
                Value::Alias(temp)
            },
        }
    }

    fn update(&mut self, ptr: &mut VariablePointer<T>, value: Value<T>) {
        *ptr = self.insert(value);
    }

    fn insert(&mut self, value: Value<T>) -> VariablePointer<T> {
        match value {
            Value::Str(value) => {
                VariablePointer::Str(self.strings.create(value))
            }
            Value::Array(value) => {
                VariablePointer::Array(self.arrays.create(value))
            }
            Value::Function(value) => {
                VariablePointer::Function(self.functions.create(value))
            }
            Value::HashMap(value) => {
               VariablePointer::HashMap(self.hash_maps.create(value))
            }
            Value::BTreeMap(value) => {
                VariablePointer::BTreeMap(self.btree_maps.create(value))
            }
            Value::Alias(value) => {
                VariablePointer::Alias(self.aliases.create(value))
            }
            Value::None => panic!("inserting a null value"),
        }
    }
}

impl<'a> Variables<'a> {
    /// Get all strings
    pub fn string_vars(&self) -> impl Iterator<Item = (&types::Str, &types::Str)> {
        let values = &self.cgs.strings;
        self.scopes
            .scopes()
            .rev()
            .flat_map(|map| map.iter())
            .filter_map(move |(key, ptr)| match ptr {
                VariablePointer::Str(ptr) => Some((key, &values[ptr])),
                _ => None
            })
    }

    /// Get all aliases
    pub fn aliases(&self) -> impl Iterator<Item = (&types::Str, &types::Str)> {
        let values = &self.cgs.aliases;
        self.scopes
            .scopes()
            .rev()
            .flat_map(|map| map.iter())
            .filter_map(move |(key, ptr)| match ptr {
                VariablePointer::Alias(ptr) => Some((key, &*values[ptr])),
                _ => None
            })
    }

    /// Get all the functions
    pub fn functions(&self) -> impl Iterator<Item = (&types::Str, &Rc<Function<'a>>)> {
        let values = &self.cgs.functions;
        self.scopes
            .scopes()
            .rev()
            .flat_map(|map| map.iter())
            .filter_map(move |(key, ptr)| match ptr {
                VariablePointer::Function(ptr) => Some((key, &values[ptr])),
                _ => None
            })
    }

    /// Get all the array values
    pub fn arrays(&self) -> impl Iterator<Item = (&types::Str, &types::Array<Rc<Function<'a>>>)> {
        let values = &self.cgs.arrays;
        self.scopes
            .scopes()
            .rev()
            .flat_map(|map| map.iter())
            .filter_map(move |(key, ptr)| match ptr {
                VariablePointer::Array(ptr) => Some((key, &values[ptr])),
                _ => None
            })
    }

    /// Create a new scope. If namespace is true, variables won't be droppable across the scope
    /// boundary
    pub fn new_scope(&mut self, namespace: bool) { self.scopes.new_scope(namespace) }

    /// Exit the current scope
    pub fn pop_scope(&mut self) {
        self.scopes.pop_scope();
    }

    pub(crate) fn pop_scopes<'b>(
        &'b mut self,
        index: usize,
    ) -> impl Iterator<Item = Scope<types::Str, VariablePointer<Rc<Function<'a>>>>> + 'b {
        self.scopes.pop_scopes(index)
    }

    pub(crate) fn append_scopes(&mut self, scopes: Vec<Scope<types::Str, VariablePointer<Rc<Function<'a>>>>>) {
        self.scopes.append_scopes(scopes)
    }

    pub(crate) fn index_scope_for_var(&self, name: &str) -> Option<usize> {
        self.scopes.index_scope_for_var(name)
    }

    /// Set a variable to a value in the current scope. If a variable already exists in a writable
    /// scope, it is updated, else a new variable is created in the current scope, possibly
    /// shadowing other variables
    pub fn set<T: Into<Value<Rc<Function<'a>>>>>(&mut self, name: &str, value: T) {
        let value = value.into();
        match self.scopes.get_mut(name) {
            Some(ptr) => {
                self.cgs.update(ptr, value);
            }
            None => {
                self.scopes.set(name, self.cgs.insert(value));
            }
        }
    }

    /// Obtains the value for the **MWD** variable.
    ///
    /// Further minimizes the directory path in the same manner that Fish does by default.
    /// That is, if more than two parents are visible in the path, all parent directories
    /// of the current directory will be reduced to a single character.
    fn get_minimal_directory(&self) -> types::Str {
        let swd = self.get_simplified_directory();

        {
            // Temporarily borrow the `swd` variable while we attempt to assemble a minimal
            // variant of the directory path. If that is not possible, we will cancel the
            // borrow and return `swd` itself as the minified path.
            let elements = swd.split('/').filter(|s| !s.is_empty()).collect::<Vec<&str>>();
            if elements.len() > 2 {
                let mut output = types::Str::new();
                for element in &elements[..elements.len() - 1] {
                    let mut segmenter = UnicodeSegmentation::graphemes(*element, true);
                    let grapheme = segmenter.next().unwrap();
                    output.push_str(grapheme);
                    if grapheme == "." {
                        output.push_str(segmenter.next().unwrap());
                    }
                    output.push('/');
                }
                output.push_str(elements[elements.len() - 1]);
                return output;
            }
        }

        swd
    }

    /// Obtains the value for the **SWD** variable.
    ///
    /// Useful for getting smaller prompts, this will produce a simplified variant of the
    /// working directory which the leading `HOME` prefix replaced with a tilde character.
    fn get_simplified_directory(&self) -> types::Str {
        let home = self.get_str("HOME").unwrap_or_else(|_| "?".into());
        env::var("PWD").unwrap().replace(&*home, "~").into()
    }

    /// Indicates if the name is valid for a variable
    pub fn is_valid_variable_name(name: &str) -> bool {
        name.chars().all(Variables::is_valid_variable_character)
    }

    fn is_valid_variable_character(c: char) -> bool {
        c.is_alphanumeric() || c == '_' || c == '?' || c == '.' || c == '-' || c == '+'
    }

    /// Remove a variable from the current scope. If the value can't be removed (it is outside a
    /// function or does not exist), returns None
    pub fn remove(&mut self, name: &str) -> Option<Value<Rc<Function<'a>>>> {
        if name.starts_with("super::") || name.starts_with("global::") {
            // Cannot mutate outer namespace
            return None;
        }

        let world = &mut self.cgs;
        self.scopes.remove_variable(name)
            .map(move |ptr| world.remove(&ptr))
    }

    /// Get the first variable that matches the name, and returns it as a function if it was a
    /// function.
    pub fn get_func(&self, name: &str) -> Option<&Rc<Function<'a>>> {
        let world = &self.cgs;
        self.get_ptr(name).and_then(move |ptr| match ptr {
            VariablePointer::Function(ptr) => Some(&world.functions[ptr]),
            _ => None
        })
    }

    /// Get the string value associated with a name on the current scope. This includes fetching
    /// env vars, colors & hexes and some extra values like MWD and SWD
    pub fn get_str(&self, name: &str) -> expansion::Result<types::Str, IonError> {
        use expansion::Error;
        match name {
            "MWD" => return Ok(self.get_minimal_directory()),
            "SWD" => return Ok(self.get_simplified_directory()),
            _ => (),
        }

        // If the parsed name contains the '::' pattern, then a namespace was
        // designated. Find it.
        match name.find("::").map(|pos| (&name[..pos], &name[pos + 2..])) {
            Some(("c", variable)) | Some(("color", variable)) => {
                Ok(Colors::collect(variable)?.to_string().into())
            }
            Some(("x", variable)) | Some(("hex", variable)) => {
                let c = u8::from_str_radix(variable, 16)
                    .map_err(|cause| Error::InvalidHex(variable.into(), cause))?;
                Ok((c as char).to_string().into())
            }
            Some(("env", variable)) => {
                env::var(variable).map(Into::into).map_err(|_| Error::UnknownEnv(variable.into()))
            }
            Some(("super", _)) | Some(("global", _)) | None => {
                // Otherwise, it's just a simple variable name.
                match self.get(name) {
                    Some(ValueRef::Str(val)) => Ok(val.into()),
                    _ => env::var(name).map(Into::into).map_err(|_| Error::VarNotFound),
                }
            }
            Some((..)) => Err(Error::UnsupportedNamespace(name.into())),
        }
    }

    /// Get a variable on the current scope
    pub fn get<'b>(&'b self, name: &str) -> Option<ValueRef<'b, Rc<Function<'a>>>> {
        let world = &self.cgs;
        self.get_ptr(name).map(move |ptr| world.get(ptr))
    }

    /// Get a mutable access to a variable on the current scope
    pub fn get_mut(&mut self, name: &str) -> Option<ValueRefMut<Rc<Function<'a>>>> {
        if name.starts_with("super::") || name.starts_with("global::") {
            // Cannot mutate outer namespace
            return None;
        }

        let world = &mut self.cgs;
        self.scopes.get_mut(name).map(move |ptr| world.get_mut(ptr))
    }

    /// Returns true if the variable in this scope is a function.
    pub fn is_func(&self, name: &str) -> bool {
        self.get_ptr(name).map_or(false, |e| if let VariablePointer::Function(_) = e { true } else { false })
    }

    fn get_ptr(&self, mut name: &str) -> Option<&VariablePointer<Rc<Function<'a>>>> {
        const GLOBAL_NS: &str = "global::";
        const SUPER_NS: &str = "super::";

        let namespace = if name.starts_with(GLOBAL_NS) {
            name = &name[GLOBAL_NS.len()..];
            // Go up as many namespaces as possible
            Namespace::Global
        } else if name.starts_with(SUPER_NS) {
            let mut up = 0;
            while name.starts_with(SUPER_NS) {
                name = &name[SUPER_NS.len()..];
                up += 1;
            }

            Namespace::Specific(up)
        } else {
            Namespace::Any
        };

        self.scopes.get(name, namespace)
    }
}

impl<'a> Default for Variables<'a> {
    fn default() -> Self {
        let mut map = Variables { cgs: CompGraphSystem::new(), scopes: Scopes::with_capacity(64) };

        map.set("HISTORY_SIZE", "1000");
        map.set("HISTFILE_SIZE", "100000");
        map.set(
            "PROMPT",
            "${x::1B}]0;${USER}: \
             ${PWD}${x::07}${c::0x55,bold}${USER}${c::default}:${c::0x4B}${SWD}${c::default}# \
             ${c::reset}",
        );

        // Set the PID, UID, and EUID variables.
        map.set("PID", Value::Str(getpid().to_string().into()));
        map.set("UID", Value::Str(getuid().to_string().into()));
        map.set("EUID", Value::Str(geteuid().to_string().into()));

        // Initialize the HISTFILE variable
        if let Ok(base_dirs) = BaseDirectories::with_prefix("ion") {
            if let Ok(path) = base_dirs.place_data_file("history") {
                map.set("HISTFILE", path.to_str().unwrap_or("?"));
                map.set("HISTFILE_ENABLED", "1");
            }
        }

        // History Timestamps enabled variable, disabled by default
        map.set("HISTORY_TIMESTAMP", "0");

        map.set("HISTORY_IGNORE", array!["no_such_command", "whitespace", "duplicates"]);

        map.set("CDPATH", Array::new());

        // Initialize the HOME variable
        dirs::home_dir().map_or_else(
            || env::set_var("HOME", "?"),
            |path| env::set_var("HOME", path.to_str().unwrap_or("?")),
        );

        // Initialize the HOST variable
        let mut host_name = [0_u8; 512];
        env::set_var(
            "HOST",
            &gethostname(&mut host_name)
                .ok()
                .map_or("?", |hostname| hostname.to_str().unwrap_or("?")),
        );

        map
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::{
        expansion::{Expander, Result, Select},
        shell::IonError,
    };
    use serial_test_derive::serial;

    pub struct VariableExpander<'a>(pub Variables<'a>);

    impl<'a> Expander for VariableExpander<'a> {
        type Error = IonError;

        fn string(&self, var: &str) -> Result<types::Str, IonError> { self.0.get_str(var) }

        fn array(
            &self,
            _variable: &str,
            _selection: &Select<types::Str>,
        ) -> Result<types::Args, Self::Error> {
            Err(expansion::Error::VarNotFound)
        }

        fn command(&self, cmd: &str) -> Result<types::Str, Self::Error> { Ok(cmd.into()) }

        fn tilde(&self, input: &str) -> Result<types::Str, Self::Error> { Ok(input.into()) }

        fn map_keys(
            &self,
            _name: &str,
            _select: &Select<types::Str>,
        ) -> Result<types::Args, Self::Error> {
            Err(expansion::Error::VarNotFound)
        }

        fn map_values(
            &self,
            _name: &str,
            _select: &Select<types::Str>,
        ) -> Result<types::Args, Self::Error> {
            Err(expansion::Error::VarNotFound)
        }
    }

    #[test]
    fn undefined_variable_errors() {
        let variables = Variables::default();
        assert!(VariableExpander(variables).expand_string("$FOO").is_err());
    }

    #[test]
    fn set_var_and_expand_a_variable() {
        let mut variables = Variables::default();
        variables.set("FOO", "BAR");
        let expanded = VariableExpander(variables).expand_string("$FOO").unwrap().join("");
        assert_eq!("BAR", &expanded);
    }

    #[test]
    #[serial]
    fn minimal_directory_var_should_compact_path() {
        let variables = Variables::default();
        env::set_var("PWD", "/var/log/nix");
        assert_eq!(
            types::Str::from("v/l/nix"),
            variables.get_str("MWD").expect("no value returned"),
        );
    }

    #[test]
    #[serial]
    fn minimal_directory_var_shouldnt_compact_path() {
        let variables = Variables::default();
        env::set_var("PWD", "/var/log");
        assert_eq!(
            types::Str::from("/var/log"),
            variables.get_str("MWD").expect("no value returned"),
        );
    }
}
