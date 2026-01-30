use hashbrown::HashMap;

pub struct ModuleInfo {
    pub classes: HashMap<String, ClassInfo>,
    pub functions: HashMap<String, FunctionInfo>,
    pub variables: HashMap<String, VariableInfo>,
}

pub struct ClassInfo {
    pub is_static: bool,
    pub subtypes: Vec<String>,
    pub function_info: HashMap<String, FunctionInfo>,
    pub static_variables: HashMap<String, VariableInfo>,
}

pub struct FunctionInfo {
    pub args_info: HashMap<String, VariableInfo>,
    pub has_all_vars_defined: bool,
    pub has_concrete_return_type: bool,
    pub is_generator: bool,
}

pub struct VariableInfo {
    pub is_mutated: bool,
    pub has_concrete_type: bool,
    pub has_frozen_attrs: bool,
}

impl Default for ModuleInfo {
    fn default() -> Self {
        Self {
            classes: HashMap::new(),
            functions: HashMap::new(),
            variables: HashMap::new(),
        }
    }
}

impl Default for ClassInfo {
    fn default() -> Self {
        Self {
            is_static: true,
            subtypes: Vec::new(),
            function_info: HashMap::new(),
            static_variables: HashMap::new(),
        }
    }
}

impl Default for FunctionInfo {
    fn default() -> Self {
        Self {
            args_info: HashMap::new(),
            has_all_vars_defined: true,
            has_concrete_return_type: true,
            is_generator: false,
        }
    }
}

impl Default for VariableInfo {
    fn default() -> Self {
        Self {
            is_mutated: false,
            has_concrete_type: true,
            has_frozen_attrs: true,
        }
    }
}


