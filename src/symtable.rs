use crate::types::Types;

use std::collections::HashMap;
use std::vec::Vec;

pub struct SymTable {
    global: HashMap<String, Types>,
    local: Vec<HashMap<String, Types>>,
    owning_proc_types: Vec<Types>,
}

impl SymTable {
    pub fn new() -> SymTable {
        SymTable {
            global: HashMap::new(),
            local: Vec::new(),
            owning_proc_types: Vec::new(),
        }
    }

    pub fn insert(&mut self, k: String, v: Types) -> Result<(), String> {
        if self.local.is_empty() {
            match self.global.get(&k) {
                Some(_) => return Err(String::from("Key already exists")),
                _ => (),
            }
            return self.insert_global(k, v);
        }

        let top = self.local.len() - 1;
        match self.local[top].get(&k) {
            Some(_) => return Err(String::from("Key already exists")),
            _ => (),
        }
        self.local[top].insert(k, v);
        Ok(())
    }

    pub fn insert_global(&mut self, k: String, v: Types) -> Result<(), String> {
        match self.global.get(&k) {
            Some(_) => return Err(String::from("Key already exists")),
            _ => (),
        }
        self.global.insert(k, v);
        Ok(())
    }

    pub fn enter_scope(&mut self, owning_proc_type: Types) {
        self.local.push(HashMap::new());
        self.owning_proc_types.push(owning_proc_type);
    }

    pub fn exit_scope(&mut self) {
        self.local.pop();
        self.owning_proc_types.pop();
    }

    pub fn get(&mut self, k: &String) -> Option<&Types> {
        for t in self.local.iter().rev() {
            match t.get(k) {
                Some(tok) => return Some(tok),
                _ => (),
            }
        }

        return self.global.get(k);
    }

    pub fn get_owning_proc_type(&mut self) -> Types {
        if self.local.is_empty() {
            return Types::Proc(Box::new(Types::Int), Vec::new());
        }

        let top = self.local.len() - 1;
        return self.owning_proc_types[top].clone();
    }
}
