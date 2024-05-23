use crate::types::Types;

use std::collections::HashMap;
use std::vec::Vec;

pub struct SymTable<T, U> {
    global: HashMap<String, T>,
    global_proc_data: U,
    local: Vec<HashMap<String, T>>,
    local_proc_data: Vec<U>,
}

impl<T, U> SymTable<T, U> {
    pub fn new(global_proc_data: U) -> SymTable<T, U> {
        SymTable {
            global: HashMap::new(),
            global_proc_data,
            local: Vec::new(),
            local_proc_data: Vec::new(),
        }
    }

    pub fn insert(&mut self, name: String, val: T) -> Result<(), String> {
        if self.local.is_empty() {
            return self.insert_global(name, val);
        }

        let top = self.local.len() - 1;
        match self.local[top].get(&name) {
            Some(_) => return Err(String::from("Name already exists")),
            _ => (),
        }
        self.local[top].insert(name, val);
        Ok(())
    }

    pub fn insert_global(&mut self, name: String, val: T) -> Result<(), String> {
        match self.global.get(&name) {
            Some(_) => return Err(String::from("Name already exists")),
            _ => (),
        }
        self.global.insert(name, val);
        Ok(())
    }

    pub fn enter_scope(&mut self, local_proc_data: U) {
        self.local.push(HashMap::new());
        self.local_proc_data.push(local_proc_data);
    }

    pub fn exit_scope(&mut self) {
        self.local.pop();
        self.local_proc_data.pop();
    }

    pub fn get(&mut self, name: &String) -> Option<&T> {
        for t in self.local.iter().rev() {
            match t.get(name) {
                Some(val) => return Some(val),
                _ => (),
            }
        }

        self.global.get(name)
    }

    pub fn get_local_proc_data(&mut self) -> &U {
        if self.local.is_empty() {
            return &self.global_proc_data;
        }

        let top = self.local.len() - 1;
        return &self.local_proc_data[top];
    }
}
