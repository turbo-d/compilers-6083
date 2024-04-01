use crate::token::Token;

use std::collections::HashMap;
use std::vec::Vec;

pub struct SymTable {
    global: HashMap<String, Token>,
    local: Vec<HashMap<String, Token>>,
}

impl SymTable {
    pub fn new() -> SymTable {
        SymTable {
            global: HashMap::new(),
            local: Vec::new(),
        }
    }

    pub fn insert(&mut self, k: String, v: Token) -> Result<(), String> {
        if self.local.is_empty() {
            match self.global.get(&k) {
                Some(_) => return Err(String::from("Key already exists")),
                _ => (),
            }
            self.insert_global(k, v);
            return Ok(());
        }

        let top = self.local.len() - 1;
        match self.local[top].get(&k) {
            Some(_) => return Err(String::from("Key already exists")),
            _ => (),
        }
        self.local[top].insert(k, v);
        Ok(())
    }

    pub fn insert_global(&mut self, k: String, v: Token) {
        self.global.insert(k, v);
    }

    pub fn enter_scope(&mut self) {
        self.local.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.local.pop();
    }

    pub fn get(&mut self, k: &String) -> Option<&Token> {
        for t in self.local.iter().rev() {
            match t.get(k) {
                Some(tok) => return Some(tok),
                _ => (),
            }
        }

        return self.global.get(k);
    }
}
