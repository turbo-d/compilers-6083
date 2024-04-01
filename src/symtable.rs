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

    pub fn insert(&mut self, k: String, v: Token) {
        if self.local.is_empty() {
            self.insert_global(k, v);
            return;
        }

        let top = self.local.len() - 1;
        self.local[top].insert(k, v);
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
