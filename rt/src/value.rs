use crate::{gc::Gc, Runtime};
use std::{fmt::Display, hash::Hash, ptr};

pub static NONE_SYMBOL_PTR_TARGET: &str = "none";
pub fn none() -> Value {
    Value::Symbol(Symbol(&NONE_SYMBOL_PTR_TARGET))
}

pub static SOME_SYMBOL_PTR_TARGET: &str = "some";
pub fn some() -> Value {
    Value::Symbol(Symbol(&SOME_SYMBOL_PTR_TARGET))
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Symbol(pub &'static &'static str);

impl Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ptr::from_ref(self.0).hash(state);
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

#[rustfmt::skip]
#[repr(C)]
#[derive(Clone, Copy)]
pub union FnPtr {
    pub f0: extern "C" fn(&Runtime) -> Value,
    pub f1: extern "C" fn(&Runtime, Value) -> Value,
    pub f2: extern "C" fn(&Runtime, Value, Value) -> Value,
    pub f3: extern "C" fn(&Runtime, Value, Value, Value) -> Value,
    pub f4: extern "C" fn(&Runtime, Value, Value, Value, Value) -> Value,
    pub f5: extern "C" fn(&Runtime, Value, Value, Value, Value, Value) -> Value,
    pub f6: extern "C" fn(&Runtime, Value, Value, Value, Value, Value, Value) -> Value,
    pub f7: extern "C" fn(&Runtime, Value, Value, Value, Value, Value, Value, Value) -> Value,
    pub f8: extern "C" fn(&Runtime, Value, Value, Value, Value, Value, Value, Value, Value) -> Value,
    pub f9: extern "C" fn(&Runtime, Value, Value, Value, Value, Value, Value, Value, Value, Value) -> Value,
    pub f10: extern "C" fn(&Runtime, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value) -> Value,
    pub f11: extern "C" fn(&Runtime, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value) -> Value,
    pub f12: extern "C" fn(&Runtime, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value) -> Value,
    pub f13: extern "C" fn(&Runtime, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value) -> Value,
    pub f14: extern "C" fn(&Runtime, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value) -> Value,
    pub f15: extern "C" fn(&Runtime, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value, Value) -> Value,
}

static_assertions::assert_eq_size!(FnPtr, usize);

impl PartialEq for FnPtr {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.f0 == other.f0 }
    }
}

impl Hash for FnPtr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        unsafe { self.f0 }.hash(state);
    }
}

// pub type FnPtr = extern "C" fn(runtime: &Runtime, args: ...) -> Value;

#[derive(Clone, Copy, PartialEq, Hash)]
pub struct Closure {
    pub fn_ptr: FnPtr,
    pub env: Value,
}

pub type List = Option<Gc<ListCons>>;

pub fn cons(runtime: &mut Runtime, head: Value, tail: List) -> List {
    Some(Gc::new(runtime, ListCons(head, tail)))
}

#[derive(Clone, Copy, PartialEq, Hash)]
pub struct ListCons(pub Value, pub List);

static_assertions::assert_eq_size!(List, usize);

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq)]
pub struct HFloat(pub f64);
impl Hash for HFloat {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Ref(pub Gc<Value>);

impl Hash for Ref {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.ptr.hash(state);
    }
}

impl PartialEq for Ref {
    fn eq(&self, other: &Self) -> bool {
        self.0.ptr == other.0.ptr
    }
}

#[repr(C)]
#[derive(Clone, Copy, PartialEq, Hash)]
pub enum Value {
    Fn(FnPtr),

    List(List),
    Tuple(Gc<Vec<Value>>),
    Str(Gc<String>),
    Closure(Gc<Closure>),

    Int(i64),
    Float(HFloat),
    Boolean(bool),

    Ref(Ref),

    Symbol(Symbol),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Fn(_) => write!(f, "<fn>"),
            Value::List(mut list) => {
                write!(f, "[")?;
                while let Some(v) = list {
                    write!(f, "{}", v.0)?;
                    list = v.1;
                    if list.is_some() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Value::Tuple(gc) => {
                write!(f, "(")?;
                for (i, value) in gc.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, ")")
            }
            Value::Str(gc) => write!(f, "{}", gc.as_ref()),
            Value::Closure(_gc) => write!(f, "<closure>"),
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(HFloat(hfloat)) => write!(f, "{}", hfloat),
            Value::Boolean(v) => write!(f, "{}", v),
            Value::Symbol(symbol) => write!(f, "'{}", symbol.0),
            Value::Ref(Ref(gc)) => write!(f, "ref({})", gc.as_ref()),
        }
    }
}

static_assertions::assert_eq_size!(Value, (usize, usize));
