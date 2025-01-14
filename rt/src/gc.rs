use crate::value::{Closure, List, ListCons, Ref, Value};
use crate::Runtime;
use std::{
    hash::{Hash, Hasher},
    mem,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

const MAGIC: u64 = 0xafedbeef;
const FREE_MAGIC: u64 = 0xdeadbeef;

pub struct GcHeader<T> {
    next: Option<NonNull<GcHeader<usize>>>,
    dropper: unsafe fn(NonNull<Self>),
    mark: bool,
    magic: u64,
    val: Box<T>,
}

impl GcHeader<usize> {
    pub unsafe fn free(this: NonNull<Self>) {
        (this.as_ref().dropper)(this)
    }
}

#[repr(transparent)]
pub struct Gc<T> {
    pub(crate) ptr: NonNull<GcHeader<T>>,
}

impl<T> Gc<T> {
    pub fn gc_header(self) -> NonNull<GcHeader<usize>> {
        unsafe { mem::transmute(self.ptr) }
    }
}

impl<T: Hash> Hash for Gc<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.deref().hash(state);
    }
}

impl<T> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}

impl<T> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe {
            assert_eq!(self.ptr.as_ref().magic, MAGIC);
            &self.ptr.as_ref().val
        }
    }
}

impl<T> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            assert_eq!(self.ptr.as_ref().magic, MAGIC);
            &mut self.ptr.as_mut().val
        }
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Gc { ptr: self.ptr }
    }
}

impl<T> Copy for Gc<T> {}

impl<T: Markable + 'static> Gc<T> {
    unsafe fn dropper(mut this: NonNull<GcHeader<T>>) {
        assert_eq!(this.as_ref().magic, MAGIC);
        this.as_mut().magic = FREE_MAGIC;
        drop(Box::from_raw(this.as_ptr()))
    }

    pub fn new(runtime: &mut Runtime, data: T) -> Self {
        runtime.gc();
        let header = GcHeader {
            next: runtime.gc_root,
            dropper: Self::dropper,
            mark: false,
            magic: MAGIC,
            val: Box::new(data),
        };
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(header))) };
        runtime.gc_root = Some(unsafe { mem::transmute(ptr) });
        let out = Gc { ptr };
        runtime.rust_frame.push(Box::new(out));
        out
    }
}

pub trait Markable {
    unsafe fn mark(&self);
}

impl<T: Markable> Markable for Gc<T> {
    unsafe fn mark(&self) {
        self.gc_header().as_mut().mark = true;
        self.deref().mark();
    }
}

impl Markable for ListCons {
    unsafe fn mark(&self) {
        self.0.mark();
        self.1.mark();
    }
}

impl Markable for List {
    unsafe fn mark(&self) {
        match self {
            None => {}
            Some(cons) => cons.mark(),
        }
    }
}

impl<T: Markable> Markable for Vec<T> {
    unsafe fn mark(&self) {
        self.iter().for_each(|x| x.mark());
    }
}

impl Markable for String {
    unsafe fn mark(&self) {}
}

impl Markable for Closure {
    unsafe fn mark(&self) {
        self.env.mark();
    }
}

impl Markable for Ref {
    unsafe fn mark(&self) {
        self.0.mark();
    }
}

impl Markable for Value {
    unsafe fn mark(&self) {
        match self {
            Value::List(list) => list.mark(),
            Value::Tuple(vals) => vals.mark(),
            Value::Str(str) => str.mark(),
            Value::Closure(cl) => cl.mark(),
            Value::Ref(rf) => rf.mark(),

            Value::Fn(_) => {}
            Value::Int(_) => {}
            Value::Float(_) => {}
            Value::Boolean(_) => {}
            Value::Symbol(_) => {}
        }
    }
}

impl<T: PartialEq> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr || self.deref() == other.deref()
    }
}

impl Runtime<'_> {
    pub fn gc(&mut self) {
        self.reset_marks();
        for val in &self.rust_frame {
            unsafe {
                val.mark();
            }
        }
        for frame in &self.frames {
            for val in frame.iter() {
                unsafe {
                    val.mark();
                }
            }
        }
        self.sweep();
        self.validate();
    }

    fn validate(&self) {
        let mut node = self.gc_root;

        unsafe {
            while let Some(header) = node {
                assert_eq!(header.as_ref().magic, MAGIC);
                node = header.as_ref().next;
            }
        }
    }

    fn reset_marks(&mut self) {
        let mut node = self.gc_root;

        unsafe {
            while let Some(mut header) = node {
                header.as_mut().mark = false;
                node = header.as_ref().next;
            }
        }
    }

    fn sweep(&mut self) {
        let mut node = self.gc_root;
        let mut prev = &mut self.gc_root;

        unsafe {
            while let Some(mut header) = node {
                let next = header.as_ref().next;
                if !header.as_ref().mark {
                    *prev = next;
                    GcHeader::free(header);
                } else {
                    prev = &mut header.as_mut().next;
                }
                node = next;
            }
        }
    }
}
