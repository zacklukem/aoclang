use crate::Runtime;
use std::{
    hash::{Hash, Hasher},
    mem,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

pub struct GcHeader<T> {
    next: Option<NonNull<GcHeader<()>>>,
    dropper: unsafe fn(NonNull<Self>),
    val: T,
}

impl GcHeader<()> {
    pub unsafe fn free(this: NonNull<Self>) {
        (this.as_ref().dropper)(this)
    }
}

#[repr(transparent)]
pub struct Gc<T> {
    pub(crate) ptr: NonNull<GcHeader<T>>,
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
        unsafe { &self.ptr.as_ref().val }
    }
}

impl<T> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut self.ptr.as_mut().val }
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Gc { ptr: self.ptr }
    }
}

impl<T> Copy for Gc<T> {}

impl<T> Gc<T> {
    unsafe fn dropper(this: NonNull<GcHeader<T>>) {
        drop(Box::from_raw(this.as_ptr()))
    }

    pub fn new(runtime: &mut Runtime, data: T) -> Self {
        let header = GcHeader {
            next: runtime.gc_root,
            dropper: Self::dropper,
            val: data,
        };
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(header))) };
        runtime.gc_root = Some(unsafe { mem::transmute(ptr) });
        Gc { ptr }
    }
}

impl<T: PartialEq> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr || self.deref() == other.deref()
    }
}
