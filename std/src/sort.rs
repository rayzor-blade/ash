use crate::hl::{hl_type, vbyte, vclosure};
use std::cmp::Ordering;
use std::ffi::c_void;
use std::mem;

// Define a trait for types that can be sorted
pub trait Sortable: Clone {
    fn hl_type() -> *mut hl_type;
}

// Implement Sortable for relevant HashLink types
impl Sortable for i32 {
    fn hl_type() -> *mut hl_type {
        crate::types::hlt_i32()
    }
}
impl Sortable for i64 {
    fn hl_type() -> *mut hl_type {
        crate::types::hlt_i64()
    }
}
impl Sortable for f32 {
    fn hl_type() -> *mut hl_type {
        crate::types::hlt_f32()
    }
}
impl Sortable for f64 {
    fn hl_type() -> *mut hl_type {
        crate::types::hlt_f64()
    }
}
impl Sortable for bool {
    fn hl_type() -> *mut hl_type {
        crate::types::hlt_bool()
    }
}

// Generic structure for sorting
pub struct MSort<'a, T: Sortable> {
    arr: &'a mut [T],
    cmp: *const vclosure,
}

impl<'a, T: Sortable> MSort<'a, T> {
    fn new(arr: &'a mut [T], cmp: *const vclosure) -> Self {
        MSort { arr, cmp }
    }

    unsafe fn compare(&self, a: usize, b: usize) -> Ordering {
        let cmp = &*self.cmp;
        let address = cmp.fun as usize;
        let resolved = crate::fiber::resolve_stub_sentinel(address);
        let fun = if resolved.is_null() {
            cmp.fun
        } else {
            resolved
        };
        let result = if crate::fiber::is_stub_sentinel(address) && resolved.is_null() {
            // Interpreter/hybrid fallback. Worker execution is forbidden from
            // sharing the main interpreter; a compiled-only resolver failure
            // therefore fails closed instead of calling a sentinel or racing
            // `HLInterpreter`.
            if crate::fiber::hlp_fiber_is_worker_lane() {
                eprintln!(
                    "[ash] sort comparator findex {} could not be compiled for a VM worker",
                    address.wrapping_sub(1)
                );
                std::process::abort();
            }
            let Some(runner) = crate::fiber::closure_runner() else {
                return Ordering::Equal;
            };
            let mut lhs = self.arr[a].clone();
            let mut rhs = self.arr[b].clone();
            let lhs = crate::cast::hlp_make_dyn(
                (&mut lhs as *mut T).cast::<c_void>(),
                T::hl_type(),
            );
            let rhs = crate::cast::hlp_make_dyn(
                (&mut rhs as *mut T).cast::<c_void>(),
                T::hl_type(),
            );
            let mut args = [lhs, rhs];
            let result = runner(self.cmp.cast_mut(), args.as_mut_ptr(), 2);
            if result.is_null() { 0 } else { (*result).v.i }
        } else if cmp.hasValue != 0 {
            let fun: unsafe extern "C" fn(*mut std::ffi::c_void, T, T) -> i32 =
                mem::transmute(fun);
            fun(cmp.value, self.arr[a].clone(), self.arr[b].clone())
        } else {
            let fun: unsafe extern "C" fn(T, T) -> i32 = mem::transmute(fun);
            fun(self.arr[a].clone(), self.arr[b].clone())
        };
        result.cmp(&0)
    }

    fn swap(&mut self, a: usize, b: usize) {
        self.arr.swap(a, b);
    }

    fn lower(&self, from: usize, to: usize, val: usize) -> usize {
        let mut from = from;
        let mut len = to - from;
        while len > 0 {
            let half = len >> 1;
            let mid = from + half;
            if unsafe { self.compare(mid, val) } == Ordering::Less {
                from = mid + 1;
                len = len - half - 1;
            } else {
                len = half;
            }
        }
        from
    }

    fn upper(&self, from: usize, to: usize, val: usize) -> usize {
        let mut from = from;
        let mut len = to - from;
        while len > 0 {
            let half = len >> 1;
            let mid = from + half;
            if unsafe { self.compare(val, mid) } == Ordering::Less {
                len = half;
            } else {
                from = mid + 1;
                len = len - half - 1;
            }
        }
        from
    }

    fn gcd(a: usize, b: usize) -> usize {
        if b == 0 {
            a
        } else {
            Self::gcd(b, a % b)
        }
    }

    fn rotate(&mut self, from: usize, mid: usize, to: usize) {
        if from == mid || mid == to {
            return;
        }
        let n = Self::gcd(to - from, mid - from);
        for i in 0..n {
            let val = self.arr[from + i].clone();
            let shift = mid - from;
            let mut p1 = from + i;
            let mut p2 = from + i + shift;
            while p2 != from + i {
                self.arr[p1] = self.arr[p2].clone();
                p1 = p2;
                if to - p2 > shift {
                    p2 += shift;
                } else {
                    p2 = from + (shift - (to - p2));
                }
            }
            self.arr[p1] = val;
        }
    }

    fn do_merge(&mut self, from: usize, pivot: usize, to: usize, len1: usize, len2: usize) {
        if len1 == 0 || len2 == 0 {
            return;
        }
        if len1 + len2 == 2 {
            if unsafe { self.compare(pivot, from) } == Ordering::Less {
                self.swap(pivot, from);
            }
            return;
        }
        let (first_cut, second_cut, len11, len22) = if len1 > len2 {
            let len11 = len1 >> 1;
            let first_cut = from + len11;
            let second_cut = self.lower(pivot, to, first_cut);
            let len22 = second_cut - pivot;
            (first_cut, second_cut, len11, len22)
        } else {
            let len22 = len2 >> 1;
            let second_cut = pivot + len22;
            let first_cut = self.upper(from, pivot, second_cut);
            let len11 = first_cut - from;
            (first_cut, second_cut, len11, len22)
        };
        self.rotate(first_cut, pivot, second_cut);
        let new_mid = first_cut + len22;
        self.do_merge(from, first_cut, new_mid, len11, len22);
        self.do_merge(new_mid, second_cut, to, len1 - len11, len2 - len22);
    }

    fn merge_sort_rec(&mut self, from: usize, to: usize) {
        if to - from < 12 {
            // Insert sort
            for i in from + 1..to {
                let mut j = i;
                while j > from {
                    if unsafe { self.compare(j, j - 1) } == Ordering::Less {
                        self.swap(j - 1, j);
                    } else {
                        break;
                    }
                    j -= 1;
                }
            }
            return;
        }
        let middle = (from + to) >> 1;
        self.merge_sort_rec(from, middle);
        self.merge_sort_rec(middle, to);
        self.do_merge(from, middle, to, middle - from, to - middle);
    }

    pub fn sort(&mut self) {
        let len = self.arr.len();
        self.merge_sort_rec(0, len);
    }
}

// Function to sort different HashLink types
pub unsafe fn hl_bsort<T: Sortable>(bytes: *mut vbyte, pos: i32, len: i32, cmp: *mut vclosure) {
    let slice =
        std::slice::from_raw_parts_mut((bytes as *mut T).offset(pos as isize), len as usize);
    let mut sorter = MSort::new(slice, cmp);
    sorter.sort();
}
