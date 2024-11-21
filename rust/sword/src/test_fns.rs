use crate::{mem::NockStack, noun::Noun};

#[allow(non_snake_case)]
pub fn T<A: crate::noun::NounAllocator>(allocator: &mut A, tup: &[Noun]) -> Noun {
    crate::noun::T(allocator, tup).unwrap()
}

#[allow(non_snake_case)]
pub fn A(stack: &mut NockStack, atom: &ibig::UBig) -> Noun {
    crate::jets::util::test::A(stack, atom).unwrap()
}
