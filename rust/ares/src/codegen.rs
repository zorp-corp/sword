use crate::interpreter::{
    inc, interpret, Context, ContextSnapshot, Error, Result, WhichInterpreter, BAIL_EXIT, BAIL_FAIL,
};
use crate::jets::seam::util::get_by;
use crate::jets::util::slot;
use crate::jets::{Jet, JetErr::*};
use crate::mem::{NockStack, Preserve};
use crate::noun::{slot_pam, DirectAtom, Noun, D, NOUN_NONE, T};
use crate::unifying_equality::unifying_equality;
use ares_macros::tas;
use either::{Left, Right};
use std::mem::size_of;
use std::ptr::{copy_nonoverlapping, write_bytes};
use std::slice::{from_raw_parts, from_raw_parts_mut};

#[derive(Copy, Clone)]
struct Frame {
    /// Slow stack as a list
    slow: Noun,
    /// Mean stack as a list
    mean: Noun,
    /// Continuation block index when returning to this frame
    cont: usize,
    /// Result register when returning to this frame
    salt: usize,
    /// Number of locals
    vars: usize,
    /// Compilation unit
    pile: Pile,
}

impl Frame {
    fn init(&mut self, vars: usize, prev: Option<&Frame>) {
        *self = Frame {
            slow: prev.map_or(D(0), |f| f.slow),
            mean: prev.map_or(D(0), |f| f.mean),
            cont: usize::MAX,
            salt: usize::MAX,
            vars,
            pile: Pile {
                sans: usize::MAX,
                well: usize::MAX,
                will: Blocks {
                    data: std::ptr::null_mut() as *mut Noun,
                    lent: 0,
                },
                walt: NOUN_NONE,
            },
        };
        unsafe { write_bytes((self as *mut Frame).add(1) as *mut u64, 0, vars) };
    }

    unsafe fn current<'a>(stack: &NockStack) -> &'a Self {
        &(*(stack.get_frame_base() as *const Frame))
    }

    unsafe fn current_mut<'a>(stack: &NockStack) -> &'a mut Self {
        &mut (*(stack.get_frame_base() as *mut Frame))
    }

    fn vars<'a>(&self) -> &'a [Noun] {
        unsafe { from_raw_parts((self as *const Frame).add(1) as *const Noun, self.vars) }
    }

    fn vars_mut<'a>(&mut self) -> &'a mut [Noun] {
        unsafe { from_raw_parts_mut((self as *mut Frame).add(1) as *mut Noun, self.vars) }
    }
}

#[derive(Copy, Clone)]
pub struct Pile {
    /// Number of locals
    pub sans: usize,
    /// Number of blocks
    pub well: usize,
    /// XX (map @uwoo blob)
    pub will: Blocks,
    /// Direct call register list
    pub walt: Noun,
    // XX Option<Jet> attribute
}

impl Pile {
    pub fn new(context: &mut Context, pile: Noun) -> Self {
        let well = slot(pile, slot_pam(3))
            .unwrap()
            .as_atom()
            .unwrap()
            .as_u64()
            .unwrap() as usize;
        Self {
            sans: slot(pile, slot_pam(1))
                .unwrap()
                .as_atom()
                .unwrap()
                .as_u64()
                .unwrap() as usize,
            well,
            will: Blocks::new(context, &mut slot(pile, slot_pam(4)).unwrap(), well),
            walt: slot(pile, slot_pam(2)).unwrap(),
        }
    }
}

impl Preserve for Pile {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        stack.preserve(&mut self.will);
        stack.preserve(&mut self.walt);
    }

    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        self.will.assert_in_stack(stack);
        self.walt.assert_in_stack(stack);
    }
}

#[derive(Copy, Clone)]
pub struct Hill {
    pub lent: usize,
    pub data: *mut Pile,
}

impl Hill {
    /// Transforms the $hill from the `CgContext.fuji` into a `Hill` structure and
    /// allocates it on the NockStack.
    fn new(context: &mut Context) -> Self {
        let stack = &mut context.stack;
        let fuji = context.cg_context.fuji;
        let mut hill = slot(fuji, slot_pam(2)).expect("Codegen fuji should have hill");
        let next = slot(fuji, slot_pam(3)) // total number of piles
            .expect("Codegen fuji should have next")
            .as_atom()
            .unwrap()
            .as_u64()
            .unwrap() as usize;
        unsafe {
            let hill_p = stack.struct_alloc::<Hill>(1);
            *hill_p = Hill {
                data: stack.struct_alloc::<Pile>(next),
                lent: next,
            };
            let pils: &mut [Pile] = from_raw_parts_mut((*hill_p).data, next);
            let mut i = 0;
            while i < next {
                // XX walk tree manually
                let piln = get_by(&mut context.stack, &mut hill, &mut D(i as u64))
                    .unwrap()
                    .unwrap();
                let pile = Pile::new(context, piln);
                pils[i] = pile;
                i += 1;
            }
            Self {
                data: pils.as_mut_ptr(),
                lent: next,
            }
        }
    }

    fn as_slice<'a>(&self) -> &'a [Pile] {
        unsafe { from_raw_parts(self.data as *const Pile, self.lent) }
    }

    fn _as_mut_slice<'a>(&mut self) -> &'a mut [Pile] {
        unsafe { from_raw_parts_mut(self.data as *mut Pile, self.lent) }
    }
}

impl Preserve for Hill {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        // if stack.is_in_frame(self as *const Hill) {
        let dest: *mut Hill = stack.struct_alloc_in_previous_frame(1);
        (*dest).data = stack.struct_alloc_in_previous_frame::<Pile>(self.lent);
        (*dest).lent = self.lent;
        copy_nonoverlapping::<Pile>(self.data, (*dest).data, self.lent);
        let mut i = 0;
        while i < self.lent {
            let pile = ((*dest).data as *mut Pile).add(i).as_mut().unwrap();
            stack.preserve(pile);
            i += 1;
        }
        (*self) = *dest;
        // }
    }

    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        stack.assert_struct_is_in(self as *const Hill, 1);
        let mut i = 0;
        while i < self.lent {
            (self.data as *const Noun)
                .add(i)
                .as_ref()
                .unwrap()
                .assert_in_stack(stack);
            i += 1;
        }
    }
}

#[derive(Copy, Clone)]
pub struct Blocks {
    pub data: *mut Noun, // blobs
    pub lent: usize,
}

impl Blocks {
    /// Transforms the $will into a `Blocks` structure and
    /// allocates it on the NockStack.
    fn new(context: &mut Context, will: &mut Noun, well: usize) -> Self {
        let stack = &mut context.stack;
        unsafe {
            let blox_p = stack.struct_alloc::<Blocks>(1);
            *blox_p = Blocks {
                data: stack.struct_alloc::<Noun>(well),
                lent: well,
            };
            let blox: &mut [Noun] = from_raw_parts_mut((*blox_p).data, well);
            let mut i = 0;
            while i < well {
                // XX walk tree manually
                let blob = get_by(&mut context.stack, will, &mut D(i as u64))
                    .expect("Codegen will is not a map");
                match blob {
                    Some(b) => {
                        blox[i] = b;
                    }
                    None => {}
                }
                i += 1;
            }
            Self {
                data: blox.as_mut_ptr(),
                lent: well,
            }
        }
    }

    fn as_slice<'a>(&self) -> &'a [Noun] {
        unsafe { from_raw_parts(self.data as *const Noun, self.lent) }
    }

    fn _as_mut_slice<'a>(&mut self) -> &'a mut [Noun] {
        unsafe { from_raw_parts_mut(self.data as *mut Noun, self.lent) }
    }
}

impl Preserve for Blocks {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        // if stack.is_in_frame(self as *const Blocks) {
        let dest: *mut Blocks = stack.struct_alloc_in_previous_frame(1);
        (*dest).data = stack.struct_alloc_in_previous_frame::<Noun>(self.lent);
        (*dest).lent = self.lent;
        copy_nonoverlapping::<Noun>(self.data, (*dest).data, self.lent);
        let mut i = 0;
        while i < self.lent {
            let blob = ((*dest).data as *mut Noun).add(i).as_mut().unwrap();
            stack.preserve(blob);
            i += 1;
        }
        (*self) = *dest;
        // }
    }

    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        stack.assert_struct_is_in(self as *const Blocks, 1);
        let mut i = 0;
        while i < self.lent {
            (self.data as *const Noun)
                .add(i)
                .as_ref()
                .unwrap()
                .assert_in_stack(stack);
            i += 1;
        }
    }
}

pub struct CgContext {
    pub line: Noun,
    pub fuji: Noun,
    pub hill: Hill,
}

impl Preserve for CgContext {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        // XX struct_alloc_in_previous_frame
        stack.preserve(&mut self.line);
        stack.preserve(&mut self.fuji);
        stack.preserve(&mut self.hill);
    }

    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        // XX struct_assert_is_in
        self.line.assert_in_stack(stack);
        self.fuji.assert_in_stack(stack);
        self.hill.assert_in_stack(stack);
    }
}

assert_eq_align!(Frame, u64, usize);
assert_eq_size!(u64, usize);
const FRAME_WORD_SIZE: usize = size_of::<Frame>() / size_of::<u64>();

fn push_interpreter_frame(stack: &mut NockStack, sans: usize) {
    let prev = unsafe { Frame::current(stack) };
    stack.frame_push(FRAME_WORD_SIZE + sans);
    let frame = unsafe { Frame::current_mut(stack) };
    frame.init(sans, Some(prev));
    frame.vars = sans;
}

fn push_outer_frame(stack: &mut NockStack, sans: usize) {
    stack.frame_push(FRAME_WORD_SIZE + sans);
    let frame = unsafe { Frame::current_mut(stack) };
    frame.init(sans, None);
    frame.vars = sans;
}

fn tail_frame(stack: &mut NockStack, sans: usize) {
    let (old_vars, sans, total_vars) = unsafe {
        let old_frame = Frame::current(stack);
        let old_vars = old_frame.pile.sans;
        let total_vars = sans + old_vars;
        stack.resize_frame(FRAME_WORD_SIZE + total_vars);
        (old_vars, sans, total_vars)
    };
    let frame = unsafe { Frame::current_mut(stack) };
    unsafe {
        let sans_ptr = (frame as *mut Frame).add(1) as *mut Noun;
        copy_nonoverlapping(sans_ptr, sans_ptr.add(sans), old_vars);
        write_bytes(sans_ptr, 0, sans);
    }
    frame.vars = total_vars;
    frame.cont = usize::MAX;
    frame.salt = usize::MAX;
}

const PEEK_AXIS: u64 = 4;
const POKE_AXIS: u64 = 10;

fn slam_line(context: &mut Context, arm_axis: u64, sample: Noun) -> Noun {
    let axis_noun = DirectAtom::new_panic(arm_axis).as_noun();
    let subject = T(&mut context.stack, &[sample, context.cg_context.line]);
    let sample_patch = T(&mut context.stack, &[D(6), D(0), D(2)]);
    let arm_kick_form = T(&mut context.stack, &[D(9), axis_noun, D(0), D(3)]);
    let gate_slam_form = T(
        &mut context.stack,
        &[D(9), D(2), D(10), sample_patch, arm_kick_form],
    );
    interpret(context, subject, gate_slam_form).expect("Crash in codegen")
}

/// Returns the call index and $fuji of the peek result, if any.
fn cg_peek(context: &mut Context, subject: Noun, formula: Noun) -> Option<(usize, Noun)> {
    assert!(!context.cg_context.line.is_none());
    let sample = T(&mut context.stack, &[subject, formula]);
    let peek_result = slam_line(context, PEEK_AXIS, sample);
    if unsafe { peek_result.raw_equals(D(0)) } {
        None
    } else {
        // [~ call_idx fuji]
        let unit_cell = peek_result.as_cell().expect("Peek should return unit");
        let t_peek = unit_cell.tail().as_cell().unwrap();

        let call_idx = t_peek.head().as_atom().unwrap().as_u64().unwrap() as usize;
        let fuji = t_peek.tail();

        Some((call_idx, fuji))
    }
}

fn cg_poke(context: &mut Context, slow: Noun, subject: Noun, formula: Noun) {
    assert!(!context.cg_context.line.is_none());
    let sample = T(
        &mut context.stack,
        &[D(tas!(b"comp")), slow, subject, formula],
    );
    let result = slam_line(context, POKE_AXIS, sample);
    let new_line = slot(result, 7).expect("Poke should return triple");
    context.cg_context.line = new_line;
}

/// Get the $pile for an arm
fn cg_direct(context: &mut Context, call_idx: usize) -> Pile {
    context.cg_context.hill.as_slice()[call_idx]
}

/// Get the $pile for an arm, possibly updating the line core
fn cg_indirect(context: &mut Context, slow: Noun, subject: Noun, formula: Noun) -> Pile {
    match cg_peek(context, subject, formula) {
        Some((call_idx, _fuji)) => context.cg_context.hill.as_slice()[call_idx],
        None => {
            cg_poke(context, slow, subject, formula);
            let (call_idx, fuji) = cg_peek(context, subject, formula)
                .expect("Codegen peek should return value after poke.");
            context.cg_context.fuji = fuji;
            context.cg_context.hill = Hill::new(context);
            context.cg_context.hill.as_slice()[call_idx]
        }
    }
}

// fn cg_call_with_jet

pub fn cg_interpret(context: &mut Context, slow: Noun, subject: Noun, formula: Noun) -> Result {
    let snapshot = context.save();
    context.which = WhichInterpreter::CodegenCodegen;
    cg_interpret_with_snapshot(context, &snapshot, slow, subject, formula)
}

pub fn cg_interpret_cg(context: &mut Context, slow: Noun, subject: Noun, formula: Noun) -> Result {
    let snapshot = context.save();
    context.which = WhichInterpreter::TreeWalkingCodegen;
    cg_interpret_with_snapshot(context, &snapshot, slow, subject, formula)
}

pub fn cg_interpret_with_snapshot(
    context: &mut Context,
    snapshot: &ContextSnapshot,
    slow: Noun,
    subject: Noun,
    formula: Noun,
) -> Result {
    let pile = cg_indirect(context, slow, subject, formula);
    let virtual_frame = context.stack.get_frame_pointer();
    push_outer_frame(&mut context.stack, pile.sans);
    let (mut body, mut bend) = (NOUN_NONE, NOUN_NONE);
    {
        let new_frame = unsafe { Frame::current_mut(&context.stack) };
        new_frame.vars_mut()[0] = subject;
        new_frame.pile = pile;
    }
    goto(context, &mut body, &mut bend, 0);
    let inner_res = 'interpret: loop {
        let frame = unsafe { Frame::current_mut(&context.stack) };
        if let Ok(body_cell) = body.as_cell() {
            body = body_cell.tail();
            let inst_cell = body_cell
                .head()
                .as_cell()
                .expect("Codegen instruction should be a cell");
            eprintln!("{}\r", inst_cell);
            let inst_tag = inst_cell
                .head()
                .as_atom()
                .expect("Codegen instruction tag should be atom")
                .as_u64()
                .expect("codegen instruction tag should convert to u64");
            match inst_tag {
                tas!(b"imm") => {
                    let imm_cell = inst_cell.tail().as_cell().unwrap();
                    let imm_n = imm_cell.head();
                    let imm_d = imm_cell.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    frame.vars_mut()[imm_d] = imm_n;
                }
                tas!(b"mov") => {
                    let mov_cell = inst_cell.tail().as_cell().unwrap();
                    let mov_s = mov_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mov_d = mov_cell.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    if mov_s == 6 && mov_d == 5 {
                        eprintln!("s none: {}\r", frame.vars()[mov_s].is_none());
                    }
                    frame.vars_mut()[mov_d] = frame.vars()[mov_s];
                }
                tas!(b"inc") => {
                    let inc_cell = inst_cell.tail().as_cell().unwrap();
                    let inc_s = inc_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let inc_d = inc_cell.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    if let Ok(s_atom) = frame.vars()[inc_s].as_atom() {
                        frame.vars_mut()[inc_d] = inc(&mut context.stack, s_atom).as_noun();
                    } else {
                        break BAIL_EXIT;
                    }
                }
                tas!(b"con") => {
                    let con_cell = inst_cell.tail().as_cell().unwrap();
                    let con_h = con_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let con_tell = con_cell.tail().as_cell().unwrap();
                    let con_t = con_tell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let con_d = con_tell.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    if frame.vars()[con_h].is_none() || frame.vars()[con_t].is_none() {
                        eprintln!("con: poisoned {}\r", con_d);
                        frame.vars_mut()[con_d] = NOUN_NONE;
                    } else {
                        frame.vars_mut()[con_d] = T(
                            &mut context.stack,
                            &[frame.vars()[con_h], frame.vars()[con_t]],
                        );
                    }
                }
                tas!(b"hed") => {
                    let hed_cell = inst_cell.tail().as_cell().unwrap();
                    let hed_s = hed_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let hed_d = hed_cell.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    let s_noun = frame.vars()[hed_s];
                    if s_noun.is_none() || !s_noun.is_cell() {
                        eprintln!("hed: poisoned {}\r", hed_d);
                        frame.vars_mut()[hed_d] = NOUN_NONE;
                    } else {
                        frame.vars_mut()[hed_d] = s_noun.as_cell().unwrap().head();
                    }
                }
                tas!(b"tal") => {
                    let tal_cell = inst_cell.tail().as_cell().unwrap();
                    let tal_s = tal_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let tal_d = tal_cell.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    let s_noun = frame.vars()[tal_s];
                    if s_noun.is_none() || !s_noun.is_cell() {
                        eprintln!("tal: poisoned {}\r", tal_d);
                        frame.vars_mut()[tal_d] = NOUN_NONE;
                    } else {
                        frame.vars_mut()[tal_d] = s_noun.as_cell().unwrap().tail();
                    }
                }
                tas!(b"men") => {
                    let men_cell = inst_cell.tail().as_cell().unwrap();
                    let men_l = men_cell.head();
                    assert!(men_l.is_atom());
                    let men_s = men_cell.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    let men_entry = T(&mut context.stack, &[men_l, frame.vars()[men_s]]);
                    frame.mean = T(&mut context.stack, &[men_entry, frame.mean])
                }
                tas!(b"man") => {
                    frame.mean = frame.mean.as_cell().unwrap().tail();
                }
                tas!(b"slo") => {
                    let slo_s = inst_cell.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    let slo_tag = frame.vars()[slo_s];
                    assert!(slo_tag.is_atom());
                    frame.slow = T(&mut context.stack, &[slo_tag, frame.slow]);
                }
                tas!(b"sld") => {
                    frame.slow = frame.slow.as_cell().unwrap().tail();
                }
                tas!(b"hit") => {
                    // XX TODO implement
                }
                tas!(b"slg") => {
                    let slg_s = inst_cell.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    eprintln!("slg s is none: {}\r", frame.vars()[slg_s].is_none());
                    context.newt.slog2(&mut context.stack, frame.vars()[slg_s]);
                    eprintln!("slgged\r");
                }
                tas!(b"mew") => {
                    let mew_kufr = inst_cell.tail().as_cell().unwrap();
                    // XX will need for persistent memoization
                    let _mew_k = mew_kufr.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mew_ufr = mew_kufr.tail().as_cell().unwrap();
                    let mew_u = mew_ufr.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mew_fr = mew_ufr.tail().as_cell().unwrap();
                    let mew_f = mew_fr.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mew_r = mew_fr.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mut key = T(
                        &mut context.stack,
                        &[frame.vars()[mew_u], frame.vars()[mew_f]],
                    );
                    context.cache =
                        context
                            .cache
                            .insert(&mut context.stack, &mut key, frame.vars()[mew_r]);
                }
                tas!(b"tim") => {
                    // XX TODO implement
                }
                tas!(b"tom") => {
                    // XX TODO implement
                }
                tas!(b"mem") => {
                    // XX TODO implement
                }
                tas!(b"poi") => {
                    let poi_p = inst_cell.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    eprintln!("poi: poisoned {}\r", poi_p);
                    frame.vars_mut()[poi_p] = NOUN_NONE;
                }
                tas!(b"ipb") => {
                    let mut ipb_p = inst_cell.tail();
                    'ipb: loop {
                        if unsafe { ipb_p.raw_equals(D(0)) } {
                            break 'ipb;
                        } else {
                            let p_cell = ipb_p.as_cell().unwrap();
                            ipb_p = p_cell.tail();
                            let p_i = p_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                            if frame.vars()[p_i].is_none() {
                                break 'interpret BAIL_EXIT;
                            }
                        }
                    }
                }
                _ => {
                    panic!("Codegen instruction unsupported");
                }
            }
        } else {
            let inst_cell = bend
                .as_cell()
                .expect("Codegen instruction should be a cell");
            eprintln!("{}\r", inst_cell);
            let inst_tag = inst_cell
                .head()
                .as_atom()
                .expect("Codegen instruction tag should be atom")
                .as_u64()
                .expect("codegen instruction tag should convert to u64");
            match inst_tag {
                tas!(b"clq") => {
                    // XX check for poisons?
                    let clq_cell = inst_cell.tail().as_cell().unwrap();
                    let clq_s = clq_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let clq_zo = clq_cell.tail().as_cell().unwrap();
                    let clq_z = clq_zo.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let clq_o = clq_zo.tail().as_atom().unwrap().as_u64().unwrap() as usize;

                    eprintln!("clq: s is none: {}\r", frame.vars()[clq_s].is_none());

                    if frame.vars()[clq_s].is_cell() {
                        goto(context, &mut body, &mut bend, clq_z);
                    } else {
                        goto(context, &mut body, &mut bend, clq_o);
                    }
                }
                tas!(b"eqq") => {
                    let eqq_cell = inst_cell.tail().as_cell().unwrap();
                    let eqq_l = eqq_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let eqq_rzo = eqq_cell.tail().as_cell().unwrap();
                    let eqq_r = eqq_rzo.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let eqq_zo = eqq_rzo.tail().as_cell().unwrap();
                    let eqq_z = eqq_zo.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let eqq_o = eqq_zo.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    let l_ref = &mut frame.vars_mut()[eqq_l];
                    let r_ref = &mut frame.vars_mut()[eqq_r];
                    if unsafe {
                        unifying_equality(
                            &mut context.stack,
                            l_ref as *mut Noun,
                            r_ref as *mut Noun,
                        )
                    } {
                        goto(context, &mut body, &mut bend, eqq_z);
                    } else {
                        goto(context, &mut body, &mut bend, eqq_o);
                    }
                }
                tas!(b"brn") => {
                    let brn_cell = inst_cell.tail().as_cell().unwrap();
                    let brn_s = brn_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let brn_zo = brn_cell.tail().as_cell().unwrap();
                    let brn_z = brn_zo.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let brn_o = brn_zo.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    if unsafe { frame.vars()[brn_s].raw_equals(D(0)) } {
                        goto(context, &mut body, &mut bend, brn_z);
                    } else if unsafe { frame.vars()[brn_s].raw_equals(D(1)) } {
                        goto(context, &mut body, &mut bend, brn_o);
                    } else {
                        break BAIL_EXIT;
                    }
                }
                tas!(b"hop") => {
                    let hop_t = inst_cell.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    goto(context, &mut body, &mut bend, hop_t);
                }
                tas!(b"hip") => {
                    panic!("hip is unsupported for execution");
                }
                tas!(b"lnk") => {
                    let lnk_cell = inst_cell.tail().as_cell().unwrap();
                    let lnk_u = lnk_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let lnk_fdt = lnk_cell.tail().as_cell().unwrap();
                    let lnk_f = lnk_fdt.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let lnk_dt = lnk_fdt.tail().as_cell().unwrap();
                    let lnk_d = lnk_dt.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let lnk_t = lnk_dt.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    let subject = frame.vars()[lnk_u];
                    let formula = frame.vars()[lnk_f];
                    frame.salt = lnk_d;
                    frame.cont = lnk_t;
                    let pile = cg_indirect(context, frame.slow, subject, formula);
                    push_interpreter_frame(&mut context.stack, pile.sans);
                    let new_frame = unsafe { Frame::current_mut(&context.stack) };
                    new_frame.pile = pile;
                    new_frame.vars_mut()[0] = subject;
                    goto(context, &mut body, &mut bend, 0);
                }
                tas!(b"cal") => {
                    // [%cal a=@uxor v=(list @uvre) d=@uvre t=@uwoo]
                    let t_cal = inst_cell.tail().as_cell().unwrap();
                    let tt_cal = t_cal.tail().as_cell().unwrap();
                    let ttt_cal = tt_cal.tail().as_cell().unwrap();

                    let cal_a = t_cal.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mut cal_v = tt_cal.head();
                    let cal_d = ttt_cal.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let cal_t = ttt_cal.tail().as_atom().unwrap().as_u64().unwrap() as usize;

                    frame.salt = cal_d;
                    frame.cont = cal_t;

                    let pile = cg_direct(context, cal_a);

                    push_interpreter_frame(&mut context.stack, pile.sans);

                    let new_frame = unsafe { Frame::current_mut(&context.stack) };
                    new_frame.pile = pile;
                    let mut walt = pile.walt;
                    'args: loop {
                        if unsafe { cal_v.raw_equals(D(0)) } {
                            assert!(unsafe { walt.raw_equals(D(0)) });
                            break 'args;
                        } else {
                            let v_cell = cal_v.as_cell().unwrap();
                            let walt_cell = walt.as_cell().unwrap();
                            cal_v = v_cell.tail();
                            walt = walt_cell.tail();
                            let v_i = v_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                            let walt_i =
                                walt_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                            new_frame.vars_mut()[walt_i] = frame.vars()[v_i];
                        }
                    }
                    goto(context, &mut body, &mut bend, 1);
                }
                tas!(b"caf") => {
                    // [%caf a=@uxor v=(list @uvre) d=@uvre t=@uwoo u=@uvre n=[path @]]
                    let t_caf = inst_cell.tail().as_cell().unwrap();
                    let tt_caf = t_caf.tail().as_cell().unwrap();
                    let ttt_caf = tt_caf.tail().as_cell().unwrap();
                    let tttt_caf = ttt_caf.tail().as_cell().unwrap();
                    let ttttt_caf = tttt_caf.tail().as_cell().unwrap();

                    let caf_a = t_caf.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mut caf_v = tt_caf.head();
                    let caf_d = ttt_caf.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let caf_t = tttt_caf.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let caf_u = ttttt_caf.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mut caf_n = ttttt_caf.tail();

                    let mut jet: Option<Jet> = None;
                    for (n, a, j) in context.hot {
                        let mut na = T(&mut context.stack, &[n, a.as_noun()]);
                        if unsafe { unifying_equality(&mut context.stack, &mut na, &mut caf_n) } {
                            jet = Some(j);
                            break;
                        }
                    }
                    if let Some(j) = jet {
                        let subject = frame.vars()[caf_u];
                        match j(context, subject) {
                            Ok(r) => {
                                frame.vars_mut()[caf_d] = r;
                                continue 'interpret;
                            }
                            Err(Fail(err)) => {
                                break Err(err);
                            }
                            _ => {}
                        }
                    }
                    let pile = cg_direct(context, caf_a);
                    let mut walt = pile.walt;
                    frame.salt = caf_d;
                    frame.cont = caf_t;
                    push_interpreter_frame(&mut context.stack, pile.sans);
                    let new_frame = unsafe { Frame::current_mut(&context.stack) };
                    new_frame.pile = pile;
                    'args: loop {
                        if unsafe { caf_v.raw_equals(D(0)) } {
                            assert!(unsafe { walt.raw_equals(D(0)) });
                            break 'args;
                        } else {
                            let v_cell = caf_v.as_cell().unwrap();
                            let walt_cell = walt.as_cell().unwrap();
                            caf_v = v_cell.tail();
                            walt = walt_cell.tail();
                            let v_i = v_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                            let walt_i =
                                walt_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                            new_frame.vars_mut()[walt_i] = frame.vars()[v_i];
                        }
                    }
                    goto(context, &mut body, &mut bend, 1);
                }
                tas!(b"lnt") => {
                    let lnt_cell = inst_cell.tail().as_cell().unwrap();
                    let lnt_u = lnt_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let lnt_f = lnt_cell.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    let subject = frame.vars()[lnt_u];
                    let formula = frame.vars()[lnt_f];
                    let pile = cg_indirect(context, frame.slow, subject, formula);
                    tail_frame(&mut context.stack, pile.sans);
                    let new_frame = unsafe { Frame::current_mut(&mut context.stack) };
                    new_frame.pile = pile;
                    new_frame.vars_mut()[0] = subject;
                    goto(context, &mut body, &mut bend, 0);
                }
                tas!(b"jmp") => {
                    // [%jmp a=@uxor v=(list @uvre)]
                    let t_jmp = inst_cell.tail().as_cell().unwrap();

                    let jmp_a = t_jmp.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mut jmp_v = t_jmp.tail();

                    let pile = cg_direct(context, jmp_a);
                    let mut walt = pile.walt;
                    tail_frame(&mut context.stack, pile.sans);
                    let new_frame = unsafe { Frame::current_mut(&mut context.stack) };
                    new_frame.pile = pile;
                    'args: loop {
                        if unsafe { jmp_v.raw_equals(D(0)) } {
                            assert!(unsafe { walt.raw_equals(D(0)) });
                            break 'args;
                        } else {
                            let v_cell = jmp_v.as_cell().unwrap();
                            let walt_cell = walt.as_cell().unwrap();
                            jmp_v = v_cell.tail();
                            walt = walt_cell.tail();
                            let v_i = v_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                            let walt_i =
                                walt_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                            new_frame.vars_mut()[walt_i] = new_frame.vars()[pile.sans..][v_i];
                        }
                    }
                    goto(context, &mut body, &mut bend, 1);
                }
                tas!(b"jmf") => {
                    // [%jmf a=@uwoo v=(list @uvre) u=@uvre n=[path @]]
                    let t_jmf = inst_cell.tail().as_cell().unwrap();
                    let tt_jmf = t_jmf.tail().as_cell().unwrap();
                    let ttt_jmf = tt_jmf.tail().as_cell().unwrap();

                    let jmf_a = t_jmf.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mut jmf_v = tt_jmf.head();
                    let jmf_u = ttt_jmf.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mut jmf_n = ttt_jmf.tail();

                    let mut jet: Option<Jet> = None;
                    for (n, a, j) in context.hot {
                        let mut na = T(&mut context.stack, &[n, a.as_noun()]);
                        if unsafe { unifying_equality(&mut context.stack, &mut na, &mut jmf_n) } {
                            jet = Some(j);
                            break;
                        }
                    }
                    if let Some(j) = jet {
                        let subject = frame.vars()[jmf_u];
                        match j(context, subject) {
                            Ok(mut r) => {
                                unsafe {
                                    context.preserve();
                                    context.stack.preserve(&mut r);
                                    context.stack.frame_pop();
                                }
                                if context.stack.get_frame_pointer() == virtual_frame {
                                    break Ok(r);
                                } else {
                                    let new_frame =
                                        unsafe { Frame::current_mut(&mut context.stack) };
                                    new_frame.vars_mut()[new_frame.salt] = r;
                                    goto(context, &mut body, &mut bend, new_frame.cont)
                                }
                            }
                            Err(Fail(err)) => {
                                break Err(err);
                            }
                            _ => {}
                        }
                    }
                    let pile = cg_direct(context, jmf_a);
                    let mut walt = pile.walt;
                    tail_frame(&mut context.stack, pile.sans);
                    let new_frame = unsafe { Frame::current_mut(&mut context.stack) };
                    new_frame.pile = pile;
                    'args: loop {
                        if unsafe { jmf_v.raw_equals(D(0)) } {
                            assert!(unsafe { walt.raw_equals(D(0)) });
                            break 'args;
                        } else {
                            let v_cell = jmf_v.as_cell().unwrap();
                            let walt_cell = walt.as_cell().unwrap();
                            jmf_v = v_cell.tail();
                            walt = walt_cell.tail();
                            let v_i = v_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                            let walt_i =
                                walt_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                            new_frame.vars_mut()[walt_i] = new_frame.vars()[pile.sans..][v_i];
                        }
                    }
                    goto(context, &mut body, &mut bend, 1);
                }
                tas!(b"spy") => {
                    // XX: what do we want to do about the slow path here?
                    let spy_cell = inst_cell.tail().as_cell().unwrap();
                    let spy_e = spy_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let spy_pdt = spy_cell.tail().as_cell().unwrap();
                    let spy_p = spy_pdt.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let spy_dt = spy_pdt.tail().as_cell().unwrap();
                    let spy_d = spy_dt.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let spy_t = spy_dt.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    frame.vars_mut()[spy_d] =
                        scry(context, frame.vars()[spy_e], frame.vars()[spy_p])?;
                    goto(context, &mut body, &mut bend, spy_t);
                }
                tas!(b"mer") => {
                    let mer_kufdim = inst_cell.tail().as_cell().unwrap();
                    // XX will need for persistent memoization
                    let _mer_k = mer_kufdim.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mer_ufdim = mer_kufdim.tail().as_cell().unwrap();
                    let mer_u = mer_ufdim.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mer_fdim = mer_ufdim.tail().as_cell().unwrap();
                    let mer_f = mer_fdim.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mer_dim = mer_fdim.tail().as_cell().unwrap();
                    let mer_d = mer_dim.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mer_im = mer_dim.tail().as_cell().unwrap();
                    let mer_i = mer_im.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mer_m = mer_im.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mut key = T(
                        &mut context.stack,
                        &[frame.vars()[mer_u], frame.vars()[mer_f]],
                    );
                    if let Some(res) = context.cache.lookup(&mut context.stack, &mut key) {
                        frame.vars_mut()[mer_d] = res;
                        goto(context, &mut body, &mut bend, mer_i);
                    } else {
                        goto(context, &mut body, &mut bend, mer_m);
                    }
                }
                tas!(b"don") => {
                    let don_s = inst_cell.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mut result = frame.vars()[don_s];
                    unsafe {
                        context.preserve();
                        context.stack.preserve(&mut result);
                        context.stack.frame_pop();
                    }
                    if context.stack.get_frame_pointer() == virtual_frame {
                        break Ok(result);
                    } else {
                        let new_frame = unsafe { Frame::current_mut(&mut context.stack) };
                        new_frame.vars_mut()[new_frame.salt] = result;
                        goto(context, &mut body, &mut bend, new_frame.cont);
                    }
                }
                tas!(b"bom") => {
                    break BAIL_EXIT;
                }
                _ => {
                    panic!("Codegen instruction unsupported");
                }
            }
        }
    };
    match inner_res {
        Ok(res) => {
            context.which = snapshot.which;
            Ok(res)
        }
        Err(err) => Err(exit(context, &snapshot, virtual_frame, err)),
    }
}

/// Crash with an error, but first unwind the stack
fn exit(
    context: &mut Context,
    snapshot: &ContextSnapshot,
    virtual_frame: *const u64,
    error: Error,
) -> Error {
    context.restore(snapshot);
    if context.stack.copying() {
        assert!(context.stack.get_frame_pointer() != virtual_frame);
        unsafe { context.stack.frame_pop() };
    }

    let stack = &mut context.stack;
    let mut preserve = match error {
        Error::ScryBlocked(path) => path,
        Error::Deterministic(_, t) | Error::NonDeterministic(_, t) | Error::ScryCrashed(t) => {
            let frame = unsafe { Frame::current(stack) };
            T(stack, &[frame.mean, t])
        }
    };

    while stack.get_frame_pointer() != virtual_frame {
        unsafe {
            stack.preserve(&mut preserve);
            stack.frame_pop();
        }
    }

    match error {
        Error::Deterministic(mote, _) => Error::Deterministic(mote, preserve),
        Error::NonDeterministic(mote, _) => Error::NonDeterministic(mote, preserve),
        Error::ScryCrashed(_) => Error::ScryCrashed(preserve),
        Error::ScryBlocked(_) => error,
    }
}

fn goto(context: &mut Context, body: &mut Noun, bend: &mut Noun, block_idx: usize) {
    let frame = unsafe { Frame::current_mut(&mut context.stack) };
    let blob = frame.pile.will.as_slice()[block_idx];
    let t_blob = blob.as_cell().unwrap().tail().as_cell().unwrap();
    *body = t_blob.head();
    *bend = t_blob.tail();
}

fn scry(context: &mut Context, reff: Noun, path: Noun) -> Result {
    if let Some(cell) = context.scry_stack.cell() {
        let scry_stack = context.scry_stack; // So we can put it back
        let scry_handler = cell.head();
        let scry_payload = T(&mut context.stack, &[reff, path]);
        let scry_patch = T(&mut context.stack, &[D(6), D(0), D(3)]);
        let scry_formula = T(
            &mut context.stack,
            &[D(9), D(2), D(10), scry_patch, D(0), D(2)],
        );
        let scry_subject = T(&mut context.stack, &[scry_handler, scry_payload]);
        context.scry_stack = cell.tail();
        let snapshot = context.save();
        match cg_interpret_with_snapshot(context, &snapshot, D(0), scry_subject, scry_formula) {
            Ok(noun) => match noun.as_either_atom_cell() {
                Left(atom) => {
                    if unsafe { atom.as_noun().raw_equals(D(0)) } {
                        Err(Error::ScryBlocked(path))
                    } else {
                        Err(Error::ScryCrashed(D(0)))
                    }
                }
                Right(cell) => match cell.tail().as_either_atom_cell() {
                    Left(_) => {
                        let hunk = T(&mut context.stack, &[D(tas!(b"hunk")), reff, path]);
                        let frame = unsafe { Frame::current_mut(&mut context.stack) };
                        frame.mean = T(&mut context.stack, &[hunk, frame.mean]);
                        Err(Error::ScryCrashed(D(0)))
                    }
                    Right(cell) => {
                        context.scry_stack = scry_stack;
                        Ok(cell.tail())
                    }
                },
            },
            Err(error) => match error {
                Error::Deterministic(_, trace) | Error::ScryCrashed(trace) => {
                    Err(Error::ScryCrashed(trace))
                }
                Error::NonDeterministic(_, _) => Err(error),
                Error::ScryBlocked(_) => BAIL_FAIL,
            },
        }
    } else {
        // no scry handler
        BAIL_EXIT
    }
}
