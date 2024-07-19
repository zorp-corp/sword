use crate::interpreter::{
    inc, interpret, Context, ContextSnapshot, Error, Result, WhichInterpreter, BAIL_EXIT, BAIL_FAIL,
};
use crate::jets::seam::util::get_by;
use crate::jets::util::slot;
use crate::jets::{Jet, JetErr::*};
use crate::mem::{NockStack, Preserve};
use crate::noun::{slot_bar, slot_pam, DirectAtom, Noun, D, NOUN_NONE, T};
use crate::unifying_equality::unifying_equality;
use ares_macros::tas;
use either::{Left, Right};
use std::borrow::BorrowMut;
use std::mem::size_of;
use std::ptr::{copy_nonoverlapping, write_bytes};
use std::slice::{from_raw_parts, from_raw_parts_mut};

#[derive(Copy, Clone)]
struct Frame {
    /// Slow stack as a list
    slow: Noun,
    /// Mean stack as a list
    mean: Noun,
    /// Code table for current arm
    pile: Noun,
    /// Continuation block when returning to this frame
    cont: usize,
    /// Result register when returning to this frame
    salt: usize,
    /// number of locals
    vars: usize,
}

impl Frame {
    fn init(&mut self, vars: usize, prev: Option<&Frame>) {
        *self = Frame {
            slow: prev.map_or(D(0), |f| f.slow),
            mean: prev.map_or(D(0), |f| f.mean),
            pile: NOUN_NONE,
            cont: usize::MAX,
            salt: usize::MAX,
            vars,
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

pub struct Block {
    pub blob: Noun,
    pub jet: Option<Jet>,
}

impl Preserve for Block {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        stack.preserve(&mut self.blob);
        // XX do we need to preserve jet?
    }

    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        self.blob.assert_in_stack(stack);
        // XX do we need to assert jet in stack?
    }
}

pub struct Blocks {
    pub data: *mut Block,
    pub lent: usize,
}

impl Preserve for Blocks {
    unsafe fn preserve(&mut self, stack: &mut NockStack) {
        let mut i = 0;
        while i < self.lent {
            let block = (self.data as *mut Block).add(i).as_mut().unwrap();
            stack.preserve(block);
            i += 1;
        }
    }

    unsafe fn assert_in_stack(&self, stack: &NockStack) {
        let mut i = 0;
        while i < self.lent {
            (self.data as *const Block)
                .add(i)
                .as_ref()
                .unwrap()
                .blob
                .assert_in_stack(stack);
            i += 1;
        }
    }
}

pub struct CgContext {
    pub line: Noun,
    pub fuji: Noun,
    pub blox: Blocks, // converted from hill.fuji
}

fn blox<'a>(cg_context: &mut CgContext) -> &'a [Block] {
    let blox = &mut cg_context.blox;
    unsafe { from_raw_parts(blox.data as *const Block, blox.lent) }
}

fn blox_mut<'a>(cg_context: &mut CgContext) -> &'a mut [Block] {
    let blox = &mut cg_context.blox;
    unsafe { from_raw_parts_mut(blox.data as *mut Block, blox.lent) }
}

/// Transforms the $hill from the `CgContext` into a `Blocks` structure then
/// allocates it on the NockStack.
fn blox_init(context: &mut Context) {
    let stack = &mut context.stack;
    let fuji = context.cg_context.fuji;
    let next = slot(fuji, slot_pam(5)) // total number of blocks
        .expect("Codegen fuji should have next")
        .as_atom()
        .unwrap()
        .as_u64()
        .unwrap() as usize;
    unsafe {
        let blocks_p = stack.struct_alloc::<Blocks>(1);
        *blocks_p = Blocks {
            data: stack.struct_alloc::<Block>(next),
            lent: next,
        };
    }
    let blox = blox_mut(&mut context.cg_context);
    let mut hill = slot(fuji, slot_pam(1)).expect("Codegen fuji should have hill");
    let mut i = 0;
    while i < next {
        let blob = get_by(&mut context.stack, &mut hill, &mut D(i as u64))
            .expect("Codegen hill lookup failed")
            .expect("Codegen hill lookup result is None");
        let jet = None; // XX jet match
        let block = Block { blob, jet };
        blox[i] = block;
        i += 1;
    }
}

assert_eq_align!(Frame, u64, usize);
assert_eq_size!(u64, usize);
const FRAME_WORD_SIZE: usize = size_of::<Frame>() / size_of::<u64>();

fn push_interpreter_frame(stack: &mut NockStack, pile: Noun) {
    let vars = pile_sans(pile);
    let prev = unsafe { Frame::current(stack) };
    stack.frame_push(FRAME_WORD_SIZE + vars);
    let frame = unsafe { Frame::current_mut(stack) };
    frame.init(vars, Some(prev));
    frame.pile = pile;
    frame.vars = vars;
}

fn push_outer_frame(stack: &mut NockStack, pile: Noun) {
    let vars = pile_sans(pile);
    stack.frame_push(FRAME_WORD_SIZE + vars);
    let frame = unsafe { Frame::current_mut(stack) };
    frame.init(vars, None);
    frame.pile = pile;
    frame.vars = vars;
}

fn tail_frame(stack: &mut NockStack, pile: Noun) {
    let (old_vars, vars, total_vars) = unsafe {
        let old_frame = Frame::current(stack);
        let old_vars = pile_sans(old_frame.pile);
        let vars = pile_sans(pile);
        let total_vars = vars + old_vars;
        stack.resize_frame(FRAME_WORD_SIZE + total_vars);
        (old_vars, vars, total_vars)
    };
    let frame = unsafe { Frame::current_mut(stack) };
    unsafe {
        let vars_ptr = (frame as *mut Frame).add(1) as *mut Noun;
        copy_nonoverlapping(vars_ptr, vars_ptr.add(vars), old_vars);
        write_bytes(vars_ptr, 0, vars);
    }
    frame.pile = pile;
    frame.vars = total_vars;
    frame.cont = usize::MAX;
    frame.salt = usize::MAX;
}

const PEEK_AXIS: u64 = 4;
const POKE_AXIS: u64 = 46;

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

/// Returns the (register, block index, $fuji) of the peek result, if any.
fn cg_peek(context: &mut Context, subject: Noun, formula: Noun) -> Option<(usize, usize, Noun)> {
    assert!(!context.cg_context.line.is_none());
    let sample = T(&mut context.stack, &[subject, formula]);
    //  XX redefine PEEK_AXIS after linearizer rewrite?
    let peek_result = slam_line(context, PEEK_AXIS, sample);
    if unsafe { peek_result.raw_equals(D(0)) } {
        None
    } else {
        let unit_cell = peek_result.as_cell().expect("Peek should return unit");
        let vals_cell = unit_cell
            .tail()
            .as_cell()
            .expect("Peek should return values");
        let register = vals_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
        let block_idx_fuji_cell = vals_cell.tail().as_cell().unwrap();
        let block_idx = vals_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
        let fuji = block_idx_fuji_cell.tail();
        Some((register, block_idx, fuji))
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

/// Get the (register, block index, $fuji) for an arm, possibly updating the line core
fn cg_indirect(
    context: &mut Context,
    slow: Noun,
    subject: Noun,
    formula: Noun,
) -> (usize, usize, Noun) {
    let (register, block_idx, fuji) = if let Some(res) = cg_peek(context, subject, formula) {
        res
    } else {
        cg_poke(context, slow, subject, formula);
        cg_peek(context, subject, formula).expect("Codegen peek should return value after poke.")
    };
    context.cg_context.fuji = fuji;
    blox_init(context);
    (register, block_idx, fuji)
}

/// XX
fn cg_direct(context: &mut Context, bell: &mut Noun, block_idx: usize) -> Noun {
    let fuji = context.cg_context.fuji;
    let mut gist = slot(fuji, slot_pam(4)).expect("Codegen fuji should have gist");
    let pile = get_by(&mut context.stack, &mut gist, bell)
        .expect("Codegen gist lookup should succeed.")
        .expect("Codegen direct bell should be in fuji");
    pile
}

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
    let (register, block_idx, mut fuji) = cg_indirect(context, slow, subject, formula);
    let mut gist = slot(fuji, slot_pam(4)).expect("Codegen fuji should have gist");
    let mut bell = NOUN_NONE; // XX
    let outer_pile = get_by(&mut context.stack, &mut gist, &mut bell)
        .unwrap()
        .unwrap();
    let virtual_frame = context.stack.get_frame_pointer();
    push_outer_frame(&mut context.stack, outer_pile);
    let (mut body, mut bend) = (NOUN_NONE, NOUN_NONE);
    let sire = usize::MAX; // XX pile_sire
    (unsafe { Frame::current_mut(&context.stack).vars_mut() })[sire] = subject;
    goto(context, &mut body, &mut bend, block_idx);
    let inner_res = 'interpret: loop {
        let frame = unsafe { Frame::current_mut(&context.stack) };
        if let Ok(body_cell) = body.as_cell() {
            body = body_cell.tail();
            let inst_cell = body_cell
                .head()
                .as_cell()
                .expect("Codegen instruction should be a cell");
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
                    frame.vars_mut()[con_d] = T(
                        &mut context.stack,
                        &[frame.vars()[con_h], frame.vars()[con_t]],
                    );
                }
                tas!(b"hed") => {
                    let hed_cell = inst_cell.tail().as_cell().unwrap();
                    let hed_s = hed_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let hed_d = hed_cell.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    let s_noun = frame.vars()[hed_s];
                    if s_noun.is_none() {
                        frame.vars_mut()[hed_d] = NOUN_NONE;
                    } else if let Ok(s_cell) = frame.vars()[hed_s].as_cell() {
                        frame.vars_mut()[hed_d] = s_cell.head();
                    } else {
                        frame.vars_mut()[hed_d] = NOUN_NONE;
                    }
                }
                tas!(b"tal") => {
                    let tal_cell = inst_cell.tail().as_cell().unwrap();
                    let tal_s = tal_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let tal_d = tal_cell.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    let s_noun = frame.vars()[tal_s];
                    if s_noun.is_none() {
                        frame.vars_mut()[tal_d] = NOUN_NONE;
                    } else if let Ok(s_cell) = frame.vars()[tal_s].as_cell() {
                        frame.vars_mut()[tal_d] = s_cell.tail();
                    } else {
                        frame.vars_mut()[tal_d] = NOUN_NONE;
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
                    context
                        .newt
                        .slog(&mut context.stack, 0, frame.vars()[slg_s]);
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
            let inst_tag = inst_cell
                .head()
                .as_atom()
                .expect("Codegen instruction tag should be atom")
                .as_u64()
                .expect("codegen instruction tag should convert to u64");
            match inst_tag {
                tas!(b"clq") => {
                    let clq_cell = inst_cell.tail().as_cell().unwrap();
                    let clq_s = clq_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let clq_zo = clq_cell.tail().as_cell().unwrap();
                    let mut clq_z = clq_zo.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mut clq_o = clq_zo.tail().as_atom().unwrap().as_u64().unwrap() as usize;

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
                    let mut eqq_z = eqq_zo.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mut eqq_o = eqq_zo.tail().as_atom().unwrap().as_u64().unwrap() as usize;
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
                    let mut brn_z = brn_zo.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mut brn_o = brn_zo.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    if unsafe { frame.vars()[brn_s].raw_equals(D(0)) } {
                        goto(context, &mut body, &mut bend, brn_z);
                    } else if unsafe { frame.vars()[brn_s].raw_equals(D(1)) } {
                        goto(context, &mut body, &mut bend, brn_o);
                    } else {
                        break BAIL_EXIT;
                    }
                }
                tas!(b"hop") => {
                    let mut hop_t = inst_cell.tail().as_atom().unwrap().as_u64().unwrap() as usize;
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
                    let (register, new_block, new_pile) =
                        cg_indirect(context, frame.slow, subject, formula);
                    let sire = pile_sire(new_pile);
                    push_interpreter_frame(&mut context.stack, new_pile);
                    let new_frame = unsafe { Frame::current_mut(&context.stack) };
                    new_frame.vars_mut()[sire] = subject;
                    goto(context, &mut body, &mut bend, new_block);
                }
                tas!(b"cal") => {
                    let cal_cell = inst_cell.tail().as_cell().unwrap();
                    let mut cal_a = cal_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let cal_vdt = cal_cell.tail().as_cell().unwrap();
                    let mut cal_v = cal_vdt.head();
                    let cal_dt = cal_vdt.tail().as_cell().unwrap();
                    let cal_d = cal_dt.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let cal_t = cal_dt.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    let new_pile = cg_direct(context, &mut NOUN_NONE, cal_a); // XX bell
                    let mut walt = pile_walt(new_pile);
                    frame.salt = cal_d;
                    frame.cont = cal_t;
                    push_interpreter_frame(&mut context.stack, new_pile);
                    let new_frame = unsafe { Frame::current_mut(&context.stack) };
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
                    goto(context, &mut body, &mut bend, usize::MAX); // XX block index
                }
                tas!(b"caf") => {
                    let caf_cell = inst_cell.tail().as_cell().unwrap();
                    let mut caf_a = caf_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let caf_vdtun = caf_cell.tail().as_cell().unwrap();
                    let mut caf_v = caf_vdtun.head();
                    let caf_dtun = caf_vdtun.tail().as_cell().unwrap();
                    let caf_d = caf_dtun.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let caf_tun = caf_dtun.tail().as_cell().unwrap();
                    let caf_t = caf_tun.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let caf_un = caf_tun.tail().as_cell().unwrap();
                    let caf_u = caf_un.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mut caf_n = caf_un.tail();
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
                            }
                            Err(Punt) => {
                                let new_pile = cg_direct(context, &mut NOUN_NONE, caf_a); // XX bell
                                let mut walt = pile_walt(new_pile);
                                frame.salt = caf_d;
                                frame.cont = caf_t;
                                push_interpreter_frame(&mut context.stack, new_pile);
                                let new_frame = unsafe { Frame::current_mut(&context.stack) };
                                'args: loop {
                                    if unsafe { caf_v.raw_equals(D(0)) } {
                                        assert!(unsafe { walt.raw_equals(D(0)) });
                                        break 'args;
                                    } else {
                                        let v_cell = caf_v.as_cell().unwrap();
                                        let walt_cell = walt.as_cell().unwrap();
                                        caf_v = v_cell.tail();
                                        walt = walt_cell.tail();
                                        let v_i = v_cell.head().as_atom().unwrap().as_u64().unwrap()
                                            as usize;
                                        let walt_i =
                                            walt_cell.head().as_atom().unwrap().as_u64().unwrap()
                                                as usize;
                                        new_frame.vars_mut()[walt_i] = frame.vars()[v_i];
                                    }
                                }
                                // XX block index
                                goto(context, &mut body, &mut bend, usize::MAX);
                            }
                            Err(Fail(err)) => {
                                break Err(err);
                            }
                        }
                    } else {
                        let new_pile = cg_direct(context, &mut NOUN_NONE, caf_a); // XX bell
                        let mut walt = pile_walt(new_pile);
                        frame.salt = caf_d;
                        frame.cont = caf_t;
                        push_interpreter_frame(&mut context.stack, new_pile);
                        let new_frame = unsafe { Frame::current_mut(&context.stack) };
                        'args: loop {
                            if unsafe { caf_v.raw_equals(D(0)) } {
                                assert!(unsafe { walt.raw_equals(D(0)) });
                                break 'args;
                            } else {
                                let v_cell = caf_v.as_cell().unwrap();
                                let walt_cell = walt.as_cell().unwrap();
                                caf_v = v_cell.tail();
                                walt = walt_cell.tail();
                                let v_i =
                                    v_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                                let walt_i =
                                    walt_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                                new_frame.vars_mut()[walt_i] = frame.vars()[v_i];
                            }
                        }
                        goto(context, &mut body, &mut bend, usize::MAX); // XX block index
                    }
                }
                tas!(b"lnt") => {
                    let lnt_cell = inst_cell.tail().as_cell().unwrap();
                    let lnt_u = lnt_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let lnt_f = lnt_cell.tail().as_atom().unwrap().as_u64().unwrap() as usize;
                    let subject = frame.vars()[lnt_u];
                    let formula = frame.vars()[lnt_f];
                    let (reg, idx, new_pile) = cg_indirect(context, frame.slow, subject, formula);
                    let sire = pile_sire(new_pile);
                    tail_frame(&mut context.stack, new_pile);
                    let new_frame = unsafe { Frame::current_mut(&mut context.stack) };
                    new_frame.vars_mut()[sire] = subject;
                    goto(context, &mut body, &mut bend, idx);
                }
                tas!(b"jmp") => {
                    let jmp_cell = inst_cell.tail().as_cell().unwrap();
                    let mut jmp_a = jmp_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mut jmp_v = jmp_cell.tail();
                    let new_pile = cg_direct(context, &mut NOUN_NONE, jmp_a); // XX bell
                    let new_vars = pile_sans(new_pile);
                    let mut walt = pile_walt(new_pile);
                    tail_frame(&mut context.stack, new_pile);
                    let new_frame = unsafe { Frame::current_mut(&mut context.stack) };
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
                            new_frame.vars_mut()[walt_i] = new_frame.vars()[new_vars..][v_i];
                        }
                    }
                    goto(context, &mut body, &mut bend, usize::MAX); // XX block index
                }
                tas!(b"jmf") => {
                    // XX
                    let jmf_cell = inst_cell.tail().as_cell().unwrap();
                    let mut jmf_a = jmf_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let jmf_vun = jmf_cell.tail().as_cell().unwrap();
                    let mut jmf_v = jmf_vun.head();
                    let jmf_un = jmf_vun.tail().as_cell().unwrap();
                    let jmf_u = jmf_un.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mut jmf_n = jmf_un.tail();
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
                            Err(Punt) => {
                                let new_pile = cg_direct(context, &mut NOUN_NONE, jmf_a); // XX bell
                                let new_vars = pile_sans(new_pile);
                                let mut walt = pile_walt(new_pile);
                                tail_frame(&mut context.stack, new_pile);
                                let new_frame = unsafe { Frame::current_mut(&mut context.stack) };
                                'args: loop {
                                    if unsafe { jmf_v.raw_equals(D(0)) } {
                                        assert!(unsafe { walt.raw_equals(D(0)) });
                                        break 'args;
                                    } else {
                                        let v_cell = jmf_v.as_cell().unwrap();
                                        let walt_cell = walt.as_cell().unwrap();
                                        jmf_v = v_cell.tail();
                                        walt = walt_cell.tail();
                                        let v_i = v_cell.head().as_atom().unwrap().as_u64().unwrap()
                                            as usize;
                                        let walt_i =
                                            walt_cell.head().as_atom().unwrap().as_u64().unwrap()
                                                as usize;
                                        new_frame.vars_mut()[walt_i] =
                                            new_frame.vars()[new_vars..][v_i];
                                    }
                                }
                                // XX block index
                                goto(context, &mut body, &mut bend, usize::MAX);
                            }
                            Err(Fail(err)) => {
                                break Err(err);
                            }
                        }
                    } else {
                        let new_pile = cg_direct(context, &mut NOUN_NONE, jmf_a); // XX bell
                        let new_vars = pile_sans(new_pile);
                        let mut walt = pile_walt(new_pile);
                        tail_frame(&mut context.stack, new_pile);
                        let new_frame = unsafe { Frame::current_mut(&mut context.stack) };
                        'args: loop {
                            if unsafe { jmf_v.raw_equals(D(0)) } {
                                assert!(unsafe { walt.raw_equals(D(0)) });
                                break 'args;
                            } else {
                                let v_cell = jmf_v.as_cell().unwrap();
                                let walt_cell = walt.as_cell().unwrap();
                                jmf_v = v_cell.tail();
                                walt = walt_cell.tail();
                                let v_i =
                                    v_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                                let walt_i =
                                    walt_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                                new_frame.vars_mut()[walt_i] = new_frame.vars()[new_vars..][v_i];
                            }
                        }
                        goto(context, &mut body, &mut bend, usize::MAX); // XX block index
                    }
                }
                tas!(b"spy") => {
                    // XX: what do we want to do about the slow path here?
                    let spy_cell = inst_cell.tail().as_cell().unwrap();
                    let spy_e = spy_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let spy_pdt = spy_cell.tail().as_cell().unwrap();
                    let spy_p = spy_pdt.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let spy_dt = spy_pdt.tail().as_cell().unwrap();
                    let spy_d = spy_dt.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mut spy_t = spy_dt.tail().as_atom().unwrap().as_u64().unwrap() as usize;
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
                    let mut mer_i = mer_im.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let mut mer_m = mer_im.tail().as_atom().unwrap().as_u64().unwrap() as usize;
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
    let frame = unsafe { Frame::current(&context.stack) };
    let (o, e) = get_blob(context, block_idx);
    *body = o;
    *bend = e;
}

fn pile_sans(pile: Noun) -> usize {
    (slot(pile, slot_bar(6))
        .expect("Codegen pile should have sans face")
        .as_atom()
        .expect("Codegen sans should be atom")
        .as_u64()
        .expect("Codegen sans too big")) as usize
}

fn pile_sire(pile: Noun) -> usize {
    (slot(pile, slot_pam(3))
        .expect("Codegen pile should have sire face")
        .as_atom()
        .expect("Codegen sire should be atom")
        .as_u64()
        .expect("Codegen sire too big")) as usize
}

fn pile_walt(pile: Noun) -> Noun {
    slot(pile, slot_pam(3)).expect("Codegen pile should have walt face")
}

fn pile_want(pile: Noun) -> Noun {
    slot(pile, slot_pam(1)).expect("Codegen pile should have want face")
}

fn get_blob(context: &mut Context, idx: usize) -> (Noun, Noun) {
    let blox = blox(&mut context.cg_context);
    assert!(0 <= idx && idx < blox.len());
    let blob_with_biff = blox[idx].blob;
    let blob_cell = slot(blob_with_biff, 3)
        .expect("Codegen blob has tail")
        .as_cell()
        .expect("Codegen blob tail should be cell");
    (blob_cell.head(), blob_cell.tail())
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
