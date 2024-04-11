use crate::interpreter::{inc, interpret, Context, Error, Result, BAIL_EXIT};
use crate::jets::seam::util::get_by;
use crate::jets::util::slot;
use crate::mem::NockStack;
use crate::noun::{DirectAtom, Noun, D, NOUN_NONE, T};
use crate::unifying_equality::unifying_equality;
use ares_macros::tas;
use std::mem::size_of;
use std::ptr::write_bytes;
use std::slice::{from_raw_parts, from_raw_parts_mut};

#[derive(Copy, Clone)]
struct Frame {
    /// Slow stack as a list
    slow: Noun,
    /// Mean stack as a list
    mean: Noun,
    /// Code table for current arm
    pile: Noun,
    /// Continuation label when returning to this frame
    cont: Noun,
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
            cont: NOUN_NONE,
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

    fn mean_push(&mut self, stack: &mut NockStack, entry: Noun) {
        self.mean = T(stack, &[entry, self.mean]);
    }

    fn mean_pop(&mut self) {
        self.mean = self
            .mean
            .as_cell()
            .expect("Cannot pop empty mean stack")
            .tail();
    }

    fn slow_push(&mut self, stack: &mut NockStack, entry: Noun) {
        self.slow = T(stack, &[entry, self.slow]);
    }

    fn slow_pop(&mut self) {
        self.slow = self
            .slow
            .as_cell()
            .expect("Cannot pop empty slow stack")
            .tail();
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
}

fn push_outer_frame(stack: &mut NockStack, pile: Noun) {
    let vars = pile_sans(pile);
    stack.frame_push(FRAME_WORD_SIZE + vars);
    let frame = unsafe { Frame::current_mut(stack) };
    frame.init(vars, None);
    frame.pile = pile;
}

const PEEK_AXIS: u64 = 4;
const POKE_AXIS: u64 = 46;

fn slam_line(context: &mut Context, arm_axis: u64, sample: Noun) -> Noun {
    let axis_noun = DirectAtom::new_panic(arm_axis).as_noun();
    let subject = T(&mut context.stack, &[sample, context.line]);
    let sample_patch = T(&mut context.stack, &[D(6), D(0), D(2)]);
    let arm_kick_form = T(&mut context.stack, &[D(9), axis_noun, D(0), D(3)]);
    let gate_slam_form = T(
        &mut context.stack,
        &[D(9), D(2), D(10), sample_patch, arm_kick_form],
    );
    interpret(context, subject, gate_slam_form).expect("Crash in codegen")
}

fn cg_peek(context: &mut Context, subject: Noun, formula: Noun) -> Option<Noun> {
    assert!(!context.line.is_none());
    let sample = T(&mut context.stack, &[subject, formula]);
    let peek_result = slam_line(context, PEEK_AXIS, sample);
    if unsafe { peek_result.raw_equals(D(0)) } {
        None
    } else {
        let unit_cell = peek_result.as_cell().expect("Peek should return unit");
        Some(unit_cell.tail())
    }
}

fn cg_poke(context: &mut Context, slow: Noun, subject: Noun, formula: Noun) {
    assert!(!context.line.is_none());
    let sample = T(
        &mut context.stack,
        &[D(tas!(b"comp")), slow, subject, formula],
    );
    let result = slam_line(context, POKE_AXIS, sample);
    let new_line = slot(result, 7).expect("Poke should return triple");
    context.line = new_line;
}

/// Get the $pile for an arm, possibly updating the line core
fn cg_indirect(
    context: &mut Context,
    hill: &mut Noun,
    slow: Noun,
    subject: Noun,
    formula: Noun,
) -> Noun {
    let bell_hill = if let Some(res) = cg_peek(context, subject, formula) {
        res
    } else {
        cg_poke(context, slow, subject, formula);
        cg_peek(context, subject, formula).expect("Codegen peek should return value after poke.")
    };
    let bell_hill_cell = bell_hill
        .as_cell()
        .expect("Codegen successful peek should return pair");
    *hill = bell_hill_cell.tail();
    get_by(
        &mut context.stack,
        &mut bell_hill_cell.tail(),
        &mut bell_hill_cell.head(),
    )
    .expect("Codegen bell lookup should succeed.")
    .expect("Codegen peek bell should be in hill")
}

fn cg_direct(context: &mut Context, hill: &mut Noun, bell: &mut Noun) -> Noun {
    get_by(&mut context.stack, hill, bell)
        .expect("Codegen bell lookup should succeed.")
        .expect("Codegen direct bell should be in hill.")
}

pub fn cg_interpret(context: &mut Context, slow: Noun, subject: Noun, formula: Noun) -> Result {
    let mut hill = NOUN_NONE;
    let outer_pile = cg_indirect(context, &mut hill, slow, subject, formula);
    let virtual_frame = context.stack.get_frame_pointer();
    push_outer_frame(&mut context.stack, outer_pile);
    let mut wish = pile_wish(outer_pile);
    let (mut body, mut bend) = get_blob(context, outer_pile, &mut wish);
    let sire = pile_sire(outer_pile);
    (unsafe { Frame::current_mut(&context.stack).vars_mut() })[sire] = subject;
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
                    todo!("sld")
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
                    // XX TODO implement
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
                    let mut clq_z = clq_zo.head();
                    let mut clq_o = clq_zo.tail();

                    if frame.vars()[clq_s].is_cell() {
                        goto(context, &mut body, &mut bend, &mut clq_z);
                    } else {
                        goto(context, &mut body, &mut bend, &mut clq_o);
                    }
                }
                tas!(b"eqq") => {
                    let eqq_cell = inst_cell.tail().as_cell().unwrap();
                    let eqq_l = eqq_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let eqq_rzo = eqq_cell.tail().as_cell().unwrap();
                    let eqq_r = eqq_rzo.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let eqq_zo = eqq_rzo.tail().as_cell().unwrap();
                    let mut eqq_z = eqq_zo.head();
                    let mut eqq_o = eqq_zo.tail();
                    let l_ref = &mut frame.vars_mut()[eqq_l];
                    let r_ref = &mut frame.vars_mut()[eqq_r];
                    if unsafe {
                        unifying_equality(
                            &mut context.stack,
                            l_ref as *mut Noun,
                            r_ref as *mut Noun,
                        )
                    } {
                        goto(context, &mut body, &mut bend, &mut eqq_z);
                    } else {
                        goto(context, &mut body, &mut bend, &mut eqq_o);
                    }
                }
                tas!(b"brn") => {
                    let brn_cell = inst_cell.tail().as_cell().unwrap();
                    let brn_s = brn_cell.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let brn_zo = brn_cell.tail().as_cell().unwrap();
                    let mut brn_z = brn_zo.head();
                    let mut brn_o = brn_zo.tail();
                    if unsafe { frame.vars()[brn_s].raw_equals(D(0)) } {
                        goto(context, &mut body, &mut bend, &mut brn_z);
                    } else if unsafe { frame.vars()[brn_s].raw_equals(D(1)) } {
                        goto(context, &mut body, &mut bend, &mut brn_o);
                    } else {
                        break BAIL_EXIT;
                    }
                }
                tas!(b"hop") => {
                    let mut hop_t = inst_cell.tail();
                    goto(context, &mut body, &mut bend, &mut hop_t);
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
                    let lnk_t = lnk_dt.tail();
                    let subject = frame.vars()[lnk_u];
                    let formula = frame.vars()[lnk_f];
                    frame.salt = lnk_d;
                    frame.cont = lnk_t;
                    let new_pile = cg_indirect(context, &mut hill, frame.slow, subject, formula);
                    let sire = pile_sire(new_pile);
                    let mut wish = pile_wish(new_pile);
                    push_interpreter_frame(&mut context.stack, new_pile);
                    let new_frame = unsafe { Frame::current_mut(&context.stack) };
                    new_frame.vars_mut()[sire] = subject;
                    goto(context, &mut body, &mut bend, &mut wish);
                }
                tas!(b"cal") => {
                    let cal_cell = inst_cell.tail().as_cell().unwrap();
                    let mut cal_a = cal_cell.head();
                    let cal_vdt = cal_cell.tail().as_cell().unwrap();
                    let cal_v = cal_vdt.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let cal_dt = cal_vdt.tail().as_cell().unwrap();
                    let cal_d = cal_dt.head().as_atom().unwrap().as_u64().unwrap() as usize;
                    let cal_t = cal_dt.tail();
                    let new_pile = cg_direct(context, &mut hill, &mut cal_a);
                    let long = pile_long(new_pile);
                    let walt = pile_walt(new_pile);
                    todo!("cal")
                }
                tas!(b"caf") => {
                    todo!("caf")
                }
                tas!(b"lnt") => {
                    todo!("lnt")
                }
                tas!(b"jmp") => {
                    todo!("jmp")
                }
                tas!(b"jmf") => {
                    todo!("jmf")
                }
                tas!(b"spy") => {
                    todo!("spy")
                }
                tas!(b"mer") => {
                    todo!("mer")
                }
                tas!(b"don") => {
                    todo!("don")
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
        Ok(res) => inner_res,
        Err(err) => exit(context, err),
    }
}

/// Crash with an error, but first unwind the stack
fn exit(context: &mut Context, err: Error) -> Result {
    todo!("exit")
}

fn goto(context: &mut Context, body: &mut Noun, bend: &mut Noun, bile: &mut Noun) {
    let frame = unsafe { Frame::current(&context.stack) };
    let (o, e) = get_blob(context, frame.pile, bile);
    *body = o;
    *bend = e;
}

fn pile_sans(pile: Noun) -> usize {
    (slot(pile, 127)
        .expect("Codegen pile should have sans face")
        .as_atom()
        .expect("Codegen sans should be atom")
        .as_u64()
        .expect("Codegen sans too big")) as usize
}

fn pile_wish(pile: Noun) -> Noun {
    slot(pile, 30).expect("Codegen pile should have wish face")
}

fn pile_sire(pile: Noun) -> usize {
    (slot(pile, 62)
        .expect("Codegen pile should have sire face")
        .as_atom()
        .expect("Codegen sire should be atom")
        .as_u64()
        .expect("Codegen sire too big")) as usize
}

fn pile_will(pile: Noun) -> Noun {
    slot(pile, 126).expect("Codegen pile should have will face")
}

fn pile_long(pile: Noun) -> Noun {
    slot(pile, 2).expect("Codegen pile should have long face")
}

fn pile_walt(pile: Noun) -> Noun {
    slot(pile, 14).expect("Codegen pile should have walt face")
}

fn get_blob(context: &mut Context, pile: Noun, bile: &mut Noun) -> (Noun, Noun) {
    let mut will = pile_will(pile);
    let blob_with_biff = get_by(&mut context.stack, &mut will, bile)
        .expect("Codegen bile lookup successful")
        .expect("Codegen will has bile");
    let blob_cell = slot(blob_with_biff, 3)
        .expect("Codegen blob has tail")
        .as_cell()
        .expect("Codegen blob tail should be cell");
    (blob_cell.head(), blob_cell.tail())
}
