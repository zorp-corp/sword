/** Parser jets
 */
use crate::interpreter::{interpret, NockErr};
use crate::jets;
use crate::jets::util::slot;
use crate::mem::NockStack;
use crate::newt::Newt;
use crate::noun::{Noun, D, T};

crate::gdb!();

// stack frame recording intermediate parse results
type stir_pair = (har: Noun, res: Noun);

pub fn jet_stir(
    stack: &mut NockStack,
    newt: &mut Option<&mut Newt>,
    subject: Noun,
) -> jets::Result {
    let sam = slot(subject, 6)?;  // vere tub
    let ctx = slot(subject, 7)?;  // vere van

    let rud = slot(ctx, 60)?;  // +12 in +7 context
    let raq = slot(ctx, 122)?;  // +26 in +7 context
    let fel = slot(ctx, 123)?;  // +27 in +7 context

    

}

/* stir
  static u3_noun
  _cqe_stir_fun(u3_noun rud,
                u3_noun raq,
                u3_noun fel,
                u3_noun sam)
  {
    //  pil_u: stack control structure
    //  par_u: frame pointer
    //  wag:   initial accumulator (deconstructed)
    //
    u3a_pile    pil_u;
    _stir_pair* par_u;
    u3_noun     p_wag, puq_wag, quq_wag;

    u3a_pile_prep(&pil_u, sizeof(*par_u));

    //  push incremental, successful [fel] parse results onto road stack
    //
    {
      u3_noun    vex, p_vex, q_vex, puq_vex, quq_vex;
      u3j_site fel_u;
      u3j_gate_prep(&fel_u, u3k(fel));

      vex = u3j_gate_slam(&fel_u, u3k(sam));
      u3x_cell(vex, &p_vex, &q_vex);

      u3k(sam);

      while ( u3_nul != q_vex ) {
        u3x_trel(q_vex, 0, &puq_vex, &quq_vex);

        par_u = u3a_push(&pil_u);
        par_u->har = u3k(p_vex);
        par_u->res = u3k(puq_vex);

        u3z(sam);
        sam = u3k(quq_vex);

        u3z(vex);
        vex = u3j_gate_slam(&fel_u, u3k(sam));
        u3x_cell(vex, &p_vex, &q_vex);
      }

      p_wag   = u3k(p_vex);
      puq_wag = u3k(rud);
      quq_wag = sam;

      u3z(vex);
      u3j_gate_lose(&fel_u);
    }

    //  unwind the stack, folding parse results into [wag] by way of [raq]
    //
    if ( c3n == u3a_pile_done(&pil_u) ) {
      u3j_site raq_u;
      u3j_gate_prep(&raq_u, u3k(raq));

      while ( c3n == u3a_pile_done(&pil_u) ) {
        p_wag   = _last_k(par_u->har, p_wag);
        puq_wag = u3j_gate_slam(&raq_u, u3nc(par_u->res, puq_wag));
        par_u   = u3a_pop(&pil_u);
      }

      u3j_gate_lose(&raq_u);
    }

    return u3nq(p_wag, u3_nul, puq_wag, quq_wag);
  }
*/