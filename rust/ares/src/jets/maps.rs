/** Map jets. */
use crate::interpreter::Context;
use crate::jets::util::slot;
use crate::jets::Result;
use crate::noun::{Noun, D, T};
use crate::site::{site_slam, Site};

crate::gdb!();

fn by_rep(context: &mut Context, tree: Noun, site: &Site, out: &mut Noun) {
    if unsafe { tree.raw_equals(D(0)) } {
        return;
    } else {
        let node = slot(tree, 2).unwrap();
        let left = slot(node, 6).unwrap();
        let rite = slot(node, 7).unwrap();

        let acc = T(&mut context.stack, &[node, *out]);
        *out = site_slam(context, site, acc);

        by_rep(context, left, site, out);
        by_rep(context, rite, site, out);
    }
}

pub fn jet_by_rep(context: &mut Context, subject: Noun) -> Result {
    let tree = slot(subject, 30)?;
    let mut gate = slot(subject, 6)?;
    let mut pro = slot(gate, 13)?;

    let site = Site::new(context, &mut gate);
    by_rep(context, tree, &site, &mut pro);
    Ok(pro)
}