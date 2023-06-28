use crate::parser::{JumpInfo, ParserNode};

// Rebranching is a process that:
// 1. Collapses if..else if..(else?) chains into strictly nested if-else statements
// 2. Wraps nodes that follow a potentially returning/breaking/continue if-else statement in a control-flow safe block
//    - For example, "if maybe() { return; } foo();" would be turned into "if maybe() { return; } JUMPSAFE{ foo(); }"
//    - JUMPSAFE blocks are blocks that are compiled into their own functions and will only be called if the break/return/continue was not run

pub fn rebranch(node: &mut ParserNode) {
    match node {
        ParserNode::Program(nodes) => {
            for node in nodes {
                rebranch(node);
            }
        }
        ParserNode::If {
            body: if_body,
            else_ifs,
            else_body,
            ..
        } => {
            for (cond, body) in else_ifs.iter().rev() {
                *else_body = Some(Box::new(ParserNode::If {
                    cond: Box::new(cond.clone()),
                    body: Box::new(body.clone()),
                    else_ifs: vec![],
                    else_body: else_body.clone(),
                }));
            }

            else_ifs.clear(); // everything has been moved into else clauses, so clear out else-ifs

            rebranch(if_body);

            if let Some(else_body) = else_body {
                rebranch(else_body);
            }
        }
        ParserNode::For { body, .. } => rebranch(body),
        ParserNode::FunctionDeclaration { body, .. } => rebranch(body),
        ParserNode::Block(nodes) => {
            //let mut wrap_idx = None;
            //let mut jump_info = JumpInfo::no_jumps();

            for (i, node) in nodes.iter_mut().enumerate() {
                rebranch(node);
                //jump_info = node_jump_info(node);

                // Can the node potentially return/break/continue? If yes, we must wrap the rest of the block in a JUMPSAFE block
                //if jump_info.may_jump() && wrap_idx.is_none() {
                //    wrap_idx = Some(i + 1);
                //}
            }

            //if let Some(wrap_idx) = wrap_idx {
            //    if wrap_idx == nodes.len() {
            //        return;
            //    }

            //    // optimization: no need to wrap if the next node is a return/break/continue
            //    match nodes[wrap_idx] {
            //        ParserNode::Return(_) | ParserNode::Break | ParserNode::Continue => return,
            //        _ => {}
            //    };

            //    let retsafe_nodes = nodes.drain(wrap_idx..).collect();
            //    nodes.push(ParserNode::JumpSafe(
            //        Box::new(ParserNode::Block(retsafe_nodes)),
            //        jump_info,
            //    ));
            //}
        }
        _ => {}
    }
}

fn node_jump_info(node: &ParserNode) -> JumpInfo {
    match node {
        ParserNode::Block(nodes) => {
            let mut jump_info = JumpInfo::no_jumps();

            for node in nodes {
                jump_info.aggregate(node_jump_info(node));
            }

            jump_info
        }
        ParserNode::If {
            body,
            else_ifs,
            else_body,
            ..
        } => {
            let mut if_jump_info = JumpInfo::no_jumps();

            if_jump_info.aggregate(node_jump_info(body));

            for (_, body) in else_ifs {
                if_jump_info.aggregate(node_jump_info(body));
            }

            if let Some(else_body) = else_body {
                if_jump_info.aggregate(node_jump_info(else_body));
            }

            if_jump_info
        }
        ParserNode::For { body, .. } => {
            let mut for_jump_info = node_jump_info(body);
            for_jump_info
        }
        ParserNode::Return(_) => JumpInfo::new(true, false),
        ParserNode::Break => JumpInfo::new(false, true),
        _ => JumpInfo::no_jumps(),
    }
}
