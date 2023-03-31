use crate::parser::ParserNode;

// Rebranching is a process that:
// - Collapses if..else if..(else?) chains into strictly nested if-else statements
// - Wraps nodes that follow a potentially returning if/else in a condition, so that they are not executed if the branch returns
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
        ParserNode::FunctionDeclaration { body, .. } => rebranch(body),
        ParserNode::Block(nodes) => {
            let mut wrap_idx = None;

            for (i, node) in nodes.iter_mut().enumerate() {
                rebranch(node);

                if node_has_return(node) && wrap_idx.is_none() {
                    wrap_idx = Some(i + 1);
                }
            }

            if let Some(wrap_idx) = wrap_idx {
                if wrap_idx == nodes.len() {
                    return;
                }

                // optimization: no need to wrap if the next node is a return
                if let ParserNode::Return(_) = nodes[wrap_idx] {
                    return;
                }

                let retsafe_nodes = nodes.drain(wrap_idx..).collect();
                nodes.push(ParserNode::ReturnSafe(Box::new(ParserNode::Block(retsafe_nodes))));
            }
        }
        _ => {}
    }
}

fn node_has_return(node: &ParserNode) -> bool {
    match node {
        ParserNode::Block(nodes) => {
            for node in nodes {
                if node_has_return(node) {
                    return true;
                }
            }

            false
        }
        ParserNode::If {
            body,
            else_ifs,
            else_body,
            ..
        } => {
            if node_has_return(body) {
                return true;
            }

            for (_, body) in else_ifs {
                if node_has_return(body) {
                    return true;
                }
            }

            if let Some(else_body) = else_body {
                return node_has_return(else_body);
            }

            false
        }
        ParserNode::Return(_) => true,
        _ => false,
    }
}
