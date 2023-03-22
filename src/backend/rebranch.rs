use crate::parser::ParserNode;

pub fn rebranch(node: &mut ParserNode) {
    match node {
        ParserNode::Program(nodes) => {
            for node in nodes {
                rebranch(node);
            }
        }
        ParserNode::FunctionDeclaration { body, .. } => rebranch(body),
        ParserNode::Block(nodes) => {
            for node in nodes {
                rebranch(node);
            }
        }
        // collapse if..else if..(else?) chains into strictly nested if-else statements
        ParserNode::If {
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
        }
        _ => {}
    }
}
