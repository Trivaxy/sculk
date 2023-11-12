use std::{io, collections::HashMap};

use ariadne::{Report, ReportKind, Label, Color, Source};

use crate::{
    backend::{type_pool::TypePool, validate::ValidationError, function::FunctionSignature},
    parser::ParseError, data::ResourceLocation,
};

pub enum CompileError {
    Parse(ParseError),
    Validate(ValidationError),
}

impl From<ParseError> for CompileError {
    fn from(err: ParseError) -> Self {
        CompileError::Parse(err)
    }
}

impl From<ValidationError> for CompileError {
    fn from(err: ValidationError) -> Self {
        CompileError::Validate(err)
    }
}

pub fn print_report(
    file_name: &str,
    file_content: &str,
    error: &CompileError,
    type_pool: &TypePool,
    signatures: &HashMap<ResourceLocation, FunctionSignature>
) {
    let offset = match error {
        CompileError::Parse(error) => error.span.end,
        CompileError::Validate(error) => error.span.end,
    };

    let mut report = Report::build(ReportKind::Error, file_name, offset);

    match error {
        CompileError::Parse(error) => {
            report = report
                .with_message(error.message.clone())
                .with_label(Label::new((file_name, error.span.clone())).with_color(Color::White));
        }
        CompileError::Validate(error) => {
            report = report
                .with_message("Test")
                .with_label(Label::new((file_name, error.span.clone())).with_color(Color::White));
        }
    }

    report.finish().print((file_name, Source::from(file_content)));
}
