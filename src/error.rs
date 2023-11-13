use std::{io, collections::HashMap};

use ariadne::{Report, ReportKind, Label, Color, Source, Fmt};

use crate::{
    backend::{type_pool::TypePool, validate::{ValidationError, ValidationErrorKind}, function::FunctionSignature},
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
            report = match &error.kind {
                ValidationErrorKind::CannotBreakOutsideLoop => {
                    report
                        .with_message("cannot break outside a for, while, or foreach loop".to_string())
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::White))
                }
                ValidationErrorKind::ExpectedBoolInIf(ty) => {
                    report
                        .with_message(format!("an if statement's condition must be of type '{}'", type_pool.bool().fg(Color::Cyan)))
                        .with_label(Label::new((file_name, error.span.clone()))
                            .with_color(Color::Cyan)
                            .with_message(format!("... but '{}' was given instead", ty.fg(Color::Cyan))))
                }
                ValidationErrorKind::ExpectedBoolInForCondition(ty) => {
                    report
                        .with_message(format!("a for loop's condition must be of type '{}'", type_pool.bool().fg(Color::Cyan)))
                        .with_label(Label::new((file_name, error.span.clone()))
                            .with_color(Color::Cyan)
                            .with_message(format!("... but '{}' was given instead", ty.fg(Color::Cyan))))
                }
                ValidationErrorKind::UnknownVariable(_) => {
                    report
                        .with_message("unknown variable")
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                }
                ValidationErrorKind::UnknownFunction(_) => {
                    report
                        .with_message("unknown function")
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                }
                ValidationErrorKind::VariableAlreadyDefined(_) => {
                    report
                        .with_message("a variable with this name already exists in this scope")
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                }
                ValidationErrorKind::VariableAssignmentTypeMismatch { name, expected, actual, expr_span } => {
                    report
                        .with_message(format!("attempted to assign a value of type '{}' to a variable of type '{}'", actual.fg(Color::Cyan), expected.fg(Color::Cyan)))
                        .with_label(Label::new((file_name, error.span.clone()))
                            .with_color(Color::Cyan)
                            .with_message(format!("'{}' is of type '{}' ...", name.fg(Color::Green), expected.fg(Color::Cyan))))
                        .with_label(Label::new((file_name, expr_span.clone()))
                            .with_color(Color::Red)
                            .with_message(format!("... but this expression is of type '{}'", actual.fg(Color::Cyan))))
                }
                _ => todo!()
            }
        }
    }

    report.finish().print((file_name, Source::from(file_content)));
}
