use std::{collections::HashMap, io};

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};

use crate::{
    backend::{resolve::ResolvedPart,
        function::FunctionSignature,
        resolve::{Resolution, ResolutionError},
        type_pool::TypePool,
        validate::{ValidationError, ValidationErrorKind},
    },
    data::ResourceLocation,
    parser::ParseError,
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
    types: &TypePool,
    signatures: &HashMap<ResourceLocation, FunctionSignature>,
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
                        .with_message(format!("an if statement's condition must be of type '{}'", types.bool().from(types).fg(Color::Cyan)))
                        .with_label(Label::new((file_name, error.span.clone()))
                            .with_color(Color::Red)
                            .with_message(format!("... but '{}' was given instead", ty.from(types).fg(Color::Cyan))))
                }
                ValidationErrorKind::ExpectedBoolInForCondition(ty) => {
                    report
                        .with_message(format!("a for loop's condition must be of type '{}'", types.bool().from(types).fg(Color::Cyan)))
                        .with_label(Label::new((file_name, error.span.clone()))
                            .with_color(Color::Red)
                            .with_message(format!("... but '{}' was given instead", ty.from(types).fg(Color::Cyan))))
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
                ValidationErrorKind::VariableAssignmentTypeMismatch { expected, actual, expr_span } => {
                    report
                        .with_message(format!("attempted to assign a value of type '{}' to a variable of type '{}'", actual.from(types).fg(Color::Cyan), expected.from(types).fg(Color::Cyan)))
                        .with_label(Label::new((file_name, error.span.clone()))
                            .with_color(Color::Cyan)
                            .with_message(format!("this is of type '{}' ...", expected.from(types).fg(Color::Cyan))))
                        .with_label(Label::new((file_name, expr_span.clone()))
                            .with_color(Color::Red)
                            .with_message(format!("... but this expression is of type '{}'", actual.from(types).fg(Color::Cyan))))
                }
                ValidationErrorKind::ReturnTypeMismatch { expected, actual, expr_span } => {
                    report
                        .with_message(format!("expected a return type of '{}'", expected.from(types).fg(Color::Cyan)))
                        .with_label(Label::new((file_name, expr_span.clone()))
                            .with_color(Color::Red)
                            .with_message(format!("... but this expression is of type '{}'", actual.from(types).fg(Color::Cyan))))
                }
                ValidationErrorKind::ReturnValueExpected(expected) => {
                    report
                        .with_message(format!("expected a return value of type '{}'", expected.from(types).fg(Color::Cyan)))
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                }
                ValidationErrorKind::NotAllPathsReturn => {
                    report
                        .with_message("not all paths return a value")
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                }
                ValidationErrorKind::FunctionCallArgTypeMismatch { name, expected, actual } => {
                    report
                        .with_message(format!("incorrect argument type, the type of parameter '{}' is '{}'", name.fg(Color::Green), expected.from(types).fg(Color::Cyan)))
                        .with_label(Label::new((file_name, error.span.clone()))
                            .with_color(Color::Red)
                            .with_message(format!("... but this expression is of type '{}'", actual.from(types).fg(Color::Cyan))))
                }
                ValidationErrorKind::NotEnoughArguments { missing, callee_span } => {
                    report
                        .with_message(format!("not enough arguments"))
                        .with_label(Label::new((file_name, error.span.clone()))
                            .with_color(Color::Red)
                            .with_message(format!("the following parameters are missing: {}", missing.join(", ").fg(Color::Cyan))))
                }
                ValidationErrorKind::OperationTypeMismatch { lhs, rhs, op } => {
                    report
                        .with_message(format!("cannot apply operator {} to operands of type '{}' and '{}'", op.fg(Color::Yellow), lhs.from(types).fg(Color::Cyan), rhs.from(types).fg(Color::Cyan)))
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                }
                ValidationErrorKind::ComparisonOperatorTypeMismatch { lhs, rhs, op } => {
                    report
                        .with_message(format!("comparison operators such as {} can only be applied to numeric operands of the same type", op.fg(Color::Yellow)))
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                        .with_note(format!("the only numeric types are '{}' and soon '{}'", types.int().from(types).fg(Color::Cyan), "fixed".fg(Color::Cyan)))
                }
                ValidationErrorKind::ArithmeticUnsupported { ty } => {
                    report
                        .with_message(format!("arithmetic operations are not valid for type '{}'", ty.from(types).fg(Color::Cyan)))
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                }
                ValidationErrorKind::FunctionAlreadyDefined(name) => {
                    report
                        .with_message(format!("a function with the name '{}' already exists", name.fg(Color::Green)))
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                }
                ValidationErrorKind::FunctionStructNameClash(name) => {
                    report
                        .with_message(format!("clash between a struct and function which share the name '{}'", name.fg(Color::Green)))
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                        .with_note(format!("this is an error because an expression like '{}{}' would be ambiguous", name.fg(Color::Green), "(...)".fg(Color::Green)))
                }
                ValidationErrorKind::StructAlreadyDefined(name) => {
                    report
                        .with_message(format!("a struct with the name '{}' already exists", name.fg(Color::Green)))
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                }
                ValidationErrorKind::StructFieldAlreadyDefined { struct_name, field_name } => {
                    report
                        .with_message(format!("a field named '{}' already exists within '{}'", field_name.fg(Color::Green), struct_name.fg(Color::Green)))
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                }
                ValidationErrorKind::AmbiguousCall(name) => {
                    report
                        .with_message("ambiguous call")
                        .with_label(Label::new((file_name, error.span.clone()))
                            .with_color(Color::Red)
                            .with_message(format!("the name '{}' is shared by a struct and function", name.fg(Color::Green))))
                }
                ValidationErrorKind::StructSelfReferences(name) => {
                    report
                        .with_message(format!("struct '{}' cannot contain itself either directly or indirectly", name.fg(Color::Green)))
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                }
                ValidationErrorKind::UnknownType(_) => {
                    report
                        .with_message("unknown type")
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                }
                ValidationErrorKind::LogicalOperatorTypeMismatch { lhs, rhs, op } => {
                    report
                        .with_message(format!("logical operators such as {} can only be applied to operands of type '{}'", op.fg(Color::Yellow), types.bool().from(types).fg(Color::Cyan)))
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                }
                ValidationErrorKind::CouldNotResolve(res_error) => match res_error {
                    ResolutionError::AmbiguousIdentifier { name, candidates } => {
                        report
                            .with_message(format!("ambiguous identifier '{}'", name.fg(Color::Green)))
                            .with_label(Label::new((file_name, error.span.clone()))
                                .with_color(Color::Red)
                                .with_message(format!("candidates: {}", candidates.iter().map(|c| match c {
                                    ResolvedPart::Variable(ty, name) => format!("- variable '{}'", name.fg(Color::Green)),
                                    ResolvedPart::GlobalFunction(name) => format!("global function '{}'", name.fg(Color::Green)),
                                    ResolvedPart::Type(ty) => format!("- type '{}'", name.fg(Color::Green)),
                                    ResolvedPart::Field(ty, name) => format!("- field '{}' of type '{}'", name.fg(Color::Green), ty.from(&types).as_struct_def().name().fg(Color::Cyan)),
                                    ResolvedPart::Method(ty, name) => format!("- method '{}' of type '{}'", name.fg(Color::Green), ty.from(&types).as_struct_def().name().fg(Color::Cyan)),
                                    ResolvedPart::Constructor(ty) => format!("- constructor of type '{}'", name.fg(Color::Cyan)),
                                }).collect::<Vec<String>>().join("\n"))))
                    }
                    ResolutionError::UnresolvedIdentifier(name) => {
                        report
                            .with_message(format!("unknown identifier '{}'", name.fg(Color::Green)))
                            .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                    }
                    ResolutionError::MemberDoesNotExist(ty, name) => {
                        report
                            .with_message(format!("type '{}' does not have a member named '{}'", ty.from(&types).as_struct_def().name().fg(Color::Cyan), name.fg(Color::Green)))
                            .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                    }
                    ResolutionError::CannotCallExpression => {
                        report
                            .with_message("expressions are not callable")
                            .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                    }
                    ResolutionError::CannotCallMember(name) => {
                        report
                            .with_message(format!("cannot call member '{}', as it is not a method", name.fg(Color::Green)))
                            .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                    }
                    ResolutionError::CannotAccessMember(name) => {
                        report
                            .with_message(format!("cannot access member '{}', as it is not a field", name.fg(Color::Green)))
                            .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                    }
                }
                ValidationErrorKind::CannotReferenceMethodAsValue => {
                    report
                        .with_message("cannot reference a method as a value")
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                }
                ValidationErrorKind::NotAssignable => {
                    report
                        .with_message("not assignable")
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                }
                ValidationErrorKind::StaticNotAllowed => {
                    report
                        .with_message("static functions can only exist inside struct definitions")
                        .with_label(Label::new((file_name, error.span.clone())).with_color(Color::Red))
                }
            }
        }
    }

    report
        .finish()
        .print((file_name, Source::from(file_content)));
}
