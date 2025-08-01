use hpi_analyzer::Diagnostic;
pub use transpiler::{TranspileArgs, Transpiler};

use crate::transpiler::TranspileOutput;

macro_rules! comment {
    ($self:ident, $vec:expr, $msg:expr) => {
        if $self.user_config.emit_comments {
            $vec.push(Statement::Comment($msg))
        }
    };
}

mod c_ast;
mod expression;
mod gc;
mod statement;
mod transpiler;

/// Transpiles rush source code to C89 / C90.
/// The `Ok(_)` variant also returns non-error diagnostics.
/// The `Err(_)` variant returns a `Vec<Diagnostic>` which contains at least one error.
pub fn transpile<'tree>(
    text: &'tree str,
    path: &'tree str,
    config: TranspileArgs,
) -> Result<(TranspileOutput, Vec<Diagnostic<'tree>>), Vec<Diagnostic<'tree>>> {
    let (tree, diagnostics) = hpi_analyzer::analyze(text, path)?;
    let c_output = Transpiler::new(config).transpile(tree);
    Ok((c_output, diagnostics))
}
