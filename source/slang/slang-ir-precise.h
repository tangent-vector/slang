#pragma once

namespace Slang
{
struct IRModule;

/// Resolve local `precise` demands into attributes on floating-point arithmetic.
///
/// Requires storage demands to have been preserved before SSA construction and mandatory early
/// inlining to have expanded calls with substitution semantics. The normal pipeline invokes this
/// pass after ordinary specialization, while instantiated field types are available. Any remaining
/// unknown type does not expose hidden legacy precise fields. Ordinary inlining must follow this
/// pass. Module constant expressions are analyzed in their own scope. Consumes declaration
/// policies and resolved demand/dependency instructions, leaving operation attributes as the
/// policy representation. Compilation options remain separate from these local restrictions.
void propagatePreciseDemands(IRModule* module);
} // namespace Slang
