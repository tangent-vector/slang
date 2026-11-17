#pragma once

#include "core/slang-dictionary.h"

namespace Slang
{
struct IRBuilder;
struct IRInst;
struct IRModule;

/// What the available type definition proves about precise field storage contained by value.
enum class PreciseFieldContainment
{
    No,
    Yes,
    Unknown,
};

/// Whether aggregate lowering has finished adding fields to the types being queried.
enum class PreciseTypeDefinitionState
{
    Complete,
    MayBeIncomplete,
};

/// A phase-local query for precise field storage contained by value in an IR type.
///
/// Follows fields, elements, and builtin by-value payloads, without following pointers or class
/// references. Opaque runtime types expose no fields; unresolved generic types return `Unknown`.
/// During AST lowering a structure may still be receiving fields, so `MayBeIncomplete` prevents
/// an incomplete field list from proving `No`. Clients must discard the query before mutating
/// type definitions or specializing their operands.
class PreciseFieldContainmentQuery
{
public:
    explicit PreciseFieldContainmentQuery(
        PreciseTypeDefinitionState definitionState = PreciseTypeDefinitionState::Complete)
        : m_definitionState(definitionState)
    {
    }

    /// Return what the available definition of `type` proves about contained precise fields.
    ///
    /// Returns `Yes` when a field contained by value is known to be precise, `No` when the
    /// visible definition rules out such fields, and `Unknown` when unresolved or incomplete
    /// types prevent either proof. The cache uses IR type identity after attribute unwrapping;
    /// the query neither reconstructs types nor defines another structural equivalence relation.
    /// The phase and definition-completeness requirements are those of this query object.
    PreciseFieldContainment get(IRInst* type);

private:
    PreciseFieldContainment calculate(IRInst* type);
    PreciseFieldContainment getDifferentialPairContainment(IRInst* type);

    PreciseTypeDefinitionState m_definitionState;
    Dictionary<IRInst*, PreciseFieldContainment> m_results;
};

/// Return whether `address` has an explicit `precise` policy on its declaration or field path.
///
/// Follows address projections, pointer casts, and precision value identities, but does not inspect
/// stored pointer values or infer policies from reads. Local/global variables and direction-wrapped
/// parameters represent storage. Ordinary input parameters and global shader parameters represent
/// values, so their pointer-valued declarations do not qualify the pointed-to storage. An
/// unrecognized or ambiguous address does not prove a declared policy.
bool hasDeclaredPreciseStoragePolicy(IRInst* address);

/// Return `value` or a marker that preserves the precision demand from a write to storage.
///
/// Canonical type, generic, and witness values are returned unchanged: they do not represent
/// runtime computations. An explicit policy on any remaining value emits `MakePrecise`. Without
/// an explicit policy, values whose types prove containment `No` are returned unchanged. Other
/// values receive `MakePreciseIfTypeContainsPrecise`; a later pass resolves that conditional
/// demand using the fields visible after specialization. Existing demands are reused, strengthened
/// for an explicit destination policy, or removed if their condition has become provably false.
/// A null `containment` uses a fresh query that allows incomplete AST-to-IR aggregate definitions.
IRInst* emitValueForPreciseStorage(
    IRBuilder* builder,
    IRInst* value,
    bool hasDeclaredPrecisePolicy,
    PreciseFieldContainmentQuery* containment = nullptr);

/// Preserve declared precision demands on writes before SSA construction removes their storage.
///
/// Requires AST-to-IR lowering to have recorded declaration policies. Adds value-demand markers
/// to stores, global initializers, and global constants without inferring any policy on ordinary
/// reads or crossing function calls. AST lowering separately records function-result demands.
/// May be repeated when early transformations expose new writes; existing markers are reused.
/// Runs before precision resolution consumes the declaration metadata and demand markers.
void preservePreciseStorageDemands(IRModule* module);
} // namespace Slang
