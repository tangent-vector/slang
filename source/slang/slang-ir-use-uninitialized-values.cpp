// slang-ir-use-uninitialized-values.cpp
#include "slang-ir-use-uninitialized-values.h"

//
// This file implements a pass to check whether storage locations
// (such as local variables) are properly initialized at each
// point where they are accessed.
//

#include "slang-ir-insts.h"
#include "slang-ir-reachability.h"
#include "slang-ir-util.h"
#include "slang-ir.h"

namespace Slang
{

    static bool isDifferentiableFunc(IRInst* func)
    {
        for (auto decor = func->getFirstDecoration(); decor; decor = decor->getNextDecoration())
        {
            switch (decor->getOp())
            {
            case kIROp_ForwardDerivativeDecoration:
            case kIROp_ForwardDifferentiableDecoration:
            case kIROp_BackwardDerivativeDecoration:
            case kIROp_BackwardDifferentiableDecoration:
            case kIROp_UserDefinedBackwardDerivativeDecoration:
                return true;
            default:
                break;
            }
        }

        return false;
    }

    static bool isReturnedValue(IRInst* inst)
    {
        for (auto use = inst->firstUse; use; use = use->nextUse)
        {
            IRInst* user = use->getUser();
            if (as<IRReturn>(user))
                return true;

            // Loading from a Ptr type should be
            // treated as an aliased path to any return
            IRLoad* load = as<IRLoad>(user);
            if (load && isReturnedValue(load))
                return true;
        }
        return false;
    }

    struct InitializationState
    {
    public:
        enum class Tag
        {
            None,
            KnownFullyUninitialized,
            KnownFullyInitialized,
            DiffersAlongControlFlowPaths,

            PerField,
            PerElement,
        };

        class ExtraData : public RefObject
        {

        };

        Tag getTag() const { return _tag; }

        InitializationState getFieldState(IRInst* fieldKey) const;
        InitializationState getElementState(IRInst* elementIndex) const;

        void setFieldState(IRStructType* structType, IRInst* fieldKey, InitializationState const& state);
        void setElementState(IRInst* fieldKey, InitializationState const& state);

        static InitializationState knownFullyUninitialized()
        {
            return InitializationState(Tag::KnownFullyUninitialized);
        }

        static InitializationState knownFullyInitialized()
        {
            return InitializationState(Tag::KnownFullyInitialized);
        }

        static InitializationState differsAlongControlFlowPaths()
        {
            return InitializationState(Tag::DiffersAlongControlFlowPaths);
        }

        bool operator==(InitializationState const& other) const
        {
            if (_tag != other._tag)
                return false;

            // TODO: tag-specific checking...

            return true;
        }

        bool operator!=(InitializationState const& other) const
        {
            return !(*this == other);
        }

        void merge(InitializationState const& other)
        {
            if (getTag() == Tag::None)
            {
                *this = other;
                return;
            }

            if (getTag() == other.getTag())
            {
                // TODO: tag-specicic checking

                return;
            }

            // fallback: assume state differs across control-flow paths
            *this = InitializationState::differsAlongControlFlowPaths();
        }

        InitializationState()
        {
        }

    private:
        InitializationState(
            Tag tag)
            : _tag(tag)
        {
        }

        Tag _tag = Tag::None;
        RefPtr<ExtraData> _extraData;
    };

    class PerFieldData : public InitializationState::ExtraData
    {
    public:
        IRStructType* structType = nullptr;
        Dictionary<IRInst*, InitializationState> mapFieldToState;
        InitializationState defaultState;
    };

    InitializationState InitializationState::getFieldState(IRInst* fieldKey) const
    {
        switch (getTag())
        {
        default:
            return *this;

        case Tag::PerElement:
            SLANG_UNEXPECTED("shouldn't be both an array and struct");
            return InitializationState();

        case Tag::PerField:
        {
            auto extra = (PerFieldData*)_extraData.get();
            if (auto found = extra->mapFieldToState.tryGetValue(fieldKey))
                return *found;
            return extra->defaultState;
        }
        }
    }

    InitializationState InitializationState::getElementState(IRInst* elementIndex) const
    {
        switch (getTag())
        {
        default:
            return *this;

        case Tag::PerField:
            SLANG_UNEXPECTED("shouldn't be both an array and struct");
            return InitializationState();

        case Tag::PerElement:
        {
            // TODO: implement this case!!!
            return InitializationState();
        }
        }

    }

    void InitializationState::setFieldState(IRStructType* structType, IRInst* fieldKey, InitializationState const& state)
    {
        if (getTag() != Tag::PerField)
        {
            SLANG_ASSERT(getTag() != Tag::PerElement);

            auto extraData = new PerFieldData();
            extraData->structType = structType;
            extraData->defaultState = *this;

            _tag = Tag::PerField;
            _extraData = extraData;
        }

        SLANG_ASSERT(getTag() == Tag::PerField);
        auto extra = (PerFieldData*)_extraData.get();
        extra->mapFieldToState[fieldKey] = state;

        // We need to check if this state has a consistent result for all the fields
        // defined on the corresponding type.
        //
        InitializationState matchingState;
        for (auto entry : extra->mapFieldToState)
        {
            if (matchingState.getTag() == Tag::None)
            {
                matchingState = entry.second;
                continue;
            }

            // If the state for this field is different
            // from the common value we are checking against,
            // then there are still different values per-field...
            //
            if (entry.second != matchingState)
                return;
        }

        if (matchingState.getTag() == Tag::None
            || matchingState == extra->defaultState)
        {
            // If all the per-field states matched, and none is
            // inconsistent with the default state, then we can
            // just use the default state instead.
            auto defaultState = extra->defaultState;
            *this = defaultState;
            return;
        }

        // If we don't have access to a struct type that we can
        // use for checking exhaustiveness of the fields, then
        // there is nothing we can really do...
        //
        if (!extra->structType)
            return;

        // If all of the fields that appear in the struct type
        // are present in the state, then we know that every field
        // is represented and thus their (common) initialization
        // state can be used on its own.
        //
        for (auto fieldInst : extra->structType->getFields())
        {
            auto fk = fieldInst->getKey();
            if (!extra->mapFieldToState.containsKey(fk))
                return;
        }

        *this = matchingState;
        return;
    }

    void InitializationState::setElementState(IRInst* fieldKey, InitializationState const& state)
    {
        // TODO: implement this case!!!
    }

    //
    // As with many other IR passes, we will encapsulate the main operation
    // of this pass in a context type so that we can scope all of the
    // scratch state that we might need.
    //

    struct InitializationCheckingContext
    {
    public:
        InitializationCheckingContext(//
            IRModule* module,
            DiagnosticSink* sink)
            : _module(module)
            , _sink(sink)
        {
        }

    private:
        IRModule* _module = nullptr;
        DiagnosticSink* _sink = nullptr;

    public:

        //
        // We start with a simple routine to recursively walk the IR
        // instruction hierarchy and attempt to perform appropriate
        // checks on each instruction we encounter.
        //
        // We will early-out on instructions that cannot possibly
        // require initialization-checking behavior.
        //

        void checkInitializationRec(IRInst* inst)
        {
            switch (inst->getOp())
            {
            // Our default disposition is to ignore an instruction,
            // since most instructions aren't candidates to the be
            // the root of our checking logic.
            //
            default:
                return;

            // Some instruction types just need us to recurse into
            // their children, but don't need to be checked on their
            // own.
            //
            case kIROp_ModuleInst:
            case kIROp_Generic:
            case kIROp_Block:
                break;

            // The main thing we are interested in is functions.
            // (Note that various different AST declarations might be
            // lowered to IR functions, so we don't need to check
            // individually for things like property/subscript accessors, etc.)
            //
            case kIROp_Func:
                checkInitializationBehaviorForFunc(cast<IRFunc>(inst));

                // Once we are done checking the function itself, we will
                // move on to checking any instructions in its body, since
                // at this point there could be things like nesting functions
                // or other constructs that require our attention.
                //
                break;

            // We will also attempt to diagnose the use of global variables
            // before they are definitively initialized.
            //
            case kIROp_GlobalVar:
                checkInitializationBehaviorForGlobalVar(cast<IRGlobalVar>(inst));
                break;
            }

            for (auto child : inst->getChildren())
            {
                checkInitializationRec(child);
            }
        }

    private:
        //
        struct BlockInfo
        {
            Dictionary<IRInst*, InitializationState> stateOfStorageLocationsAtEndOfBlock;
        };
        Dictionary<IRBlock*, BlockInfo> _mapBlockToInfo;

        enum class Mode
        {
            CalculatingState,
            CheckingBehavior,
        };
        Mode mode = Mode::CalculatingState;

        IRFunc* _func = nullptr;
        IRConstructorDecoration* _constructorDecoration = nullptr;

        void checkInitializationBehaviorForFunc(IRFunc* func)
        {
            _func = func;
            _constructorDecoration = func->findDecoration<IRConstructorDecoration>();

            // First, we'll do the easy part.
            //
            // Early passes that optimized the IR will have generated
            // `LoadUninitialized` instructions to represent places where
            // the code could be statically determined to read from
            // a storage location that had not been initialized.
            //
            for (auto block : func->getBlocks())
            {
                for (auto inst : block->getChildren())
                {
                    auto loadUninitialized = as<IRLoadUninitialized>(inst);
                    if (!loadUninitialized)
                        continue;

                    //
                    // TODO(tfoley): We need a bit of a hack in the front-end to
                    // make it so that any type that can be field-by-field initialized
                    // *and* that has zero fields is implicitly constructed/initialized
                    // as part of lowering (rather than leaving it up to the user).
                    //

                    // This will be looked into later
                    if (_constructorDecoration && isReturnedValue(inst))
                        continue;

                    _sink->diagnose(loadUninitialized, Diagnostics::usingUninitializedVariable, inst);
                }
            }


            // If the IR for `func` doesn't include a body, then there
            // is no checking that we can do; we have to trust that
            // whatever module will provide the function body that gets
            // linked in will have been correctly checked.
            //
            if (!isDefinition(func))
                return;

            // HACK: The previous version of this pass that is being adapted
            // here was explicitly ignoring all differentiable functions
            // and just not performing any initialization-related validation
            // on them. Because I'm not ready to dive into trying to fix
            // the underlying issue(s), I'm going to leave this hack in place
            // for now.
            //
            // TODO(tfoley): Properly investigate what led to the introduction
            // of this hack, and try to resolve the root cause. We really need
            // to get proper validation for differentiable functions.
            //
//            if (isDifferentiableFunc(func))
//                return;

            // We will handle this as an iterative dataflow problem.
            //
            // For each basic block we will track the statically-determined
            // initialization state of each storage location on exit from that block.
            // When iterating, we can compute an initial state for a block
            // by merging the output states of its predecessors, and then
            // stepping through the instructions in the block to identify how
            // they will modify that initial state. At the end the new state
            // will replace the output state that was stored for the block
            // (and we'll track whether anything changed at that point).
            //
            mode = Mode::CalculatingState;
            bool anyChanges = true;
            while (anyChanges)
            {
                anyChanges = false;

                for (auto block : func->getBlocks())
                {
                    bool blockChanged = updateInitializationInfoForBlock(block);
                    if (blockChanged)
                        anyChanges = true;
                }
            }

            // Once things have converged, we want to make one more
            // pass over all of the blocks, so that we can actually
            // perform the validation of initialization state at each
            // use site.
            //
            mode = Mode::CheckingBehavior;
            for (auto block : func->getBlocks())
            {
                updateInitializationInfoForBlock(block);
            }

#if 0

            // Check out parameters
            if (!isUnmodifying(func))
            {
                int index = 0;
                for (auto param : firstBlock->getParams())
                {
                    ParameterCheckType checkType = isPotentiallyUnintended(param, stage, index);
                    if (checkType == AsOut)
                        checkParameterAsOut(reachability, func, param, sink);
                    index++;
                }
            }

            // Check ordinary instructions
            for (auto block : func->getBlocks())
            {
                for (auto inst = block->getFirstInst(); inst; inst = inst->getNextInst())
                {
                    if (!isUninitializedValue(inst))
                        continue;

                    // This will be looked into later
                    if (constructor && isReturnedValue(inst))
                        continue;

                    IRType* type = inst->getFullType();
                    if (canIgnoreType(type, nullptr))
                        continue;

                    auto loads = getUnresolvedVariableLoads(reachability, inst);
                    for (auto load : loads)
                    {
                        sink->diagnose(load, Diagnostics::usingUninitializedVariable, inst);
                    }
                }
            }

            // Separate analysis for constructors
            checkConstructor(func, reachability, sink);
#endif


        }

        BlockInfo getInitializationInfoFor(IRBlock* block)
        {
            return _mapBlockToInfo[block];
        }

        bool updateInitializationInfoForBlock(IRBlock* block)
        {
            // First, we will merge the initialization state info for all of the predecessors
            // of `block`.
            //
            BlockInfo updatedInfo;
            for (auto predecessor : block->getPredecessors())
            {
                merge(updatedInfo, getInitializationInfoFor(predecessor));
            }

            // Next we will iterate over the instructions in `block`,
            // and compute how they should modify the initialization
            // state.
            //
            for (auto inst : block->getChildren())
            {
                updateInitializationInfoBasedOnInst(updatedInfo, inst);
            }

            // Finally, we will overwrite the intialization state
            // that is currently being stored for `block` using the
            // `updatedInfo`, and note whether anything changed.
            //
            bool anyChanges = overwriteInitializationInfoForBlock(block, updatedInfo);
            return anyChanges;
        }

        void merge(BlockInfo& dstInfo, BlockInfo const& srcInfo)
        {
            for (auto entry : srcInfo.stateOfStorageLocationsAtEndOfBlock)
            {
                if (!dstInfo.stateOfStorageLocationsAtEndOfBlock.containsKey(entry.first))
                {
                    dstInfo.stateOfStorageLocationsAtEndOfBlock[entry.first] = entry.second;
                }
                else
                {
                    merge(dstInfo.stateOfStorageLocationsAtEndOfBlock[entry.first], entry.second);
                }
            }
        }

        void merge(InitializationState& dstState, InitializationState const& srcState)
        {
            // No need to merge states if they are identical.
            if (dstState == srcState)
                return;

            dstState.merge(srcState);
//            dstState = InitializationState::DiffersAlongControlFlowPaths;
        }

        void updateInitializationInfoBasedOnInst(BlockInfo& info, IRInst* inst)
        {
            switch (inst->getOp())
            {
            default:
                break;

            case kIROp_Var:
                updateInitializationInfoBasedOnVarInst(info, cast<IRVar>(inst));
                break;

            case kIROp_Param:
                updateInitializationInfoBasedOnParamInst(info, cast<IRParam>(inst));
                break;


            case kIROp_Store:
                updateInitializationInfoBasedOnStoreInst(info, cast<IRStore>(inst));
                break;

            case kIROp_Call:
                updateInitializationInfoBasedOnCallInst(info, cast<IRCall>(inst));
                break;

            case kIROp_Load:
                handleLoad(info, cast<IRLoad>(inst));
                break;

            case kIROp_Return:
                handleReturn(info, cast<IRReturn>(inst));
                break;
            }
        }

        struct AccessPath
        {
            AccessPath const* parent = nullptr;
            IRInst* inst = nullptr;
        };

        void updateInitializationInfoBasedOnVarInst(BlockInfo& info, IRVar* var)
        {
            info.stateOfStorageLocationsAtEndOfBlock[var] = InitializationState::knownFullyUninitialized();
        }

        void updateInitializationInfoBasedOnParamInst(BlockInfo& info, IRParam* param)
        {
            auto paramType = param->getDataType();
            switch (paramType->getOp())
            {
            case kIROp_OutType:
                info.stateOfStorageLocationsAtEndOfBlock[param] = InitializationState::knownFullyUninitialized();
                break;

            case kIROp_InOutType:
                info.stateOfStorageLocationsAtEndOfBlock[param] = InitializationState::knownFullyInitialized();
                break;

            default:
                // other parameters do not represent cases relevant to our checking...
                break;
            }

        }

        void updateInitializationInfoBasedOnStoreInst(BlockInfo& info, IRStore* storeInst)
        {
            auto destinationStorageLocationInst = storeInst->getPtr();
            handleWrite(info, storeInst, destinationStorageLocationInst);
        }

        void updateInitializationInfoBasedOnCallInst(BlockInfo& info, IRCall* callInst)
        {
            // TODO: need to iterate over the parameters of the callee (or at least
            // the type of the callee) to determine which parameters should count
            // as a write to the argument at that position, and then delegate
            // down to the write logic below for the arguments in those positions.

            auto funcInst = callInst->getCallee();
            auto funcInstType = funcInst->getDataType();
            if (auto funcType = as<IRFuncType>(funcInstType))
            {
                int argIndex = 0;
                for (auto paramType : funcType->getParamTypes())
                {
                    auto argInst = callInst->getArg(argIndex++);

                    updateInitializationInfoBasedOnCallArg(info, callInst, argInst, paramType);
                }
            }
            else
            {
                _sink->diagnose(callInst, Diagnostics::unexpected);
            }
        }

        void updateInitializationInfoBasedOnCallArg(BlockInfo& info, IRInst* initiatingInst, IRInst* argInst, IRType* paramType)
        {
            switch (paramType->getOp())
            {
            case kIROp_OutType:
                handleWrite(info, initiatingInst, argInst);
                break;

            case kIROp_InOutType:
            case kIROp_RefType:
                handleModify(info, initiatingInst, argInst);
                break;

            case kIROp_ConstRefType:
                handleRead(info, initiatingInst, argInst);
                break;
            }
        }

        enum class AccessKind
        {
            Read,
            Return,
            Write,
            Modify,
        };

        void handleWrite(BlockInfo& info, IRInst* initiatingInst, IRInst* storageLocation)
        {
            handleAccess(info, AccessKind::Write, initiatingInst, storageLocation);
        }

        void handleRead(BlockInfo& info, IRInst* initiatingInst, IRInst* storageLocation)
        {
            if (mode != Mode::CheckingBehavior)
                return;

            if (_constructorDecoration && isReturnedValue(storageLocation))
                return;

            handleAccess(info, AccessKind::Read, initiatingInst, storageLocation);
        }

        void handleModify(BlockInfo& info, IRInst* initiatingInst, IRInst* storageLocation)
        {
            handleRead(info, initiatingInst, storageLocation);
            handleWrite(info, initiatingInst, storageLocation);
        }

        void handleAccess(BlockInfo& info, AccessKind access, IRInst* initiatingInst, IRInst* storageLocation, AccessPath const* path = nullptr)
        {
            switch (storageLocation->getOp())
            {
            default:
//                _sink->diagnose(storageLocation, Diagnostics::unimplemented, "missing case in `updateInitializationStateBasedOnWrite`");
                break;

            case kIROp_GetElementPtr:
                {
                    AccessPath elementPath;
                    elementPath.parent = path;
                    elementPath.inst = storageLocation;
                    handleAccess(
                        info,
                        access,
                        initiatingInst,
                        cast<IRGetElementPtr>(storageLocation)->getBase(),
                        &elementPath);
                }
                break;

            case kIROp_FieldAddress:
                {
                    AccessPath fieldPath;
                    fieldPath.parent = path;
                    fieldPath.inst = storageLocation;
                    handleAccess(
                        info,
                        access,
                        initiatingInst,
                        cast<IRFieldAddress>(storageLocation)->getBase(),
                        &fieldPath);
                }
                break;

            case kIROp_Var:
            case kIROp_Param:
                handleVarOrParamAccess(info, access, initiatingInst, storageLocation, path);
                break;

                // For this part of the pass, we want to ignore the initialization state
                // info for global variables (that is a distinct pass).
            case kIROp_GlobalVar:
                break;
            }
        }

        void handleVarOrParamAccess(BlockInfo& info, AccessKind access, IRInst* initiatingInst, IRInst* varOrParam, AccessPath const* path)
        {
            switch (access)
            {
            default:
                break;

            case AccessKind::Read:
            case AccessKind::Return:
            case AccessKind::Modify:
                handleVarOrParamRead(info, access, initiatingInst, varOrParam, path);
                break;
            }

            switch (access)
            {
            default:
                break;

            case AccessKind::Write:
            case AccessKind::Modify:
                if (auto statePtr = info.stateOfStorageLocationsAtEndOfBlock.tryGetValue(varOrParam))
                    updateStorageLocationInitializationStateBasedOnWrite(*statePtr, path);
                break;
            }
        }

        IRInst* maybeGetUnspecializedValue(IRInst* inst)
        {
            while (auto specialize = as<IRSpecialize>(inst))
            {
                auto generic = as<IRGeneric>(specialize->getBase());
                if (!generic)
                    break;

                auto returnVal = findGenericReturnVal(generic);
                if (!returnVal)
                    break;

                inst = returnVal;
            }
            return inst;
        }

        void updateStorageLocationInitializationStateBasedOnWrite(InitializationState& ioStorageLocationState, AccessPath const* path)
        {
            // If the storage location was already in a fully initialized state, then
            // we know that this operation will not change that fact.
            //
            if (ioStorageLocationState.getTag() == InitializationState::Tag::KnownFullyInitialized)
                return;

            // If there is no path, then the operation represents a write to the full
            // storage location, and thus guarantees that it is in a fully-initialized
            // state after this point, even if it wasn't before.
            //
            if (!path)
            {
                ioStorageLocationState = InitializationState::knownFullyInitialized();
            }
            else
            {
                // If there is a path, then we will follow it in order to refine the location
                // that will be written to...
                switch (path->inst->getOp())
                {
                default:
                    _sink->diagnose(path->inst, Diagnostics::unimplemented, "missing case in `updateStorageLocationInitializationStateBasedOnWrite`");
                    break;

                case kIROp_FieldAddress:
                    {
                        // We need to adjust the state for the location to store per-field information,
                        // so that we can update just the field that is being written...
                        //
                        auto fieldAddrInst = cast<IRFieldAddress>(path->inst);
                        auto fieldKey = fieldAddrInst->getField();

                        InitializationState fieldState = ioStorageLocationState.getFieldState(fieldKey);
                        updateStorageLocationInitializationStateBasedOnWrite(
                            fieldState,
                            path->parent);

                        // TODO(tfoley): We need to map the IR-level `struct` type back to its
                        // AST-level analogue, and then determine whether field-by-field
                        // initialization is allowed for that type, or not.
                        //
                        // The default behavior should be that field-by-field initialization
                        // is not allowed.
                        //

                        IRStructType* structType = nullptr;
                        auto structPtrInst = fieldAddrInst->getBase();
                        if (auto structPtrType = as<IRPtrTypeBase>(structPtrInst->getDataType()))
                        {
                            IRInst* valueType = structPtrType->getValueType();
                            valueType = maybeGetUnspecializedValue(valueType);
                            structType = as<IRStructType>(valueType);
                        }

                        ioStorageLocationState.setFieldState(structType, fieldKey, fieldState);
                    }
                    break;

                case kIROp_GetElementPtr:
                {
                    auto elementAddrInst = cast<IRGetElementPtr>(path->inst);
                    auto elementIndex = elementAddrInst->getIndex();

                    InitializationState elementState = ioStorageLocationState.getElementState(elementIndex);
                    updateStorageLocationInitializationStateBasedOnWrite(
                        elementState,
                        path->parent);
                    ioStorageLocationState.setElementState(elementIndex, elementState);
                }
                break;
                }
            }
        }

        void handleLoad(BlockInfo& info, IRLoad* load)
        {
            // TODO: check the state for whatever is being loaded.
            //
            auto srcStorageLocation = load->getPtr();
            handleRead(info, load, srcStorageLocation);
        }

        void handleVarOrParamRead(BlockInfo const& info, AccessKind access, IRInst* initiatingInst, IRInst* storageLocation, AccessPath const* path = nullptr)
        {
            if (mode != Mode::CheckingBehavior)
                return;

            auto storageLocationStatePtr = info.stateOfStorageLocationsAtEndOfBlock.tryGetValue(storageLocation);
            if (!storageLocationStatePtr)
                return;
            auto& storageLocationState = *storageLocationStatePtr;

            switch (storageLocationState.getTag())
            {
            case InitializationState::Tag::KnownFullyInitialized:
                return;

            default:
                break;
            }

            // We start by diagnosing the problem, based on the context in
            // which the access occured.
            //
            switch (access)
            {
            case AccessKind::Return:
                _sink->diagnose(initiatingInst, Diagnostics::returningWithUninitializedOut, storageLocation);
                break;

            default:
                _sink->diagnose(initiatingInst, Diagnostics::usingUninitializedVariable, storageLocation);
                break;
            }

            // TODO: start taking the `path` into account, since the code might
            // only be accessing a specific field or element, and we should note
            // when the element is initialized but the aggregate isn't...
        }

        void handleReturn(BlockInfo& info, IRReturn* returnInst)
        {
            // TODO: check the state of all `out` parameters here.
            //

            // Basically we just want to loop over all of the `out` parameters of the
            // function and effectively perform a "read" access to them here...
            //
            for (auto param : _func->getParams())
            {
                auto paramType = param->getDataType();
                auto paramOutType = as<IROutType>(paramType);
                if (!paramOutType)
                    continue;

                handleAccess(info, AccessKind::Return, returnInst, param);
            }
        }

        bool overwriteInitializationInfoForBlock(IRBlock* block, BlockInfo const& newInfo)
        {
            auto& oldInfo = _mapBlockToInfo[block];

            // If the new state is the same as the state that was already being
            // tracked, then we don't need to update anything.
            //
            if (areBlockStatesEqual(oldInfo, newInfo))
                return false;

            oldInfo = newInfo;

            return false;
        }

        bool areBlockStatesEqual(BlockInfo const& left, BlockInfo const& right)
        {
            for (auto rightPair : right.stateOfStorageLocationsAtEndOfBlock)
            {
                auto key = rightPair.first;
                if (!left.stateOfStorageLocationsAtEndOfBlock.containsKey(key))
                    return false;
            }

            for (auto leftPair : left.stateOfStorageLocationsAtEndOfBlock)
            {
                auto& leftVal = leftPair.second;
                auto key = leftPair.first;

                auto rightValPtr = right.stateOfStorageLocationsAtEndOfBlock.tryGetValue(key);
                if (!rightValPtr)
                    return false;
                auto& rightVal = *rightValPtr;

                if (leftVal != rightVal)
                    return false;
            }

            return true;
        }

        void checkInitializationBehaviorForGlobalVar(IRGlobalVar* func)
        {

        }

    };

#if 0

static bool isMetaOp(IRInst* inst)
{
    switch (inst->getOp())
    {
    // These instructions only look at the parameter's type,
    // so passing an undefined value to them is permissible
    case kIROp_IsBool:
    case kIROp_IsInt:
    case kIROp_IsUnsignedInt:
    case kIROp_IsSignedInt:
    case kIROp_IsHalf:
    case kIROp_IsFloat:
    case kIROp_IsVector:
    case kIROp_GetNaturalStride:
    case kIROp_TypeEquals:
        return true;
    default:
        break;
    }

    return false;
}

static bool isUninitializedValue(IRInst* inst)
{
    // Also consider var since it does not
    // automatically mean it will be initialized
    // (at least not as the user may have intended)
    return (as<IRUndefined>(inst) || (inst->m_op == kIROp_Var));
}

static bool isUnmodifying(IRFunc* func)
{
    auto intr = func->findDecoration<IRIntrinsicOpDecoration>();
    return (intr && intr->getIntrinsicOp() == kIROp_Unmodified);
}

enum ParameterCheckType
{
    Never,  // Parameter does NOT to be checked for uninitialization (e.g. is `in` or special type)
    AsOut,  // Parameter DOES need to be checked for usage before initializations
    AsInOut // Parameter DOES need to be checked to see if it is ever written to
};

static ParameterCheckType isPotentiallyUnintended(IRParam* param, Stage stage, int index)
{
    IRType* type = param->getFullType();
    if (auto out = as<IROutType>(param->getFullType()))
    {
        // Don't check `out Vertices<T>` or `out Indices<T>` parameters
        // in mesh shaders.
        // TODO: we should find a better way to represent these mesh shader
        // parameters so they conform to the initialize before use convention.
        // For example, we can use a `OutputVetices` and `OutputIndices` type
        // to represent an output, like `OutputPatch` in domain shader.
        // For now, we just skip the check for these parameters.
        switch (out->getValueType()->getOp())
        {
        case kIROp_VerticesType:
        case kIROp_IndicesType:
        case kIROp_PrimitivesType:
            return Never;
        default:
            break;
        }

        return AsOut;
    }
    else if (auto inout = as<IRInOutType>(type))
    {
        // TODO: some way to check if the method
        // is actually used for autodiff
        if (as<IRDifferentialPairUserCodeType>(inout->getValueType()))
            return Never;

        switch (stage)
        {
        case Stage::AnyHit:
        case Stage::ClosestHit:
            // In HLSL the payload is required to be `inout`
            return (index == 0) ? Never : AsInOut;
        case Stage::Geometry:
            // Second parameter is the triangle stream
            return (index == 1) ? Never : AsInOut;
        default:
            break;
        }

        return AsInOut;
    }

    return Never;
}

static bool isAliasable(IRInst* inst)
{
    switch (inst->getOp())
    {
    // These instructions generate (implicit) references to inst
    case kIROp_FieldExtract:
    case kIROp_FieldAddress:
    case kIROp_GetElement:
    case kIROp_GetElementPtr:
    case kIROp_InOutImplicitCast:
        return true;
    default:
        break;
    }

    return false;
}

static bool isDifferentiableFunc(IRInst* func)
{
    for (auto decor = func->getFirstDecoration(); decor; decor = decor->getNextDecoration())
    {
        switch (decor->getOp())
        {
        case kIROp_ForwardDerivativeDecoration:
        case kIROp_ForwardDifferentiableDecoration:
        case kIROp_BackwardDerivativeDecoration:
        case kIROp_BackwardDifferentiableDecoration:
        case kIROp_UserDefinedBackwardDerivativeDecoration:
            return true;
        default:
            break;
        }
    }

    return false;
}

// The `upper` field contains the struct that the type is
// is contained in. It is used to check for empty structs.
static bool canIgnoreType(IRType* type, IRType* upper)
{
    // In case specialization returns a function instead
    if (!type)
        return true;

    if (as<IRVoidType>(type))
        return true;

    // For structs, ignore if its empty
    if (auto str = as<IRStructType>(type))
    {
        int count = 0;
        for (auto field : str->getFields())
        {
            IRType* ftype = field->getFieldType();
            count += !canIgnoreType(ftype, type);
        }

        return (count == 0);
    }

    // Nothing to initialize for a pure interface
    if (as<IRInterfaceType>(type))
        return true;

    // We don't know what type it will be yet.
    if (as<IRParam>(type))
        return true;

    // For pointers, check the value type (primarily for globals)
    if (auto ptr = as<IRPtrType>(type))
    {
        // Avoid the recursive step if its a
        // recursive structure like a linked list
        IRType* ptype = ptr->getValueType();
        if (auto resolvedType = as<IRType>(getResolvedInstForDecorations(ptype)))
            ptype = resolvedType;
        return (ptype != upper) && canIgnoreType(ptype, upper);
    }

    // In the case of specializations, check returned type
    if (auto spec = as<IRSpecialize>(type))
    {
        IRInst* inner = getResolvedInstForDecorations(spec);
        IRType* innerType = (IRType*)(inner);
        return canIgnoreType(innerType, upper);
    }

    return false;
}

static List<IRInst*> getAliasableInstructions(IRInst* inst)
{
    List<IRInst*> addresses;

    addresses.add(inst);
    for (auto use = inst->firstUse; use; use = use->nextUse)
    {
        IRInst* user = use->getUser();

        // Meta instructions only use the argument type
        if (isMetaOp(user) || !isAliasable(user))
            continue;

        addresses.addRange(getAliasableInstructions(user));
    }

    return addresses;
}

enum InstructionUsageType
{
    None,        // Instruction neither stores nor loads from the soruce (e.g. meta operations)
    Store,       // Instruction acts as a write to the source
    StoreParent, // Instruction's parent acts as a write to the source
    Load         // Instruciton acts as a load from the source
};

static InstructionUsageType getCallUsageType(IRCall* call, IRInst* inst)
{
    IRInst* callee = call->getCallee();

    // Resolve the actual function
    IRFunc* ftn = nullptr;
    IRFuncType* ftype = nullptr;
    if (auto spec = as<IRSpecialize>(callee))
        ftn = as<IRFunc>(getResolvedInstForDecorations(spec));

    // Differentiable functions are mostly ignored, treated as having inout parameters
    else if (as<IRForwardDifferentiate>(callee))
        return Store;
    else if (as<IRBackwardDifferentiate>(callee))
        return Store;

    else if (auto wit = as<IRLookupWitnessMethod>(callee))
        ftype = as<IRFuncType>(wit->getFullType());
    else
        ftn = as<IRFunc>(callee);

    // Find the argument index so we can fetch the type
    int index = 0;

    auto args = call->getArgsList();
    for (int i = 0; i < args.getCount(); i++)
    {
        if (args[i] == inst)
        {
            index = i;
            break;
        }
    }

    if (ftn)
        ftype = as<IRFuncType>(ftn->getFullType());

    if (!ftype)
        return None;

    // Consider it as a store if its passed
    // as an out/inout/ref parameter
    auto type = unwrapAttributedType(ftype->getParamType(index));
    return (as<IROutType>(type) || as<IRInOutType>(type) || as<IRRefType>(type)) ? Store : Load;
}

static InstructionUsageType getInstructionUsageType(IRInst* user, IRInst* inst)
{
    // Meta intrinsics (which evaluate on type) do nothing
    if (isMetaOp(user))
        return None;

    // Ignore instructions generating more aliases
    if (isAliasable(user))
        return None;

    switch (user->getOp())
    {
    case kIROp_Loop:
    case kIROp_UnconditionalBranch:
        // TODO: Ignore branches for now
        return None;

    case kIROp_Call:
        // Function calls can be either
        // stores or loads depending on
        // whether the callee takes it
        // in as a out parameter or not
        return getCallUsageType(as<IRCall>(user), inst);

    // These instructions will store data...
    case kIROp_Store:
    case kIROp_SwizzledStore:
    case kIROp_SPIRVAsm:
    case kIROp_AtomicStore:
        return Store;

    case kIROp_SPIRVAsmOperandInst:
        // For SPIRV asm instructions, need to check out the entire
        // block when doing reachability checks
        return StoreParent;

    case kIROp_MakeExistential:
    case kIROp_MakeExistentialWithRTTI:
        // For specializing generic structs
        return Store;

    // Miscellaenous cases
    case kIROp_ManagedPtrAttach:
    case kIROp_Unmodified:
        return Store;

    default:
        // Default case is that if the instruction is a pointer, it
        // is considered a store, otherwise a load.
        if (as<IRPtrTypeBase>(user->getDataType()))
            return Store;
        return Load;
    }
}

static void collectSpecialCaseInstructions(List<IRInst*>& stores, IRBlock* block)
{
    for (auto inst = block->getFirstInst(); inst; inst = inst->next)
    {
        if (as<IRGenericAsm>(inst))
            stores.add(inst);
    }
}

static void collectInstructionByUsage(
    List<IRInst*>& stores,
    List<IRInst*>& loads,
    IRInst* user,
    IRInst* inst)
{
    InstructionUsageType usage = getInstructionUsageType(user, inst);
    switch (usage)
    {
    case Load:
        return loads.add(user);
    case Store:
        return stores.add(user);
    case StoreParent:
        return stores.add(user->getParent());
    }
}

static void cancelLoads(
    ReachabilityContext& reachability,
    const List<IRInst*>& stores,
    List<IRInst*>& loads)
{
    // Remove all loads which are reachable from stores
    for (auto store : stores)
    {
        for (Index i = 0; i < loads.getCount();)
        {
            if (reachability.isInstReachable(store, loads[i]))
                loads.fastRemoveAt(i);
            else
                i++;
        }
    }
}

static void collectAliasableLoadStores(IRInst* inst, List<IRInst*>& stores, List<IRInst*>& loads)
{
    auto addresses = getAliasableInstructions(inst);

    for (auto alias : addresses)
    {
        // TODO: Mark specific parts assigned to for partial initialization checks
        for (auto use = alias->firstUse; use; use = use->nextUse)
            collectInstructionByUsage(stores, loads, use->getUser(), alias);
    }
}

static List<IRInst*> getUnresolvedParamLoads(
    ReachabilityContext& reachability,
    IRFunc* func,
    IRInst* inst)
{
    // Partition instructions
    List<IRInst*> stores;
    List<IRInst*> loads;

    collectAliasableLoadStores(inst, stores, loads);

    // Special cases for parameters
    for (const auto& b : func->getBlocks())
    {
        collectSpecialCaseInstructions(stores, b);

        auto t = b->getTerminator();
        if (as<IRReturn>(t))
            loads.add(t);
    }

    cancelLoads(reachability, stores, loads);

    return loads;
}

static List<IRInst*> getUnresolvedVariableLoads(ReachabilityContext& reachability, IRInst* inst)
{
    // Partition instructions
    List<IRInst*> stores;
    List<IRInst*> loads;

    collectAliasableLoadStores(inst, stores, loads);

    cancelLoads(reachability, stores, loads);

    return loads;
}

static bool isInstStoredInto(ReachabilityContext& reachability, IRInst* reference, IRInst* inst)
{
    List<IRInst*> stores;
    List<IRInst*> loads;

    for (auto alias : getAliasableInstructions(inst))
    {
        for (auto use = alias->firstUse; use; use = use->nextUse)
            collectInstructionByUsage(stores, loads, use->getUser(), alias);
    }

    for (auto store : stores)
    {
        if (reachability.isInstReachable(store, reference))
            return true;
    }

    return false;
}

static IRInst* traceInstOrigin(IRInst* inst)
{
    if (auto load = as<IRLoad>(inst))
        return traceInstOrigin(load->getPtr());

    return inst;
}


static bool isDirectlyWrittenTo(IRInst* inst)
{
    for (auto use = inst->firstUse; use; use = use->nextUse)
    {
        InstructionUsageType usage = getInstructionUsageType(use->getUser(), inst);
        if (usage == Store || usage == StoreParent)
            return true;
    }

    return false;
}

static List<IRStructField*> checkFieldsFromExit(
    ReachabilityContext& reachability,
    IRReturn* ret,
    IRStructType* type)
{
    IRInst* origin = traceInstOrigin(ret->getVal());

    // We don't want to warn on delegated construction
    if (!isUninitializedValue(origin))
        return {};

    // Check if the origin instruction is ever written to
    if (isDirectlyWrittenTo(origin))
        return {};

    // Now we can look for all references to fields
    HashSet<IRStructKey*> usedKeys;
    for (auto use = origin->firstUse; use; use = use->nextUse)
    {
        IRInst* user = use->getUser();

        auto fieldAddress = as<IRFieldAddress>(user);
        if (!fieldAddress || !isInstStoredInto(reachability, ret, user))
            continue;

        IRInst* field = fieldAddress->getField();
        usedKeys.add(as<IRStructKey>(field));
    }

    List<IRStructField*> uninitializedFields;

    auto fields = type->getFields();
    for (auto field : fields)
    {
        if (canIgnoreType(field->getFieldType(), nullptr))
            continue;

        if (!usedKeys.contains(field->getKey()))
            uninitializedFields.add(field);
    }

    return uninitializedFields;
}

static void checkConstructor(IRFunc* func, ReachabilityContext& reachability, DiagnosticSink* sink)
{
    auto constructor = func->findDecoration<IRConstructorDecoration>();
    if (!constructor)
        return;

    IRStructType* stype = as<IRStructType>(func->getResultType());
    if (!stype)
        return;

    // Don't bother giving warnings if its not being used
    bool synthesized = constructor->getSynthesizedStatus();
    if (synthesized && !func->firstUse)
        return;

    auto printWarnings = [&](const List<IRStructField*>& fields, IRReturn* ret)
    {
        for (auto field : fields)
        {
            if (synthesized)
            {
                sink->diagnose(
                    field->getKey(),
                    Diagnostics::fieldNotDefaultInitialized,
                    stype,
                    field->getKey());
            }
            else
            {
                sink->diagnose(ret, Diagnostics::constructorUninitializedField, field->getKey());
            }
        }
    };

    // Work backwards, get exit points and find sources
    for (auto block : func->getBlocks())
    {
        for (auto inst = block->getFirstInst(); inst; inst = inst->next)
        {
            auto ret = as<IRReturn>(inst);
            if (!ret)
                continue;

            auto fields = checkFieldsFromExit(reachability, ret, stype);
            printWarnings(fields, ret);
        }
    }
}

static void checkParameterAsOut(
    ReachabilityContext& reachability,
    IRFunc* func,
    IRParam* param,
    DiagnosticSink* sink)
{
    auto loads = getUnresolvedParamLoads(reachability, func, param);
    for (auto load : loads)
    {
        sink->diagnose(
            load,
            as<IRTerminatorInst>(load) ? Diagnostics::returningWithUninitializedOut
                                       : Diagnostics::usingUninitializedOut,
            param);
    }
}

static void checkUninitializedValues(IRFunc* func, DiagnosticSink* sink)
{
    // Differentiable functions will generate undefined values
    // strictly so that they can be set in a differentiable way
    if (isDifferentiableFunc(func))
        return;

    auto firstBlock = func->getFirstBlock();
    if (!firstBlock)
        return;

    ReachabilityContext reachability(func);

    // Used for a further analysis and to skip usual return checks
    auto constructor = func->findDecoration<IRConstructorDecoration>();

    // Special checks for stages e.g. raytracing shader
    Stage stage = Stage::Unknown;
    if (auto entry = func->findDecoration<IREntryPointDecoration>())
        stage = entry->getProfile().getStage();

    // Check out parameters
    if (!isUnmodifying(func))
    {
        int index = 0;
        for (auto param : firstBlock->getParams())
        {
            ParameterCheckType checkType = isPotentiallyUnintended(param, stage, index);
            if (checkType == AsOut)
                checkParameterAsOut(reachability, func, param, sink);
            index++;
        }
    }

    // Check ordinary instructions
    for (auto block : func->getBlocks())
    {
        for (auto inst = block->getFirstInst(); inst; inst = inst->getNextInst())
        {
            if (!isUninitializedValue(inst))
                continue;

            // This will be looked into later
            if (constructor && isReturnedValue(inst))
                continue;

            IRType* type = inst->getFullType();
            if (canIgnoreType(type, nullptr))
                continue;

            auto loads = getUnresolvedVariableLoads(reachability, inst);
            for (auto load : loads)
            {
                sink->diagnose(load, Diagnostics::usingUninitializedVariable, inst);
            }
        }
    }

    // Separate analysis for constructors
    checkConstructor(func, reachability, sink);
}

static void checkUninitializedGlobals(IRGlobalVar* variable, DiagnosticSink* sink)
{
    IRType* type = variable->getFullType();
    if (canIgnoreType(type, nullptr))
        return;

    // Check for semantic decorations
    // (e.g. globals like gl_GlobalInvocationID)
    if (variable->findDecoration<IRSemanticDecoration>())
        return;

    if (variable->findDecoration<IRGlobalInputDecoration>())
        return;

    if (variable->findDecoration<IRVulkanHitAttributesDecoration>())
        return;

    // Check for initialization blocks
    for (auto inst : variable->getChildren())
    {
        if (as<IRBlock>(inst))
            return;
    }

    auto addresses = getAliasableInstructions(variable);

    List<IRInst*> loads;
    for (auto alias : addresses)
    {
        for (auto use = alias->firstUse; use; use = use->nextUse)
        {
            InstructionUsageType usage = getInstructionUsageType(use->getUser(), alias);
            if (usage == Store || usage == StoreParent)
                return;

            if (usage == Load)
                loads.add(use->getUser());
        }
    }

    for (auto load : loads)
    {
        sink->diagnose(load, Diagnostics::usingUninitializedGlobalVariable, variable);
    }
}

void checkForUsingUninitializedValues(IRModule* module, DiagnosticSink* sink)
{
    for (auto inst : module->getGlobalInsts())
    {
        if (auto func = as<IRFunc>(inst))
        {
            checkUninitializedValues(func, sink);
        }
        else if (auto generic = as<IRGeneric>(inst))
        {
            auto retVal = findGenericReturnVal(generic);
            if (auto funcVal = as<IRFunc>(retVal))
                checkUninitializedValues(funcVal, sink);
        }
        else if (auto global = as<IRGlobalVar>(inst))
        {
            checkUninitializedGlobals(global, sink);
        }
    }
}

#endif

void checkForUsingUninitializedValues(//
    IRModule* module,
    DiagnosticSink* sink)
{
    InitializationCheckingContext context(module, sink);
    context.checkInitializationRec(module->getModuleInst());
}

} // namespace Slang
