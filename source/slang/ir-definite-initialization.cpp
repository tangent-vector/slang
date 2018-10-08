// ir-definite-initialization.cpp
#include "ir-definite-initialization.h"

#include "ir.h"
#include "ir-insts.h"

namespace Slang {

struct SharedCheckDefiniteInitializationContext
{
    IRModule* module;
    IRModule* getModule() { return module; }

    DiagnosticSink* sink;
    DiagnosticSink* getSink() { return sink; }

    SharedIRBuilder sharedBuilder;
    Session* getSession() { return sharedBuilder.session; }

    IRBuilder builder;
    IRBuilder* getBuilder() { return &builder; }

    MemoryArena arena;
    MemoryArena* getArena() { return &arena; }

};

struct CheckDefiniteInitializationContext
{
    SharedCheckDefiniteInitializationContext* shared;

    IRModule* getModule() { return shared->getModule(); }
    DiagnosticSink* getSink() { return shared->getSink(); }
    Session* getSession() { return shared->getSession(); }
    IRBuilder* getBuilder() { return shared->getBuilder(); }
    MemoryArena* getArena() { return shared->getArena(); }

    IRGlobalValueWithCode* code;
    IRInst* varToCheck;


        /// The initialization state of a variable.
        ///
        /// A null `VarInitializationState*` represents
        /// no uninitialization. Non-null states use the
        /// `flavor` field to distinguish fully-initialized
        /// and partially-initialized cases. The partially-initialized
        /// state tracks additional information for individual
        /// fields or elements of a value.
        ///
    struct VarInitializationState
    {
        enum class Flavor
        {
            FullyInitialized,
            PartiallyInitialized,
        };

        Flavor flavor;

        // TODO: tracking data for partial initialization case
    };

        /// Initialization state for a block.
    struct BlockInitializationState
    {
        Dictionary<IRInst*, VarInitializationState*> mapVarToState;
    };

    struct BlockInfo : RefObject
    {
        // What is the initialization state of the various variables
        // on input to this block?
        BlockInitializationState inputState;

        // What is the effect of this block on the initialization state
        // of the variables?
        BlockInitializationState effect;

        // What is the initialization state on output from this block?
        BlockInitializationState outputState;
    };

    Dictionary<IRBlock*, RefPtr<BlockInfo>> mapBlockToInfo;

    BlockInfo* getBlockInfo(
        IRBlock*                            block)
    {
        RefPtr<BlockInfo> blockInfo;
        if(mapBlockToInfo.TryGetValue(block, blockInfo))
            return blockInfo;

        SLANG_UNEXPECTED("no information for block");
        UNREACHABLE_RETURN(nullptr);
    }

    VarInitializationState* applyWritePath(
        VarInitializationState*             oldState,
        List<IRInst*> const&                path)
    {
        // Writing into a fully initialized variable doesn't change its initialization state.
        if(oldState && oldState->flavor == VarInitializationState::Flavor::FullyInitialized)
            return oldState;

        // If the `path` is empty, then this is a full-variable write, and so
        // the result should be a fully initialized variable.
        if( path.Count() == 0 )
        {
            VarInitializationState* newState = getArena()->allocate<VarInitializationState>();
            newState->flavor = VarInitializationState::Flavor::FullyInitialized;
            return newState;
        }

        SLANG_UNIMPLEMENTED_X("partial initialization support");
        UNREACHABLE_RETURN(nullptr);
    }

        /// Record the initialization of a given `var` within a block.
        ///
        /// This function makes a note that the variable `var` was written
        /// to by `inst`, using the given `path`, within the block tracked by `blockState`.
    void addInitForVarAndPath(
        BlockInitializationState*           blockState,
        IRInst*                             var,
        List<IRInst*> const&                path)
    {
        // Look up any existing state for the variable already in this block.
        VarInitializationState* oldState = nullptr;
        blockState->mapVarToState.TryGetValue(var, oldState);

        // Apply the new path to the old state to generate a new state
        VarInitializationState* newState = applyWritePath(
            oldState,
            path);
        blockState->mapVarToState[var] = newState;
    }
/*
    void checkDefiniteInitialization(
        IRGlobalValueWithCode*              code)
    {
        // We want to determine what variables (including both local `IRVar`s,
        // and also parameters with `out` or `inout` qualification) are assigned
        // inside of each block, and then iterate over the CFG to determine
        // what is guaranteed to be initialized on input to each block.
        //
        // For aggregate values (`struct` types, etc.) we will track the initialization
        // state on a per-field basis, recursively. Thus a sequence of assignments
        // that write to the fields of a `struct` type variable will count as
        // definite assignment of the whole struct.
        //
        // For scalar variables we will only track things at the granularity of
        // the whole variable. Systems like LLVM support more fine-grained tracking
        // of initialization status at the bit level, but the benefits of this
        // for existing shader code would be minimal.
        //
        // For arrays, we could conceivably do a best-effort analysis to see
        // if per-element assignments end up initializing the whole array, but
        // eventually we need to deal with assignments to an unknown location
        // at which point we lose full information and have to treat those
        // assignments as non-definite.
        //
        // Once we've converged the information on entry/exit of each block,
        // we can then do a per-block scan to track the state within the block,
        // and to see if there are any loads from a variable that is not
        // definitely initialized at that point.
        //
        //



        // We will start by identifying the variables we will care about for this analysis.
        //
        // This includes all local variables in all blocks, so we collect those with
        // a linear scan over all of the blocks and instructions.
        //
        List<IRInst*> vars;
        for( auto block : code->getBlocks() )
        {
            for( auto inst : block->getChildren() )
            {
                auto var = as<IRVar>(inst);
                if(!var) continue;

                vars.Add(var);
            }
        }
        //
        // We also care about `out` and `in out` parameters of the function,
        // which will always be the parameters of its first basic block.
        //
        if( auto block = code->getFirstBlock() )
        {
            for( auto param : block->getParams() )
            {
                // We check if the type of the parameter is the special
                // pointer sub-types that represent `out` or `in out`
                // parameters.
                //
                auto paramType = param->getDataType();
                if( as<IROutTypeBase>(paramType) )
                {
                    vars.Add(param);
                }
            }
        }

        // Now that we've identified the variables we care about,
        // we can walk through each block and determine the consequences
        // of executing that block on the initialization state
        // of those variables.
        //
        for( auto block : code->getBlocks() )
        {
            // We sill store the initialization state of each block as
            // a dense mapping from the indices of our variables to
            // their initialization state as produced by the block.
            //
            // This means we need to allocate a few arrays to track
            // things, and we will use a memory arena to do it so that
            // we don't have to do fine-grained cleanup when we are done.
            //
            // Note: this allocation means that the time/space complexity
            // of this analysis pass ends up being O(B*V) where B is
            // the number of blocks and V is the number of variables.
            // If this starts to be a bottleneck we can do more sparse
            // allocation, and only have each block record information
            // on the variables it actually touches.

            RefPtr<BlockInfo> blockInfo = new BlockInfo();
            mapBlockToInfo.Add(block, blockInfo);

            for( auto inst : block->getChildren() )
            {
                // We will inspect each instruction to try and
                // determine what impact it has on the state
                // of our variables.
                //
                switch( inst->op )
                {
                default:
                    // By default we will assume that an instruction has
                    // no impact on the state of any variables; things that
                    // were defined stay defined, and things that were
                    // undefined stay undefined.
                    break;

                case kIROp_Store:
                    {
                        // We assume that the value being stored represents
                        // something well-defined (or else upstream code should
                        // produce a warning), so all that we need to
                        // do is chase down what we are storing into.
                        //
                        auto storeInst = cast<IRStore>(inst);
                        auto addrOperand = storeInst->getAddr();

                        // A `store` might target a variable, or a "path" into
                        // a variable, based on its pointer argument. We will
                        // "walk" down the use-def chains to see if we bottom
                        // out at one of our interesting variables.
                        //
                        auto addr = addrOperand;
                        List<IRInst*> pathInfo;
                        for(;;)
                        {
                            switch( addr->op )
                            {
                            default:
                                // If we run into an instruction type we don't
                                // understand along the way, then we assume that
                                // the instructions that produced the destination
                                // address were too complex for us to analyze.
                                break;

                            case kIROp_Var:
                                {
                                    // We found a variable at the end of the chain,
                                    // so we know we are storing into something potentially
                                    // interesting.
                                    auto var = cast<IRVar>(addr);
                                    addInitForVarAndPath(&blockInfo->effect, var, pathInfo);
                                }
                                break;
                            }
                        }

                    }
                    break;

                }

            }
        }

        // Once we've figured out the impact of each block on the initialization
        // state of the various variables, we are set up to be able to iteratively
        // solve the dataflow equation.
        //
        // The initial condition here will be what is or isn't initialized on
        // entry to the function, so we start by scanning through the parameters
        // of the first block.
        //
        if( auto block = code->getFirstBlock() )
        {
            auto blockInfo = getBlockInfo(block);
            for( auto param : block->getParams() )
            {
                // We don't care about `in` parameters here, even if they have
                // pointer types (that kind of analysis is beyond our aspirations).
                // We only look for `out` and `in out` parameters.
                //
                auto paramType = param->getDataType();
                if( as<IRInOutType>(paramType) )
                {
                    // An `in out` variable should be assumed to be fully initialized
                    // on input to the first block.
                    addInitForVarAndPath(&blockInfo->inputState, param, List<IRInst*>());
                }

                // We check if the type of the parameter is the special
                // pointer sub-types that represent `out` or `in out`
                // parameters.
                //
                if( as<IROutTypeBase>(paramType) )
                {
                    vars.Add(param);
                }
            }
        }
        //
        // We will now iteratively apply our dataflow rules, and stop once we
        // reach an iteration where nothing changes.
        //
        bool anyChanges = true;
        while(anyChanges)
        {
            anyChanges = false;

            // For each block, the initialization state of the variables on its input
            // should be the the "intersection" of what is initialized on output
            // from all its predecessor blocks.
            //
            for( auto block : code->getBlocks() )
            {
                // The one exception here is the first block, which was handled already.
                //
                if(block == code->getFirstBlock()) continue;

                //
            }

        }

        // Now that we've identified the variables we care about,
        // we can walk through each block and determine the consequences
        // of executing that block on the initialization state
        // of those variables.
        //


    }
*/

    void collectReadAccess(IRInst* inst, IRInst* val)
    {
        SLANG_UNUSED(inst);
        SLANG_UNUSED(val);
    }

    void collectWriteAccess(IRInst* inst, IRInst* val)
    {
        SLANG_UNUSED(inst);
        SLANG_UNUSED(val);
    }

    void collectUnknownAccess(IRInst* inst, IRInst* val)
    {
        SLANG_UNUSED(inst);
        SLANG_UNUSED(val);
    }

    void collectAccessForUse(
        IRInst*         inst,
        IRUse*          use,
        List<IRInst*>&  workList)
    {
        SLANG_UNUSED(workList);

        auto user = use->getUser();
        switch( user->op )
        {
        default:
            break;

        case kIROp_Load:
            // A load will count as a use of the value being pointed to.
            // At some point we could try to get a bit more clever
            // and recognize if subsequent use of the loaded value
            // only touches a subset of fields/elements.
            collectReadAccess(user, inst);
            return;

        case kIROp_Store:
            // Quickly check if the use of `inst` is the address being
            // stored into.
            if( use == &user->getOperands()[0] )
            {
                // A store into `inst` counts as a write of the entire value.
                collectWriteAccess(user, inst);
                return;
            }
            break;

        // TODO: getFieldPtr, getElementPtr, etc.
        }

        // Any instruction we don't handle specially will be
        // treated as an "escape" of the variable, which might
        // access it in unknown ways outside of our knowledge/control.
        collectUnknownAccess(user, inst);
    }

    void checkDefiniteInitializationForLocalVariable()
    {
        getArena()->reset();

        // Start block is the block that defines the variable,
        // end blocks are those that use the variable in
        // potential read positions.

        List<IRInst*> workList;
        workList.Add(varToCheck);

        while( workList.Count() != 0 )
        {
            // Take an instruction that represents a pointer to,
            // or into our variable, and process its users.
            IRInst* inst = workList[0];
            workList.FastRemoveAt(0);

            for( auto use = inst->firstUse; use; use = use->nextUse )
            {
                // We need to identify uses that are relevant to the analysis.
                collectAccessForUse(inst, use, workList);
            }
        }

        // Now that we've collected a list of accesses, and the
        // blocks that contain them, we will try to solve for
        // the iterative effect of all those accesses.
    }

    void checkDefiniteInitializationForOutParameter()
    {
        getArena()->reset();

        // Start block is the entry block (which defines the parameter),
        // and end blocks are those with `return` instructions.

        for( auto block : code->getBlocks() )
        {
            if( as<IRReturn>(block->getTerminator()) )
            {
                // The `return` instruction should count as a use/read of the entire value.
            }
        }
    }
};

static IRInst* pickEarlierInst(IRInst* left, IRInst* right)
{
    // We want to find which of `left` and `right` is earlier
    // in a preorder walk of the IR module.

    // Start with some simply sanity checks: always take the non-null one
    if(!left) return right;
    if(!right) return left;

    // Now we will need to find the level of the hierarchy where the two meet.
    // The first step is to figure out how deep each instruction is:
    int leftDepth = 0;
    for(auto ii = left; ii; ii = ii->getParent())
        leftDepth++;

    int rightDepth = 0;
    for(auto ii = right; ii; ii = ii->getParent())
        rightDepth++;

    // We will then move to the minimum depth of the two as the
    // start of our search, knowing that a deeper instruction
    // should appear after its ancestors in the preorder.
    int minDepth = leftDepth < rightDepth ? leftDepth : rightDepth;

    // Now we will construct the ancestor of the two instructions.
    auto ll = left;
    for(int ii = minDepth; ii < leftDepth; ++ii)
        ll = ll->getParent();

    auto rr = right;
    for(int ii = minDepth; ii < rightDepth; ++ii)
        rr = rr->getParent();

    // If at this point our cursors are already at a common parent,
    // then it implies one of the nodes was the parent of the other.
    if( ll == rr )
    {
        if( ll == right )
        {
            return right;
        }
        else
        {
            SLANG_ASSERT(rr == left);
            return left;
        }
    }

    // Otherwise, we need to proceed until we find a case where
    // our two cursors have a common parent:
    while( ll->getParent() != rr->getParent() )
    {
        ll = ll->getParent();
        rr = rr->getParent();

        // We expect this to always pass until we find nodes
        // with a common parent, so long as everything is
        // in a single tree.
        SLANG_ASSERT(ll);
        SLANG_ASSERT(rr);

        // As an insurance policy, bail out on null pointers
        if(!ll) return right;
        if(!rr) return left;
    }

    // Okay, our cursors are two nodes with the same parent.
    // Now we just need to figure out which one of them
    // comes first in the common parent.

    // Now we scan forward from each of the two cursors
    // using a "finger" for each. The first finger to run
    // into something interesting tells us what is going on.
    auto lf = ll;
    auto rf = rr;

    for(;;)
    {
        // Left finger hit right cursor -> left node is first
        if(lf == rr) return left;

        // Right finger hit left cursor -> right node is first
        if(rf == ll) return right;

        // Left finger hit end of parent -> right node is first
        if(!lf) return right;

        // Right finger hit end of parent -> left node is first
        if(!rf) return left;

        lf = lf->getNextInst();
        rf = rf->getNextInst();
    }
}

static IRInst* pickBestUser(IRInst* left, IRInst* right)
{
    if(!left) return right;
    if(!right) return left;

    // Favor a user that isn't a block argument
    if(as<IRUnconditionalBranch>(left)) return right;
    if(as<IRUnconditionalBranch>(right)) return left;

    // Favor a user that has location info
    if(left->sourceLoc.isValid()) return left;
    if(right->sourceLoc.isValid()) return right;

    // Otherwise pick the user that comes "first" in the overall IR module
    return pickEarlierInst(left, right);
}

#if 0
static void diagnoseUseOfUndefinedVariable(
    IRInst*                 value,
    DiagnosticSink*         sink,
    DiagnosticInfo const&   diagnostic)
{
    auto bestUser = pickBestUser(value);
    if( !bestUser ) return;

    auto highLevelDeclDecoration = value->findDecoration<IRHighLevelDeclDecoration>();
    if(!highLevelDeclDecoration)
        return;
    auto decl = highLevelDeclDecoration->decl;

    SourceLoc loc = bestUser->sourceLoc;
    if(!loc.isValid())
        loc = decl->loc;

    sink->diagnose(loc, diagnostic, decl->getName());
}
#endif

struct SimpleUndefinedCheckingContext
{
    IRGlobalValueWithCode*  code;
    DiagnosticSink*         sink;

    void doIt()
    {
        // The basic premise here is that we will scan for `undefind` instructions
        // that were created when constructing SSA form, and collect those
        // according to what variable the `undefined` represents, so that we emit
        // at most one diagnostic per variable, at the "best" location for
        // explaining the problem to the user.
        for( auto block : code->getBlocks() )
        {
            for( auto inst : block->getChildren() )
            {
                if( auto undefinedInst = as<IRUndefined>(inst) )
                {
                    // What variable is this for?
                    auto highLevelDeclDecoration = undefinedInst->findDecoration<IRHighLevelDeclDecoration>();
                    if(!highLevelDeclDecoration)
                        continue;
                    auto decl = highLevelDeclDecoration->decl;

                    addPotentiallyUndefinedValue(decl, undefinedInst);
                }
            }
        }

        for( auto decl : varsToDiagnose )
        {
            IRInst* bestUser;
            if( bestUserForVar.TryGetValue(decl, bestUser) )
            {
                SourceLoc loc = bestUser->sourceLoc;
                if(!loc.isValid())
                    loc = decl->loc;

                sink->diagnose(loc, Diagnostics::useOfUndefinedVariable, decl->getName());
            }
        }
    }

    HashSet<IRInst*> undefinedValuesSeen;
    void addPotentiallyUndefinedValue(Decl* decl, IRInst* value)
    {
        // Don't process the same value more than once
        if(undefinedValuesSeen.Contains(value))
            return;
        undefinedValuesSeen.Add(value);

        updateBestUserForVar(decl, value);

        // If this value is used in any unconditional branches,
        // then that means it is being used as a block argument.
        for( auto use = value->firstUse; use; use = use->nextUse )
        {
            auto user = use->getUser();
            if(auto branch = as<IRUnconditionalBranch>(user))
            {
                // The given `value` appears to have been used
                // as a block argument, which is effectively a
                // phi node. We should consider the parameter
                // of the target block as potentially undefined
                // as well, for the same variable.

                UInt argIndex = use - branch->getArgs();
                SLANG_ASSERT(argIndex < branch->getArgCount());

                auto targetBlock = branch->getTargetBlock();

                UInt paramCounter = 0;
                for(auto pp : targetBlock->getParams())
                {
                    auto paramIndex = paramCounter++;
                    if(paramIndex == argIndex)
                    {
                        addPotentiallyUndefinedValue(decl, pp);
                        break;
                    }
                }
            }
        }
    }

    Dictionary<Decl*, IRInst*> bestUserForVar;
    List<Decl*> varsToDiagnose;
    void updateBestUserForVar(Decl* decl, IRInst* value)
    {
        IRInst* bestUser = nullptr;
        if( !bestUserForVar.TryGetValue(decl, bestUser) )
        {
            varsToDiagnose.Add(decl);
        }

        for( auto newUse = value->firstUse; newUse; newUse = newUse->nextUse )
        {
            auto newUser = newUse->getUser();
            bestUser = pickBestUser(bestUser, newUser);
        }
        bestUserForVar[decl] = bestUser;
    }
};

#if 0
            if(pp == param)
                paramIndex == ppIndex;
        }

        // What values are being passed in for this parameter from
        // predecessor blocks? Are any of them undefined?
        bool anyUndefined = false;
        for( auto pred : parentBlock->getPredecessors() )
        {
            auto terminator = as<IRUnconditionalBranch>(pred->getTerminator());
            SLANG_ASSERT(terminator);
            if(!terminator)
                continue;

            SLANG_ASSERT(paramIndex < terminator->getArgCount());
            if(paramIndex >= terminator->getArgCount())
                continue;

            auto arg = terminator->getArg(paramIndex);
            if( as<IRUndefined>(arg) )
            {
                anyUndefined = true;
                break;
            }
        }

        if( anyUndefined )
        {
            diagnoseUseOfUndefinedVariable(param, sink, Diagnostics::useOfUndefinedVariable);
        }
    }
    else if( auto undefinedInst = as<IRUndefined>(inst) )
    {
        diagnoseUseOfUndefinedVariable(undefinedInst, sink, Diagnostics::useOfUndefinedVariable);
    }

    // If the use in question is an unconditional branch, then we expect the
    // `undefined` is being used as an argument (passed to a parameter of
    // the downstream block). The alternative is that we are jumping to
    // an undefined branch target, and that would be really scary.
    //
    if( auto branchInst = as<IRUnconditionalBranch>(bestUser) )
    {
        auto argCount = branchInst->getArgCount();
        for( UInt aa = 0; aa < argCount; ++aa )
        {
            if( branchInst->getArg(aa) == undefinedInst )
            {
                // Okay, argument `aa` of the branch is our `undefined`, and
                // we therefore want to give a diagnostic at the use site
                // of the parameter that receives it.
            }
        }
    }

    // TODO: if the best user is effectively a "phi" operation (an argument
    // on a branch instruction), then we should really report at the best
    // use of the block parameter that accepts that argument.

    // Pick a reasonable source location to use. Ideally this comes from
    // the instruction that uses the variable, but we fall back to using
    // the location of the variable itself if needed.
    SourceLoc loc = bestUser->sourceLoc;
    if(!loc.isValid())
        loc = decl->loc;

    sink->diagnose(loc, Diagnostics::useOfUndefinedVariable, decl->getName());
}
#endif

    /// Check that `inst` and its children are free of `undefined` uses.
static void checkSimpleUndefinedUsesRec(
    IRInst*         inst,
    DiagnosticSink* sink)
{
    // We will recursively search for `undefined` instructions and report
    // an error on any that we find.
    //
    // First, check hte instruction itself, if we have someting with code;
    //
    if( auto code = as<IRGlobalValueWithCode>(inst) )
    {
        SimpleUndefinedCheckingContext context;
        context.code = code;
        context.sink = sink;
        context.doIt();
    }

    // Once we've checked the instruction itself, we will recursively check
    // any children it might have.
    //
    if( auto parentInst = as<IRParentInst>(inst) )
    {
        for( auto childInst : parentInst->getChildren() )
        {
            checkSimpleUndefinedUsesRec(childInst, sink);
        }
    }
}

void checkDefiniteInitialization(
    IRModule*       module,
    DiagnosticSink* sink)
{
    // Definite initialization will be checked on function at a time.
    // This means that each function must obey its logical contract,
    // and it cannot rely on the implementation details of functions it calls.
    //
    // One example of where this matters is:
    //
    //      void helpful(in out int x) { x = 42; }
    //      int bad()
    //      {
    //          int u;
    //          helpful(u);
    //          return u;
    //      }
    //
    // An interprocedural analysis might recognize that `helpful`
    // always initializes its `x` parameter, and doesn't read
    // from it, so that `bad` is actually guaranteed to see
    // valid data when it reads from `u` for the `return` statement.
    //
    // Our analysis will intentionally be simpler, and will treat
    // the call to `helpful` as both a potential read and a write
    // of `u`, so that we issue a diagnostic blaming `bad` for
    // failing to uphold its responsibilities.

    // At this point the basic SSA promotion pass will have been
    // run, so many temporary registers will be promoted to SSA
    // registers. Any uses of those variables that would yield
    // an undefined value will then be encoded as `undefined`
    // instructiosn in the IR. Thus, we can in principle just
    // search for any `undefined` instructions and issue a
    // diagnostic for each of them.
    //
    checkSimpleUndefinedUsesRec(module->getModuleInst(), sink);

    //
    // We will end up proceeding one function at a time, and then
    // within a function one *variable* at a time, to keep the analysis
    // simple. When analyzying a given variable, we will assume
    // that any loads of other variables must be valid.

    auto session = module->session;

    SharedCheckDefiniteInitializationContext sharedContext;
    sharedContext.module = module;
    sharedContext.sink = sink;
    sharedContext.sharedBuilder.module = module;
    sharedContext.sharedBuilder.session = session;
    sharedContext.builder.sharedBuilder = &sharedContext.sharedBuilder;

    for( auto global : module->getGlobalInsts() )
    {
        auto code = as<IRGlobalValueWithCode>(global);
        if (!code)
            continue;

        for( auto block : code->getBlocks() )
        {
            for( auto inst : block->getChildren() )
            {
                auto var = as<IRVar>(inst);
                if(!var)
                    continue;

                CheckDefiniteInitializationContext context;
                context.shared = &sharedContext;
                context.code = code;
                context.varToCheck = var;
                context.checkDefiniteInitializationForLocalVariable();
            }
        }

        if( auto firstBlock = code->getFirstBlock() )
        {
            for( auto param : firstBlock->getParams() )
            {
                auto paramType = param->getDataType();
                if(!as<IROutType>(paramType))
                    continue;

                CheckDefiniteInitializationContext context;
                context.shared = &sharedContext;
                context.code = code;
                context.varToCheck = param;
                context.checkDefiniteInitializationForOutParameter();
            }
        }
    }
}

}
