// ir-definite-initialization.cpp
#include "ir-definite-initialization.h"

#include "ir.h"
#include "ir-insts.h"

namespace Slang {

struct CheckDefiniteInitializationContext
{
    IRModule* module;
    IRModule* getModule() { return module; }

    DiagnosticSink* sink;
    DiagnosticSink* getSink() { return sink; }

    SharedIRBuilder sharedBuilder;
    Session* getSession() { return sharedBuilder.session; }

    IRBuilder builder;
    IRBuilder* getBuilder() { return &builder; }

};

static void checkDefiniteInitialization(
    CheckDefiniteInitializationContext* context,
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
}

static void checkDefiniteInitialization(
    CheckDefiniteInitializationContext* context)
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
    //

    auto module = context->module;
    for( auto ii : module->getGlobalInsts() )
    {
        auto code = as<IRGlobalValueWithCode>(ii);
        if (!code)
            continue;

        checkDefiniteInitialization(context, code);
    }
}

void checkDefiniteInitialization(
    IRModule*       module,
    DiagnosticSink* sink)
{
        auto session = module->session;

    CheckDefiniteInitializationContext context;
    context.module = module;
    context.sink = sink;
    context.sharedBuilder.module = module;
    context.sharedBuilder.session = session;
    context.builder.sharedBuilder = &context.sharedBuilder;

    checkDefiniteInitialization(&context);
}

}
