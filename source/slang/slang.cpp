#include "../../slang.h"

#include "../core/slang-io.h"
#include "parameter-binding.h"
#include "lower-to-ir.h"
#include "../slang/parser.h"
#include "../slang/preprocessor.h"
#include "../slang/reflection.h"
#include "syntax-visitors.h"
#include "../slang/type-layout.h"

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>
#undef WIN32_LEAN_AND_MEAN
#undef NOMINMAX
#endif

namespace Slang {

Session::Session()
{
    // Initialize name pool
    getNamePool()->setRootNamePool(getRootNamePool());

    // Initialize the lookup table of syntax classes:

    #define SYNTAX_CLASS(NAME, BASE) \
        mapNameToSyntaxClass.Add(getNamePool()->getName(#NAME), getClass<NAME>());

#include "object-meta-begin.h"
#include "syntax-base-defs.h"
#include "expr-defs.h"
#include "decl-defs.h"
#include "modifier-defs.h"
#include "stmt-defs.h"
#include "type-defs.h"
#include "val-defs.h"
#include "object-meta-end.h"

    // Make sure our source manager is initialized
    builtinSourceManager.initialize(nullptr);

    // Initialize representations of some very basic types:
    initializeTypes();

    // Create scopes for various language builtins.
    //
    // TODO: load these on-demand to avoid parsing
    // stdlib code for languages the user won't use.

    baseLanguageScope = new Scope();

    auto baseModuleDecl = populateBaseLanguageModule(
        this,
        baseLanguageScope);
    loadedModuleCode.Add(baseModuleDecl);

    coreLanguageScope = new Scope();
    coreLanguageScope->nextSibling = baseLanguageScope;

    hlslLanguageScope = new Scope();
    hlslLanguageScope->nextSibling = coreLanguageScope;

    slangLanguageScope = new Scope();
    slangLanguageScope->nextSibling = hlslLanguageScope;

    glslLanguageScope = new Scope();
    glslLanguageScope->nextSibling = coreLanguageScope;

    addBuiltinSource(coreLanguageScope, "core", getCoreLibraryCode());
    addBuiltinSource(hlslLanguageScope, "hlsl", getHLSLLibraryCode());
    addBuiltinSource(glslLanguageScope, "glsl", getGLSLLibraryCode());
}

struct IncludeHandlerImpl : IncludeHandler
{
    CompileRequest* request;

    virtual IncludeResult TryToFindIncludeFile(
        String const& pathToInclude,
        String const& pathIncludedFrom,
        String* outFoundPath,
        String* outFoundSource) override
    {
        String path = Path::Combine(Path::GetDirectoryName(pathIncludedFrom), pathToInclude);
        if (File::Exists(path))
        {
            *outFoundPath = path;
            *outFoundSource = File::ReadAllText(path);

            request->mDependencyFilePaths.Add(path);

            return IncludeResult::Found;
        }

        for (auto & dir : request->getSearchDirectories())
        {
            path = Path::Combine(dir.path, pathToInclude);
            if (File::Exists(path))
            {
                *outFoundPath = path;
                *outFoundSource = File::ReadAllText(path);

                request->mDependencyFilePaths.Add(path);

                return IncludeResult::Found;
            }
        }
        return IncludeResult::NotFound;
    }
};

//

TranslationUnitRequest::TranslationUnitRequest()
{
    module = new LoadedModule();
}

//

CompileRequest::CompileRequest(Session* session)
    : mSession(session)
{
    getNamePool()->setRootNamePool(session->getRootNamePool());

    setSourceManager(&sourceManagerStorage);

    sourceManager->initialize(session->getBuiltinSourceManager());
}

CompileRequest::~CompileRequest()
{}

void CompileRequest::parseTranslationUnit(
    TranslationUnitRequest* translationUnit)
{
    IncludeHandlerImpl includeHandler;
    includeHandler.request = this;

    RefPtr<Scope> languageScope;
    switch (translationUnit->sourceLanguage)
    {
    case SourceLanguage::HLSL:
        languageScope = mSession->hlslLanguageScope;
        break;

    case SourceLanguage::GLSL:
        languageScope = mSession->glslLanguageScope;
        break;

    case SourceLanguage::Slang:
    default:
        languageScope = mSession->slangLanguageScope;
        break;
    }

    auto& combinedPreprocessorDefinitions = translationUnit->getPreprocessorDefinitions();

    RefPtr<ModuleDecl> translationUnitSyntax = new ModuleDecl();
    translationUnit->module->moduleDecl = translationUnitSyntax;

    for (auto sourceFile : translationUnit->sourceFiles)
    {
        auto tokens = preprocessSource(
            sourceFile,
            &mSink,
            &includeHandler,
            combinedPreprocessorDefinitions,
            translationUnit);

        parseSourceFile(
            translationUnit,
            tokens,
            &mSink,
            languageScope);
    }
}

void CompileRequest::checkAllTranslationUnits()
{
    // Iterate over all translation units and
    // apply the semantic checking logic.
    for( auto& translationUnit : translationUnits )
    {
        checkTranslationUnit(translationUnit.Ptr());
    }
}

void CompileRequest::generateIR()
{
    // Our task in this function is to generate IR code
    // for all of the declarations in the translation
    // units that were loaded.

    // At the moment, use of the IR is not enabled by
    // default, so we will skip this step unless
    // the flag was set to op in.
    if (!(getCompileFlags() & SLANG_COMPILE_FLAG_USE_IR))
        return;

    // Each translation unit is its own little world
    // for code generation (we are not trying to
    // replicate the GLSL linkage model), and so
    // we will generate IR for each (if needed)
    // in isolation.
    for( auto& translationUnit : translationUnits )
    {
        // Also skip IR generation if semantic checking is turned off
        // for a given translation unit.
        if(translationUnit->getCompileFlags() & SLANG_COMPILE_FLAG_NO_CHECKING)
            continue;

        translationUnit->module->irModule = generateIRForTranslationUnit(translationUnit);
    }
}

// Try to infer a single common source language for a request
static SourceLanguage inferSourceLanguage(CompileRequest* request)
{
    SourceLanguage language = SourceLanguage::Unknown;
    for (auto& translationUnit : request->translationUnits)
    {
        // Allow any other language to overide Slang as a choice
        if (language == SourceLanguage::Unknown
            || language == SourceLanguage::Slang)
        {
            language = translationUnit->sourceLanguage;
        }
        else if (language == translationUnit->sourceLanguage)
        {
            // same language as we currently have, so keep going
        }
        else
        {
            // we found a mismatch, so inference fails
            return SourceLanguage::Unknown;
        }
    }
    return language;
}

int CompileRequest::executeActionsInner()
{
    // Do some cleanup on settings specified by user.
    for (auto& translationUnit : translationUnits)
    {
        // The "no checking" flag shouldn't be applied to
        // any translation unit that is native Slang code.
        if (translationUnit->sourceLanguage == SourceLanguage::Slang)
        {
            translationUnit->options->ensureCooked();
            translationUnit->options->cookedCompileFlags &= ~SLANG_COMPILE_FLAG_NO_CHECKING;
        }
    }

    // If no code-generation target was specified, then try to infer one from the source language,
    // just to make sure we can do something reasonable when invoked from the command line.
    if (getTargetDescs().Count() == 0)
    {
        auto language = inferSourceLanguage(this);
        switch (language)
        {
        case SourceLanguage::HLSL:
            addTarget(CodeGenTarget::DXBytecode);
            break;

        case SourceLanguage::GLSL:
            addTarget(CodeGenTarget::SPIRV);
            break;

        default:
            break;
        }
    }

    // We only do parsing and semantic checking if we *aren't* doing
    // a pass-through compilation.
    //
    // Note that we *do* perform output generation as normal in pass-through mode.
    if (passThrough == PassThroughMode::None)
    {
        // Parse everything from the input files requested
        for (auto& translationUnit : translationUnits)
        {
            parseTranslationUnit(translationUnit.Ptr());
        }
        if (mSink.GetErrorCount() != 0)
            return 1;

        // Perform semantic checking on the whole collection
        checkAllTranslationUnits();
        if (mSink.GetErrorCount() != 0)
            return 1;

        if ((getCompileFlags() & SLANG_COMPILE_FLAG_NO_CODEGEN) == 0)
        {
            // Generate initial IR for all the translation
            // units, if we are in a mode where IR is called for.
            generateIR();
        }

        if (mSink.GetErrorCount() != 0)
            return 1;

        // For each code generation target generate
        // parameter binding information.
        // This step is done globaly, because all translation
        // units and entry points need to agree on where
        // parameters are allocated.
        for (auto targetReq : getTargetRequests())
        {
            generateParameterBindings(targetReq);
            if (mSink.GetErrorCount() != 0)
                return 1;
        }
    }

    // If command line specifies to skip codegen, we exit here.
    // Note: this is a debugging option.
    if (shouldSkipCodegen ||
        ((getCompileFlags() & SLANG_COMPILE_FLAG_NO_CODEGEN) != 0))
        return 0;

    // Generate output code, in whatever format was requested
    generateOutput(this);
    if (mSink.GetErrorCount() != 0)
        return 1;

    return 0;
}

// Act as expected of the API-based compiler
int CompileRequest::executeActions()
{
    int err = executeActionsInner();

    mDiagnosticOutput = mSink.outputBuffer.ProduceString();

    return err;
}

int CompileRequest::addTranslationUnit(SourceLanguage language, String const&)
{
    UInt result = translationUnits.Count();

    RefPtr<TranslationUnitRequest> translationUnit = new TranslationUnitRequest();
    translationUnit->compileRequest = this;
    translationUnit->sourceLanguage = SourceLanguage(language);

    translationUnit->options = new CompileOptions(options);

    translationUnits.Add(translationUnit);

    return (int) result;
}

void CompileRequest::addTranslationUnitSourceFile(
    int             translationUnitIndex,
    SourceFile*     sourceFile)
{
    translationUnits[translationUnitIndex]->sourceFiles.Add(sourceFile);
}

void CompileRequest::addTranslationUnitSourceString(
    int             translationUnitIndex,
    String const&   path,
    String const&   source)
{
    RefPtr<SourceFile> sourceFile = getSourceManager()->allocateSourceFile(path, source);

    addTranslationUnitSourceFile(translationUnitIndex, sourceFile);
}

void CompileRequest::addTranslationUnitSourceFile(
    int             translationUnitIndex,
    String const&   path)
{
    String source;
    try
    {
        source = File::ReadAllText(path);
    }
    catch (...)
    {
        // Emit a diagnostic!
        mSink.diagnose(
            SourceLoc(),
            Diagnostics::cannotOpenFile,
            path);
        return;
    }

    addTranslationUnitSourceString(
        translationUnitIndex,
        path,
        source);

    mDependencyFilePaths.Add(path);
}

int CompileRequest::addEntryPoint(
    int                     translationUnitIndex,
    String const&           name,
    Profile                 entryPointProfile,
    List<String> const &    genericTypeNames)
{
    RefPtr<EntryPointRequest> entryPoint = new EntryPointRequest();
    entryPoint->compileRequest = this;
    entryPoint->name = getNamePool()->getName(name);
    entryPoint->profile = entryPointProfile;
    entryPoint->translationUnitIndex = translationUnitIndex;
    for (auto typeName : genericTypeNames)
        entryPoint->genericParameterTypeNames.Add(getNamePool()->getName(typeName));
    auto translationUnit = translationUnits[translationUnitIndex].Ptr();
    translationUnit->entryPoints.Add(entryPoint);

    UInt result = entryPoints.Count();
    entryPoints.Add(entryPoint);
    return (int) result;
}

void CompileOptions::ensureCooked()
{
    if(mode == Mode::cooked)
        return;

    mode = Mode::cooked;

    if(base)
    {
        base->ensureCooked();
        cookedCompileFlags = base->cookedCompileFlags;
        cookedSearchDirectories = base->cookedSearchDirectories;
        cookedPreprocessorDefinitions = base->cookedPreprocessorDefinitions;
        cookedTargets = base->cookedTargets;
    }

    cookedCompileFlags = (cookedCompileFlags & ~rawCompileFlagsMask) | (rawCompileFlags & rawCompileFlagsMask);
    cookedSearchDirectories.AddRange(rawSearchDirectories);
    for(auto p : rawPreprocessorDefinitions)
        cookedPreprocessorDefinitions[p.Key] = p.Value;
    cookedTargets.AddRange(rawTargets);
}


UInt CompileOptions::addTarget(
    CodeGenTarget   target)
{
    RefPtr<TargetDesc> targetDesc = new TargetDesc();
    targetDesc->target = target;

    UInt result = rawTargets.Count();
    rawTargets.Add(targetDesc);
    return (int) result;
}

UInt CompileRequest::addTarget(
    CodeGenTarget   target)
{
    return options->addTarget(target);
}

}

Slang::RefPtr<Slang::LoadedModule> SlangLinkage::loadParsedModule(
    Slang::RefPtr<Slang::TranslationUnitRequest> const& translationUnit,
    Slang::Name*                                        name,
    Slang::String const&                                path)
{
    using namespace Slang;

    checkTranslationUnit(translationUnit.Ptr());

    RefPtr<LoadedModule> loadedModule = translationUnit->module;
    loadedModule->irModule = generateIRForTranslationUnit(translationUnit);

    mapPathToLoadedModule.Add(path, loadedModule);
    mapNameToLoadedModules.Add(name, loadedModule);
    loadedModulesList.Add(loadedModule);

    return loadedModule;
}

Slang::RefPtr<Slang::LoadedModule> SlangLinkage::loadModule(
    Slang::CompileRequest*  compileRequest,
    Slang::Name*            name,
    Slang::String const&    path,
    Slang::String const&    source,
    Slang::SourceLoc const&)
{
    using namespace Slang;

    RefPtr<TranslationUnitRequest> translationUnit = new TranslationUnitRequest();
    translationUnit->compileRequest = compileRequest;
    translationUnit->options = compileRequest->options;

    RefPtr<SourceFile> sourceFile = compileRequest->getSourceManager()->allocateSourceFile(path, source);

    translationUnit->sourceFiles.Add(sourceFile);

    compileRequest->parseTranslationUnit(translationUnit.Ptr());

    // TODO: handle errors

    return loadParsedModule(
        translationUnit,
        name,
        path);
}

void SlangLinkage::handlePoundImport(
    Slang::String const&    path,
    Slang::TokenList const& tokens)
{
    using namespace Slang;

    // Create a compile request to handle the load
    RefPtr<CompileRequest> compileRequest = new CompileRequest(session);
    compileRequest->options = options;

    RefPtr<TranslationUnitRequest> translationUnit = new TranslationUnitRequest();
    translationUnit->compileRequest = compileRequest;
    translationUnit->options = compileRequest->options;

    // Imported code is always native Slang code
    RefPtr<Scope> languageScope = session->slangLanguageScope;

    RefPtr<ModuleDecl> translationUnitSyntax = new ModuleDecl();
    translationUnit->module->moduleDecl = translationUnitSyntax;

    parseSourceFile(
        translationUnit.Ptr(),
        tokens,
        &compileRequest->mSink,
        languageScope);

    // TODO: handle errors

    // TODO: It is a bit broken here that we use the module path,
    // as the "name" when registering things, but this saves
    // us the trouble of trying to special-case things when
    // checking an `import` down the road.
    //
    // Ideally we'd construct a suitable name by effectively
    // running the name->path logic in reverse (e.g., replacing
    // `-` with `_` and `/` with `.`).
    Name* name = session->getNamePool()->getName(path);

    loadParsedModule(
        translationUnit,
        name,
        path);
}

Slang::RefPtr<Slang::LoadedModule> SlangLinkage::findOrImportModule(
    Slang::CompileRequest*  originalRequest,
    Slang::Name*            name,
    Slang::SourceLoc const& loc)
{
    using namespace Slang;

    // Have we already loaded a module matching this name?
    // If so, return it.
    RefPtr<LoadedModule> loadedModule;
    if (mapNameToLoadedModules.TryGetValue(name, loadedModule))
        return loadedModule;

    // Derive a file name for the module, by taking the given
    // identifier, replacing all occurences of `_` with `-`,
    // and then appending `.slang`.
    //
    // For example, `foo_bar` becomes `foo-bar.slang`.

    StringBuilder sb;
    for (auto c : getText(name))
    {
        if (c == '_')
            c = '-';

        sb.Append(c);
    }
    sb.Append(".slang");

    String fileName = sb.ProduceString();

    // Create a compile request to use for the load operation
    RefPtr<CompileRequest> compileRequest = new CompileRequest(session);
    compileRequest->options = options;

    // Next, try to find the file of the given name,
    // using our ordinary include-handling logic.

    IncludeHandlerImpl includeHandler;
    includeHandler.request = compileRequest;

    auto expandedLoc = originalRequest->getSourceManager()->expandSourceLoc(loc);

    String pathIncludedFrom = expandedLoc.getSpellingPath();

    String foundPath;
    String foundSource;
    IncludeResult includeResult = includeHandler.TryToFindIncludeFile(fileName, pathIncludedFrom, &foundPath, &foundSource);
    switch( includeResult )
    {
    case IncludeResult::NotFound:
    case IncludeResult::Error:
        {
            originalRequest->mSink.diagnose(loc, Diagnostics::cannotFindFile, fileName);

            mapNameToLoadedModules[name] = nullptr;
            return nullptr;
        }
        break;

    default:
        break;
    }

    // Maybe this was loaded previously via `#import`
    if (mapPathToLoadedModule.TryGetValue(foundPath, loadedModule))
        return loadedModule;


    // We've found a file that we can load for the given module, so
    // go ahead and perform the module-load action
    return loadModule(
        compileRequest,
        name,
        foundPath,
        foundSource,
        loc);
}

namespace Slang
{

CompileRequest::TargetRequestList const & CompileRequest::getTargetRequests()
{
    auto& targetDescs = getTargetDescs();
    if( targetRequests.Count() != targetDescs.Count() )
    {
        assert(targetRequests.Count() == 0);

        for(auto targetDesc : targetDescs)
        {
            RefPtr<TargetRequest> targetRequest = new TargetRequest();
            targetRequest->compileRequest = this;
            targetRequest->targetDesc = targetDesc;

            targetRequests.Add(targetRequest);
        }
    }
    return targetRequests;
}

Linkage * CompileRequest::getLinkage()
{
    // If a linkage was specified for our compile request to use, then use
    // that one. Otherwise, create a linkage that will be specific to our
    // compile request, that clones all of the relevant state.

    if(!linkageForImports)
    {
        linkageForImports = new Linkage(mSession);

        // We need to ensure that anything loaded through the linkage
        // will have semantic checking applied (since it is required
        // for Slang code), so the linkgae will need its own options
        // object if our has the no-checking flag.
        if(options->getCompileFlags() & SLANG_COMPILE_FLAG_NO_CHECKING)
        {
            RefPtr<CompileOptions> linkageOptions = new CompileOptions(options);
            linkageOptions->setCompileFlags(SLANG_COMPILE_FLAG_NO_CHECKING, 0);
            linkageForImports->options = linkageOptions;
        }
        else
        {
            linkageForImports->options = options;
        }

    }

    return linkageForImports;
}

Linkage* TranslationUnitRequest::getLinkage()
{
    return compileRequest->getLinkage();
}

void TranslationUnitRequest::registerImportedModule(
    RefPtr<LoadedModule>    loadedModule)
{
    // We need to register that we depend on the given module,
    // but we also need to remember that we depend on everything
    // it transitively depends on.

    for(auto dependency : loadedModule->importedModules)
    {
        registerImportedModuleImpl(dependency);
    }
    registerImportedModuleImpl(loadedModule);
}

void TranslationUnitRequest::registerImportedModuleImpl(
    RefPtr<LoadedModule>    loadedModule)
{
    // If we've already imported this module before, there is
    // nothing left to do.
    if(importedModuleSet.Contains(loadedModule))
        return;

    // Othwerise, add the module to our dependency list,
    // and then also put it in our set so we don't add
    // it again.
    module->importedModules.Add(loadedModule);
    importedModuleSet.Add(loadedModule);
}

RefPtr<LoadedModule> TranslationUnitRequest::findOrImportModule(
    Name*            name,
    SourceLoc const& loc)
{
    // Delegate to the linkage to perform the actual load operation.
    auto loadedModule = getLinkage()->findOrImportModule(compileRequest, name, loc);

    // Bail out if loading failed.
    if(!loadedModule)
        return nullptr;

    // Remember that we've imported this module.
    registerImportedModule(loadedModule);

    return loadedModule;
}

void CompileRequest::handlePoundImport(
    String const&    path,
    TokenList const& tokens)
{
    getLinkage()->handlePoundImport(path, tokens);
}



RefPtr<ModuleDecl> findOrImportModule(
    TranslationUnitRequest* request,
    Name*                   name,
    SourceLoc const&        loc)
{
    auto loadedModule = request->findOrImportModule(name, loc);
    if(!loadedModule)
        return nullptr;
    return loadedModule->moduleDecl;
}


void Session::addBuiltinSource(
    RefPtr<Scope> const&    scope,
    String const&           path,
    String const&           source)
{
    RefPtr<CompileRequest> compileRequest = new CompileRequest(this);
    compileRequest->options = new CompileOptions();
    compileRequest->setSourceManager(getBuiltinSourceManager());

    auto translationUnitIndex = compileRequest->addTranslationUnit(SourceLanguage::Slang, path);

    RefPtr<SourceFile> sourceFile = builtinSourceManager.allocateSourceFile(path, source);

    compileRequest->addTranslationUnitSourceString(
        translationUnitIndex,
        path,
        source);

    int err = compileRequest->executeActions();
    if (err)
    {
        fprintf(stderr, "%s", compileRequest->mDiagnosticOutput.Buffer());

#ifdef _WIN32
        OutputDebugStringA(compileRequest->mDiagnosticOutput.Buffer());
#endif

        SLANG_UNEXPECTED("error in Slang standard library");
    }

    // Extract the AST for the code we just parsed
    auto syntax = compileRequest->translationUnits[translationUnitIndex]->getModuleDecl();

    // HACK(tfoley): mark all declarations in the "stdlib" so
    // that we can detect them later (e.g., so we don't emit them)
    for (auto m : syntax->Members)
    {
        auto fromStdLibModifier = new FromStdLibModifier();

        fromStdLibModifier->next = m->modifiers.first;
        m->modifiers.first = fromStdLibModifier;
    }

    // Add the resulting code to the appropriate scope
    if (!scope->containerDecl)
    {
        // We are the first chunk of code to be loaded for this scope
        scope->containerDecl = syntax.Ptr();
    }
    else
    {
        // We need to create a new scope to link into the whole thing
        auto subScope = new Scope();
        subScope->containerDecl = syntax.Ptr();
        subScope->nextSibling = scope->nextSibling;
        scope->nextSibling = subScope;
    }

    // We need to retain this AST so that we can use it in other code
    // (Note that the `Scope` type does not retain the AST it points to)
    loadedModuleCode.Add(syntax);
}

}

// implementation of C interface

#define SESSION(x) reinterpret_cast<Slang::Session *>(x)
#define REQ(x) reinterpret_cast<Slang::CompileRequest*>(x)

SLANG_API SlangSession* spCreateSession(const char*)
{
    return reinterpret_cast<SlangSession *>(new Slang::Session());
}

SLANG_API void spDestroySession(
    SlangSession*   session)
{
    if(!session) return;
    delete SESSION(session);
}

SLANG_API void spAddBuiltins(
    SlangSession*   session,
    char const*     sourcePath,
    char const*     sourceString)
{
    auto s = SESSION(session);
    s->addBuiltinSource(

        // TODO(tfoley): Add ability to directly new builtins to the approriate scope
        s->coreLanguageScope,

        sourcePath,
        sourceString);
}

SLANG_API SlangLinkage* spCreateLinkage(
    SlangSession*   session)
{
    auto s = SESSION(session);
    auto linkage = new Slang::Linkage(s);
    linkage->options = new Slang::CompileOptions();
    return linkage;
}

SLANG_API void spDestroyLinkage(
    SlangLinkage*   linkage)
{
    linkage->releaseReference();
}

SLANG_API SlangCompileRequest* spLoadModuleIntoLinkage(
    SlangLinkage*   linkage,
    char const*     name)
{
    (void) linkage;
    (void) name;
    return nullptr;
}

SLANG_API void spLinkage_AddSearchPath(
    SlangLinkage*   linkage,
    const char*     path)
{
    linkage->options->addSearchPath(path);
}

SLANG_API void spLinkage_AddPreprocessorDefine(
    SlangLinkage*   linkage,
    const char*     key,
    const char*     value)
{
    linkage->options->addPreprocessorDefine(key,value);
}

SLANG_API void spLinkage_AddCodeGenTarget(
    SlangLinkage*       linkage,
    SlangCompileTarget  target)
{
    linkage->options->addTarget(Slang::CodeGenTarget(target));
}

SLANG_API void spLinkage_SetCompileFlags(
    SlangLinkage*       linkage,
    SlangCompileFlags   flags)
{
    linkage->options->setCompileFlags(flags, flags);
}


SLANG_API SlangCompileRequest* spCreateCompileRequest(
    SlangSession* session)
{
    auto s = SESSION(session);
    auto req = new Slang::CompileRequest(s);
    req->options = new Slang::CompileOptions();
    return reinterpret_cast<SlangCompileRequest*>(req);
}

/*!
@brief Destroy a compile request.
*/
SLANG_API void spDestroyCompileRequest(
    SlangCompileRequest*    request)
{
    if(!request) return;
    auto req = REQ(request);
    delete req;
}

SLANG_API void spSetLinkageForImports(
    SlangCompileRequest*    request,
    SlangLinkage*           linkage)
{
    if(!request) return;
    auto req = REQ(request);
    req->linkageForImports = linkage;
    req->options->base = linkage->options;
}


SLANG_API void spSetCompileFlags(
    SlangCompileRequest*    request,
    SlangCompileFlags       flags)
{
    // Note: We are interpreting this option as enabling the given flags,
    // but not changing flags that were previously set.

    REQ(request)->options->enableCompileFlags(flags);
}

SLANG_API void spSetDumpIntermediates(
    SlangCompileRequest*    request,
    int                     enable)
{
    REQ(request)->shouldDumpIntermediates = enable != 0;
}

SLANG_API void spSetLineDirectiveMode(
    SlangCompileRequest*    request,
    SlangLineDirectiveMode  mode)
{
    // TODO: validation

    REQ(request)->lineDirectiveMode = Slang::LineDirectiveMode(mode);
}

SLANG_API void spSetCommandLineCompilerMode(
    SlangCompileRequest* request)
{
    REQ(request)->isCommandLineCompile = true;

}

SLANG_API void spSetCodeGenTarget(
        SlangCompileRequest*    request,
        int target)
{
    auto req = REQ(request);
    auto options = req->options;
    options->rawTargets.Clear();
    options->addTarget(Slang::CodeGenTarget(target));
}

SLANG_API int spAddCodeGenTarget(
    SlangCompileRequest*    request,
    SlangCompileTarget      target)
{
    auto req = REQ(request);
    auto options = req->options;
    return (int) options->addTarget(Slang::CodeGenTarget(target));
}

SLANG_API void spSetTargetProfile(
    SlangCompileRequest*    request,
    int                     targetIndex,
    SlangProfileID          profile)
{
    auto req = REQ(request);
    req->options->rawTargets[targetIndex]->targetProfile = profile;
}

SLANG_API void spSetTargetFlags(
    SlangCompileRequest*    request,
    int                     targetIndex,
    SlangTargetFlags        flags)
{
    auto req = REQ(request);
    req->options->rawTargets[targetIndex]->targetFlags = flags;
}

SLANG_API void spSetOutputContainerFormat(
    SlangCompileRequest*    request,
    SlangContainerFormat    format)
{
    auto req = REQ(request);
    req->containerFormat = Slang::ContainerFormat(format);
}


SLANG_API void spSetPassThrough(
    SlangCompileRequest*    request,
    SlangPassThrough        passThrough)
{
    REQ(request)->passThrough = Slang::PassThroughMode(passThrough);
}

SLANG_API void spSetDiagnosticCallback(
    SlangCompileRequest*    request,
    SlangDiagnosticCallback callback,
    void const*             userData)
{
    if(!request) return;
    auto req = REQ(request);

    req->mSink.callback = callback;
    req->mSink.callbackUserData = (void*) userData;
}

SLANG_API void spAddSearchPath(
        SlangCompileRequest*    request,
        const char*             path)
{
    REQ(request)->options->addSearchPath(path);
}

SLANG_API void spAddPreprocessorDefine(
    SlangCompileRequest*    request,
    const char*             key,
    const char*             value)
{
    REQ(request)->options->addPreprocessorDefine(key, value);
}

SLANG_API char const* spGetDiagnosticOutput(
    SlangCompileRequest*    request)
{
    if(!request) return 0;
    auto req = REQ(request);
    return req->mDiagnosticOutput.begin();
}

// New-fangled compilation API

SLANG_API int spAddTranslationUnit(
    SlangCompileRequest*    request,
    SlangSourceLanguage     language,
    char const*             name)
{
    auto req = REQ(request);

    return req->addTranslationUnit(
        Slang::SourceLanguage(language),
        name ? name : "");
}

SLANG_API void spTranslationUnit_addPreprocessorDefine(
    SlangCompileRequest*    request,
    int                     translationUnitIndex,
    const char*             key,
    const char*             value)
{
    auto req = REQ(request);

    req->translationUnits[translationUnitIndex]->options->addPreprocessorDefine(key, value);
}

SLANG_API void spAddTranslationUnitSourceFile(
    SlangCompileRequest*    request,
    int                     translationUnitIndex,
    char const*             path)
{
    if(!request) return;
    auto req = REQ(request);
    if(!path) return;
    if(translationUnitIndex < 0) return;
    if(Slang::UInt(translationUnitIndex) >= req->translationUnits.Count()) return;

    req->addTranslationUnitSourceFile(
        translationUnitIndex,
        path);
}

// Add a source string to the given translation unit
SLANG_API void spAddTranslationUnitSourceString(
    SlangCompileRequest*    request,
    int                     translationUnitIndex,
    char const*             path,
    char const*             source)
{
    if(!request) return;
    auto req = REQ(request);
    if(!source) return;
    if(translationUnitIndex < 0) return;
    if(Slang::UInt(translationUnitIndex) >= req->translationUnits.Count()) return;

    if(!path) path = "";

    req->addTranslationUnitSourceString(
        translationUnitIndex,
        path,
        source);

}

SLANG_API SlangProfileID spFindProfile(
    SlangSession*,
    char const*     name)
{
    return Slang::Profile::LookUp(name).raw;
}

SLANG_API int spAddEntryPoint(
    SlangCompileRequest*    request,
    int                     translationUnitIndex,
    char const*             name,
    SlangProfileID          profile)
{
    if(!request) return -1;
    auto req = REQ(request);
    if(!name) return -1;
    if(translationUnitIndex < 0) return -1;
    if(Slang::UInt(translationUnitIndex) >= req->translationUnits.Count()) return -1;

    return req->addEntryPoint(
        translationUnitIndex,
        name,
        Slang::Profile(Slang::Profile::RawVal(profile)),
        Slang::List<Slang::String>());
}

SLANG_API int spAddEntryPointEx(
    SlangCompileRequest*    request,
    int                     translationUnitIndex,
    char const*             name,
    SlangProfileID          profile,
    int                     genericParamTypeNameCount,
    char const **           genericParamTypeNames)
{
    if (!request) return -1;
    auto req = REQ(request);
    if (!name) return -1;
    if (translationUnitIndex < 0) return -1;
    if (Slang::UInt(translationUnitIndex) >= req->translationUnits.Count()) return -1;
    Slang::List<Slang::String> typeNames;
    for (int i = 0; i < genericParamTypeNameCount; i++)
        typeNames.Add(genericParamTypeNames[i]);
    return req->addEntryPoint(
        translationUnitIndex,
        name,
        Slang::Profile(Slang::Profile::RawVal(profile)),
        typeNames);
}


// Compile in a context that already has its translation units specified
SLANG_API int spCompile(
    SlangCompileRequest*    request)
{
    auto req = REQ(request);

#if !defined(SLANG_DEBUG_INTERNAL_ERROR)
    // By default we'd like to catch as many internal errors as possible,
    // and report them to the user nicely (rather than just crash their
    // application). Internally Slang currently uses exceptions for this.
    //
    // TODO: Consider using `setjmp()`-style escape so that we can work
    // with applications that disable exceptions.
    //
    // TODO: Consider supporting Windows "Structured Exception Handling"
    // so that we can also recover from a wider class of crashes.
    try
    {
        int anyErrors = req->executeActions();
        return anyErrors;
    }
    catch (...)
    {
        req->mSink.diagnose(Slang::SourceLoc(), Slang::Diagnostics::compilationAborted);
        req->mDiagnosticOutput = req->mSink.outputBuffer.ProduceString();
        return 1;
    }
#else
    // When debugging, we probably don't want to filter out any errors, since
    // we are probably trying to root-cause and *fix* those errors.
    {
        int anyErrors = req->executeActions();
        return anyErrors;
    }
#endif
}

SLANG_API int
spGetDependencyFileCount(
    SlangCompileRequest*    request)
{
    if(!request) return 0;
    auto req = REQ(request);
    return (int) req->mDependencyFilePaths.Count();
}

/** Get the path to a file this compilation dependend on.
*/
SLANG_API char const*
spGetDependencyFilePath(
    SlangCompileRequest*    request,
    int                     index)
{
    if(!request) return 0;
    auto req = REQ(request);
    return req->mDependencyFilePaths[index].begin();
}

SLANG_API int
spGetTranslationUnitCount(
    SlangCompileRequest*    request)
{
    auto req = REQ(request);
    return (int) req->translationUnits.Count();
}

// Get the output code associated with a specific translation unit
SLANG_API char const* spGetTranslationUnitSource(
    SlangCompileRequest*    /*request*/,
    int                     /*translationUnitIndex*/)
{
    fprintf(stderr, "DEPRECATED: spGetTranslationUnitSource()\n");
    return nullptr;
}

SLANG_API void const* spGetEntryPointCode(
    SlangCompileRequest*    request,
    int                     entryPointIndex,
    size_t*                 outSize)
{
    auto req = REQ(request);

    // TODO: We should really accept a target index in this API
    auto& targets = req->getTargetRequests();
    auto targetCount = targets.Count();
    if (targetCount == 0)
        return nullptr;
    auto targetReq = targets[0];

    Slang::CompileResult& result = targetReq->entryPointResults[entryPointIndex];

    void const* data = nullptr;
    size_t size = 0;

    switch (result.format)
    {
    case Slang::ResultFormat::None:
    default:
        break;

    case Slang::ResultFormat::Binary:
        data = result.outputBinary.Buffer();
        size = result.outputBinary.Count();
        break;

    case Slang::ResultFormat::Text:
        data = result.outputString.Buffer();
        size = result.outputString.Length();
        break;
    }

    if(outSize) *outSize = size;
    return data;
}

SLANG_API char const* spGetEntryPointSource(
    SlangCompileRequest*    request,
    int                     entryPointIndex)
{
    return (char const*) spGetEntryPointCode(request, entryPointIndex, nullptr);
}

SLANG_API void const* spGetCompileRequestCode(
    SlangCompileRequest*    request,
    size_t*                 outSize)
{
    auto req = REQ(request);

    void const* data = req->generatedBytecode.Buffer();
    size_t size = req->generatedBytecode.Count();

    if(outSize) *outSize = size;
    return data;
}

// Reflection API

SLANG_API SlangReflection* spGetReflection(
    SlangCompileRequest*    request)
{
    if( !request ) return 0;
    auto req = REQ(request);

    // Note(tfoley): The API signature doesn't let the client
    // specify which target they want to access reflection
    // information for, so for now we default to the first one.
    //
    // TODO: Add a new `spGetReflectionForTarget(req, targetIndex)`
    // so that we can do this better, and make it clear that
    // `spGetReflection()` is shorthand for `targetIndex == 0`.
    //
    auto& targets = req->getTargetRequests();
    auto targetCount = targets.Count();
    if (targetCount == 0)
        return 0;
    auto targetReq = targets[0];

    return (SlangReflection*) targetReq->layout.Ptr();
}

// ... rest of reflection API implementation is in `Reflection.cpp`
