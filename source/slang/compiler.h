#ifndef RASTER_SHADER_COMPILER_H
#define RASTER_SHADER_COMPILER_H

#include "../core/basic.h"

#include "diagnostics.h"
#include "name.h"
#include "profile.h"
#include "syntax.h"

#include "../../slang.h"

namespace Slang
{
    struct IncludeHandler;
    class CompileRequest;
    class ProgramLayout;
    class PtrType;

    enum class CompilerMode
    {
        ProduceLibrary,
        ProduceShader,
        GenerateChoice
    };

    enum class StageTarget
    {
        Unknown,
        VertexShader,
        HullShader,
        DomainShader,
        GeometryShader,
        FragmentShader,
        ComputeShader,
    };

    enum class CodeGenTarget
    {
        Unknown             = SLANG_TARGET_UNKNOWN,
        None                = SLANG_TARGET_NONE,
        GLSL                = SLANG_GLSL,
        GLSL_Vulkan         = SLANG_GLSL_VULKAN,
        GLSL_Vulkan_OneDesc = SLANG_GLSL_VULKAN_ONE_DESC,
        HLSL                = SLANG_HLSL,
        SPIRV               = SLANG_SPIRV,
        SPIRVAssembly       = SLANG_SPIRV_ASM,
        DXBytecode          = SLANG_DXBC,
        DXBytecodeAssembly  = SLANG_DXBC_ASM,
        DXIL                = SLANG_DXIL,
        DXILAssembly        = SLANG_DXIL_ASM,
    };

    enum class ContainerFormat
    {
        None            = SLANG_CONTAINER_FORMAT_NONE,
        SlangModule     = SLANG_CONTAINER_FORMAT_SLANG_MODULE,
    };

    enum class LineDirectiveMode : SlangLineDirectiveMode
    {
        Default     = SLANG_LINE_DIRECTIVE_MODE_DEFAULT,
        None        = SLANG_LINE_DIRECTIVE_MODE_NONE,
        Standard    = SLANG_LINE_DIRECTIVE_MODE_STANDARD,
        GLSL        = SLANG_LINE_DIRECTIVE_MODE_GLSL,
    };

    enum class ResultFormat
    {
        None,
        Text,
        Binary
    };

    class CompileRequest;
    class TranslationUnitRequest;

    // Result of compiling an entry point.
    // Should only ever be string OR binary.
    class CompileResult
    {
    public:
        CompileResult() = default;
        CompileResult(String const& str) : format(ResultFormat::Text), outputString(str) {}
        CompileResult(List<uint8_t> const& buffer) : format(ResultFormat::Binary), outputBinary(buffer) {}

        void append(CompileResult const& result);

        ResultFormat format = ResultFormat::None;
        String outputString;
        List<uint8_t> outputBinary;
    };

    // Describes an entry point that we've been requested to compile
    class EntryPointRequest : public RefObject
    {
    public:
        // The parent compile request
        CompileRequest* compileRequest = nullptr;

        // The name of the entry point function (e.g., `main`)
        Name* name;
        
        // The type names we want to substitute into the 
        // global generic type parameters
        List<Name*> genericParameterTypeNames;

        // The profile that the entry point will be compiled for
        // (this is a combination of the target state, and also
        // a feature level that sets capabilities)
        Profile profile;

        // The index of the translation unit (within the parent
        // compile request) that the entry point function is
        // supposed to be defined in.
        int translationUnitIndex;

        // The output path requested for this entry point.
        // (only used when compiling from the command line)
        String outputPath;

        // The translation unit that this entry point came from
        TranslationUnitRequest* getTranslationUnit();

        // The declaration of the entry-point function itself.
        // This will be filled in as part of semantic analysis;
        // it should not be assumed to be available in cases
        // where any errors were diagnosed.
        RefPtr<FuncDecl> decl;

        // The declaration of the global generic parameter types
        // This will be filled in as part of semantic analysis.
        List<RefPtr<Type>> genericParameterTypes;
        List<RefPtr<Val>> genericParameterWitnesses;
    };

    enum class PassThroughMode : SlangPassThrough
    {
        None = SLANG_PASS_THROUGH_NONE,	// don't pass through: use Slang compiler
        fxc = SLANG_PASS_THROUGH_FXC,	// pass through HLSL to `D3DCompile` API
        dxc = SLANG_PASS_THROUGH_DXC,	// pass through HLSL to `IDxcCompiler` API
        glslang = SLANG_PASS_THROUGH_GLSLANG,	// pass through GLSL to `glslang` library
    };

    class SourceFile;

    // Represents a module that has been loaded through the front-end
    // (up through IR generation).
    //
    // Inherits from `CompileRequest` so that the public API can
    // query it the same as any other compile request.
    //
    class LoadedModule : public RefObject
    {
    public:
        // The AST for the module
        RefPtr<ModuleDecl>  moduleDecl;

        // The IR for the module
        IRModule* irModule = nullptr;

        // An ordered list of other modules that this module depends on.
        // This is not just the modules it directly `import`s, but also
        // those that are transitively imported. The order of the list
        // reflects dependency order: later entries may depend on earlier
        // entries, but not vice versa.
        List<RefPtr<LoadedModule>>  importedModules;

    };

    typedef struct SlangLinkage Linkage;

    // A single translation unit requested to be compiled.
    // Each translation unit will turn into a single `LoadedModule`
    // after compilation.
    //
    class TranslationUnitRequest : public RefObject
    {
    public:
        TranslationUnitRequest();

        // The parent compile request
        CompileRequest* compileRequest = nullptr;

        // The language in which the source file(s)
        // are assumed to be written
        SourceLanguage sourceLanguage = SourceLanguage::Unknown;

        // The source file(s) that will be compiled to form this translation unit
        //
        // Usually, for HLSL or GLSL there will be only one file.
        List<RefPtr<SourceFile> > sourceFiles;

        // The entry points associated with this translation unit
        List<RefPtr<EntryPointRequest> > entryPoints;

        // Preprocessor definitions to use for this translation unit only
        // (whereas the ones on `CompileOptions` will be shared)
        Dictionary<String, String> preprocessorDefinitions;

        // Compile flags for this translation unit
        SlangCompileFlags compileFlags = 0;

        // The module that results from this translation unit.
        // This will be filled in during the compilation process,
        // so in general it isn't safe to use this field or
        // the fields inside it without checking for NULL.
        RefPtr<LoadedModule>    module;

        RefPtr<ModuleDecl> getModuleDecl() { return module->moduleDecl; }
        IRModule* getIRModule() { return module->irModule; }

        Linkage* getLinkage();

        // While compiling, we need to build the list of modules
        // that the translation unit depends on; these are stored
        // in `module->importedModules`. We don't want to add
        // anything to that list twice, so we track which modules
        // have already been added here.
        HashSet<RefPtr<LoadedModule>> importedModuleSet;

        void registerImportedModule(
            RefPtr<LoadedModule>    loadedModule);
        void registerImportedModuleImpl(
            RefPtr<LoadedModule>    loadedModule);

        RefPtr<LoadedModule> findOrImportModule(
            Name*            name,
            SourceLoc const& loc);
    };

    // A request to generate output in some target format
    class TargetRequest : public RefObject
    {
    public:
        CompileRequest*     compileRequest;
        CodeGenTarget       target;
        SlangTargetFlags    targetFlags = 0;
        Slang::Profile      targetProfile = Slang::Profile::Unknown;

        // The resulting reflection layout information
        RefPtr<ProgramLayout> layout;

        // Generated compile results for each entry point
        // in the parent compile request (indexing matches
        // the order they are given in the compile request)
        List<CompileResult> entryPointResults;
    };

    // A directory to be searched when looking for files (e.g., `#include`)
    struct SearchDirectory
    {
        SearchDirectory() = default;
        SearchDirectory(SearchDirectory const& other) = default;
        SearchDirectory(String const& path)
            : path(path)
        {}

        String  path;
    };

    class Session;

    // Shared logic between `Linkage` and `CompileRequest`
    class CompileRequestBase : public Slang::RefObject
    {
    public:
        Slang::Session*    mSession;

        // Compile flags to be shared by all translation units
        SlangCompileFlags compileFlags = 0;

        // Directories to search for `#include` files or `import`ed modules
        List<SearchDirectory> searchDirectories;

        // Definitions to provide during preprocessing
        Dictionary<String, String> preprocessorDefinitions;

        CompileRequestBase(Session* session)
            : mSession(session)
        {}

        void copyConfigurationFrom(CompileRequestBase* other);
    };

}

// A linkage holds a collection of loaded modules, so that multiple
// compile requests don't need to load the same library code over
// and over again.
//
struct SlangLinkage : public Slang::CompileRequestBase
{
public:
    // Modules that have been dynamically loaded via `import`
    //
    // This is a list of unique modules loaded, in the order they were encountered.
    Slang::List<Slang::RefPtr<Slang::LoadedModule> > loadedModulesList;

    // Map from the path of a module file to its definition
    Slang::Dictionary<Slang::String, Slang::RefPtr<Slang::LoadedModule>> mapPathToLoadedModule;

    // Map from the logical name of a module to its definition
    Slang::Dictionary<Slang::Name*, Slang::RefPtr<Slang::LoadedModule>> mapNameToLoadedModules;


    SlangLinkage(Slang::Session* session)
        : Slang::CompileRequestBase(session)
    {}

    Slang::RefPtr<Slang::LoadedModule> loadParsedModule(
        Slang::RefPtr<Slang::TranslationUnitRequest> const& translationUnit,
        Slang::Name*                                        name,
        Slang::String const&                                path);

    Slang::RefPtr<Slang::LoadedModule> loadModule(
        Slang::CompileRequest*  compileRequest,
        Slang::Name*            name,
        Slang::String const&    path,
        Slang::String const&    source,
        Slang::SourceLoc const& loc);

    Slang::RefPtr<Slang::LoadedModule> findOrImportModule(
        Slang::CompileRequest*  originalRequest,
        Slang::Name*            name,
        Slang::SourceLoc const& loc);

    void handlePoundImport(
        Slang::String const&    path,
        Slang::TokenList const& tokens);
};

namespace Slang
{
    class CompileRequest : public CompileRequestBase
    {
    public:
        // Information on the targets we are being asked to
        // generate code for.
        List<RefPtr<TargetRequest>> targets;

        // What container format are we being asked to generate?
        ContainerFormat containerFormat = ContainerFormat::None;

        // Path to output container to
        String containerOutputPath;

        // Translation units we are being asked to compile
        List<RefPtr<TranslationUnitRequest> > translationUnits;

        // Entry points we've been asked to compile (each
        // assocaited with a translation unit).
        List<RefPtr<EntryPointRequest> > entryPoints;

        // The code generation profile we've been asked to use.
        Profile profile;

        // Should we just pass the input to another compiler?
        PassThroughMode passThrough = PassThroughMode::None;

        // Should we dump intermediate results along the way, for debugging?
        bool shouldDumpIntermediates = false;

        bool shouldDumpIR = false;
        bool shouldSkipCodegen = false;

        // How should `#line` directives be emitted (if at all)?
        LineDirectiveMode lineDirectiveMode = LineDirectiveMode::Default;

        // Are we being driven by the command-line `slangc`, and should act accordingly?
        bool isCommandLineCompile = false;

        // Source manager to help track files loaded
        SourceManager sourceManagerStorage;
        SourceManager* sourceManager;

        // Name pool for looking up names
        NamePool namePool;

        NamePool* getNamePool() { return &namePool; }

        // Output stuff
        DiagnosticSink mSink;
        String mDiagnosticOutput;

        // Files that compilation depended on
        List<String> mDependencyFilePaths;

        // Generated bytecode representation of all the code
        List<uint8_t> generatedBytecode;

        // The linkage to use when `import`ing modules
        RefPtr<Linkage> linkageForImports;

        Linkage* getLinkage();

        void handlePoundImport(
            String const&    path,
            TokenList const& tokens);

        CompileRequest(Session* session);

        ~CompileRequest();

        void parseTranslationUnit(
            TranslationUnitRequest* translationUnit);

        // Perform primary semantic checking on all
        // of the translation units in the program
        void checkAllTranslationUnits();

        void generateIR();

        int executeActionsInner();
        int executeActions();

        int addTranslationUnit(SourceLanguage language, String const& name);

        void addTranslationUnitSourceFile(
            int             translationUnitIndex,
            SourceFile*     sourceFile);

        void addTranslationUnitSourceString(
            int             translationUnitIndex,
            String const&   path,
            String const&   source);

        void addTranslationUnitSourceFile(
            int             translationUnitIndex,
            String const&   path);

        int addEntryPoint(
            int                     translationUnitIndex,
            String const&           name,
            Profile                 profile,
            List<String> const &    genericTypeNames);

        UInt addTarget(
            CodeGenTarget   target);

        SourceManager* getSourceManager()
        {
            return sourceManager;
        }

        void setSourceManager(SourceManager* sm)
        {
            sourceManager = sm;
            mSink.sourceManager = sm;
        }
    };

    void generateOutput(
        CompileRequest* compileRequest);

    // Helper to dump intermediate output when debugging
    void maybeDumpIntermediate(
        CompileRequest* compileRequest,
        void const*     data,
        size_t          size,
        CodeGenTarget   target);
    void maybeDumpIntermediate(
        CompileRequest* compileRequest,
        char const*     text,
        CodeGenTarget   target);

    //

    class Session
    {
    public:
        //

        RefPtr<Scope>   baseLanguageScope;
        RefPtr<Scope>   coreLanguageScope;
        RefPtr<Scope>   hlslLanguageScope;
        RefPtr<Scope>   slangLanguageScope;
        RefPtr<Scope>   glslLanguageScope;

        List<RefPtr<ModuleDecl>> loadedModuleCode;

        SourceManager   builtinSourceManager;

        SourceManager* getBuiltinSourceManager() { return &builtinSourceManager; }

        // Name pool stuff for unique-ing identifiers

        RootNamePool rootNamePool;
        NamePool namePool;

        RootNamePool* getRootNamePool() { return &rootNamePool; }
        NamePool* getNamePool() { return &namePool; }

        //

        // Generated code for stdlib, etc.
        String stdlibPath;
        String coreLibraryCode;
        String slangLibraryCode;
        String hlslLibraryCode;
        String glslLibraryCode;

        String getStdlibPath();
        String getCoreLibraryCode();
        String getHLSLLibraryCode();
        String getGLSLLibraryCode();

        // Basic types that we don't want to re-create all the time
        RefPtr<Type> errorType;
        RefPtr<Type> initializerListType;
        RefPtr<Type> overloadedType;
        RefPtr<Type> irBasicBlockType;

        Dictionary<int, RefPtr<Type>> builtinTypes;
        Dictionary<String, Decl*> magicDecls;
        List<RefPtr<Type>> canonicalTypes;

        void initializeTypes();

        Type* getBoolType();
        Type* getFloatType();
        Type* getDoubleType();
        Type* getIntType();
        Type* getUIntType();
        Type* getVoidType();
        Type* getBuiltinType(BaseType flavor);

        Type* getInitializerListType();
        Type* getOverloadedType();
        Type* getErrorType();

        // Should not be used in front-end code
        Type* getIRBasicBlockType();

        // Construct the type `Ptr<valueType>`, where `Ptr`
        // is looked up as a builtin type.
        RefPtr<PtrType> getPtrType(RefPtr<Type> valueType);

        // Construct the type `Out<valueType>`
        RefPtr<OutType> getOutType(RefPtr<Type> valueType);

        // Construct the type `InOut<valueType>`
        RefPtr<InOutType> getInOutType(RefPtr<Type> valueType);

        // Construct a pointer type like `Ptr<valueType>`, but where
        // the actual type name for the pointer type is given by `ptrTypeName`
        RefPtr<PtrTypeBase> getPtrType(RefPtr<Type> valueType, char const* ptrTypeName);

        // Construct a pointer type like `Ptr<valueType>`, but where
        // the generic declaration for the pointer type is `genericDecl`
        RefPtr<PtrTypeBase> getPtrType(RefPtr<Type> valueType, GenericDecl* genericDecl);

        RefPtr<ArrayExpressionType> getArrayType(
            Type*   elementType,
            IntVal* elementCount);

        RefPtr<GroupSharedType> getGroupSharedType(RefPtr<Type> valueType);

        SyntaxClass<RefObject> findSyntaxClass(Name* name);

        Dictionary<Name*, SyntaxClass<RefObject> > mapNameToSyntaxClass;

        //

        Session();

        void addBuiltinSource(
            RefPtr<Scope> const&    scope,
            String const&           path,
            String const&           source);
    };

}

#endif