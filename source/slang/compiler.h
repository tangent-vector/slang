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

    // A single translation unit requested to be compiled.
    //
    class TranslationUnitRequest : public RefObject
    {
    public:
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

        // The parsed syntax for the translation unit
        RefPtr<ModuleDecl>   SyntaxNode;

        // The IR-level code for this translation unit.
        // This will only be valid/non-null after semantic
        // checking and IR generation are complete, so it
        // is not safe to use this field without testing for NULL.
        IRModule* irModule;
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

    class LoadedModule;
    class Session;

    // Shared logic between `Linkage` and `CompileRequest`
    class CompileRequestBase : public Slang::RefObject
    {
    public:
        Slang::Session*    mSession;

        CompileRequestBase(Session* session)
            : mSession(session)
        {}

    };

}

// A linkage holds a collection of loaded modules, so that multiple
// compile requests don't need to load the same library code over
// and over again.
//
struct SlangLinkage : public Slang::CompileRequestBase
{
public:

    // Compile flags for this linkage
    SlangCompileFlags compileFlags = 0;

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

    void loadParsedModule(
        Slang::RefPtr<Slang::LoadedModule>           const& loadedModule,
        Slang::RefPtr<Slang::TranslationUnitRequest> const& translationUnit,
        Slang::Name*                                        name,
        Slang::String const&                                path);

    Slang::RefPtr<Slang::ModuleDecl> loadModule(
        Slang::LoadedModule*    loadedModule,
        Slang::Name*            name,
        Slang::String const&    path,
        Slang::String const&    source,
        Slang::SourceLoc const& loc);

    Slang::RefPtr<Slang::ModuleDecl> findOrImportModule(
        Slang::CompileRequest*  originalRequest,
        Slang::Name*            name,
        Slang::SourceLoc const& loc);

    void handlePoundImport(
        Slang::String const&    path,
        Slang::TokenList const& tokens);
};

namespace Slang
{
    typedef struct SlangLinkage Linkage;

    class CompileRequest : public CompileRequestBase
    {
    public:
        // Pointer to parent session
        Session* mSession;

        // Information on the targets we are being asked to
        // generate code for.
        List<RefPtr<TargetRequest>> targets;

        // What container format are we being asked to generate?
        ContainerFormat containerFormat = ContainerFormat::None;

        // Path to output container to
        String containerOutputPath;

        // Directories to search for `#include` files or `import`ed modules
        List<SearchDirectory> searchDirectories;

        // Definitions to provide during preprocessing
        Dictionary<String, String> preprocessorDefinitions;

        // Translation units we are being asked to compile
        List<RefPtr<TranslationUnitRequest> > translationUnits;

        // Entry points we've been asked to compile (each
        // assocaited with a translation unit).
        List<RefPtr<EntryPointRequest> > entryPoints;

        // The code generation profile we've been asked to use.
        Profile profile;

        // Should we just pass the input to another compiler?
        PassThroughMode passThrough = PassThroughMode::None;

        // Compile flags to be shared by all translation units
        SlangCompileFlags compileFlags = 0;

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

        Linkage* getLinkage() { return linkageForImports; }

        // We need to track a list of modules that have been `import`ed
        // directly or indireclty by this compile request, separate
        // from those that have been imported into the linkage. This
        // is because the linkage might have loaded many modules that
        // we don't use, and we don't want to pay for them.
        //
        // We track both a list (that we hope to keep in dependency
        // order), and a set (to avoid adding anything to the list twice).
        List<RefPtr<LoadedModule>>  importedModuleList;
        HashSet<RefPtr<LoadedModule>> importedModuleSet;

        RefPtr<ModuleDecl> findOrImportModule(
            Name*            name,
            SourceLoc const& loc);

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

    // Represents a module that has been loaded through the front-end
    // (up through IR generation).
    //
    // Inherits from `CompileRequest` so that the public API can
    // query it the same as any other compile request.
    //
    class LoadedModule : public CompileRequest
    {
    public:
        LoadedModule(
            Session* session)
            : CompileRequest(session)
        {}

        // The AST for the module
        RefPtr<ModuleDecl>  moduleDecl;

        // The IR for the module
        IRModule* irModule = nullptr;
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