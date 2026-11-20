// unit-test-module-ptr.cpp

#include "core/slang-memory-file-system.h"
#include "slang-com-ptr.h"
#include "slang.h"
#include "unit-test/slang-unit-test.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

using namespace Slang;

SLANG_UNIT_TEST(modulePtr)
{
    const char* testModuleSource = R"(
        module test_module;

        public void atomicFunc(__ref Atomic<int> ptr) {
            ptr.add(1);
        }
    )";

    const char* testSource = R"(
        import "test_module";

        RWStructuredBuffer<Atomic<int>> input0;

        [shader("compute")]
        [numthreads(1,1,1)]
        void computeMain(uint3 workGroup : SV_GroupID)
        {
            atomicFunc(input0[0]);
        }
    )";
    ComPtr<ISlangMutableFileSystem> memoryFileSystem =
        ComPtr<ISlangMutableFileSystem>(new Slang::MemoryFileSystem());

    ComPtr<slang::IGlobalSession> globalSession;
    SLANG_CHECK(slang_createGlobalSession(SLANG_API_VERSION, globalSession.writeRef()) == SLANG_OK);
    slang::TargetDesc targetDesc = {};
    targetDesc.format = SLANG_SPIRV;
    targetDesc.profile = globalSession->findProfile("spirv_1_5");
    slang::SessionDesc sessionDesc = {};
    sessionDesc.targetCount = 1;
    sessionDesc.targets = &targetDesc;
    sessionDesc.compilerOptionEntryCount = 0;
    sessionDesc.fileSystem = memoryFileSystem;

    // Precompile test_module to file.
    {
        ComPtr<slang::ISession> session;
        SLANG_CHECK(globalSession->createSession(sessionDesc, session.writeRef()) == SLANG_OK);

        ComPtr<slang::IBlob> diagnosticBlob;
        auto module = session->loadModuleFromSourceString(
            "test_module",
            "test_module.slang",
            testModuleSource,
            diagnosticBlob.writeRef());
        SLANG_CHECK(module != nullptr);

        ComPtr<slang::IBlob> moduleBlob;
        module->serialize(moduleBlob.writeRef());
        memoryFileSystem->saveFile(
            "test_module.slang-module",
            moduleBlob->getBufferPointer(),
            moduleBlob->getBufferSize());
    }

    // compile test.
    {
        ComPtr<slang::ISession> session;
        SLANG_CHECK(globalSession->createSession(sessionDesc, session.writeRef()) == SLANG_OK);

        ComPtr<slang::IBlob> diagnosticBlob;
        auto module = session->loadModuleFromSourceString(
            "test",
            "test.slang",
            testSource,
            diagnosticBlob.writeRef());
        SLANG_CHECK(module != nullptr);

        ComPtr<slang::IComponentType> linkedProgram;
        module->link(linkedProgram.writeRef());

        ComPtr<slang::IBlob> code;

        linkedProgram->getTargetCode(0, code.writeRef(), diagnosticBlob.writeRef());

        SLANG_CHECK(code->getBufferSize() > 0);
    }
}

SLANG_UNIT_TEST(precompiledModuleWithoutSourceWithUpToDateCheck)
{
    const char* testModuleSource = R"(
        module test_module;

        public int getValue()
        {
            return 7;
        }
    )";

    const char* testSource = R"(
        import "test_module";

        [shader("compute")]
        [numthreads(1,1,1)]
        void computeMain(uint3 workGroup : SV_GroupID)
        {
            int value = getValue();
            if (value == 7)
                return;
        }
    )";

    ComPtr<slang::IGlobalSession> globalSession;
    SLANG_CHECK(slang_createGlobalSession(SLANG_API_VERSION, globalSession.writeRef()) == SLANG_OK);

    slang::TargetDesc targetDesc = {};
    targetDesc.format = SLANG_SPIRV;
    targetDesc.profile = globalSession->findProfile("spirv_1_5");

    slang::SessionDesc sessionDesc = {};
    sessionDesc.targetCount = 1;
    sessionDesc.targets = &targetDesc;

    ComPtr<slang::IBlob> moduleBlob;

    {
        ComPtr<ISlangMutableFileSystem> memoryFileSystem =
            ComPtr<ISlangMutableFileSystem>(new Slang::MemoryFileSystem());

        ComPtr<slang::ISession> session;
        sessionDesc.fileSystem = memoryFileSystem;
        SLANG_CHECK(globalSession->createSession(sessionDesc, session.writeRef()) == SLANG_OK);

        ComPtr<slang::IBlob> diagnosticBlob;
        auto module = session->loadModuleFromSourceString(
            "test_module",
            "test_module.slang",
            testModuleSource,
            diagnosticBlob.writeRef());
        SLANG_CHECK(module != nullptr);
        SLANG_CHECK(module->serialize(moduleBlob.writeRef()) == SLANG_OK);
    }

    {
        ComPtr<ISlangMutableFileSystem> memoryFileSystem =
            ComPtr<ISlangMutableFileSystem>(new Slang::MemoryFileSystem());
        memoryFileSystem->saveFileBlob("test_module.slang-module", moduleBlob);

        slang::CompilerOptionEntry compilerOption = {};
        compilerOption.name = slang::CompilerOptionName::UseUpToDateBinaryModule;
        compilerOption.value.kind = slang::CompilerOptionValueKind::Int;
        compilerOption.value.intValue0 = 1;

        sessionDesc.fileSystem = memoryFileSystem;
        sessionDesc.compilerOptionEntries = &compilerOption;
        sessionDesc.compilerOptionEntryCount = 1;

        ComPtr<slang::ISession> session;
        SLANG_CHECK(globalSession->createSession(sessionDesc, session.writeRef()) == SLANG_OK);

        ComPtr<slang::IBlob> diagnosticBlob;
        auto precompiledModule = session->loadModule("test_module", diagnosticBlob.writeRef());
        SLANG_CHECK(precompiledModule != nullptr);

        auto module = session->loadModuleFromSourceString(
            "test",
            "test.slang",
            testSource,
            diagnosticBlob.writeRef());
        SLANG_CHECK(module != nullptr);

        ComPtr<slang::IComponentType> linkedProgram;
        SLANG_CHECK(module->link(linkedProgram.writeRef()) == SLANG_OK);
        SLANG_CHECK(linkedProgram != nullptr);

        ComPtr<slang::IBlob> code;
        SLANG_CHECK(
            linkedProgram->getTargetCode(0, code.writeRef(), diagnosticBlob.writeRef()) ==
            SLANG_OK);
        SLANG_CHECK(code != nullptr);
        SLANG_CHECK(code->getBufferSize() > 0);
    }
}

// A file-relative module import checks the roots before binding the qualified
// name. Those reads belong to the importing session, so adding a file to an
// earlier root cannot change that name's owner in the same session.
SLANG_UNIT_TEST(moduleImportReadsStayStableWithinSession)
{
    const char secondPalette[] = "public struct SecondPalette {}";
    const char consumer[] = R"(
        import palette;
        public struct Consumer { SecondPalette palette; }
    )";
    ComPtr<ISlangMutableFileSystem> fileSystem(new MemoryFileSystem());
    // `MemoryFileSystem` requires the parent directories before each save.
    const char* directories[] = {"first", "first/tools", "second", "second/tools"};
    for (const char* directory : directories)
    {
        SLANG_CHECK_ABORT(SLANG_SUCCEEDED(fileSystem->createDirectory(directory)));
    }
    SLANG_CHECK_ABORT(SLANG_SUCCEEDED(fileSystem->saveFile(
        "second/tools/palette.slang",
        secondPalette,
        sizeof(secondPalette) - 1)));
    SLANG_CHECK_ABORT(SLANG_SUCCEEDED(
        fileSystem->saveFile("second/tools/consumer.slang", consumer, sizeof(consumer) - 1)));

    // The memory filesystem exposes the extended interface, so the session
    // uses it directly. Inserting a file below changes what the provider
    // returns when the compiler actually reads that path.
    const char* searchPaths[] = {"first", "second"};
    slang::SessionDesc sessionDesc = {};
    sessionDesc.fileSystem = fileSystem;
    sessionDesc.searchPaths = searchPaths;
    sessionDesc.searchPathCount = SLANG_COUNT_OF(searchPaths);

    ComPtr<slang::IGlobalSession> globalSession;
    SLANG_CHECK_ABORT(
        slang_createGlobalSession(SLANG_API_VERSION, globalSession.writeRef()) == SLANG_OK);
    ComPtr<slang::ISession> originalSession;
    SLANG_CHECK_ABORT(
        globalSession->createSession(sessionDesc, originalSession.writeRef()) == SLANG_OK);

    // The file-relative import sees the second root's palette, as required
    // for the imported type in `Consumer`. The first root has no palette yet.
    SLANG_CHECK_ABORT(originalSession->loadModule("tools/consumer") != nullptr);
    const char firstPalette[] = "public struct FirstPalette {}";
    SLANG_CHECK_ABORT(SLANG_SUCCEEDED(
        fileSystem->saveFile("first/tools/palette.slang", firstPalette, sizeof(firstPalette) - 1)));

    auto originalPalette = originalSession->loadModule("tools/palette");
    SLANG_CHECK_ABORT(originalPalette != nullptr);
    const char* originalPath = originalPalette->getFilePath();
    SLANG_CHECK_ABORT(originalPath != nullptr);
    SLANG_CHECK(strcmp(originalPath, "second/tools/palette.slang") == 0);

    // The first root's file is still missing when requested by its workspace-relative path.
    // A lookup through `tools/palette` could return an already loaded module even if
    // the compiler accidentally read and saw the newly added file.
    SLANG_CHECK(originalSession->loadModule("first/tools/palette.slang") == nullptr);

    // The fresh session shares the provider and search paths. Its first lookup
    // sees the newly populated earlier root.
    ComPtr<slang::ISession> freshSession;
    SLANG_CHECK_ABORT(
        globalSession->createSession(sessionDesc, freshSession.writeRef()) == SLANG_OK);
    auto freshPalette = freshSession->loadModule("tools/palette");
    SLANG_CHECK_ABORT(freshPalette != nullptr);
    const char* freshPath = freshPalette->getFilePath();
    SLANG_CHECK_ABORT(freshPath != nullptr);
    SLANG_CHECK(strcmp(freshPath, "first/tools/palette.slang") == 0);
}

// A caller can register an existing file under an explicit module name.
// Discovering the same file through its path must reuse that module.
SLANG_UNIT_TEST(moduleImportReusesExplicitFileAlias)
{
    const char palettePath[] = "tools/palette.slang";
    const char paletteSource[] = "public struct Palette {}";
    ComPtr<ISlangMutableFileSystem> fileSystem(new MemoryFileSystem());
    SLANG_CHECK_ABORT(SLANG_SUCCEEDED(fileSystem->createDirectory("tools")));
    SLANG_CHECK_ABORT(SLANG_SUCCEEDED(
        fileSystem->saveFile(palettePath, paletteSource, sizeof(paletteSource) - 1)));

    slang::SessionDesc sessionDesc = {};
    sessionDesc.fileSystem = fileSystem;
    ComPtr<slang::IGlobalSession> globalSession;
    SLANG_CHECK_ABORT(
        slang_createGlobalSession(SLANG_API_VERSION, globalSession.writeRef()) == SLANG_OK);
    ComPtr<slang::ISession> session;
    SLANG_CHECK_ABORT(globalSession->createSession(sessionDesc, session.writeRef()) == SLANG_OK);

    auto aliasedPalette = session->loadModuleFromSourceString("alias", palettePath, paletteSource);
    SLANG_CHECK_ABORT(aliasedPalette != nullptr);

    // The request spells the file's path instead of the registered name `alias`.
    auto importedPalette = session->loadModule("tools/palette");
    SLANG_CHECK(importedPalette == aliasedPalette);
}
