#include "slang.h"

#include "../core/slang-archive-file-system.h"
#include "../core/slang-castable.h"
#include "../core/slang-io.h"
#include "../core/slang-performance-profiler.h"
#include "../core/slang-platform.h"
#include "../core/slang-shared-library.h"
#include "../core/slang-string-util.h"
#include "../core/slang-string.h"
#include "../core/slang-type-convert-util.h"
#include "../core/slang-type-text-util.h"
// Artifact
#include "../compiler-core/slang-artifact-associated-impl.h"
#include "../compiler-core/slang-artifact-container-util.h"
#include "../compiler-core/slang-artifact-desc-util.h"
#include "../compiler-core/slang-artifact-impl.h"
#include "../compiler-core/slang-artifact-util.h"
#include "../compiler-core/slang-source-loc.h"
#include "../core/slang-file-system.h"
#include "../core/slang-memory-file-system.h"
#include "../core/slang-writer.h"
#include "core/slang-shared-library.h"
#include "slang-ast-dump.h"
#include "slang-check-impl.h"
#include "slang-check.h"
#include "slang-doc-ast.h"
#include "slang-doc-markdown-writer.h"
#include "slang-ir.h"
#include "slang-lookup.h"
#include "slang-lower-to-ir.h"
#include "slang-mangle.h"
#include "slang-module-library.h"
#include "slang-options.h"
#include "slang-parameter-binding.h"
#include "slang-parser.h"
#include "slang-preprocessor.h"
#include "slang-reflection-json.h"
#include "slang-repro.h"
#include "slang-serialize-ast.h"
#include "slang-serialize-container.h"
#include "slang-serialize-ir.h"
#include "slang-tag-version.h"
#include "slang-type-layout.h"

#include <sys/stat.h>

// Used to print exception type names in internal-compiler-error messages
#include <typeinfo>

namespace Slang
{

const char* getBuildTagString()
{
    if (UnownedStringSlice(SLANG_TAG_VERSION) == "0.0.0-unknown")
    {
        // If the tag is unknown, then we will try to get the timestamp of the shared library
        // and use that as the version string, so that we can at least return something
        // that uniquely identifies the build.
        static String timeStampString =
            String(SharedLibraryUtils::getSharedLibraryTimestamp((void*)spCreateSession));
        return timeStampString.getBuffer();
    }
    return SLANG_TAG_VERSION;
}

} // namespace Slang
