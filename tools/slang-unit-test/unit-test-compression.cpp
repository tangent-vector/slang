// unit-compression.cpp
#include "core/slang-list.h"
#include "slang-com-ptr.h"
#include "slang/foundation/compression/compression.h"
#include "unit-test/slang-unit-test.h"

#include <string.h>

using namespace Slang;

static slang::foundation::compression::ICompressionSystem* _getCompressionSystem(
    slang::foundation::compression::CompressionSystemType type)
{
    switch (type)
    {
    case slang::foundation::compression::CompressionSystemType::Deflate:
        return slang::foundation::compression::DeflateCompressionSystem::getSingleton();
        break;
    case slang::foundation::compression::CompressionSystemType::LZ4:
        return slang::foundation::compression::LZ4CompressionSystem::getSingleton();
        break;
    default:
        break;
    }
    return nullptr;
}

SLANG_UNIT_TEST(compression)
{
    // Test out compression systems
    for (Index i = 0; i < Count(slang::foundation::compression::CompressionSystemType::CountOf);
         ++i)
    {
        slang::foundation::compression::ICompressionSystem* system =
            _getCompressionSystem(slang::foundation::compression::CompressionSystemType(i));

        if (!system)
        {
            continue;
        }

        const char src[] = "Some text to compress";
        size_t srcSize = sizeof(src);

        ComPtr<ISlangBlob> compressedBlob;

        // Use the default style
        slang::foundation::compression::CompressionStyle style;

        SLANG_CHECK(
            SLANG_SUCCEEDED(system->compress(&style, src, srcSize, compressedBlob.writeRef())));

        // Now lets decompress
        List<char> decompressedData;
        decompressedData.setCount(srcSize);

        SLANG_CHECK(SLANG_SUCCEEDED(system->decompress(
            compressedBlob->getBufferPointer(),
            compressedBlob->getBufferSize(),
            srcSize,
            decompressedData.getBuffer())));
        SLANG_CHECK(::memcmp(src, decompressedData.getBuffer(), srcSize) == 0);
    }
}
