# We register the vocabulary header with `core` and enabled `slang-rt`.
# Each target's `slang-test-tool-util.cpp` compiles the enum through the retained compatibility
# header.
target_sources(core PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/tool-status-codes.h")
if(SLANG_ENABLE_SLANGRT)
    target_sources(
        slang-rt
        PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/tool-status-codes.h"
    )
endif()
