# The scalar declarations belong to both existing consumers and add no translation unit.
target_sources(core PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/scalar-types.h")
if(SLANG_ENABLE_SLANGRT)
    target_sources(
        slang-rt
        PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/scalar-types.h"
    )
endif()
