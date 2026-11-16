# The existing `core` and enabled `slang-rt` targets compile the one Versions implementation
# under their respective settings.
target_sources(
    core
    PRIVATE
        "${CMAKE_CURRENT_SOURCE_DIR}/versions.cpp"
        "${CMAKE_CURRENT_SOURCE_DIR}/versions.h"
)
if(SLANG_ENABLE_SLANGRT)
    target_sources(
        slang-rt
        PRIVATE
            "${CMAKE_CURRENT_SOURCE_DIR}/versions.cpp"
            "${CMAKE_CURRENT_SOURCE_DIR}/versions.h"
    )
endif()
