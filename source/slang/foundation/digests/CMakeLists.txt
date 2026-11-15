# The existing core and runtime targets compile the one Digests implementation.
target_sources(core PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/digests.cpp")
if(SLANG_ENABLE_SLANGRT)
    target_sources(slang-rt PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/digests.cpp")
endif()
