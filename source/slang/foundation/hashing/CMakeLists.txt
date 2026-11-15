# The inline and template Hashing implementation belongs to both existing consumers.
target_sources(core PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/hashing.h")
if(SLANG_ENABLE_SLANGRT)
    target_sources(slang-rt PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/hashing.h")
endif()
