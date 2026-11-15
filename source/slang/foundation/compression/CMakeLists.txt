# Both existing consumers compile the same Foundation compression implementation.
target_sources(core PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/compression.cpp")
if(SLANG_ENABLE_SLANGRT)
    target_sources(
        slang-rt
        PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/compression.cpp"
    )
endif()
