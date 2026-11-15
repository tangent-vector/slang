# The runtime compiles Foundation separately under its own export settings.
target_sources(core PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/random-numbers.cpp")
if(SLANG_ENABLE_SLANGRT)
    target_sources(
        slang-rt
        PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/random-numbers.cpp"
    )
endif()
