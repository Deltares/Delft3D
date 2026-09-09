add_executable(dflowfm_io_tool
    dflowfm_io_tool.cpp
)

target_link_libraries(dflowfm_io_tool
    PRIVATE dflowfm_io
)
