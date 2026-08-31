add_library(cgal_triangulation STATIC
    src/tricall.cpp
)

target_link_libraries(cgal_triangulation PUBLIC CGAL::CGAL)

set_target_properties(cgal_triangulation PROPERTIES
    FOLDER utils_gpl/cgal_triangulation
    POSITION_INDEPENDENT_CODE ON
)
