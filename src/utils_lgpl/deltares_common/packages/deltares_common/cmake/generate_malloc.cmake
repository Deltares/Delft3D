# Generates malloc_generated.f90 — all realloc/reallocP overloads for all dtype/attr/rank combinations.
#
# Usage:
#   generate_malloc(OUTPUT_FILE <out_var>)
function(generate_malloc)
    cmake_parse_arguments(ARG "" "OUTPUT_FILE" "" ${ARGN})
    if(NOT ARG_OUTPUT_FILE)
        message(FATAL_ERROR "generate_malloc requires OUTPUT_FILE")
    endif()

    set(template_file "${CMAKE_CURRENT_SOURCE_DIR}/src/malloc_includes/malloc_body.f90.in")
    set(output_file "${CMAKE_CURRENT_SOURCE_DIR}/src/malloc_generated.f90")

    file(READ "${template_file}" body_template)

    # All supported Fortran types. The type string is used directly as the declaration.
    # Exception: character(len=*) gets a special DTYPE_TEMP (see below).
    # Note: integer(kind=1)/Byte is excluded here — it only exists as reallocByte2
    #       and is handled as a one-off in malloc.f90 alongside reallocString.
    set(dtypes
        "double precision"
        "real"
        "integer"
        "logical"
        "logical(kind=c_bool)"
        "character(len=*)"
    )

    # Subroutine name suffix per type, matching the original malloc.f90 naming
    set(dtype_name_double_precision       "Double")
    set(dtype_name_real                   "Real")
    set(dtype_name_integer                "Int")
    set(dtype_name_logical                "Logical")
    set("dtype_name_logical(kind=c_bool)" "Bool")
    set("dtype_name_character(len=*)"     "Character")

    # attr entries: p_prefix | fortran_attr | is_allocated_expr
    # move_alloc_stmt is stored separately because the pointer variant contains a semicolon
    set(attrs
        "|allocatable|allocated(arr)"
        "P|pointer|associated(arr)"
    )
    set(move_alloc_   "call move_alloc(temp, arr)")
    set(move_alloc_P  "if (associated(arr)) deallocate(arr); arr => temp")

    # All rank-varying fields that contain commas or semicolons are stored as
    # named variables per rank to avoid corrupting the CMake list split.
    set(ranks "1" "2" "3" "4")

    set(drank_1        "(:)")
    set(drank_2        "(:,:)")
    set(drank_3        "(:,:,:)")
    set(drank_4        "(:,:,:,:)")

    set(dindex_1       "integer")
    set(dindex_2       "integer, dimension(2)")
    set(dindex_3       "integer, dimension(3)")
    set(dindex_4       "integer, dimension(4)")

    set(lindex_one_1   "1")
    set(lindex_one_2   "(/1, 1/)")
    set(lindex_one_3   "(/1, 1, 1/)")
    set(lindex_one_4   "(/1, 1, 1, 1/)")

    set(shift_zero_1   "0")
    set(shift_zero_2   "(/0, 0/)")
    set(shift_zero_3   "(/0, 0, 0/)")
    set(shift_zero_4   "(/0, 0, 0, 0/)")

    set(allocate_temp_1 "allocate(temp(new_l_index:new_u_index))")
    set(allocate_temp_2 "allocate(temp(new_l_index(1):new_u_index(1), new_l_index(2):new_u_index(2)))")
    set(allocate_temp_3 "allocate(temp(new_l_index(1):new_u_index(1), new_l_index(2):new_u_index(2), new_l_index(3):new_u_index(3)))")
    set(allocate_temp_4 "allocate(temp(new_l_index(1):new_u_index(1), new_l_index(2):new_u_index(2), new_l_index(3):new_u_index(3), new_l_index(4):new_u_index(4)))")

    set(bounds_unchanged_1 "new_l_index == old_l_index .and. new_u_index == old_u_index .and. shift_ == 0")
    set(bounds_unchanged_2 "all(new_l_index == old_l_index) .and. all(new_u_index == old_u_index) .and. all(shift_ == 0)")
    set(bounds_unchanged_3 "${bounds_unchanged_2}")
    set(bounds_unchanged_4 "${bounds_unchanged_2}")

    set(get_bounds_1   "old_l_index = lbound(arr, 1); old_u_index = ubound(arr, 1)")
    set(get_bounds_2   "old_l_index = lbound(arr); old_u_index = ubound(arr)")
    set(get_bounds_3   "${get_bounds_2}")
    set(get_bounds_4   "${get_bounds_2}")

    set(overlap_nonempty_1 "data_l_index <= data_u_index")
    set(overlap_nonempty_2 "all(data_l_index <= data_u_index)")
    set(overlap_nonempty_3 "${overlap_nonempty_2}")
    set(overlap_nonempty_4 "${overlap_nonempty_2}")

    set(copy_section_1 "temp(data_l_index:data_u_index) = arr(data_l_index - shift_:data_u_index - shift_)")
    set(copy_section_2 "temp(data_l_index(1):data_u_index(1), data_l_index(2):data_u_index(2)) = arr(data_l_index(1) - shift_(1):data_u_index(1) - shift_(1), data_l_index(2) - shift_(2):data_u_index(2) - shift_(2))")
    set(copy_section_3 "temp(data_l_index(1):data_u_index(1), data_l_index(2):data_u_index(2), data_l_index(3):data_u_index(3)) = arr(data_l_index(1) - shift_(1):data_u_index(1) - shift_(1), data_l_index(2) - shift_(2):data_u_index(2) - shift_(2), data_l_index(3) - shift_(3):data_u_index(3) - shift_(3))")
    set(copy_section_4 "temp(data_l_index(1):data_u_index(1), data_l_index(2):data_u_index(2), data_l_index(3):data_u_index(3), data_l_index(4):data_u_index(4)) = arr(data_l_index(1) - shift_(1):data_u_index(1) - shift_(1), data_l_index(2) - shift_(2):data_u_index(2) - shift_(2), data_l_index(3) - shift_(3):data_u_index(3) - shift_(3), data_l_index(4) - shift_(4):data_u_index(4) - shift_(4))")

    set(realloc_procs "")
    set(reallocP_procs "")
    set(all_bodies "")

    foreach(rank_suffix IN LISTS ranks)
        set(DRANK            "${drank_${rank_suffix}}")
        set(DINDEX           "${dindex_${rank_suffix}}")
        set(LINDEX_ONE       "${lindex_one_${rank_suffix}}")
        set(SHIFT_ZERO       "${shift_zero_${rank_suffix}}")
        set(ALLOCATE_TEMP    "${allocate_temp_${rank_suffix}}")
        set(BOUNDS_UNCHANGED "${bounds_unchanged_${rank_suffix}}")
        set(GET_BOUNDS       "${get_bounds_${rank_suffix}}")
        set(OVERLAP_NONEMPTY "${overlap_nonempty_${rank_suffix}}")
        set(COPY_SECTION     "${copy_section_${rank_suffix}}")

        foreach(attr_entry IN LISTS attrs)
            string(REPLACE "|" ";" af "${attr_entry}")
            list(GET af 0 attr_prefix)
            list(GET af 1 DATTR)
            list(GET af 2 IS_ALLOCATED)
            set(MOVE_ALLOC "${move_alloc_${attr_prefix}}")

            foreach(DTYPE IN LISTS dtypes)
                set(dtype_name "${dtype_name_${DTYPE}}")

                # character(len=*) cannot be used for a local allocatable — use len=len(arr) instead
                if(DTYPE STREQUAL "character(len=*)")
                    set(DTYPE_TEMP "character(len=len(arr))")
                else()
                    set(DTYPE_TEMP "${DTYPE}")
                endif()

                set(proc_name "realloc${attr_prefix}${dtype_name}${rank_suffix}")

                # Render the body by substituting @VAR@ placeholders
                set(body "${body_template}")
                foreach(var DTYPE DTYPE_TEMP DATTR DRANK DINDEX
                            LINDEX_ONE SHIFT_ZERO IS_ALLOCATED GET_BOUNDS
                            BOUNDS_UNCHANGED ALLOCATE_TEMP OVERLAP_NONEMPTY
                            COPY_SECTION MOVE_ALLOC)
                    string(REPLACE "@${var}@" "${${var}}" body "${body}")
                endforeach()

                string(APPEND all_bodies
                    "\n   subroutine ${proc_name}(arr, uindex, lindex, stat, fill, shift, keepExisting)\n"
                    "${body}"
                    "   end subroutine ${proc_name}\n"
                )

                if(attr_prefix STREQUAL "P")
                    list(APPEND reallocP_procs "      module procedure ${proc_name}")
                else()
                    list(APPEND realloc_procs "      module procedure ${proc_name}")
                endif()
            endforeach()
        endforeach()
    endforeach()

    list(JOIN realloc_procs "\n" realloc_interface_procs)
    list(JOIN reallocP_procs "\n" reallocP_interface_procs)

    file(WRITE "${output_file}"
"! This file is generated by generate_malloc.cmake — do not edit manually.
   interface realloc
${realloc_interface_procs}
   end interface

   interface reallocP
${reallocP_interface_procs}
   end interface

contains
${all_bodies}")

    set(${ARG_OUTPUT_FILE} "${output_file}" PARENT_SCOPE)
endfunction()