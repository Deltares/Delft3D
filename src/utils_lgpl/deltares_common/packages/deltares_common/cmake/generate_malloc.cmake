# Generates malloc_generated.f90 — all realloc/reallocP overloads for all dtype/attr/rank combinations.
#
# Usage:
#   generate_malloc(OUTPUT_FILE <out_var>)
function(generate_malloc)
    file(READ "${malloc_template_file}" body_template)

    # All supported Fortran types. The type string is used directly as the declaration.
    # Exception: character(len=*) gets a special DTYPE_TEMP (see below).
    # Note: integer(kind=1)/Byte is excluded here — it only exists as reallocByte2
    #       and is handled as a one-off in malloc.f90 alongside reallocString.
set(dtypes
    "real(dp)"
    "real(sp)"
    "integer"
    "integer(kind=1)"
    "logical"
    "logical(kind=c_bool)"
    "character(len=*)"
)
set(dtype_names
    "Double"
    "Real"
    "Int"
    "Byte"
    "Logical"
    "Bool"
    "Character"
)

    # attr entries: p_prefix | fortran_attr | is_allocated_expr
    # move_alloc_stmt is stored separately because the pointer variant contains a semicolon
    set(attr_prefixes    ""             "P")
    set(attr_fortran     "allocatable"  "pointer")
    set(attr_is_alloc    "allocated(arr)" "associated(arr)")
    set(attr_move_alloc
    "call move_alloc(temp, arr)"
    "if (associated(arr)) deallocate(arr,stat=stat_)\n      arr => temp"
)

    # All rank-varying fields that contain commas or semicolons are stored as
    # named variables per rank to avoid corrupting the CMake list split.
    set(ranks "1" "2" "3" "4")

    set(drank_list        "(:)"  "(:,:)"  "(:,:,:)"  "(:,:,:,:)")
    set(dindex_list       "integer"  "integer, dimension(2)"  "integer, dimension(3)"  "integer, dimension(4)")

  set(lindex_one_list
    "1"
    "(/1, 1/)"
    "(/1, 1, 1/)"
    "(/1, 1, 1, 1/)"
)
set(shift_zero_list
    "0"
    "(/0, 0/)"
    "(/0, 0, 0/)"
    "(/0, 0, 0, 0/)"
)

set(allocate_temp_list
    "allocate(temp(new_l_index:uindex), stat=stat_)"
    "allocate(temp(new_l_index(1):uindex(1), new_l_index(2):uindex(2)), stat=stat_)"
    "allocate(temp(new_l_index(1):uindex(1), new_l_index(2):uindex(2), new_l_index(3):uindex(3)), stat=stat_)"
    "allocate(temp(new_l_index(1):uindex(1), new_l_index(2):uindex(2), new_l_index(3):uindex(3), new_l_index(4):uindex(4)), stat=stat_)"
)

    set(_bounds_unchanged_1 "new_l_index == old_l_index .and. uindex == old_u_index .and. shift_ == 0")
    set(_bounds_unchanged_N "all(new_l_index == old_l_index) .and. all(uindex == old_u_index) .and. all(shift_ == 0)")
    set(bounds_unchanged_list
        "${_bounds_unchanged_1}"
        "${_bounds_unchanged_N}"
        "${_bounds_unchanged_N}"
        "${_bounds_unchanged_N}"
    )

    set(_get_bounds_1 "old_l_index = lbound(arr, 1)
         old_u_index = ubound(arr, 1)")
    set(_get_bounds_N "old_l_index = lbound(arr)
         old_u_index = ubound(arr)")
    set(get_bounds_list
        "${_get_bounds_1}"
        "${_get_bounds_N}"
        "${_get_bounds_N}"
        "${_get_bounds_N}"
    )

    set(_overlap_nonempty_1 "data_l_index <= data_u_index")
    set(_overlap_nonempty_N "all(data_l_index <= data_u_index)")
    set(overlap_nonempty_list
        "${_overlap_nonempty_1}"
        "${_overlap_nonempty_N}"
        "${_overlap_nonempty_N}"
        "${_overlap_nonempty_N}"
    )

set(_cs1 "temp(data_l_index:data_u_index) = arr(data_l_index - shift_:data_u_index - shift_)")
set(_sub2 "data_l_index(1):data_u_index(1), data_l_index(2):data_u_index(2)")
set(_shr2 "data_l_index(1) - shift_(1):data_u_index(1) - shift_(1), data_l_index(2) - shift_(2):data_u_index(2) - shift_(2)")
#incrementally build 3 and 4 by appending to the 2D versions
set(_sub3 "${_sub2}, data_l_index(3):data_u_index(3)")
set(_shr3 "${_shr2}, data_l_index(3) - shift_(3):data_u_index(3) - shift_(3)")
set(_sub4 "${_sub3}, data_l_index(4):data_u_index(4)")
set(_shr4 "${_shr3}, data_l_index(4) - shift_(4):data_u_index(4) - shift_(4)")

set(copy_section_list
    "${_cs1}"
    "temp(${_sub2}) = arr(${_shr2})"
    "temp(${_sub3}) = arr(${_shr3})"
    "temp(${_sub4}) = arr(${_shr4})"
)
    set(realloc_procs "")
    set(reallocP_procs "")
    set(all_bodies "")

    foreach(rank_idx RANGE 3)
        math(EXPR rank_suffix "${rank_idx} + 1")
        list(GET drank_list            ${rank_idx} DRANK)
        list(GET dindex_list           ${rank_idx} DINDEX)
        list(GET lindex_one_list       ${rank_idx} LINDEX_ONE)
        list(GET shift_zero_list       ${rank_idx} SHIFT_ZERO)
        list(GET allocate_temp_list    ${rank_idx} ALLOCATE_TEMP)
        list(GET bounds_unchanged_list ${rank_idx} BOUNDS_UNCHANGED)
        list(GET get_bounds_list       ${rank_idx} GET_BOUNDS)
        list(GET overlap_nonempty_list ${rank_idx} OVERLAP_NONEMPTY)
        list(GET copy_section_list     ${rank_idx} COPY_SECTION)

        foreach(attr_idx RANGE 1)
            list(GET attr_prefixes   ${attr_idx} attr_prefix)
            list(GET attr_fortran    ${attr_idx} DATTR)
            list(GET attr_is_alloc   ${attr_idx} IS_ALLOCATED)
            list(GET attr_move_alloc ${attr_idx} MOVE_ALLOC)

            foreach(DTYPE IN LISTS dtypes)
                list(FIND dtypes "${DTYPE}" dtype_idx)
                list(GET dtype_names ${dtype_idx} dtype_name)

                # character(len=*) cannot be used for a local allocatable — use len=len(arr) instead
                if(DTYPE STREQUAL "character(len=*)")
                    set(DTYPE_TEMP "character(len=len(arr))")
                else()
                    set(DTYPE_TEMP "${DTYPE}")
                endif()

                set(PROC_NAME "realloc${attr_prefix}${dtype_name}${rank_suffix}")

                # Render the body by substituting @VAR@ placeholders
                set(body "${body_template}")
                foreach(var PROC_NAME DTYPE DTYPE_TEMP DATTR DRANK DINDEX
                            LINDEX_ONE SHIFT_ZERO IS_ALLOCATED GET_BOUNDS
                            BOUNDS_UNCHANGED ALLOCATE_TEMP OVERLAP_NONEMPTY
                            COPY_SECTION MOVE_ALLOC)
                    string(REPLACE "@${var}@" "${${var}}" body "${body}")
                endforeach()

                string(APPEND all_bodies "\n${body}\n")

                if(attr_prefix STREQUAL "P")
                    list(APPEND reallocP_procs "      module procedure ${PROC_NAME}")
                else()
                    list(APPEND realloc_procs "      module procedure ${PROC_NAME}")
                endif()
            endforeach()
        endforeach()
    endforeach()

    list(JOIN realloc_procs "\n" realloc_interface_procs)
    list(JOIN reallocP_procs "\n" reallocP_interface_procs)

    file(WRITE "${malloc_output_file}"
"! This file is generated by generate_malloc.cmake — do not edit manually.
module m_alloc_generated
   use stdlib_kinds, only: c_bool
   use precision, only: dp, sp
   implicit none

   interface realloc
${realloc_interface_procs}
   end interface

   interface reallocP
${reallocP_interface_procs}
   end interface

contains
${all_bodies}
end module m_alloc_generated")
endfunction()