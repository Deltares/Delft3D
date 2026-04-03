# Generates malloc_generated.f90 — all realloc/reallocP overloads for all dtype/attr/rank combinations.

# Renders a subroutine body by substituting all @VAR@ placeholders.
# Result is returned in the variable named by output_var in the parent scope.
function(render_template template output_var)
    set(body "${template}")
    foreach(var PROC_NAME DTYPE DTYPE_TEMP DATTR DRANK DINDEX
                LINDEX_ONE SHIFT_ZERO IS_ALLOCATED GET_BOUNDS
                BOUNDS_UNCHANGED ALLOCATE_TEMP OVERLAP_NONEMPTY
                COPY_SECTION MOVE_ALLOC)
        string(REPLACE "@${var}@" "${${var}}" body "${body}")
    endforeach()
    set(${output_var} "${body}" PARENT_SCOPE)
endfunction()

# Generates all type-specific subroutine bodies for a given rank and attribute (allocatable/pointer).
# Results are appended to all_bodies, realloc_procs, and reallocP_procs in the parent scope.
function(generate_realloc rank attr)
    # read rank as an integer for indexing lists, shifted by 1
    math(EXPR rank_idx "${rank} - 1")
    file(READ "${malloc_template_file}" body_template)
        # Attribute-varying data
    if(attr STREQUAL "pointer")
        set(attr_prefix  "P")
        set(DATTR        "pointer")
        set(IS_ALLOCATED "associated(arr)")
        set(MOVE_ALLOC   "if (associated(arr)) deallocate(arr,stat=stat_)\n      arr => temp")
    else() # allocatable, is default
        set(attr_prefix  "")
        set(DATTR        "allocatable")
        set(IS_ALLOCATED "allocated(arr)")
        set(MOVE_ALLOC   "call move_alloc(temp, arr)")
    endif()

    #rank varying data, create lists for each variable with one entry per rank, then get the right one by index
    set(drank_list        "(:)"  "(:,:)"  "(:,:,:)"  "(:,:,:,:)")
    set(dindex_list       "integer"  "integer, dimension(2)"  "integer, dimension(3)"  "integer, dimension(4)")
    set(lindex_one_list   "1"  "(/1, 1/)"  "(/1, 1, 1/)"  "(/1, 1, 1, 1/)")
    set(shift_zero_list   "0"  "(/0, 0/)"  "(/0, 0, 0/)"  "(/0, 0, 0, 0/)")
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
    set(temp2 "data_l_index(1):data_u_index(1), data_l_index(2):data_u_index(2)")
    set(temp3 "${temp2}, data_l_index(3):data_u_index(3)")
    set(temp4 "${temp3}, data_l_index(4):data_u_index(4)")
    set(arr2  "data_l_index(1) - shift_(1):data_u_index(1) - shift_(1), data_l_index(2) - shift_(2):data_u_index(2) - shift_(2)")
    set(arr3  "${arr2}, data_l_index(3) - shift_(3):data_u_index(3) - shift_(3)")
    set(arr4  "${arr3}, data_l_index(4) - shift_(4):data_u_index(4) - shift_(4)")
    set(copy_section_list
        "temp(data_l_index:data_u_index) = arr(data_l_index - shift_:data_u_index - shift_)"
        "temp(${temp2}) = arr(${arr2})"
        "temp(${temp3}) = arr(${arr3})"
        "temp(${temp4}) = arr(${arr4})"
    )
    #get the correct rank-specific value from each list by index
    list(GET drank_list            ${rank_idx} DRANK)
    list(GET dindex_list           ${rank_idx} DINDEX)
    list(GET lindex_one_list       ${rank_idx} LINDEX_ONE)
    list(GET shift_zero_list       ${rank_idx} SHIFT_ZERO)
    list(GET allocate_temp_list    ${rank_idx} ALLOCATE_TEMP)
    list(GET bounds_unchanged_list ${rank_idx} BOUNDS_UNCHANGED)
    list(GET get_bounds_list       ${rank_idx} GET_BOUNDS)
    list(GET overlap_nonempty_list ${rank_idx} OVERLAP_NONEMPTY)
    list(GET copy_section_list     ${rank_idx} COPY_SECTION)

    # List of types to generate, and corresponding names for the procedure suffixes
    set(dtypes      "real(dp)" "real(sp)" "integer" "integer(kind=1)" "logical" "logical(kind=c_bool)" "character(len=*)")
    set(dtype_names "Double"   "Real"     "Int"     "Byte"            "Logical" "Bool"                 "Character")

    set(local_bodies "")
    set(local_procs "")

    foreach(DTYPE IN LISTS dtypes)
        list(FIND dtypes "${DTYPE}" dtype_idx)
        list(GET dtype_names ${dtype_idx} dtype_name)

                # character(len=*) cannot be used for a local allocatable — use len=len(arr) instead
        if(DTYPE STREQUAL "character(len=*)")
            set(DTYPE_TEMP "character(len=len(arr))")
        else()
            set(DTYPE_TEMP "${DTYPE}")
        endif()

        set(PROC_NAME "realloc${attr_prefix}${dtype_name}${rank}")

        render_template("${body_template}" body)

        string(APPEND local_bodies "\n${body}\n")
        list(APPEND local_procs "      module procedure ${PROC_NAME}")
    endforeach()

    # Propagate results to parent scope
    set(all_bodies "${all_bodies}${local_bodies}" PARENT_SCOPE)
    if(attr STREQUAL "pointer")
        set(reallocP_procs "${reallocP_procs};${local_procs}" PARENT_SCOPE)
    else()
        set(realloc_procs  "${realloc_procs};${local_procs}"  PARENT_SCOPE)
    endif()
endfunction()

function(generate_m_alloc)
    set(realloc_procs  "")
    set(reallocP_procs "")
    set(all_bodies     "")

    generate_realloc(1 allocatable)
    generate_realloc(2 allocatable)
    generate_realloc(3 allocatable)
    generate_realloc(4 allocatable)
    generate_realloc(1 pointer)
    generate_realloc(2 pointer)
    generate_realloc(3 pointer)
    generate_realloc(4 pointer)

    list(JOIN realloc_procs  "\n" realloc_interface_procs)
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

# When invoked as a script via -P, generate immediately.
if(CMAKE_SCRIPT_MODE_FILE)
    generate_m_alloc()
endif()