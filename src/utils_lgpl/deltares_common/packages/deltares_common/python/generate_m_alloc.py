"""
Generates m_alloc_generated.f90 — all realloc/reallocP overloads for all dtype/attr/rank combinations.
Usage: python generate_m_alloc.py <template_file> <output_file>
"""

import sys
from pathlib import Path
from itertools import product

RANKS = [1, 2, 3, 4]

DTYPES = [
    ("real(dp)",            "Double"),
    ("real(sp)",            "Real"),
    ("integer",             "Int"),
    ("integer(kind=1)",     "Byte"),
    ("logical",             "Logical"),
    ("logical(kind=c_bool)","Bool"),
    ("character(len=*)",    "Character"),
]

DRANK         = ["(:)", "(:,:)", "(:,:,:)", "(:,:,:,:)"]
DINDEX        = ["integer", "integer, dimension(2)", "integer, dimension(3)", "integer, dimension(4)"]
LINDEX_ONE    = ["1", "(/1, 1/)", "(/1, 1, 1/)", "(/1, 1, 1, 1/)"]
SHIFT_ZERO    = ["0", "(/0, 0/)", "(/0, 0, 0/)", "(/0, 0, 0, 0/)"]

ALLOCATE_TEMP = [
    "allocate(temp(new_l_index:uindex), stat=stat_)",
    "allocate(temp(new_l_index(1):uindex(1), new_l_index(2):uindex(2)), stat=stat_)",
    "allocate(temp(new_l_index(1):uindex(1), new_l_index(2):uindex(2), new_l_index(3):uindex(3)), stat=stat_)",
    "allocate(temp(new_l_index(1):uindex(1), new_l_index(2):uindex(2), new_l_index(3):uindex(3), new_l_index(4):uindex(4)), stat=stat_)",
]

BOUNDS_UNCHANGED = [
    "new_l_index == old_l_index .and. uindex == old_u_index .and. shift_ == 0",
    "all(new_l_index == old_l_index) .and. all(uindex == old_u_index) .and. all(shift_ == 0)",
    "all(new_l_index == old_l_index) .and. all(uindex == old_u_index) .and. all(shift_ == 0)",
    "all(new_l_index == old_l_index) .and. all(uindex == old_u_index) .and. all(shift_ == 0)",
]

GET_BOUNDS = [
    "old_l_index = lbound(arr, 1)\n         old_u_index = ubound(arr, 1)",
    "old_l_index = lbound(arr)\n         old_u_index = ubound(arr)",
    "old_l_index = lbound(arr)\n         old_u_index = ubound(arr)",
    "old_l_index = lbound(arr)\n         old_u_index = ubound(arr)",
]

OVERLAP_NONEMPTY = [
    "data_l_index <= data_u_index",
    "all(data_l_index <= data_u_index)",
    "all(data_l_index <= data_u_index)",
    "all(data_l_index <= data_u_index)",
]

def _copy_section(n):
    def seg(i):
        return f"data_l_index({i}):data_u_index({i})"
    def arr(i):
        return f"data_l_index({i}) - shift_({i}):data_u_index({i}) - shift_({i})"
    if n == 1:
        return "temp(data_l_index:data_u_index) = arr(data_l_index - shift_:data_u_index - shift_)"
    temp_idx = ", ".join(seg(i) for i in range(1, n + 1))
    arr_idx  = ", ".join(arr(i) for i in range(1, n + 1))
    return f"temp({temp_idx}) = arr({arr_idx})"

COPY_SECTION = [_copy_section(n) for n in RANKS]


def render_template(template: str, subs: dict) -> str:
    body = template
    for key, value in subs.items():
        body = body.replace(f"@{key}@", value)
    return body


def generate_realloc(template: str, rank: int, attr: str) -> tuple[str, list[str]]:
    idx = rank - 1

    if attr == "pointer":
        attr_prefix  = "P"
        DATTR        = "pointer"
        IS_ALLOCATED = "associated(arr)"
        MOVE_ALLOC   = "if (associated(arr)) deallocate(arr,stat=stat_)\n      arr => temp"
    else:
        attr_prefix  = ""
        DATTR        = "allocatable"
        IS_ALLOCATED = "allocated(arr)"
        MOVE_ALLOC   = "call move_alloc(temp, arr)"

    bodies = []
    procs  = []

    for dtype, dtype_name in DTYPES:
        dtype_temp = "character(len=len(arr))" if dtype == "character(len=*)" else dtype
        proc_name  = f"realloc{attr_prefix}{dtype_name}{rank}"

        subs = {
            "PROC_NAME":        proc_name,
            "DTYPE":            dtype,
            "DTYPE_TEMP":       dtype_temp,
            "DATTR":            DATTR,
            "DRANK":            DRANK[idx],
            "DINDEX":           DINDEX[idx],
            "LINDEX_ONE":       LINDEX_ONE[idx],
            "SHIFT_ZERO":       SHIFT_ZERO[idx],
            "IS_ALLOCATED":     IS_ALLOCATED,
            "GET_BOUNDS":       GET_BOUNDS[idx],
            "BOUNDS_UNCHANGED": BOUNDS_UNCHANGED[idx],
            "ALLOCATE_TEMP":    ALLOCATE_TEMP[idx],
            "OVERLAP_NONEMPTY": OVERLAP_NONEMPTY[idx],
            "COPY_SECTION":     COPY_SECTION[idx],
            "MOVE_ALLOC":       MOVE_ALLOC,
        }

        bodies.append(render_template(template, subs))
        procs.append(f"      module procedure {proc_name}")

    return "\n".join(bodies), procs


def generate(template_file: Path, output_file: Path) -> None:
    template = template_file.read_text(encoding="utf-8")

    realloc_procs  = []
    reallocP_procs = []
    all_bodies     = []

    for rank, attr in product(RANKS, ["allocatable", "pointer"]):
        bodies, procs = generate_realloc(template, rank, attr)
        all_bodies.append(bodies)
        if attr == "pointer":
            reallocP_procs.extend(procs)
        else:
            realloc_procs.extend(procs)

    realloc_interface  = "\n".join(realloc_procs)
    reallocP_interface = "\n".join(reallocP_procs)
    all_bodies_str     = "\n".join(all_bodies)

    content = f"""\
! This file is generated by generate_m_alloc.py — do not edit manually.
module m_alloc_generated
   use stdlib_kinds, only: c_bool
   use precision, only: dp, sp
   implicit none

   interface realloc
{realloc_interface}
   end interface

   interface reallocP
{reallocP_interface}
   end interface

contains
{all_bodies_str}
end module m_alloc_generated
"""

    output_file.write_text(content, encoding="utf-8")
    print(f"Generated {output_file}")


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <template_file> <output_file>")
        sys.exit(1)

    generate(Path(sys.argv[1]), Path(sys.argv[2]))