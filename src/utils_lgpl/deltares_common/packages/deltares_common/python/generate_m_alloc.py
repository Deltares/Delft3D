"""
Generates m_alloc_generated.f90 — all realloc/reallocP overloads for all dtype/attr/rank combinations.
Usage: python generate_m_alloc.py <output_file>
"""

import sys
from dataclasses import dataclass
from pathlib import Path

# --- Type definitions --------------------------------------------------------

@dataclass
class FortranType:
    dtype: str       # e.g. real(dp)
    dtype_temp: str  # local allocatable version (differs only for character)
    name: str        # suffix for procedure name, e.g. Double

    @staticmethod
    def all():
        def t(dtype, name, dtype_temp=None):
            return FortranType(dtype, dtype_temp or dtype, name)
        return [
            t("real(dp)",             "Double"),
            t("real(sp)",             "Real"),
            t("integer",              "Int"),
            t("integer(kind=1)",      "Byte"),
            t("logical",              "Logical"),
            t("logical(kind=c_bool)", "Bool"),
            t("character(len=*)",     "Character", dtype_temp="character(len=len(arr))"),
        ]


@dataclass
class Rank:
    n: int

    @property
    def drank(self):
        # Repeat ":" n times and join with ",":
        # n=1 -> "(:)", n=3 -> "(:,:,:)"
        return "(" + ",".join([":"] * self.n) + ")"

    @property
    def dindex(self):
        return "integer" if self.n == 1 else f"integer, dimension({self.n})"

    @property
    def lindex_one(self):
        # n=1 -> "1", n=3 -> "(1, 1, 1)"
        return "1" if self.n == 1 else "[" + ", ".join(["1"] * self.n) + "]"

    @property
    def shift_zero(self):
        # n=1 -> "0", n=4 -> "(0, 0, 0, 0)"
        return "0" if self.n == 1 else "[" + ", ".join(["0"] * self.n) + "]"

    @property
    def allocate_temp(self):
        # Build per-dimension bounds and join by ", ":
        # n=3 -> "new_l_index(1):uindex(1), new_l_index(2):uindex(2), new_l_index(3):uindex(3)"
        dims = ", ".join(f"new_l_index({i}):uindex({i})" for i in range(1, self.n + 1))
        if self.n == 1:
            dims = "new_l_index:uindex"
        return f"allocate(temp({dims}), stat=stat_)"

    @property
    def bounds_unchanged(self):
        if self.n == 1:
            return "new_l_index == old_l_index .and. uindex == old_u_index .and. shift_ == 0"
        return "all(new_l_index == old_l_index) .and. all(uindex == old_u_index) .and. all(shift_ == 0)"

    @property
    def get_bounds(self):
        if self.n == 1:
            return "old_l_index = lbound(arr, 1)\n         old_u_index = ubound(arr, 1)"
        return "old_l_index = lbound(arr)\n         old_u_index = ubound(arr)"

    @property
    def overlap_nonempty(self):
        return "data_l_index <= data_u_index" if self.n == 1 else "all(data_l_index <= data_u_index)"

    @property
    def copy_section(self):
        if self.n == 1:
            return "temp(data_l_index:data_u_index) = arr(data_l_index - shift_:data_u_index - shift_)"
        # Build "data_l_index(k):data_u_index(k)" for each dimension k and join with ", ".
        # n=2 temp_idx:
        # "data_l_index(1):data_u_index(1), data_l_index(2):data_u_index(2)"
        temp_idx = ", ".join(
            f"data_l_index({i}):data_u_index({i})"
            for i in range(1, self.n + 1)
        )
        # Build shifted source slices and join with ", ".
        # n=2 arr_idx:
        # "data_l_index(1) - shift_(1):data_u_index(1) - shift_(1), data_l_index(2) - shift_(2):data_u_index(2) - shift_(2)"
        arr_idx = ", ".join(
            f"data_l_index({i}) - shift_({i}):data_u_index({i}) - shift_({i})"
            for i in range(1, self.n + 1)
        )
        return f"temp({temp_idx}) = arr({arr_idx})"


@dataclass
class Attr:
    name: str  # "allocatable" or "pointer"

    @property
    def prefix(self):
        return "P" if self.name == "pointer" else ""

    @property
    def is_allocated(self):
        return "associated(arr)" if self.name == "pointer" else "allocated(arr)"

    @property
    def move_alloc(self):
        if self.name == "pointer":
            return ("if (associated(arr)) then\n"
              "         deallocate(arr,stat=stat_)\n"
              "         arr => temp\n"
              "      end if")
        return "call move_alloc(temp, arr)"


# --- Code generation ---------------------------------------------------------

def generate_subroutine(ftype: FortranType, rank: Rank, attr: Attr) -> str:
    proc_name = f"realloc{attr.prefix}{ftype.name}{rank.n}"
    return f"""\
  subroutine {proc_name}(arr, uindex, lindex, stat, fill, shift, keepExisting)
      implicit none
      {ftype.dtype}, {attr.name}, intent(inout) :: arr {rank.drank}
      {rank.dindex}, intent(in)            :: uindex
      {rank.dindex}, intent(in), optional  :: lindex
      integer, intent(out), optional  :: stat
      {ftype.dtype}, intent(in), optional   :: fill
      {rank.dindex}, intent(in), optional  :: shift
      logical, intent(in), optional   :: keepExisting

      {ftype.dtype_temp}, {attr.name} :: temp {rank.drank}
      {rank.dindex} :: old_l_index, old_u_index
      {rank.dindex} :: new_l_index
      {rank.dindex} :: data_l_index, data_u_index
      {rank.dindex} :: shift_
      integer :: stat_
      logical :: keepExisting_
      logical :: allocated_old

      new_l_index = {rank.lindex_one}
      shift_ = {rank.shift_zero}
      keepExisting_ = .true.
      stat_ = 0

      if (present(lindex)) then
         new_l_index = lindex
      end if
      if (present(shift)) then
         shift_ = shift
      end if
      if (present(keepExisting)) then
         keepExisting_ = keepExisting
      end if

      allocated_old = {attr.is_allocated}

      if (allocated_old) then
         {rank.get_bounds}

         if ({rank.bounds_unchanged}) then
            if (.not. keepExisting_ .and. present(fill)) then
               arr = fill
            end if
            goto 999
         end if
      end if

      ! Reallocation required
      {rank.allocate_temp}
      if (stat_ /= 0) then
         goto 999
      end if
      if (present(fill)) then
         temp = fill
      end if
      if (keepExisting_ .and. allocated_old) then
         data_l_index = max(old_l_index + shift_, new_l_index)
         data_u_index = min(old_u_index + shift_, uindex)
         if ({rank.overlap_nonempty}) then
            {rank.copy_section}
         end if
      end if

      {attr.move_alloc}
      999   continue
      if (present(stat)) then 
         stat = stat_
      end if
   end subroutine {proc_name}"""

def generate(output_file: Path) -> None:
    types = FortranType.all()
    ranks = [Rank(n) for n in [1, 2, 3, 4]]
    attrs = [Attr("allocatable"), Attr("pointer")]

    realloc_procs  = []
    reallocP_procs = []
    subroutines    = []

    for attr in attrs:
        for rank in ranks:
            for ftype in types:
                proc_name = f"realloc{attr.prefix}{ftype.name}{rank.n}"
                procs = reallocP_procs if attr.name == "pointer" else realloc_procs
                procs.append(f"      module procedure {proc_name}")
                subroutines.append(generate_subroutine(ftype, rank, attr))

    realloc_interface  = "\n".join(realloc_procs)
    reallocP_interface = "\n".join(reallocP_procs)
    all_bodies         = "\n\n".join(subroutines)

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

{all_bodies}

end module m_alloc_generated
"""
    output_file.parent.mkdir(parents=True, exist_ok=True)
    output_file.write_text(content, encoding="utf-8")
    print(f"Generated {output_file}")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <output_file>")
        sys.exit(1)

    generate(Path(sys.argv[1]))