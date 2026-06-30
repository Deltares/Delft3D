!> Minimal IFPORT replacement for compilers that lack Intel's IFPORT module.
!!
!! Intel Fortran ships an IFPORT module that the dflowfm kernel tests use for
!! CHANGEDIRQQ. nvfortran (and other non-Intel compilers) do not provide it,
!! so the `use ifport` statements in the tests fail to resolve. For those
!! compilers we supply our own `ifport` module providing the small subset of
!! IFPORT actually used by the tests (currently only CHANGEDIRQQ).
!!
!! The whole module body is compiled only under nvfortran; on Intel this file
!! is empty so the genuine IFPORT module is used instead.
#ifdef __NVCOMPILER
module ifport
   implicit none
   private
   public :: changedirqq

contains

   !> Portable replacement for Intel IFPORT CHANGEDIRQQ.
   !! Returns .true. on success, .false. otherwise.
   logical function changedirqq(dir) result(success)
      character(len=*), intent(in) :: dir
      integer, external :: chdir ! nvfortran runtime routine, returns 0 on success
      success = (chdir(dir) == 0)
   end function changedirqq
end module ifport
#endif
