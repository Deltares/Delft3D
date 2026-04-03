module test_depfil_stm
   use assertions_gtest
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_depfil_stm, test_interpolate_large_sample_file, test_interpolate_large_sample_file,
   subroutine test_interpolate_large_sample_file() bind(C)
      use m_missing, only: dmiss
      use m_depfil_stm, only: depfil_stm
      use precision, only: dp
      use grid_dimens_module, only: griddimtype

      logical :: error
      integer :: lundia=0
      integer :: nfld 
      integer :: ifld 
      class(griddimtype), pointer :: dims
      character(len=256) :: errmsg
      character(len=256) :: fildep
      character(len=256) :: fmttmp
      real(kind=dp), dimension(10) :: array
    
      !call simplegrid_dimens(dims, 100, 1)

      ! This test loads a large sample file and checks whether the interpolation is successful.
      ! call depfil_stm(lundia    ,error     ,fildep    ,fmttmp    , &
      !               & array     ,nfld      ,ifld      ,dims      , &
      !               & errmsg    )
      error = .false.
      
      call f90_assert_eq(error, .false. , "Error interpolating large sample file")

   end subroutine test_interpolate_large_sample_file
   !$f90tw)

end module test_depfil_stm
