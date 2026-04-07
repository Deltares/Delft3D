module test_depfil_stm
   use assertions_gtest
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_depfil_stm, test_interpolate_large_sample_file, test_interpolate_large_sample_file,
   subroutine test_interpolate_large_sample_file() bind(C)
      use m_missing, only: dmiss
      use m_depfil_stm, only: depfil_stm
      use precision, only: dp
      use grid_dimens_module, only: griddimtype, simplegrid_dimens

      integer, parameter :: grid_count = 100
      integer, parameter :: nfld=3 
      logical :: error
      integer :: lundia=1
      integer :: ifld=3
      character(len=256) :: errmsg = ''
      character(len=256) :: fildep = 'lyr01_thk_small.xyz' ! 'lyr01_thk.xyz'
      character(len=256) :: fmttmp = 'formatted'
      real(kind=dp), dimension(grid_count, nfld) :: array
      type(griddimtype) :: grid_dim
      real(kind=dp), dimension(grid_count), target :: x = 0.0_dp
      real(kind=dp), dimension(grid_count), target :: y = 0.0_dp

      integer :: j

      x = [(real(j, kind=dp), j=1, grid_count)]
      y = [(real(j, kind=dp), j=1, grid_count)]

      call simplegrid_dimens(grid_dim, grid_count, 1)
      grid_dim%xz => x
      grid_dim%yz => y
      error = .false.

      ! This test loads a large sample file and checks whether the interpolation is successful.
      call depfil_stm(lundia    ,error     ,fildep    ,fmttmp    , &
                      & array     ,nfld      ,ifld    ,grid_dim   , &
                      & errmsg    )
      
      call f90_assert_eq(error, .false. , "Error interpolating large sample file")

   end subroutine test_interpolate_large_sample_file
   !$f90tw)

end module test_depfil_stm
