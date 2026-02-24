module test_ec_module
   use assertions_gtest
   use m_missing, only: dmiss
   use m_ec_basic_interpolation, only : interpolate_linear_in_triangle
   implicit none

   !$f90tw TESTCODE(TEST, test_ec_module, test_interpolate_linear_in_triangle, test_interpolate_linear_in_triangle,
   subroutine test_interpolate_linear_in_triangle( X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, JATEK, wf, dmiss, jsferic), bind(C)

   integer :: NDIM=1   !< sample vector dimension
   real(kind=hp), dimension(3) :: X
   real(kind=hp), dimension(3) :: Y
   real(kind=hp), dimension(NDIM,3) :: Z
   real(kind=hp) :: wf(3)
   real(kind=hp) :: zp(NDIM)
   real(kind=hp) :: slo(NDIM)
   integer :: jsferic
   integer :: jatek
   integer :: jslo
   real(kind=hp) :: xp
   real(kind=hp) :: yp
   real(kind=hp) :: zp
   
   X = [809724.001042720, 809699.760677868, 809676.983950382]
   Y = [179638.029659005, 179654.755887270, 179639.225971817]
   Z = [[3.030621639573760D-040, 1.420180850975200D-063, 1.080075836864300D-062]]
   slo = 0.d0
   
   XP = 809699.760677868
   YP = 179654.755887270
   
   jatek = 0
   jsferic = 0
   jslo = 0
   call interpolate_linear_in_triangle( X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, JATEK, wf, dmiss, jsferic)
   
   call f90_expect_ge(zp, 0.0_hp)
   
   end subroutine test_interpolate_linear_in_triangle
   !$f90tw)
   
end module test_ec_module