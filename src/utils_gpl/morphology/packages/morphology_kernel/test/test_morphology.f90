module test_morphology
   use assertions_gtest
   use precision, only: dp, fp
   use m_missing, only: dmiss
   use m_trab19, only: trab19
   use m_rdtrafrm, only: traparams
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_morphology, test_Van_Thiel_Van_Rijn, test_trab19,
   subroutine test_trab19() bind(C)
      integer, parameter :: npar = 25 
      integer, parameter :: npardef = 15
      logical :: ubot_from_com = .false.
      real(dp) :: chezy = 65.0_dp ! Chezy coefficient [m^1/2/s]
      real(dp) :: d15 = 0.0001_dp ! D15 of the sediment [m]
      real(dp) :: di50 = 0.0002_dp ! D50 of the sediment [m]
      real(dp) :: d90 = 0.0003_dp ! D90 of the sediment [m]
      real(dp) :: dzbdt = 0.1_dp !  erosion/sedimentation velocity [m/s]
      real(dp) :: dzdx = 0.01_dp ! slope in x direction [-]
      real(dp) :: dzdy = 0.0_dp ! slope in y direction [-]
      real(dp) :: h = 1.0_dp ! water depth [m]
      real(dp) :: hrms = 1.1_dp ! Root mean square wave height  [m]
      real(dp) :: kwtur = 0.1_dp ! Breaker induced turbulence [m^2/s^2]
      real(dp) :: poros = 0.4_dp ! Porosity of the sediment [-]
      real(dp) :: rlabda = 0.5_dp ! Parameter from Ruessink et al. 2009 JGR 
      real(dp) :: teta = 0.1_dp ! angle between wave direction and x-axis [degrees]
      real(dp) :: tp = 5_dp ! Wave period   [s]
      real(dp) :: ubot = 0.1_dp ! velocity at the bed [m/s]
      real(dp) :: u = 1.0_dp ! velocity in x direction [m/s] 
      real(dp) :: v = 0.0_dp ! velocity in y direction [m/s]
      real(dp) :: vicmol = 1e-6 ! kinematic viscosity of water [m^2/s]
      ! output variables
      real(dp) :: sbcu
      real(dp) :: sbcv
      real(dp) :: cesus
      real(dp) :: ua
      real(dp) :: va
      integer :: j

      integer :: iform = 19 
      character(100) :: name
      integer :: nparreq
      integer :: nparopt
      real(fp), dimension(:), allocatable :: pardef
      character(25), dimension(:), allocatable :: parkeyw
      character(25) , dimension(:,:) , pointer :: parname

      real(dp), dimension(npar) :: par

      real(dp) :: ag = 9.81_dp ! gravity acceleration [m/s^2]
      real(dp) :: delta = 1.65_dp ! relative density of sediment [-]

      allocate(pardef(npardef))
      allocate(parkeyw(npardef))
      
      call traparams(iform, name, nparreq, nparopt, parkeyw, pardef) !, noutpar, outpar_name, outpar_longname)
      ag = 9.81_dp ! gravity acceleration
      delta = 1.65_dp ! relative density of sediment 

      par(1) = ag
      par(4) = delta
      do j = 1, npardef
         par(j+10) = pardef(j)
      end do

      call trab19(u         ,v         ,hrms      ,rlabda    ,teta      ,h        ,tp        , &
                & di50      ,d15       ,d90       ,npar      ,par       ,dzbdt     ,vicmol    , &
                & poros     ,chezy     ,dzdx      ,dzdy      ,sbcu      ,sbcv      ,cesus      , &
                & ua        ,va        ,ubot      ,kwtur     ,ubot_from_com )

      call f90_assert_near(sbcu, 6.8192570641767482e-05_dp, 1.0E-8_dp, "sbcu is not near the expected value"//c_null_char) 
      call f90_assert_near(sbcv, 3.1473055602818149e-08_dp, 1.0E-11_dp, "sbcv is not near the expected value"//c_null_char) 
      call f90_assert_near(cesus, 0.0012548999533282801_dp, 1.0E-7_dp, "cesus is not near the expected value"//c_null_char)
      call f90_assert_near(ua, 0.35950490143587227_dp, 1.0E-4_dp, "ua is not near the expected value"//c_null_char)
      call f90_assert_near(va, 0.00062745505782396465_dp, 1.0E-8_dp, "va is not near the expected value"//c_null_char)

   end subroutine test_trab19
   !$f90tw)

end module test_morphology
