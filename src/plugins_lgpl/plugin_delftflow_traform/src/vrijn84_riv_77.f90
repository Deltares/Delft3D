subroutine vrijn84_riv_77(dll_integers, max_integers, &
                          dll_reals   , max_reals   , &
                          dll_strings , max_strings , &
                          sbc_total, sbc  , sbcu, sbcv, sbwu, sbwv     , &
                          equi_conc, cesus, ssus, sswu, sswv, t_relax  , &
                          error_message_c   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'VRIJN84_RIV_77' :: VRIJN84_RIV_77
!!--description-----------------------------------------------------------------
!
! Computes sediment transport according to
! Van Rijn (1984)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use iso_c_binding, only: c_char
implicit none
!
! Local constants
! Interface is in high precision
!
integer        , parameter :: hp   = kind(1.0d0)
!
! Subroutine arguments: input
!
integer                                    , intent(in)  :: max_integers
integer                                    , intent(in)  :: max_reals
integer                                    , intent(in)  :: max_strings
integer           , dimension(max_integers), intent(in)  :: dll_integers
real(hp)          , dimension(max_reals)   , intent(in)  :: dll_reals
character(len=256), dimension(max_strings) , intent(in)  :: dll_strings
!
! Subroutine arguments: output
!
real(hp)          , intent(out) :: sbc           ! bed load due to currents, magnitude [m3/m/s]
real(hp)          , intent(out) :: sbcu          ! bed load due to currents, m component [m3/m/s]
real(hp)          , intent(out) :: sbcv          ! bed load due to currents, n component [m3/m/s]
real(hp)          , intent(out) :: sbwu          ! bed load due to waves, m component [m3/m/s]
real(hp)          , intent(out) :: sbwv          ! bed load due to waves, n component [m3/m/s]
real(hp)          , intent(out) :: cesus         ! susp load concentration [m3/m3]
real(hp)          , intent(out) :: ssus          ! susp load due to currents, magnitude [m3/m/s]
real(hp)          , intent(out) :: sswu          ! susp load due to waves, m component [m3/m/s]
real(hp)          , intent(out) :: sswv          ! susp load due to waves, n component [m3/m/s]
real(hp)          , intent(out) :: t_relax       ! relaxation time in 2D mode [s]
character(kind=c_char), intent(out) :: error_message_c(*) ! not empty: echo and stop run
logical           , intent(out) :: equi_conc     ! true: contration cesus returned by formula
                                                 ! false: susp load ssus returned by formula
logical           , intent(out) :: sbc_total     ! true: bed load magnitude returned by formula
                                                 ! false: bed load components returned
!
! Local variables for error message
!
character(len=256) :: error_message
!
call core_function() ! Core function call 
!
call message2c(error_message, error_message_c)

contains
    
!
! Core function definition 
!
subroutine core_function()
!
! Local variables for input parameters
!
integer            :: i
integer            :: l
integer            :: m
integer            :: n, nm
integer, save      :: write_count = 0
real(hp)           :: ag
real(hp)           :: chezy
real(hp)           :: d10, d50, d90, dss, dstar
real(hp)           :: h, hidexp, hrms
real(hp)           :: mudfrac
real(hp)           :: rhosol, rhowat, rlabda
real(hp)           :: sal
real(hp)           :: taub, tem, teta, timsec, tp
real(hp)           :: u, umod, uorb, utot, uuu
real(hp)           :: v, vicmol, vvv
real(hp)           :: ws
real(hp)           :: zumod
character(len=256) :: runid
character(len=256) :: filenm
character(len=256) :: error_message
!
! Local variables: parameters
!
integer, save      :: sbform
integer, save      :: wsform
real(hp), save     :: acal_b
real(hp), save     :: acal_s
real(hp), save     :: alf1 = -1.0_hp
real(hp), save     :: rksc
real(hp), save     :: thetcr0
!
! Local variables, functions
!
real(hp), external :: shld
!
! Local variables
!
integer            :: iocond
integer            :: lun
real(hp)           :: ah
real(hp)           :: archim
real(hp)           :: beta
real(hp)           :: ca
real(hp)           :: cl
real(hp)           :: ct
real(hp)           :: delta
real(hp)           :: fc
real(hp)           :: ff
real(hp)           :: psi
real(hp)           :: rkap
real(hp)           :: rmuc
real(hp)           :: t
real(hp)           :: tbc
real(hp)           :: tbce
real(hp)           :: tbcr
real(hp)           :: thetcr
real(hp)           :: ustar
real(hp)           :: zc
logical            :: opened
!
!! extract array variables -----------------------------------------------------
!
if (max_integers < 4) then
   error_message = 'Insufficient integer values provided by delftflow'
   return
endif
nm      = dll_integers( 1) ! nm index of the grid cell
m       = dll_integers( 2) ! m index of the grid cell
n       = dll_integers( 3) ! n index of the grid cell
l       = dll_integers( 4) ! number of the sediment fraction in the computation
!
if (max_reals < 30) then
   error_message = 'Insufficient real values provided by delftflow'
   return
endif
timsec  = dll_reals( 1)    ! current time since reference time [s]
u       = dll_reals( 2)    ! m component of effective depth-averaged velocity [m/s]
v       = dll_reals( 3)    ! n component of effective depth-averaged velocity [m/s]
utot    = dll_reals( 4)    ! magnitude of effective depth-averaged velocity [m/s]
uuu     = dll_reals( 5)    ! m component of characteristic velocity [m/s]
vvv     = dll_reals( 6)    ! n component of characteristic velocity [m/s]
umod    = dll_reals( 7)    ! magnitude of characteristic velocity [m/s]
zumod   = dll_reals( 8)    ! height above bed of characteristic velocity [m]
h       = dll_reals( 9)    ! water depth [m]
chezy   = dll_reals(10)    ! local Chézy value [m1/2/s]
hrms    = dll_reals(11)    ! wave height [m]
tp      = dll_reals(12)    ! wave period [s]
teta    = dll_reals(13)    ! angle between wave dir and local grid orientation [deg]
rlabda  = dll_reals(14)    ! wave length [m]
uorb    = dll_reals(15)    ! orbital velocity at the bed [m/s]
d50     = dll_reals(16)    ! sediment diameter of fraction [m]
dss     = dll_reals(17)    ! sediment diameter of fraction  when in suspension [m]
dstar   = dll_reals(18)    ! critical dimensionless grain size parameter [-]
d10     = dll_reals(19)    ! 10-percentile diameter of local sediment mixture [m]
d90     = dll_reals(20)    ! 90-percentile diameter of local sediment mixture [m]
mudfrac = dll_reals(21)    ! mud fraction [-]
hidexp  = dll_reals(22)    ! hiding & exposure factor [-]
ws      = dll_reals(23)    ! settling velocity [m/s]
rhosol  = dll_reals(24)    ! solid sediment density [kg/m3]
rhowat  = dll_reals(25)    ! local water density [kg/m3]
sal     = dll_reals(26)    ! local salinity [ppt]
tem     = dll_reals(27)    ! local water temperature [degC]
ag      = dll_reals(28)    ! gravitational acceleration [m/s2]
vicmol  = dll_reals(29)    ! molecular viscosity of water [m2/s]
taub    = dll_reals(30)    ! bed shear stress [N/m2]
!
if (max_strings < 2) then
   error_message = 'Insufficient strings provided by delftflow'
   return
endif
runid   = dll_strings( 1)  ! user-specified run-identification
filenm  = dll_strings( 2)  ! user-specified file name (keyword: InputFile)
!
!! executable statements -------------------------------------------------------
!
if (write_count < 100) then
   write(*,*) 'plugin_delftflow_traform.dll : vrijn84_riv_77 : called'
   write_count = write_count + 1
   if (write_count == 100) then
      write(*,*) 'plugin_delftflow_traform.dll : message suppressed'
   endif
endif
! The output argument error_message MUST have value ' ' to continue the calculation.
!
error_message = ' '
!
! If you want to indicate that this subroutine has encountered some invalid input or
! encountered some unexpected situation, you can set the error_message to a non-empty
! string. This error_message will then be shown in the log file of the calling program
! and the simulation will abort. This is shown by the next line, remove it to enable
! this subroutine.
!
! error_message = 'Carefully read the plugin source code: this message should not appear.'
!
! Set some parameters and compute derivative quantities.
!
delta = (rhosol - rhowat) / rhowat ! relative density of sediment particle
!
! Set the transport rates and concentrations.
! Note: all transport quantities should be given in m3 solid material per s.
!       i.e. rate (or concentration) in kg/s =
!               rhosol * specified rate (or concentration)
!
sbc_total     = .true.    ! set flag to indicate that bed load magnitude is given
sbc     = 0.0_hp          !* bed load magnitude will computed below *!
sbcu    = 0.0_hp          ! bed load component, m direction (here dummy since sbc_total is true)
sbcv    = 0.0_hp          ! bed load component, n direction (here dummy since sbc_total is true)
!
! There should be no suspended load.
!
equi_conc     = .false.   ! set flag to indicate that susp load magnitude is given
cesus   = 0.0_hp          ! suspended load concentration (here dummy since equi_conc is false)
ssus    = 0.0_hp          !* suspended load transport will be computed below *!
!
! This formula does not include wave driven transport.
!
sbwu    = 0.0_hp          ! bed load transport, m direction due to waves is zero
sbwv    = 0.0_hp          ! bed load transport, n direction due to waves is zero
sswu    = 0.0_hp          ! suspended load transport, m direction due to waves is zero
sswv    = 0.0_hp          ! suspended load transport, n direction due to waves is zero
!
! Set the relaxation time for depth-averaged models to a negative value to use the default
! Galappatti formulation.
!
t_relax = -1.0_hp          ! relaxation time is zero
!
if (alf1 < 0.0_hp) then
    open (newunit=lun, file = filenm, form = 'formatted', iostat = iocond, status = 'old')
    if (iocond/=0) then
       error_message = 'Unable to open parameter file: ' // filenm
       return
    endif
    !
    read(lun,*, iostat = iocond) alf1
    if (iocond/=0) then
       error_message = 'Problem reading alf1 from file: ' // filenm
       return
    endif
    !
    read(lun,*, iostat = iocond) rksc
    if (iocond/=0) then
       error_message = 'Problem reading rksc from file: ' // filenm
       return
    endif
    !
    read(lun,*, iostat = iocond) wsform
    if (iocond/=0) then
       error_message = 'Problem reading wsform from file: ' // filenm
       return
    endif
    !
    read(lun,*, iostat = iocond) acal_b
    if (iocond/=0) then
       error_message = 'Problem reading acal_b from file: ' // filenm
       return
    endif
    !
    read(lun,*, iostat = iocond) acal_s
    if (iocond/=0) then
       error_message = 'Problem reading acal_s from file: ' // filenm
       return
    endif
    !
    read(lun,*, iostat = iocond) sbform
    if (iocond/=0) then
       error_message = 'Problem reading sbform from file: ' // filenm
       return
    endif
    !
    read(lun,*, iostat = iocond) thetcr0
    if (iocond/=0) then
       error_message = 'Problem reading thetcr0 from file: ' // filenm
       return
    endif
    !
    close(lun)
endif
    !
    if (h/rksc<1.33_hp .or. utot<1.0E-3_hp) then
       return
    endif
    !
    if (wsform == 1) then
       ! Van Rijn
       if (d50>1.0E-3_hp) then
          ws = 1.1_hp * (delta * ag *d50)**0.5_hp
       elseif (d50<=1.0E-3_hp .and. d50>100E-6_hp) then
          ws = 10.0_hp * vicmol / d50*(sqrt(1.0_hp+(0.01_hp*delta*ag*d50**3/vicmol/vicmol))-1.0_hp)
       else
          ws = delta * ag * d50**2/18.0_hp/vicmol
       endif
    elseif (wsform == 2) then
       ! AHRENS, J. P. (2000). "A fall-velocity equation" Journal of Waterway, Port, Coastal, and Ocean Engineering, ASCE, 126(2), 99-102.
       archim  = dstar**3
       ! coefficient for laminar regime
       Cl = 0.055_hp*tanh((12.0_hp*archim**(-0.59_hp))*(exp(-0.0004_hp*archim)))
       ! coefficient for turbulent regime
       Ct = 1.060_hp*tanh((0.016_hp*archim**0.5_hp)*(exp(-120.0_hp/archim)))
       !
       ws =(vicmol/d50)*(Cl*archim + Ct*sqrt(archim))
    elseif (wsform == 3) then
       ! Ahrnes 2003
       archim  = dstar**3
       ! coefficient for laminar regime
       Cl = 0.055_hp*tanh((12.0_hp*archim**(-0.59_hp))*(exp(-0.0004_hp*archim)))
       ! coefficient for turbulent regime
       Ct = 1.010_hp*tanh((0.016_hp*archim**0.5_hp)*(exp(-115.0_hp/archim)))
       !
       ws =(vicmol/d50)*(Cl*archim + Ct*sqrt(archim))
    else
       ! settling velocity as computed by Delft3D core
    endif    !
    !
    rmuc = (log10(12.0_hp*h/rksc)/log10(12.0_hp*h/3.0_hp/d90))**2
    fc = 0.24_hp*(log10(12.0_hp*h/rksc))**(-2)
    tbc = 0.125_hp*rhowat*fc*utot**2
    tbce = rmuc*tbc
    if (thetcr0 < 0.0_hp) then
       thetcr = shld(dstar)
    else 
       thetcr = thetcr0
    endif
    tbcr = (rhosol - rhowat)*ag*d50*thetcr
    t = (tbce - tbcr)/tbcr
    !
    if (t<0.000001_hp) t = 0.000001_hp
    ca = 0.015*alf1*d50/rksc*t**1.5_hp/dstar**0.3_hp
    rkap = 0.4_hp
    !
    ustar = sqrt(0.125_hp*fc)*utot
    zc = 0.0_hp
    beta = 1.0_hp + 2.0_hp*(ws/ustar)**2
    beta = min(beta, 1.5_hp)
    psi = 2.5_hp*(ws/ustar)**0.8_hp*(ca/0.65_hp)**0.4_hp
    if (ustar>0.0_hp) zc = ws/rkap/ustar/beta + psi
    if (zc>20.0_hp) zc = 20.0_hp
    ah = rksc/h
    fc = 0.0_hp
    if (abs(zc - 1.2_hp)>1.0E-4_hp) then
       fc = (ah**zc - ah**1.2_hp)/(1.0_hp - ah)**zc/(1.2_hp - zc)
    else
       fc = -(ah/(1.0_hp - ah))**1.2_hp*log(ah)
    endif
    ff = fc
    ssus = acal_s*ff*utot*h*ca
    !
    if ((t<3.0_hp) .and. (sbform/=1)) then
       sbc = 0.053_hp*acal_b*(delta)**0.5_hp*sqrt(ag)*d50**1.5_hp*dstar**(-0.3_hp)*t**2.1_hp
    else
       sbc = 0.100_hp*acal_b*(delta)**0.5_hp*sqrt(ag)*d50**1.5_hp*dstar**(-0.3_hp)*t**1.5_hp
    endif

end subroutine core_function
    
end subroutine vrijn84_riv_77
