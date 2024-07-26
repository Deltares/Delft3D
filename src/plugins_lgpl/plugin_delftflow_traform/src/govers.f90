!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!
subroutine govers(dll_integers, max_integers, &
                  dll_reals   , max_reals   , &
                  dll_strings , max_strings , &
                  sbc_total, sbc  , sbcu, sbcv, sbwu, sbwv     , &
                  equi_conc, cesus, ssus, sswu, sswv, t_relax  , &
                  error_message_c   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'GOVERS' :: GOVERS
!!--description-----------------------------------------------------------------
!
! Computes sediment transport according to
! Govers (1990) and Meyer-Peter & Müller (1948)
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
real(hp)              , intent(out) :: sbc                ! bed load due to currents, magnitude [m3/m/s]
real(hp)              , intent(out) :: sbcu               ! bed load due to currents, m component [m3/m/s]
real(hp)              , intent(out) :: sbcv               ! bed load due to currents, n component [m3/m/s]
real(hp)              , intent(out) :: sbwu               ! bed load due to waves, m component [m3/m/s]
real(hp)              , intent(out) :: sbwv               ! bed load due to waves, n component [m3/m/s]
real(hp)              , intent(out) :: cesus              ! susp load concentration [m3/m3]
real(hp)              , intent(out) :: ssus               ! susp load due to currents, magnitude [m3/m/s]
real(hp)              , intent(out) :: sswu               ! susp load due to waves, m component [m3/m/s]
real(hp)              , intent(out) :: sswv               ! susp load due to waves, n component [m3/m/s]
real(hp)              , intent(out) :: t_relax            ! relaxation time in 2D mode [s]
character(kind=c_char), intent(out) :: error_message_c(*) ! not empty: echo and stop run
logical               , intent(out) :: equi_conc          ! true: contration cesus returned by formula
                                                          ! false: susp load ssus returned by formula
logical               , intent(out) :: sbc_total          ! true: bed load magnitude returned by formula
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
!
! Local variables
!
real(hp)           :: acal
real(hp)           :: cf
real(hp)           :: delta
real(hp)           :: ustar
real(hp)           :: amurs
real(hp)           :: cgrain
real(hp)           :: y
real(hp)           :: d505
real(hp)           :: gov_a
real(hp)           :: gov_b
real(hp)           :: p
real(hp)           :: p_cr


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
   write(*,*) 'plugin_delftflow_traform.dll : govers : called'
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
! error_message = 'Use the Govers formula inside Delft3D-FLOW'
!
! Set some parameters and compute derivative quantities.
!
delta = (rhosol - rhowat) / rhowat ! relative density of sediment particle
acal  = 1.0_hp
cf    = ag/chezy/chezy
ustar = sqrt(cf)*utot
!
! Set the transport rates and concentrations.
! Note: all transport quantities should be given in m3 solid material per s.
!       i.e. rate (or concentration) in kg/s =
!               rhosol * specified rate (or concentration)
!
! Bed load transport: MPM formula
!
sbc_total     = .true.    ! set flag to indicate that bed load magnitude is given
if (chezy < 1.0e-6) then
    return
endif
if ((12.0*h/d90) < 1.0e-6) then
    amurs = 1.0_hp
else
    cgrain = 18.0 * log10(12.0*h/d90)
    amurs  = (cgrain/chezy)**1.5
    amurs  = max(amurs, 1.0_hp)
endif
y = (utot/chezy)**2 / amurs / delta / d50
y = max(y - hidexp * 0.047, 0.0_hp)

!Do not return to compute suspended load
!if (y < 1.0e-6) then
!    return
!endif
sbc = acal*8.0*sqrt(ag*delta*d50*y)*d50*y ! bed load magnitude is non-zero
sbcu    = 0.0_hp          ! bed load component, m direction (here dummy since sbc_total is true)
sbcv    = 0.0_hp          ! bed load component, n direction (here dummy since sbc_total is true)
!
! Suspended load transport: Govers equation
!
equi_conc     = .false.   ! set flag to indicate that susp load magnitude is given
cesus   = 0.0_hp          ! suspended load concentration (here dummy since equi_conc is false)

d505 = d50+5.0_hp 
gov_a = (d505/0.32_hp)**(-0.6_hp)
gov_b = (d505/300.0_hp)**(0.25_hp)
P = utot**3/chezy**2.0_hp/h
P_cr=0.004_hp
P=max(P-P_cr,0.0_hp) !from here on it is effective
ssus = utot*h*gov_a*P**gov_b

!    
! This formula does not include wave driven transport.
!
sbwu    = 0.0_hp          ! bed load transport, m direction due to waves is zero
sbwv    = 0.0_hp          ! bed load transport, n direction due to waves is zero
sswu    = 0.0_hp          ! suspended load transport, m direction due to waves is zero
sswv    = 0.0_hp          ! suspended load transport, n direction due to waves is zero
!
!
t_relax = 0.0_hp          ! relaxation time is zero
!
end subroutine core_function

end subroutine govers
