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
!  
subroutine parkro(dll_integers, max_integers, &
                  dll_reals   , max_reals   , &
                  dll_strings , max_strings , &
                  sink        , source      , &
                  error_message_c   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'PARKRO' :: PARKRO
!!--description-----------------------------------------------------------------
!
! Computes source and sink terms according to
! Partheniades and Krone (1965)
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
real(hp)          , intent(out) :: source                 ! source term [kg/m2/s]
real(hp)          , intent(out) :: sink                   ! sink term [-] (to be multiplied with concentration and settling velocity)
character(kind=c_char), intent(out) :: error_message_c(*) ! not empty: echo and stop run
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
! Local variables
!
real(hp)           :: eropar
real(hp)           :: taum, tcrero, tcrdep
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
!
!---- the following parameters are undefined for mud fractions -----------------
d50     = dll_reals(16)    ! sediment diameter of fraction [m]
dss     = dll_reals(17)    ! sediment diameter of fraction  when in suspension [m]
dstar   = dll_reals(18)    ! critical dimensionless grain size parameter [-]
d10     = dll_reals(19)    ! 10-percentile diameter of local sediment mixture [m]
d90     = dll_reals(20)    ! 90-percentile diameter of local sediment mixture [m]
mudfrac = dll_reals(21)    ! mud fraction [-]
hidexp  = dll_reals(22)    ! hiding & exposure factor [-]
!---- the parameters above are undefined for mud fractions ---------------------
!
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
   write(*,*) 'plugin_delftflow_traform.dll : parkro : called'
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
! error_message = 'Use the Partheniades Krone formula inside Delft3D-FLOW'
!
! Set some parameters and compute derivative quantities.
!
tcrero = 0.1_hp
tcrdep = 0.1_hp
eropar = 2.5e-6_hp
!
! Compute source and sink terms
!
taum   = max(0.0_hp, taub/tcrero - 1.0_hp)
source = eropar * taum
if (tcrdep > 0.0_hp) then
   sink   = max(0.0_hp , 1.0_hp-taub/tcrdep)
else
   sink   = 0.0_hp
endif
!
end subroutine core_function 

end subroutine parkro
