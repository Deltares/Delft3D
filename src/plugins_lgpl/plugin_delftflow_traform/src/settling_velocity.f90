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
subroutine settle(dll_integers, max_integers, &
                  dll_reals   , max_reals   , &
                  dll_strings , max_strings , &
                  ws          , &
                  error_message_c   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'SETTLE' :: SETTLE
!!--description-----------------------------------------------------------------
!
! Computes simple salinity dependent settling velocity.
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
real(hp)          , intent(out) :: ws                     ! settling velocity [m/s]
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
integer            :: k
integer            :: l
integer            :: m
integer            :: n, nm
integer, save      :: write_count = 0
real(hp)           :: ag
real(hp)           :: cconc, chezy, conc, csoil
real(hp)           :: d50, dss
real(hp)           :: epstur
real(hp)           :: h
real(hp)           :: ktur
real(hp)           :: rhosol, rhowat
real(hp)           :: sal
real(hp)           :: tem, timsec
real(hp)           :: u, um
real(hp)           :: v, vicmol, vm
real(hp)           :: w
character(len=256) :: runid
character(len=256) :: filenm
!
! Local variables
!
real(hp)          :: f
real(hp)          :: pi
real(hp)          :: salmax
real(hp)          :: ws0, ws1
!
!! extract array variables -----------------------------------------------------
!
if (max_integers < 5) then
   error_message = 'Insufficient integer values provided by delftflow'
   return
endif
nm      = dll_integers( 1) ! nm index of the grid cell
m       = dll_integers( 2) ! m index of the grid cell
n       = dll_integers( 3) ! n index of the grid cell
k       = dll_integers( 4) ! layer number of the grid cell
l       = dll_integers( 5) ! number of the sediment fraction in the computation
!
if (max_reals < 21) then
   error_message = 'Insufficient real values provided by delftflow'
   return
endif
timsec  = dll_reals( 1)    ! current time since reference time [s]
u       = dll_reals( 2)    ! m component of the local velocity [m/s]
v       = dll_reals( 3)    ! n component of the local velocity [m/s]
w       = dll_reals( 4)    ! vertical component of the local velocity [m/s]
sal     = dll_reals( 5)    ! local salinity [ppt]
tem     = dll_reals( 6)    ! local water temperature [degC]
rhowat  = dll_reals( 7)    ! local water density [kg/m3]
conc    = dll_reals( 8)    ! sediment concentration [kg/m3] of considered fraction
cconc   = dll_reals( 9)    ! total sediment concentration [kg/m3] over all fractions
ktur    = dll_reals(10)    ! turbulent kinetic energy [m2/s2]
epstur  = dll_reals(11)    ! turbulent dissipation [m2/s3]
!
!---- the following parameters are undefined for mud fractions -----------------
d50     = dll_reals(12)    ! sediment diameter of fraction [m]
dss     = dll_reals(13)    ! sediment diameter of fraction  when in suspension [m]
!---- the parameters above are undefined for mud fractions ---------------------
!
rhosol  = dll_reals(14)    ! solid sediment density [kg/m3]
csoil   = dll_reals(15)    ! reference density for hindered settling calculations [kg/m3]
ag      = dll_reals(16)    ! gravitational acceleration [m/s2]
vicmol  = dll_reals(17)    ! molecular viscosity of water [m2/s]
h       = dll_reals(18)    ! water depth [m]
um      = dll_reals(19)    ! m component of effective depth-averaged velocity [m/s]
vm      = dll_reals(20)    ! n component of effective depth-averaged velocity [m/s]
chezy   = dll_reals(21)    ! local Chézy value [m1/2/s]
!
if (max_strings < 2) then
   error_message = 'Insufficient strings provided by delftflow'
   return
endif
runid   = dll_strings( 1)  ! user-specified run-identification
filenm  = dll_strings( 2)  ! user-specified file name (keyword: SettleInput)
!
!! executable statements -------------------------------------------------------
!
if (write_count < 100) then
   write(*,*) 'plugin_delftflow_traform.dll : settle : called'
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
! error_message = 'Use the settling velocity routine inside Delft3D-FLOW'
!
! Set some parameters and compute derivative quantities.
!
ws1    = 5.0e-4_hp    ! settling velocity at S >= S_max
ws0    = 5.0e-4_hp    ! settling velocity at S = 0
salmax = 30.0_hp
!
pi = 4.0_hp * atan(1.0_hp)
!
! Compute settling velocity
!
if (sal > salmax) then
   f = 1.0_hp
else
   f = 0.5_hp * (1 - cos(pi*sal/salmax))
endif
ws = ws0 + (ws1 - ws0) * f
!
end subroutine core_function

end subroutine settle
