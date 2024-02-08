module precision_basics
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
!!--description-----------------------------------------------------------------
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use, intrinsic :: ieee_arithmetic, only : ieee_is_nan, ieee_is_finite

implicit none
!
! parameters, used in conversions: sp=single precision, hp=high (double) precision
!
integer, parameter :: sp=kind(1.0e00)
integer, parameter :: hp=kind(1.0d00)
!
! double precision integers:
!
integer, parameter :: long = selected_int_kind(16)
!
! interfaces
!
  interface comparereal
     module procedure comparerealdouble
     module procedure comparerealsingle
     module procedure comparerealdouble_finite_check
     module procedure comparerealsingle_finite_check
  end interface
!
  private :: ieee_is_nan, ieee_is_finite

contains

function comparerealdouble(val1, val2, eps)
!!--description-----------------------------------------------------------------
!
! Compares two double precision numbers
! Allow users to define the value of eps. If not, eps equals to the default machine eps
!
! Return value: -1 if val1 < val2
!                0 if val1 = val2
!               +1 if val1 > val2
!
!!--pseudo code and references--------------------------------------------------
!
! The functionality in this subroutine is copied from subroutine Ifdbl,
! written by Jaap Zeekant.
!
! eps must be machine precision dependent.
! eps may not be given by the user! See what happens when
! val1 = -666.0, val2 = -999.0, eps = 0.5
!
!!--declarations----------------------------------------------------------------
    implicit none
!
! Return value
!
integer :: comparerealdouble
!
! Global variables
!
real(kind=hp), intent(in)           :: val1
real(kind=hp), intent(in)           :: val2
real(kind=hp), optional, intent(in) :: eps
!
! Local variables
!
real(kind=hp) :: eps0
real(kind=hp) :: value
!
!! executable statements -------------------------------------------------------
!
if (present(eps)) then
    eps0 = eps
else 
    eps0 = 2.0_hp * epsilon(val1)
endif
!
if (abs(val1)<1.0_hp .or. abs(val2)<1.0_hp) then
   value = val1 - val2
else
   value = val1/val2 - 1.0_hp
endif
!
if (abs(value)<eps0) then
   comparerealdouble = 0
elseif (val1<val2) then
   comparerealdouble = -1
else
   comparerealdouble = 1
endif

end function comparerealdouble

function comparerealsingle(val1, val2,eps)
!!--description-----------------------------------------------------------------
!
! Compares two real numbers of type sp
! Allow users to define the value of eps. If not, eps equals to the default machine eps
!
! Return value: -1 if val1 < val2
!                0 if val1 = val2
!               +1 if val1 > val2
!
!!--pseudo code and references--------------------------------------------------
!
! The functionality in this subroutine is copied from subroutine Ifflt,
! written by Jaap Zeekant.
!
! eps must be machine precision dependent.
! eps may not be given by the user! See what happens when
! val1 = -666.0, val2 = -999.0, eps = 0.5
!
!!--declarations----------------------------------------------------------------
implicit none
!
! Return value
!
integer :: comparerealsingle
!
! Global variables
!
real(kind=sp), intent(in)           :: val1
real(kind=sp), intent(in)           :: val2
real(kind=sp), optional, intent(in) :: eps
!
! Local variables
!
real(kind=sp) :: eps0
real(kind=sp) :: value
!
!! executable statements -------------------------------------------------------
!
!  
if (present(eps)) then
    eps0 = eps
else
    eps0 = 2.0_sp * epsilon(val1)
endif
!
if (abs(val1)<1.0_sp .or. abs(val2)<1.0_sp) then
   value = val1 - val2
else
   value = val1/val2 - 1.0_sp
endif
!
if (abs(value)<eps0) then
   comparerealsingle = 0
elseif (val1<val2) then
   comparerealsingle = -1
else
   comparerealsingle = 1
endif

end function comparerealsingle

function comparerealdouble_finite_check(val1, val2, check_finite, eps) result(compare)
!!--description-----------------------------------------------------------------
!
! Compares two double precision numbers
! Allow users to define the value of eps. If not, eps equals to the default machine eps
!
! Return value: -2 if val1 = NaN and val2 not NaN
!               -1 if val1 < val2
!                0 if val1 = val2 (also if both val1 and val2 are NaN, or both Inf with the same sign)
!               +1 if val1 > val2
!               +2 if val1 is not NaN and val2 is NaN
!
!!--declarations----------------------------------------------------------------
    implicit none
!
! Return value
!
integer :: compare
!
! Global variables
!
real(kind=hp), intent(in)           :: val1
real(kind=hp), intent(in)           :: val2
logical,       intent(in)           :: check_finite
real(kind=hp), optional, intent(in) :: eps
!
! Local variables
!
real(kind=hp) :: value
!
!! executable statements -------------------------------------------------------
!
if (.not. check_finite) then
   compare = comparereal(val1, val2, eps)
else if (ieee_is_finite(val1) .and. ieee_is_finite(val2)) then
   compare = comparereal(val1, val2, eps)
else
   if (ieee_is_nan(val1) .and. ieee_is_nan(val2)) then
      compare = 0
   else if (ieee_is_nan(val1)) then
      compare = -2
   else if (ieee_is_nan(val2)) then
      compare = 2
   else if (val1 > val2) then ! now val1 = +/- Inf or val2 = +/- Inf
      compare = 1
   else if (val1 < val2) then
      compare = -1
   else
      compare = 0
   end if
end if

end function comparerealdouble_finite_check



function comparerealsingle_finite_check(val1, val2, check_finite, eps) result(compare)
!!--description-----------------------------------------------------------------
!
! Compares two real numbers of type sp
! Allow users to define the value of eps. If not, eps equals to the default machine eps
!
! Return value: -2 if val1 = NaN and val2 not NaN
!               -1 if val1 < val2
!                0 if val1 = val2 (also if both val1 and val2 are NaN, or both Inf with the same sign)
!               +1 if val1 > val2
!               +2 if val1 is not NaN and val2 is NaN
!
!!--declarations----------------------------------------------------------------
implicit none
!
! Return value
!
integer :: compare
!
! Global variables
!
real(kind=sp), intent(in)           :: val1
real(kind=sp), intent(in)           :: val2
logical,       intent(in)           :: check_finite
real(kind=sp), optional, intent(in) :: eps
!
! Local variables
!
real(kind=sp) :: value
!
!! executable statements -------------------------------------------------------
!
if (.not. check_finite) then
   compare = comparereal(val1, val2, eps)
else if (ieee_is_finite(val1) .and. ieee_is_finite(val2)) then
   compare = comparereal(val1, val2, eps)
else
   if (ieee_is_nan(val1) .and. ieee_is_nan(val2)) then
      compare = 0
   else if (ieee_is_nan(val1)) then
      compare = -2
   else if (ieee_is_nan(val2)) then
      compare = 2
   else if (val1 > val2) then ! now val1 = +/- Inf or val2 = +/- Inf
      compare = 1
   else if (val1 < val2) then
      compare = -1
   else
      compare = 0
   end if
end if

end function comparerealsingle_finite_check


end module precision_basics
