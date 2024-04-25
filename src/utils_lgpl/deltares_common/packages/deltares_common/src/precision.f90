module precision
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
! This module contains the parameters used to switch easily from
! single precision mode to double precision mode.
!
!
! See also precision.h file for C-code (DD)
! See also tri-dyn.igd file for connection with esm
!
! sp: single precision
! hp: high (or double) precision
! fp: flexible precision, single or double
!     fp is the used precision
!
! SWITCHING FROM SINGLE PRECISION   FP   TO DOUBLE PRECISION:
! 1) File libsrc\flow_modsrc\precision.f90
!    - Comment out the following line:
!      INTEGER, PARAMETER :: FP=SP
!    - Activate the following line:
!      INTEGER, PARAMETER :: FP=HP
! 2) File include\flow\tri-dyn.igd
!    - Comment out the following line:
!      equivalence ( r(0),  rbuf(0))
!    - Activate the following line:
!      equivalence ( r(0),  dbuf(0))
! 3) File include\hydra\precision.h
!    - Comment out the following line:
!      #undef FLOW_DOUBLE_PRECISION
!    - Activate the following line:
!
! SWITCHING FROM SINGLE PRECISION BODSED/DPS TO DOUBLE PRECISION:
! 1) File libsrc\flow_modsrc\precision.f90
!    - Comment out the following line:
!      integer, parameter :: prec=sp
!    - Activate the following line:
!      integer, parameter :: prec=hp
! 2) File include\flow\tri-dyn.igd
!    - Comment out the following line:
!      equivalence ( d(0),  rbuf(0))
!    - Activate the following line:
!      equivalence ( d(0),  dbuf(0))
! 3) File libsrc\flow_dd\hyexth\precision.h
!    - Comment out the following line:
!      #undef PREC_DOUBLE_PRECISION
!    - Activate the following line:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use precision_basics
use iso_c_binding
implicit none
!
! fp is the generally used precision in Delft3D-FLOW
!
integer, parameter :: fp=hp
!integer, parameter :: fp=sp
!
! prec is used to switch bodsed/dps from sp to hp
!
integer, parameter :: prec=hp
!integer, parameter :: prec=sp
!
! old hp's in sobek that should stay sp
!
integer, parameter :: fhp=sp 
!
! length of integers which are esm/fsm pointers
! = 4 for 32bit versions
! = 8 for 64bit versions
!
integer, parameter :: pntrsize=c_size_t

end module precision
