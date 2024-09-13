function ulim(du1,du2, gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
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
!  $Id: ulim.f90 6033 2016-04-19 08:23:40Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute/ulim.f90 $
!!--description-----------------------------------------------------------------
!
! This routine computes a limiter value based on the velocity gradients du1 & 
! du2 (Ref.: J.Th. van Kester en R.E. Uittenbogaard "Nader onderzoek 
!      overlaatroutines WAQUA, Deltares Report Z3402 - June 2003, 
!      p. 8-5). 
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Function return value
!
    real(fp) :: ulim
    logical, pointer :: SECordVEL
!
! Global variables
!
    real(fp) :: du1
    real(fp) :: du2
!
!! executable statements -------------------------------------------------------
!
    SECordVEL => gdp%gdimbound%SECordVEL
    !
    ! default value of ULIM = 0.0
    !
    ulim = 0.0
    IF (SECordVEL) THEN
       if (du1*du2 > 0.0) then
          ulim = 0.5*min(1.0_fp,du2/du1)
       endif
    ENDIF
end function ulim
