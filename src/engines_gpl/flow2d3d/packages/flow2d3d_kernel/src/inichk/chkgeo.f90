subroutine chkgeo(lundia    ,error     ,kmax      ,thick     ,sig       ,gdp)
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
!  $Id: chkgeo.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/inichk/chkgeo.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Checks the geometry parameters of the model.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                   , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                :: lundia !  Description and declaration in inout.igs
    logical                                :: error
    real(fp), dimension(kmax)              :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)              :: thick  !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer    :: k
    logical    :: error1
    logical    :: error2
    logical    :: error3
    real(fp)   :: som
!
!
!! executable statements -------------------------------------------------------
!
    !
    !-----initialize local parameters
    !
    error1 = .false.
    error2 = .false.
    error3 = .false.
    som = 0.0
    !
    !
    !-----redefine thick, check if thick .lt. 0.01
    !
    do k = 1, kmax
       if (thick(k)<0.01) then
          error2 = .true.
       endif
       thick(k) = thick(k)/100.
       som = som + thick(k)
    enddo
    if (error2) then
       call prterr(lundia    ,'U004'    ,' 0.01 - 100.00'     )
    !
    endif
    !
    !-----check som of thick = 1. (100 %)
    !
    if (abs(som - 1.)>1.E-5) then
       error3 = .true.
       call prterr(lundia    ,'U006'    ,' '       )
    !
    endif
    !
    !-----set error
    !
    error = error1 .or. error2 .or. error3
    if (error) goto 9999
    !
    !-----calculate sig   from thick
    !
    sig(1) = -0.5*thick(1)
    do k = 2, kmax
       sig(k) = sig(k - 1) - 0.5*(thick(k) + thick(k - 1))
    enddo
    !
 9999 continue
end subroutine chkgeo
