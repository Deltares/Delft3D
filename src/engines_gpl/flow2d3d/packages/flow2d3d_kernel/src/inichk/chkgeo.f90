subroutine chkgeo(lundia    ,error     ,kmax      ,thick     ,sig       ,gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
!  
!  
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
    integer       :: k
    logical       :: error1
    logical       :: error2
    real(fp)      :: som
    character(16) :: kstring
!
!
!! executable statements -------------------------------------------------------
!
    !
    ! initialize local parameters
    !
    error1  = .false.
    error2  = .false.
    som     = 0.0
    kstring = ' '    !
    !
    ! redefine thick, check if thick less than or equal to 0.0 and compute the sum
    !
    do k = 1, kmax
       if (thick(k)<=0.0_fp) then
          error1 = .true.
          write(kstring, '(i0)') k
          call prterr(lundia    ,'U004'    , kstring     )
       endif
       thick(k) = thick(k)/100.
       som = som + thick(k)
    enddo
    !
    ! check if sum of thick = 1.0 (100 %)
    !
    if (abs(som - 1.)>1.E-5) then
       error2 = .true.
       call prterr(lundia    ,'U006'    ,' '       )
    endif
    !
    ! set error
    !
    error = error1 .or. error2
    if (error) goto 9999
    !
    ! calculate sig from thick
    !
    sig(1) = -0.5*thick(1)
    do k = 2, kmax
       sig(k) = sig(k - 1) - 0.5*(thick(k) + thick(k - 1))
    enddo
    !
 9999 continue
end subroutine chkgeo
