subroutine checkQbnd(nob,nrob,nto,error,lundia,gdp)

!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
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
!  $Id: checkQbnd.f90 6133 2016-05-13 14:53:11Z platzek $
!  $HeadURL: 
!!--description-----------------------------------------------------------------
!
!    Function: if cutcell, check that the discharge boundary does not have only 1 cell 
!               (if thin cut cell discharge per unit width and velocity very high)
!
! Method used:
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
    integer, pointer :: cutcell
!
! Global variables
!

    integer , dimension(8, nrob)                                       , intent(in)  :: nob 
    integer                                                            , intent(in)  :: nto
    integer                                                            , intent(in)  :: nrob
    integer                                                            , intent(in)  :: lundia
    logical                                                            , intent(out) :: error
!
! local variables
!
    integer :: N
    integer :: n1
    integer :: NcellBND(1:nto)
!
!! executable statements -------------------------------------------------------
!
    cutcell => gdp%gdimbound%cutcell
    !
    NcellBND(1:nto) = 0
    do n = 1, nrob
       !
       ! Only for total discharge boundaries (nob=7)
       !
       if (nob(3,n) == 7) then
          n1           = nob(8,n)
          NcellBND(n1) = NcellBND(n1) + 1       
       endif
    enddo
    if (any(NcellBND(:) == 1)) then
       !
       ! This needs to be fixed, it stops here for the straight channel at a atan(43/80) angle
       !
       write(lundia,*) 'ERROR: If cutcell==2 then single cell discharge boundaries are not allowed'
       write(*,*)      'ERROR: If cutcell==2 then single cell discharge boundaries are not allowed'
       !error = .true.
    endif
    return
end subroutine checkQbnd
