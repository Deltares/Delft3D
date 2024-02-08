subroutine qkwbn(qxkw      ,cgc       ,kfu       ,irocol    ,norow     , &
               & icx       ,icy       ,gdp)
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
! Transport velocities at boundary points are set.
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
    integer                                  , intent(in) :: icx
    integer                                  , intent(in) :: icy
    integer                                  , intent(in) :: norow !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, norow)             , intent(in) :: irocol !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfu !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)             :: cgc !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)             :: qxkw !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer :: ddb
    integer :: ic
    integer :: icxy
    integer :: mf
    integer :: ml
    integer :: n
    integer :: nmf
    integer :: nmfu
    integer :: nml
    integer :: nmld
!
!
!! executable statements -------------------------------------------------------
!
    icxy = max(icx, icy)
    ddb  = gdp%d%ddbound
    !
    ! SET BOUNDARY CONDITIONS
    !
    ! LOOP OVER GRID ROWS FOR BOUNDARY CONDITIONS
    !
    do ic = 1, norow
       n = irocol(1, ic)
       mf = irocol(2, ic) - 1
       ml = irocol(3, ic)
       nmf = (n + ddb)*icy + (mf + ddb)*icx - icxy
       nmfu = nmf + icx
       nml = (n + ddb)*icy + (ml + ddb)*icx - icxy
       nmld = nml - icx
       !
       ! SET BOUND. COND. FOR BEGIN OF ROW IN THE CASE OF A CLOSED BOUNDARY
       !
       if (kfu(nmf)==0) then
          !
          ! CLOSED BOUNDARY OR DRY VELOCITY-POINT AT BEGIN OF ROW
          !
          qxkw(nmf) = qxkw(nmfu)
          cgc(nmf) = cgc(nmfu)
       endif
       !
       ! SET BOUND. COND. FOR END OF ROW IN THE CASE OF A CLOSED BOUNDARY
       !
       if (kfu(nml)==0) then
          !
          ! CLOSED BOUNDARY OR PERMANENT DRY VELOCITY POINT AT END OF ROW
          !
          qxkw(nml) = qxkw(nmld)
          cgc(nml) = cgc(nmld)
       endif
    enddo
end subroutine qkwbn
