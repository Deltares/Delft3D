subroutine sort(n         ,ra        ,wksp      ,iwksp     )
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision_kubint
    implicit none
!
! Global variables
!
    integer         :: n
    integer, dimension(n) :: iwksp
    real(hp), dimension(n) :: ra
    real(hp), dimension(n), intent(out) :: wksp
!
!
! Local variables
!
    integer                        :: j
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    call indexx(n         ,ra        ,iwksp     )
    do j = 1, n
       wksp(j) = ra(iwksp(j))
    enddo
end subroutine sort
