subroutine co1cen(f         ,k         ,n         ,m         )
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
    use precision_datsel
    implicit none
!
! Global variables
!
    integer, intent(in)            :: m
    integer, intent(in)            :: n
    integer, dimension(n, m) :: k
    real(hp), dimension(n, m) :: f
!
!
! Local variables
!
    integer                        :: i
    integer                        :: j
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    write (*, *) ' CO1CEN'
    do i = m, 1, -1
       do j = n, 1, -1
          if (k(j, i)==1 .and. i>1 .and. j>1) then
             f(j, i) = .25*(f(j - 1, i - 1) + f(j, i - 1) + f(j - 1, i)         &
                     & + f(j, i))
          else
             f(j, i) = 999.999_hp
             k(j, i) = 0
          endif
       enddo
    enddo
end subroutine co1cen
