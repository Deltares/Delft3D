subroutine dir2uv(u         ,v         ,k         ,n         ,m         )
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
    !
    ! COMMON variables
    !
    real(hp)  :: facrad
    common /fr    / facrad
!
! Global variables
!
    integer, intent(in)            :: m
    integer, intent(in)            :: n
    integer, dimension(n, m), intent(in) :: k
    real(hp), dimension(n, m) :: u
    real(hp), dimension(n, m) :: v
!
!
! Local variables
!
    integer                        :: i
    integer                        :: j
    real(hp)                       :: ang
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    write (*, *) ' DIR2UV'
    do i = m, 1, -1
       do j = n, 1, -1
          if (k(j, i)==1 .and. i>1 .and. j>1) then
             ang = facrad*v(j, i)
             v(j, i) = u(j, i)*sin(ang)
             u(j, i) = u(j, i)*cos(ang)
          else
             u(j, i) = 0._hp
             v(j, i) = 0._hp
          endif
       enddo
    enddo
end subroutine dir2uv
