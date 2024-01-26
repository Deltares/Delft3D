subroutine grmap(f1        ,n1        ,f2        ,n2        ,iref      , &
               & w         ,np        ,iprint    )
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
    integer, intent(in)            :: iprint
    integer, intent(in)            :: n1
    integer, intent(in)            :: n2
    integer, intent(in)            :: np
    integer, dimension(np, n2), intent(in) :: iref
    real(hp), dimension(n1), intent(in) :: f1
    real(hp), dimension(n2) :: f2
    real(hp), dimension(np, n2), intent(in) :: w
!
!
! Local variables
!
    integer                        :: i
    integer                        :: i1
    integer                        :: i2
    integer                        :: ifac
    integer                        :: ip
!
!
!! executable statements -------------------------------------------------------
!
    !
    !     compute interpolated values for all points on grid 2
    !
    if (iprint==1) write (*, *) 'in grmap n1 n2', n1, n2
    do i2 = 1, n2
       !
       ! Function values at grid 2 are expressed as weighted average
       ! of function values in Np surrounding points of grid 1
       !
       if (iprint==1 .and. i2<=n2) &
        & write (*, '(1X,A,I6,4(1X,E11.4))') ' i2 w ', i2, (w(ip, i2) , ip = 1,  &
        & np)
       !
       f2(i2) = 0.0
       !
       do ip = 1, np
          i = iref(ip, i2)
          i1 = max(i, 1)
          if (iprint==1 .and. i2<=n2) write (*, *) ' i1,f1(i1) ', i1, f1(i1)
          f2(i2) = f2(i2) + w(ip, i2)*f1(i1)
       enddo
    enddo
end subroutine grmap
