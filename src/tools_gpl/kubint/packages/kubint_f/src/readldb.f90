subroutine readldb(x,y,nopol,nop,fin)
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
!
! Global variables
!
    character(256), intent(in)                   :: fin
    integer, intent(out)                         :: nopol
    integer, dimension(500), intent(out)        :: nop
    real(hp), dimension(1000,500), intent(out) :: x
    real(hp), dimension(1000,500), intent(out) :: y
!
! Local variables
!
    character(256)                               :: cdummy
    integer                                      :: k
    integer                                      :: npoints0
    real(hp)                                     :: x00
    real(hp)                                     :: xlast
    real(hp)                                     :: y00
!
!
!! executable statements -------------------------------------------------------
!
    open(531,file=trim(fin))
    !
    nopol = 0
    !
150 read(531,'(a)',end=750)cdummy
    if (cdummy(1:1)=='*') goto 150
    read(531,*)npoints0
    xlast = 999.999
    do k = 1,npoints0
       read(531,*)x00,y00
       if (x00<999.9989 .or. x00>999.9991) then
          if (xlast>999.9989 .and. xlast<999.9991) then
             nopol =nopol + 1
             nop(nopol) = 0
          endif
          nop(nopol) = nop(nopol) + 1
          x(nop(nopol),nopol) = x00
          y(nop(nopol),nopol) = y00
       endif
       xlast=x00
    enddo
    !
    goto 150
    !
750 close(531)
    !
end subroutine readldb
