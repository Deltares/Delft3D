subroutine indexx(n         ,arrin     ,indx      )
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
    integer , intent(in)   :: n
    integer , dimension(n) :: indx
    real(hp), dimension(n), intent(in) :: arrin
!
!
! Local variables
!
    integer  :: i
    integer  :: indxt
    integer  :: ir
    integer  :: j
    integer  :: l
    real(hp) :: q
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    do j = 1, n
       indx(j) = j
    enddo
    l = n/2 + 1
    ir = n
   10 continue
    if (l>1) then
       l = l - 1
       indxt = indx(l)
       q = arrin(indxt)
    else
       indxt = indx(ir)
       q = arrin(indxt)
       indx(ir) = indx(1)
       ir = ir - 1
       if (ir==1) then
          indx(1) = indxt
          return
       endif
    endif
    i = l
    j = l + l
   20 continue
    if (j<=ir) then
       if (j<ir) then
          if (arrin(indx(j))<arrin(indx(j + 1))) j = j + 1
       endif
       if (q<arrin(indx(j))) then
          indx(i) = indx(j)
          i = j
          j = j + j
       else
          j = ir + 1
       endif
       goto 20
    endif
    indx(i) = indxt
    goto 10
end subroutine indexx
