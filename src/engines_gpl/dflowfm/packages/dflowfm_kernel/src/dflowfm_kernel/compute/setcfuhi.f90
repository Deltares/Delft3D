!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

module m_setcfuhi

implicit none

private

public :: setcfuhi

contains

!> set friction coefficients g/C2 etc
!! sqrt(g/C2) in both in 2D and in 3D
 subroutine setcfuhi() 
    use m_flowtimes 
    use m_flow
    use m_flowgeom, only: lnx, lnx1d
    use m_missing
    use m_get_cz

    ! locals
    double precision :: h0, cz, frcn
    integer :: l

    ! NOTE: When frcuni==0, the initial friction fields in frcu also become noneffective:
    if (jatrt == 0 .and. (frcmax == 0d0 .or. ifrctypuni == -999)) then
       cfuhi = 0; return
    end if
    if (jaconveyance2D >= 1) then ! .and. kmx <=1 ) then
       return
    end if

    if (kmx <= 1) then ! 2D
       if (ifrctypuni == 4) then
          cfuhi = 0
       else

          !$OMP PARALLEL DO                             &
          !$OMP PRIVATE(L,h0,frcn,cz)
          do L = lnx1D + 1, lnx
             if (hu(L) > 0) then
                if (jaconveyance2D == 0) then ! original default
                   h0 = max(epshs, 1d0 / huvli(L))
                else if (jaconveyance2D == -1) then ! better for straight test
                   h0 = max(epshs, hu(L)) ! does it whole not
                end if
                frcn = frcu(L)
                if (frcn > 0d0) then
                   call getcz(h0, frcn, ifrcutp(L), cz, L)
                   cfuhi(L) = ag / (h0 * cz * cz)
                   z0ucur(L) = h0 * exp(-1d0 - vonkar * cz / sag)
                else
                   cfuhi(L) = 0d0
                   z0ucur(L) = epsz0
                end if
                z0urou(L) = z0ucur(L) ! 3D analogue in getustbcfhi
             end if
          end do
          !$OMP END PARALLEL DO

       end if

    end if

 end subroutine setcfuhi

end module m_setcfuhi
