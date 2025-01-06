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

module m_reaarc

implicit none

private

public :: reaarc

contains

      subroutine REAARC(MINP, japrompt)
         use precision, only: dp
         use m_getreal
         use m_arcinfo
         use m_polygon
         use m_missing
         use m_alloc
         use m_qnerror
         use m_readarcinfoheader
         use m_read_arc_info_block
         use m_read_large_arc_info_block

         integer :: ierr
         integer :: minp

         integer, intent(in) :: japrompt !< prompt for step size (1) or not (0)

         integer :: istep, jstep, MCfile, NCfile
         integer :: istart, iend, jstart, jend !< block to be read in file-index numbering

         real(kind=dp) :: distep, djstep, dsqrtnumcur

         call READARCINFOHEADER(MINP, MCa, NCa, X0, Y0, DXa, DYa, RMIS)

         if (allocated(D)) then
            deallocate (D)
         end if

         if (japrompt == 0 .or. mca * nca < MAXARCTILE) then

            allocate (D(MCa, NCa), STAT=IERR)
            call AERR('D(MCa,NCa)', IERR, MCa * NCa)

            call READARCINFOBLOCK(MINP, D, MCa, NCa, RMIS)

         else
            istep = 1
            jstep = 1
            ierr = 1

            MCfile = MCa
            NCfile = NCa

!         do while ( ierr.ne.0 )

            if (NPL <= 0) then
               istart = 1
               iend = MCa
               jstart = 1
               jend = NCa
            else ! use selecting polygon for dimensions of block to be read
               istart = max(1 + int((minval(xpl(1:NPL), xpl(1:NPL) /= DMISS) - X0) / DXa), 1)
               iend = min(1 + int((maxval(xpl(1:NPL), xpl(1:NPL) /= DMISS) - X0) / DXa), MCa)

               jstart = max(1 + int((minval(ypl(1:NPL), ypl(1:NPL) /= DMISS) - Y0) / DYa), 1)
               jend = min(1 + int((maxval(ypl(1:NPL), ypl(1:NPL) /= DMISS) - Y0) / DYa), NCa)
            end if

            if (japrompt == 1) then
!           automatic istep, jstep
               dsqrtnumcur = sqrt(dble(iend - istart + 1)) * sqrt(dble(jend - jstart + 1))
               distep = dsqrtnumcur / sqrt(dble(MAXARCTILE))
               distep = dble(int(distep + 0.5d0))
               djstep = distep

               if (distep > 1d0) then ! only if necessary
                  call getreal("istep = ", distep)
                  call getreal("jstep = ", djstep)
               end if

               istep = max(int(distep), 1)
               jstep = max(int(djstep), 1)
            end if

            MCa = (iend - istart + 1) / istep
            NCa = (jend - jstart + 1) / jstep

            allocate (D(MCa, NCa), STAT=IERR)
!            CALL AERR('D(MCa,NCa)',IERR,MCa*NCa)

            !        check for allocation error
            if (IERR /= 0) then
               call qnerror('Sample file too large: increase istep and/or jstep', ' ', ' ')
               MCA = 0
               NCA = 0
               !           we cannot deallocate erroneously allocated arrays directly and need to reallocate it correctly first
               allocate (D(1, 1))
               deallocate (D)
               goto 1234
            end if
!         end do   ! do while ( ierr.ne.0 )

            call ReadLargeArcInfoBlock(MINP, MCfile, NCfile, istart, iend, jstart, jend, MCa, NCa, RMIS, istep, jstep, D)

!        modife arcinfo module data
!         X0 = X0 + dble(istep-1)*0.5d0*DXa
!         Y0 = Y0 + dble(jstep-1)*0.5d0*DYa
            X0 = X0 + (istart - 1) * Dxa + dble(istep - 1) * 0.5d0 * DXa
            Y0 = Y0 + (jstart - 1) * Dya + dble(jstep - 1) * 0.5d0 * DYa
            DXa = dble(istep) * DXa
            DYa = dble(jstep) * DYa

         end if ! if ( LdirectReadBlock )

!     error handling
1234     continue

         return
      end

end module m_reaarc
