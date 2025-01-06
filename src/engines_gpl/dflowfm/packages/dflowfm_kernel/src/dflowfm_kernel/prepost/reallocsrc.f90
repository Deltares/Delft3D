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

module m_reallocsrc

implicit none

private

public :: reallocsrc

contains

subroutine reallocsrc(n)
   use m_transport, only: NUMCONST
   use fm_external_forcings_data
   use m_alloc
   use m_missing
   use m_polygon, only: npl

   integer :: n

   msrc = max(msrc, npl)
   call realloc(ksrc, (/6, n/), keepexisting=.true., fill=0)
   call realloc(qsrc, n, keepExisting=.true., fill=0d0)
   call realloc(tmsrc, n, keepExisting=.true., fill=0d0)
   call realloc(sasrc, n, keepExisting=.true., fill=0d0)
   call realloc(CCsrc, (/NUMCONST, n/), keepExisting=.true., fill=0d0)
   call realloc(arsrc, n, keepExisting=.true., fill=0d0)
   call realloc(cssrc, (/2, n/), keepExisting=.true., fill=0d0)
   call realloc(snsrc, (/2, n/), keepExisting=.true., fill=0d0)
   call realloc(zsrc, (/2, n/), keepExisting=.true., fill=dmiss)
   call realloc(zsrc2, (/2, n/), keepExisting=.true., fill=dmiss) ! ipv  ; zsrc2 = dmiss
   ! call realloc (srsn , (/ 6,n /), keepExisting = .true.)
   call realloc(srsn, (/2 * (NUMCONST + 1), n/), keepExisting=.true.)
   call realloc(jamess, n, keepExisting=.true.)
   ! call realloc (qstss, 3*n,       keepExisting = .true., fill=0d0)
   call realloc(qstss, (NUMCONST + 1) * n, keepExisting=.true., fill=0d0)
   call realloc(srcname, n, keepExisting=.true., fill=' ')
   call realloc(xsrc, (/n, msrc/), keepExisting=.true., fill=dmiss)
   call realloc(ysrc, (/n, msrc/), keepExisting=.true., fill=dmiss)
   call realloc(nxsrc, n, keepExisting=.true., fill=0)
   call realloc(qsrcavg, n, keepExisting=.true., fill=0d0)
   call realloc(vsrccum, n, keepExisting=.true., fill=0d0)
   call realloc(vsrccum_pre, n, keepExisting=.true., fill=0d0)
end subroutine reallocsrc

end module m_reallocsrc
