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

module m_reaweir

   implicit none

   private

   public :: reaweir

contains

   subroutine REAweir(MMDD, JA)
      use precision, only: dp
      use m_missing
      use m_fixedweirs
      use M_GRID
      use m_readyy
      use m_qn_read_error
      use m_qn_eof_error
      use m_filez, only: doclose, newfil

      integer :: mmdd, ja
      integer :: m, n, MOUT
      real(kind=dp) :: hu, hv, Du1, Du2, Dv1, Dv2

      character REC * 132

      JA = 0

      call NEWFIL(MOUT, 'WEIRS.POL')

5     continue

      read (MMDD, '(A)', end=777) REC

      if (index(rec, '#') == 0) then

         read (REC(2:), *, ERR=999) M, N, HU, Du1, Du2, HV, Dv1, Dv2

         if (HU > 0) then
            write (MOUT, *) XC(M, N), YC(M, N), HU, DU1, DU2
            write (MOUT, *) XC(M, N - 1), YC(M, N - 1), HU, DU1, DU2
            write (MOUT, *) DMISS, DMISS, DMISS
         end if

         if (HV > 0) then
            write (MOUT, *) XC(M, N), YC(M, N), HV, DV1, DV2
            write (MOUT, *) XC(M - 1, N), YC(M - 1, N), HV, DV1, DV2
            write (MOUT, *) DMISS, DMISS, DMISS
         end if

      end if

      goto 5

777   call DOCLOSE(MMDD)
      call DOCLOSE(MOUT)
      JA = 1
      return

999   continue
      call QNEOFERROR(MMDD)
      call READYY('Reading SIMONA *.bottom File', -1d0)
      call DOCLOSE(MMDD)
      JA = 0
      return

888   call QNREADERROR('Reading ERROR SIMONA WEIR File', REC, MMDD)
      call DOCLOSE(MMDD)
      JA = 0
   end subroutine REAWEIR

end module m_reaweir
