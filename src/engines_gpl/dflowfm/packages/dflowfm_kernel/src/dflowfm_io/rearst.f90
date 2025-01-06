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

module m_rearst

   implicit none

   private

   public :: rearst

contains

   !> Reads raw restart data from a formatted restart file by wrirst.
   !! Water levels and velocities are directly stored into the flow arrays.
   subroutine REARST(Mrst, JA)
      use precision, only: dp
      use unstruc_model
      use UNSTRUC_MESSAGES
      use M_FLOWTIMES
      use M_FLOW
      use M_FLOWGEOM
      use m_qnerror
      use m_qn_eof_error
      use m_filez, only: doclose

      integer, intent(inout) :: Mrst !< Input file pointer (should already be open)
      integer, intent(out) :: ja !< Return status (0 = success)

      integer :: k
      integer :: l
      integer :: NDXR, LNXR ! alleen binnen deze subroutine
      real(kind=dp) :: DUM

      ja = 0
      ! READ(Mrst,*)  REFDATLOC, TSTART_USERLOC, NDXR, LNXR
      read (Mrst, *) DUM, DUM, NDXR, LNXR

      if (NDXR /= NDX .or. LNXR /= LNX) then
         write (MSGBUF, '(A)') 'DIMENSIONS ON RESTART FILE NOT EQUAL TO CURRENT MODEL DIMENSIONS'; call MSG_FLUSH()
         call QNERROR('DIMENSIONS ON RESTART FILE NOT EQUAL TO CURRENT MODEL DIMENSIONS', ' ', ' ')
         ja = 1
      end if

      read (Mrst, *)
      do K = 1, NDX
         read (Mrst, *, end=999, ERR=888) S0(K)
      end do
      S0 = max(BL, S0)
      s1 = s0

      read (Mrst, *)

      do L = 1, LNX
         read (Mrst, *, end=999) U0(L)
      end do
      call doclose(mrst)
      u1 = u0

      return

888   ja = 1
      return
999   call QNEOFERROR(MRST)
      call doclose(mrst)
      ja = 1

   end subroutine reaRST

end module m_rearst
