!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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
module m_makessq
   implicit none
contains
   subroutine MAKESSQ(S, A, SR, SL, SSQ, NT, MFAC, IMAX)
      use precision, only: dp
      use m_makesr, only: makesr

      integer :: nt, mfac, imax
      real(kind=dp) :: S(IMAX), A(IMAX), SR(IMAX), SL(IMAX), SSQ(IMAX)
      integer :: i, k, kr
      real(kind=dp) :: ar, al

      if (NT == 2) then
         do K = 1, MFAC + 1
            SSQ(K) = S(1) + (S(2) - S(1)) * (real(K - 1, kind=dp)) / real(MFAC, kind=dp)
         end do
      else if (NT >= 3) then
         do I = 2, NT - 1
            A(I) = (S(I + 1) - S(I)) / (S(I) - S(I - 1))
         end do
         A(1) = A(2)
         A(NT) = A(NT - 1)

         do I = 1, NT - 1
            AR = A(I + 1)**(1.0 / real(MFAC, kind=dp))
            call MAKESR(AR, S(I), S(I + 1), SR, MFAC)
            AL = A(I)**(1.0 / real(MFAC, kind=dp))
            call MAKESR(AL, S(I), S(I + 1), SL, MFAC)
            do k = 1, MFAC + 1
               KR = (I - 1) * MFAC + K
               AR = real(K - 1, kind=dp) / real(MFAC, kind=dp)
               AL = 1 - AR
               SSQ(KR) = AR * SR(K) + AL * SL(K)

               AR = (SSQ(KR) - S(I)) / (S(I + 1) - S(I))
               AL = 1 - AR
               SSQ(KR) = AR * SR(K) + AL * SL(K)

!              AL = ( S(I+1) - SL(K) ) / ( S(I+1) - S(I) )
!              AR = ( SR(K)  -  S(I) ) / ( S(I+1) - S(I) )
!              AT = AL + AR
!              AL = AL/AT
!              AR = AR/AT
!              SSQ(KR) = AR*SR(K) + AL*SL(K)
            end do
         end do

      end if

      return
   end subroutine makessq
end module m_makessq
