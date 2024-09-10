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

!***************7***  INTERPOLATION ************************************
      subroutine DEREFINE(M1, N1, M2, N2, NUM)
         use m_grid
         use m_gridsettings
         use unstruc_messages
         use m_missing
         use m_readyy
         use m_qnerror
         implicit none

         integer :: m1, n1, m2, n2, num

         integer :: I, J, IR, INOW, JR, JNOW, MFA, NFA, MFAA, NFAA, MD, ND
         double precision, allocatable :: XR(:, :), YR(:, :)
         allocate (xr(mmax, nmax), yr(mmax, nmax))

         if (MFAC >= MC) then
            call QNERROR('M-refinement factor larger than grid M-dimension', ' ', ' ')
            NUM = 0
            return
         end if
         if (NFAC >= NC) then
            call QNERROR('N-refinement factor larger than grid M-dimension', ' ', ' ')
            NUM = 0
            return
         end if
         call SAVEgrd()

         call mess(LEVEL_DEBUG, 'DEREFINE BY: ', MFAC, NFAC)
         call READYY('DEREFINE', 0d0)

         XR = dmiss
         YR = dmiss

         MD = M2 - M1
         ND = N2 - N1
         MFAA = MFAC
         NFAA = NFAC
         if (MD == 0) MFAA = 1
         if (ND == 0) NFAA = 1

         IR = 1
         INOW = 1
         do I = 1, MC
            if (INOW >= M1 .and. INOW < M2) then
               MFA = MFAA
            else
               MFA = 1
            end if
            JR = 1
            JNOW = 1
            if (INOW <= MC) then
               do j = 1, NC
                  if (JNOW >= N1 .and. JNOW < N2) then
                     NFA = NFAA
                  else
                     NFA = 1
                  end if
                  if (JNOW <= NC) then
                     XR(IR, JR) = Xc(INOW, JNOW)
                     YR(IR, JR) = Yc(INOW, JNOW)
                     JR = JR + 1
                     JNOW = JNOW + NFA
                  end if
               end do
               IR = IR + 1
               INOW = INOW + MFA
            end if
         end do

         call PUTARR(XR, Xc, MMAX, NMAX)
         call PUTARR(YR, Yc, MMAX, NMAX)
         call NUMS(Xc, mmax, nmax, MC, NC)
!     MC = INOW
!     NC = JNOW

         call READYY('DEREFINE', 1d0)
         call READYY('DEREFINE', -1d0)
         deallocate (XR, YR)
         return
      end subroutine derefine
