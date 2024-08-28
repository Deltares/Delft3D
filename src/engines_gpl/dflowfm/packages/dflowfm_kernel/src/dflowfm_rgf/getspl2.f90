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

      subroutine GETSPL2(X, XI2, XJ2, MC, NC, MMAX, NMAX)
         use M_MISSING
         use m_spline
         implicit none
         integer :: i
         integer :: iff
         integer :: il
         integer :: in
         integer :: jalin
         integer :: jn
         integer :: k
         integer :: mc
         integer :: mmax
         integer :: mnmax
         integer :: nc
         integer :: nmax
!     VUL DE ARRAY MET TWEEDE AFGELEIDES IN I EN J RICHTING
!     HAAL TELKENS EEN LIJNTJE, DOE SPLINE EN ZET TERUG

         double precision :: X(MMAX, NMAX), XI2(MMAX, NMAX), XJ2(MMAX, NMAX)
         double precision, allocatable :: XH1(:), XH21(:), XHH(:)

         MNMAX = max(MMAX, NMAX)

         allocate (XH1(MNMAX), XH21(MNMAX), XHH(MNMAX))

         XI2 = DXYMIS
         XJ2 = DXYMIS

         do JN = 1, NC
            call GETIJ(X, XH1, MMAX, NMAX, MNMAX, 1, MC, JN, JN)
            JALIN = 0
            K = 0
            do I = 1, MC
               if (XH1(I) /= DXYMIS) then
                  if (JALIN == 0) then
!                 BEGIN LIJN BIJ I
                     IFF = I
                     JALIN = 1
                  end if
                  K = K + 1
                  XHH(K) = XH1(I)
                  if (JALIN == 1 .and. I == MC) then
!                 EINDE LIJN BIJ MC DUS SPLINE VRAGEN
                     call SPLINE(XHH, K, XH21)
                     call PUTIJ(XJ2, XH21, MMAX, NMAX, MNMAX, IFF, MC, JN, JN)
                  end if
               else if (JALIN == 1) then
!              EINDE LIJN BIJ I - 1 DOORDAT I NUL IS, SPLINE VRAGEN
                  JALIN = 0
                  IL = I - 1
                  call SPLINE(XHH, K, XH21)
                  call PUTIJ(XJ2, XH21, MMAX, NMAX, MNMAX, IFF, IL, JN, JN)
                  K = 0
               end if
            end do
         end do

         do IN = 1, MC
            call GETIJ(X, XH1, MMAX, NMAX, MNMAX, IN, IN, 1, NC)
            JALIN = 0
            K = 0
            do I = 1, NC
               if (XH1(I) /= DXYMIS) then
                  if (JALIN == 0) then
!                 BEGIN LIJN BIJ I
                     IFF = I
                     JALIN = 1
                  end if
                  K = K + 1
                  XHH(K) = XH1(I)
                  if (JALIN == 1 .and. I == NC) then
!                 EINDE LIJN BIJ MC DUS SPLINE VRAGEN
                     call SPLINE(XHH, K, XH21)
                     call PUTIJ(XI2, XH21, MMAX, NMAX, MNMAX, IN, IN, IFF, NC)
                  end if
               else if (JALIN == 1) then
!              EINDE LIJN BIJ I - 1 DOORDAT I NUL IS, SPLINE VRAGEN
                  JALIN = 0
                  IL = I - 1
                  call SPLINE(XHH, K, XH21)
                  call PUTIJ(XI2, XH21, MMAX, NMAX, MNMAX, IN, IN, IFF, IL)
                  K = 0
               end if
            end do
         end do

         deallocate (XH1, XH21, XHH)

         return
      end subroutine GETSPL2
