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
module m_delsam
   implicit none
contains
!>    delete samples
!>      jaconfirm=0: do not prompt for confirmation,       keep arrays,        make copy
!>                1:        prompt for confirmation,       keep arrays,        make copy
!>               -1: do not prompt for confirmation, deallocate arrays, do not make copy
   subroutine DELSAM(JACONFIRM) ! SPvdP: need promptless delsam in orthogonalisenet
      use M_SAMPLES
      use m_polygon
      use m_missing
      use geometry_module, only: dbpinpol

      integer, intent(in) :: JACONFIRM !< prompt for confirmation (1) or not (0)

      integer :: i
      integer :: inhul
      integer :: ja
      integer :: k
      integer :: key
      integer :: nsol
      double precision :: rd
      double precision :: xi
      double precision :: yi

      if (jaconfirm == -1) then
         if (nsmax > 0) then
            nsmax = 0; ns = 0
            if (allocated(xs)) deallocate (xs, ys, zs)
            if (allocated(ipsam)) deallocate (ipsam)
         end if
         return
      end if

      if (Npl <= 2) then
         if (JACONFIRM == 1) then
            call CONFRM('NO POLYON, SO DELETE all SAMPLE POINTS ? ', JA)
         else
            JA = 1
         end if
         if (JA == 0) then
            KEY = 0
            return
         end if
         call SAVESAM()
         do I = 1, NS
            XS(I) = DMISS
            YS(I) = DMISS
            ZS(I) = DMISS
            ipsam(i) = 0
         end do
         NS = 0
         return
      end if
      ! Else: check in polygon
      call SAVESAM()
      INHUL = -1
      do I = 1, NS
         RD = ZS(I)
         XI = XS(I)
         YI = YS(I)
         call DBPINPOL(xI, yI, INHUL, dmiss, JINS, NPL, xpl, ypl, zpl)
         if (INHUL == 1) ZS(I) = dmiss
      end do

      K = 0
      NSOL = NS
      do i = 1, NS
         if (ZS(I) /= dmiss) then
            K = K + 1
            XS(K) = XS(I)
            YS(K) = YS(I)
            ZS(K) = ZS(I)
            ipsam(k) = ipsam(i)
         end if
      end do
      NS = K

      do I = NS + 1, NSOL
         XS(I) = DMISS
         YS(I) = DMISS
         ZS(I) = DMISS
         ipsam(i) = 0
      end do

      return
   end subroutine DELSAM
end module m_delsam
