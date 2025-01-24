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

module m_addlink2d

   implicit none

   private

   public :: addlink2D

contains

   subroutine addlink2D(L, japerim) ! and add area's and volumes of 2D links
      use precision, only: dp
      use m_flowgeom
      use m_flow
      use m_sferic, only: jasfer3d
      use m_get_link_area_wid2D
      use m_get_prof2d
      use m_get_hpr_nostruc
      use m_cor2linx, only: cor2linx
      use m_cor2liny, only: cor2liny

      implicit none

      integer :: japerim, L

      integer :: k1, k2, k3, k4, jaconv, ifrctyp
      real(kind=dp) :: hpr1, ar1, wid1, hpr2, ar2, wid2, aru, widu, aconvu
      real(kind=dp) :: dx1, dx2, frcn, BL1, BL2, b21, wu2, ai
      real(kind=dp) :: beta, bt2, deltaa, hyr, uucn, ucna, bob1, bob2, hsmall

      if (japerim == 0) then

         bob1 = bob(1, L); bob2 = bob(2, L)
         if (bob1 < bob2) then
            BL1 = bob1; BL2 = bob2
         else
            BL1 = bob2; BL2 = bob1
         end if
         wu2 = wu(L); b21 = BL2 - BL1; ai = b21 / wu2
         k1 = ln(1, L); k2 = ln(2, L)

         hpr1 = s1(k1) - BL1
         if (hpr1 > 0) then
            call getlinkareawid2D(wu2, b21, ai, hpr1, ar1, wid1)
            dx1 = 0.5d0 * dx(L) * acl(L)
            a1(k1) = a1(k1) + dx1 * wid1
            vol1(k1) = vol1(k1) + dx1 * ar1
         end if

         hpr2 = s1(k2) - BL1 ! == 5,6: (ibedlevtyp=3), 2D conveyance, link or node
         if (hpr2 > 0) then
            call getlinkareawid2D(wu2, b21, ai, hpr2, ar2, wid2)
            dx2 = 0.5d0 * dx(L) * (1d0 - acl(L))
            a1(k2) = a1(k2) + dx2 * wid2
            vol1(k2) = vol1(k2) + dx2 * ar2
         end if

      else if (hu(L) > 0d0) then

         bob1 = bob(1, L); bob2 = bob(2, L)

         if (bob1 < bob2) then
            BL1 = bob1; BL2 = bob2
         else
            BL1 = bob2; BL2 = bob1
         end if
         wu2 = wu(L)

         b21 = BL2 - BL1; ai = b21 / wu2
         k1 = ln(1, L); k2 = ln(2, L)

         hpr1 = get_hpr_nostruc(L)

         if (jaconveyance2D > 0) then

            frcn = frcu(L); ifrctyp = ifrcutp(L)

            jaconv = jaconveyance2D

            if (jaconv >= 3) then !> see sysdoc5, 2D conveyance approach I , II
               if (bob(1, L) < bob(2, L)) then
                  k3 = lncn(1, L); k4 = lncn(2, L)
               else
                  k3 = lncn(2, L); k4 = lncn(1, L)
               end if

               !bb1 = sign(1d0, bob(2,L) - bob(1,L) )   ! faster coding?
               !k3  = lncn(1,L)*bb1 + lncn(2,L)*(1d0-bb1)
               !k4  = lncn(2,L)*bb1 + lncn(1,L)*(1d0-bb1)

               if (jaconv == 4) then
                  hsmall = BL1 + hpr1 - BL2 ! depth at shallow side
                  if (hsmall / hpr1 > 0.9d0) then
                     jaconv = 1 ! Hydr rad
                  end if
               end if

               if (jaconv >= 3) then
                  if (jasfer3D == 0) then
                     uucn = abs(ucnx(k3) * csu(L) + ucny(k3) * snu(L))
                  else
                     uucn = abs(csu(L) * cor2linx(L, 1, ucnx(k3), ucny(k3)) + snu(L) * cor2liny(L, 1, ucnx(k3), ucny(k3)))
                  end if
                  ucna = (ucnx(k3)**2 + ucny(k3)**2)
                  if (ucna > 0d0 .and. uucn > 0d0) then
                     ucna = sqrt(ucna)
                     beta = sqrt(uucn / ucna)
                     if (beta > 0.97d0) then
                        beta = 1d0
                     end if
                  else
                     beta = 1d0 ! do simple hydraulic radius approach
                  end if

                  if (jasfer3D == 0) then
                     uucn = abs(ucnx(k4) * csu(L) + ucny(k4) * snu(L))
                  else
                     uucn = abs(csu(L) * cor2linx(L, 2, ucnx(k4), ucny(k4)) + snu(L) * cor2liny(L, 2, ucnx(k4), ucny(k4)))
                  end if
                  ucna = (ucnx(k4)**2 + ucny(k4)**2)
                  if (ucna > 0d0 .and. uucn > 0d0) then
                     ucna = sqrt(ucna)
                     bt2 = sqrt(uucn / ucna)
                     if (bt2 > 0.97d0) then
                        bt2 = 1d0
                     end if
                  else
                     bt2 = 1d0
                  end if

                  deltaa = (beta - bt2) / wu2

                  if (jaconv == 4) then
                     if (beta == 1d0 .and. bt2 == 1d0) then
                        jaconv = 2
                     end if
                  end if

               end if

            end if

            call getprof2d(hpr1, wu2, b21, ai, frcn, ifrctyp, widu, aru, aconvu, jaconv, beta, deltaa, hyr)

            if (frcn > 0) then
               cfuhi(L) = aifu(L) * ag * aconvu
            else
               cfuhi(L) = 0d0
            end if
            au(L) = aru
         else
            au(L) = hpr1 * wu(L)
         end if
      end if
   end subroutine addlink2D

end module m_addlink2d
