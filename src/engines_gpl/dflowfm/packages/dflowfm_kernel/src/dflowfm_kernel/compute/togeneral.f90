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

subroutine togeneral(ng, hulp, ngen, widths)
   use m_strucs
   use m_alloc
   implicit none
   integer, intent(in) :: ng !< Index of this general structure in the generalstruc(:) array
   double precision, intent(in) :: hulp(26) !< genstru params read from file
   integer, intent(in) :: ngen !< Number of flow links crossed by this single general structure
   double precision, intent(in) :: widths(ngen) !< wu(L) values for all links crossed by this single general structure

   generalstruc(ng)%widthleftW1 = hulp(1) !< this and following: see Sobek manual
   generalstruc(ng)%levelleftZb1 = hulp(2)
   generalstruc(ng)%widthleftWsdl = hulp(3)
   generalstruc(ng)%levelleftZbsl = hulp(4)
   generalstruc(ng)%widthcenter = hulp(5)
   generalstruc(ng)%levelcenter = hulp(6)
   generalstruc(ng)%widthrightWsdr = hulp(7)
   generalstruc(ng)%levelrightZbsr = hulp(8)
   generalstruc(ng)%widthrightW2 = hulp(9)
   generalstruc(ng)%levelrightZb2 = hulp(10)
   generalstruc(ng)%gateheight = hulp(11)
   generalstruc(ng)%gateheightintervalcntrl = hulp(12)
   generalstruc(ng)%pos_freegateflowcoeff = hulp(13)
   generalstruc(ng)%pos_drowngateflowcoeff = hulp(14)
   generalstruc(ng)%pos_freeweirflowcoeff = hulp(15)
   generalstruc(ng)%pos_drownweirflowcoeff = hulp(16)
   generalstruc(ng)%pos_contrcoeffreegate = hulp(17)
   generalstruc(ng)%neg_freegateflowcoeff = hulp(18)
   generalstruc(ng)%neg_drowngateflowcoeff = hulp(19)
   generalstruc(ng)%neg_freeweirflowcoeff = hulp(20)
   generalstruc(ng)%neg_drownweirflowcoeff = hulp(21)
   generalstruc(ng)%neg_contrcoeffreegate = hulp(22)
   generalstruc(ng)%extraresistance = hulp(23) ! lambda = L*g/ (C*C)
   generalstruc(ng)%dynstructext = hulp(24)
   if (hulp(25) > 0d0) then
      generalstruc(ng)%gatedoorheight = hulp(25)
   end if
   generalstruc(ng)%dooropeningwidth = hulp(26)
   generalstruc(ng)%stabilitycounter = 0d0

   call realloc(generalstruc(ng)%widthcenteronlink, ngen)
   generalstruc(ng)%widthcenteronlink(1:ngen) = widths(1:ngen)
   call realloc(generalstruc(ng)%gateheightonlink, ngen)
   generalstruc(ng)%gateheightonlink(1:ngen) = generalstruc(ng)%gateheight
   generalstruc(ng)%numlinks = ngen

   call realloc(generalstruc(ng)%gateclosedfractiononlink, ngen, fill=0d0)
end subroutine togeneral
