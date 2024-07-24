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

   subroutine reconstructsedtransports()
      ! Reconstructs cell centre transports from link based values for output purposes
      use m_fm_erosed
      use m_flowgeom
      use m_sediment

      implicit none

      integer :: l, ll, k1, k2, k

      ! init
      sbcx = 0.0_fp
      sbcy = 0.0_fp
      sbwx = 0.0_fp
      sbwy = 0.0_fp
      sscx = 0.0_fp
      sscy = 0.0_fp
      sswx = 0.0_fp
      sswy = 0.0_fp
      sxtot = 0.0_fp
      sytot = 0.0_fp

      do l = 1, lsed
         do ll = 1, lnx
            k1 = ln(1, ll); k2 = ln(2, ll)
            sscx(k1, sedtot2sedsus(l)) = sscx(k1, sedtot2sedsus(l)) + wcx1(ll) * e_ssn(ll, l)
            sscx(k2, sedtot2sedsus(l)) = sscx(k2, sedtot2sedsus(l)) + wcx2(ll) * e_ssn(ll, l)
            sscy(k1, sedtot2sedsus(l)) = sscy(k1, sedtot2sedsus(l)) + wcy1(ll) * e_ssn(ll, l)
            sscy(k2, sedtot2sedsus(l)) = sscy(k2, sedtot2sedsus(l)) + wcy2(ll) * e_ssn(ll, l)
         end do
      end do

      do l = 1, lsedtot
         if (has_bedload(tratyp(l))) then
            do ll = 1, lnx
               k1 = ln(1, ll); k2 = ln(2, ll)
               ! bed load transports due to currents
               sbcx(k1, l) = sbcx(k1, l) + wcx1(ll) * e_sbcn(ll, l)
               sbcx(k2, l) = sbcx(k2, l) + wcx2(ll) * e_sbcn(ll, l)
               sbcy(k1, l) = sbcy(k1, l) + wcy1(ll) * e_sbcn(ll, l)
               sbcy(k2, l) = sbcy(k2, l) + wcy2(ll) * e_sbcn(ll, l)
               ! bed load transports due to waves
               sbwx(k1, l) = sbwx(k1, l) + wcx1(ll) * e_sbwn(ll, l)
               sbwx(k2, l) = sbwx(k2, l) + wcx2(ll) * e_sbwn(ll, l)
               sbwy(k1, l) = sbwy(k1, l) + wcy1(ll) * e_sbwn(ll, l)
               sbwy(k2, l) = sbwy(k2, l) + wcy2(ll) * e_sbwn(ll, l)
               ! suspended transports due to waves
               sswx(k1, l) = sswx(k1, l) + wcx1(ll) * e_sswn(ll, l)
               sswx(k2, l) = sswx(k2, l) + wcx2(ll) * e_sswn(ll, l)
               sswy(k1, l) = sswy(k1, l) + wcy1(ll) * e_sswn(ll, l)
               sswy(k2, l) = sswy(k2, l) + wcy2(ll) * e_sswn(ll, l)
            end do
         end if

         ! total transports
         do k = 1, ndx
            sxtot(k, l) = sbcx(k, l) + sbwx(k, l) + sswx(k, l) + sscx(k, l)
            sytot(k, l) = sbcy(k, l) + sbwy(k, l) + sswy(k, l) + sscy(k, l)
         end do
      end do

   end subroutine reconstructsedtransports
