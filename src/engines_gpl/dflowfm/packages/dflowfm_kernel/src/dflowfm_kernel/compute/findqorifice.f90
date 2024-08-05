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

subroutine findqorifice(gateheight, crestheight, h1, h3, q, h2, hg, regime, num, qcrit) ! bepaal q en hoogte h2 achter schuif, waterstand links = h1, rechts= h4, schuif = a, alles tov bodem
   implicit none
   double precision :: gateheight ! gate height above crest
   double precision :: crestheight ! crest height above bed
   double precision :: h1 ! upstream waterheight above crest
   double precision :: h3 ! downstream waterheight above crest
   double precision :: q ! flux m3/s                                    (out)
   double precision :: h2 ! pressure height above crest       after gate (out)
   double precision :: hg ! vena contracta height above crest after gate (out)
   double precision :: qcrit ! critical discharge m2/s                      (out)
   character(len=*) :: regime !                                              (out)
   double precision :: g, ha, hb, qa, qb, qc, hc, a, d, qda, qdb, qdc, hgc
   integer :: num, k, nummin
   double precision :: qf, hgf, h2f, qer, qermin
   g = 9.81 ! h1 = waterhoogte bovenstrooms
   h3 = min(h3, h1 - 0.0001) ! hg = gateheight * contractie = effectieve keeldoorsnee
   d = crestheight
   a = gateheight
   h1 = max(h1, 0.0001d0)
   h3 = max(h3, 0.00001d0)
   h2 = h3
   qermin = 1d9

   hg = gateheight * 0.5d0 ! lower boundary
   hg = max(hg, 0.0001d0)

   if (gateheight >= h1) then ! gate above water
      q = 11111d0
      regime = 'gate above water'
      return
   else if (gateheight < 0.001) then
      q = 0d0
      regime = 'gate closed, a<0.001 '
      return
   end if

   qcrit = sqrt(2d0 * g * (h1 - hg) / (hg**(-2) - h1**(-2)))
   if (h3 < 0.60 * h1) then
      regime = 'free gate flow '
      q = qcrit
      return
   end if

   do k = 1, 50

      ha = hg; hb = h3; hgc = hg
      call qorifdif(hg, d, h1, h3, ha, qda)
      call qorifdif(hg, d, h1, h3, hb, qdb)

      num = 0; qdc = 1d9
      do while (abs(qdc) > 1d-6 .and. abs(qda - qdb) > 1d-6 .and. num < 50)

         num = num + 1

         !    if (ha >= h2) then
         !       regime = 'free weir flow' ; return
         !    endif

         hc = ha - qda * (ha - hb) / (qda - qdb) ! regula falsi
         hc = max(hc, hg)
         hc = min(hc, h3)
         call qorifdif(hg, d, h1, h3, hc, qdc)
         if (qda * qdc > 0) then
            ha = hc; qda = qdc
         else if (qdb * qdc > 0) then
            hb = hc; qdb = qdc
         end if

      end do

      h2 = hc
      call getq1(hg, d, h1, h2, qa)
      call getq2(hg, d, h2, h3, qb)
      call getq3(hg, d, a, h1, h2, qc)
      q = 0.5d0 * (qa + qb)
      qer = abs(q - qc)
      if (qer < qermin) then
         qermin = qer; qf = q; hgf = hg; h2f = h2; nummin = num
      end if

      hg = hg + 0.01d0 * a

      regime = 'submerged gate flow '

   end do

   h2 = h2f
   hg = hgf
   q = qf
   num = nummin

end subroutine findqorifice
