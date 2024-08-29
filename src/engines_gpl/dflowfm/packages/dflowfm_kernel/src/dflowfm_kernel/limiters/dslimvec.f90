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

   !> limited higher-order correction of vector data
   subroutine dslimvec(ds1x, ds1y, ds2x, ds2y, csu, snu, limtyp, dsx, dsy)
      use m_flowparameters
      use m_dslim
      implicit none

      double precision, intent(in) :: ds1x, ds1y !< "voorslope" components
      double precision, intent(in) :: ds2x, ds2y !< "naslope" components
      double precision, intent(in) :: csu, snu !< orientation vector components
      integer, intent(in) :: limtyp !< limiter type
      double precision, intent(out) :: dsx, dsy !< correction components

      double precision :: ds1n, ds1t !< normal and tangential component, respectively
      double precision :: ds2n, ds2t !< normal and tangential component, respectively
      double precision :: dsn, dst

      if (jalimnor == 1) then
         ds1n = csu * ds1x + snu * ds1y
         ds1t = -snu * ds1x + csu * ds1y

         ds2n = csu * ds2x + snu * ds2y
         ds2t = -snu * ds2x + csu * ds2y

         dsn = 0d0
         dst = 0d0

         if (abs(ds2n) > eps10 .and. abs(ds1n) > eps10) then
            dsn = dslim(ds1n, ds2n, limtyp)
         end if

         if (abs(ds2y) > eps10 .and. abs(ds1y) > eps10) then
            dst = dslim(ds1t, ds2t, limtyp)
         end if

         dsx = csu * dsn - snu * dst
         dsy = snu * dsn + csu * dst

      else
         dsx = dslim(ds1x, ds2x, limtyp)
         dsy = dslim(ds1y, ds2y, limtyp)
      end if

      return
   end subroutine dslimvec
