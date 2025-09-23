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

module m_dslimvec

   implicit none

   private

   public :: dslimvec

contains

   !> limited higher-order correction of vector data
   subroutine dslimvec(ds1x, ds1y, ds2x, ds2y, csu, snu, limtyp, dsx, dsy)
      use precision, only: dp
      use m_flowparameters, only: eps10
      use m_dslim, only: dslim
      use m_sferic, only: jasfer3d

      implicit none

      real(kind=dp), intent(in) :: ds1x !< "voorslope" x-component
      real(kind=dp), intent(in) :: ds1y !< "voorslope" y-component
      real(kind=dp), intent(in) :: ds2x !< "naslope" x-component
      real(kind=dp), intent(in) :: ds2y !< "naslope" y-component
      real(kind=dp), intent(in) :: csu !< orientation vector x-component
      real(kind=dp), intent(in) :: snu !< orientation vector y-component
      integer, intent(in) :: limtyp !< limiter type
      real(kind=dp), intent(out) :: dsx !< correction x-component
      real(kind=dp), intent(out) :: dsy !< correction y-component

      real(kind=dp) :: ds1n, ds1t !< normal and tangential component, respectively
      real(kind=dp) :: ds2n, ds2t !< normal and tangential component, respectively
      real(kind=dp) :: dsn, dst

      if (jasfer3d == 1) then
         ds1n = csu * ds1x + snu * ds1y
         ds1t = -snu * ds1x + csu * ds1y

         ds2n = csu * ds2x + snu * ds2y
         ds2t = -snu * ds2x + csu * ds2y

         dsn = 0.0_dp
         dst = 0.0_dp

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

end module m_dslimvec
