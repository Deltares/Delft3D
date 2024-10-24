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
module m_get_chezy
   implicit none
   private

   public :: get_chezy
contains
!> Get the Chezy coefficient
!! This routine is not safe for frcn == 0
   function get_chezy(h1, frcn, ifrctyp, L) result(cz)
      use m_physcoef, only: sag, vonkar, ee
      use m_flow, only: u1, v
      use precision, only: dp

      real(kind=dp), intent(in) :: h1 !< hydraulic radius
      real(kind=dp), intent(in) :: frcn !< friction coeff
      integer, intent(in) :: ifrctyp !< friction type
      integer, intent(in) :: L !< index of the cell on which the Chezy coefficient is computed
      real(kind=dp) :: cz !< Computed Chezy coefficient

      real(kind=dp) :: h0
      real(kind=dp) :: hurou, sqcf, z0, umod
      real(kind=dp), parameter :: sixth = 1.0_dp / 6.0_dp

      h0 = max(h1, 1e-4_dp)
      if (ifrctyp == 0) then ! Chezy type
         cz = frcn
      else if (ifrctyp == 1) then ! Manning type
         cz = (h0**sixth) / frcn
      else if (ifrctyp == 2) then ! White Colebrook Delft3
         z0 = min(frcn / 30.0_dp, h0 * 0.3_dp)
         sqcf = vonkar / log(h0 / (ee * z0))
         cz = sag / sqcf
      else if (ifrctyp == 3) then ! White Colebrook WAQUA
         hurou = max(0.5_dp, h0 / frcn)
         cz = 18.0_dp * log10(12.0_dp * hurou)
      else if (ifrctyp == 4 .or. ifrctyp == 5 .or. ifrctyp == 6) then ! also manning, just testing implicitness in furu
         cz = (h0**sixth) / frcn
      else
         cz = 60.0_dp ! dummy
         umod = sqrt(u1(L) * u1(L) + v(L) * v(L))
         call hydraulicallysmooth(umod, h0, sqcf)
         if (sqcf > 0.0_dp) then
            Cz = sag / sqcf
         else
            Cz = 0.0_dp
         end if
      end if
   end function get_chezy
end module m_get_chezy
