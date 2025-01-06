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

module m_reabl

   implicit none

contains

   subroutine reabl(mout) ! read bottom level
      use m_flowgeom
      use M_samples
      use m_missing
      use m_delsam
      use m_reasam
      use m_qn_read_error
      use m_interpdivers
      use m_filez, only: doclose

      integer :: mout
      character(len=256) :: rec

      call reasam(mout, 0)

      bl = dmiss

      call interpdivers(1)

      call delsam(-1) ! deallocate

      return

888   call qnreaderror('trying to read nr of internal flow nodes but getting', rec, mout)
      call doclose(mout)

   end subroutine reabl

end module m_reabl
