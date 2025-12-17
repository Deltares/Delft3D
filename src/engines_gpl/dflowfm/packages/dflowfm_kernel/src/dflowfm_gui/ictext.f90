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

module m_ictext

   implicit none

contains

   subroutine ICTEXT(TEX, NX, NY, NCOL)
      use precision, only: dp
      use unstruc_colors, only: x1, x2, iws, y2, y1, ihs
      use m_set_col, only: setcol
      use m_draw_text, only: drawtext
      implicit none
      integer :: l
      integer :: ncol
      integer :: nx
      integer :: ny
      real(kind=dp) :: x
      real(kind=dp) :: y
!     grafische tekst op normale text posities
      character TEX * (*)
      X = X1 + (X2 - X1) * real(NX, kind=dp) / real(IWS, kind=dp)
      Y = Y2 + (Y1 - Y2) * real(NY, kind=dp) / real(IHS, kind=dp)
      call SETCOL(NCOL)
      L = len_trim(TEX)
      call DRAWTEXT(real(X), real(Y), TEX(1:L))
      return
   end

end module m_ictext
