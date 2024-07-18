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

  subroutine JGTEXT(TEX, X, Y, NCOL, WIC, HIC, JAHOOG) ! grafische tekst, grafische posities, met kleurblokjes ERONDER
     use unstruc_colors
     implicit none
     double precision :: hic, WIC
     integer :: jahoog
     integer :: ncol
     integer :: ndraw
     double precision :: x
     double precision :: xa
     double precision :: xb
     double precision :: xp
     double precision :: y
     double precision :: ya
     double precision :: yb
     double precision :: yp
     character TEX * (*)
     common / DRAWTHIS / ndraw(50)

     call SETCOL(KLTEX)
     call DRAWTEXT(real(X), real(Y), TEX)
     call GETPOS(XP, YP)

     XA = XP + 0.3d0 * WIC
     YA = YP - 0.8d0 * HIC + JAHOOG * HIC
     XB = XA + 1.3d0 * WIC
     YB = YA + 0.7d0 * HIC

     if (NCOL /= 0) then
        call SETCOL(NCOL)
        if (JAHOOG == 0) then
           call FBOXnop(XA, YA, XB, YB)
           call SETCOL(KLTEX)
           call BOXnop(XA, YA, XB, YB)
        else
           call FBOXnop(XA, YA, XB, YB)
        end if
     end if
     return
  end
