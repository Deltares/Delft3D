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

      subroutine SETWYnew(X, Y, DY)
!     Set zoomwindow limits at proper aspect ratio
         use m_wearelt
         use m_sferic
         use m_sferzoom
         use unstruc_display
         !COMMON /WEARELT/  XMIN,YMIN,XMAX,YMAX,X1,Y1,X2,Y2,RCIR,CR,DSIX
         !COMMON /SFERIC/   JSFERIC, JSFERTEK
         !COMMON /SFERZOOM/ X0,Y0,FAC,X1W,Y1W,X2W,Y2W  ! GRADEN
         !COMMON /MFILES/   MDIA,MINI,MFRM,MRRR,MHLP
         !real(dp) X0,Y0,FAC,X1W,Y1W,X2W,Y2W
         ! X1W = Links, X2W = Rechts, Y1W = Onder, Y2W = Boven v/h Scherm
         implicit none
         double precision :: asp, x, y, dy, dx, XA, Y1A, y2a

         FAC = 1
         call INQASP(ASP)
         DY = max(DY, 1d-8)
         dyh = dy

         if (JSFERTEK >= 1) then
            DY = min(DY, 180d0)
            X = max(-360.0d0, min(X, 360.0d0))
            Y = max(-89.9d0, min(Y, 89.9d0))
         end if

         Y0 = Y
         X0 = X

         Y1 = Y - DY / 2
         Y2 = Y + DY / 2

         if (JSFERTEK >= 1) then
            FAC = 1d0
            call dPROJECT(X, Y1, XA, Y1A, 1)
            call dPROJECT(X, Y2, XA, Y2A, 1)
            if (Y2 - Y1 > 1e-10) FAC = (Y2 - Y1) / (Y2A - Y1A)
         end if

         DX = DY / ASP
         X1 = X - DX / 2
         X2 = X + DX / 2

         X1W = X1
         Y1W = Y1
         X2W = X2
         Y2W = Y2

         if (JSFERTEK >= 1) then
            X1 = X1 - X0 ! SCHERMPJE ROND 0,0
            X2 = X2 - X0
            Y1 = Y1 - Y0
            Y2 = Y2 - Y0
         end if

         call SETWOR(X1, Y1, X2, Y2)

         RCIR = CR * dx
         DSIX = dx / 6
         call XYDISFORMAT()
         return
      end
