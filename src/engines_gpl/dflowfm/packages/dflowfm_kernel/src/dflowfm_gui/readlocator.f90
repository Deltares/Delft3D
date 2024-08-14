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

      subroutine READLOCATOR(X, Y, KEY)
         use m_wearelt
         use m_devices
         use m_partitioninfo
         implicit none
         double precision :: dpx
         double precision, save :: f = 1d0
         integer :: ini, jashow, jmouse, key, ixp, iyp
         integer, save :: keyold = 0
         real :: xloc, yloc
         double precision :: x, y
         double precision :: xa, ya, xlc, ylc
         common / LOCATORA / XLC, YLC, XA, YA, JMOUSE, JASHOW

         real, external :: INFOGRAPHICS

         DPX = (X2 - X1) / NPX
         call IMOUSECURSORSHAPE(1, 'G')
         call IMouseCursorShow()
         INI = KEY

10       continue

         if (NOPSYS == 1) then
!        CALL InKeyEventIMM(KEY)
            call InKeyEvent(KEY)
         else
            if (jampi == 0) then
               call InKeyEvent(KEY)
            else
               call InKeyEventIMM(KEY)
!           reduce key
!            call reduce_key(key)
            end if
         end if

         if (KEY == -999) then
!        er gebeurt helemaal niets
            goto 10
!      ELSE IF (KEY .GE. 128 .AND. KEY .LE. 131) THEN
!        pijltjesbeweging
            if (KEYOLD /= KEY) then
               F = 1
            end if
            KEYOLD = KEY
            F = F * 1.08d0
            F = min(F, 10d0)
            if (KEY == 128) then
               YLC = YLC + DPX * F
            else if (KEY == 129) then
               YLC = YLC - DPX * F
            else if (KEY == 130) then
               XLC = XLC + DPX * F
            else if (KEY == 131) then
               XLC = XLC - DPX * F
            end if
            call IMOUSECURSORXYG(real(XLC), real(YLC))
            X = XLC
            Y = YLC
            if (INI == 999) then
               KEY = -10
               call IMOUSECURSORHIDE()
               call GIVEKEY(KEY)
               return
            end if
         end if

!     muisbeweging
         Xloc = InfoGraphics(5)
         Yloc = InfoGraphics(6)
         X = dble(xloc)
         y = dble(yloc)

         call IGRUNITSTOPIXELS(Xloc, Yloc, IXP, IYP)
         call dPROJECT(X, Y, XLC, YLC, 2)
         X = XLC
         Y = YLC

!     buiten veld?
         if (INI /= 999) then
            if (IYP > NPY - 15) then
               KEY = 1
               call IMOUSECURSORSHAPE(0, 'G')
               return
            else if (IYP < 15) then
               KEY = 2
               call IMOUSECURSORSHAPE(0, 'G')
               return
            end if
         end if

!     muisbeweging
!      Xloc   = InfoGraphics(5)
!      Yloc   = InfoGraphics(6)
!      X=dble(xloc)
!      y=dble(yloc)
!      y = min(max(y, y1), y2)

!      CALL dPROJECT(X,Y,XLC,YLC,2)
!      X = XLC
!      Y = YLC

!     buiten veld?
!      IF (INI .NE. 999) THEN
!         IF (Y .GT. Y1 + 0.98d0*(Y2-Y1) ) THEN
!            KEY = 1
!            CALL IMOUSECURSORSHAPE(0,'G')
!            RETURN
!         ELSE IF (Y .LT. Y1 + 0.02d0*(Y2-Y1) ) THEN
!            KEY = 2
!            CALL IMOUSECURSORSHAPE(0,'G')
!            RETURN
!         ENDIF
!      ENDIF

!      XLC = X
!      YLC = Y

         if (INI == 999) then
            if (KEY >= 254 .and. KEY <= 257) then
!           zo snel mogelijk lopen, geen keys of display
               KEY = -10
               return
            else
               call DISPOS()
               call DISDIS()
               call GETKEY2(KEY)
               call GIVEKEY(KEY)
               call IMOUSECURSORHIDE()
               return
            end if
         else
            call DISPOS()
            call DISDIS()
            if ((KEY >= 254 .and. KEY <= 257) .or. &
                (KEY >= 128 .and. KEY <= 131)) then
!        IF (KEY .EQ. 257 .OR. KEY .GE. 128 .AND. KEY .LE. 131) THEN
!           zo snel mogelijk lopen
               goto 10
            else
               call GETKEY2(KEY)
               call GIVEKEY(KEY)
               call TIMLIN()
               call IMOUSECURSORHIDE()
            end if
         end if
         return
      end
