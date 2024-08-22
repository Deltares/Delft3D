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

      subroutine READARCINFOHEADER(MINP, MMAX, NMAX, X0, Y0, DX, DY, RMIS)
         implicit none
         double precision :: dx, dy
         integer :: jacornerx
         integer :: jacornery
         integer :: minp
         integer :: mmax
         integer :: nmax
         double precision :: rmis
         double precision :: x0
         double precision :: y0
         double precision :: DumX, DumY
         character REC * 132

         DumY = -1d10

10       continue
         read (minp, *, end=100) rec, mmax
         call ilowercase(rec)
         if (index(REC, 'ncol') < 1) goto 101 ! wrong format
         read (minp, *, end=100, err=102) rec, nmax
         read (minp, *, end=100, err=103) rec, x0
         call ilowercase(rec)
         JACORNERX = 0
         if (index(REC, 'cor') >= 1) JACORNERX = 1
         read (minp, *, end=100, err=104) rec, y0
         call ilowercase(rec)
         JACORNERY = 0
         if (index(REC, 'cor') >= 1) JACORNERY = 1
         read (minp, '(A)', end=100) rec
         read (REC(10:), *, ERR=105) DX
         DY = DX
         read (REC(10:), *, end=107) DumX, DumY
         if (DumY > 0) DY = DumY

107      continue

         !READ(MINP,'(A)',END = 100) REC
         !READ(REC(13:),*,ERR = 106) RMIS
         read (minp, *, end=100, err=106) rec, rmis
         if (JACORNERX == 1) X0 = X0 + DX / 2
         if (JACORNERy == 1) Y0 = Y0 + DX / 2
         return
100      continue
         call EOFERROR(MINP)

101      call READERROR('LOOKING FOR NCOLS (ARC-INFO), BUT GETTING', REC, MINP)
102      call READERROR('LOOKING FOR NROWS (ARC-INFO), BUT GETTING', REC, MINP)
103      call READERROR('LOOKING FOR XLLCORNER (ARC-INFO), BUT GETTING', REC, MINP)
104      call READERROR('LOOKING FOR YLLCORNER (ARCINFO), BUT GETTING', REC, MINP)
105      call READERROR('LOOKING FOR CELLSIZE (ARCINFO), BUT GETTING', REC, MINP)
106      call READERROR('LOOKING FOR MISSING VALUE (ARCINFO), BUT GETTING', REC, MINP)
      end
