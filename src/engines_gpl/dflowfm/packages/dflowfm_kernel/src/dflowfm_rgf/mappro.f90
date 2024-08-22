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

      subroutine MAPPRO(XX, YY, XG, YG, IZONE, NZONE, IHEM, ITYPE, JSFERIC, INIA)
         use M_MISSING
         !use proj4
         implicit none
         integer :: ierr
         integer :: ini
         integer :: inia
         integer :: itype
         integer :: izone
         integer :: jsferic
         integer :: nzone
         integer :: ihem
         double precision :: XX, YY, XG, YG

         data INI/0/
         if (INI == 0) then
            call SETELLIPS(3) ! WGS84
            INI = 1
!         ierr = pjf90_init_plus(proj_latlon, &
!            '+proj=latlong +datum=WGS84')
!         ierr = pjf90_init_plus(proj_magsirwest, &
!            '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-77.0775079166666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ')
!         ierr = pjf90_init_plus(proj_utm31,
!     +   '+proj=utm +zone=31 +datum=WGS84')
!         ierr = pjf90_init_plus(proj_rdnew,
!     +  '+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889'
!     +//' +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m'
!     +//' +no_defs')
!         allocate(xp(1),yp(1))
         end if

         XG = DXYMIS
         YG = DXYMIS
         if (JSFERIC == 0) then ! Cartesisch => Spherisch
            if (ITYPE == 0) then ! except for itype = 0
               call TRAROT(XX, YY, XG, YG)
            else if (ITYPE == 1) then ! UTM
!            xp(1) = xx
!            yp(1) = yy
!            ierr = pjf90_transform(proj_magsirwest,proj_latlon, 1, 1, xp, yp, null())
!            xg = xp(1)*RAD_TO_DEG
!            yg = yp(1)*RAD_TO_DEG
               !CALL UTMGEO(XX,YY,XG,YG,IZONE,IERR) ! IZONE = input !TMP disable
               call UTMGEO2(XX, YY, XG, YG, IZONE, IHEM) ! IZONE = input !TMP disable
            else if (ITYPE == 2) then ! Amersfoorts
               call RDGEO(XX, YY, XG, YG, 0)
            else if (ITYPE == 3) then ! RD (Ofwel Parijs)
               call RDGEO(XX, YY, XG, YG, 1)
            else if (ITYPE == 4) then ! MERCATOR
               call MERCGEO(XX, YY, XG, YG)
            else if (ITYPE == -1) then ! AFFINE
               call AFFINE(XX, YY, XG, YG, INIA)
            end if
         else if (JSFERIC == 1) then ! Spherisch  => Cartesisch
            if (ITYPE == 1) then ! UTM
               call GEOUTM(XX, YY, XG, YG, IZONE, NZONE, IERR) ! IZONE = output
            else if (ITYPE == 2) then ! Amersfoorts
               call GEORD(XX, YY, XG, YG, 0)
            else if (ITYPE == 3) then ! RD (Ofwel Parijs)
               call GEORD(XX, YY, XG, YG, 1)
            else if (ITYPE == 4) then ! MERCATOR
               call GEOMERC(XX, YY, XG, YG)
            else if (ITYPE == -1) then ! AFFINE
               call AFFINE(XX, YY, XG, YG, INIA)
            end if
         end if
         return
      end subroutine MAPPRO
