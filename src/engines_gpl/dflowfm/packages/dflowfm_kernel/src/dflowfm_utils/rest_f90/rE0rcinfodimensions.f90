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

      subroutine RE0RCINFODIMENSIONS(MINP, MMAX, NMAX, DX, X0, Y0)
         implicit none
         double precision :: dx
         integer :: ja
         integer :: larc
         integer :: marc
         integer :: mc
         integer :: minp
         integer :: mmax
         integer :: nc
         integer :: nmax
         double precision :: rmis
         double precision :: x0
         double precision :: y0
         character REC * 132, FILENAME * 80

         rewind (MINP)
         call ZOEKJA(MINP, REC, 'ARC-INFO', JA)
         if (JA == 1) then
            LARC = index(REC, 'ARC')
            read (REC(LARC + 9:), '(A)', ERR=888) FILENAME
            call OLDFIL(MARC, FILENAME)
            call READARCINFOHEADER(MARC, MC, NC, X0, Y0, DX, dx, RMIS)
            call doclose(MARC)
            MMAX = MC
            NMAX = NC
            call MESSAGE('MAIN DIMENSIONS PLUS GRID PARAMETERS HAVE', &
                         'BEEN READ FROM ARC-INFO FILE:', FILENAME)
         else
            call ERROR('NEITHER MAIN DIMENSIONS NOR ARC-INFO FILE FOUND,', &
                       'SPECIFY MAIN DIMENSIONS BY KEYWORD:', &
                       'MAIN DIMENSIONS OR BY ARC-INFO FILE')
         end if
         return

888      call ERROR('LOOKING FOR ARC-INFO FILENAME, BUT GETTING:', REC, ' ')
      end
