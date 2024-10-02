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

     subroutine delgrd(KEY, JASAVE, jadelpol)
!    delete grid
        use m_confrm
        use m_grid
        use m_missing
        use m_polygon, only: NPL, xpl, ypl, zpl
        use geometry_module, only: dbpinpol
        use m_delpol

        implicit none
        integer :: inhul, ja, i, j
        integer, intent(in) :: jasave, jadelpol
        integer, intent(inout) :: key

        inhul = -1

        if (JASAVE == 1) call SAVEgrd()
        KEY = 3
        if (NPL <= 2) then
           if (NPL >= 1) then
              call CONFRM('NO POLYON, SO DELETE all GRID POINTS ? ', JA)
              if (JA == 0) then
                 KEY = 0
              else
                 XC = 0d0; YC = 0d0; MC = 0; NC = 0
              end if
           else
              XC = 0d0; YC = 0d0; MC = 0; NC = 0
           end if
           return
        end if

        do I = 1, MC
           do J = 1, NC
              if (Xc(I, J) /= DXYMIS) then
                 call dbpinpol(Xc(i, j), yc(i, j), INHUL, dmiss, JINS, NPL, xpl, ypl, zpl)
                 if (INHUL == 1) Xc(I, J) = XYMIS
              end if

           end do
        end do

!      CALL ADJUST(X, Y, MC, NC, WW1, WW2)
        if (jadelpol == 1) call delpol()
        return
     end subroutine delgrd
