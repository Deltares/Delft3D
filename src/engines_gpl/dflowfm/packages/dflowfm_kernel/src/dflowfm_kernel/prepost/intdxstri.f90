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

      subroutine INTDXSTRI(XH, YH, DXS, NPH, JDLA)
         use m_missing
         use m_samples
         use m_sferic, only: jsferic, jasfer3D
         use m_polygon, only: NPL, xpl, ypl, zpl
         use m_ec_basic_interpolation, only: triinterp2
         use fm_external_forcings_data, only: transformcoef

         implicit none
         integer :: nph, jdla
         double precision :: XH(NPH), YH(NPH), DXS(NPH)

         double precision :: dxsav
         integer :: n
         integer :: nn

         DXS = DXYMIS

         call triinterp2(XH, YH, DXS, NPH, JDLA, &
                         XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)

         NN = 0
         do N = 1, NPH
            if (DXS(N) /= DXYMIS) then
               DXSAV = DXSAV + DXS(N); NN = NN + 1
            end if
         end do

         if (NN < NPH) then ! TODO, LINEAR INTER- AND EXTRAPOLATION
            DXSAV = DXSAV / NN
            do N = 1, NPH
               if (DXS(N) == DXYMIS) then
                  DXS(N) = DXSAV
               end if
            end do
         end if

      end subroutine INTDXSTRI
