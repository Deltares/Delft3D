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

  subroutine MINMXNETNODS()
     use m_netw
     use m_missing
     use unstruc_display

     implicit none

     integer :: i, k
     double precision :: rd, rmax, rmin

     double precision :: VMAX, VMIN, DV, VAL
     integer :: NCOLS, NV, NIS, NIE, JAAUTO
     common / DEPMAX / VMAX, VMIN, DV, VAL(256), NCOLS(256), NV, NIS, NIE, JAAUTO
     logical inview

     ! BEPAAL MINIMUM EN MAXIMUM VAN DIEPTES BINNEN VIEWING AREA

     if (JAAUTO > 0) then
        RMIN = 1.0d30
        NODMIN = 0
        RMAX = -1.0d30
        NODMAX = 0
        do K = 1, NUMK
           if (INVIEW(XK(K), YK(K))) then
              RD = RNOD(K)
              if (rd /= dmiss) then
                 if (RD < RMIN) then
                    RMIN = RD
                    NODMIN = K
                 end if
                 if (RD > RMAX) then
                    RMAX = RD
                    NODMAX = K
                 end if
              end if
           end if
        end do
        VMAX = RMAX
        VMIN = RMIN
     end if

     DV = VMAX - VMIN
     do I = 1, NV
        VAL(I) = VMIN + (I - 1) * DV / (NV - 1)
     end do

     return
  end subroutine MINMXNETNODS
