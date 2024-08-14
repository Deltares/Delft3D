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

      subroutine MAPPROJECTIONS(IT, JA)
         use M_MAPPROPARAMETERS
         use M_SFERIC
         use M_MISSING
         use m_netw
         use M_GRID
         use M_LANDBOUNDARY
         use M_POLYGON
         use M_XYTEXTS
         use M_SAMPLES
         use M_SPLINES
         implicit none
         integer :: i
         integer :: ini
         integer :: it
         integer :: j
         integer :: ja
         integer :: k
         double precision :: XG, YG

         INI = 1
         DELTX = 0d0
         DELTY = 0d0
         FI = 0d0
         XF = 1d0
         YF = 1d0
         ! IZONE  =  UTMZONE DIE JE WIL, NZONE = ADVIESZONE
         ! ITYPE  = 1  ! 0 = ROTATIE/TRANSLATIE, 1 = UTM, 2=RD, 3 = PARIJS, 5 = AFFINE

10       if (IT == -1) then
            call CONVERPARAMETERS(JA)
         else
            JA = 1
         end if
         if (JA == 1) then
            if (ITYPE == 0 .and. JSFERIC == 1) then
               call QNERROR('Spherical Coordinates Should Not Be', 'Scaled, Translated or Rotated', ' ')
               return
            end if
            if (ITYPE == 1 .and. JSFERIC == 0 .and. IZONE == 0) then
               call QNERROR('Please Specify a Valid ZONE Nr', 'in the range 1-60', ' ')
               goto 10
            end if

            do K = 1, NUMK
               if (XK(K) /= DXYMIS) then
                  call MAPPRO(XK(K), YK(K), XG, YG, IZONE, NZONE, IHEM, ITYPE, JSFERIC, INI)
                  if (XG /= DXYMIS) then
                     XK(K) = XG
                     YK(K) = YG
                  else
                     XK(K) = DXYMIS
                     YK(K) = DXYMIS
                  end if
               end if
            end do

            do I = 1, MC
               do J = 1, NC
                  if (XC(I, J) /= DXYMIS) then
                     call MAPPRO(XC(I, J), YC(I, J), XG, YG, IZONE, NZONE, IHEM, ITYPE, JSFERIC, INI)
                     if (XG /= DXYMIS) then
                        XC(I, J) = XG
                        YC(I, J) = YG
                     else
                        XC(I, J) = DXYMIS
                        YC(I, J) = DXYMIS
                     end if
                  end if
               end do
            end do
            do K = 1, MXLAN
               if (XLAN(K) /= DXYMIS) then
                  call MAPPRO(XLAN(K), YLAN(K), XG, YG, IZONE, NZONE, IHEM, ITYPE, JSFERIC, INI)
                  if (XG /= DXYMIS) then
                     XLAN(K) = XG
                     YLAN(K) = YG
                  else
                     XLAN(K) = DXYMIS
                     YLAN(K) = DXYMIS
                  end if
               end if
            end do

            do K = 1, NPL
               if (XPL(K) /= XYMIS) then
                  call MAPPRO(XPL(K), YPL(K), XG, YG, IZONE, NZONE, IHEM, ITYPE, JSFERIC, INI)
                  if (XG /= DXYMIS) then
                     XPL(K) = XG
                     YPL(K) = YG
                  else
                     XPL(K) = XYMIS
                     YPL(K) = XYMIS
                  end if
               end if
            end do

            do K = 1, NTXT
               if (XTXT(K) /= XYMIS) then
                  call MAPPRO(XTXT(K), YTXT(K), XG, YG, IZONE, NZONE, IHEM, ITYPE, JSFERIC, INI)
                  if (XG /= DXYMIS) then
                     XTXT(K) = XG
                     YTXT(K) = YG
                  else
                     XTXT(K) = XYMIS
                     YTXT(K) = XYMIS
                  end if
               end if
            end do

            do K = 1, NS
               if (XS(K) /= XYMIS) then
                  call MAPPRO(XS(K), YS(K), XG, YG, IZONE, NZONE, IHEM, ITYPE, JSFERIC, INI)
                  if (XG /= DXYMIS) then
                     XS(K) = XG
                     YS(K) = YG
                  else
                     XS(K) = XYMIS
                     YS(K) = XYMIS
                  end if
               end if
            end do

            do J = 1, maxsplen
               do I = 1, MCs
                  if (XSP(I, J) /= DXYMIS) then
                     call MAPPRO(XSp(I, J), YSp(I, J), XG, YG, IZONE, NZONE, IHEM, ITYPE, JSFERIC, INI)
                     if (XG /= DXYMIS) then
                        XSP(I, J) = XG
                        YSP(I, J) = YG
                     else
                        XSP(I, J) = XYMIS
                        YSP(I, J) = XYMIS
                     end if
                  end if
               end do
            end do

!        IF (ITYPE .EQ. 1) THEN
!           IF (IZONE .EQ. 0) IZONE = NZONE   ! if initialised as zero
!        ENDIF

            if (ITYPE >= 1) then
               JSFERIC = 1 - JSFERIC
               JSFERTEK = JSFERIC
            end if
         end if

         return
      end subroutine MAPPROJECTIONS
