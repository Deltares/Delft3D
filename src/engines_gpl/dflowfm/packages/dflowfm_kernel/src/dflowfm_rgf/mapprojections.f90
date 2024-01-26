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

      SUBROUTINE MAPPROJECTIONS(IT,JA)
      USE M_MAPPROPARAMETERS
      USE M_SFERIC
      USE M_MISSING
      use m_netw
      USE M_GRID
      USE M_LANDBOUNDARY
      USE M_POLYGON
      USE M_XYTEXTS
      USE M_SAMPLES
      USE M_SPLINES
      implicit none
      integer :: i
      integer :: ini
      integer :: it
      integer :: j
      integer :: ja
      integer :: k
      DOUBLE PRECISION :: XG, YG

      INI    = 1
      DELTX  = 0d0
      DELTY  = 0d0
      FI     = 0d0
      XF     = 1d0
      YF     = 1d0
      ! IZONE  =  UTMZONE DIE JE WIL, NZONE = ADVIESZONE
      ! ITYPE  = 1  ! 0 = ROTATIE/TRANSLATIE, 1 = UTM, 2=RD, 3 = PARIJS, 5 = AFFINE

   10 IF (IT .EQ. -1) THEN
       CALL CONVERPARAMETERS(JA)
      ELSE
       JA    = 1
      ENDIF
      IF (JA .EQ. 1) THEN
         IF (ITYPE .EQ. 0 .AND. JSFERIC .EQ. 1) THEN
            CALL QNERROR('Spherical Coordinates Should Not Be', 'Scaled, Translated or Rotated',' ')
            RETURN
         ENDIF
         IF (ITYPE .EQ. 1 .AND. JSFERIC .EQ. 0 .AND. IZONE .EQ. 0) THEN
            CALL QNERROR('Please Specify a Valid ZONE Nr', 'in the range 1-60',' '          )
            GOTO 10
         ENDIF


         DO K = 1,NUMK
            IF (XK(K) .NE. DXYMIS) THEN
               CALL MAPPRO (XK(K),YK(K),XG,YG,IZONE,NZONE,IHEM,ITYPE,JSFERIC,INI)
               IF (XG .NE. DXYMIS) THEN
                  XK(K) = XG
                  YK(K) = YG
               ELSE
                  XK(K) = DXYMIS
                  YK(K) = DXYMIS
               ENDIF
             ENDIF
         ENDDO


         DO I = 1,MC
            DO J = 1,NC
               IF (XC(I,J) .NE. DXYMIS) THEN
                  CALL MAPPRO (XC(I,J),YC(I,J),XG,YG,IZONE,NZONE,IHEM,ITYPE,JSFERIC,INI)
                  IF (XG .NE. DXYMIS) THEN
                     XC(I,J) = XG
                     YC(I,J) = YG
                  ELSE
                     XC(I,J) = DXYMIS
                     YC(I,J) = DXYMIS
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
         DO K = 1,MXLAN
            IF (XLAN(K) .NE. DXYMIS) THEN
               CALL MAPPRO (XLAN(K),YLAN(K),XG,YG,IZONE,NZONE,IHEM,ITYPE,JSFERIC,INI)
               IF (XG .NE. DXYMIS) THEN
                  XLAN(K) = XG
                  YLAN(K) = YG
               ELSE
                  XLAN(K) = DXYMIS
                  YLAN(K) = DXYMIS
               ENDIF
            ENDIF
         ENDDO

         DO K = 1,NPL
            IF (XPL(K) .NE. XYMIS) THEN
               CALL MAPPRO (XPL(K),YPL(K),XG,YG,IZONE,NZONE,IHEM,ITYPE,JSFERIC,INI)
               IF (XG .NE. DXYMIS) THEN
                  XPL(K) = XG
                  YPL(K) = YG
               ELSE
                  XPL(K) = XYMIS
                  YPL(K) = XYMIS
               ENDIF
            ENDIF
         ENDDO

         DO K = 1,NTXT
            IF (XTXT(K) .NE. XYMIS) THEN
               CALL MAPPRO (XTXT(K),YTXT(K),XG,YG,IZONE,NZONE,IHEM,ITYPE,JSFERIC,INI)
               IF (XG .NE. DXYMIS) THEN
                  XTXT(K) = XG
                  YTXT(K) = YG
               ELSE
                  XTXT(K) = XYMIS
                  YTXT(K) = XYMIS
               ENDIF
            ENDIF
         ENDDO

         DO K = 1,NS
            IF (XS(K) .NE. XYMIS) THEN
               CALL MAPPRO (XS(K),YS(K),XG,YG,IZONE,NZONE,IHEM,ITYPE,JSFERIC,INI)
               IF (XG .NE. DXYMIS) THEN
                  XS(K) = XG
                  YS(K) = YG
               ELSE
                  XS(K) = XYMIS
                  YS(K) = XYMIS
               ENDIF
            ENDIF
         ENDDO

         DO J = 1,maxsplen
             DO I = 1,MCs
                IF (XSP(I,J) .NE. DXYMIS) THEN
                   CALL MAPPRO (XSp(I,J),YSp(I,J),XG,YG,IZONE,NZONE,IHEM,ITYPE,JSFERIC,INI)
                   IF (XG .NE. DXYMIS) THEN
                       XSP(I,J) = XG
                       YSP(I,J) = YG
                    ELSE
                       XSP(I,J) = XYMIS
                       YSP(I,J) = XYMIS
                    ENDIF
                endif
             ENDDO
         ENDDO


!        IF (ITYPE .EQ. 1) THEN
!           IF (IZONE .EQ. 0) IZONE = NZONE   ! if initialised as zero
!        ENDIF

         IF (ITYPE .GE. 1) THEN
            JSFERIC  = 1 - JSFERIC
            JSFERTEK = JSFERIC
         ENDIF
      ENDIF

      RETURN
      END SUBROUTINE MAPPROJECTIONS
