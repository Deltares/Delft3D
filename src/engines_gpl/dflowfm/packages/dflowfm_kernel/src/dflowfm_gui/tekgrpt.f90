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

      SUBROUTINE TEKGRPT(      X,      Y,     mmax, nmax, MC,     NC,  MP,     NP,   NCOL        )
!     TEKEN GRIDLIJNEN UITKOMEND OP DIT PUNT
      use m_missing
      implicit none
      integer :: mmax, nmax, mc, nc, mp, np, ncol
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)

      double precision :: xp, yp
      integer :: mpu, mpd, npu, npd

      CALL SETCOL(NCOL)
      IF (MP .EQ. 0) RETURN
      XP = X(MP,NP)
      YP = Y(MP,NP)
      IF (XP .EQ. 0) RETURN
      MPU = MP + 1
      MPD = MP - 1
      NPU = NP + 1
      NPD = NP - 1
      IF (MPU .LE. MC) THEN
         IF (X(MPU,NP) .NE. XYMIS) THEN
            CALL MOVABS(X(MPU,NP),Y(MPU,NP))
            CALL LNABS(XP,YP)
         ENDIF
      ENDIF
      IF (MPD .GE. 1) THEN
         IF (X(MPD,NP) .NE. XYMIS) THEN
            CALL MOVABS(X(MPD,NP),Y(MPD,NP))
            CALL LNABS(XP,YP)
         ENDIF
      ENDIF
      IF (NPU .LE. NC) THEN
         IF (X(MP,NPU) .NE. XYMIS) THEN
            CALL MOVABS(X(MP,NPU),Y(MP,NPU))
            CALL LNABS(XP,YP)
         ENDIF
      ENDIF
      IF (NPD .GE. 1) THEN
         IF (X(MP,NPD) .NE. XYMIS) THEN
            CALL MOVABS(X(MP,NPD),Y(MP,NPD))
            CALL LNABS(XP,YP)
         ENDIF
      ENDIF
      RETURN
      END
