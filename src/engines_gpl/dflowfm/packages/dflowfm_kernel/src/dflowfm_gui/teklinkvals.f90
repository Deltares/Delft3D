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

  SUBROUTINE TEKLINKVALS(MET)
  USE m_missing
  use m_netw
  use unstruc_colors, only: ncolhl
  use geometry_module, only: getdx, getdy, getdxdy
  use m_sferic, only: jsferic
  use gridoperations

  implicit none
  integer :: MET

  double precision :: d
  integer :: jav
  integer :: jview
  integer :: k1
  integer :: k2
  integer :: l
  integer :: ncol, key
  double precision :: rd
  double precision :: vv
  double precision :: xyz

  DOUBLE PRECISION XD,YD,ZD,DX,DY,DZ,XX1,YY1,ZZ1,XX2,YY2,ZZ2,X3,Y3,Z3
  double precision :: X(4), Y(4), Z(4), GETRCIR
  logical :: invnod


  integer :: NCOLS,NV,NIS,NIE,JAAUTO
  double precision :: VMAX,VMIN,DV,VAL
  COMMON /DEPMAX2/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO



  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4

  D = 0.5D0*GETRCIR()  !
  IF (MET .GE. 3) THEN
     LMOD = MAX(1,NUML/100)
     DO L  = 1,NUML
        IF (MOD(L,LMOD) .EQ. 0) THEN
            CALL HALT2(KEY)
            IF (KEY .EQ. 1) then
               RETURN
            end if
        ENDIF
        VV = RLIN(L)
        IF (VV .NE. dmiss) THEN
           K1 = KN(1,L)
           K2 = KN(2,L)
           IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
              IF (.NOT. INVNOD(K1) .and. .NOT. INVNOD(K2) )  CYCLE
              XX1 = XK(K1)
              YY1 = YK(K1)
              ZZ1 = ZK(K1)
              XX2 = XK(K2)
              YY2 = YK(K2)
              ZZ2 = ZK(K2)
              CALL ISOCOL2(VV,NCOL)
              IF (MET .EQ. 3 .OR. MET .EQ. 6) THEN
                 CALL DMOVABS(XX1,YY1,ZZ1)
                 CALL DLNABS(XX2,YY2,ZZ2)
              ELSE IF (MET .EQ. 4 .OR. MET .EQ. 7) THEN
                 !XD = getdx (XX1, yy1, xx2, yy2)
                 !YD = getdy (XX1, yy1, xx2, yy2)
                 call getdxdy(XX1, yy1, xx2, yy2, xd, yd, jsferic)
                 RD = sqrt(xd*xd + yd*yd)
                 IF (RD .NE. 0) THEN
                    IF (JVIEW .EQ. 1 .OR. JVIEW .EQ. 4) THEN
                       DX = -D*YD/RD
                       DY = D*XD/RD
                       DZ = 0
                    ELSE IF (JVIEW .EQ. 2) THEN
                       DZ = -D*YD/RD
                       DY = D*ZD/RD
                       DX = 0
                    ELSE IF (JVIEW .EQ. 3) THEN
                       DX = -D*ZD/RD
                       DZ = D*XD/RD
                       DY = 0
                    ENDIF
                    CALL DRIETWEE(XX2+DX,YY2+DY,ZZ2+DZ,X(1),Y(1),Z(1))
                    CALL DRIETWEE(XX1+DX,YY1+DY,ZZ1+DZ,X(2),Y(2),Z(2))
                    CALL DRIETWEE(XX1-DX,YY1-DY,ZZ1-DZ,X(3),Y(3),Z(3))
                    CALL DRIETWEE(XX2-DX,YY2-DY,ZZ2-DZ,X(4),Y(4),Z(4))
                    CALL pfiller(X,Y,4,ncol,ncol)
!                    CALL IGRJOIN(real(x(1)),real(y(1)),real(x(2)),real(y(2)))
                    call movabs(x(1),y(1))
                    call lnabs(x(2),y(2))
                 ENDIF
              ELSE IF (MET .EQ. 5 .OR. MET .EQ. 8) THEN
                 X3 = 0.5d0*(XX1+XX2)
                 Y3 = 0.5d0*(YY1+YY2)
                 Z3 = 0.5d0*(ZZ1+ZZ2)
                 CALL DRCIRC(X3,Y3,Z3)
              ELSE IF (MET == 9) THEN
                 IF (VV .NE. dmiss .and. VV < vmin + 0.05d0*(vmax-vmin)) THEN
                     X3 = 0.5d0*(XX1+XX2)
                     Y3 = 0.5d0*(YY1+YY2)
                     Z3 = 0.5d0*(ZZ1+ZZ2)
                     CALL CIRR(X3,Y3,ncolhl)
                 endif
              ELSE IF (MET == 10) THEN
                 IF (VV .NE. dmiss .and. VV > vmax - 0.05d0*(vmax-vmin)) THEN
                     X3 = 0.5d0*(XX1+XX2)
                     Y3 = 0.5d0*(YY1+YY2)
                     Z3 = 0.5d0*(ZZ1+ZZ2)
                     CALL CIRR(X3,Y3,ncolhl)
                 endif
              ENDIF
           ENDIF
        ENDIF
     ENDDO

  ENDIF

  RETURN
  END SUBROUTINE TEKLINKVALS
