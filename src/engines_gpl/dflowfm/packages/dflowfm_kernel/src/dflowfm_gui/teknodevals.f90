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

  SUBROUTINE TEKNODEVALS(MET)
  USE m_missing
  use m_netw
  use geometry_module, only: getdxdy, getdx, getdy
  use m_sferic, only: jsferic
  use unstruc_colors ! , ONLY :NCOLWARN1, ncolhl
  use unstruc_display
  use gridoperations

  implicit none
  integer :: MET

  double precision :: d
  integer :: jav
  integer :: jview
  integer :: k1, k
  integer :: k2
  integer :: key
  integer :: l
  integer :: n
  integer :: ncol
  double precision :: rd
  double precision :: vv
  double precision :: xyz

  DOUBLE PRECISION XD,YD,ZD,DX,DY,DZ,XX1,YY1,ZZ1,XX2,YY2,ZZ2,X3,Y3,Z3,H
  double precision :: X(4), Y(4), Z(4)
  double precision :: getrcir
  LOGICAL INVNOD, inview

  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4

  integer :: NCOLS,NV,NIS,NIE,JAAUTO
  double precision :: VMAX,VMIN,DV,VAL
  COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

  KMOD = MAX(1,NUMK/100)

  H = 0.5d0

  if (met == 3) then ! smooth iso of netnode stuff based upon netcells

     if ( numk + numl .ne. lasttopology ) THEN  ! coarsening info
         if ( ubound(lnn,1).ne.numL ) then
             call findcells(100)
         endif
     endif

     do k = 1,nump
        IF (MOD(K,KMOD) .EQ. 0) THEN
           CALL HALT2(KEY)
           IF (KEY .EQ. 1) then
              RETURN
           end if
        ENDIF
        IF (inview(xzw(k), yzw(k) )  ) then
           call isosmoothnet(k)
        endif
     enddo

  else IF (MET .Gt. 3) THEN
    D = 0.5D0*GETRCIR()  !
    DO K1 = 1,NUMK
       IF (MOD(K1,KMOD) .EQ. 0) THEN
         CALL HALT2(KEY)
         IF (KEY .EQ. 1) then
            RETURN
         end if
       ENDIF
       IF (.NOT. INVNOD(K1)) CYCLE
       VV = RNOD(K1)
       XX1 = XK(K1)
       YY1 = YK(K1)
       ZZ1 = ZK(K1)
       IF (VV .NE. dmiss) THEN
         CALL ISOCOL(VV,NCOL)
         IF (MET .EQ. 3 .OR. MET .EQ. 4 .OR. &
             MET .EQ. 6 .OR. MET .EQ. 7 ) THEN
            DO N  = 1,NMK(K1)
               L  = NOD(K1)%LIN(N)
               CALL OTHERNODE(K1,L,K2)
               IF (K2 == 0) then
                  CYCLE
               end if

               XX2 = H*(XK(K2)+XX1)
               YY2 = H*(YK(K2)+YY1)
               ZZ2 = H*(ZK(K2)+ZZ1)
               IF (MET .EQ. 6) THEN
                  CALL DMOVABS(XX1,YY1,ZZ1)
                  CALL DLNABS(XX2,YY2,ZZ2)
               ELSE IF (MET .EQ. 4 .OR. MET .EQ. 7) THEN
                  ! XD = getdx (XX1, yy1, xx2, yy2)
                  ! YD = getdy (XX1, yy1, xx2, yy2)
                  call getdxdy(XX1, yy1, xx2, yy2, xd, yd, jsferic)
                  RD = SQRT(XD*XD + YD*YD)
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
                     !CALL IGRJOIN(real(x(1)),real(y(1)),real(x(2)),real(y(2)))
                  ENDIF
               ENDIF
            ENDDO
         ELSE IF (MET .EQ. 5 .OR. MET .EQ. 8) THEN
            CALL DRCIRC(XX1,YY1,ZZ1)
         ELSE IF (MET == 9) THEN
            IF (VV .NE. dmiss .and. VV < vmin + 0.05d0*(vmax-vmin)) THEN
                CALL CIRR(Xx1,Yy1,ncolhl)
            endif
         ELSE IF (MET == 10) THEN
            IF (VV .NE. dmiss .and. VV > vmax - 0.05d0*(vmax-vmin)) THEN
                CALL CIRR(Xx1,Yy1,ncolhl)
            endif
         ENDIF
       ELSE
          CALL CIRR(XX1,YY1,NCOLWARN1)
       ENDIF
    ENDDO
  ENDIF

  RETURN
  END SUBROUTINE TEKNODEVALS
