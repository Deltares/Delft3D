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

  SUBROUTINE ISOSCALE2()  !   tekenen legenda
  use M_isoscaleunit
  use unstruc_display
  use m_samples
  implicit none

  double precision :: dv
  double precision :: dx
  double precision :: dxshow
  double precision :: dy
  double precision :: hic
  integer :: i, j
  integer :: INC
  integer :: ihcopts
  integer :: jaauto
  integer :: ncols
  integer :: ndec
  integer :: ndraw
  integer :: nhcdev
  integer :: nie
  integer :: nis
  integer :: numhcopts
  integer :: nv
  integer :: nvec

  double precision :: rmiss
  double precision :: scalesize
  double precision :: val
  double precision :: vfac
  double precision :: vfac2
  double precision :: vfacforce
  double precision :: vmax
  double precision :: vmin
  double precision :: wi
  double precision :: wic
  double precision :: x0
  double precision :: xd
  double precision :: xleg
  double precision :: xsc
  double precision :: xsc1
  double precision :: xsc2
  double precision :: y0
  double precision :: yleg
  double precision :: ysc
  double precision :: ysc1
  double precision :: ysc2
  double precision :: yt

  COMMON /DEPMAX2/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
  COMMON /HARDCOPY/ NHCDEV,NUMHCOPTS,IHCOPTS(2,20)
  COMMON /DRAWTHIS/  ndraw(50)
  COMMON /SCALEPOS/ XSC,YSC,SCALESIZE,NDEC
  COMMON /VFAC/     VFAC, VFACFORCE,NVEC
  CHARACTER TEXT2*10, FMT*7
  CHARACTER (LEN=8)  :: TEX
  CHARACTER (LEN=16) :: MINTEX, MAXTEX
  REAL INFOGRAPHICS

  IF (NDRAW(12) == 1  .OR. NDRAW(12) == 4) RETURN  ! 1 = isoscale off
  IF (NDRAW(29) <= 1 .AND. NDRAW (7) <= 1) then
     if (ndraw(32) <= 0 .or. NS<1) RETURN  ! 1 = no, which linval
  endif

  CALL IGRCHARSIZE(real(SCALESIZE),real(SCALESIZE))
  WIC = INFOGRAPHICS(3)
  HIC = INFOGRAPHICS(4)

  INC = NV/30 + 1 ! Max 30 color boxes, otherwise increment > 1

  WI  = 10*WIC + 1.8d0*HIC
  IF (XSC .LT. 0.6d0) THEN
     XSC1 = X1 + XSC*(X2-X1)
  ELSE
     XSC1 = X2 - (1-XSC)*(X2-X1) - WI
  ENDIF
  XSC2 = XSC1 + WI
  YSC1 = Y1 + YSC*(Y2-Y1)

  MINTEX = 'MIN:            '
  MAXTEX = 'MAX:            '
  WRITE(MINTEX(6:16),'(E11.5)') VMIN
  WRITE(MAXTEX(6:16),'(E11.5)') VMAX

  IF (VMAX .GT. VMIN .AND. NDRAW(11) .GE. 2) THEN
     YSC2 = MIN(YSC1 + (NV/INC+1d0)*HIC + 2.5d0*HIC,Y2)
  ELSE
     YSC2 = MIN(YSC1 + (   1d0)*HIC + 3.5d0*HIC,Y2)
     XSC2 = XSC2 + 2*WIC
  ENDIF

  CALL SETCOL(KLSCL)
  CALL FBOXnop(XSC1,YSC1,XSC2,YSC2)

  CALL SETCOL(KLTEX)
  CALL BOXnop(XSC1,YSC1,XSC2,YSC2)

  CALL IGRCHARJUSTIFY('L')

  CALL GTEXT(PARAMTEX(2),XSC1+WIC,YSC2-1*HIC,KLTEX)
  CALL GTEXT(UNIT(2)    ,XSC1+WIC,YSC2-2*HIC,KLTEX)

  IF (VMAX .GT. VMIN .AND. NDRAW(11) .GE. 2) THEN
     IF ( ABS(VMIN) .GT. ABS(VMAX) ) THEN
        CALL DISPFORMscale(VMIN,FMT,NDEC)
     ELSE
        CALL DISPFORMscale(VMAX,FMT,NDEC)
     ENDIF

     XLEG = XSC1 + WIC
     J = 1
     DO I = 1,NV, INC
        YLEG = YSC1 + J*HIC
        WRITE(TEXT2(1:10),FMT) real(VAL(I))
        CALL JGTEXT (TEXT2,XLEG,YLEG,NCOLS(I),WIC,HIC,0)
        J = J+1
     ENDDO
     TEXT2 = '          '
     CALL JGTEXT (TEXT2,XLEG,YLEG+HIC,NCOLS(NV+1),WIC,HIC,0)
  ELSE
     CALL GTEXT(MAXTEX,XSC1+WIC,YSC2-3*HIC,KLTEX)
     CALL GTEXT(MINTEX,XSC1+WIC,YSC2-4*HIC,KLTEX)
  ENDIF

  IF (NDRAW(15) .EQ. 11 .OR. NDRAW(15) .EQ. 13 .OR. NDRAW(15) .EQ. 15 .OR. NDRAW(15) .EQ. 16) THEN
      CALL SETCOL(KLSCL)
      YT = YSC1-5*HIC
      CALL FBOXnop(XSC1,YT-4*HIC,XSC2,YT)
      CALL SETCOL(KLTEX)
      CALL BOXnop(XSC1,YT-4*HIC,XSC2,YT)
      VFAC2 = 0.3d0*(XSC2-XSC1)
      CALL SETCOL(KLVEC)
      CALL ARROWS(XSC1+WIC,YT-2*HIC,1d0,0d0,0d0,VFAC2)
      TEX = ' 2.3 m/s'
      ! WRITE(TEX(1:4),'(F4.1)')  VFAC2/(DX*VFAC)
      WRITE(TEX(1:4),'(F4.1)')  real(VFAC2/(VFAC))
      CALL IGRCHARJUSTIFY('R')
      CALL GTEXT(TEX,XSC2-WIC,YT-2*HIC,KLTEX)
  ENDIF

  RETURN
  END
