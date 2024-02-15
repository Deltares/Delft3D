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

  SUBROUTINE ISOSCALE()  !   COPY OF ISOSCALE, DIRTY BUT QUICK
  use unstruc_colors
  use M_isoscaleunit
  use m_flowgeom, only: ndx
  use m_netw,     only: nump, numk
  use m_polygon,  only: npl
  use unstruc_display

  implicit none
  double precision :: dv
  double precision :: dx
  double precision :: dxshow
  double precision :: dy
  double precision :: hic
  integer :: i, j, ihcopts, jaauto, ncols, ndec, ndraw, nhcdev, nie, nis, numhcopts, nv, nvec
  integer :: INC

  double precision :: rmiss
  double precision :: scalesize
  double precision :: val
  double precision :: vfac
  double precision :: vfacforce
  double precision :: vmax
  double precision :: vmin
  double precision :: wi
  double precision :: wic
  double precision :: x0
  double precision :: xd
  double precision :: xleg
  double precision :: xsc
  double precision :: xsc0
  double precision :: xsc1
  double precision :: xsc2
  double precision :: y0
  double precision :: yleg

  double precision :: ysc
  double precision :: ysc1
  double precision :: ysc2

  COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
  COMMON /HARDCOPY/ NHCDEV,NUMHCOPTS,IHCOPTS(2,20)
  COMMON /DRAWTHIS/  ndraw(50)
  COMMON /SCALEPOS/ XSC,YSC,SCALESIZE,NDEC
  COMMON /VFAC/     VFAC,VFACFORCE,NVEC
  CHARACTER TEXT2*10, FMT*7
  CHARACTER (LEN=8)  :: TEX
  CHARACTER (LEN=17) :: MINTEX, MAXTEX
  REAL INFOGRAPHICS

  IF (NDRAW(12) == 2 .OR. NDRAW(12) == 4) RETURN

  IF (NDRAW(8) .LE. 1 .and. NDRAW(28) .le. 1 .and. ndrawpol .le. 2 ) return

  if ( max(ndx,nump,npl,numk) == 0) return

  CALL IGRCHARSIZE(real(SCALESIZE),real(SCALESIZE))
  WIC = dble(INFOGRAPHICS(3))
  HIC = dble(INFOGRAPHICS(4))

  INC = NV/30 + 1 ! Max 30 color boxes, otherwise increment > 1

  WI  = 11*WIC + 1.8d0*HIC
  XSC0 = 1-XSC
  IF (XSC0 .LT. 0.6d0) THEN
     XSC1 = X1 + XSC0*(X2-X1)
  ELSE
     XSC1 = X2 - (1-XSC0)*(X2-X1) - WI
  ENDIF
  XSC2 = XSC1 + WI
  YSC1 = Y1 + YSC*(Y2-Y1)

  MINTEX = 'MN=  '
  MAXTEX = 'MX=  '
  WRITE(MINTEX(4:15),'(E11.4)') VMIN
  WRITE(MAXTEX(4:15),'(E11.4)') VMAX

  IF (VMAX .GT. VMIN .AND. NDRAW(19) .GE. 2) THEN
     YSC2 = MIN(YSC1 + (NV/INC+1d0)*HIC + 2.5d0*HIC,Y2)
  ELSE
     YSC2 = MIN(YSC1 + (   1d0)*HIC + 3.5d0*HIC,Y2)
     XSC2 = XSC2 + 2*WIC
  ENDIF

  CALL SETCOL(KLSCL)
  CALL FBOXNOP(XSC1,YSC1,XSC2,YSC2)

  CALL SETCOL(KLTEX)
  CALL BOXNOP(XSC1,YSC1,XSC2,YSC2)

  CALL IGRCHARJUSTIFY('L')

  CALL GTEXT(PARAMTEX(1),XSC1+WIC,YSC2-1*HIC,KLTEX)
  CALL GTEXT(UNIT(1)    ,XSC1+WIC,YSC2-2*HIC,KLTEX)

  IF (VMAX .GT. VMIN .AND. NDRAW(19) .GE. 2) THEN
     IF ( ABS(VMIN) .GT. ABS(VMAX) ) THEN
        CALL DISPFORMscale(VMIN,FMT,NDEC)
     ELSE
        CALL DISPFORMscale(VMAX,FMT,NDEC)
     ENDIF

     XLEG = XSC1 + WIC
     J = 1
     DO I = 1,NV,INC
        YLEG = YSC1 + J*HIC
        WRITE(TEXT2(1:10),FMT) VAL(I)
        CALL JGTEXT (TEXT2,XLEG,YLEG,NCOLS(I),WIC,HIC,0)
        J = J+1
     ENDDO
     TEXT2 = '          '
     CALL JGTEXT (TEXT2,XLEG,YLEG+HIC,NCOLS(NV+1),WIC,HIC,0)
  ELSE
     CALL GTEXT(MAXTEX,XSC1+WIC,YSC2-3*HIC,KLTEX)
     CALL GTEXT(MINTEX,XSC1+WIC,YSC2-4*HIC,KLTEX)
  ENDIF

  RETURN
  END
