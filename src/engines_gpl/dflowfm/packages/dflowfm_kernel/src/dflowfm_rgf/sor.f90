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

      SUBROUTINE SOR(A,B,C,D,E,U,RJAC,M1,N1,M2,N2)
      use m_grid
      use m_gridsettings
      use m_orthosettings, only: ITIN
      implicit none
      double precision :: anorm
      double precision :: anormf
      double precision :: half
      integer :: j
      integer :: l
      integer :: m1
      integer :: m2
      integer :: maxits
      integer :: n
      integer :: n1
      integer :: n2
      double precision :: one
      double precision :: qtr
      double precision :: rjac
      double precision :: zero
!     IMPLICIT double precision ::(A-H,O-Z)
      DOUBLE PRECISION :: A(MMAX,NMAX),B(MMAX,NMAX),C(MMAX,NMAX), D(MMAX,NMAX),E(MMAX,NMAX),U(MMAX,NMAX)

      PARAMETER(ZERO=0D0,HALF=.5D0,QTR=.25D0,ONE=1D0)
      DOUBLE PRECISION :: RESID, OMEGA
!     WRITE (MDIA,*) 'MEGS AVAILABLE SOR ', N4*4.096*0.001,
!      (N1+N2)*4.096*0.001d0
      MAXITS=ITIN
      ANORMF=ZERO
      OMEGA =ONE

      DO 15 N=1,MAXITS
        ANORM=ZERO
        DO 14 J=MAX(2,M1),MIN(M2,MC-1)
          DO 13 L=MAX(2,N1),MIN(N2,NC-1)
            IF (IJC (J,L) .EQ. 10) THEN
!              IF(MOD(J+L,2).EQ.MOD(N,2))THEN
                 RESID=A(J,L)*U(J+1,L)+B(J,L)*U(J-1,L)+    &
                     C(J,L)*U(J,L+1)+D(J,L)*U(J,L-1)+ E(J,L)*U(J,L)
                 U(J,L)=U(J,L)-OMEGA*RESID/E(J,L)
!              ENDIF
            ENDIF
13        CONTINUE
14      CONTINUE
        IF(N.EQ.1) THEN
          OMEGA=ONE/(ONE-HALF*RJAC**2)
        ELSE
          OMEGA=ONE/(ONE-QTR*RJAC**2*OMEGA)
        ENDIF
!       write(mdia,*) omega, rjac

15    CONTINUE
      RETURN
      END SUBROUTINE SOR
