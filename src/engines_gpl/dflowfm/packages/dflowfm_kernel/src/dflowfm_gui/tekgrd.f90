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

      SUBROUTINE TEKgrd(XC, YC, MMAX, NMAX, m1,n1,m2,n2,NCOL,MET,key,MC)
      implicit none
      integer :: mmax, nmax, m1, n1, m2, n2, ncol, met, key, mc
      DOUBLE PRECISION :: XC(MMAX,NMAX), YC(MMAX,NMAX), xlist(nmax), ylist(nmax)

      integer :: i, j, kmax, ja


      IF (MET .EQ. 0 .OR. MC == 0) RETURN
      JA = 0

      CALL SETCOL(NCOL)
      IF (MET .EQ. 2 .OR. MET .EQ. 4) CALL IGRLINETYPE(1)

      KMAX = 8
      DO J = N1,N2
        IF (MOD (J,10) .EQ. 0) CALL HALT2(JA)
        IF (JA .EQ. 1) THEN
           IF (MET .EQ. 2 .OR. MET .EQ. 4) CALL IGRLINETYPE(0)
           RETURN
        ENDIF

        CALL JGRLINE8(Xc(M1,J),Yc(M1,J),M2-M1+1)
      ENDDO

      DO I = M1,M2
        IF (MOD (I,10) .EQ. 0) CALL HALT2(JA)
        IF (JA .EQ. 1) THEN
           IF (MET .EQ. 2 .OR. MET .EQ. 4) CALL IGRLINETYPE(0)
           RETURN
        ENDIF

        xlist(1:N2-N1+1) = xc(i,N1:N2)
        ylist(1:N2-N1+1) = yc(i,N1:N2)
        CALL JGRLINE8(xlist,ylist,N2-N1+1)
      ENDDO

      IF (MET .EQ. 2 .OR. MET .EQ. 4) CALL IGRLINETYPE(0)
      IF (MET .EQ. 5) THEN
         CALL TEKnumnetcells(0)
      ENDIF

      END subroutine tekgrd
