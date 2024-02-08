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

      SUBROUTINE REAJANET(MNET,JA,JADOORLADEN)
      use m_netw
      use gridoperations

      implicit none
      integer :: MNET, JA, JADOORLADEN

      integer :: k
      integer :: k0
      integer :: l
      integer :: l0
      integer :: n1
      integer :: numkn
      integer :: numln
      double precision :: x10

      CHARACTER REC*3320

      IF (JADOORLADEN .EQ. 0) THEN
         K0 = 0
         L0 = 0
      ELSE
         K0 = NUMK
         L0 = NUML
      ENDIF

      JA = 0
      READ(MNET,'(A)',end = 777) REC   ! COMMENT

      READ(MNET,'(A)',end = 777) REC
      N1 = INDEX(REC,'=') + 1
      READ(REC(N1:),*, err = 555) NUMKN

      READ(MNET,'(A)',end = 777) REC
      N1 = INDEX(REC,'=') + 1
      READ(REC(N1:),*, err = 555) NUMP



      READ(MNET,'(A)',end = 777) REC


      READ(MNET,'(A)',end = 777) REC
      N1 = INDEX(REC,'=') + 1
      READ(REC(N1:),*, err = 555) NUMLN



      READ(MNET,'(A)',end = 777) REC

      READ(MNET,'(A)',end = 777) REC

      READ(MNET,'(A)',end = 777) REC

      DO K = 1,4
         READ(MNET,'(A)',end = 777) REC
      ENDDO

      CALL INCREASENETW(K0+NUMKN, L0 + NUMLN)

      DO K = K0+1, K0+NUMKN
         READ(MNET,'(A)',END = 777) REC
         READ(REC,*,ERR = 999) XK(K), YK(K)
      ENDDO
      !XK   = XK - 270000
      !YK   = YK - 2700000

      NUMK = K0+NUMKN
      KC   = 1

      DO K = 1,NUMP
         READ(MNET,*)
      ENDDO

      DO L = L0+1, L0+NUMLN
         READ(MNET,'(A)',END = 777) REC
         READ(REC,*,ERR = 888) x10, KN(1,L), KN(2,L)
         KN(1,L) = KN(1,L) + K0
         KN(2,L) = KN(2,L) + K0
         KN(3,L) = 2
      ENDDO
      NUML = L0+NUMLN

      CALL SETNODADM(0)

      ja = 1
      return

  999 CALL QNREADERROR('READING NETNODES, BUT GETTING ', REC, MNET)
      RETURN

  888 CALL QNREADERROR('READING NETLINKS, BUT GETTING ', REC, MNET)

  777 CALL QNEOFERROR(MNET)
      RETURN

  555 CALL QNREADERROR('READING NR OF NETNODES, BUT GETTING ', REC, MNET)
      RETURN

  444 CALL QNREADERROR('READING NR OF NETLINKS, BUT GETTING ', REC, MNET)
      RETURN

      END SUBROUTINE REAJANET
