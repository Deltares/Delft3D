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

      SUBROUTINE REANET(filename, k0, L0, NUMKN, NUMLN, istat)
      use m_netw
      use gridoperations
      implicit none

      character(len=*), intent(in)     :: filename  !< inderdaad, filename
      INTEGER                          :: k0, L0, NUMKN, NUMLN, istat
 
      integer :: JADOORLADEN,  MNET, JA, LMOD, KMOD
      integer :: i, mout,  k, nr, knread, L, N1 
      integer :: numbersonline
      double precision :: af
      CHARACTER REC*332 
 
      call oldfil(mnet, filename)

      JA = 1
      READ(MNET,'(A)',end = 777, err = 707) REC
      N1 = INDEX(REC,'=') + 1
      READ(REC(N1:),*, end = 555, err = 555) NUMKN

      READ(MNET,'(A)') REC
      N1 = INDEX(REC,'=') + 1
      READ(REC(N1:),*, end = 444, err = 444) NUMLN
      READ(MNET,'(A)') REC

      call readyy('reanet',0d0)

      CALL INCREASENETW(K0+NUMKN, L0 + NUMLN)

      call readyy('reanet',0.05d0)

      KMOD = MAX(1,NUMK/100)
      DO K = K0+1, K0+NUMKN
         if (mod(k,KMOD) == 0) then
            af = 0.05d0 + 0.45d0*dble(k-1-K0)/dble(numkn)
            call readyy('reanet',af)
         endif
         READ(MNET,'(A)',err=888, END = 777) REC
         nr = numbersonline(rec)
         if (nr == 3) then
            READ(REC,*,ERR = 999) XK(K), YK(K), ZK(K)
         else
            READ(REC,*,ERR = 999) XK(K), YK(K)
            ZK(K) = ZKUNI
         endif
      ENDDO

      READ(MNET,*) rec

      LMOD = MAX(1,NUMLn/1000)
      DO L = L0+1, L0+NUMLN
         if (mod(l,LMOD) == 0) then
            af = 0.5d0 + 0.5d0*dble(l-1)/dble(numln)
            call readyy('reanet',af)
         endif
         READ(MNET,'(A)',END = 777) REC
        
         nr = numbersonline(rec)
         if (nr == 3) then
            READ(REC,*,ERR = 888) KN(1,L), KN(2,L), KNREAD
         else
            READ(REC,*,ERR = 888) KN(1,L), KN(2,L)
            KNREAD = 2
         endif
         KN(1,L) = KN(1,L) + K0
         KN(2,L) = KN(2,L) + K0
         ! IF (KNREAD .NE. 1) KNREAD = 2
         KN(3,L) = KNREAD
      ENDDO

  666 continue 
      istat = 0
      CALL DOCLOSE(MNET)

      call readyy('reanet',-1d0)

      netstat = NETSTAT_CELLS_DIRTY

      xkmin = minval(xk(1:numk))
      xkmax = maxval(xk(1:numk))

      RETURN

  999 CALL QNREADERROR('READING NETNODES, BUT GETTING ', REC, MNET)
      RETURN

  888 CALL QNREADERROR('READING NETLINKS, BUT GETTING ', REC, MNET)
      RETURN

  707 CALL QNREADERROR('READING NET FILE, GOT UNEXPECTED CONTENT ', REC, MNET)
      RETURN

  777 CALL QNEOFERROR(MNET)
      RETURN

  555 CALL QNREADERROR('READING NR OF NETNODES, BUT GETTING ', REC, MNET)
      RETURN

  444 CALL QNREADERROR('READING NR OF NETLINKS, BUT GETTING ', REC, MNET)
      RETURN

      END SUBROUTINE REANET
