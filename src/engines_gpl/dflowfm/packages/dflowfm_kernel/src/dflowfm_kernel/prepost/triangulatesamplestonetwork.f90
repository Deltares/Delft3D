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

   SUBROUTINE Triangulatesamplestonetwork(JADOORLADEN)
   use m_netw, only : numk, numl, kn, xk, yk, zk, nb, LMAX, KMAX
   USE M_SAMPLES
   use m_ec_triangle
   USE M_ALLOC
   use m_missing, only: dmiss, JINS
   use m_ec_basic_interpolation, only: dlaun
   use geometry_module, only: pinpok, dbpinpol, get_startend
   use gridoperations
   use m_polygon ! , only: savepol, restorepol
   use m_mergenodes
   implicit none
   integer :: jadoorladen ! ,npl
   !double precision :: xpl(npl),ypl(npl)
   double precision :: af
   integer :: in
   integer :: ja
   integer :: k
   integer :: k0
   integer :: k1
   integer :: k1l
   integer :: k2
   integer :: k2l
   integer :: k3
   integer :: ksx
   integer :: l
   integer :: l0
   integer :: ll
   integer :: n
   integer :: n1
   integer :: n2
   integer :: new
   integer :: nn
   integer :: nsdl
   integer :: nsin
   integer :: nmod

   integer :: jstart, jend

   integer :: IERR

   INTEGER, ALLOCATABLE :: KS(:)
   DOUBLE PRECISION     :: XP, YP, THIRD, phimin,phimax

   THIRD = 1D0/3D0

   CALL FINDCELLS(0)

   CALL MAKENETNODESCODING()

   IF (JADOORLADEN .EQ. 0) THEN
      K0 = 0
      L0 = 0
   ELSE
      K0 = NUMK
      L0 = NUML
   ENDIF

   ! CALL SAVEPOL()
   CALL SAVESAM()

   N = 0
   DO K = 1,NS                     ! SELECT SAMPLES IN POLYGON
      IF (NPL .NE. 0) THEN
          CALL PINPOK(XS(K), YS(K), NPL, XPL, YPL, IN, jins, dmiss)
      ELSE
          IN = 1
      ENDIF
      IF (IN == 1) THEN
         N     = N + 1
         XS(N) = XS2(K)
         YS(N) = YS2(K)
         ZS(N) = ZS2(K)
      ENDIF
   ENDDO
   NSIN = N

   CALL INCREASENETW(K0+NSIN,L0+6*NSIN)

   KSX = 6*NSIN + 100000
   ALLOCATE ( KS(KSX) )

   N = 0                           ! ADD SELECTED SAMPLES TO NETWORK + ADMIN NODE NRS
   DO K = K0+1, K0+NSIN
      N     = N + 1
      XK(K) = XS(N)
      YK(K) = YS(N)
      ZK(K) = ZS(N)
      KS(N) = K
   ENDDO

   N = NSIN                        ! ADD NETPOINTS IN ORIGINAL NUMK SET TO SAMPLES
   IF (NPL > 0) THEN
      DO K = 1, K0                 ! NUMK of old net
         IF (NB(K) == 2 .OR. NB(K) == 3) THEN
            N     = N + 1
            CALL INCREASESAM(N)
            XS(N) = XK(K)
            YS(N) = YK(K)
            ZS(N) = ZK(K)
            KS(N) = K
         ENDIF
      ENDDO
   ENDIF

   IF (N < 1 .and. NPL > 0 ) THEN              ! IF THERE AREN'T ANY SAMPLES YET, USE THE POLYGON
      call get_startend(NPL,XPL,YPL,jstart,jend, dmiss)
      NSIN = max(jend-jstart+1,0)
      call increasesam(NSIN)
      CALL INCREASENETW(K0+NSIN,L0+6*NSIN)
      KSX = 6*NSIN + 100000
      call REALLOC( KS, KSX )

      do k=jstart,jend
         N = N+1
         XS(N) = XPL(K)
         YS(N) = YPL(K)
         ! ZS(N) = ZPL(K)

         XK(K) = XS(N)
         YK(K) = YS(N)
         ! ZK(K) = ZS(N)
         KS(N) = K
      end do

   END IF

   NSDL = N

   CALL READYY('TRIANGULATING', 0d0)

   CALL DLAUN(XS,YS,NSDL,3,ierr)

   CALL READYY('TRIANGULATING', 0.3d0)

   IN = -1
   ! Check triangles and disable some links if necessary.
   NMOD = int(NUMTRI/40.0)+1
   DO N = 1,NUMTRI
      if (mod(N,NMOD) == 0) then
        AF = 0.3d0 + 0.4d0*dble(N)/dble(NUMTRI)
        CALL READYY('TRIANGULATING', AF)
      end if

      JA = 1
      CALL CHECKTRIANGLE(N,JA,phimin,phimax)
      ! Mark an edge with minus sign if triangle is correct!
      IF (JA == 0) THEN
         CYCLE
      ENDIF

      K1 = INDX(1,N) ; K2 = INDX(2,N) ; K3 = INDX(3,N)
      K1 = KS (K1)   ; K2 = KS (K2)   ; K3 = KS (K3)
      XP = THIRD*( XK(K1) + XK(K2) + XK(K3) )
      YP = THIRD*( YK(K1) + YK(K2) + YK(K3) )
      CALL DBPINPOL(XP, YP, IN, dmiss, JINS, NPL, xpl, ypl, ypl)
      IF (IN == 0) THEN
         CYCLE
      ELSE
      ! Mark an edge with minus sign if triangle is correct!
         DO NN=1,3
            K1 = TRIEDGE(NN,N)
            EDGEINDX(1,K1) = -ABS(EDGEINDX(1,K1))
         END DO
      ENDIF
   END DO

   ! All triangles were just checked, and for all good ones, their edges
   ! were marked with a minus sign. Add only these to kn array.
   NMOD = int(NUMEDGE/30.0)+1
   L  = L0
   DO LL = 1,NUMEDGE
      if (mod(N,NMOD) == 0) then
        AF = 0.7d0 + 0.3d0*dble(LL)/dble(NUMEDGE)
        CALL READYY('TRIANGULATING', AF)
      end if
    IF (EDGEINDX(1,LL) > 0) then
        CYCLE
    else
        EDGEINDX(1,LL) = abs(EDGEINDX(1,LL))
    end if
    L=L+1
    KN(1,L) = KS(EDGEINDX(1,LL))
    KN(2,L) = KS(EDGEINDX(2,LL))
    KN(3,L) = 2

    call setcol(31)
    call movabs(xk(kn(1,L)),yk(kn(1,L)))
    call lnabs(xk(kn(2,L)),yk(kn(2,L)))

    IF (L > LMAX) THEN
        write (*,*) 'INCREASENETW(KMAX, INT(1.2d0*NUML) )', NUML
       CALL INCREASENETW(KMAX, INT(1.2d0*NUML) )
    ENDIF

   END DO

   CALL READYY('TRIANGULATING', -1d0)

   NUMK = K0 + NSIN
   NUML = L

   ! merge nodes in polygon
   !call mergenodesinpolygon()

   ns  = 0 ! call delsam(1)
   npl = 0 ! call delpol()
   CALL SETNODADM(0) ! No cross checks for now.

   DEALLOCATE (KS,NB)
   IF (ALLOCATED(TRIEDGE) ) THEN
       DEALLOCATE(TRIEDGE, EDGEINDX)
   ENDIF

   RETURN
   END SUBROUTINE Triangulatesamplestonetwork
