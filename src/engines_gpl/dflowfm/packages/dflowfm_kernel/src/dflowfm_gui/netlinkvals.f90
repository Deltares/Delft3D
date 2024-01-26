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

  SUBROUTINE NETLINKVALS(MET,NCOL)

  use m_flowgeom, ONLY : XZ, YZ, lne2ln
  use m_missing
  use network_data
  use m_alloc
  use m_flow, only: cftrt
  use geometry_module, only: dbdistance, dcosphi
  use m_sferic, only: jsferic, jasfer3D
  use gridoperations
  use m_statistics

  implicit none

  integer :: MET, NCOL
  integer :: jacftrt
  double precision :: ag
  double precision :: cfl
  double precision :: dv
  double precision :: e0
  double precision :: eal
  double precision :: eps
  double precision :: fsp
  integer :: jaauto
  integer :: k1, k2, L, jaxz, kL, kR
  integer :: ncols
  integer :: nie, nis, nv, i
  double precision :: pi
  double precision :: rd
  double precision :: rek
  double precision :: rho
  double precision :: rhow
  double precision :: sp
  double precision :: uu
  double precision :: v
  double precision :: val
  double precision :: vmax
  double precision :: vmin
  double precision :: VV, WW, X3, Y3, X4, Y4
  double precision :: xd, YD, ZD
  double precision :: areaL, areaR, xc, yc, aa

  double precision, external :: topo_info

  COMMON /CONSTANTS/ E0, RHO, RHOW, CFL, EPS, AG, PI

  COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
  IF (MET .EQ. 1) RETURN

  jaxz = 0
  if (allocated (xz)) then
     jaxz = 1
  end if

  jacftrt = 0
  if (allocated (cftrt)) then
     jacftrt = 1
  end if

! refresh netcell administartion, based on module variable netstat
  if ( netstat /= NETSTAT_OK .and. (met == 4 .or. met == 5 .or. (met >= 7 .and. met <= 9) .or. met >= 12)) then
     call findcells(100)
     call find1dcells()
     netstat = NETSTAT_OK
  endif

  IF (MET .EQ. 15 .or. MET.EQ.16) THEN  ! topology information
     call makenetnodescoding()
  END IF

  if ( numL.gt.size(rlin) ) then
     call realloc(rlin,numL)
  end if

  DO L  = 1,NUML
     V  = dmiss
     K1 = KN(1,L)
     K2 = KN(2,L)
     IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
        IF (MET .EQ. 2) THEN
           V = L
        ELSE IF (MET .EQ. 3) THEN
           CALL DHITEXT(K1,XK(K1),YK(K1),ZK(K1))
           CALL DHITEXT(K2,XK(K2),YK(K2),ZK(K2))
        ELSE IF (MET .EQ. 4) THEN
           IF (NUMP > 0 .and. jaxz == 1 .and. L <= size(LNN)) THEN
              IF (LNN(L) == 2) THEN
                 X3 = XZ(iabs(LNE(1,L)))
                 X4 = XZ(iabs(LNE(2,L)))
                 Y3 = YZ(iabs(LNE(1,L)))
                 Y4 = YZ(iabs(LNE(2,L)))
                 V  = DCOSPHI(XK(K1), YK(K1), XK(K2), YK(K2), X3, Y3, X4, Y4, jsferic, jasfer3D, dxymis)
                 if (v /= dmiss) then
                    v = abs(v)
                 end if
              ENDIF
           ENDIF
        ELSE IF (MET .EQ. 5) THEN
           if ( size(lne2ln) .ge. numl) then
              V = lne2ln(L)
           else
              v = 0
           endif
        ELSE IF (MET .EQ. 6) THEN
           V = LC(L)
        ELSE IF (MET .EQ. 7) THEN
           if (lc(L) > 0) V = netbr(LC(L))%nx
        ELSE IF (MET .GE. 7 .AND. MET .LE. 9) THEN
           XD  = XK(K2) - XK(K1)
           YD  = YK(K2) - YK(K1)
           ZD  = ZK(K2) - ZK(K1)
           RD  = SQRT(XD*XD + YD*YD + ZD*ZD)
           REK = 0 ! ( RD - RL(L) ) / RL(L)
           IF (LC(L) .EQ. 0) REK = MAX(0d0,REK)
           SP  = E0*REK
           FSP = 0 ! SP*EA(L)/1e3  ! spanning in kN
           IF (MET .EQ. 9) THEN
              V = 0d0 ! TODO: AvD: Mag weg
           ENDIF
        ELSE IF (MET .EQ. 10) THEN
           V = DBDISTANCE( XK(K1), YK(K1), XK(K2), YK(K2), jsferic, jasfer3D, dmiss)
        ELSE IF (MET .EQ. 11) THEN
           V = KN(3,L)
        ELSE IF (MET .EQ. 12) THEN
           V = 0
           IF ( L <= SIZE(LNN) ) V = LNN(L)
        ELSE IF (MET .EQ. 13) THEN
           V = 0
           IF (L <= SIZE(LNN)) V = LNE(1,L)
        ELSE IF (MET .EQ. 14) THEN
           V = 0
           IF (L <= SIZE(LNN)) V = LNE(2,L)
        ELSE IF (MET .EQ. 15) THEN  ! topology information
           V = topo_info(L)
        ELSE IF (MET .EQ. 16) THEN  ! area ratio
           if ( lnn(L).lt.2 ) then
              V = dmiss
           else
              kL = lne(1,L)
              kR = lne(2,L)
              call getcellsurface(kL,areaL,xc,yc)
              call getcellsurface(kR,areaR,xc,yc)
              if ( areaL.lt.1d-12 .or. areaR.lt.1d-12 ) cycle
              V = areaR/areaL
              if ( V.lt.1d0 ) V = 1d0/V
           end if
        ELSE IF (MET .EQ. 17) THEN  ! link size criterion
           if ( lnn(L).lt.2 ) then
              V = dmiss
           else
              kL = lne(1,L)
              kR = lne(2,L)
              call getcellsurface(kL,areaL,xc,yc)
              call getcellsurface(kR,areaR,xc,yc)
              if ( areaL.lt.1d-12 .or. areaR.lt.1d-12 ) cycle
              k1 = kn(1,L)
              k2 = kn(2,L)
              aa = dbdistance(xk(k1), yk(k1), xk(k2), yk(k2), jsferic, jasfer3D, dmiss) * dbdistance(xz(kL), yz(kL), xz(kR), yz(kR), jsferic, jasfer3D, dmiss)
              V  = aa  / (areaR+areaL)
           end if
        ELSE IF (MET .EQ. 18) THEN
            if (jacftrt .eq. 1) then
                V = cftrt(L,2)
            else
                V = DBDISTANCE( XK(K1), YK(K1), XK(K2), YK(K2), jsferic, jasfer3D, dmiss)
                if (v > 0) then
                   V = (zk(k2) - zk(k1)) / v
            end if
            end if
        ENDIF
        RLIN(L) = V
     ENDIF
  ENDDO

  if (met == 4) then
     npdf = 20
     if (allocated (xpdf) ) deallocate (xpdf,ypdf) ; allocate( xpdf(npdf), ypdf(npdf) ) ; xpdf = 0d0
     aa = 1d0
     do i = 1,npdf-1
        aa = 0.6666d0*aa
        ypdf(i) = aa
     enddo
     ypdf(npdf) = 0d0
     call makepdf(rlin,numL)
  endif

  RETURN
  END SUBROUTINE NETLINKVALS
