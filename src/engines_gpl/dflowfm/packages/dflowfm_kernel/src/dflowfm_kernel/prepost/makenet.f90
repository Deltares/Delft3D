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

   SUBROUTINE MAKENET(japaramscreen)

   use m_netw
   use m_makenet  ! NTYP ANGLE SIZE THICK NRX NRY
   use m_polygon
   use m_grid
   use m_missing
   use m_sferic
   use geometry_module, only: pinpok
   use gridoperations
   use m_flowparameters, only : bedslope
   use m_mergenodes
   
   implicit none

   integer, intent(in) :: japaramscreen !< Load parameter screen or not (1/0)
   double precision :: ael, ag, cfl, cs, dx, dy, e0, eps, hs
   integer          :: in, jn, k0, l0, m, mh, n, nh, nn, numkn, numln, jaklaar, jafive, L, k1, k2, n12, i, k, LL, mou2
   double precision :: siz
   double precision :: sn
   double precision :: xplmax, xplmin, xx, yplmax, yplmin, yy, asp, c, phi, dphi, dxt, rr, f, rl, dd, z2, zbn

   double precision :: X(8), Y(8), Z(8), XD, YD
   character(len = 20) :: fnam
!   COMMON /CONSTANTS/ E0, RHO, RHOW, CFL, EPS, AG, PI


   if (japaramscreen == 1) then
      !ntyp = 7 ; nrx = 32 ; bedslope = 1d-4
      CALL MAKENETPARAMETERS()
   end if


   if (ntyp .le. 5) then

      ! get parameters from polygon if available
      call pol2netparams()

      AEL = PI*THICK*THICK/4  ! RDIAM in mm
      SIZ = SIZE
      HS  = SIZE*0.5d0
      CS  = COS(ANGLE*PI/180.) ; SN = SIN(ANGLE*PI/180.)

      IF (NTYP .LE. 2) THEN
         DX = DX0
         DY = DY0
      ELSE IF (NTYP .LE. 3) THEN
         DX = SIZ*COS(ANGLE*PI/180.)
         DY = SIZ*SIN(ANGLE*PI/180.)
      ELSE IF (NTYP .EQ. 4) THEN
         DX = 0.5d0*SIZ ; DY = DX*SQRT(3d0)
      ELSE IF (NTYP .EQ. 5) THEN
         DX = HS; DY = sqrt(3d0)*DX
      ENDIF


      IF (NPL > 0) THEN
         IF (NRX <= 1) THEN
            NRX = (XPLMAX-XPLMIN)/DX
            NRY = (YPLMAX-YPLMIN)/DY
         ELSE IF (DX == 0) THEN
            DX  = (XPLMAX-XPLMIN)/NRX
            DY  = (YPLMAX-YPLMIN)/NRY
         ENDIF
      ENDIF


      IF (NTYP == 0) THEN
         MC = NRX+1 ; NC = NRY+1
         CALL INCREASEGRID(MC,NC)
         DO N = 1,NC
             DO M = 1,MC
                XC(M,N) = X0 + (M-1)*DX*CS - (N-1)*DY*SN
                YC(M,N) = Y0 + (M-1)*DX*SN + (N-1)*DY*CS
                if (jsferic == 1 .and. n > 1)  then
                   c   = cos(dg2rd*yc(m,n-1))
                   asp = c*1d0 + (1d0-c)*0.3d0
                   dy  = dx*c*asp
                   YC(M,N) = YC(M,N-1) + dy
                endif

             ENDDO
             if (jsferic == 1) then
                if (dy*dg2rd*ra < 20000.d0) then
                    nc = n+1
                    yc(1:mc,nc) = 90d0
                    xc(1:mc,nc) = xc(1:mc,nc-1)
                    exit
                endif
             endif
         ENDDO

         call del_grid_outside_pol()

         ! CALL GRIDTONET()
         ! MC = 0 ; NC = 0; XC = DMISS; YC = DMISS

      ELSE

         K0    = NUMK
         L0    = NUML
         NUMKN = (NRX+1)*(NRY+1)
         NUMLN = 6*NUMKN

         CALL INCREASENETW(K0+NUMKN, L0 + NUMLN)

         call readyy('makenet',0d0)

         Z    = Z0
         DO N = 1,NRY
            call readyy('makenet', dble(n-1)/dble(nry-1) )
            DO M = 1,NRX
               IF (NTYP .EQ. 0) THEN
                  XX   = dble(M-1)*DX0
                  YY   = dble(N-1)*DY0
                  X(1) = X0 + XX*CS - YY*SN                   ; NN = 4
                  Y(1) = Y0 + YY*CS + XX*SN
                  XX   = XX + DX0
                  X(2) = X0 + XX*CS - YY*SN                   ; NN = 4
                  Y(2) = Y0 + YY*CS + XX*SN
                  YY   = YY + DY0
                  X(3) = X0 + XX*CS - YY*SN                   ; NN = 4
                  Y(3) = Y0 + YY*CS + XX*SN
                  XX   = XX - DX0
                  X(4) = X0 + XX*CS - YY*SN                   ; NN = 4
                  Y(4) = Y0 + YY*CS + XX*SN
                  XD   = 0.25d0*(X(1)+X(2)+X(3)+X(4))
                  YD   = 0.25d0*(Y(1)+Y(2)+Y(3)+Y(4))
               ELSE IF (NTYP .EQ. 1) THEN
                  XD   = X0 + DX + dble(M-1)*2*DX              ; NN = 4
                  YD   = Y0 + DY + dble(N-1)*2*DY
                  X(1) = XD      ; Y(1) = YD - DY
                  X(2) = XD + DX ; Y(2) = YD
                  X(3) = XD      ; Y(3) = YD + DY
                  X(4) = XD - DX ; Y(4) = YD
               ELSE IF (NTYP .EQ. 2) THEN
                  JN   = MOD(M-1,2)
                  XD   = X0 + DX + HS    + dble(M-1)*(DX+2*HS) ; NN = 6
                  YD   = Y0 + DY + JN*DY + dble(N-1)*(2*DY)
                  X(1) = XD - HS      ; Y(1) = YD - DY
                  X(2) = XD + HS      ; Y(2) = YD - DY
                  X(3) = XD + HS + DX ; Y(3) = YD
                  X(4) = XD + HS      ; Y(4) = YD + DY
                  X(5) = XD - HS      ; Y(5) = YD + DY
                  X(6) = XD - HS - DX ; Y(6) = YD
               ELSE IF (NTYP .EQ. 3) THEN
                  XD   = X0 + DX + HS + dble(M-1)*2*(DX+HS)   ; NN = 6
                  YD   = Y0 + DY + dble(N-1)*2*DY
                  X(1) = XD - HS      ; Y(1) = YD - DY
                  X(2) = XD + HS      ; Y(2) = YD - DY
                  X(3) = XD + HS + DX ; Y(3) = YD
                  X(4) = XD + HS      ; Y(4) = YD + DY
                  X(5) = XD - HS      ; Y(5) = YD + DY
                  X(6) = XD - HS - DX ; Y(6) = YD
               ELSE IF (NTYP .EQ. 4) THEN
                  XD   = X0 + DX + dble(M-1)*2*DX             ; NN = 6
                  YD   = Y0 + DY + dble(N-1)*2*DY
                  X(1) = XD - DX      ; Y(1) = YD - DY
                  X(2) = XD + DX      ; Y(2) = YD - DY
                  X(3) = XD + DX + DX ; Y(3) = YD
                  X(4) = XD + DX      ; Y(4) = YD + DY
                  X(5) = XD - DX      ; Y(5) = YD + DY
                  X(6) = XD           ; Y(6) = YD
               ELSE IF (NTYP .EQ. 5) THEN
                  mh = nrx/2 ; nh = nry/2
                  JN   = MOD(M-1,2)
                  XD   = X0 + DX - HS    + dble(M-1-mh)*(DX+2*HS) ; NN = 6
                  YD   = Y0 + JN*DY + dble(N-1-nh)*(2*DY) - dy
                  X(1) = XD - HS      ; Y(1) = YD - DY
                  X(2) = XD + HS      ; Y(2) = YD - DY
                  X(3) = XD + HS + DX ; Y(3) = YD
                  X(4) = XD + HS      ; Y(4) = YD + DY
                  X(5) = XD - HS      ; Y(5) = YD + DY
                  X(6) = XD - HS - DX ; Y(6) = YD
              ENDIF
              CALL PINPOK(XD,YD,NPL,XPL,YPL,IN, jins, dmiss)
               IF (IN == 1) THEN
                   CALL ADDMAZE(X,Y,Z,NN,JAFIVE)
               ENDIF
            ENDDO
          ENDDO
      ENDIF

   else IF (NTYP .EQ. 6) THEN

      dx0     = 360d0/nrx
      dy0     = dx0
      JAFIVE  = 0
      jsferic = 1 ; jasfer3D = 1 ; jaklaar = 0 ; z = dmiss
      YY      = 0d0
      dy0     = dx0

      K0    = NUMK
      L0    = NUML
      NUMKN = (NRX+1)*(NRY+1)
      NUMLN = 6*NUMKN
      CALL INCREASENETW(K0+NUMKN, L0 + NUMLN)

      DO N = 1,NRY
         call readyy('makenet', dble(n-1)/dble(nry-1) )

         call getdeltay(yy, dx0, dy0)

         if (yy + 1.5d0*dy0 > 90d0) then
            dy0 = 90d0 - YY ; jaklaar = 1 ; jafive = 0
         else
            if (dy0*dg2rd*ra < dxdouble .and. jafive == 0) then
                dx0 = 2d0*dx0 ; jafive = 1
                call getdeltay(yy, dx0, dy0)
            else
                jafive = 0; n12 = 0
            endif
            if (yy + 1.5d0*dy0 > 90d0) then
               dy0 = 0.51d0*(90d0 - yy)
            endif
         endif

         DO M = 1,NRX
            XX  = dble(M-1)*DX0 + xwleft

            X(1) = XX
            Y(1) = YY

            if (jafive == 0) then
               X(2) = XX + dx0
               Y(2) = YY
               X(3) = XX + dx0
               Y(3) = YY + dy0
               X(4) = XX
               Y(4) = YY + dy0
               NN   = 4
            else
               x(2) = XX + 0.5D0*DX0
               y(2) = YY
               X(3) = XX + dx0
               Y(3) = YY
               X(4) = XX + dx0
               Y(4) = YY + dy0
               X(5) = XX
               Y(5) = YY + dy0
               NN   = 5
            endif

            CALL PINPOK(XD,YD,NPL,XPL,YPL,IN, jins, dmiss)
            IF (IN == 1 .and. x(3) <= xwleft + 360d0) THEN
                jafive = 0
                CALL ADDMAZE(X,Y,Z,NN,JAFIVE)
                CALL ADDMAZE(X,-Y,Z,NN,JAFIVE)
            ENDIF
         ENDDO
         if (jaklaar == 1) exit

         yy  = yy + dy0
      ENDDO
      call MERGENODESINPOLYGON()

      do L = 1, numL
         k1 = kn(1,L) ; k2 = kn(2,L)
         if (k1 .ne. 0 .and. k2 .ne. 0) then ! jammer dan, nb na setnodadm nog zooi
            if ( (nmk(k1) == 5 .or. nmk(k1) == 6) .and. (nmk(k2) == 5 .or. nmk(k2) == 6) ) then
               if (yk(k1) == yk(k2) ) then
                  call DELLINK(L)
               endif
            endif
         endif
      enddo

   else IF (NTYP .EQ. 7 .and. radius > 0) THEN

      K0    = 0
      L0    = 0
      NUMKN = NRX+1
      NUMLN = NRX
      CALL INCREASENETW(2*NUMKN, 2*NUMLN)
      dphi  = -pi/nrx
      dxt   = 0d0
      rr    = radius
      dx0   = dphi*rr
      do i = 1,3
         phi   = angle + pi + 0.5d0*dphi
         dxt   = 0d0
         k     = k0 + 1
         L     = L0
         xk(k) = x0 + rr*cos(phi)
         yk(k) = y0 + rr*sin(phi)
         zk(k) = zkuni - 0.5d0*dx0*bedslope
         do LL = 1,nrx-1
            phi = phi  + dphi ; k = k + 1; L = L + 1
            xk(k) = x0 + rr*cos(phi)
            yk(k) = y0 + rr*sin(phi)
            call dbdistancehk(xk(k-1), yk(k-1), xk(k), yk(k), dx0)
            zk(k) = zk(k-1) - bedslope*dx0
            dxt   = dxt + dx0
            kn(1,L+L0) = k-1
            kn(2,L+L0) = k
            kn(3,L+L0) = 1
         enddo
         dxt = dxt + dx0
         f   = pi*radius/dxt
         rr  = rr*f
      enddo

      k = k + 1
      xk(k) = x0 - 1.5d0*radius
      yk(k) = y0 + 0.5d0*dx0
      zk(k) = zkuni - 0.5d0*dx0*bedslope

      !call add2Dcell(xk(k)+3d0*radius,yk(k),zkuni,bedslope)

      do LL =  1,nrx-1
         k = k + 1; L = L + 1
         xk(k) = xk(k-1)
         yk(k) = yk(k-1) + dx0
         zk(k) = zk(k-1) - bedslope*dx0
         kn(1,L+L0) = k-1
         kn(2,L+L0) = k
         kn(3,L+L0) = 1
      enddo

      numk = k; numl = L


      rl = pi*radius
      dd = rl/dble(nrx)
      z2 = -(rl+dd)*bedslope
      zbn = -5d0 - (rl-0.5d0*dd)*bedslope


      fnam = 'c128_0001.tim'
      write(fnam(2:4) , '(i3.3)') nrx
      call newfil(mou2, fnam)
      write(mou2,*) '0d0  ', z2  ! zk(k) + 5d0 - bedslope*dx0
      write(mou2,*) '9d10 ', z2  ! zk(k) + 5d0 - bedslope*dx0
      call doclose(mou2)

   else IF (NTYP .EQ. 8) THEN ! 90 degrees bend 1D

      NUMKN = NRX+1
      NUMLN = NRX
      CALL INCREASENETW(2*NUMKN, 2*NUMLN)

      K = 1 ; xk(k) = x0; yk(k) = y0 ; zk(k) = zkuni - 0.5*bedslope*dx0
      L = 0 ; xd = 1d0 ; yd = 0d0
      do LL = 1,nrx
         k = k + 1; L = L + 1
         if (LL > nrx/2) then
            xd = -0.5d0*sqrt(2d0)
            yd = xd
         endif
         xk(k) = xk(k-1) + dx0*xd
         yk(k) = yk(k-1) + dx0*yd
         zk(k) = zk(k-1) - bedslope*dx0
         kn(1,L) = k-1
         kn(2,L) = k
         kn(3,L) = 1
      enddo

      numk = k; numl = L


   endif


   CALL SETNODADM(0)
   call readyy('makenet', -1d0 )


   RETURN
   END SUBROUTINE MAKENET
