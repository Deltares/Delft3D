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

 subroutine tekflowlinks()
 use unstruc_display
 use m_netw
 use m_flowgeom
 use m_flow
 use m_sferic
 use m_missing
 implicit none
 integer :: nodemode, nodewhat,ndraw(50)
 integer :: k, L, ja, ja2, k1, k2, nn, ncol, linkmode
 double precision :: zlin, zL
 double precision :: xcl, ycl, zcl                   ! help only
 double precision :: xx1, yy1, Zz1                   ! help only
 double precision :: xx2, yy2, Zz2                   ! help only
 double precision :: x3, y3, x4, y4                  ! help only
 double precision :: x(4), y(4), z(4), hw, cs, sn
 real             :: xr(4), yr(4)
 logical inview

 common /drawthis/ ndraw

 linkmode = ndraw(11)
 if (LINKMODE > 1 .AND. ndraw(29) .ge. 2) then                          ! show VALUES AT links
   IF (NDRAW(7) == 1) call minmxlns()    ! ONLY ADAPT VERTICAL LIMITS FOR FLOW links IF NO NET links ASKED

     call setisoscale2is1()

     IF (linkmode == 3 .OR. linkmode == 6) THEN

        call copyzlintornod()
        do k = 1,ndx2d
           if (mod(k,200) == 0) then
              call halt2(ja)
              if (ja == 1) return
           endif
           if (inview( xz(k), yz(k) ) ) then
               call ISOSMOOTHflownode2(k)
           endif
        enddo

     else
        do L = 1,lnx
           if (mod(L,200) == 0) then
              call halt2(ja)
              if (ja == 1) return
           endif

           if ( inview( xu(L), yu(L)  ) ) then
              ZZ1 = 0d0  !Bob(1,L)
              ZZ2 = 0d0  !Bob(2,L)

              xcl = xu(L)
              ycl = yu(L)
              zcl=0.5*(ZZ1+ZZ2)

              zl = zlin(L)
              if ( zL.eq.DMISS ) cycle
              CALL ISOCOL2(zl,NCOL)

              k1 = ln(1,L)
              xX1 = Xz(K1)
              yY1 = Yz(K1)
              k2 = ln(2,L)
              xX2 = Xz(K2)
              yY2 = Yz(K2)

              IF (linkmode .EQ. 3 .OR. linkmode .EQ. 6) THEN
                  CALL DMOVABS(XX1,YY1,ZZ1)
                  CALL DLNABS(XX2,YY2,ZZ2)
              ELSE IF (linkmode .EQ. 4 .OR. linkmode .EQ. 7) THEN
                  if (L > Lnx1D) then ! 2D
                     k1 = lncn(1,L)
                     X3 = Xk(K1)
                     Y3 = Yk(K1)
                     k2 = lncn(2,L)
                     X4 = Xk(K2)
                     Y4 = Yk(K2)

                     CALL DRIETWEE(Xx1, Yy1, ZZ1, X(1), Y(1), Z(1) )
                     CALL DRIETWEE(X3, Y3, ZZ1, X(2), Y(2), Z(2) )
                     CALL DRIETWEE(Xx2, Yy2, ZZ2, X(3), Y(3), Z(3) )
                     CALL DRIETWEE(X4, Y4, ZZ2, X(4), Y(4), Z(4) )
                     xr = x
                     yr = y
                     CALL PFILLERCORE(Xr,Yr,4)
                  else
!                     hw    = 0.25d0*( a1(k1) + a1(k2) )/dx(L)
                     if (hu(L) > 0d0) then
                        hw = 0.5d0 * Au(L) / hu(L)   ! flat bed, half width
                     else
                        hw = 1d-3
                     endif
                     if (jsferic == 1) then
                        hw = hw*rd2dg/ra
                     endif

                     sn    = snu(L)
                     cs    = csu(L)
                     x(1)  = xx1 + sn*hw
                     y(1)  = yy1 - cs*hw
                     x(2)  = xx2 + sn*hw
                     y(2)  = yy2 - cs*hw
                     x(3)  = xx2 - sn*hw
                     y(3)  = yy2 + cs*hw
                     x(4)  = xx1 - sn*hw
                     y(4)  = yy1 + cs*hw
                     xr = x
                     yr = y
                     CALL PFILLERCORE(Xr,Yr,4)
                 endif
              ELSE IF (linkmode .EQ. 5 .OR. linkmode .EQ. 8) THEN
                  CALL DRCIRC(XCL,YCL,ZCL)
              ENDIF
              if ( linkmode == 2 .or. linkmode == 6 .or. linkmode == 7 .or. linkmode == 8) then
                 IF (NDRAW(29) .EQ. 12 .or. NDRAW(29) .EQ. 29 .or. NDRAW(29) .EQ. 33 .or. NDRAW(29) .EQ. 35 .or. NDRAW(29) .EQ. 36) THEN
                    CALL DHITEXT( int(zl), xCL, yCL, zCL )
                 else
                    call dhtext( zl, xCL, yCL, zCL )
                 end if
              endif

           endif
        enddo
     endif ! linkmode
 endif ! ndraw(29)
 end subroutine tekflowlinks
