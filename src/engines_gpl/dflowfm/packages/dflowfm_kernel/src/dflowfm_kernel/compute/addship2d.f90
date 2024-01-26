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

 subroutine addship2D(japerim)
 use m_ship
 use m_flowgeom
 use m_flow
 use m_arcinfo
 use m_sferic
 use m_flowtimes
 implicit none
 integer :: k, L, k1, k2, japerim, i0, i1, j0, j1, n, numi, kk
 double precision :: slotsav, h1, h2, dx2d, d2, css, sns, dxsa, dysa, dxx, dyy, xx, yy, sxr, syr, sxrL, sxrR, alfa
 double precision :: alf, alfy, dss, frb, yf, v1, v2, domp, omegadomp, za, zs1, zs2

 if (japressurehull == 2) then

    slotsav = slotw2D
    slotw2D = 0d0
    do L = lnx1D + 1, lnxi
       call addlinkship2D(L,japerim)                   ! substract the ship
    enddo
    slotw2D = slotsav

 else if (japressurehull == 3) then

    v1ship = 0d0 ; a1m = 0d0


    omegadomp = 1.0
    domp      = 0d0 ! 0.01*sin(time1*omegadomp)
    do n = 1,nshiptxy
       css  = cos(shi(n))
       sns  = sin(shi(n))
       dxsa = 2d0*shL(n)/(mca-1)
       dysa = 2d0*shb(n)/(nca-1)

       do k = 1, ndx
          dx2d = sqrt(ba(k)) ; d2 = 0.5d0*dx2d*css
          syr  = (yz(k) - shy(n))*css - (xz(k) - shx(n))*sns
          sxr  = (xz(k) - shx(n))*css + (yz(k) - shy(n))*sns
          sxrL = (xz(k) - d2 - shx(n))*css + (yz(k) - shy(n))*sns
          sxrR = (xz(k) + d2 - shx(n))*css + (yz(k) - shy(n))*sns
          yf   = 1d0 - ( 0.1d0*abs( syr ) / shb(n) )
          if ( syr  >  -shb(n) .and. syr  < shb(n) .and. &   ! within ship contours
               sxrR >  -shL(n) .and. sxrL < shL(n)  ) then

               if (ihullmethod == -1) then                     ! constant
                   zsp(k) = shd(n)
               else if (ihullmethod == 0) then                 ! cosine
                   zsp(k) = 0d0
                   numi   = 2
                   dxx    = dx2D/numsmo
                   do kk = 1,numsmo
                      sxr  = sxrL + (kk - 0.5d0)*dxx
                      alf  = 1d0
                      dss  = abs( sxr)  / (shL(n)*yf) ; frb = 0.40d0  ! 0.25d0
                      if (dss > frb) then
                          alf = 0.5d0*( cos(pi*(dss-frb)/(1d0-frb)) + 1d0)
                      endif

                      alfy = 1d0
                      dss  = abs(syr) / shb(n)
                      if (icontroltyp(n) < 4 ) then
                         frb = max(0.2d0, 0.8d0*alf)
                       else
                         frb = 0.6d0  ! relax man
                      endif
                      if (dss > frb) then
                          alfy = 0.5d0*( cos(pi*(dss-frb)/(1d0-frb)) + 1d0)
                      endif
                      zsp(k) = zsp(k) + shd(n)*alf*alfy   ! 17d0
                   enddo
                   zsp(k) = zsp(k) / numsmo
               else if (ihullmethod == 4) then                ! linear
                   if (sxrL >= 0d0) then
                      zsp(k) = shd(n)*(1d0 - sxr/shL(n))      ! bow
                   else if (sxrR <= 0d0) then
                      zsp(k) = shd(n)*(1d0 + sxr/shL(n))      ! stern
                   else
                      za     = -sxrL / (sxrR - sxrL)
                      zs1    =  shd(n)*(1d0 + 0.5d0*sxrL/shL(n)) ! stern
                      zs2    =  shd(n)*(1d0 - 0.5d0*sxrR/shL(n)) ! bow
                      zsp(k) = za*zs1 + (1d0-za)*zs2
                   endif
               else if (ihullmethod == 5) then                ! linear
                   if (sxr >= 0d0) then
                      zsp(k) = shd(n)*(1d0 - sxr/shL(n))      ! bow
                   else
                      zsp(k) = shd(n)*(1d0 + sxr/shL(n))      ! stern
                   endif
               else                                           ! arcinfo
                   xx  = sxr + shL(n)
                   i0  = 1 + (mca-1)*xx/(2d0*shL(n)) ; i1 = i0 + 1
                   dxx = (xx - (i0-1)*dxsa)/dxsa
                   yy  = syr + shB(n)
                   j0  = 1 + (nca-1)*yy/(2d0*shB(n)) ; j1 = j0 + 1
                   dyy = (yy - (j0-1)*dysa)/dysa
                   zsp(k)    = D(i0,j0)*(1d0-dxx)*(1d0-dyy) + &
                               D(i1,j0)*(    dxx)*(1d0-dyy) + &
                               D(i0,j1)*(1d0-dxx)*(    dyy) + &
                               D(i1,j1)*(    dxx)*(    dyy)
               endif
               alfa   = 1d0
               if ( sxrL < -shL(n) ) then
                  alfa = ( SxrR  - - shL(n) ) / dx2d
               else if (sxrR > shL(n) ) then
                  alfa = ( shL(n) - sxRL) / dx2d
               endif
               a1m(k)    = ba(k)*alfa
               zsp(k)    = zsp(k) + domp
               zsp(k)    = max(0.02d0*shd(n), zsp(k))
               v1ship(k) = (s1m(k) + zsp(k))*a1m(k)
          endif
       enddo
    enddo

    if (japerim == 1) then

       do L = lnx1D + 1, lnxi
          k1 = ln(1,L) ; k2 = ln(2,L)
          if (zsp(k1) .ne. 0d0 .or. zsp(k2) .ne. 0d0) then
             !h1    = s1(k1) + zsp(k1)
             !h2    = s1(k2) + zsp(k2)
             !h1    = zsp(k1)
             !h2    = zsp(k2)
             !au(L) = au(L) - 0.5d0*( h1 + h2 )*wu(L)

             v1    = v1ship(k1)
             v2    = v1ship(k2)
             au(L) = au(L) - 0.5d0*( v1 + v2 )/dx(L)
          endif
       enddo

    endif

 else
    if (japerim == 0) then
       do k = 1,ndx
          if (zsp(k) .ne. 0d0) then
             h1      = s1m(k) + zsp(k)
             vol1(k) = vol1(k) - ba(k)*h1
             a1m(k)  = ba(k)
          endif
       enddo
    else
       do L = lnx1D + 1, lnxi
          k1 = ln(1,L) ; k2 = ln(2,L)
          if (zsp(k1) .ne. 0d0 .or. zsp(k2) .ne. 0d0) then
             h1    = s1(k1) + zsp(k1)
             h2    = s1(k2) + zsp(k2)
             au(L) = au(L) - 0.5d0*( h1 + h2 )*wu(L)
          endif
       enddo
    endif
 endif

 end subroutine addship2D
