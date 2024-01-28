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

subroutine setbobsonroofs( )      ! override bobs along pliz's
 use m_netw
 use m_flowgeom
 use m_flow
 use m_missing
 use unstruc_model
 use unstruc_messages
 use kdtree2Factory
 use m_sferic
 use m_polygon
 use geometry_module
 use m_roofs

 implicit none

 integer                       :: i, k, L, n1, n2, k1, k2, nt, nt2, minp, lastfoundk, kL, kint, kf, jacros
 integer                       :: iL, numLL, numcrossedLinks, ierror, jakdtree=1, inp, n, ip, ip1, ip2, ierr
 double precision              :: SL, SM, XCR, YCR, CRP, Xa, Ya, Xb, Yb, zc, af, roofgutterheight = 0.1d0
 double precision, allocatable :: dSL(:),   blav(:)
 integer,          allocatable :: iLink(:), iLcr(:), iPol(:), nblav(:)
 double precision              :: t0, t1
 character(len=128)            :: mesg

 character(len=5)              :: sd

 if ( len_trim(md_roofsfile) == 0 ) then
    return
 endif

 allocate ( frcuroofs(lnx)      , stat = ierr)
 call aerr('frcuroofs(lnx)'     , ierr,   lnx) ; frcuroofs      = dmiss
 if (infiltrationmodel == DFM_HYD_INFILT_CONST) then
    allocate ( infiltcaproofs(ndx) , stat = ierr)
    call aerr('infiltcaproofs(ndx)', ierr,   lnx) ; infiltcaproofs = dmiss
 endif

 call readyy('Setbobsonroofs', 0d0)

 call oldfil (minp, md_roofsfile)
 call reapol (minp, 0)

 inp  = -1
 kc   = 0
 do n = 1,ndxi
    call inwhichpolygon(xzw(n), yzw(n), inp)
    if (inp > 0) then
       kc(n) = inp
    endif
 enddo

 zpmin = 1d10
 do n = 1, npoly
    zpmin(n) = minval(zpl(iistart(n):iiend(n) ) )
 enddo

 if ( jakdtree.eq.1 ) then
    call klok(t0)
    allocate(iLink(Lnx),ipol(Lnx),dSL(Lnx))
    call find_crossed_links_kdtree2(treeglob,NPL,XPL,YPL,2,Lnxi,0,numcrossedLinks, iLink, iPol, dSL, ierror)
    numLL = numcrossedLinks
    if ( ierror.ne.0 ) then                               !   check if kdtree was succesfull, disable if not so
       deallocate(iLink,ipoL,dSL)
       jakdtree = 0
    end if
    call klok(t1)
    write(mesg,"('set bobs (on roofs) with kdtree2, elapsed time: ', G15.5, 's.')") t1-t0
    call mess(LEVEL_INFO, trim(mesg))
 else
    numLL = Lnxi
 end if

 do i = 1,npoly
    do k = iistart(i), iiend(i)
       ipsection(k) = i
    enddo
 enddo

 kint = max(numLL/100,1) ; nt = 0

 if (roofheightuni > 0d0) then
    allocate( blav(npoly), nblav(npoly) ); blav = 0d0 ; nblav = 0d0
    do iL = 1,numLL

       jacros = 0
       if ( jakdtree.eq.0 ) then
          L = iL
       else
          L = iLink(iL)
          IF (L <= 0) cycle
          k = iPol(iL)
       end if

       if (kcu(L) .ne. 2) then
           cycle
       endif

       if (mod(iL,kint) == 0) then
           AF = dble(iL)/dble(numLL)
           call readyy('Setbobsonroofs', af )
       endif

       n1 = ln(1,L) ; n2 = ln(2,L)
       if ( jakdtree.eq.0 ) then

          xa = xz(n1)  ; ya = yz(n1)
          xb = xz(n2)  ; yb = yz(n2)

          iloop:do i = 1,2

              if (i == 1) then
                 if (Lastfoundk == 0) cycle
                 kf = max(1,     Lastfoundk - 100)
                 kL = min(npl-1, Lastfoundk + 100)
              else
                 kf = 1
                 kL = npl-1
              endif

              Lastfoundk = 0
              do k = kf,kL

                  if (xpl(k) .ne. dmiss .and. xpl(k+1) .ne. dmiss) then
                      CALL CROSSinbox (XPL(k), YPL(k), XPL(k+1), YPL(k+1), Xa, Ya, Xb, Yb, jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)

                      if ( jacros.eq.1 ) then
                         Lastfoundk = k
                         exit iloop
                      end if
                  endif
              enddo

           enddo iloop
        else                                                  ! use kdtree to find nearest dike
           k      = iPol(iL)
           jacros = 1
           sL     = dSL(iL)
        end if

        if (jacros == 1) then                                 !  set roofgutterheight
            n1  = ln(1,L) ; n2 = ln(2,L)
            i   = ipsection(k)
            blav(i)  = blav(i)  + min( bl(n1), bl(n2) )
            nblav(i) = nblav(i) + 1
            nt = nt + 1
         endif

    enddo

    do k = 1,npoly
       if (nblav(k) > 0) then
          blav(k) = blav(k) / nblav(k)
       endif
    enddo

    do L  = 1,lnxi      ! make flat roof
       if (kcu(L) == 2) then
          n1 = ln(1,L) ; n2  = ln(2,L)
          ip1 = kc(n1) ; ip2 = kc(n2)
          if (ip1 > 0 .and. ip2 == ip1) then     ! both in same poly
             zc       = blav(ip1) + roofheightuni
             bob(1,L) = zc ; bob(2,L) = bob(1,L)
             bob0(:,L) = bob(:,L)
             bl(n1)   = zc
             bl(n2)   = zc
             frcuroofs(L)       = frcuniroof
             if (infiltrationmodel == DFM_HYD_INFILT_CONST) then
                infiltcaproofs(n1) = 0d0
                infiltcaproofs(n2) = 0d0
             endif
          endif
       endif
    enddo

    do L  = 1,lnxi 
       if (kcu(L) == 2) then
           n1 = ln(1,L) ; n2  = ln(2,L)
           bob(1,L) = max(bob(1,L),bl(n1),bl(n2)) 
           bob(2,L) = max(bob(2,L),bl(n1),bl(n2)) 
           bob0(:,L) = bob(:,L)
       endif
    enddo 

 endif

 nt2 = 0
 do iL = 1,numLL

    jacros = 0
    if ( jakdtree.eq.0 ) then
       L = iL
    else
       L = iLink(iL)
       IF (L <= 0) cycle
       k = iPol(iL)
    end if

    if (kcu(L) .ne. 2) then
        cycle
    endif

    if (mod(iL,kint) == 0) then
        AF = dble(iL)/dble(numLL)
        call readyy('Setbobsonroofs', af )
    endif

    n1 = ln(1,L) ; n2 = ln(2,L)
    if ( jakdtree.eq.0 ) then

       xa = xz(n1)  ; ya = yz(n1)
       xb = xz(n2)  ; yb = yz(n2)

       iloop2:do i = 1,2

           if (i == 1) then
              if (Lastfoundk == 0) cycle
              kf = max(1,     Lastfoundk - 100)
              kL = min(npl-1, Lastfoundk + 100)
           else
              kf = 1
              kL = npl-1
           endif

           Lastfoundk = 0
           do k = kf,kL

               if (xpl(k) .ne. dmiss .and. xpl(k+1) .ne. dmiss) then
                   CALL CROSSinbox (XPL(k), YPL(k), XPL(k+1), YPL(k+1), Xa, Ya, Xb, Yb, jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)

                   if ( jacros.eq.1 ) then
                      Lastfoundk = k
                      exit iloop2
                   end if
               endif
           enddo

        enddo iloop2
     else                                                  ! use kdtree to find nearest dike
        k      = iPol(iL)
        jacros = 1
        sL     = dSL(iL)
     end if

     if (jacros == 1) then                                 !  set roofgutterheight
         n1 = ln(1,L) ; n2 = ln(2,L)
         ip = ipsection(k)
         if (roofheightuni > 0) then
            zc = blav(ip) + roofheightuni + roofedgeheight
         else
            zc = sl*zpL(k+1) + (1d0-sl)*zpL(k)
         endif
         bob(1,L) = zc ; bob(2,L) = bob(1,L)
         bob0(:,L) = bob(:,L)
         bl(n1)   = min(bl(n1),zc)
         bl(n2)   = min(bl(n2),zc)
         ! Do not change the advection for this link, when advection was turned off
         if (iadv(L) /= 0) then
            iadv(L)  = 8
         endif
         nt2 = nt2 + 1
     endif

 enddo


 do L  = 1,lnxi    ! roofgutter connection
    if (kcu(L) == 7) then
       n1  = ln(1,L)   ; n2  = ln(2,L)
       k1  = lncn(1,L) ; k2  = lncn(2,L)
       ip1 = kc(n1)  ; ip2 = kc(n2)
       if (ip1 > 0) then
          bob(1,L) = bl(n1) ; ! zk(k1) = bl(n1)
       endif
       if (ip2 > 0) then
          bob(2,L) = bl(n2) ; ! zk(k2) = bl(n2)
       endif
       bob0(:,L) = bob(:,L)
       if (ip1 > 0 .or. ip2 > 0) then
          dx(L)        = max(dx(L), dxminroofgutterpipe)
          frcuroofs(L) = frcuniroofgutterpipe
       endif
    endif
 enddo

 if (nt > 0) then
    call mess(LEVEL_INFO,'Number of roof polygons                   :: ', npoly)
    call mess(LEVEL_INFO,'Number of flow Links with roof attributes :: ', nt2  )
 endif

 call readyy(' ', -1d0 )

1234 continue

! deallocate
 if ( jakdtree.eq.1 ) then
    if ( allocated(iLink) ) deallocate(iLink)
    if ( allocated(iPol)  ) deallocate(iPol)
    if ( allocated(dSL)   ) deallocate(dSL)
 end if

 call deallocpoladm()
 if (allocated (blav) ) deallocate(blav, nblav)

 end subroutine setbobsonroofs
