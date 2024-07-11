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

 subroutine setupwslopes()                           ! set upwind slope pointers and weightfactors
 ! TODO: 1D upwind slope pointers (gewoon de vorige)
 use m_flowgeom
 use m_flow, only  : plotlin
 use m_flowparameters, only: jaupwindsrc
 use m_sferic
 use m_alloc
 use geometry_module, only: getdx, getdy, dbdistance, spher2locvec
 use stdlib_sorting, only: sort_index
 use m_missing, only: dmiss
 use fm_external_forcings_data

 implicit none


 integer           :: L, k12, k2
 double precision  :: dxn, dyn, rmin, xc, yc, dxu, dyu, r, rli
 integer           :: k,kk,LL,ku,kd,ja, ku2, nn, jacros
 integer           :: i, iup, ib, ng

 double precision  :: xzup, yzup, dxx, dyy, rfr, sum, slnupmax, dxk, dis, xn, yn, sl, sm, crp, xcr, ycr, dxl

 double precision, allocatable :: xzu(:), yzu(:)     ! temparrs for triangulations
 double precision, allocatable :: zz(:), zzu(:), wfn(:,:)
 integer         , allocatable :: indxn(:,:) , kcz(:), kcuu(:)
 integer                       :: jdla, ierr, n, NLNUPMAX
 double precision              :: rn (6)
 integer                       :: kun(6), nri(6)

 double precision, external    :: dprodin


 if (allocated (klnup) ) then
    deallocate (klnup, slnup)
 endif
 allocate (  klnup(6,lnx) , stat=ierr ); klnup = 0
 call aerr( 'klnup(6,lnx)', ierr, lnx )
 allocate (  slnup(6,lnx) , stat=ierr ); slnup = 0d0
 call aerr( 'slnup(6,lnx)', ierr, lnx )

 if ( allocated(csbup) ) then
    deallocate(csbup)
    deallocate(snbup)
 end if

 if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
!   allocate orientation arrays
    allocate(  csbup(4,Lnx) , stat=ierr); csbup = 1d0
   call aerr( 'csbup(4,Lnx)', ierr, Lnx )
    allocate(  snbup(4,Lnx) , stat=ierr); snbup = 0d0
   call aerr( 'snbup(4,Lnx)', ierr, Lnx )
 end if

 do L = 1, lnx

    if (kcu(L) >= 3 .and. kcu(L) <= 7) then          ! switch off at 1D2D connections
       cycle
    endif


    dxn = -csu(L)  ; dyn = -snu(L)                   ! normal vector in upwind dir

    do k12  = 1,2

       rmin = 0
       k    = ln(k12,L)
       kd   = ln(2,L) ; if (k12 == 2) kd = ln(1,L)

! SPvdP: (xzup, yzup) not used here
!       xzup = 2d0*xz(k) - xz(kd)                     ! upwind position for which cell centre interpolated values
!       yzup = 2d0*yz(k) - yz(kd)                     ! need be found

       if (abs(kcu(L)) ==  1 ) then
          if ( nd(k)%lnx .ne. 2) then                 ! dangling
             cycle
          endif
       endif

       if (k12 == 2) then
          dxn = -dxn; dyn = -dyn
       endif

       n = 0
       do kk = 1,nd(k)%lnx                           ! first try to find 1 point that is sufficiently close to link line
          LL = iabs(nd(k)%ln(kk))                    ! use this 1 point if it is less than e.g. 0.1dx away from xzup
          if (LL .gt. lnx1D .and. LL .ne. L) then                        !
             ku = ln(1,LL)
             if (ku == k) ku = ln(2,LL)

!             dxx  = getdx( xz(k), yz(k), xz(ku), yz(ku) )
!             dyy  = getdy( xz(k), yz(k), xz(ku), yz(ku) )
!
!             dxu = dxx*dxi(LL)
!             dyu = dyy*dxi(LL)
!             r   = dxu*dxn + dyu*dyn

             r = dprodin(xz(kd),yz(kd),xz(k),yz(k),xz(k),yz(k),xz(ku),yz(ku))
             r = r*(dxi(L)**2)

             if (r > 0) then                         ! points upwind
                n      = n + 1
                call dlinedis2(xz(ku), yz(ku), xz(k), yz(k), xz(kd), yz(kd), ja, dis, xn, yn, sl)
                rn(n)  = dis
                kun(n) = ku
             endif
          endif
       enddo

       if (n > 0) then
          nri(1) = 1
          if (n > 1) then
             call sort_index(rn(1:n), nri(1:n)) ! sorted in closeness to linkline
          end if

          nn  = 1
          ku  = kun(nri(nn))
          rfr =  rn(nn) * dxi(L)
!         if (n == 1 .or. rfr < 0.1d0) then          ! if only 1 link attached or upwind point sufficiently close
          if (            rfr < 0.1d0) then          ! if only 1 link attached or upwind point sufficiently close

             klnup(1+3*(k12-1),L) = -ku              ! flag for single value weighting
!             dxx  = getdx( xz(k), yz(k), xz(ku), yz(ku) )
!             dyy  = getdy( xz(k), yz(k), xz(ku), yz(ku) )
!             dxk  = sqrt(dxx*dxx + dyy*dyy)

             dxk = dbdistance(xz(k),yz(k),xz(ku),yz(ku), jsferic, jasfer3D, dmiss)

             dxl  = dx(L)
             slnup(3+3*(k12-1),L) = dxl/dxk          ! slope weigths in 3 or 6

             if (L > lnx1D) then                           ! switch of when intersecting fixed weir flagged by iadv type 6 or 8
                do kk = 1,nd(k)%lnx                        !
                   LL = iabs(nd(k)%ln(kk))                 ! see testcase transport harbour
                   k2 = ln(1,LL) + ln(2,LL) - k
                   if (k2 == ku) then
                       if (iadv(LL) == 6 .or. iadv(LL) == 8) then
                           klnup(1+3*(k12-1),L) = 0
                       endif
                   endif
                enddo
             endif


             cycle
          endif

          jacros = 0
          if (n >= 2) then
             nn  = 2
             ku2 = kun(nri(nn))                      ! can we interpolate in ku and ku2?
             call dcross (xz(kd), yz(kd), xz(k), yz(k), xz(ku), yz(ku), xz(ku2), yz(ku2),JACROS,SL,SM,XCR,YCR,CRP)
             if (sl < 1.2) jacros = 0 ! int point too close to xz(k)
          endif

          if (n >= 3 .and. jacros == 0) then
             nn  = 3
             ku2 = kun(nri(nn))
             call dcross (xz(kd), yz(kd), xz(k), yz(k), xz(ku), yz(ku), xz(ku2), yz(ku2),JACROS,SL,SM,XCR,YCR,CRP)
             if (sl < 1.2) jacros = 0 ! int point too close to xz(k)
          endif

          if (n >= 4 .and. jacros == 0) then
             nn  = 4
             ku2 = kun(nri(nn))
             call dcross (xz(kd), yz(kd), xz(k), yz(k), xz(ku), yz(ku), xz(ku2), yz(ku2),JACROS,SL,SM,XCR,YCR,CRP)
             if (sl < 1.2) jacros = 0 ! int point too close to xz(k)
          endif

          if (jacros == 1) then

             if (L > lnx1D) then                           ! switch of when intersecting fixed weir flagged by iadv type 6 or 8
                do kk = 1,nd(k)%lnx                        !
                   LL = iabs(nd(k)%ln(kk))                 ! see testcase transport harbour
                   k2 = ln(1,LL) + ln(2,LL) - k
                   if (k2 == ku .or. k2 == ku2) then
                       if (iadv(LL) == 6 .or. iadv(LL) == 8) then
                          ku = 0; ku2 = 0
                       endif
                   endif
                enddo
             endif

             klnup(2+3*(k12-1),L) = ku2
             slnup(2+3*(k12-1),L) = sm

             klnup(1+3*(k12-1),L) = ku
             slnup(1+3*(k12-1),L) = 1d0 - sm

!             dxx  = getdx(xz(k), yz(k), xcr, ycr )
!             dyy  = getdy(xz(k), yz(k), xcr, ycr )
!             dxk  = sqrt(dxx*dxx + dyy*dyy)
              dxk = dbdistance(xz(k),yz(k),xcr,ycr, jsferic, jasfer3D, dmiss)

             dxl  = dx(L)
             slnup(3+3*(k12-1),L) = dxL/dxk         ! slope weigths in 3 or 6

             if (size(nd(k)%x) == 3 .or. size(nd(kd)%x) == 3) then ! flag links connected to triangle on either side as negative through klnup(2,*)
                klnup(2+3*(k12-1),L) = -iabs( klnup(2+3*(k12-1),L) ) ! for maxlimontris
             endif

          endif

      endif

    enddo
    !plotlin(L) = klnup(2,L)
 enddo



 if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
!   compute orientation
    do L=1,Lnx
       do k12=1,2
          do i=1,2
             iup = (k12-1)*3 + i
             ib  = (k12-1)*2 + i
             k = iabs(klnup(iup,L))
             if ( k.gt.0 ) then
                call spher2locvec(xz(k),yz(k),1,(/xu(L)/),(/yu(L)/),(/1d0/),(/0d0/),csbup(ib,L),snbup(ib,L),jsferic, jasfer3D, dmiss)
             end if
          end do
       end do
    end do
 end if


 goto 1234

! allocate ( xzu(lnx), yzu(lnx), zzu(lnx), kcuu(lnx), indxn(3,lnx), wfn(3,lnx) , stat=ierr)
! call aerr('xzu(lnx), yzu(lnx), zzu(lnx), kcuu(lnx), indxn(3,lnx), wfn(3,lnx)', ierr, 9*lnx)
!
!
! allocate ( zz(ndx), kcz(ndx) , stat=ierr   ) ; zz= 0 ; kcz = 1
! call aerr('zz(ndx), kcz(ndx)', ierr, 2*ndx )
!
! do k12  = 1,2
!
!    kcuu = 0 ; xzu = 0 ; yzu = 0 ; zzu = 0
!    do L = 1,lnx
!
!       xzup = 2d0*xz( ln(1,L) ) - xz( ln(2,L) )     ! upwind position for which cell centre interpolated values
!       yzup = 2d0*yz( ln(1,L) ) - yz( ln(2,L) )     ! need be found
!       if (k12 == 2) then
!          dxn = -dxn ; dyn = -dyn
!          xzup = 2d0*xz( ln(2,L) ) - xz( ln(1,L) )
!          yzup = 2d0*yz( ln(2,L) ) - yz( ln(1,L) )
!       endif
!       xzu(L) = xzup + 0.1d0 ; yzu(L) = yzup + 0.1d0 ; kcuu(L) = 1
!
!    enddo
!
!    jdla  = 1
!    indxn = 0
!    wfn   = 0
!    call triint( xz  , yz  ,  zz, kcz , ndx,                     &
!                 xzu , yzu , zzu, kcuu, 1, lnx, jdla, indxn, wfn )
!
!    do L = 1,lnx
!       if (klnup(1+3*(k12-1),L) == 0) then
!           slnupmax = 0  ; nlnupmax = 0
!           do n = 1,3
!              klnup(n+3*(k12-1),L) = indxn(n,L)
!              slnup(n+3*(k12-1),L) =   wfn(n,L)
!              sum = sum + slnup(n+3*(k12-1),L)
!              if (slnup(n+3*(k12-1),L) > slnupmax ) then
!                  slnupmax = slnup(n+3*(k12-1),L)
!                  nlnupmax = n
!              endif
!              sum = sum - slnupmax
!           enddo
!           do n = 1,3
!              if (n == nlnupmax) then
!                 slnup(n+3*(k12-1),L) = 1 - sum
!              endif
!           enddo
!       endif
!    enddo
! enddo
!
! deallocate ( xzu, yzu, zzu, kcuu, indxn, wfn)
! deallocate ( zz, kcz )

1234 continue

 if ( jaupwindsrc.eq.1 ) then
!  disable higher-order reconstruction at all flowlinks connected to source/sink flownodes
   call disable_higherorder_at_sorsin()
 end if

 do ng = 1,ngatesg  ! loop over gate signals
    do n  = L1gatesg(ng), L2gatesg(ng)
       LL  = kgate(3,n)
       klnup(:,LL) = 0
    enddo
 enddo

 end subroutine setupwslopes
