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

 subroutine s1nod()                                  ! nodes in continuity eq
 use precision_basics
 use time_module, only : seconds_to_datetimestring
 use m_plotdots
 use MessageHandling
 use m_flow
 use m_flowgeom
 use m_flowtimes
 use m_reduce
 use m_partitioninfo
 use m_missing
 use m_alloc
 use m_sobekdfm
 use unstruc_channel_flow
 use iso_c_utils, only : MAXSTRINGLEN
 use m_fm_icecover, only : ice_apply_pressure, ice_p

 implicit none

 integer          :: n
 integer          :: kb , k2 , L, k, LL, itpbn
 integer          :: ibr
 double precision :: dtiba, hh, zb, dtgh
 double precision :: sqrtgfh, cffu, rowsum, fuL, ruL, huL, hep
 integer          :: i, ierr
 character(len=2) :: dim_text
 double precision, parameter :: HBMIN = 1d-3
 double precision, pointer, dimension(:)  :: gridPointsChainages
 type(t_branch), pointer, dimension(:)    :: branch
 logical                                  :: domainCheck
 character(len=MAXSTRINGLEN)              :: msgbufpar   ! can not use msgbuf, as each OpenMP thread must have it's own

 !bbr = bb + dti*a1     !m2/s
 !ddr = dd + dti*a1*s1  !m3/s

! BEGIN DEBUG
  !if ( jampi.eq.1 ) then
  !   call reduce_dt()
  !   call update_ghosts(ITYPE_Sall, Ndx, s1, ierr)
  !   call update_ghosts(ITYPE_Sall, Ndx, a1, ierr)
  !   call update_ghosts(ITYPE_Sall, Ndx, vol0, ierr)
  !   call update_ghosts(ITYPE_Sall, Ndx, vol1, ierr)
  !   call update_ghosts(ITYPE_U, 1,    Lnx, fu,   ierr)
  !   call update_ghosts(ITYPE_U, 1,    Lnx, ru,   ierr)
  !end if
! END DEBUG


 !! remove entries for 1d2d nodes in bb, dd and ccr
 !  do n  = 1, nbnd1d2d                                   ! 1D2D boundaries
 !   kb      = kbnd1d2d(1,n)
 !   k2      = kbnd1d2d(2,n)
 !   L       = kbnd1d2d(3,n)
 !   bb(k2) = bb(k2) -bb(kb)
 !   dd(k2) = dd(k2) + dd(kb)
 !   bb(kb) = 0d0
 !   dd(kb) = 0d0
 !   ccr(lv2(L)) = 0d0
 ! end do

 !$OMP PARALLEL DO           &
 !$OMP PRIVATE(n,dtiba,domaincheck,dim_text,L,i,LL,ibr,branch,gridPointsChainages,k)
 do n = 1,ndx                                        ! Waterlevels, = s1ini
    dtiba  = dti*a1(n)
    bbr(n) = bb(n) + dtiba                           ! need it also for kfs.ne.1 at the boundaries (for parallel runs, see partition_setkfs)
    if (nonlin >= 2) then                            ! pressurised
        bbr(n)  = bbr(n) - dti*a1m(n)
    endif

    domaincheck = .true.
    if (jampi == 1) then
       domaincheck = (idomain(n) == my_rank)
    end if

    ! Check for zero (0d0) value on diagonal, to print a warning before a resulting Saad crash.
    if (comparerealdouble(bbr(n),0d0)==0 .and. domainCheck) then
       if (n <= ndx2d) then
          dim_text = '2D'
       else
          dim_text = '1D'
       endif
       write(msgbufpar,'(a, i0, a)') 'The surface area of '//dim_text//'-node with node number ''', n, ''' is equal to 0'
       call setMessage(LEVEL_WARN, msgbufpar)
       call SetMessage(-1, 'This might lead to a SAAD error in the solve process' )
       write(msgbufpar, '(a)') 'Current time is: '
       call seconds_to_datetimestring(msgbufpar(18:), refdat, time1)
       call setMessage(-1, msgbufpar)
       write(msgbufpar,'(a,f10.2,a,f10.2,a)') 'The location of the node is at (',xz(n),',',yz(n),')'
       call setMessage(-1, msgbufpar)
       L = -1
       if (n > ndx2d .and. network%loaded) then
          do i = 1, nd(n)%lnx
             if (abs(nd(n)%ln(i)) <= lnx1d) then
                L = abs(nd(n)%ln(i))
                exit
             endif
          enddo
          if (L/=-1) then
            ibr = network%adm%lin2ibr(L)
            LL = network%adm%lin2local(L)

            branch => network%brs%branch
            gridPointsChainages => branch(ibr)%gridPointsChainages
            ! UNST-4031: Dangerous code: this assumes that flow links and flow nodes
            ! on a single branch are perfectly sorted, which is not required in input
            ! networks. The k value below may not be correct.
            if (nd(n)%ln(i) < 0) then
               ! gridpoint at start of link internal gridpoint is equal to
               k = LL
            else
               k = LL+1
            endif
            write(msgbufpar,'(a, f9.2)') 'The gridpoint lies at branch with id ''' //trim(branch(ibr)%id)// ''' at chainage: ', gridPointsChainages(k)
            call setMessage(-1, msgbufpar)
         endif
      endif
      call adddot(xz(n), yz(n), colournumber = 247)
    endif

    if (kfs(n) == 1) then                            ! only for implicit points
       if (nonlin > 0) then
          ddr(n) = dd(n)  + dtiba*s1(n)              !
          ddr(n) = ddr(n) + dti*( vol0(n) - vol1(n) )
          if (nonlin >= 2) then                      ! pressurised
             ddr(n) = ddr(n) - s1m(n)*a1m(n)*dti
          endif
       else
          ddr(n) = dd(n)  + dtiba*s0(n)              ! Use s0 for the linear solver. Normally s0 = s1, however in
                                                     ! iterative couplings this might not be the case (e.g. 1d2d
                                                     ! SOBEK D-FlowFM coupling
       endif
    endif                                            ! then also setback s1 !
 enddo
 !$OMP END PARALLEL DO

 ! compute right-hand sides
 do n  = 1, nbndz                                    ! overrides for waterlevel boundaries
    kb      = kbndz(1,n)
    k2      = kbndz(2,n)
    L       = kbndz(3,n)
    itpbn   = kbndz(4,n)
!    bbr(kb) = 1d0
    if (     itpbn == 1) then                        ! waterlevelbnd
       zb   = zbndz(n)
       if (alfsmo < 1d0) then
          zb = alfsmo*zb + (1d0-alfsmo)*zbndz0(n)
       endif
    else if (itpbn == 2) then                        ! neumannbnd, positive specified slope leads to inflow
       !zb   = s1(k2) + zbndz(n)*dx(L)
       zb    = -zbndz(n)*dx(L)*ccr(Lv2(L))            ! right-hand side
    else if (itpbn == 5) then                        ! Riemannbnd
!       hh   = max(epshs, 0.5d0*( hs(kb) + hs(k2) ) )
!       zb   = 2d0*zbndz(n) - zbndz0(n) - sqrt(hh/ag)*u1(L)
       zb   = 2d0*zbndz(n) - zbndz0(n)
    else if (itpbn == 6) then                        ! outflowbnd
       if (u0(L) > 0d0) then
          zb   = s1(k2)
       else
          hh   = max(epshs, 0.5d0*( hs(kb) + hs(k2) ) )
          dtgh = dts*( sqrt(ag*hh) )
          zb   = s1(kb) - dtgh*(  dxi(L)*( s1(kb) - s1(k2) ) - zbndz(n) )  ! verder testen
       endif
    else if (itpbn == 7) then                        ! qhbnd
       zb   = zbndz(n)
       if (alfsmo < 1d0) then
          zb = alfsmo*zb + (1d0-alfsmo)*zbndz0(n)
       endif
    endif

!   set matrix entries
    if ( itpbn.eq.2 ) then
!      Neumann boundary condition
       if ( ccr(Lv2(L)).eq.0d0 ) then  ! internal cell is wet, but boundary face is inactive (see setkfs)
          ccr(Lv2(L)) = -bbr(kb)
          bbr(k2) = bbr(k2) + bbr(kb)
          ddr(k2) = ddr(k2) + ccr(Lv2(L))*zbndz(n)*dx(L)
       end if

       bbr(kb)     = -ccr(Lv2(L))
       ddr(kb)     = -zbndz(n)*dx(L)*ccr(Lv2(L))      ! double for safety
    else if ( itpbn.eq.5 ) then
!      Riemann boundary condition (note: ccr= -Au theta fu)
       zb = max(zb,bl(kb)+HBMIN)
       if ( ccr(Lv2(L)).eq.0d0 ) then   ! internal cell is wet, but boundary face is inactive (see setkfs)
          ddr(kb) = bbr(kb) * zb ! u(L)=0 assumed
       else
          hh   = max(epshs, 0.5d0*( hs(kb) + hs(k2) ) )
          sqrtgfh = sqrt(ag/hh)
          if (kmx == 0) then
             fuL = fu(L)
             ruL = ru(L)
          else
             fuL = 0d0; ruL = 0d0; huL = 0d0
             do LL  = Lbot(L), Ltop(L)
                hep = max(epshu,hu(L)-hu(L-1))
                fuL = fuL + fu(LL)*hep
                ruL = ruL + ru(LL)*hep
                huL = huL +        hep
             enddo
             ful = fuL/huL ; ruL = ruL/huL
          endif
          cffu    = ccr(Lv2(L))/fuL
          bbr(kb) = -cffu*(fuL+sqrtgfh)
          ddr(kb) = -cffu*(sqrtgfh*zb-ruL)
       end if
    else
!      Dirichlet boundary condition

       if (japatm > 0 .and. PavBnd > 0) then
          zb = zb - ( patm(kb) - PavBnd )/(ag*rhomean)
       endif

       if (ice_apply_pressure) then
          zb = zb - ice_p(kb) /(ag*rhomean)
       endif

       zb = max( zb, bl(kb) + HBMIN )

       ddr(kb)     = bbr(kb)*zb
       ddr(k2)     = ddr(k2) - ccr(Lv2(L)) * zb       ! met link(L) in s1ini
       ccr(Lv2(L)) = 0d0
    end if
 enddo

 do n  = 1, nbndu                                   ! velocity boundaries
    kb      = kbndu(1,n)
    k2      = kbndu(2,n)
    L       = kbndu(3,n)
 !   bbr(kb) = 1d0
 !   ddr(kb) = s1(k2)
 !     SPvdP: apply Neumann conditions to water level at velocity boundaries
       ccr(Lv2(L)) = -bbr(k2)            ! some non-zero value
       bbr(k2)     = bbr(k2) - ccr(Lv2(L))
       bbr(kb)     = -ccr(Lv2(L))   ! should not be zero
       ddr(kb)     = 0d0
 enddo

 if (nbnd1d2d > 0) then
   call compute_1d2d_boundaries()
 endif

 jacheckmatrix = 0
 if (jacheckmatrix > 0) then
    do k = 1,ndxi
       rowsum = 0
       do LL = 1, size( nd(n)%ln )
          L  = iabs( nd(n)%ln(LL) )
          rowsum = rowsum + abs(ccr(Lv2(L)))
       enddo
       if ( bbr(k) <= rowsum ) then
          call qnerror('checkmatrix = nocheck', ' ', ' ')
       endif
    enddo
 endif

! update overlapping ghost-parts of matrix
 if ( jampi.eq.1 .and. jaoverlap.eq.1 ) then
    call update_matrix(ierr)
 end if

 end subroutine s1nod
