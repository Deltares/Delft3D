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

 subroutine interpdivers(naar)

 use m_netw
 use M_FLOWGEOM
 use m_flow
 use m_samples
 use m_flowparameters
 use m_ec_interpolationsettings
 use m_missing
 use m_grid
 use kdtree2Factory
 use m_polygon, only: NPL, xpl, ypl, zpl
 use m_ec_basic_interpolation, only: triinterp2, averaging2, TerrorInfo
 use m_sferic, only: jsferic, jasfer3D
 use gridoperations

 implicit none

 DOUBLE PRECISION, ALLOCATABLE :: XX(:,:), YY(:,:)
 DOUBLE PRECISION, ALLOCATABLE :: XXX(:),  YYY(:)
 INTEGER         , ALLOCATABLE :: NNN (:)

 integer, intent(in) :: naar !< 1: To flow nodes, 2: to zk net nodes.
 integer :: N,NN,L,LK,K,KK, N6, mnx, md,mu, n1,n2
 integer :: i, ierror
 integer :: jdla, jakdtree = 1
 double precision           :: xn, yn, dist
 type(TerrorInfo)           :: errorInfo

 if (NAAR == 1 .AND. ndx == 0) then
    call qnerror('First reinitialise flow model, current dimensions are 0',' ',' ')
    return
 endif

 ! get sample permutation array (increasing x-coordinate order)
 if ( IPSTAT.ne.IPSTAT_OK ) then
    call tidysamples(xs,ys,zs,ipsam,NS,MXSAM,MYSAM)
    call get_samples_boundingbox()

    IPSTAT = IPSTAT_OK
 end if


 if ( ( (naar.eq.1) .and. (interpolationtype.ne.1) ) .or.  &
      ( (naar.eq.2) .and. (interpolationtype.eq.2) ) .or.  &
      ( (naar.eq.3) .and. (interpolationtype.ne.1) ) ) then
    if ( jakdtree.eq.1 ) then
!      initialize kdtree
       call build_kdtree(treeglob, Ns,xs,ys, ierror, jsferic, dmiss)
       if ( ierror.ne.0 ) then
!         disable kdtree
          call delete_kdtree2(treeglob)
          jakdtree = 0
       end if
    end if
 end if

 if (naar == 2 .and. interpolationtype == 2) then
    ! Only do this when not in flowgeom mode.
    call findcells(0)
 endif

 JDLA = 1
 if (naar == 1) then
    if (ibedlevtyp == 1) then
       IF (INTERPOLATIONTYPE == 1) THEN
          CALL triinterp2(XZ,YZ,BL,NDX,JDLA, &
                          XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef) ! to flownodes bl, tiledepth approach
       ELSE
          N6 = MAXVAL(NETCELL%N)
          ALLOCATE( XX(N6,NDX), YY(N6,NDX), NNN(NDX) )
          DO N = 1,NDX
             NNN(N) = NETCELL(N)%N
             DO NN = 1, NNN(N) ! make search cells based on net cell contour + RCEL search radius factor
                XX(NN,N) = xzw(N) + RCEL*(XK(NETCELL(N)%NOD(NN))-xzw(N))
                YY(NN,N) = yzw(N) + RCEL*(YK(NETCELL(N)%NOD(NN))-yzw(N))
             ENDDO
          ENDDO
          call averaging2(1,NS,XS,YS,ZS,IPSAM,XZ,YZ,BL,NDX,XX,YY,N6,NNN,jakdtree, &
                          dmiss, jsferic, jasfer3D, JINS, NPL, xpl, ypl, zpl, errorInfo)
          DEALLOCATE(XX,YY,NNN)
          call checkErrorInfo(errorInfo)
       ENDIF
    else if (ibedlevtyp == 2) then                             ! to flowlinks bottomlevel blu will be phased out but not yet..
       if (INTERPOLATIONTYPE == 1) THEN
          CALL triinterp2(Xu,Yu,Blu,LNX,JDLA, &
                          XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)
       else
          N6 = 4
          ALLOCATE( XX(N6,lnx), YY(N6,lnx), NNN(lnx) )
          do L = 1,lnx
             xx(1,L) = xz(ln(1,L))  ; yy(1,L) = yz(ln(1,L))
             xx(3,L) = xz(ln(2,L))  ; yy(3,L) = yz(ln(2,L))
             Lk      = ln2lne(L)
             xx(2,L) = xk(kn(1,Lk)) ; yy(2,L) = yk(kn(1,Lk))
             xx(4,L) = xk(kn(2,Lk)) ; yy(4,L) = yk(kn(2,Lk))
          enddo
          nnn = 4 ! array nnn
          call averaging2(1,NS,XS,YS,ZS,IPSAM,Xu,Yu,BLu,lnx,XX,YY,N6,NNN,jakdtree, &
                          dmiss, jsferic, jasfer3D, JINS, NPL, xpl, ypl, zpl, errorInfo)  ! interpdivers
          DEALLOCATE(XX,YY,NNN)
          call checkErrorInfo(errorInfo)
       endif
    endif

 else if (naar == 2) then                                      ! to network ZK

    if (INTERPOLATIONTYPE == 1) THEN
       CALL triinterp2(Xk,Yk,Zk,Numk,JDLA, &
                       XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)
    ELSE IF (INTERPOLATIONTYPE == 2) THEN
       n6 = 3*maxval(nmk)   ! 2: safe upper bound , 3 : even safer!
       ALLOCATE( XX(N6,NUMK), YY(N6,NUMK), NNN(NUMK), xxx(N6), yyy(N6) )
       do K = 1,NUMK
!         get the celllist
          call make_dual_cell(k, n6, rcel, xxx, yyy, nnn(k), Wu1Duni)
          do i=1,nnn(k)
             xx(i,k) = xxx(i)
             yy(i,k) = yyy(i)
          enddo
       enddo
       call averaging2(1,NS,XS,YS,ZS,IPSAM,XK,YK,ZK,NUMK,XX,YY,N6,NNN,jakdtree, &
                       dmiss, jsferic, jasfer3D, JINS, NPL, xpl, ypl, zpl, errorInfo)
       DEALLOCATE(XX,YY,xxx,yyy,NNN)
       call checkErrorInfo(errorInfo)
    ELSE IF (INTERPOLATIONTYPE == 3 ) THEN
      call sam2net_curvi(numk,xk,yk,zk)
    ENDIF
 else if (naar == 3) then                                      ! to waterlevels S1
    s1 = dmiss
    if (INTERPOLATIONTYPE == 1) THEN
       CALL triinterp2(Xz,Yz,s1,Ndx,JDLA, &
                       XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)
    ELSE
       N6 = 6
       ALLOCATE( XX(N6,Ndx), YY(N6,Ndx), NNN(Ndx) )
       do K = 1,Ndx
          NN = nd(k)%lnx
          XX(1:nn,K) = xzw(k) + RCEL*(nd(k)%x-xzw(k))
          yy(1:nn,K) = yzw(k) + RCEL*(nd(k)%y-yzw(k))
          nnn(K) = NN  ! array nnn
       enddo
       call averaging2(1,NS,XS,YS,ZS,IPSAM,Xz,Yz,s1,Ndx,XX,YY,N6,NNN,jakdtree, &
                       dmiss, jsferic, jasfer3D, JINS, NPL, xpl, ypl, zpl, errorInfo)
       DEALLOCATE(XX,YY,NNN)
       call checkErrorInfo(errorInfo)
    ENDIF
    do k = 1,ndx
       if (s1(k) == dmiss) then
           s1(k) = bl(k)
       endif
       s1(k) = max(s1(k), bl(k))
    enddo

 else if (naar == 4) then                                       ! to curvilinear grid ZC

    mnx = mmax*nmax
    IF (INTERPOLATIONTYPE == 1) THEN
       ! TODO: UNST-1770: triinterp2 interface temporarily cannot be used for Interacter-interpolate to ZCcurvigrid values
       ! Needs fix.
       !CALL triinterp2(Xc,Yc,Zc,mnx,JDLA, &
       !                   XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL,transformcoef)
    ELSE
       N6 = 4
       ALLOCATE( XX(N6,mnx), YY(N6,mnx), NNN(mnx) )
       DEALLOCATE(XX,YY,NNN)
       !k = 0
       !do n = 1,nc
       !   do m = 1,mc
       !      md = max(1 ,m-1); nd = max(1, n-1)
       !      mu = min(mc,m+1); nu = min(nc,n+1)
       !      k = k + 1
       !      XX(1,K) = 0.25*(
       !
       call qnerror('not implemented yet',' ',' ')
       return
    ENDIF
 endif

 if ( jakdtree == 1 ) then
    call delete_kdtree2(treeglob)
 end if

 contains

   subroutine checkErrorInfo(errorInfo)
      use MessageHandling
      type(TerrorInfo), intent(in) :: errorInfo

      if (errorInfo%cntNoSamples > 0) then
         write (msgbuf,'(a,i0,a)') 'In interpdivers, no values found for ', errorInfo%cntNoSamples, ' cells/links.'
         call warn_flush()
      end if
      if (allocated(errorInfo%message)) then
         msgbuf = errorInfo%message
         if (errorInfo%success) then
            call warn_flush()
         else
            call err_flush()
         endif
      endif
   end subroutine checkErrorInfo

 end subroutine interpdivers
