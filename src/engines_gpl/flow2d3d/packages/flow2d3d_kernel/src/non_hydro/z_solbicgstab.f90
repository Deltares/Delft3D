subroutine z_solbicgstab(aak       ,bbk       ,cck      ,aak2      ,cck2      , &
                       & bbka      ,bbkc      ,ddk      ,kmax      ,icx       , &
                       & icy       ,nmmax     ,nst      ,kfsz0     ,pnhcor    , &
                       & pj        ,rj        ,vj       ,dinv      ,pbbk      , &
                       & pbbkc     ,p1        ,rjshadow ,sj        ,sjprec    , &
                       & tj        ,kfs       ,kfsmin   ,kfsmx0    ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
!
! Solves system of equations.
! BiCGSTAB-method.
!
!!--pseudo code and references--------------------------------------------------
!
! do
!
!    breakdown = 0
!    iter = 0
!    initialize
!
!    do iter = iter+1,nhiter
!      calculate
!      if (BiCGSTAB breaks down) then
!         breakdown = 1
!         exit
!      endif
!      if (solution is accurate enough) then exit
!    enddo
!
!    if (breakdown == 0) then exit
!    if (iter == nhiter) then exit
!   
! enddo
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer     , pointer :: m1_nhy
    integer     , pointer :: m2_nhy
    integer     , pointer :: n1_nhy
    integer     , pointer :: n2_nhy
    integer     , pointer :: nhiter
    logical     , pointer :: l2norm
    real(fp)    , pointer :: epsnh
    real(fp)    , pointer :: rel_epsnh
    character(8), pointer :: precon
    integer     , pointer :: lundia
!
! Global variables
!
    integer                                                      :: icx
    integer                                                      :: icy
    integer                                                      :: kmax      !  Description and declaration in esm_alloc_int.f90
    integer                                                      :: nmmax     !  Description and declaration in dimens.igs
    integer                                         , intent(in) :: nst
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                   :: kfs       !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                   :: kfsmin    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                   :: kfsmx0    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: kfsz0     !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: aak
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: aak2
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: vj
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: bbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: bbka
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: bbkc
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: cck
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: cck2
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: ddk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: dinv
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: p1        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: pbbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: pbbkc
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: pj
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: rjshadow  ! Extra array for BiCGSTAB
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: sj        ! Extra array for BiCGSTAB
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: sjprec    ! Extra array for BiCGSTAB
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: tj        ! Extra array for BiCGSTAB
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: pnhcor    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: rj
!
! Local variables
!
    integer            :: ddb
    integer            :: icxy
    integer            :: iter
    integer            :: k
    integer            :: m
    integer            :: ndelta
    integer            :: nm
    integer            :: nmst
    integer            :: nmstart
    integer            :: breakdown      ! is 0 if no breakdown has occured
    integer            :: maxbreakdowns  ! maximum number of breakdowns until the calculation is aborted
    integer            :: ilu            ! use ILU if ilu == 0, otherwise LU of tridiag(A)
    real(fp)           :: alpha
    real(fp)           :: beta
    real(fp)           :: rho_new
    real(fp)           :: rho_old
    real(fp)           :: omega
    real(fp)           :: conv
    real(fp)           :: rk         ! unscaled equations, updated residual norm
    real(fp)           :: rk0        ! unscaled equations, updated residual norm
    real(fp)           :: rkinf      ! unscaled equations, updated residual norm
    real(fp)           :: rkinf0     ! unscaled equations, updated residual norm
    real(fp)           :: rktrue     ! unscaled equations, true residual 2-norm
    real(fp)           :: rkinftrue  ! unscaled equations, true residual inf-norm
    real(fp)           :: normb      ! unscaled equations, 2-norm of right hand side
    real(fp)           :: normbinf   ! unscaled equations, inf-norm of right hand side
    real(fp), external :: z_ainpro
    character(30)      :: errtxt
!
!! executable statements -------------------------------------------------------
!
    m1_nhy    => gdp%gdnonhyd%m1_nhy
    m2_nhy    => gdp%gdnonhyd%m2_nhy
    n1_nhy    => gdp%gdnonhyd%n1_nhy
    n2_nhy    => gdp%gdnonhyd%n2_nhy
    nhiter    => gdp%gdnonhyd%nhiter
    l2norm    => gdp%gdnonhyd%l2norm
    epsnh     => gdp%gdnonhyd%epsnh
    rel_epsnh => gdp%gdnonhyd%rel_epsnh
    precon    => gdp%gdnonhyd%precon
    lundia    => gdp%gdinout%lundia
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    errtxt  = '         ;NHITER:             '
    ndelta  = n2_nhy - n1_nhy
    nmstart = (n1_nhy+ddb) + (m1_nhy-1+ddb)*icxy
    !
    pj   = 0.0_fp
    iter = 0
    !
    ! Enter restart-loop. Runs as long as there is a breakdown.
    ! Usually the loop is run only once, because breakdowns should be rare.
    !
    do
       breakdown = 0
       !
       ! Compute residual, shadow residual and initial search direction from rj.
       !
       call z_matpro(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                   & bbka      ,bbkc      ,kmax      ,icx       ,icy       , &
                   & nmmax     ,kfsz0     ,pnhcor    ,rj        ,kfs       , &
                   & kfsmin    ,kfsmx0    ,gdp       )
       do m = m1_nhy, m2_nhy
          nmst = nmstart + (m-m1_nhy)*icxy
          do nm = nmst, nmst+ndelta
             if (kfs(nm) == 1) then
                do k = 1, kmax
                   if (kfsz0(nm,k) /= 0) then
                      rj      (nm,k) = ddk(nm,k) - rj(nm,k)
                      rjshadow(nm,k) = rj (nm,k)
                      pj      (nm,k) = rj (nm,k)
                   endif
                enddo
             endif
          enddo
       enddo
       !
       ! Compute norm of the right hand side.
       !
       normb    = 0.0_fp
       normbinf = 0.0_fp
       do m = m1_nhy, m2_nhy
          nmst = nmstart + (m-m1_nhy)*icxy
          do nm = nmst, nmst+ndelta
             if (kfs(nm) == 1) then
                do k = 1, kmax
                   if (kfsz0(nm,k) /= 0) then
                      normb = normb + ddk(nm,k)*ddk(nm,k)/(dinv(nm,k)*dinv(nm,k))
                      if (abs(ddk(nm,k)/dinv(nm,k)) > abs(normbinf)) then
                         normbinf = abs(ddk(nm,k) / dinv(nm,k))
                      endif
                   endif
                enddo
             endif
          enddo
       enddo
       normb = sqrt(normb)
       !
       ! Compute initial rho.
       !
       rho_new = z_ainpro(rj, rjshadow, kmax, kfs, kfsz0, icx, icy, gdp)
       !
       !  Start iteration loop. When the loop is entered the first time, iter == 1.
       !  We start with iter+1. In the worst case, when BiCGSTAB breaks down every time
       !  it enters this loop, we ensure maximal 'nhiter' restarts of the algorithm.
       !  Noted that array p1 contains preconditioned search direction.
       !
       do iter = iter+1,nhiter
          !
          ! Calculate the norms of the updated, unscaled residual.
          !
          rk    = 0.0_fp
          rkinf = 0.0_fp
          do m = m1_nhy, m2_nhy
             nmst = nmstart + (m-m1_nhy)*icxy
             do nm = nmst, nmst+ndelta
                if (kfs(nm) == 1) then
                   do k = 1, kmax
                      if (kfsz0(nm,k) /= 0) then
                         rk = rk + rj(nm,k)*rj(nm,k)/(dinv(nm,k)*dinv(nm,k))
                         if (abs(rj(nm, k)/dinv(nm, k)) > abs(rkinf)) then
                            rkinf = abs(rj(nm,k) / dinv(nm,k))
                         endif
                      endif
                   enddo
                endif
             enddo
          enddo
          rk = sqrt(rk)
          ! 
          ! Store initial residual norms.
          !
          if (iter==1) then
             if (rk == 0.0d0) then
                !
                ! Needed in case whole domain is dry
                ! Avoid division by zero, but assure convergence
                !
                rk = 0.1_fp*epsnh
             endif
             rk0    = rk
             if (rkinf == 0.0d0) then
                !
                ! Needed in case whole domain is dry
                ! Avoid division by zero, but assure convergence
                !
                rkinf = 0.1_fp*epsnh
             endif
             rkinf0 = rkinf
          endif
          !
          ! print convergence behaviour
          ! print *, 'number of iterations in CG:', iter
          !
          ! If residual norm is small enough, then stop.
          ! Note that if the iter == 1 and the residual is already small enough,
          ! we don't need to iterate at all.
          !
          if (l2norm) then
             if (rk < normb*rel_epsnh) exit
             if (rk < epsnh) exit
          else
             if (rkinf < normbinf*rel_epsnh) exit
             if (rkinf < epsnh) exit
          endif
          !
          ! Calculate the preconditioned search direction and store it in p1.
          !
          if (precon == 'ilu') then
             call z_precon_ilu(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                             & bbka      ,bbkc      ,kmax      ,icx       ,icy       , &
                             & nmmax     ,kfsz0     ,pj        ,p1        ,kfs       , &
                             & kfsmin    ,kfsmx0    ,gdp       )
          endif
          if (precon == 'tridiag') then
             call z_precon(bbka      ,bbkc      ,pbbk      ,pbbkc     ,kmax      , &
                         & icx       ,icy       ,nmmax     ,kfsz0     ,pj        , &
                         & p1        ,kfs       ,kfsmin    ,kfsmx0    ,gdp       )
          endif
          if (precon=='none' .or. precon=='diag') then
             do m = m1_nhy, m2_nhy
                nmst = nmstart + (m-m1_nhy)*icxy
                do nm = nmst, nmst+ndelta
                   if (kfs(nm) == 1) then
                      do k = 1, kmax
                         if (kfsz0(nm,k) /= 0) then   
                            p1(nm,k) = pj(nm,k)
                         endif
                      enddo
                   endif
                enddo
             enddo
          endif
          !
          ! Calculate A*p1 and store the result in vj.
          !
          call z_matpro(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                      & bbka      ,bbkc      ,kmax      ,icx       ,icy       , &
                      & nmmax     ,kfsz0     ,p1        ,vj        ,kfs       , &
                      & kfsmin    ,kfsmx0    ,gdp       )
          alpha = rho_new / z_ainpro(rjshadow, vj, kmax, kfs, kfsz0, icx, icy, gdp)
          !
          ! Calculate scaled intermediate residual sj.
          !
          do m = m1_nhy, m2_nhy
             nmst = nmstart + (m-m1_nhy)*icxy
             do nm = nmst, nmst + ndelta
                if (kfs(nm) == 1) then
                   do k = 1, kmax
                      if (kfsz0(nm,k) /= 0) then
                         sj(nm,k) = rj(nm,k) - alpha*vj(nm,k)
                      endif
                   enddo
                endif
             enddo
          enddo
          !
          ! Calculate the preconditioned search direction and store it in sjprec.
          !
          if (precon == 'ilu') then
             call z_precon_ilu(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                             & bbka      ,bbkc      ,kmax      ,icx       ,icy       , &
                             & nmmax     ,kfsz0     ,sj        ,sjprec    ,kfs       , &
                             & kfsmin    ,kfsmx0    ,gdp       )
          endif
          if (precon == 'tridiag') then
             call z_precon(bbka      ,bbkc      ,pbbk      ,pbbkc     ,kmax      , &
                         & icx       ,icy       ,nmmax     ,kfsz0     ,sj        , &
                         & sjprec    ,kfs       ,kfsmin    ,kfsmx0    ,gdp       )
          endif
          if (precon=='none' .or. precon=='diag') then
             do m = m1_nhy, m2_nhy
                nmst = nmstart + (m-m1_nhy)*icxy
                do nm = nmst, nmst+ndelta
                   if (kfs(nm) == 1) then
                      do k = 1, kmax
                         if (kfsz0(nm,k) /= 0) then
                            sjprec(nm,k) = sj(nm,k)
                         endif
                      enddo
                   endif
                enddo
             enddo
          endif
          !
          ! Calculate A*sjprec and store the result in tj.
          !
          call z_matpro(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                      & bbka      ,bbkc      ,kmax      ,icx       ,icy       , &
                      & nmmax     ,kfsz0     ,sjprec    ,tj        ,kfs       , &
                      & kfsmin    ,kfsmx0    ,gdp       )
          omega = z_ainpro(tj, sj, kmax, kfs, kfsz0, icx, icy, gdp)
          omega = omega / z_ainpro(tj, tj, kmax, kfs, kfsz0, icx, icy, gdp)
          if (omega == 0) then
             !
             ! We check if omega is zero because we do not want to divide by zero.
             !
             breakdown = 1
             write (lundia,'(a)') 'RESTARTING: omega is zero, break down'
             exit  ! iteration loop
          endif
          !
          ! Calculate scaled solution pnhcor and scaled residual rj.
          !
          do m = m1_nhy, m2_nhy
             nmst = nmstart + (m-m1_nhy)*icxy
             do nm = nmst, nmst+ndelta
                if (kfs(nm) == 1) then
                   do k = 1, kmax
                      if (kfsz0(nm,k) /= 0) then
                         pnhcor(nm,k) = pnhcor(nm,k) + alpha*p1(nm,k) + omega*sjprec(nm,k)
                         rj(nm,k)     = sj(nm,k) - omega*tj(nm,k)
                      endif
                   enddo
                endif
             enddo
          enddo
          rho_old = rho_new
          rho_new = z_ainpro(rjshadow, rj, kmax, kfs, kfsz0, icx, icy, gdp)
          if (rho_new == 0) then
             !
             ! We check if rho_new is zero because we do not want to divide by zero.
             !
             breakdown = 1
             write (lundia,'(a)') 'RESTARTING: rho is zero, break down'
             exit  ! iteration loop
          endif
          beta = (rho_new/rho_old) * (alpha/omega)
          !
          ! compute new search direction
          !
          do m = m1_nhy, m2_nhy
             nmst = nmstart + (m-m1_nhy)*icxy
             do nm = nmst, nmst+ndelta
                if (kfs(nm) == 1) then
                   do k = 1, kmax
                      if (kfsz0(nm,k) /= 0) then
                         pj(nm,k) = rj(nm,k) + beta*(pj(nm,k) - omega*vj(nm,k))
                      endif
                   enddo
                endif
             enddo
          enddo
          !
          ! convergence check
          !
          if (iter == nhiter) then
             write (errtxt(1:8), '(i8)') nst
             write (errtxt(25:30), '(i6)') nhiter
             call prterr(lundia    ,'Z021'    ,trim(errtxt)    )
             exit
          endif
       enddo
       !
       ! Calculate A*pnhcor and store the result in rj.
       !
       call z_matpro(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                   & bbka      ,bbkc      ,kmax      ,icx       ,icy       , &
                   & nmmax     ,kfsz0     ,pnhcor    ,rj        ,kfs       , &
                   & kfsmin    ,kfsmx0    ,gdp       )
       !
       ! Calculate the true residual and store it in rj.
       !
       do m = m1_nhy, m2_nhy
          nmst = nmstart + (m-m1_nhy)*icxy
          do nm = nmst, nmst+ndelta
             if (kfs(nm) == 1) then
                do k = 1, kmax
                   if (kfsz0(nm,k) /= 0) then
                      rj(nm,k) = (ddk(nm,k)-rj(nm,k)) / dinv(nm,k)
                   endif
                enddo
             endif
          enddo
       enddo
       !
       ! Calculate the norms of rj.
       !
       rktrue    = 0.0_fp
       rkinftrue = 0.0_fp
       do m = m1_nhy, m2_nhy
          nmst = nmstart + (m-m1_nhy)*icxy
          do nm = nmst, nmst+ndelta
             if (kfs(nm) == 1) then
                do k = 1, kmax
                   if (kfsz0(nm,k) /= 0) then
                      rktrue = rktrue + rj(nm,k)*rj(nm,k)/(dinv(nm,k)*dinv(nm,k))
                      if (abs(rj(nm,k)/dinv(nm,k)) > abs(rkinftrue)) then
                         rkinftrue = abs(rj(nm,k)/dinv(nm,k))
                      endif
                   endif
                enddo
             endif
          enddo
       enddo
       rktrue = sqrt(rktrue)
       !
       ! Check if the true residual norm is smaller than the updated residual norm.
       ! The updated residual norm may have accumulated roundoff errors, especially in
       ! case the method nearly suffered from break down. That's why it is 
       ! generally not not save to trust the updated residual as a stopping criterium.
       !
       if (l2norm) then
          if (rktrue>normb*rel_epsnh .or. rktrue>epsnh) then
             breakdown = 1
             write (lundia,'(a)') 'RESTARTING: 2-norm of true residual is too large'
          endif
       else
          if (rkinftrue>normbinf*rel_epsnh .or. rkinftrue>epsnh) then
             breakdown = 1
             write (lundia,'(a)') 'RESTARTING: inf-norm of true residual is too large'
          endif
       endif
       !
       ! Don't restart if there was no breakdown.
       !
       if (breakdown == 0) exit
       !
       ! Don't restart if the maximum number of iterations is reached.
       !
       if (iter == nhiter) exit   
       !
       ! Restart, because there was a break down and the maximum number of iteration is not yet reached.
       !
    enddo
    !
    ! Now convergence or too many iterations or too many breakdowns.
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m-m1_nhy)*icxy
       do nm = nmst, nmst+ndelta
          if (kfs(nm) == 1) then
             do k = 1, kmax
                if (kfsz0(nm,k) /= 0) then
                   p1(nm,k) = pnhcor(nm,k)
                else
                   p1(nm,k) = 0.0_fp
                endif
             enddo
          endif
       enddo
    enddo
    !
    ! compute (unscaled) solution
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m-m1_nhy)*icxy
       do nm = nmst, nmst+ndelta
          if (kfs(nm) == 1) then
             do k = 1, kmax
                if (kfsz0(nm,k) /= 0) then
                   p1(nm,k) = p1(nm,k) * dinv(nm,k)
                endif
             enddo
          endif
       enddo
    enddo
    !
    ! print convergence behaviour
    !
   if (l2norm) then
      conv = rk / rk0
      conv = conv**(1.0/max(1,iter))
         write (lundia,'(a,f5.2,a,i4,4(a,e10.3))') 'conv (L2 norm)=', &
              & conv,'  iter=',iter,' ',rk0,' ',rk,' ',rkinf0,' ',rkinf
   else
      conv = rk / rk0
      conv = conv**(1.0/max(1,iter))
         write (lundia,'(a,f5.2,a,i4,4(a,e10.3))') 'conv (Linf norm)=', &
              & conv,'  iter=',iter,' ',rk0,' ',rk,' ',rkinf0,' ',rkinf
   endif
end subroutine z_solbicgstab
