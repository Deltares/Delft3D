subroutine z_solcg_nhfull(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                        & bbka      ,bbkc      ,ddk       ,kmax      ,icx       , &
                        & icy       ,nmmax     ,nst       ,kfsz0     ,pnhcor    , &
                        & pj        ,rj        ,apj       ,dinv      ,pbbk      , &
                        & pbbkc     ,p1        ,kfs       ,kfsmin    ,kfsmx0    , &
                        & gdp       )
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
! CG-method.
!
!!--pseudo code and references--------------------------------------------------
! NONE
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
    real(fp)    , pointer :: epsnh
    real(fp)    , pointer :: rel_epsnh
    logical     , pointer :: l2norm
    character(8), pointer :: precon
    integer     , pointer :: lundia
!
! Global variables
!
    integer                                                      :: icx
    integer                                                      :: icy
    integer                                                      :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                      :: nmmax  !  Description and declaration in dimens.igs
    integer                                         , intent(in) :: nst
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                   :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: kfsz0  !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: aak
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: aak2
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: apj
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: bbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: bbka
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: bbkc
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: cck
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: cck2
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: ddk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: dinv
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: p1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: pbbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: pbbkc
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: pj
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: pnhcor !  Description and declaration in esm_alloc_real.f90
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
    real(fp)           :: alphaj
    real(fp)           :: alphan
    real(fp)           :: alphat
    real(fp)           :: betaj
    real(fp)           :: betat
    real(fp)           :: conv
    real(fp)           :: rk
    real(fp)           :: rk0
    real(fp)           :: rkinf
    real(fp)           :: rkinf0
    real(fp)           :: normb      ! unscaled equations, 2-norm of right hand side
    real(fp)           :: normbinf   ! unscaled equations, inf-norm of right hand side
    real(fp), external :: z_ainpro
    character(30)      :: errtxt
!
!! executable statements -------------------------------------------------------
!
    lundia    => gdp%gdinout%lundia
    m1_nhy    => gdp%gdnonhyd%m1_nhy
    m2_nhy    => gdp%gdnonhyd%m2_nhy
    n1_nhy    => gdp%gdnonhyd%n1_nhy
    n2_nhy    => gdp%gdnonhyd%n2_nhy
    nhiter    => gdp%gdnonhyd%nhiter
    epsnh     => gdp%gdnonhyd%epsnh
    l2norm    => gdp%gdnonhyd%l2norm
    precon    => gdp%gdnonhyd%precon
    rel_epsnh => gdp%gdnonhyd%rel_epsnh
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    errtxt  = '         ;NHITER:             '
    ndelta  = n2_nhy - n1_nhy
    nmstart = (n1_nhy + ddb) + (m1_nhy - 1 + ddb)*icxy
    !
    ! compute residu (store matrix vector product in array rj)
    !
    call z_matpro(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                & bbka      ,bbkc      ,kmax      ,icx       ,icy       , &
                & nmmax     ,kfsz0     ,pnhcor    ,rj        ,kfs       , &
                & kfsmin    ,kfsmx0    ,gdp       )
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          if (kfs(nm) == 1) then
             do k = 1, kmax
                if (kfsz0(nm,k) /= 0) then
                   rj(nm, k) = ddk(nm, k) - rj(nm, k)
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
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          if (kfs(nm) == 1) then
             do k = 1, kmax
                if (kfsz0(nm,k) /= 0) then
                   normb = normb + ddk(nm, k)*ddk(nm, k)/(dinv(nm, k)*dinv(nm, k))
                   if (abs(ddk(nm,k)/dinv(nm,k)) > abs(normbinf)) then
                      normbinf = abs(ddk(nm, k)/dinv(nm, k))
                   endif
                endif
             enddo
          endif
       enddo
    enddo
    normb = sqrt(normb)
    !
    ! Calculate the preconditioned residual and store it in p1.
    !
    if (precon == 'ilu') then
       call z_precon_ilu(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                       & bbka      ,bbkc      ,kmax      ,icx       ,icy       , &
                       & nmmax     ,kfsz0     ,rj        ,p1        ,kfs       , &
                       & kfsmin    ,kfsmx0    ,gdp       )
    endif
     if (precon == 'tridiag') then
       call z_precon(bbka      ,bbkc      ,pbbk      ,pbbkc     ,kmax      , &
                   & icx       ,icy       ,nmmax     ,kfsz0     ,rj        , &
                   & p1        ,kfs       ,kfsmin    ,kfsmx0    ,gdp       )
    endif
    if ((precon=='none') .or. (precon=='diag')) then
       do m = m1_nhy, m2_nhy
          nmst = nmstart + (m - m1_nhy)*icxy
          do nm = nmst, nmst + ndelta
             if (kfs(nm) == 1) then
                do k = 1, kmax
                   if (kfsz0(nm,k) /= 0) then
                      p1(nm, k) = rj(nm,k)
                   endif
                enddo
             endif
          enddo
       enddo
    endif
    !
    !  compute (initial) search direction pj
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          if (kfs(nm) == 1) then
             do k = 1, kmax
                if (kfsz0(nm,k) /= 0) then
                   pj(nm,k) = p1(nm,k)
                endif
             enddo
          endif
       enddo
    enddo
    !
    !  compute inner product of initial residu
    !
    alphat = z_ainpro(rj, p1, kmax, kfs, kfsz0, icx, icy, gdp)
    !
    !  start iteration process
    !  Noted that array p1 contains A_inv * rj
    !
    iter = 0
    do iter = iter+1,nhiter
       rk    = 0.0_fp
       rkinf = 0.0_fp
       do m = m1_nhy, m2_nhy
          nmst = nmstart + (m - m1_nhy)*icxy
          do nm = nmst, nmst + ndelta
             if (kfs(nm) == 1) then
                do k = 1, kmax
                   if (kfsz0(nm,k) /= 0) then
                      rk = rk + rj(nm, k)*rj(nm, k)/(dinv(nm, k)*dinv(nm, k))
                      if (abs(rj(nm,k)/dinv(nm, k)) > abs(rkinf)) then
                         rkinf = abs(rj(nm,k) / dinv(nm,k))
                      endif
                   endif
                enddo
             endif
          enddo
       enddo
       rk = sqrt(abs(rk))
       if (iter == 1) then
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
       ! convergence check
       ! Use L2NORM or Linfinity norm
       !
       if (l2norm) then
          if (rk < normb*rel_epsnh) exit
          if (rk < epsnh) exit
       else
         if (rkinf < normbinf*rel_epsnh) exit
         if (rkinf < epsnh) exit
       endif
       !
       ! product matrix vector
       !
       call z_matpro(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                   & bbka      ,bbkc      ,kmax      ,icx       ,icy       , &
                   & nmmax     ,kfsz0     ,pj        ,apj       ,kfs       , &
                   & kfsmin    ,kfsmx0    ,gdp       )
       !
       ! inner product
       !
       alphan = z_ainpro(pj, apj, kmax, kfs, kfsz0, icx, icy, gdp)
       alphaj = alphat / alphan
       !
       ! next iteration
       !
       do m = m1_nhy, m2_nhy
          nmst = nmstart + (m - m1_nhy)*icxy
          do nm = nmst, nmst + ndelta
             if (kfs(nm) == 1) then
                do k = 1, kmax
                   if (kfsz0(nm,k) /= 0) then
                      pnhcor(nm,k) = pnhcor(nm,k) + alphaj*pj (nm,k)
                      rj(nm,k)     = rj(nm,k)     - alphaj*apj(nm,k)
                   endif
                enddo
             endif
          enddo
       enddo
       !
       ! Calculate the preconditioned residual and store it in p1.
       !
       if (precon == 'ilu') then
          call z_precon_ilu(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                          & bbka      ,bbkc      ,kmax      ,icx       ,icy       , &
                          & nmmax     ,kfsz0     ,rj        ,p1        ,kfs       , &
                          & kfsmin    ,kfsmx0    ,gdp       )
       endif
       if (precon == 'tridiag') then
          call z_precon(bbka      ,bbkc      ,pbbk      ,pbbkc     ,kmax      , &
                      & icx       ,icy       ,nmmax     ,kfsz0     ,rj        , &
                      & p1        ,kfs       ,kfsmin    ,kfsmx0    ,gdp       )
       endif
       if ((precon == 'none') .or. (precon == 'diag')) then
          do m = m1_nhy, m2_nhy
             nmst = nmstart + (m - m1_nhy)*icxy
             do nm = nmst, nmst + ndelta
                if (kfs(nm) == 1) then
                   do k = 1, kmax
                      if (kfsz0(nm,k) /= 0) then
                         p1(nm,k) = rj(nm,k)
                      endif
                   enddo
                endif
             enddo
          enddo
       endif
       betat = z_ainpro(rj, p1, kmax, kfs, kfsz0, icx, icy, gdp)
       betaj = betat / alphat
       do m = m1_nhy, m2_nhy
          nmst = nmstart + (m - m1_nhy)*icxy
          do nm = nmst, nmst + ndelta
             if (kfs(nm) == 1) then
                do k = 1, kmax
                   if (kfsz0(nm,k) /= 0) then
                      pj(nm,k) = p1(nm,k) + betaj*pj(nm,k)
                   endif
                enddo
             endif
          enddo
       enddo
       !
       ! copy for the next iteration
       !
       alphat = betat
       !
       ! convergence check
       !
       if (iter > nhiter) then
          write (errtxt(1:8), '(i8)') nst
          write (errtxt(25:30), '(i6)') nhiter
          call prterr(lundia    ,'Z021'    ,trim(errtxt)    )
          exit
       endif
    enddo
    !
    ! now convergence
    !
    ! Calculate the true residual and store it in rj.
    !
    call z_matpro(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                & bbka      ,bbkc      ,kmax      ,icx       ,icy       , &
                & nmmax     ,kfsz0     ,pnhcor    ,rj        ,kfs       , & 
                & kfsmin    ,kfsmx0    ,gdp       )
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
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
    rk    = 0.0_fp
    rkinf = 0.0_fp
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          if (kfs(nm) == 1) then
             do k = 1, kmax
                if (kfsz0(nm,k) /= 0) then
                   rk = rk + rj(nm,k)*rj(nm,k)/(dinv(nm,k)*dinv(nm,k))
                   if (abs(rj(nm,k)/dinv(nm,k)) > abs(rkinf)) then
                      rkinf = abs(rj(nm,k) / dinv(nm,k))
                   endif
                endif
             enddo
          endif
       enddo
    enddo
    rk = sqrt(rk)
    !
    ! Compute true solution.
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          if (kfs(nm) == 1) then
             do k = 1, kmax
                if (kfsz0(nm,k) /= 0) then
                   p1(nm,k) = pnhcor(nm,k) * dinv(nm,k)
                else
                   p1(nm,k) = 0.0_fp
                endif
             enddo
          endif
       enddo
    enddo
    !
    ! print convergence behaviour
    ! print *, 'number of iterations in CG:', iter
    !
   if (l2norm) then
      conv = rk / rk0
      conv = conv**(1.0_fp/real(max(1, iter),fp))
         write (lundia,'(a,f5.2,a,i4,4(a,e10.3))') 'conv (L2 norm)=', &
              & conv,'  iter=',iter,' ',rk0,' ',rk,' ',rkinf0,' ',rkinf
   else
      conv = rk / rk0
      conv = conv**(1.0_fp/real(max(1, iter),fp))
         write (lundia,'(a,f5.2,a,i4,4(a,e10.3))') 'conv (Linf norm)=', &
              & conv,'  iter=',iter,' ',rk0,' ',rk,' ',rkinf0,' ',rkinf
   endif
end subroutine z_solcg_nhfull
