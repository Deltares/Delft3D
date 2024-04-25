subroutine z_solcg(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
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
    integer  , pointer :: m1_nhy
    integer  , pointer :: m2_nhy
    integer  , pointer :: n1_nhy
    integer  , pointer :: n2_nhy
    integer  , pointer :: nhiter
    real(fp) , pointer :: epsnh
    logical  , pointer :: l2norm
    integer  , pointer :: lundia
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
    integer                  :: ddb
    integer                  :: icxy
    integer                  :: iter
    integer                  :: k
    integer                  :: m
    integer                  :: ndelta
    integer                  :: nm
    integer                  :: nmst
    integer                  :: nmstart
    real(fp)                 :: alphaj
    real(fp)                 :: alphan
    real(fp)                 :: alphat
    real(fp)                 :: betaj
    real(fp)                 :: betat
    real(fp)                 :: conv
    real(fp)                 :: rk
    real(fp)                 :: rk0
    real(fp)                 :: rkinf
    real(fp)                 :: rkinf0
    real(fp)      , external :: z_ainpro
    character(30)            :: errtxt
!
!! executable statements -------------------------------------------------------
!
    lundia  => gdp%gdinout%lundia
    m1_nhy  => gdp%gdnonhyd%m1_nhy
    m2_nhy  => gdp%gdnonhyd%m2_nhy
    n1_nhy  => gdp%gdnonhyd%n1_nhy
    n2_nhy  => gdp%gdnonhyd%n2_nhy
    nhiter  => gdp%gdnonhyd%nhiter
    epsnh   => gdp%gdnonhyd%epsnh
    l2norm  => gdp%gdnonhyd%l2norm
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    errtxt  = '         ;NHITER:             '
    ndelta  = n2_nhy - n1_nhy
    nmstart = (n1_nhy + ddb) + (m1_nhy - 1 + ddb)*icxy
    !
    !  compute inner product of initial residu
    !
    alphat = z_ainpro(rj, p1, kmax, kfs, kfsz0, icx, icy, gdp)
    !
    ! Start iteration process
    ! Noted that array p1 contains A_inv * rj
    !
    iter   = 0
    rk0    = 0.0_fp
    rkinf0 = 0.0_fp
  111 continue
    iter = iter + 1
    !
    rk    = 0.0_fp
    rkinf = 0.0_fp
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          do k = 1, kmax
             if (kfsz0(nm, k) /= 0) then
                if (l2norm) then
                   rk = rk + rj(nm, k)*rj(nm, k)
                endif
                if (abs(rj(nm, k)) > rkinf) then
                   rkinf = abs(rj(nm, k))
                endif
             endif
          enddo
       enddo
    enddo
    !
    if (iter == 1) then
       if (rk == 0.0_fp) then
          !
          ! Needed in case whole domain is dry
          ! Avoid division by zero, but assure convergence
          !
          rk    = 0.1_fp*epsnh
       endif
       rk0      = rk
       if (rkinf == 0.0_fp) then
          !
          ! Needed in case whole domain is dry
          ! Avoid division by zero, but assure convergence
          !
          rkinf = 0.1_fp*epsnh
       endif
       rkinf0   = rkinf
    endif
    ! 
    ! convergence check: ||r_k|| / ||r_k_init|| < eps
    ! Use L2NORM or Linfinity norm
    !
    if (l2norm) then
       if ((sqrt(rk)<sqrt(rk0)*epsnh .and. iter>0) .or. rkinf<1.0e-2) then
          goto 999
       endif
    else
       if ((rkinf<rkinf0*epsnh .and. iter>0) .or. rkinf<1.0e-2) then
          goto 999
       endif
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
    alphaj = alphat/alphan
    !
    ! next iteration
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          do k = 1, kmax
             if (kfsz0(nm, k) /= 0) then
                pnhcor(nm, k) = pnhcor(nm, k) + alphaj*pj(nm, k)
                rj(nm, k)     = rj(nm, k) - alphaj*apj(nm, k)
             endif
          enddo
       enddo
    enddo
    !
    ! compute the preconditioner (and store the result in array p1)
    !
    call z_precon(bbka      ,bbkc      ,pbbk      ,pbbkc     ,kmax      , &
                & icx       ,icy       ,nmmax     ,kfsz0     ,rj        , &
                & p1        ,kfs       ,kfsmin    ,kfsmx0    ,gdp       )
    betat = z_ainpro(rj, p1, kmax, kfs, kfsz0, icx, icy, gdp)
    betaj = betat / alphat
    !
    ! compute new search direction
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          do k = 1, kmax
             if (kfsz0(nm, k) /= 0) then
                pj(nm, k) = p1(nm, k) + betaj*pj(nm, k)
             endif
          enddo
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
       goto 999
    endif
    goto 111
    !
    ! now convergence
    !
  999 continue
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          do k = 1, kmax
             if (kfsz0(nm, k) /= 0) then
                p1(nm, k) = pnhcor(nm, k)
             else
                p1(nm, k) = 0.0_fp
             endif
          enddo
       enddo
    enddo
    !
    ! compute solution x (by M(-1) p1, see z_initcg)
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          do k = 1, kmax
             if (kfsz0(nm, k) /= 0) then
                p1(nm, k) = p1(nm, k) * dinv(nm, k)
             endif
          enddo
       enddo
    enddo
    !
    ! print convergence behaviour
    ! print *, 'number of iterations in CG:', iter
    !
    if (l2norm) then
       conv = sqrt(rk) / sqrt(rk0)
       conv = conv**(1.0_fp/real(max(1, iter),fp))
       !write (lundia,*) 'conv-l2', conv,iter,sqrt(rk0),sqrt(rk),rkinf
    else
       conv = rkinf / rkinf0
       conv = conv**(1.0_fp/real(max(1, iter),fp))
       !write (lundia,*) 'conv-linf', conv,iter,rkinf0,rkinf
    endif
end subroutine z_solcg
