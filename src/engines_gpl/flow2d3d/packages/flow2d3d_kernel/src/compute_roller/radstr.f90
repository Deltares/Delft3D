subroutine radstr(ewave1    ,eroll1    ,sinkr     ,c         ,cgc       , &
                & dir       ,wsu       ,wsv       ,fxw       ,fyw       , &
                & alfas     ,x         ,y         ,guv       ,gvu       , &
                & guu       ,gvv       ,gsqs      ,kfs       ,kcs       , &
                & nmax      ,mmax      ,norow     ,nocol     ,irocol    , &
                & wavcmp    ,s0        ,dps       ,hu        ,hv        , &
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
! calculation of wave forces from the divergence of
! the stress tensor. The components of the stress
! tensor can be obtained from the energy density
! See "Extension of SURFBEAT model to two dimensions" by H. Petit
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer      , pointer :: nmskf
    integer      , pointer :: nmskl
    integer      , pointer :: mmskf
    integer      , pointer :: mmskl
    real(fp)     , pointer :: dryflc
    logical      , pointer :: wavfrc
!
! Global variables
!
    integer                                                        , intent(in) :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                        , intent(in) :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                        , intent(in) :: nocol  !  Description and declaration in esm_alloc_int.f90
    integer                                                        , intent(in) :: norow  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(5, norow + nocol)                        , intent(in) :: irocol !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    logical                                                        , intent(in) :: wavcmp
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: alfas  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: c      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: cgc    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: dir
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: eroll1 !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: ewave1 !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)             :: wsu
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)             :: fxw    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)             :: wsv
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)             :: fyw    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: sinkr  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: x
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: y
!
! Local variables
!
    integer                              :: i
    integer                              :: ic
    integer                              :: j
    integer                              :: kmp1
    integer                              :: knp1
    integer                              :: m
    integer                              :: m0
    integer                              :: m1
    integer                              :: mp1
    integer                              :: n
    integer                              :: n0
    integer                              :: n1
    integer                              :: np1
    integer                              :: nsh
    real(fp)                             :: al
    real(fp)                             :: alu
    real(fp)                             :: alv
    real(fp)                             :: ca
    real(fp)                             :: cau
    real(fp)                             :: cav
    real(fp)                             :: costet
    real(fp)                             :: d1
    real(fp)                             :: d2
    real(fp)                             :: ds1k1u
    real(fp)                             :: ds1k1v
    real(fp)                             :: ds1k2u
    real(fp)                             :: ds1k2v
    real(fp)                             :: ds2k1u
    real(fp)                             :: ds2k1v
    real(fp)                             :: ds2k2u
    real(fp)                             :: ds2k2v
    real(fp)                             :: ds3k1u
    real(fp)                             :: ds3k1v
    real(fp)                             :: ds3k2u
    real(fp)                             :: ds3k2v
    real(fp)                             :: dxk1u
    real(fp)                             :: dxk1v
    real(fp)                             :: dxk2u
    real(fp)                             :: dxk2v
    real(fp)                             :: dyk1u
    real(fp)                             :: dyk1v
    real(fp)                             :: dyk2u
    real(fp)                             :: dyk2v
    real(fp)                             :: en
    real(fp)                             :: f1
    real(fp)                             :: f2
    real(fp)                             :: hnm
    real(fp)                             :: hnmp1
    real(fp)                             :: hnp1m
    real(fp)                             :: inm
    real(fp)                             :: inmp1
    real(fp)                             :: inp1m
    real(fp)                             :: sa
    real(fp)                             :: sau
    real(fp)                             :: sav
    real(fp)                             :: sintet
    real(fp)                             :: tau
    real(fp)                             :: teta
    real(fp), dimension(1:3, -1:1, -1:1) :: sr
    real(fp), dimension(1:3, -1:1, -1:1) :: sw
    real(fp), external                   :: le
    real(fp), external                   :: tapf
!
!! executable statements -------------------------------------------------------
!
    nmskf     => gdp%gdbcdat%nmskf
    nmskl     => gdp%gdbcdat%nmskl
    mmskf     => gdp%gdbcdat%mmskf
    mmskl     => gdp%gdbcdat%mmskl
    dryflc    => gdp%gdnumeco%dryflc
    wavfrc    => gdp%gdbetaro%wavfrc
    !                                          +
    !
    !                                          g
    !    o-----gvv------o               o------u-------o
    !    |              |               |      v       |
    !    |              g               |              |
    !    |      +       u               |      +      gvu      +
    !    |              u               |              |
    !    |              |               |              |
    !    o--------------o               o--------------o
    !
    do m = 1, mmax - 1
       do n = 1, nmax - 1
          if (kfs(n, m)==1.and. kcs(n, m)==1) then
             al  = alfas(n,m)*degrad   ! angle at m,n point
             sa  = sin(al)
             ca  = cos(al)

             if (kfs(n,m+1)>=1) then
                alu = alfas(n,m+1)*degrad
                sau = ( sa  + sin(alu) ) * 0.5
                cau = ( ca  + cos(alu) ) * 0.5
             else
                sau =   sa
                cau =   ca
             endif

             if (kfs(n+1,m)>=1) then
                alv = alfas(n+1,m)*degrad
                sav = ( sa  + sin(alv) ) * 0.5
                cav = ( ca  + cos(alv) ) * 0.5
             else
                sav =   sa
                cav =   ca
             endif

             do i = -1, 1
                do j = -1, 1
                   if (i/= - 1 .or. j/= - 1) then
                      teta        = (dir(n + j, m + i) + alfas(n + j, m + i))*degrad
                      costet      = cos(teta)
                      sintet      = sin(teta)
                      en          = cgc(n + j, m + i)
                      sw(1, j, i) = (en - 0.5 + en*costet*costet)                &
                                  & *ewave1(n + j, m + i)
                      sr(1, j, i) = 2.0*costet*costet*eroll1(n + j, m + i)
                      sw(2, j, i) = en*ewave1(n + j, m + i)
                      sr(2, j, i) = 2.0*eroll1(n + j, m + i)
                      sw(2, j, i) = sw(2, j, i)*costet*sintet
                      sr(2, j, i) = sr(2, j, i)*costet*sintet
                      sw(3, j, i) = (en - 0.5 + en*sintet*sintet)                &
                                  & *ewave1(n + j, m + i)
                      sr(3, j, i) = 2.0*sintet*sintet*eroll1(n + j, m + i)
                   else
                      sw(1, j, i) = 0.0
                      sw(2, j, i) = 0.0
                      sw(3, j, i) = 0.0
                      sr(1, j, i) = 0.0
                      sr(2, j, i) = 0.0
                      sr(3, j, i) = 0.0
                   endif
                enddo
             enddo
             !
             ! All points with KFS=1 are located inside the boundaries
             ! which means that for neighbour grid points there are X and Y
             ! coordinates. However, the GSQS may not be available.
             !
             mp1       = m + 1
             np1       = n + 1
             kmp1      = min(1, kfs(n, mp1))
             knp1      = min(1, kfs(np1, m))
             !
             hnm    = real(dps(n  , m)  ,fp) + s0(n  , m)
             hnmp1  = real(dps(n  , mp1),fp) + s0(n  , mp1)
             hnp1m  = real(dps(np1, m)  ,fp) + s0(np1, m)
             if (hnm>dryflc) then
                inm = 1.0
             else
                inm = 0.0
             endif
             if (hnmp1>dryflc) then
                inmp1 = 1.0
             else
                inmp1 = 0.0
             endif
             if (hnp1m>dryflc) then
                inp1m = 1.0
             else
                inp1m = 0.0
             endif
             !
             ds1k1u    = sr(1, 0, 1) - sr(1, 0, 0)
             ds1k2u    = (sr(1, 1, 1) + sr(1, 1, 0) - sr(1, -1, 0) - sr(1, -1, 1)) &
                       & *0.25
             ds2k1u    = sr(2, 0, 1) - sr(2, 0, 0)
             ds2k2u    = (sr(2, 1, 1) + sr(2, 1, 0) - sr(2, -1, 0) - sr(2, -1, 1)) &
                       & *0.25
             ds3k1u    = sr(3, 0, 1) - sr(3, 0, 0)
             ds3k2u    = (sr(3, 1, 1) + sr(3, 1, 0) - sr(3, -1, 0) - sr(3, -1, 1)) &
                       & *0.25
             !
             dxk1u     =  gvu(n,m)*cau
             dyk1u     =  gvu(n,m)*sau
             dxk2u     = -guu(n,m)*sau
             dyk2u     =  guu(n,m)*cau
             !
             fxw(n, m) = - (  ds1k1u*dyk2u*dyk2u - 2.0*ds2k1u*dyk2u*dxk2u  &
                       &    + ds3k1u*dxk2u*dxk2u -     ds1k2u*dyk1u*dyk2u  &
                       &    + ds2k2u*dyk1u*dxk2u +     ds2k2u*dxk1u*dyk2u  &
                       &    - ds3k2u*dxk1u*dxk2u                         ) &
                       & * gvu(n, m) / ( (gsqs(n,m)+gsqs(n,mp1)*kmp1)/(1.0+kmp1) )**2
             !
             ds1k1u    = sw(1, 0, 1) - sw(1, 0, 0)
             ds1k2u    = (sw(1, 1, 1) + sw(1, 1, 0) - sw(1, -1, 0) - sw(1, -1, 1)) &
                       & *0.25
             ds2k1u    = sw(2, 0, 1) - sw(2, 0, 0)
             ds2k2u    = (sw(2, 1, 1) + sw(2, 1, 0) - sw(2, -1, 0) - sw(2, -1, 1)) &
                       & *0.25
             ds3k1u    = sw(3, 0, 1) - sw(3, 0, 0)
             ds3k2u    = (sw(3, 1, 1) + sw(3, 1, 0) - sw(3, -1, 0) - sw(3, -1, 1)) &
                       & *0.25
             fxw(n, m) = fxw(n, m)                                         &
                       & - (  ds1k1u*dyk2u*dyk2u - 2.0*ds2k1u*dyk2u*dxk2u  &
                       &    + ds3k1u*dxk2u*dxk2u -     ds1k2u*dyk1u*dyk2u  &
                       &    + ds2k2u*dyk1u*dxk2u +     ds2k2u*dxk1u*dyk2u  &
                       &    - ds3k2u*dxk1u*dxk2u                         ) &
                       & * gvu(n, m) / ( (gsqs(n,m)+gsqs(n, mp1)*kmp1)/(1.0+kmp1) )**2
             !
             wsu(n, m) = 0.5*(  inm   * sinkr(n,m  ) * eroll1(n,m  )                      &
                       &        / max(c(n,m  ),1.0e-1_fp) * cos(dir(n, m  ) * degrad)     &
                       &      + inmp1 * sinkr(n,mp1) * eroll1(n,mp1)                      &
                       &        / max(c(n,mp1),1.0e-1_fp) * cos(dir(n, mp1) * degrad)   )
             !
             fxw(n, m) = fxw(n, m) - wsu(n, m)
             !
             ! Reduce forces in water depths less than 2*DRYFLC
             !
             if (hu(n, m)<2.0_fp*dryflc) then
                wsu(n, m) = 0.0
                fxw(n, m) = 0.0
             endif
             !
             if (.not.wavfrc) fxw(n, m) = 0.0
             !
             ! wsu and fxw are limited by gammax in uzd and cucnp
             !
             ds1k1v    = (sr(1, 1, 1) + sr(1, 0, 1) - sr(1, 1, -1) - sr(1, 0, -1)) &
                       & *0.25
             ds1k2v    = sr(1, 1, 0) - sr(1, 0, 0)
             ds2k1v    = (sr(2, 1, 1) + sr(2, 0, 1) - sr(2, 1, -1) - sr(2, 0, -1)) &
                       & *0.25
             ds2k2v    = sr(2, 1, 0) - sr(2, 0, 0)
             ds3k1v    = (sr(3, 1, 1) + sr(3, 0, 1) - sr(3, 1, -1) - sr(3, 0, -1)) &
                       & *0.25
             ds3k2v    = sr(3, 1, 0) - sr(3, 0, 0)
             !
             dxk1v     =  gvv(n,m)*cav
             dyk1v     =  gvv(n,m)*sav
             dxk2v     = -guv(n,m)*sav
             dyk2v     =  guv(n,m)*cav
             !
             fyw(n, m) = - ( - ds1k1v*dyk2v*dyk1v +     ds2k1v*dyk2v*dxk1v  &
                       &     + ds2k1v*dxk2v*dyk1v -     ds3k1v*dxk2v*dxk1v  &
                       &     + ds1k2v*dyk1v*dyk1v - 2.0*ds2k2v*dyk1v*dxk1v  &
                       &     + ds3k2v*dxk1v*dxk1v                         ) &
                       & * guv(n,m) / ( (gsqs(n,m)+gsqs(np1,m)*knp1)/(1.0+knp1) )**2
             !
             ds1k1v    = (sw(1, 1, 1) + sw(1, 0, 1) - sw(1, 1, -1) - sw(1, 0, -1)) &
                       & *0.25
             ds1k2v    = sw(1, 1, 0) - sw(1, 0, 0)
             ds2k1v    = (sw(2, 1, 1) + sw(2, 0, 1) - sw(2, 1, -1) - sw(2, 0, -1)) &
                       & *0.25
             ds2k2v    = sw(2, 1, 0) - sw(2, 0, 0)
             ds3k1v    = (sw(3, 1, 1) + sw(3, 0, 1) - sw(3, 1, -1) - sw(3, 0, -1)) &
                       & *0.25
             ds3k2v    = sw(3, 1, 0) - sw(3, 0, 0)
             fyw(n, m) = fyw(n, m)                                          &
                       & - ( - ds1k1v*dyk2v*dyk1v +     ds2k1v*dyk2v*dxk1v  &
                       &     + ds2k1v*dxk2v*dyk1v -     ds3k1v*dxk2v*dxk1v  &
                       &     + ds1k2v*dyk1v*dyk1v - 2.0*ds2k2v*dyk1v*dxk1v  &
                       &     + ds3k2v*dxk1v*dxk1v                         ) &
                       & * guv(n,m) / ( (gsqs(n,m)+gsqs(np1,m)*knp1)/(1.0+knp1) )**2
             !
             wsv(n, m) = 0.5*(  inm   * sinkr(n  ,m) * eroll1(n  ,m)                 &
                       &        / max(c(n  ,m),1.0e-1_fp) * sin(dir(n  ,m) * degrad) &
                       &      + inp1m * sinkr(np1,m) * eroll1(np1,m)                 &
                       &        / max(c(np1,m),1.0e-1_fp) * sin(dir(np1,m) * degrad)   )
             !
             fyw(n, m) = fyw(n, m) - wsv(n, m)
             !
             ! Reduce forces in water depths less than 2*DRYFLC
             !
             if (hv(n, m)<2.0_fp*dryflc) then
                wsv(n, m) = 0.0
                fyw(n, m) = 0.0
             endif
             !
             if (.not.wavfrc) fyw(n, m) = 0.0
             !
             ! wsv and fyw are limited by gammax in uzd and cucnp
             !
          else
             fxw(n, m) = 0.0
             fyw(n, m) = 0.0
             wsu(n, m) = 0.0
             wsv(n, m) = 0.0
          endif
       enddo
    enddo
    !
    ! extrapolation of wave forces
    !
    do ic = 1, norow
       !
       ! left
       !
       n             = irocol(1, ic)
       m             = irocol(2, ic)
       d1            = 0.5*(gvv(n, m) + gvv(n - 1, m))
       d2            = 0.5*(gvv(n, m + 1) + gvv(n - 1, m + 1))
       f1            = fxw(n, m)
       f2            = fxw(n, m + 1)
       fxw(n, m - 1) = le(d1, d2, f1, f2)
       f1            = wsu(n, m)
       f2            = wsu(n, m + 1)
       wsu(n, m - 1) = le(d1, d2, f1, f2)
       d1            = gvu(n, m )
       d2            = gvu(n, m + 1)
       f1            = fyw(n, m + 1)
       f2            = fyw(n, m + 2)
       fyw(n, m)     = le(d1, d2, f1, f2)
       f1            = wsv(n, m + 1)
       f2            = wsv(n, m + 2)
       wsv(n, m)     = le(d1, d2, f1, f2)
       !
       ! right
       !
       m             = irocol(3, ic)
       d1            = 0.5*(gvv(n, m) + gvv(n - 1, m))
       d2            = 0.5*(gvv(n, m - 1) + gvv(n - 1, m - 1))
       f1            = fxw(n, m - 1)
       f2            = fxw(n, m - 2)
       fxw(n, m)     = le(d1, d2, f1, f2)
       f1            = wsu(n, m - 1)
       f2            = wsu(n, m - 2)
       wsu(n, m)     = le(d1, d2, f1, f2)
       d1            = gvu(n, m - 1)
       d2            = gvu(n, m - 2)
       f1            = fyw(n, m - 1)
       f2            = fyw(n, m - 2)
       fyw(n, m)     = le(d1, d2, f1, f2)
       f1            = wsv(n, m - 1)
       f2            = wsv(n, m - 2)
       wsv(n, m)     = le(d1, d2, f1, f2)
    enddo
    do ic = norow + 1, norow + nocol
       !
       ! bottom
       !
       m = irocol(1, ic)
       n = irocol(2, ic)
       if (nmax <= 3) then
         !
         ! needed to be able to run model in profile mode
         !
         nsh = n
       else
         !
         ! default situation
         !
         nsh = n + 1
       endif
       d1            = 0.5*(guu(n, m) + guu(n, m - 1))
       d2            = 0.5*(guu(nsh, m) + guu(nsh, m - 1))
       f1            = fyw(n, m)
       f2            = fyw(nsh, m)
       fyw(n - 1, m) = le(d1, d2, f1, f2)
       f1            = wsv(n, m)
       f2            = wsv(nsh, m)
       wsv(n - 1, m) = le(d1, d2, f1, f2)
       !
       ! top
       !
       n = irocol(3, ic)
       if (nmax <= 3) then
         !
         ! needed to be able to run model in profile mode
         !
         nsh = n - 1
       else
         !
         ! default situation
         !
         nsh = n - 2
       endif
       d1        = 0.5*(guu(n, m) + guu(n, m - 1))
       d2        = 0.5*(guu(n - 1, m) + guu(n - 1, m - 1))
       f1        = fyw(n - 1, m)
       f2        = fyw(nsh  , m)
       fyw(n, m) = le(d1, d2, f1, f2)
       f1        = wsv(n - 1, m)
       f2        = wsv(nsh  , m)
       wsv(n, m) = le(d1, d2, f1, f2)
       if (nmax > 3) then
          !
          ! This part can not be executed in profile mode
          !
          d1        = guv(n - 1, m)
          d2        = guv(nsh, m)
          f1        = fxw(n - 1, m)
          f2        = fxw(nsh, m)
          fxw(n, m) = le(d1, d2, f1, f2)
          f1        = wsu(n - 1, m)
          f2        = wsu(nsh, m)
          wsu(n, m) = le(d1, d2, f1, f2)
       endif
    enddo
    if (wavcmp) then
       !
       ! Artificial reduction of wave forces at boundaries where locked waves
       ! are outgoing.
       !
       do ic = 1, norow
          !
          ! left
          !
          n0 = irocol(1, ic)
          m0 = irocol(2, ic)
          if (mmskf>m0) then
             do m = m0, mmskf
                tau = sqrt(  (   (x(n0, m    ) - x(n0, m0 - 1))**2  &
                    &          + (y(n0, m    ) - y(n0, m0 - 1))**2) &
                    &      / (   (x(n0, mmskf) - x(n0, m0 - 1))**2  &
                    &          + (y(n0, mmskf) - y(n0, m0 - 1))**2) )
                fxw(n0, m) = fxw(n0, m)*tapf(tau)
                fyw(n0, m) = fyw(n0, m)*tapf(tau)
                wsu(n0, m) = wsu(n0, m)*tapf(tau)
                wsv(n0, m) = wsv(n0, m)*tapf(tau)
             enddo
          endif
          !
          ! right
          !
          m1 = irocol(3, ic)
          if (mmskl<m1 .and. mmskl>0) then
             do m = mmskl, m1
                tau = sqrt(  (   (x(n0, m    ) - x(n0, m1))**2  &
                    &          + (y(n0, m    ) - y(n0, m1))**2) &
                    &      / (   (x(n0, mmskl) - x(n0, m1))**2  &
                    &          + (y(n0, mmskl) - y(n0, m1))**2)  )
                fxw(n0, m) = fxw(n0, m)*tapf(tau)
                fyw(n0, m) = fyw(n0, m)*tapf(tau)
                wsu(n0, m) = wsu(n0, m)*tapf(tau)
                wsv(n0, m) = wsv(n0, m)*tapf(tau)
             enddo
          endif
       enddo
       do ic = norow + 1, norow + nocol
          !
          ! bottom
          !
          m0 = irocol(1, ic)
          n0 = irocol(2, ic)
          if (nmskf>n0) then
             do n = n0, nmskf
                tau = sqrt(  (   (x(n    , m0) - x(n0 - 1, m0))**2  &
                    &          + (y(n    , m0) - y(n0 - 1, m0))**2) &
                    &      / (   (x(nmskf, m0) - x(n0 - 1, m0))**2  &
                    &          + (y(nmskf, m0) - y(n0 - 1, m0))**2)  )
                fxw(n, m0) = fxw(n, m0)*tapf(tau)
                fyw(n, m0) = fyw(n, m0)*tapf(tau)
                wsu(n, m0) = wsu(n, m0)*tapf(tau)
                wsv(n, m0) = wsv(n, m0)*tapf(tau)
             enddo
          endif
          !
          ! top
          !
          n1 = irocol(3, ic)
          if (nmskl>0 .and. n1>nmskl) then
             do n = nmskl, n1
                tau = sqrt(  (   (x(n    , m0) - x(n1, m0))**2  &
                    &          + (y(n    , m0) - y(n1, m0))**2) &
                    &      / (   (x(nmskl, m0) - x(n1, m0))**2  &
                    &          + (y(nmskl, m0) - y(n1, m0))**2)  )
                fxw(n, m0) = fxw(n, m0)*tapf(tau)
                fyw(n, m0) = fyw(n, m0)*tapf(tau)
                wsu(n, m0) = wsu(n, m0)*tapf(tau)
                wsv(n, m0) = wsv(n, m0)*tapf(tau)
             enddo
          endif
       enddo
    endif
end subroutine radstr
