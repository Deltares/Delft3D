subroutine z_hormom_mdui(nmmax     ,kmax      ,icx       , &
                       & icy       ,kcs       ,kcs45     ,kcscut    , &
                       & kfu       ,kfuz0     ,kfumn0    ,kfumx0    , &
                       & kfvz0     ,kfsmn0    ,kfsmx0    , &
                       & u0        ,v1        ,hu        ,bdx       ,&
                       & bux       ,bdy       ,buy       ,buxuy     ,buxdy     , & 
                       & bdxuy     ,bdxdy     ,bbk       , &
                       & guu       ,gvv       ,gvu       ,guv       ,gsqs      , &
                       & gud       ,gvd       ,guz       ,gvz       ,gsqiu     , &
                       & ddk       ,gdp       )
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
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use flow2d3d_timers
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
!
! Global variables
!
    integer                                                         :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                         :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                         :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: nmmax  !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfsmn0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfumx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfumn0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kcs45
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: kcscut !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfuz0  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfvz0  !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gsqiu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gud    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: guz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvd    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbk    !!  Internal work array, diagonal space at (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ddk    !!  Internal work array, diagonal space at (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bdx    !!  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                              !!  with layer velocity in (N,M-1,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bdy    !!  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                              !!  with layer velocity in (N-1,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bux    !!  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                              !!  with layer velocity in (N,M+1,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: buy    !!  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                              !!  with layer velocity in (N+1,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: buxuy  !!  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                              !!  with layer velocity in (N+1,M+1,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: buxdy  !!  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                              !!  with layer velocity in (N-1,M+1,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bdxuy  !!  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                              !!  with layer velocity in (N+1,M-1,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bdxdy  !!  Internal work array, implicit coupling of layer velocity in (N,M,K)
                                                                               !!  with layer velocity in (N-1,M-1,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: v1     !  Description and declaration in esm_alloc_real.f90
    
!
! Local variables
!
    integer            :: idifc
    integer            :: idifd
    integer            :: idifu
    integer            :: k
    integer            :: kenm
    integer            :: ndm
    integer            :: ndmd
    integer            :: ndmu
    integer            :: nm
    integer            :: nmd
    integer            :: nmu
    integer            :: num
    integer            :: numd
    integer            :: numu
    real(fp)           :: advecx
    real(fp)           :: advecy
    real(fp)           :: cuu
    real(fp)           :: cvv
    real(fp)           :: curvx
    real(fp)           :: curvy
    real(fp)           :: dgeta
    real(fp)           :: dgvnm
    real(fp)           :: geta
    real(fp)           :: gksi
    real(fp)           :: gsqi
    real(fp)           :: uuu
    real(fp)           :: vvv
!
!! executable statements -------------------------------------------------------
!
    !
    ! 45 degrees staircase boundary
    !
    do nm = 1, nmmax
       nmd = nm - icx
       ndm = nm - icy
       if (kcs(nm) > 0) then
          do k = kfsmn0(nm), kfsmx0(nm)
             if (kcs45(nm, k)==3) then
                v1(ndm, k) = -u0(nm, k)*guu(nm)/gvv(nm)
                u0(nmd, k) = -v1(nm, k)*gvv(nm)/guu(nm)
             elseif (kcs45(nm, k)==6) then
                v1(ndm, k) = u0(nmd, k)*guu(nmd)/gvv(nm)
                u0(nm , k) = v1(nm, k)*gvv(nm)/guu(nmd)
             elseif (kcs45(nm, k)==9) then
                v1(nm , k) = u0(nm, k)*guu(nm)/gvv(ndm)
                u0(nmd, k) = v1(ndm, k)*gvv(ndm)/guu(nm)
             elseif (kcs45(nm, k)==12) then
                v1(nm, k) = -u0(nmd, k)*guu(nmd)/gvv(ndm)
                u0(nm, k) = -v1(ndm, k)*gvv(ndm)/guu(nmd)
             else
             endif
          enddo
       endif
    enddo
    do nm = 1, nmmax
       if (kfu(nm) == 1) then
          nmd   = nm - icx
          ndm   = nm - icy
          ndmd  = nm - icx - icy
          nmu   = nm + icx
          num   = nm + icy
          numu  = nm + icx + icy
          ndmu  = nm + icx - icy
          numd  = nm - icx + icy
          gksi  = gvu(nm)
          geta  = guu(nm)
          dgeta = guz(nmu) - guz(nm)
          dgvnm = gvd(nm)  - gvd(ndm)
          gsqi  = gsqiu(nm)
          !
          do k = kfumn0(nm), kfumx0(nm)
             if (kfuz0(nm, k)==1 .and. kcs(nm)*kcs(nmu)>0) then
                vvv   = 0.25_fp * (v1(ndm,k)+v1(ndmu,k)+v1(nm,k)+v1(nmu,k))
                uuu   = u0(nm,k)
                cvv   = vvv / geta
                cuu   = uuu / gksi
                curvx = uuu * vvv * gsqi * dgvnm
                curvy = 0.5_fp * vvv * vvv * gsqi * dgeta
                idifd = kfvz0(ndm,k) * kfvz0(ndmu,k) * kfuz0(ndm,k)
                idifu = kfvz0(nm ,k) * kfvz0(nmu ,k) * kfuz0(num,k)
                !
                ! For 1:n stair case (cut-cell Herman) boundary:
                ! - check dgvnm
                ! - reset geta
                ! - reset vvv
                !
                if (kcscut(nm,k)==1 .or. kcscut(nmu,k)==1) then
                   kenm = max(1, kfvz0(nm,k)+kfvz0(ndm,k)+kfvz0(ndmu,k)+kfvz0(nmu,k))
                   vvv  =   v1(nm  ,k)*kfvz0(nm  ,k) + v1(ndm,k)*kfvz0(ndm,k) &
                        & + v1(ndmu,k)*kfvz0(ndmu,k) + v1(nmu,k)*kfvz0(nmu,k)
                   vvv  = vvv / kenm
                endif
                !
                ! ADVECTION IN U-DIRECTION; DU/DX AND CENTRIFUGAL ACCELERATION
                !
                ! method is not identical to (Koren & Vreugdenhil, pp. 73).
                ! uuu/vvv are not taken at correct point
                ! dx and dy according to zeta-definition (for energy conservation)
                ! tests for cuu and cvv are not based on local courant-number but
                ! based on an average velocity and gksi/geta
                !
                if (        kcs45(nm ,k)/=3 .and. kcs45(nm ,k)/= 9 &
                    & .and. kcs45(nmu,k)/=6 .and. kcs45(nmu,k)/=12  ) then
                   !
                   ! All cases except 45 degrees staircase
                   !
                   if (uuu>=0.0_fp .and. vvv>=0.0_fp) then
                      if (cuu > cvv) then
                         advecx    = kfuz0(nmd,k) * cuu  
                         bbk(nm,k) = bbk(nm,k) + advecx
                         bdx(nm,k) = bdx(nm,k) - advecx
                         ddk(nm,k) = ddk(nm,k) - kfuz0(nmd,k)*curvx
                         if (kfuz0(nmd,k)*kfuz0(ndmd, k) == 0) then
                            advecy    = idifd * cvv 
                            bbk(nm,k) = bbk(nm,k) + advecy
                            bdy(nm,k) = bdy(nm,k) - advecy
                            ddk(nm,k) = ddk(nm,k) + idifd*curvy
                         else
                            advecy      = kfvz0(ndm,k) * cvv     
                            bdx  (nm,k) = bdx  (nm,k) - advecy
                            bdxdy(nm,k) = bdxdy(nm,k) + advecy
                            ddk  (nm,k) = ddk  (nm,k) + kfvz0(ndm,k)*curvy
                         endif
                      else
                         advecy    = idifd * cvv 
                         bbk(nm,k) = bbk(nm,k) + advecy
                         bdy(nm,k) = bdy(nm,k) - advecy
                         ddk(nm,k) = ddk(nm,k) + idifd*curvy
                         if (kfuz0(ndmd,k)*kfuz0(ndm,k)*kfvz0(ndm,k) == 0) then
                            advecx    = kfuz0(nmd,k) * cuu  
                            bbk(nm,k) = bbk(nm,k) + advecx
                            bdx(nm,k) = bdx(nm,k) - advecx
                            ddk(nm,k) = ddk(nm,k) - kfuz0(nmd,k)*curvx
                         else
                            bdy  (nm,k) = bdy  (nm,k) + cuu
                            bdxdy(nm,k) = bdxdy(nm,k) - cuu
                            ddk  (nm,k) = ddk  (nm,k) - curvx
                         endif
                      endif
                   elseif (uuu<0.0_fp .and. vvv>=0.0_fp) then
                      if (-cuu > cvv) then
                         advecx    = kfuz0(nmu,k) * cuu  
                         bbk(nm,k) = bbk(nm,k) - advecx
                         bux(nm,k) = bux(nm,k) + advecx
                         ddk(nm,k) = ddk(nm,k) - kfuz0(nmu,k)*curvx
                         if (kfuz0(nmu,k)*kfuz0(ndmu,k) == 0) then
                            advecy    = idifd * cvv 
                            bbk(nm,k) = bbk(nm,k) + advecy
                            bdy(nm,k) = bdy(nm,k) - advecy
                            ddk(nm,k) = ddk(nm,k) + idifd*curvy
                         else
                            advecy      = kfvz0(ndmu,k) * cvv     
                            bux  (nm,k) = bux  (nm,k) + advecy
                            buxdy(nm,k) = buxdy(nm,k) - advecy
                            ddk  (nm,k) = ddk  (nm,k) + kfvz0(ndmu,k)*curvy
                         endif
                      else
                         advecy    = idifd * cvv 
                         bbk(nm,k) = bbk(nm,k) + advecy
                         bdy(nm,k) = bdy(nm,k) - advecy
                         ddk(nm,k) = ddk(nm,k) + idifd*curvy
                         if (kfuz0(ndmu,k)*kfuz0(ndm,k)*kfvz0(ndmu,k) == 0) then
                            advecx    = kfuz0(nmu,k) * cuu  
                            bbk(nm,k) = bbk(nm,k) - advecx
                            bux(nm,k) = bux(nm,k) + advecx
                            ddk(nm,k) = ddk(nm,k) - kfuz0(nmu,k)*curvx
                         else
                            bdy  (nm,k) = bdy  (nm,k) - cuu
                            buxdy(nm,k) = buxdy(nm,k) + cuu
                            ddk  (nm,k) = ddk  (nm,k) - curvx
                         endif
                      endif
                   elseif (uuu>=0.0_fp .and. vvv<0.0_fp) then
                      if (cuu > -cvv) then
                         advecx = kfuz0(nmd,k) * cuu  
                         bbk(nm,k) = bbk(nm,k) + advecx
                         bdx(nm,k) = bdx(nm,k) - advecx
                         ddk(nm,k) = ddk(nm,k) - kfuz0(nmd,k)*curvx
                         if (kfuz0(numd,k)*kfuz0(nmd,k) == 0) then
                            advecy    = idifu * cvv 
                            bbk(nm,k) = bbk(nm,k) - advecy
                            buy(nm,k) = buy(nm,k) + advecy
                            ddk(nm,k) = ddk(nm,k) + idifu*curvy
                         else
                            advecy      = kfvz0(nm,k) * cvv     
                            bdx  (nm,k) = bdx  (nm,k) - advecy
                            bdxuy(nm,k) = bdxuy(nm,k) + advecy
                            ddk  (nm,k) = ddk  (nm,k) + kfvz0(nm,k)*curvy
                         endif
                      else
                         advecy    = idifu * cvv 
                         bbk(nm,k) = bbk(nm,k) - advecy
                         buy(nm,k) = buy(nm,k) + advecy
                         ddk(nm,k) = ddk(nm,k) + idifu*curvy
                         if (kfuz0(num,k)*kfuz0(numd,k)*kfvz0(nm,k) == 0) then
                            advecx    = kfuz0(nmd,k) * cuu  
                            bbk(nm,k) = bbk(nm,k) + advecx
                            bdx(nm,k) = bdx(nm,k) - advecx
                            ddk(nm,k) = ddk(nm,k) - kfuz0(nmd,k)*curvx
                         else
                            buy  (nm,k) = buy  (nm,k) + cuu
                            bdxuy(nm,k) = bdxuy(nm,k) - cuu
                            ddk  (nm,k) = ddk  (nm,k) - curvx
                         endif
                      endif
                   elseif (uuu<0.0_fp .and. vvv<0.0_fp) then
                      if (-cuu > -cvv) then
                         advecx    = kfuz0(nmu,k) * cuu  
                         bbk(nm,k) = bbk(nm,k) - advecx
                         bux(nm,k) = bux(nm,k) + advecx
                         ddk(nm,k) = ddk(nm,k) - kfuz0(nmu,k)*curvx
                         if (kfuz0(numu,k)*kfuz0(nmu,k) == 0) then
                            advecy    = idifu * cvv 
                            bbk(nm,k) = bbk(nm,k) - advecy
                            buy(nm,k) = buy(nm,k) + advecy
                            ddk(nm,k) = ddk(nm,k) + idifu*curvy
                         else
                            advecy      = kfvz0(nmu,k) * cvv     
                            bux  (nm,k) = bux  (nm,k) - advecy
                            buxuy(nm,k) = buxuy(nm,k) + advecy
                            ddk  (nm,k) = ddk  (nm,k) + kfvz0(nmu,k)*curvy
                         endif
                      else
                         advecy    = idifu * cvv 
                         bbk(nm,k) = bbk(nm,k) - advecy
                         buy(nm,k) = buy(nm,k) + advecy
                         ddk(nm,k) = ddk(nm,k) + idifu*curvy
                         if (kfuz0(numu,k)*kfuz0(num,k)*kfvz0(nmu,k) == 0) then
                            advecx    = kfuz0(nmu,k) * cuu  
                            bbk(nm,k) = bbk(nm,k) - advecx
                            bux(nm,k) = bux(nm,k) + advecx
                            ddk(nm,k) = ddk(nm,k) - kfuz0(nmu,k)*curvx
                         else
                            buy  (nm,k) = buy  (nm,k) - cuu
                            buxuy(nm,k) = buxuy(nm,k) + cuu
                            ddk  (nm,k) = ddk  (nm,k) - curvx
                         endif
                      endif
                   endif
                else
                   !
                   ! 45 degree staircase
                   !
                   if (uuu > 0.0_fp) then
                      bbk(nm,k) = bbk(nm,k) + cuu
                      bdx(nm,k) = bdx(nm,k) - cuu
                      ddk(nm,k) = ddk(nm,k) - curvx
                   else
                      bbk(nm,k) = bbk(nm,k) - cuu
                      bux(nm,k) = bux(nm,k) + cuu
                      ddk(nm,k) = ddk(nm,k) - curvx
                   endif
                   if (vvv > 0.0_fp) then
                      bbk(nm,k) = bbk(nm,k) + cvv
                      bdy(nm,k) = bdy(nm,k) - cvv
                      ddk(nm,k) = ddk(nm,k) + curvy
                   else
                      bbk(nm,k) = bbk(nm,k) - cvv
                      buy(nm,k) = buy(nm,k) + cvv
                      ddk(nm,k) = ddk(nm,k) + curvy
                   endif
                endif
             endif
          enddo
       endif
    enddo
end subroutine z_hormom_mdui
