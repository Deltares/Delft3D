subroutine inigeo(lundia    ,error     ,filrgf    ,sferic    ,            &
                & dx        ,dy        ,mmax      ,nmax      ,nmaxus    , &
                & alfas     ,fcorio    ,xcor      ,ycor      ,xz        , &
                & yz        ,guu       ,gvv       ,guv       ,gvu       , &
                & gsqs      ,gsqd      ,guz       ,gvz       ,gud       , &
                & gvd       ,gsqiu     ,gsqiv     ,kcu       ,kcv       , &
                & kcs       ,dfguu     ,dfgvv     ,gdp       )
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
!    Function: Sets the geometry parameters of the model. if the
!              model is a curvi-linear model then the coordinates
!              of the depth points are read first
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use dfparall
    use globaldata
    use geometry_module, only: clockwise
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                                                                                                            :: lundia !  Description and declaration in inout.igs
    integer                                                                                                            :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                                            :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                            , intent(in)    :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer , dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound) , intent(in)    :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound) , intent(in)    :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound) , intent(in)    :: kcv    !  Description and declaration in esm_alloc_int.f90
    logical                                                                                                            :: error  !!  Flag=TRUE if an error is encountered
    logical                                                                                                            :: sferic !  Description and declaration in tricom.igs
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)                 :: alfas  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound) , intent(out)   :: fcorio !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound) , intent(out)   :: gsqd   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)                 :: gsqiu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)                 :: gsqiv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)                 :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)                 :: gud    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)                 :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)                 :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)                 :: guz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)                 :: gvd    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)                 :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)                 :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)                 :: gvz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)                 :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)                 :: xz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)                 :: ycor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound)                 :: yz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                                                         , intent(inout) :: dfguu  !  complete guu array for exchange between domains
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                                                         , intent(inout) :: dfgvv  !  complete gvv array for exchange between domains
    real(fp)                                                                                           , intent(in)    :: dx     !!  Uniform grid-distance in the x-dir.
    real(fp)                                                                                           , intent(in)    :: dy     !!  Uniform grid-distance in the y-dir.
    character(*)                                                                                                       :: filrgf !!  File name for the curvi-linear grid
!
! Local variables
!
    integer                :: ddb
    integer                :: kenm
    integer                :: kenmu
    integer                :: kenmv
    integer                :: m               ! Current M-index of the active point in the current computational ROW 
    integer                :: md              ! Current M-index minus 1 (see M) ROW 
    integer                :: mdd
    integer                :: mu              ! Current M-index plus  1 (see M) 
    integer                :: n               ! Current N-index of the active point in the current computational COLUMN 
    integer                :: nd              ! Current N-index minus 1 (see N) 
    integer                :: ndd
    integer                :: nu              ! Current N-index plus  1 (see N)
    logical                :: success
    logical                :: kcd
    logical                :: new_area_method ! .True.  when gsqs, gsqd, gsqiu and gsqiv are determined with the new approach
                                              ! .False. when gsqs, gsqd, gsqiu and gsqiv are determined with the old approach
    real(fp)               :: sidday
    real(fp)               :: dxdknd
    real(fp)               :: dxdknu
    real(fp)               :: dxdksi          ! Help var. 
    real(fp)               :: dydknd
    real(fp)               :: dydknu
    real(fp)               :: dydksi          ! Help var. 
    real(fp)               :: small
    real(fp), dimension(4) :: xcell
    real(fp)               :: xcndm
    real(fp)               :: xcndmd
    real(fp)               :: xcnm
    real(fp)               :: xcnmd
    real(fp)               :: xcor_mean
    real(fp)               :: xdis
    real(fp)               :: xdis1
    real(fp)               :: xdis2
    real(fp)               :: xm
    real(fp)               :: xmd
    real(fp)               :: xmu
    real(fp)               :: xnm
    real(fp)               :: xndm
    real(fp)               :: xnmd
    real(fp)               :: xndmd
    real(fp), dimension(4) :: ycell
    real(fp)               :: ycndm
    real(fp)               :: ycndmd
    real(fp)               :: ycnm
    real(fp)               :: ycnmd
    real(fp)               :: ycor_mean
    real(fp)               :: ycor_min
    real(fp)               :: ydis
    real(fp)               :: ydis1
    real(fp)               :: ydis2
    real(fp)               :: ym
    real(fp)               :: ymd
    real(fp)               :: ymu
    real(fp), dimension(4) :: kenmgsqd        ! Mask array for qsqd determination
    character(20)          :: errmsg          ! Character var. containing the error message to be written to file. The message depend on the error.
    character(300)         :: message
    integer                :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    ! Initialization
    ! WARNING: the const-include file can not be used here
    !          inigeo is (also) called by wave, without initialization
    !          of the const-parameters.
    !
    errmsg = ' '
    small  = 1.0E-10
    sidday = (365.25/366.25)*86400.0
    ddb    = gdp%d%ddbound
    !
    success = .true.
    nm_pos  = 1
    !
    ! Temporary solution: the new method for determining GSQS, GSQD, GSQIU and GSQIV is switched off
    ! leads to difference in test bench results
    !
    new_area_method = .false.
    !
    if (ddb == 1) then 
       call creategridborder(xcor,ycor,mmax,nmax,gdp)
    endif
    !
    ! Initialise GSQS and GSQD with value 1.0
    !
    gsqs = 1.0_fp
    gsqd = 1.0_fp
    !
    ! Compute GUU
    !
    do n = 2, nmaxus
       nd = n - 1
       do m = 1, mmax - 1
          mu = m + 1
          if (kcs(n, m) == 1 .or. abs(kcs(n, mu)) == 1) then
             call distance_gdp(sferic    ,xcor(n, m), ycor(n, m), xcor(nd, m), ycor(nd, m), &
                             & guu(n, m) ,gdp       )
          endif
       enddo
    enddo
    !
    do n = 1, nmaxus - 1
       nu = n + 1
       do m = 1, mmax
          md = max(1, m - 1)
          if (kcs(n, m) == 2 .and. kcs(nu, m) == 1) then
             guu(n, m ) = guu(nu, m)
             guu(n, md) = guu(nu, md)
          elseif (kcs(n, m) == 1 .and. kcs(nu, m) == 2) then
             guu(nu, m ) = guu(n, m)
             guu(nu, md) = guu(n, md)
          else
          endif
       enddo
    enddo
    !
    ! Compute GVV
    !
    do m = 2, mmax
       md = m - 1
       do n = 1, nmaxus - 1
          nu = n + 1
          if (kcs(n, m) == 1 .or. abs(kcs(nu, m)) == 1) then
             call distance_gdp(sferic    ,xcor(n, m), ycor(n, m), xcor(n, md), ycor(n, md), &
                             & gvv(n, m) ,gdp       )
          endif
       enddo
    enddo
    !
    do m = 1, mmax - 1
       mu = m + 1
       do n = 1, nmaxus
          nd = max(1, n - 1)
          if (kcs(n, m) == 2 .and. kcs(n, mu) == 1) then
             gvv(n , m) = gvv(n, mu)
             gvv(nd, m) = gvv(nd, mu)
          elseif (kcs(n, m) == 1 .and. kcs(n, mu) == 2) then
             gvv(n , mu) = gvv(n, m)
             gvv(nd, mu) = gvv(nd, m)
          else
          endif
       enddo
    enddo
    !
    call dfexchg ( dfguu   , 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( dfgvv   , 1, 1, dfloat, nm_pos, gdp )
    !
    ! Compute GUV only if one of the GUU values <> 0. (KCS's <> 0)
    ! (GUV is GUU at V-velocity point)
    !
    do m = 1, mmax
       md = max(1, m - 1)
       do n = 1, nmaxus
          nu = min(n + 1, nmaxus)
          if (kcs(n, m) > 0 .or. kcs(nu, m) /= 0) then
             kenm = 0
             if (guu(n , m )/=0.) kenm = kenm + 1
             if (guu(n , md)/=0.) kenm = kenm + 1
             if (guu(nu, m )/=0.) kenm = kenm + 1
             if (guu(nu, md)/=0.) kenm = kenm + 1
             kenm = max(1, kenm)
             guv(n, m) = (guu(n, m) + guu(n, md) + guu(nu, m) + guu(nu, md)) / kenm
          endif
       enddo
    enddo
    !
    ! Compute GVU only if one of the GVV values <> 0. (KCS's <> 0)
    ! (GVU is GVV at U-velocity point)
    !
    do n = 1, nmaxus
       nd = max(1, n - 1)
       do m = 1, mmax
          mu = min(m + 1, mmax)
          if (kcs(n, m) > 0 .or. kcs(n, mu) /= 0) then
             kenm = 0
             if (gvv(n , m )/=0.) kenm = kenm + 1
             if (gvv(nd, m )/=0.) kenm = kenm + 1
             if (gvv(n , mu)/=0.) kenm = kenm + 1
             if (gvv(nd, mu)/=0.) kenm = kenm + 1
             kenm = max(1, kenm)
             gvu(n, m) = (gvv(n, m) + gvv(nd, m) + gvv(n, mu) + gvv(nd, mu)) / kenm
          endif
       enddo
    enddo
    !
    ! Compute GSQS: cell area in water level points
    !
    ! The approach follows Green's Theorem to determine the area of a cell,
    ! based on a line integral along the edges of the cell.
    ! The area is computed by adding the four area 'below' all cell edges.
    ! The distance in X-direction between points 1 and 2 is determined as the average between:
    ! (x2(y1) - x1(y1)) and (x2(y2) - x1(y2))
    ! Distances in Y-direction are determined with respect to the minimum Y-coordinate of the cell considered, as:
    ! (y1(x1) - ycor_min(x1)) and (y2(x2) - ycor_min(x2))
    ! The four contributions are added/subtracted based on the sign of x2-x1 (Conform Green's Theorem). 
    ! In spherical coordinates this approach is not completely correct (issue Delft3D-12967).
    !

    if (new_area_method) then
       !
       ! new way of determining gsqs and gsqs
       ! old way was wrong in case of strongly non-orthogonal cells
       !
       do n = 2, nmaxus
          nd = max(n-1, 1)
          do m = 2, mmax
             md = max(m-1, 1)
             if (kcs(n, m) == 1) then
                !
                ! minimum y-coordinate as reference level
                !
                ycor_min = min(ycor(n,m), ycor(n, md), ycor(nd, md), ycor(nd, m))
                !
                ! xdis at y(n,m), xdis at y(n,md), ydis at x(n,m), ydis at x(n,md)
                !
                call distance_gdp(sferic, xcor(n,m ), ycor(n,m ), xcor(n,md), ycor(n,m ), xdis1, gdp)
                call distance_gdp(sferic, xcor(n,m ), ycor(n,md), xcor(n,md), ycor(n,md), xdis2, gdp)
                call distance_gdp(sferic, xcor(n,m ), ycor(n,m ), xcor(n,m ), ycor_min  , ydis1, gdp)
                call distance_gdp(sferic, xcor(n,md), ycor(n,md), xcor(n,md), ycor_min  , ydis2, gdp)
                !
                ! average xdis and ydis
                !
                xdis = (xdis1 + xdis2) * 0.5_fp
                ydis = (ydis1 + ydis2) * 0.5_fp
                !
                ! 1st contribution to cell area 
                !
                gsqs(n,m) = xdis * ydis * sign(1.0_fp, xcor(n,m)-xcor(n,md))
                !
                ! xdis at y(n,md), xdis at y(nd,md), ydis at x(n,md), ydis at x(nd,md)
                !
                call distance_gdp(sferic, xcor(n ,md), ycor(n ,md), xcor(nd,md), ycor(n ,md), xdis1, gdp)
                call distance_gdp(sferic, xcor(n ,md), ycor(nd,md), xcor(nd,md), ycor(nd,md), xdis2, gdp)
                call distance_gdp(sferic, xcor(n ,md), ycor(n ,md), xcor(n ,md), ycor_min   , ydis1, gdp)
                call distance_gdp(sferic, xcor(nd,md), ycor(nd,md), xcor(nd,md), ycor_min   , ydis2, gdp)
                !
                ! average xdis and ydis
                !
                xdis = (xdis1 + xdis2) * 0.5_fp
                ydis = (ydis1 + ydis2) * 0.5_fp
                !
                ! 2nd contribution to cell area 
                !
                gsqs(n,m) = gsqs(n,m) + xdis * ydis * sign(1.0_fp, xcor(n,md)-xcor(nd,md))
                !
                ! xdis at y(nd,md), xdis at y(nd,m), ydis at x(nd,md), ydis at x(nd,m)
                !
                call distance_gdp(sferic, xcor(nd,md), ycor(nd,md), xcor(nd,m ), ycor(nd,md), xdis1, gdp)
                call distance_gdp(sferic, xcor(nd,md), ycor(nd,m ), xcor(nd,m ), ycor(nd,m ), xdis2, gdp)
                call distance_gdp(sferic, xcor(nd,md), ycor(nd,md), xcor(nd,md), ycor_min   , ydis1, gdp)
                call distance_gdp(sferic, xcor(nd,m ), ycor(nd,m ), xcor(nd,m ), ycor_min   , ydis2, gdp)
                !
                ! average xdis and ydis
                !
                xdis = (xdis1 + xdis2) * 0.5_fp
                ydis = (ydis1 + ydis2) * 0.5_fp
                !
                ! 3rd contribution to cell area 
                !
                gsqs(n,m) = gsqs(n,m) + xdis * ydis * sign(1.0_fp, xcor(nd,md)-xcor(nd,m))
                !
                ! xdis at y(nd,m), xdis at y(n,m), ydis at x(nd,m), ydis at x(n,m)
                !
                call distance_gdp(sferic, xcor(nd,m), ycor(nd,m), xcor(n ,m), ycor(nd,m), xdis1, gdp)
                call distance_gdp(sferic, xcor(nd,m), ycor(n ,m), xcor(n ,m), ycor(n ,m), xdis2, gdp)
                call distance_gdp(sferic, xcor(nd,m), ycor(nd,m), xcor(nd,m), ycor_min  , ydis1, gdp)
                call distance_gdp(sferic, xcor(n ,m), ycor(n ,m), xcor(n ,m), ycor_min  , ydis2, gdp)
                !
                ! average xdis and ydis
                !
                xdis = (xdis1 + xdis2) * 0.5_fp
                ydis = (ydis1 + ydis2) * 0.5_fp
                !
                ! 4th contribution to cell area 
                !
                gsqs(n,m) = gsqs(n,m) + xdis * ydis * sign(1.0_fp, xcor(nd,m)-xcor(n,m))
                !
                ! boundary integral might be computed in the wrong direction
                ! therefore the absolute value of gsqs is used
                !
                gsqs(n,m) = abs(gsqs(n,m))
             endif
          enddo
       enddo
       !
       ! Mirroring of interior cells to the boundaries
       !
       do n = 1, nmaxus
          nd = max(n-1, 1)
          nu = min(n+1, nmaxus)
          do m = 1, mmax
             md = max(m-1, 1)
             mu = min(m+1, mmax)
             if (kcs(n,m) == 2) then
                !
                ! boundary cell
                !
                if (kcs(n,md) == 1) then
                   gsqs(n,m) = gsqs(n,md)
                elseif (kcs(n,mu) == 1) then
                   gsqs(n,m) = gsqs(n,mu)
                elseif(kcs(nd,m) == 1) then
                   gsqs(n,m) = gsqs(nd,m)
                elseif(kcs(nu,m) == 1) then
                   gsqs(n,m) = gsqs(nu,m)
                endif
             endif
          enddo
       enddo
       !
       ! Compute GSQD: cell area in depth points
       ! Determined as average of the areas (GSQS) of surrounding active cells
       !
       do n = 1, nmaxus
          nu = min(n+1, nmaxus)
          do m = 1, mmax
             mu = min(m+1, mmax)
             kcd = kcs(n, m) > 0 .or. kcs(n, mu) > 0 .or. kcs(nu, m) > 0 .or. kcs(nu, mu) > 0
             if (kcd) then
                kenmgsqd = 0.0_fp
                if (gvv(n, m ) /= 0.0_fp) then
                   if (guu(n , m) /= 0.0_fp .and. kcs(n,m) > 0) then
                      kenmgsqd(1) = 1.0_fp
                   endif
                   if (guu(nu, m) /= 0.0_fp .and. kcs(nu,m) > 0) then
                      kenmgsqd(2) = 1.0_fp
                   endif
                endif
                if (gvv(n, mu) /= 0.0_fp) then
                   if (guu(n , m) /= 0.0_fp .and. kcs(n,mu) > 0) then
                      kenmgsqd(3) = 1.0_fp
                   endif
                   if (guu(nu, m) /= 0.0_fp .and. kcs(nu,mu) > 0) then
                      kenmgsqd(4) = 1.0_fp
                   endif
                endif
                if (sum(kenmgsqd) > 0.0_fp) then 
                   gsqd(n, m) = (kenmgsqd(1)*gsqs(n,m ) + kenmgsqd(2)*gsqs(nu,m ) + &
                               & kenmgsqd(3)*gsqs(n,mu) + kenmgsqd(4)*gsqs(nu,mu)) / sum(kenmgsqd)
                else
                   gsqd(n, m) = 1.0_fp
                endif
             endif
          enddo
       enddo
    else
       !
       ! old way of determining gsqs and gsqs
       ! was wrong in case of strongly non-orthogonal cells
       !
       do n = 1, nmaxus
          nd = max(n - 1, 1)
          nu = min(n + 1, nmaxus)
          do m = 1, mmax
             md = max(m - 1, 1)
             mu = min(m + 1, mmax)
             if (kcs(n, m) /= 0) then
                kenmv = 0
                if (gvv(n , m) /= 0.) kenmv = kenmv + 1
                if (gvv(nd, m) /= 0.) kenmv = kenmv + 1
                kenmv = max(1, kenmv)
                kenmu = 0
                if (guu(n, m ) /= 0.) kenmu = kenmu + 1
                if (guu(n, md) /= 0.) kenmu = kenmu + 1
                kenmu = max(1, kenmu)
                gsqs(n, m) = ((gvv(n, m) + gvv(nd, m))/kenmv) * ((guu(n, m) + guu(n, md))/kenmu)
             endif
             !
             kcd = kcs(n, m) == 1 .or. kcs(n, mu) == 1 .or. kcs(nu, m) == 1 .or. kcs(nu, mu) == 1
             if (kcd) then
                kenmv = 0
                if (gvv(n, m ) /= 0.) kenmv = kenmv + 1
                if (gvv(n, mu) /= 0.) kenmv = kenmv + 1
                kenmv = max(1, kenmv)
                kenmu = 0
                if (guu(n , m) /= 0.) kenmu = kenmu + 1
                if (guu(nu, m) /= 0.) kenmu = kenmu + 1
                kenmu = max(1, kenmu)
                gsqd(n, m) = ((gvv(n, m) + gvv(n, mu))/kenmv) * ((guu(n, m) + guu(nu, m))/kenmu)
             endif
          enddo
       enddo
    endif
    !
    ! Check computed GUU, GVU, GVV, GUV and GSQS
    !
    do n = 1, nmaxus
       do m = 1, mmax
          if (kcu(n, m) > 0) then
             if (guu(n, m) <= 0.) then
                write (errmsg(10:), '(2i5)') m, n
                errmsg(:9) = 'GUU   at '
                call prterr(lundia    ,'V016'    ,errmsg    )
                success = .false.
             endif
             if (gvu(n, m) <= 0.) then
                write (errmsg(10:), '(2i5)') m, n
                errmsg(:9) = 'GVU   at '
                call prterr(lundia    ,'V016'    ,errmsg    )
                success = .false.
             endif
          endif
          if (kcv(n, m) > 0) then
             if (gvv(n, m) <= 0.) then
                write (errmsg(10:), '(2i5)') m, n
                errmsg(:9) = 'GVV   at '
                call prterr(lundia    ,'V016'    ,errmsg    )
                success = .false.
             endif
             if (guv(n, m) <= 0.) then
                write (errmsg(10:), '(2i5)') m, n
                errmsg(:9) = 'GUV   at '
                call prterr(lundia    ,'V016'    ,errmsg    )
                success = .false.
             endif
          endif
          if (kcs(n, m) > 0) then
             if (gsqs(n, m) <= 0.) then
                success = .false.
                nd = max(n - 1, 1)
                nu = min(n + 1, nmaxus)
                md = max(m - 1, 1)
                mu = min(m + 1, mmax)
                if (      (nd==n .or. kcs(nd,m )/=1) &
                    .and. (nu==n .or. kcs(nu,m )/=1) &
                    .and. (md==m .or. kcs(n ,md)/=1) &
                    .and. (mu==m .or. kcs(n ,mu)/=1)) then
                   write (message, '(a,i0,a,i0,a)') 'Boundary point (m,n) = (', m, ',', n, ') is not connected to an active cell'
                   call prterr(lundia, 'P004', trim(message))
                else
                   write (errmsg(10:), '(2i5)') m, n
                   errmsg(:9) = 'GSQS  at '
                   call prterr(lundia, 'V016', errmsg)
                endif
             endif
          elseif (kcs(n, m) == -1) then
             if (gsqs(n, m) <= 0.) then
                gsqs(n, m) = 1.23456789000000_fp
             endif
             if (guu(n, m) <= 0.) then
                guu (n, m) = 1.23456789000000_fp
             endif
             if (guv(n, m) <= 0.) then
                guv (n, m) = 1.23456789000000_fp
             endif
             if (gvu(n, m) <= 0.) then
                gvu (n, m) = 1.23456789000000_fp
             endif
             if (gvv(n, m) <= 0.) then
                gvv (n, m) = 1.23456789000000_fp
             endif
          endif
       enddo
    enddo
    !
    ! Compute XZ, YZ and ALFAS (inside coordinates)
    ! WARNING: The alfas had to be calculated in a different way when
    ! the curvilinear spherical coordinates were introduced.
    ! The original alfas calculation is maintained to ensure identical
    ! results in existing models.
    ! This holds for all alfas calculations in this inigeo subroutine.
    !
    do n = 2, nmaxus
       nd = n - 1
       do m = 2, mmax
          md = m - 1
          if (abs(kcs(n, m)) == 1) then   
             if (sferic) then
                xndm  = xcor(nd,m)
                xnmd  = xcor(n,md)
                xndmd = xcor(nd,md)
                !
                ! Check if main coordinate is positive
                !
                if (xcor(n,m) > 0.0_fp) then
                   !
                   ! Check if current distance between coordinates is too great
                   !
                   if (abs(xcor(nd,m)-xcor(n,m)) > 180.0_fp) then
                      xndm = xcor(nd,m) + 360.0_fp
                   endif
                   if (abs(xcor(n,md) - xcor(n,m)) > 180.0_fp) then
                      xnmd = xcor(n,md) + 360.0_fp
                   endif
                   if (abs(xcor(nd,md) - xcor(n,m)) > 180.0_fp) then
                      xndmd = xcor(nd,md) + 360.0_fp
                   endif
                else 
                   !
                   ! Main coordinate is negative
                   ! Check if current distance between coordinates is too great
                   !
                   if (abs(xcor(nd,m) - xcor(n,m)) > 180.0_fp) then
                      xndm = xcor(nd,m) - 360.0_fp
                   endif
                   if (abs(xcor(n,md) - xcor(n,m)) > 180.0_fp) then
                      xnmd = xcor(n,md) - 360.0_fp
                   endif
                   if (abs(xcor(nd,md) - xcor(n,m)) > 180.0_fp) then
                      xndmd = xcor(nd,md) - 360.0_fp
                   endif    
                endif
                xz(n,m) = (xcor(n,m) + xndm + xnmd + xndmd)                   / 4.0_fp
                yz(n,m) = (ycor(n,m) + ycor(nd,m) + ycor(n,md) + ycor(nd,md)) / 4.0_fp
                xm  = (xcor(n,m)+xndm)        / 2.0_fp
                ym  = (ycor(n,m)+ycor(nd,m )) / 2.0_fp
                xmd = (xcor(n,md)+xndmd)        / 2.0_fp
                ymd = (ycor(n,md)+ycor(nd, md)) / 2.0_fp
                call angle(sferic, xmd, ymd, xm, ym, alfas(n,m), gdp)
                alfas(n,m) = alfas(n,m) * raddeg
             else
                xz(n, m) = (xcor(n, m) + xcor(nd, m) + xcor(n, md) + xcor(nd, md)) / 4.0_fp
                yz(n, m) = (ycor(n, m) + ycor(nd, m) + ycor(n, md) + ycor(nd, md)) / 4.0_fp
                dydksi = (ycor(n, m) - ycor(n, md) + ycor(nd, m) - ycor(nd, md)) / 2.0_fp
                dxdksi = (xcor(n, m) - xcor(n, md) + xcor(nd, m) - xcor(nd, md)) / 2.0_fp
                if (abs(dxdksi) < small .and. abs(dydksi) < small) then
                   alfas(n, m) = 0.0_fp
                else
                   alfas(n, m) = atan2(dydksi, dxdksi) * raddeg
                endif
             endif
          endif
       enddo
    enddo
    !
    ! Compute XZ, YZ and ALFAS (open UPPER N boundaries) 1->nmaxus
    !
    do n = 1, nmaxus
       nd  = max(1, n - 1)
       nu  = min(n + 1, nmaxus)
       ndd = nd - 1
       do m = 1, mmax
          md = max(1, m - 1)
          mu = min(m + 1, mmax)
          if (kcs(n, m) == 2) then
             !
             !NOTE: kcs (nd,m) <> 1 for nd = 1
             !      and always 4 (xcor,ycor) coordinates around kcs = 1
             !
             if ((kcs(nd, m) == 1 .and. kcs(nu, m) == 0) .or. (n == nmaxus)) then
                xz(n, m) = 2.0_fp*(xcor(nd, m) + xcor(nd, md))/2.0_fp - xz(nd, m)
                yz(n, m) = 2.0_fp*(ycor(nd, m) + ycor(nd, md))/2.0_fp - yz(nd, m)
                if (sferic) then
                   xm  = 1.5*xcor(nd, m ) - 0.5*xcor(ndd, m )
                   ym  = 1.5*ycor(nd, m ) - 0.5*ycor(ndd, m )
                   xmd = 1.5*xcor(nd, md) - 0.5*xcor(ndd, md)
                   ymd = 1.5*ycor(nd, md) - 0.5*ycor(ndd, md)
                   call angle(sferic      ,xmd       ,ymd       ,xm        ,ym        , &
                            & alfas(n, m) ,gdp       )
                   alfas(n, m) = alfas(n, m) * raddeg
                else
                   dydknd = (ycor(nd, m) - ycor(nd, md) + ycor(ndd, m) - ycor(ndd, md)) / 2.0_fp
                   dydksi = 2.*(ycor(nd, m) - ycor(nd, md)) - dydknd
                   dxdknd = (xcor(nd, m) - xcor(nd, md) + xcor(ndd, m) - xcor(ndd, md)) / 2.0_fp
                   dxdksi = 2.*(xcor(nd, m) - xcor(nd, md)) - dxdknd
                   if (abs(dxdksi) < small .and. abs(dydksi) < small) then
                      alfas(n, m) = 0.0_fp
                   else
                      alfas(n, m) = atan2(dydksi, dxdksi) * raddeg
                   endif
                endif
             endif
          endif
       enddo
    enddo
    !
    ! Compute XZ, YZ and ALFAS (open LOWER N boundaries) from nmaxus->1
    !
    do n = nmaxus, 1, -1
       nd = max(1, n - 1)
       nu = min(n + 1, nmaxus)
       do m = 1, mmax
          md = max(1, m - 1)
          mu = min(m + 1, mmax)
          if (kcs(n, m) == 2) then
             !
             ! NOTE: kcs (nu,m) <> 1 for nu = nmaxus
             !       and always 4 (xcor,ycor) coordinates around kcs = 1
             !
             if ((kcs(nd, m) == 0 .and. kcs(nu, m) == 1) .or. (n == 1)) then
                xz(n, m) = 2.0_fp*(xcor(n, m) + xcor(n, md))/2.0_fp - xz(nu, m)
                yz(n, m) = 2.0_fp*(ycor(n, m) + ycor(n, md))/2.0_fp - yz(nu, m)
                if (sferic) then
                   xm  = 1.50_fp*xcor(n, m ) - 0.5*xcor(nu, m )
                   ym  = 1.50_fp*ycor(n, m ) - 0.5*ycor(nu, m )
                   xmd = 1.50_fp*xcor(n, md) - 0.5*xcor(nu, md)
                   ymd = 1.50_fp*ycor(n, md) - 0.5*ycor(nu, md)
                   call angle(sferic      ,xmd       ,ymd       ,xm        ,ym        , &
                            & alfas(n, m) ,gdp       )
                   alfas(n, m) = alfas(n, m) * raddeg
                else
                   dydknu = (ycor(nu, m) - ycor(nu, md) + ycor(n, m) - ycor(n, md)) / 2.0_fp
                   dydksi = 2.0_fp*(ycor(n, m) - ycor(n, md)) - dydknu
                   dxdknu = (xcor(nu, m) - xcor(nu, md) + xcor(n, m) - xcor(n, md)) / 2.0_fp
                   dxdksi = 2.0_fp*(xcor(n, m) - xcor(n, md)) - dxdknu
                   if (abs(dxdksi) < small .and. abs(dydksi) < small) then
                      alfas(n, m) = 0.0_fp
                   else
                      alfas(n, m) = atan2(dydksi, dxdksi) * raddeg
                   endif
                endif
             endif
          endif
       enddo
    enddo
    !
    ! Compute XZ, YZ and ALFAS (open right M boundaries) 1->mmax
    !
    do m = 1, mmax
       md  = max(1, m - 1)
       mu  = min(m + 1, mmax)
       mdd = md - 1
       do n = 1, nmaxus
          nd = max(1, n - 1)
          nu = min(n + 1, nmaxus)
          if (kcs(n, m) == 2) then
             !
             ! NOTE: kcs (n,md) <> 1 for md = 1
             !       and always 4 (xcor,ycor) coordinates around kcs = 1
             !
             if ((kcs(n, md) == 1 .and. kcs(n, mu) == 0) .or. (m == mmax)) then
                xz(n, m) = 2.0_fp*(xcor(n, md) + xcor(nd, md))/2.0_fp - xz(n, md)
                yz(n, m) = 2.0_fp*(ycor(n, md) + ycor(nd, md))/2.0_fp - yz(n, md)
                if (sferic) then
                   xm  = (xcor(n, md) + xcor(nd, md)) / 2.0_fp
                   ym  = (ycor(n, md) + ycor(nd, md)) / 2.0_fp
                   xmu = 2.0_fp*xm - (xcor(n, mdd) + xcor(nd, mdd)) / 2.0_fp
                   ymu = 2.0_fp*ym - (ycor(n, mdd) + ycor(nd, mdd)) / 2.0_fp
                   call angle(sferic      ,xm        ,ym        ,xmu       ,ymu       , &
                            & alfas(n, m) ,gdp       )
                   alfas(n, m) = alfas(n, m) * raddeg
                else
                   ycnm   = 2.0_fp*ycor(n , md) - ycor(n , mdd)
                   ycndm  = 2.0_fp*ycor(nd, md) - ycor(nd, mdd)
                   dydksi = (ycnm - ycor(n, md) + ycndm - ycor(nd, md)) / 2.0_fp
                   xcnm   = 2.0_fp*xcor(n , md) - xcor(n , mdd)
                   xcndm  = 2.0_fp*xcor(nd, md) - xcor(nd, mdd)
                   dxdksi = (xcnm - xcor(n, md) + xcndm - xcor(nd, md)) / 2.0_fp
                   if (abs(dxdksi) < small .and. abs(dydksi) < small) then
                      alfas(n, m) = 0.0_fp
                   else
                      alfas(n, m) = atan2(dydksi, dxdksi) * raddeg
                   endif
                endif
             endif
          endif
       enddo
    enddo
    !
    ! Compute XZ, YZ and ALFAS (open left M boundaries) mmax->1
    !
    do m = mmax, 1, -1
       md = max(1, m - 1)
       mu = min(m + 1, mmax)
       do n = 1, nmaxus
          nd = max(1, n - 1)
          nu = min(n + 1, nmaxus)
          if (kcs(n, m) == 2) then
             !
             ! NOTE: kcs (n,mu) <> 1 for mu = mmax
             !       and always 4 (xcor,ycor) coordinates around kcs = 1
             !
             if ((kcs(n, md) == 0 .and. kcs(n, mu) == 1) .or. (m == 1)) then
                xz(n, m) = 2.0_fp*(xcor(n, m) + xcor(nd, m))/2.0_fp - xz(n, mu)
                yz(n, m) = 2.0_fp*(ycor(n, m) + ycor(nd, m))/2.0_fp - yz(n, mu)
                if (sferic) then
                   xm  = (xcor(n, m) + xcor(nd, m)) / 2.0_fp
                   ym  = (ycor(n, m) + ycor(nd, m)) / 2.0_fp
                   xmd = 2.0_fp*xm - (xcor(n, mu) + xcor(nd, mu)) / 2.0_fp
                   ymd = 2.0_fp*ym - (ycor(n, mu) + ycor(nd, mu)) / 2.0_fp
                   call angle(sferic      ,xmd       ,ymd       ,xm        ,ym        , &
                            & alfas(n, m) ,gdp       )
                   alfas(n, m) = alfas(n, m) * raddeg
                else
                   ycnmd  = 2.0_fp*ycor(n , m) - ycor(n , mu)
                   ycndmd = 2.0_fp*ycor(nd, m) - ycor(nd, mu)
                   dydksi = (ycor(n, m) - ycnmd + ycor(nd, m) - ycndmd) / 2.0_fp
                   xcnmd  = 2.0_fp*xcor(n , m) - xcor(n , mu)
                   xcndmd = 2.0_fp*xcor(nd, m) - xcor(nd, mu)
                   dxdksi = (xcor(n, m) - xcnmd + xcor(nd, m) - xcndmd) / 2.0_fp
                   if (abs(dxdksi) < small .and. abs(dydksi) < small) then
                      alfas(n, m) = 0.0_fp
                   else
                      alfas(n, m) = atan2(dydksi, dxdksi) * raddeg
                   endif
                endif
             endif
          endif
       enddo
    enddo
    !
    ! Compute: GUZ, GVZ, GUD, GVD, GSQIU and GSQIV
    !
    do m = mmax, 1, -1
       md = max(m - 1, 1)
       mu = min(m + 1, mmax)
       do n = 1, nmaxus
          nd = max(1, n - 1)
          nu = min(n + 1, nmaxus)
          guz(n, m) = 0.5_fp * (guu(n, m) + guu(n, md))
          gvz(n, m) = 0.5_fp * (gvv(n, m) + gvv(nd, m))
          gud(n, m) = 0.5_fp * (guu(n, m) + guu(nu, m))
          gvd(n, m) = 0.5_fp * (gvv(n, m) + gvv(n, mu))
          !
          if (kcu(n, m) > 0) then
             if (new_area_method) then
                gsqiu(n, m) = 2.0_fp / (gsqs(n, m)+gsqs(n,mu))
             else
                gsqiu(n, m) = 1.0_fp / (gvu(n, m)*guu(n, m))
             endif
          else
             gsqiu(n, m) = 0.0_fp
          endif
          if (kcv(n, m) > 0) then
             if (new_area_method) then
                gsqiv(n, m) = 2.0_fp / (gsqs(n, m)+gsqs(nu,m))
             else
                gsqiv(n, m) = 1.0_fp / (guv(n, m)*gvv(n, m))
             endif
          else
             gsqiv(n, m) = 0.0_fp
          endif
       enddo
    enddo
    !
    ! Check computed GUZ, GVZ, GUD and GVD
    !
    do n = 1, nmaxus
       do m = 1, mmax
          if (kcs(n, m) == 1) then
             if (guz(n, m) <= 0.) then
                write (errmsg(10:), '(2i5)') m, n
                errmsg(:9) = 'GUZ   at '
                call prterr(lundia    ,'V016'    ,errmsg    )
                success = .false.
             endif
             if (gvz(n, m) <= 0.) then
                write (errmsg(10:), '(2i5)') m, n
                errmsg(:9) = 'GVZ   at '
                call prterr(lundia    ,'V016'    ,errmsg    )
                success = .false.
             endif
             if (gud(n, m) <= 0.) then
                write (errmsg(10:), '(2i5)') m, n
                errmsg(:9) = 'GUD   at '
                call prterr(lundia    ,'V016'    ,errmsg    )
                success = .false.
             endif
             if (gvd(n, m) <= 0.) then
                write (errmsg(10:), '(2i5)') m, n
                errmsg(:9) = 'GVD   at '
                call prterr(lundia    ,'V016'    ,errmsg    )
                success = .false.
             endif
          endif
       enddo
    enddo
    !
    ! Calculate fcorio in case of spheric coordinates, whether gridfile or not
    !
    if (sferic) then
       do n = 1, nmaxus
          do m = 1, mmax
             fcorio(n, m) = sin(degrad*yz(n, m)) * 4.0_fp * pi / sidday
          enddo
       enddo
    endif
    !
    !      write (lundia,*) lundia
    !      write (lundia,'(a,$)') ' guu'
    !      call prisep(nst ,lundia,GUU  ,nmax  ,mmax         )
    !      write (lundia,'(a,$)') ' gvv'
    !      call prisep(nst ,lundia,GVV  ,nmax  ,mmax         )
    !      write (lundia,'(a,$)') ' guv'
    !      call prisep(nst ,lundia,GUV  ,nmax  ,mmax         )
    !      write (lundia,'(a,$)') ' gvu'
    !      call prisep(nst ,lundia,GVU  ,nmax  ,mmax         )
    !      write (lundia,'(a,$)') 'gsqs'
    !      call prisep(nst ,lundia,GSQS ,nmax  ,mmax         )
    !      write (lundia,'(a,$)') 'alfa'
    !      call prisep(nst ,lundia,ALFAS,nmax  ,mmax         )
    !
    !
    ! check grid orientation based on first internal point
    !
    CHK_ORIENT : do m = 2, mmax
       md = m - 1
       do n = 2, nmaxus
          nd = n - 1
          if (kcs(n, m) == 1) then
             xcell(1) = xcor(n , m)
             ycell(1) = ycor(n , m)
             xcell(2) = xcor(n , md)
             ycell(2) = ycor(n , md)
             xcell(3) = xcor(nd, md)
             ycell(3) = ycor(nd, md)
             xcell(4) = xcor(nd, m)
             ycell(4) = ycor(nd, m)
             if (clockwise(xcell,ycell)) then
                call prterr(lundia ,'P004' ,'Grid orientation incorrect: Delft3D requires (M,N) to form a counter-clockwise coordinate system.')
                success = .false.
             endif
             exit CHK_ORIENT
          endif
       enddo
    enddo CHK_ORIENT
    if (.not. success) then
       call d3stop (3,gdp)
    endif
end subroutine inigeo
