subroutine z_difuflux(stage   ,lundia    ,kmax      ,nmmax     ,nmmaxj    , &
                  & lstsci    ,r0        ,r1        ,qxk       ,qyk       , &
                  & u         ,v         ,s1        ,dps       , &
                  & dicuv     ,guv       ,gvu       ,areau     ,areav     , &
                  & kfuz1     ,kfvz1     ,kfsz1     ,kcs       ,kfs       , &
                  & kfu       ,kfuz0     ,kfv       ,kfvz0     , &
                  & kfsmx0    ,kfsmax    ,kfsz0     , &
                  & kfumin    ,kfumx0    ,kfvmin    ,kfvmx0    ,sigdif    , &
                  & timest    ,icx       ,icy       ,gdp       )
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
!    Function: Compute flux corresponding to Z_DIFU
!
! Method used:
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
    integer                             , pointer :: lsed
    real(fp) , dimension(:,:,:)         , pointer :: fluxu
    real(fp) , dimension(:,:,:)         , pointer :: fluxuc
    real(fp) , dimension(:,:,:)         , pointer :: fluxv
    real(fp) , dimension(:,:,:)         , pointer :: fluxvc
    real(fp)                            , pointer :: dryflc
    character(13)                       , pointer :: trasol
    type (flwoutputtype)                , pointer :: flwoutput
    type (gd_flwpar)                    , pointer :: gdflwpar
!
! Global variables
!
    integer                                                       , intent(in) :: icx    !  Description and declaration in inout.igs
    integer                                                       , intent(in) :: icy    !  Description and declaration in inout.igs
    integer                                                       , intent(in) :: kmax   !  Description and declaration in inout.igs
    integer                                                       , intent(in) :: nmmax  !  Description and declaration in inout.igs
    integer                                                       , intent(in) :: nmmaxj !  Description and declaration in inout.igs
    integer                                                       , intent(in) :: lstsci !  Description and declaration in inout.igs
    integer                                                       , intent(in) :: lundia !  Description and declaration in inout.igs
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90

    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kfumx0 !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: kfuz0  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kfvmin !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kfvmx0 !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: kfvz0  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kfsmx0
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: kfsmax
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: kfsz0  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: kfsz1  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: kfuz1  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: kfvz1  !  Description and declaration in esm_alloc_int.f90
    real(fp)                                                      , intent(in) :: timest !!  Half Integration time step [sec.]
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: areau
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: areav
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: dicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(prec)   , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci) , intent(in) :: r0     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci) , intent(in) :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(lstsci)                              , intent(in) :: sigdif !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: u
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: v
    character(8)                                                  , intent(in) :: stage  !!  First or second half time step
!
! Local variables
!
    integer  :: istat
    integer  :: k
    integer  :: l
    integer  :: kenu, kenv
    integer  :: kmin
    integer  :: kkmin, kkminu, kkminv
    integer  :: mink2
    integer  :: mink, mink_old, mink_new
    integer  :: ndm
    integer  :: nm
    integer  :: nmd
    integer  :: nmu
    integer  :: nmuu
    integer  :: num
    integer  :: nuum
    real(fp) :: cl, cr, difl, difr
    real(fp) :: cfl
    real(fp) :: flux
    real(fp) :: hmin
    real(fp) :: r00
    real(fp) :: rr1, rr2
    real(fp) :: rmax
    real(fp) :: rmin
    real(fp) :: switch

!
!! executable statements -------------------------------------------------------
!
    lsed           => gdp%d%lsed
    fluxu          => gdp%gdflwpar%fluxu
    fluxuc         => gdp%gdflwpar%fluxuc
    fluxv          => gdp%gdflwpar%fluxv
    fluxvc         => gdp%gdflwpar%fluxvc
    dryflc         => gdp%gdnumeco%dryflc
    trasol         => gdp%gdtricom%trasol
    flwoutput      => gdp%gdflwpar%flwoutput
    gdflwpar       => gdp%gdflwpar

    !
    if (.not. flwoutput%difuflux .and. lsed == 0) return
    !
    istat = 0
    if (.not. associated(gdflwpar%fluxu)) then
       if (istat==0) allocate (gdflwpar%fluxu(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
       if (istat==0) allocate (gdflwpar%fluxv(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
       if (flwoutput%cumdifuflux) then
          if (istat==0) allocate (gdflwpar%fluxuc(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
          if (istat==0) allocate (gdflwpar%fluxvc(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
       endif
       !
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'Z_DIFUFLUX: memory alloc error')
          call d3stop(1, gdp)
       endif
       !
       ! Define GDP pointers after the GDP allocations
       !
       fluxu          => gdp%gdflwpar%fluxu
       fluxv          => gdp%gdflwpar%fluxv
       flwoutput      => gdp%gdflwpar%flwoutput
       gdflwpar       => gdp%gdflwpar
       !
       if (flwoutput%cumdifuflux) then
          fluxuc => gdp%gdflwpar%fluxuc
          fluxvc => gdp%gdflwpar%fluxvc
          fluxuc = 0.0_fp
          fluxvc = 0.0_fp
       endif
    endif
    !
    ! Initialization
    !
    fluxu = 0.0_fp
    fluxv = 0.0_fp
    !

    if (trasol == 'van leer-2   ') then

       do nm = 1, nmmax
          nmd  = nm  - icx
          nmu  = nm  + icx
          nmuu = nmu + icx
          ndm  = nm  - icy
          num  = nm  + icy
          nuum = num + icy
          !
          ! ADVECTIVE TRANSPORT IN X-DIRECTION
          !
          if (kfu(nm) /= 0) then
             kmin = max(kfumin(nm), 1)
             do k = kmin, kmax
                if (kfuz0(nm,k) == 1) then
                   cfl = u(nm, k) * timest / gvu(nm)
                   if (qxk(nm, k) > 0.0_fp) then
                      do l = 1, lstsci
                         rr1 = abs(r0(nmd, k, l) - 2.0_fp*r0(nm, k, l) + r0(nmu, k, l))
                         rr2 = abs(r0(nmd, k, l) - r0(nmu, k, l))
                         if (kfsz0(nmd, k)*kfsz0(nmu, k) == 0 .or. rr1 >= rr2 .or. rr2 < eps_fp .or. kcs(nm)==3 .or. kcs(nmu)==3) then 
                            r00 = r0(nm, k, l)
                         else
                            r00 = r0(nm , k, l)                                     &
                                & + (1.0_fp - cfl)*(r0(nm , k, l) - r0(nmd, k, l))  &
                                &                 *(r0(nmu, k, l) - r0(nm , k, l))  &
                                &                 /(r0(nmu, k, l) - r0(nmd, k, l))
                         endif
                         flux = qxk(nm, k) * r00
                         if (kcs(nm) == 1) then
                            fluxu(nm, k, l) = fluxu(nm, k, l) + flux
                         endif
                      enddo
                   elseif (qxk(nm, k) < 0.0_fp) then
                      do l = 1, lstsci
                         rr1 = abs(r0(nmuu, k, l) - 2.0_fp*r0(nmu, k, l) + r0(nm, k, l))
                         rr2 = abs(r0(nmuu, k, l) - r0(nm, k, l))
                         if (kfsz0(nm, k)*kfsz0(nmuu, k) == 0 .or. rr1 >= rr2 .or. rr2 < eps_fp .or. kcs(nm)==3 .or. kcs(nmu)==3) then
                            r00 = r0(nmu, k, l)
                         else
                            r00 = r0(nmu, k, l)                                      &
                                & + (1.0_fp + cfl)*(r0(nm , k, l) - r0(nmu , k, l))  &
                                &                 *(r0(nmu, k, l) - r0(nmuu, k, l))  &
                                &                 /(r0(nm , k, l) - r0(nmuu, k, l))
                         endif
                         flux = qxk(nm, k) * r00
                         if (kcs(nm) == 1) then
                            fluxu(nm, k, l) = fluxu(nm, k, l) + flux
                         endif
                      enddo
                   endif
                endif
             enddo
          endif
          !
          ! DIFFUSIVE TRANSPORT IN X-DIRECTION
          !
          hmin = min(s1(nm) + real(dps(nm),fp), s1(nmu) + real(dps(nmu),fp))
          if (kfs(nm)*kfs(nmu) == 1 .and. hmin > dryflc) then
             do k = kfumin(nm), kmax
                if (kfuz0(nm, k)==1 .and. kfsz0(nm, k)*kfsz0(nmu, k)==1) then
                   difl = dicuv(nm, k)
                   difr = dicuv(nmu, k)
                   kenu = max(0, abs(2 - kcs(nm)))
                   do l = 1, lstsci
                      cl              = r0(nm, k, l)
                      cr              = r0(nmu, k, l)
                      flux            = 0.5_fp*(cr - cl)*(difl + difr)/(sigdif(l)*gvu(nm))
                      fluxu(nm, k, l) = fluxu(nm, k, l) - areau(nm, k)*flux*kenu
                   enddo
                endif
             enddo
          endif
          !
          ! ADVECTIVE TRANSPORT IN Y-DIRECTION
          !
          if (kfv(nm) /= 0) then
             kmin = max(kfvmin(nm), 1)
             do k = kmin, kmax
                if (kfvz0(nm,k) == 1) then
                   cfl = v(nm, k) * timest / guv(nm)
                   if (qyk(nm, k) > 0.0_fp) then
                      do l = 1, lstsci
                         rr1 = abs(r0(ndm, k, l) - 2.0_fp*r0(nm, k, l) + r0(num, k, l))
                         rr2 = abs(r0(ndm, k, l) - r0(num, k, l))
                         if (kfsz0(ndm, k)*kfsz0(num, k) == 0 .or. rr1 >= rr2 .or. rr2 < eps_fp .or. kcs(nm)==3 .or. kcs(num)==3) then
                            r00 = r0(nm, k, l)
                         else
                            r00 = r0(nm , k, l)                                     &
                                & + (1.0_fp - cfl)*(r0(nm , k, l) - r0(ndm, k, l))  &
                                &                 *(r0(num, k, l) - r0(nm , k, l))  &
                                &                 /(r0(num, k, l) - r0(ndm, k, l))
                         endif
                         flux = qyk(nm, k) * r00
                         if (kcs(nm) == 1) then 
                            fluxv(nm, k, l) = fluxv(nm, k, l) + flux
                         endif
                      enddo
                   elseif (qyk(nm, k) < 0.0_fp) then
                      do l = 1, lstsci
                         rr1 = abs(r0(nuum, k, l) - 2.0_fp*r0(num, k, l) + r0(nm, k, l))
                         rr2 = abs(r0(nuum, k, l) - r0(nm, k, l))
                         if (kfsz0(nm, k)*kfsz0(nuum, k) == 0 .or. rr1 >= rr2 .or. rr2 < eps_fp .or. kcs(nm)==3 .or. kcs(num)==3) then
                            r00 = r0(num, k, l)
                         else
                            r00 = r0(num, k, l)                                      &
                                & + (1.0_fp + cfl)*(r0(nm , k, l) - r0(num , k, l))  &
                                &                 *(r0(num, k, l) - r0(nuum, k, l))  &
                                &                 /(r0(nm , k, l) - r0(nuum, k, l))
                         endif
                         flux = qyk(nm, k) * r00
                         if (kcs(nm) == 1) then
                            fluxv(nm, k, l) = fluxv(nm, k, l) + flux
                         endif
                      enddo
                   endif
                endif
             enddo
          endif
          !
          ! DIFFUSIVE TRANSPORT IN Y-DIRECTION
          !
          hmin = min(s1(nm) + real(dps(nm),fp), s1(num) + real(dps(num),fp))
          if (kfs(nm)*kfs(num) == 1 .and. hmin > dryflc) then
             do k = kfvmin(nm), kmax
                if (kfvz0(nm, k)==1 .and. kfsz0(nm, k)*kfsz0(num, k)==1) then
                   difl = dicuv(nm, k)
                   difr = dicuv(num, k)
                   kenv = max(0, abs(2 - kcs(nm)))
                   do l = 1, lstsci
                      cl              = r0(nm, k, l)
                      cr              = r0(num, k, l)
                      flux            = 0.5_fp*(cr - cl)*(difl + difr)/(sigdif(l)*guv(nm))
                      fluxv(nm, k, l) = fluxv(nm, k, l) - areav(nm, k)*flux*kenv
                   enddo
                endif
             enddo
          endif
       enddo

    elseif(trasol == 'iupw         ') then

       do nm = 1, nmmax
          !
          ! ADVECTIVE TRANSPORT IN X-DIRECTION
          !
          nmu  = nm + icx
          num  = nm + icy
          if (kfu(nm) /= 0 .and. kcs(nm)*kcs(nmu) /= 0) then
             kmin   = max(kfumin(nm), 1)
             kkmin  = min(kfsmx0(nm), kfsmax(nm))
             kkminu = min(kfsmx0(nmu), kfsmax(nmu))
             do k = kmin, min(kkmin, kkminu)
                do l = 1, lstsci
                   if (qxk(nm,k) > 0.0_fp) then
                      fluxu(nm,k,l) = fluxu(nm,k,l) + qxk(nm, k) * r1(nm , k, l)
                   elseif (qxk(nm,k) < 0.0_fp) then
                      fluxu(nm,k,l) = fluxu(nm,k,l) + qxk(nm, k) * r1(nmu, k, l)
                   endif
                enddo
             enddo
          endif
          !
          ! Adjacent cell water fall correction (explicit)
          !
          if (kfumx0(nm) > min(kkmin,kkminu)) then
             do k = min(kkmin,kkminu)+1, kfumx0(nm)
                if (qxk(nm, k) > 0.0_fp) then
                   do l = 1, lstsci
                      flux = qxk(nm, k) * r0(nm , k, l)
                      if (kcs(nm) == 1) then
                         fluxu(nm, k, l) = fluxu(nm, k, l) + flux
                      endif
                   enddo
                elseif (qxk(nm, k) < 0.0_fp) then
                   do l = 1, lstsci
                      flux = qxk(nm, k) * r0(num, k, l)
                      if (kcs(nm) == 1) then
                         fluxu(nm, kkmin, l) = fluxu(nm, kkmin, l) + flux
                      endif
                   enddo
                endif
             enddo
          endif
          !
          ! ADVECTIVE TRANSPORT IN Y-DIRECTION
          !
          if (kfv(nm) /= 0  .and. kcs(nm)*kcs(num) /= 0) then
             kmin   = max(kfvmin(nm), 1)
             kkmin  = min(kfsmx0(nm), kfsmax(nm))
             kkminv = min(kfsmx0(num), kfsmax(num))
             do k = kmin, min(kkmin,kkminv)
                do l = 1, lstsci
                   if (qyk(nm,k) > 0.0_fp) then
                      fluxv(nm,k,l) = fluxv(nm,k,l) + qyk(nm, k) * r1(nm , k, l)
                   elseif (qyk(nm,k) < 0.0_fp) then
                      fluxv(nm,k,l) = fluxv(nm,k,l) + qyk(nm, k) * r1(num, k, l)
                   endif
                enddo
             enddo
          endif
          !
          ! Adjacent cell water fall correction (explicit)
          !
          if (kfvmx0(nm) > min(kkmin,kkminv)) then
             do k = min(kkmin,kkminv)+1, kfvmx0(nm)
                if (qyk(nm, k) > 0.0_fp) then
                   do l = 1, lstsci
                      flux = qyk(nm, k) * r0(nm , k, l)
                      if (kcs(nm) == 1) then
                         fluxv(nm, k, l) = fluxv(nm, k, l) + flux
                      endif
                   enddo
                elseif (qyk(nm, k) < 0.0_fp) then
                   do l = 1, lstsci
                      flux = qyk(nm, k) * r0(num, k, l)
                      if (kcs(nm) == 1) then
                         fluxv(nm, kkmin, l) = fluxv(nm, kkmin, l) + flux
                      endif
                   enddo
                endif
             enddo
          endif
          !
          ! DIFFUSIVE TRANSPORT IN X-DIRECTION
          !
          hmin = min(s1(nm) + real(dps(nm),fp), s1(nmu) + real(dps(nmu),fp))
          if (kfu(nm) == 1 .and. hmin > dryflc) then
             mink_old = min(kfsmx0(nm),kfsmx0(nmu))
             mink_new = min(kfsmax(nm),kfsmax(nmu))
             mink     = min(mink_old  ,mink_new)
             do k = kfumin(nm), mink
                if (kfuz0(nm, k)==1 .and. kfsz0(nm, k)*kfsz0(nmu, k)==1) then
                   difl = dicuv(nm, k)
                   difr = dicuv(nmu, k)
                   flux = 0.5_fp * (difl + difr) / (0.7_fp*gvu(nm))
                   kenu = max(0, abs(2 - kcs(nm)))
                   do l = 1, lstsci
                      fluxu(nm,k,l) = fluxu(nm,k,l) + areau(nm, k)*flux*real(kenu,fp)
                   enddo
                endif
             enddo
          endif
          !
          ! DIFFUSIVE TRANSPORT IN Y-DIRECTION
          !
          hmin = min(s1(nm) + real(dps(nm),fp), s1(num) + real(dps(num),fp))
          if (kfv(nm) == 1 .and. hmin > dryflc) then
             mink_old = min(kfsmx0(nm),kfsmx0(num))
             mink_new = min(kfsmax(nm),kfsmax(num))
             mink     = min(mink_old  ,mink_new)
             do k = kfvmin(nm), mink
                if (kfvz0(nm, k)==1 .and. kfsz0(nm, k)*kfsz0(num, k)==1) then
                   difl = dicuv(nm, k)
                   difr = dicuv(num, k)
                   flux = 0.5_fp * (difl + difr) / (0.7_fp*guv(nm))
                   kenv = max(0, abs(2 - kcs(nm)))
                   do l = 1, lstsci
                      fluxv(nm,k,l) = fluxv(nm,k,l) + areav(nm, k)*flux*real(kenv,fp)
                   enddo
                endif
             enddo
          endif
       enddo
    endif
    !
    ! Cumulative flux
    !
    if (flwoutput%cumdifuflux) then
       if (icx == 1) then
         fluxuc = fluxuc + fluxu * timest
         fluxvc = fluxvc + fluxv * timest
       else
         fluxuc = fluxuc + fluxv * timest
         fluxvc = fluxvc + fluxu * timest
       endif
    endif
    !
    ! swap fluxu and fluxv for sediment transport in z layer morphology.
    ! Originally, switch was a full sized array and the code was without do-loops:
    !     switch = fluxu
    !     fluxu  = fluxv
    !     fluxv  = fluxu
    ! These statements may cause problems when the arrays are very big; values are put
    ! on the (restricted) stack
    ! Therefore, the primitive triple do loop is used, where switch can be a scalar
    !
    if (icx == 1) then
       do nm =  gdp%d%nmlb,gdp%d%nmub
           do k = 1, kmax
               do l =  1,lstsci
                 switch          = fluxu (nm,k,l)
                 fluxu (nm,k,l)  = fluxv (nm,k,l)
                 fluxv (nm,k,l)  = switch
               enddo
           enddo
       enddo
    endif
end subroutine z_difuflux
