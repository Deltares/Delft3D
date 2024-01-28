subroutine difuflux(stage     ,lundia    ,kmax      ,nmmax     ,nmmaxj    , &
                  & lstsci    ,r0        ,r1        ,qxk       ,qyk       , &
                  & dicuv     ,guv       ,gvu       ,areau     ,areav     , &
                  & kfu       ,kfv       ,kfs       ,kcs       ,timest    , &
                  & icx       ,icy       ,lsed      ,s1        ,dps       , &
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
!    Function: Compute flux corresponding to DIFU
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp), dimension(:,:,:)         , pointer :: fluxu
    real(fp), dimension(:,:,:)         , pointer :: fluxuc
    real(fp), dimension(:,:,:)         , pointer :: fluxv
    real(fp), dimension(:,:,:)         , pointer :: fluxvc
    type (flwoutputtype)               , pointer :: flwoutput
    !
    logical                            , pointer :: massbal
    real(fp)                           , pointer :: dryflc
!
! Global variables
!
    integer                                                  , intent(in) :: icx    !  Description and declaration in inout.igs
    integer                                                  , intent(in) :: icy    !  Description and declaration in inout.igs
    integer                                                  , intent(in) :: kmax   !  Description and declaration in inout.igs
    integer                                                  , intent(in) :: lsed   !  Description and declaration in inout.igs
    integer                                                  , intent(in) :: nmmax  !  Description and declaration in inout.igs
    integer                                                  , intent(in) :: nmmaxj !  Description and declaration in inout.igs
    integer                                                  , intent(in) :: lstsci !  Description and declaration in inout.igs
    integer                                                  , intent(in) :: lundia !  Description and declaration in inout.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                , intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                , intent(in) :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                , intent(in) :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(fp)                                                 , intent(in) :: timest !!  Half Integration time step [sec.]
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: areau
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: areav
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: dicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)             , intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in) :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci) , intent(in) :: r0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci) , intent(in) :: r1     !  Description and declaration in esm_alloc_real.f90
    character(8)                                             , intent(in) :: stage  !!  First or second half time step
!
! Local variables
!
    integer  :: iad1
    integer  :: iad2
    integer  :: iad3
    integer  :: istat
    integer  :: j1
    integer  :: j2
    integer  :: j3
    integer  :: k
    integer  :: l
    integer  :: maskval
    integer  :: nddm
    integer  :: ndm
    integer  :: nm
    integer  :: nmd
    integer  :: nmdd
    integer  :: nmu
    integer  :: nmuu
    integer  :: nmuuu
    integer  :: num
    integer  :: nuum
    integer  :: nuuum
    real(fp) :: flux
    real(fp) :: flux1
    real(fp) :: qxu
    real(fp) :: qyv
    real(fp) :: hmin
!
!! executable statements -------------------------------------------------------
!
    fluxu          => gdp%gdflwpar%fluxu
    fluxuc         => gdp%gdflwpar%fluxuc
    fluxv          => gdp%gdflwpar%fluxv
    fluxvc         => gdp%gdflwpar%fluxvc
    flwoutput      => gdp%gdflwpar%flwoutput
    !
    massbal        => gdp%gdmassbal%massbal
    dryflc         => gdp%gdnumeco%dryflc
    !
    if (.not. flwoutput%difuflux .and. lsed==0 .and. .not. massbal) return
    !
    istat = 0
    if (.not. associated(gdp%gdflwpar%fluxu)) then
       if (istat==0) allocate (gdp%gdflwpar%fluxu(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
       if (istat==0) allocate (gdp%gdflwpar%fluxv(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
       if (flwoutput%cumdifuflux) then
          if (istat==0) allocate (gdp%gdflwpar%fluxuc(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
          if (istat==0) allocate (gdp%gdflwpar%fluxvc(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
       endif
       !
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'DIFUFLUX: memory alloc error')
          call d3stop(1, gdp)
       endif
       !
       ! update local pointers
       !
       fluxu          => gdp%gdflwpar%fluxu
       fluxuc         => gdp%gdflwpar%fluxuc
       fluxv          => gdp%gdflwpar%fluxv
       fluxvc         => gdp%gdflwpar%fluxvc
       !
       if (flwoutput%cumdifuflux) then
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
    ! Advective transport in X-direction
    !
    do k = 1,kmax
       do nm = 1,nmmax
          if (kfu(nm)==1 .and. kcs(nm)>0) then 
             nmdd  = nm - 2*icx
             nmd   = nm - icx
             nmu   = nm + icx
             nmuu  = nm + 2*icx
             nmuuu = min(nmmaxj , nm+3*icx)
             !
             flux    = 0.5_fp*(dicuv(nm, k) + dicuv(nmu, k))/(0.7_fp*gvu(nm))
             maskval = max(0, 2 - kcs(nm))
             flux1   = areau(nm, k)*flux*maskval
             !
             if (stage == 'stage2  ') then
                qxu = qxk(nm,k)
                !
                ! Explicit direction: advection
                !
                iad1 = kfu(nm)
                iad2 = iad1*kfu(nmu)*kfu(nmd)
                if (kcs(nm)==2) iad2=0
                !
                do l = 1,lstsci
                   if (qxu > 0.0) then
                      flux = 0.5_fp*qxu*((2*iad1-iad2)*r0(nm ,k,l) + &
                                      &       iad2 *r0(nmu,k,l))
                   else
                      flux = 0.5_fp*qxu*((2*iad1-iad2)*r0(nmu,k,l) + &
                                      &       iad2 *r0(nm ,k,l))
                   endif
                   fluxu(nm,k,l) = fluxu(nm,k,l) + flux
                enddo
                !
                ! Explicit direction: diffusion
                !
                hmin = min(s1(nm) + real(dps(nm),fp), s1(nmu) + real(dps(nmu),fp))
                if (hmin > dryflc) then
                   do l = 1,lstsci
                      fluxu(nm,k,l) = fluxu(nm,k,l) + &
                                    & flux1 * (r0(nm,k,l) - r0(nmu,k,l))
                   enddo
                endif
             else
                !
                ! Implicit direction: advection
                !
                qxu = qxk(nm,k)/6.0_fp
                if (qxu > 0.0_fp) then
                   iad1 = kfu(nm   )
                   iad2 = iad1 * kfu(nmd  )
                   iad3 = iad2 * kfu(nmdd )
                   !
                   j1 = 6*iad1+3*iad2+  iad3
                   j2 =       -3*iad2-2*iad3
                   j3 =                 iad3
                   !
                   do l = 1,lstsci
                      flux = ( j1 * r1(nm,k,l) + j2 * r1(nmd,k,l) &
                           & + j3 * r1(nmdd,k,l) ) * qxu
                      fluxu(nm,k,l) = fluxu(nm,k,l) + flux
                   enddo
                else
                   iad1 = kfu(nm   )
                   iad2 = iad1 * kfu(nmu  )
                   iad3 = iad2 * kfu(nmuu )
                   !
                   j1 = 6*iad1+3*iad2+  iad3
                   j2 =       -3*iad2-2*iad3
                   j3 =                 iad3
                   !
                   do l = 1,lstsci
                      flux = ( j1 * r1(nmu,k,l) + j2 * r1(nmuu,k,l) &
                           & + j3 * r1(nmuuu,k,l) ) * qxu
                      fluxu(nm,k,l) = fluxu(nm,k,l) + flux
                   enddo
                endif
                !
                ! Implicit direction: diffusion
                !
                hmin = min(s1(nm) + real(dps(nm),fp), s1(nmu) + real(dps(nmu),fp))
                if (hmin > dryflc) then
                   do l = 1,lstsci
                      fluxu(nm,k,l) = fluxu(nm,k,l) + &
                                    & flux1 * (r1(nm,k,l) - r1(nmu,k,l))
                   enddo
                endif
             endif    ! stage 2/1
          endif       ! kfu == 1 .and. kcs /=0
       enddo          ! nm
    enddo             ! k
    !
    ! Advective transport in Y-direction
    !
    do k = 1,kmax
       do nm = 1,nmmax
          if (kfs(nm)==1 .and. kcs(nm)>0) then 
             nddm  = nm - 2*icy
             ndm   = nm - icy
             num   = nm + icy
             nuum  = nm + 2*icy
             nuuum = min(nmmaxj , nm+3*icy)
             !
             flux    = 0.5_fp*(dicuv(nm, k) + dicuv(num, k))/(0.7_fp*guv(nm))
             maskval = max(0, 2 - kcs(nm))
             flux1   = areav(nm, k)*flux*maskval
             !
             if (stage == 'stage1  ') then
                qyv = qyk(nm,k)
                !
                ! Explicit direction: advection
                !
                iad1 = kfv(nm)
                iad2 = iad1*kfv(num)*kfv(ndm)
                if (kcs(nm) == 2) iad2=0
                !
                do l = 1,lstsci
                   if (qyv > 0.0) then
                      flux = 0.5_fp*qyv*((2*iad1-iad2)*r0(nm ,k,l) + &
                                      &       iad2 *r0(num,k,l))
                   else
                      flux = 0.5_fp*qyv*((2*iad1-iad2)*r0(num,k,l) + &
                                      &       iad2 *r0(nm ,k,l))
                   endif
                   fluxv(nm,k,l) = fluxv(nm,k,l) + flux
                enddo
                !
                ! Explicit direction: diffusion
                !
                hmin = min(s1(nm) + real(dps(nm),fp), s1(num) + real(dps(num),fp))
                if (hmin > dryflc) then
                   do l = 1,lstsci
                      fluxv(nm,k,l) = fluxv(nm,k,l) + &
                                    & flux1 * (r0(nm,k,l) - r0(num,k,l))
                   enddo
                endif
             else
                !
                ! Implicit direction: advection
                !
                qyv = qyk(nm,k)/6.0
                if (qyv > 0.0) then
                   iad1 = kfv(nm   )
                   iad2 = iad1 * kfv(ndm  )
                   iad3 = iad2 * kfv(nddm )
                   !
                   j1 = 6*iad1+3*iad2+  iad3
                   j2 =       -3*iad2-2*iad3
                   j3 =                 iad3
                   !
                   do l = 1,lstsci
                      flux = ( j1 * r1(nm,k,l) + j2 * r1(ndm,k,l) &
                           & + j3 * r1(nddm,k,l) ) * qyv
                      fluxv(nm,k,l) = fluxv(nm,k,l) + flux
                   enddo
                else
                   iad1 = kfv(nm   )
                   iad2 = iad1 * kfv(num  )
                   iad3 = iad2 * kfv(nuum )
                   !
                   j1 = 6*iad1+3*iad2+  iad3
                   j2 =       -3*iad2-2*iad3
                   j3 =                 iad3
                   !
                   do l = 1,lstsci
                      flux = ( j1 * r1(num,k,l) + j2 * r1(nuum,k,l) &
                           & + j3 * r1(nuuum,k,l) ) * qyv
                      fluxv(nm,k,l) = fluxv(nm,k,l) + flux
                   enddo
                endif
                !
                ! Implicit direction: diffusion
                !
                hmin = min(s1(nm) + real(dps(nm),fp), s1(num) + real(dps(num),fp))
                if (hmin > dryflc) then
                   do l = 1,lstsci
                      fluxv(nm,k,l) = fluxv(nm,k,l) + &
                                    & flux1 * (r1(nm,k,l) - r1(num,k,l))
                   enddo
                endif
             endif    ! stage 1/2
          endif       ! kfu == 1 .and. kcs /=0
       enddo          ! nm
    enddo             ! k
    !
    ! Cumulative flux
    !
    if (flwoutput%cumdifuflux) then
       ! Do loops are needed to avoid a stack overflow
       do l = 1,lstsci
          do k = 1,kmax
             do nm = 1,nmmax
                fluxuc(nm,k,l) = fluxuc(nm,k,l) + fluxu(nm,k,l) * timest
                fluxvc(nm,k,l) = fluxvc(nm,k,l) + fluxv(nm,k,l) * timest
             enddo
          enddo
       enddo
    endif
end subroutine difuflux
