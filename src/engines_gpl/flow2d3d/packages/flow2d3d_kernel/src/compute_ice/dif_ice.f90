subroutine dif_ice(hdt    ,nst       ,icx       ,icy       , &
              & j         ,nmmaxj    ,nmmax     ,kmax      , &
              & norow     ,irocol    ,kcs       ,kfs       ,kfu       ,kfv       , &
              & kfsice    ,kfssnw    ,toth_i    ,toth_w    ,f_w       , &
              & u_ice     ,v_ice     ,a_ice     , &
              & h_ice     ,h_snow    ,t_ice     ,t_snow    ,evap      , &
              & kspu      ,kspv      ,s1        ,pship     ,precip    , &
              & dps       ,dpu       ,dpv       ,sxice     ,sxsn      ,sxa       , & 
              & zs1       ,zs2       ,zs12      ,ut_ice    ,vt_ice    , &
              & r1        ,lstsci    ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                     
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
!  $Id: dif_ice.f90 66456 2020-04-26 14:09:37Z goede $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20190705_ice_modelling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_ice/dif_ice.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes thickness of ice and snow
!              It follows the Semtner approach.
! Method used: It follows the Semtner (1975) approach, which is
!              e.g. also applied in ROMS, CIOM and many other systems.
!              The current implementation only contains a thermodynamics
!              part. So, the ice dynamics (i.e. horizontal transport)
!              is lacking yet.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use meteo
    use timers
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                 , pointer :: lundia
    integer                 , pointer :: lsal
    real(fp)                , pointer :: ag
    real(fp)                , pointer :: cp_ice
    real(fp)                , pointer :: cp_snow
    real(fp)                , pointer :: k_ice
    real(fp)                , pointer :: k_snow
    real(fp)                , pointer :: lh_ice
    real(fp)                , pointer :: lh_snow
    real(fp)                , pointer :: rhow
    real(fp)                , pointer :: rho_ice
    real(fp)                , pointer :: rho_snow
    logical                 , pointer :: zmodel
    logical                 , pointer :: dyn_ice
    logical                 , pointer :: prs_ice
    logical                 , pointer :: his_ice
    real(fp)                , pointer :: timjan
    real(fp)                , pointer :: timhr
    real(fp)                , pointer :: tair
    real(fp) , dimension(:) , pointer :: tairarr
    logical                 , pointer :: tair_file
    real(fp)                , pointer :: drycrt
    integer                 , pointer :: itdate
    real(fp)                , pointer :: tzone

! Global variables
!
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: evap
real(fp)                                                , intent(in)  :: hdt
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: f_w
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: h_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: h_snow
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: precip
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: pship
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: dps
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: dpu
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: dpv
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: s1
integer                                                               :: icx
integer                                                               :: icy
integer, dimension(5, norow)                                          :: irocol
integer                                                               :: j
integer                                                               :: kmax
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfsice
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfssnw
integer, dimension(gdp%d%nmlb:gdp%d%nmub,0:kmax)                      :: kspu
integer, dimension(gdp%d%nmlb:gdp%d%nmub,0:kmax)                      :: kspv
integer                                                               :: lstsci
integer                                                               :: nmmax
integer                                                               :: nmmaxj
integer                                                               :: norow
integer                                                               :: nst
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kcs
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfs
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfu
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfv
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: t_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: t_snow
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: u_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: v_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: a_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: toth_i
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: toth_w
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: ut_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: vt_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub,5)                          :: sxice, sxsn, sxa ! contains field to be advected and 1st and 2nd moments for ice, snow and ice concentration
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: zs1
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: zs2
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: zs12
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: r1
!
! Local variables
!
    integer            :: ddb
    integer            :: ic
    integer            :: icxy
    integer            :: ii
    integer            :: k
    integer            :: k0
    integer            :: mf
    integer            :: ml
    integer            :: n
    integer            :: nm
    integer            :: nmd, nmu, ndm, num
    integer            :: nmf
    integer            :: nmfu
    integer            :: nml
    integer            :: nmlu
    real(fp)           :: coef_1, coef_2 
    real(fp)           :: conduc    ! conductivity
    real(fp)           :: flux_ib   ! flux in between snow and ice layer
    real(fp)           :: t_ib      ! temperature in between snow and ice
    real(fp)           :: t_freeze  ! freezing temperature
    character(20)      :: errtxt
    real(fp)           :: ice_growth, ice_melt, snow_melt, snow_growth
    real(fp)           :: h_old, snow_old
    real(fp)           :: time 
    real(fp)           :: treshold_ice_snow 
    real(fp)           :: h0
    logical            :: success
!
!! executable statements -------------------------------------------------------
!
    cp_ice      => gdp%gdice%cp_ice
    cp_snow     => gdp%gdice%cp_snow
    k_ice       => gdp%gdice%k_ice
    k_snow      => gdp%gdice%k_snow
    lh_ice      => gdp%gdice%lh_ice
    lh_snow     => gdp%gdice%lh_snow
    lundia      => gdp%gdinout%lundia
    rho_ice     => gdp%gdice%rho_ice
    rho_snow    => gdp%gdice%rho_snow
    dyn_ice     => gdp%gdice%dyn_ice
    prs_ice     => gdp%gdice%prs_ice
    his_ice     => gdp%gdice%his_ice
    timjan      => gdp%gdheat%timjan
    timhr       => gdp%gdinttim%timhr
    tair        => gdp%gdheat%tair
    tairarr     => gdp%gdheat%tairarr
    tair_file   => gdp%gdheat%tair_file
    itdate      => gdp%gdexttim%itdate
    tzone       => gdp%gdexttim%tzone
    ag          => gdp%gdphysco%ag
    rhow        => gdp%gdphysco%rhow
    drycrt      => gdp%gdnumeco%drycrt
    lsal        => gdp%d%lsal
    zmodel      => gdp%gdprocs%zmodel
    ! INITIALISATION
    !
    !! For testing of horizontal ice model only:
    !! tair = -1.23 !! negative value to ensure that DIF_ICE is carried out
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    ! Set threshold value for active/inactive ice and snow
    !
    treshold_ice_snow  = 1e-3_fp
    !
    ! Set freezing temperature (copy of code in HEATU.f90 to prevent extra array for T_FREEZE
    !
    t_freeze = 0.0_fp
    if (zmodel) then
       k0 = kmax  ! ToDO kfsmx0(nm)
    else
       k0 = 1
    endif
    if (lsal .ne. 0) then
       t_freeze = -0.0526_fp * r1(nm,k0,lsal)
    endif
    !
    ! update meteo input (if necessary)
    !
    if (tair_file) then
       time    = timhr * 60.0_fp
       success = meteoupdate(gdp%runid, itdate, tzone, time)
       call checkmeteoresult(success, gdp)
       !
       success = getmeteoval(gdp%runid, 'airtemp', time, gdp%gdparall%mfg, gdp%gdparall%nfg,&
                           & gdp%d%nlb, gdp%d%nub, gdp%d%mlb, gdp%d%mub, tairarr , 0 )
       call checkmeteoresult(success, gdp)
    endif
    !   
    do nm = 1, nmmax
      if (tair_file) tair = tairarr(nm)
      if (kfs(nm) == 1 .and. (tair .lt. t_freeze .or. kfsice(nm) == 1)  )  then
          nmd = nm - icx
          ndm = nm - icy
          nmu = nm + icx
          num = nm + icy
          !
          !! For testing of horizontal ice model only:
          !! toth_i(nm) = 0.0
          !! f_w(nm) = 0.0
          !
          ! compute snow fall
          !
          snow_old = h_snow(nm)
          if (kfsice(nm) == 1) then
             if (tair .lt. t_freeze) then
                snow_growth = hdt * (precip(nm) - max(0.0_fp,evap(nm)/rhow))
                h_snow(nm) = h_snow(nm) + hdt * (precip(nm) - max(0.0_fp,evap(nm)/rhow))
                h_snow(nm) = max (0.0_fp,h_snow(nm))
                if (kfssnw(nm) .eq. 0 .and. h_snow(nm) .gt. treshold_ice_snow) then
                    kfssnw(nm) = 1
                endif     
                if (h_snow(nm) .lt. treshold_ice_snow) then
                    kfssnw(nm) = 0
                endif
             endif
          endif
          !
          h_old = h_ice(nm)
          if (toth_i(nm) .gt. 0.0_fp) then
             if (kfssnw(nm) .eq. 0) then
                !
                ! melting of ice
                !
                ice_melt = hdt * ( 0.0_fp - toth_i(nm) ) / lh_ice
                h_ice(nm) = h_ice(nm) + hdt * ( -toth_i(nm) + f_w(nm) ) / lh_ice
             else
                !
                ! melting of snow
                !
                snow_melt  = hdt / lh_snow * ( 0.0_fp - toth_i(nm) )
                h_snow(nm) = h_snow(nm) + (hdt / lh_snow) * ( 0.0_fp - toth_i(nm) )
                if (h_snow(nm) .lt. treshold_ice_snow) then
                    kfssnw(nm) = 0
                endif
             endif
          else
             !
             ! Compute thickness of ice through ice growth:
             !
             ice_growth = (hdt /lh_ice) * ( -toth_i(nm) + f_w(nm) ) 
             h_ice(nm) = h_ice(nm) + ice_growth
             !
             ! Maximize ice growth to 10 m
             !
             h_ice(nm) = min(10.0_fp, h_ice(nm)) 
             !
             ! Check whether ice growth is sufficient to changing status arrays
             !
             if (kfsice(nm) .eq. 0 .and. h_ice(nm)*a_ice(nm) .gt. treshold_ice_snow) then
                 kfsice(nm) = 1
                 if (a_ice(nm) .lt. 1e-6_fp .or. ice_growth .gt. 1e-6_fp) then
                    a_ice(nm) = 1.0_fp !! only in case of new (vertical) ice
                 endif   
                 if (prs_ice) then
                     pship(nm) = 0.93_fp * h_ice(nm) * a_ice(nm) * ag * rhow
                 endif     
                 kspu(nm,0) = -2
                 kspu(nmd,0) = -2
                 kspv(nm,0) = -2
                 kspv(ndm,0) = -2
             endif  
          endif  
          !
          ! Check whether ice melt is sufficient to change status of ice arrays
          !
          if (kfsice(nm) .eq. 1 .and. h_ice(nm) .lt. treshold_ice_snow) then
              kfsice(nm) = 0
              h_ice(nm) = 0.0_fp
              if (dyn_ice) a_ice(nm) = 0.0_fp
              !
              t_ice(nm) = 0.0_fp
              kspu(nm,0) = 0
              kspu(nmd,0) = 0
              kspv(nm,0) = 0
              kspv(ndm,0) = 0
              ! remove snow in case of no ice
              h_snow(nm) = 0.0_fp
              kfssnw(nm) = 0
              t_snow(nm) = 0.0_fp
              ! set ice currents to zero
              u_ice(nm)  = 0.0_fp
              u_ice(nmd) = 0.0_fp
              v_ice(nm)  = 0.0_fp
              v_ice(ndm) = 0.0_fp
              ! set stress terms to zero
              zs1(nm) = 0.0_fp
              zs2(nm) = 0.0_fp
              zs12(nm) = 0.0_fp
              ! set 1st and 2nd moments to zero
              do ii =1,5
                 sxice(nm,ii) = 0.0_fp
                 sxsn(nm,ii)  = 0.0_fp
                 sxa(nm,ii)   = 0.0_fp
              enddo   
          endif
          !
          if (kfsice(nm) == 1 .and. kfssnw(nm) .eq. 1) then
             !
             ! Compute flux between ice and snow layer:
             !
             !   k_s (t_snow - t_ib) / Dx_snow = k_i (t_ib - t_freeze) / Dx_ice
             coef_1 = t_freeze * k_ice/h_ice(nm) + t_snow(nm) * k_snow/h_snow(nm)
             coef_2 = (k_ice/h_ice(nm) + k_snow/h_snow(nm) )
             t_ib = coef_1 / coef_2
             flux_ib = k_ice * (t_ib - t_freeze) / h_ice(nm)
          endif   
          ! 
          !
          ! Copy ice thickness to PSHIP and S1 arrays
          !
          if (prs_ice) then
             pship(nm) = 0.93_fp * a_ice(nm) * h_ice(nm) * ag * rhow
             s1(nm) = s1(nm) + 0.93_fp * a_ice(nm) * (h_old - h_ice(nm))
          endif   
          !
          ! Check for drying
          !
          if ( s1(nm) + real(dps(nm),fp) < drycrt ) then
             kfs(nm) = 0
             kfu(nm) = 0
             kfu(nmd) = 0
             kfv(nm) = 0
             kfv(ndm) = 0
             kfsice(nm) = 0
             !
             kspu(nm,0) = 0
             kspu(nmd,0) = 0
             kspv(nm,0) = 0
             kspv(ndm,0) = 0
             ! set ice currents to zero
             u_ice(nm)  = 0.0_fp
             u_ice(nmd) = 0.0_fp
             v_ice(nm)  = 0.0_fp
             v_ice(ndm) = 0.0_fp
             ! set stress terms to zero
             zs1(nm) = 0.0_fp
             zs2(nm) = 0.0_fp
             zs12(nm) = 0.0_fp
             ! set 1st and 2nd moments to zero
             do ii =1,5
                sxice(nm,ii) = 0.0_fp
                sxsn(nm,ii)  = 0.0_fp
                sxa(nm,ii)   = 0.0_fp
             enddo   
          endif
       endif
    enddo
	!
    ! Check for drying in velocity points
    !
    do nm = 1, nmmax
       if (kfu(nm) == 1 .and. kfsice(nm) == 1)  then
           nmu = nm + icx
           h0 = min(s1(nmu), s1(nm)) + dpu(nm)
           if (h0 < drycrt) then
              kfu(nm) = 0
              u_ice(nm) = 0.0_fp
           endif
       endif
       if (kfv(nm) == 1 .and. kfsice(nm) == 1)  then
           num = nm + icy
           h0 = min(s1(num), s1(nm)) + dpv(nm)
           if (h0 < drycrt) then
              kfv(nm) = 0
              v_ice(nm) = 0.0_fp
           endif
       endif

    enddo
    !
    ! Compute ice and snow thickness at boundaries
    !
    do ic = 1, norow
       n    = irocol(1, ic)
       mf   = irocol(2, ic) - 1
       ml   = irocol(3, ic)
       nmf  = (n + ddb)*icy + (mf + ddb)*icx - icxy
       nml  = (n + ddb)*icy + (ml + ddb)*icx - icxy
       nmfu = nmf + icx
       nmlu = nml + icx
       !
       if (kcs(nmf) == 1) then
          h_ice(nmf)  = h_ice(nmfu)
          h_snow(nmf) = h_snow(nmfu)
       endif
       if (kcs(nml) == 1) then
          h_ice(nmlu)  = h_ice(nml)
          h_snow(nmlu) = h_snow(nml)
       endif
       !
    enddo
    !
    ! Copy ice output to history file
    !
    if (his_ice) then   
       do nm = 1, nmmax
          if (kfs(nm) == 1 .and. kfsice(nm) == 1 ) then
             r1(nm,1,lstsci) = h_ice(nm)
             r1(nm,2,lstsci) = a_ice(nm)
             r1(nm,3,lstsci) = h_ice(nm) * a_ice(nm)
             r1(nm,4,lstsci) = h_snow(nm)
             !! r1(nm,0,lstsci) = t_ice(nm)
             !! r1(nm,0,lstsci) = t_snow(nm)
             r1(nm,5,lstsci) = toth_i(nm)
             r1(nm,6,lstsci) = f_w(nm)
             !
             t_freeze = 0.0_fp
             if (zmodel) then
                k0 = kmax  ! ToDO kfsmx0(nm)
             else
                k0 = 1
             endif
             !
             if (lsal .ne. 0) then
                t_freeze = -0.0526_fp * r1(nm,k0,lsal)
             endif
             r1(nm,7,lstsci) = t_freeze
             r1(nm,8,lstsci) = 1.0   !   representing KFSICE
             r1(nm,9,lstsci) = kfssnw(nm)
          else
             do k=1,kmax
                r1(nm,k,lstsci) = 0.0
             enddo
          endif
       enddo
    endif 
end subroutine dif_ice