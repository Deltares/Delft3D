subroutine d_ice(hdt      ,nst       ,icx       ,icy       , &
              & j         ,nmmaxj    ,nmmax     ,kmax      ,lstsci    , &
              & u1        ,v1        ,r1        ,norow     ,irocol    , &
              & kcs       ,kfu       ,kfv       ,kfs       , &
              & dps       ,dpu       ,dpv       ,anglat    ,w10mag    ,evap      , &
              & kspu      ,kspv      ,s1        ,pship     ,precip    , &
              & guu       ,gvv       ,gvu       ,guv       , &
              & gud       ,gvd       ,guz       ,gvz       , &
              & fcorio    ,gsqs      , &
              & h_ice     ,h_snow    ,t_ice     ,t_snow    , &
              & u_ice     ,v_ice     ,a_ice     ,kfsice    ,kfssnw    , &
              & toth_i    ,toth_w    ,f_w       ,ut_ice    ,vt_ice    , &
              & icestr    ,icknmi    ,sxice     ,sxsn      ,sxa       , &
              & uuu       ,vvv       ,uuu_ice   ,vvv_ice   , &
              & zpresh    ,zpreshc   ,zfrld1    ,zfrld2    , &
              & zmass1    ,zmass2    ,za1ct     ,za2ct     , &
              & zf1       ,zf2       ,zc1       ,guu2      ,gvv2      , &
              & gdp       )     
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
!  $Id: d_ice.f90 64423 2019-07-24 11:30:09Z goede $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20190705_ice_modelling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_ice/d_ice.f90 $
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
    use timers
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    character(10)           , pointer :: ice_model
    logical                 , pointer :: dyn_ice
    integer                 , pointer :: lundia
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
    real(fp)                , pointer :: timjan
    real(fp)                , pointer :: timhr
    real(fp)                , pointer :: tair
!
! Global variables
!
integer                                                               :: icx
integer                                                               :: icy
integer, dimension(5, norow)                                          :: irocol
integer                                                               :: j
integer                                                               :: kmax
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kcs
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfu
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfv
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfs
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfsice
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfssnw
integer, dimension(gdp%d%nmlb:gdp%d%nmub,0:kmax)                      :: kspu
integer, dimension(gdp%d%nmlb:gdp%d%nmub,0:kmax)                      :: kspv
integer                                                               :: lstsci
integer                                                               :: nmmax
integer                                                               :: nmmaxj
integer                                                               :: norow
integer                                                               :: nst
!
real(fp)                                                , intent(in)  :: anglat
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: evap
real(fp)                                                , intent(in)  :: hdt
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: fcorio
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gsqs
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: guu
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: guv
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: guz
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gud
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gvv
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gvu
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gvz
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gvd
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: f_w
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: h_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: h_snow
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: precip
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: pship
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: s1
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: dps
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: dpu
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: dpv
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: w10mag
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: r1
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: t_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: t_snow
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: u1
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: v1
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: u_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: v_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: a_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: ut_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: vt_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: toth_i
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: toth_w
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub,5)                          :: sxice, sxsn, sxa  !:  field to be advected and 1st and 2nd moments for ice, snow and ice concentration
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub,3)                          :: icestr            !:  ice stressses ZS!, ZS2 and ZS!@ in LIM3 
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub,3)                          :: icknmi            !:  ice stressses ZS!, ZS2 and ZS!@ in LIM3 
! work arrays:
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: uuu, vvv, uuu_ice, vvv_ice
real(fp), dimension(:),allocatable                            :: vol_ice, vol_snow 
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: zf1, zf2          !: arrays for internal stresses
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: guu2, gvv2

real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: zpresh  , &       !: temporary array for ice strength
                                                                         zpreshc , &       !: Ice strength on grid cell corners (zpreshc)
                                                                         zfrld1, zfrld2, & !: lead fraction on U/V points                                    
                                                                         zmass1, zmass2, & !: ice/snow mass on U/V points                                    
                                                                         za1ct, za2ct  , & !: temporary arrays
                                                                         zc1               !: ice mass
real(fp), dimension(:,:,:), pointer                                   :: wght
!
! Local variables
!
    integer            :: ddb
    integer            :: ic
    integer            :: icxy
    integer            :: istat
    integer            :: mf
    integer            :: ml
    integer            :: n
    integer            :: nm
    integer            :: nmin
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
    real(fp)           :: ice_growth, ice_melt
    real(fp)           :: h_old, snow_old
    real(fp)           :: tm 
    real(fp)           :: ice_factor 
!
!! executable statements -------------------------------------------------------
!
    dyn_ice     => gdp%gdice%dyn_ice
    ice_model   => gdp%gdice%ice_model
    cp_ice      => gdp%gdice%cp_ice
    cp_snow     => gdp%gdice%cp_snow
    k_ice       => gdp%gdice%k_ice
    k_snow      => gdp%gdice%k_snow
    lh_ice      => gdp%gdice%lh_ice
    lh_snow     => gdp%gdice%lh_snow
    lundia      => gdp%gdinout%lundia
    rho_ice     => gdp%gdice%rho_ice
    rho_snow    => gdp%gdice%rho_snow
    timjan      => gdp%gdheat%timjan
    timhr       => gdp%gdinttim%timhr
    tair        => gdp%gdheat%tair
    ag          => gdp%gdphysco%ag
    rhow        => gdp%gdphysco%rhow
    !
    !
    if (dyn_ice) then
       !
       ! Dynamic ice model
       !
       allocate (wght(j:nmmaxj,2,2), stat = istat)
       allocate (vol_ice(gdp%d%nmlb:gdp%d%nmub), stat = istat)
       allocate (vol_snow(gdp%d%nmlb:gdp%d%nmub), stat = istat)
       !
       call limrhg(nst    ,hdt       ,icx       ,icy       , &
              & nmmax     ,kmax      , &
              & gsqs      ,guu       ,gvu       ,gvv       ,guv       , &
              & guz       ,gvz       ,gud       ,gvd       , &
              & fcorio    ,norow     ,irocol    ,kcs       ,kfs       , &
              & kspu      ,kspv      ,s1        ,pship     ,precip    , &
              & kfsice    ,kfssnw    ,kfu       ,kfv       , &
              & u1        ,v1        ,u_ice     ,v_ice     ,a_ice     , &
              & h_ice     ,h_snow    ,t_ice     ,t_snow    ,wght      , &
              & icestr(j,1), icestr(j,2),      icestr(j,3) , &
              & ut_ice    ,vt_ice    , &
! work arrays:               
              & vol_ice   ,vol_snow  , &
              & uuu       ,vvv       ,uuu_ice   ,vvv_ice   , &
              & zpresh    ,zpreshc   ,zfrld1    ,zfrld2    , &
              & zmass1    ,zmass2    ,za1ct     ,za2ct     , &
              & zf1       ,zf2       ,zc1       , &     
              & gdp       )
       !
       call limtrp(        hdt       ,icx       ,icy       , &
              & j         ,nmmaxj    ,nmmax     , &
              & gsqs      ,guu       ,gvv       , &
              & norow     ,irocol    ,kcs       ,kfs       , &
              & u_ice     ,v_ice     ,h_ice     ,h_snow    ,a_ice     , &
              & kfsice    ,kfssnw    , &
              & sxice     ,sxsn      ,sxa       , &
! work arrays:               
              & vol_ice   ,vol_snow  ,uuu       , &  !! & zs0ice    ,zs0sn     ,zs0a      , &
              & vvv       ,uuu_ice   ,vvv_ice   , &  !! & vol_ice   ,vol_snow  ,zsm       , &
              & zpresh    ,zpreshc   ,zfrld1    ,zfrld2    , &
              & zmass1    ,zmass2    ,za1ct     ,za2ct     , &
              & zf1       ,zf2       ,zc1       ,guu2      ,gvv2      , &     
              & gdp       )
       !
       deallocate (wght, stat = istat)
       deallocate (vol_ice, stat = istat)
       deallocate (vol_snow, stat = istat)
       !
    endif
    !
    ! Thermodynamic model
    !
    if (ice_model == 'deltares') then
       !
       call dif_ice(   hdt       ,nst       ,icx       ,icy       , &
          & j         ,nmmaxj    ,nmmax     ,kmax      , &
          & norow     ,irocol    ,kcs       ,kfs       ,kfu       ,kfv       , &
          & kfsice    ,kfssnw    ,toth_i    ,toth_w    ,f_w       , &
          & u_ice     ,v_ice     ,a_ice     , &
          & h_ice     ,h_snow    ,t_ice     ,t_snow    ,evap      , &
          & kspu      ,kspv      ,s1        ,pship     ,precip    , &
          & dps       ,dpu       ,dpv       ,sxice     ,sxsn      ,sxa       , & 
          & icestr(j,1), icestr(j,2),      icestr(j,3) , &
          & ut_ice    ,vt_ice    , &
          & r1        ,lstsci    ,gdp       )
    else if (ice_model == 'knmi') then
       call knmi(      nst       ,icx       ,icy       , &
          & j         ,nmmaxj    ,nmmax     ,kmax      , &
          & norow     ,irocol    ,kcs       ,kfs       ,kfu       ,kfv       , &
          & kfsice    ,kfssnw    ,toth_i    ,toth_w    ,f_w       , &
          & u_ice     ,v_ice     ,a_ice     , &
          & h_ice     ,h_snow    ,t_ice     ,t_snow    ,evap      , &
          & kspu      ,kspv      ,s1        ,pship     ,precip    , &
          & dps       ,sxice     ,sxsn      ,sxa       ,icknmi    , & 
          & icestr(j,1), icestr(j,2),      icestr(j,3) , &
          & ut_ice    ,vt_ice    , &
          & anglat    ,w10mag    ,gdp       )
    endif
    !
end subroutine d_ice
