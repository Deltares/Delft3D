subroutine knmi(           nst       ,icx       ,icy       , &
              & j         ,nmmaxj    ,nmmax     ,kmax      , &
              & norow     ,irocol    ,kcs       ,kfs       ,kfu       ,kfv       , &
              & kfsice    ,kfssnw    ,toth_i    ,toth_w    ,f_w       , &
              & u_ice     ,v_ice     ,a_ice     , &
              & h_ice     ,h_snow    ,t_ice     ,t_snow    ,evap      , &
              & kspu      ,kspv      ,s1        ,pship     ,precip    , &
              & dps       ,sxice     ,sxsn      ,sxa       ,icknmi    , & 
              & zs1       ,zs2       ,zs12      ,ut_ice    ,vt_ice    , &
              & anglat    ,w10mag    ,gdp       )
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
!  $Id: knmi.f90 64423 2019-07-24 11:30:09Z goede $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20190705_ice_modelling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_ice/knmi.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes thickness of ice and snow
!              It uses the (open source) KNMI code from H.A.R. de Bruin
! Method used: It follows  De Bruin & Wessels (1975), which is
!              published in J. Appl Meteorology, Vol. 27
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
    integer                 , pointer :: lundia
    real(fp)                , pointer :: ag
    real(fp)                , pointer :: cfclou
    real(fp)                , pointer :: rhow
    real(fp)                , pointer :: rhum
    real(fp)                , pointer :: drycrt
    real(fp)                , pointer :: sboltz
    real(fp)                , pointer :: tair
    real(fp)                , pointer :: timhr
    real(fp)                , pointer :: timjan
    real(fp) , dimension(:) , pointer :: rhumarr
    real(fp) , dimension(:) , pointer :: tairarr
    real(fp) , dimension(:) , pointer :: clouarr
    logical                 , pointer :: clou_file
    logical                 , pointer :: rhum_file
    logical                 , pointer :: tair_file
!
! Global variables
!
real(fp)                                                , intent(in)  :: anglat
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: w10mag
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: evap
!! real(fp)                                                , intent(in)  :: hdt
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: f_w
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: h_ice
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: h_snow
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: precip
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: pship
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: dps
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
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub,3)                          :: icknmi 
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub,5)                          :: sxice, sxsn, sxa ! contains field to be advected and 1st and 2nd moments for ice, snow and ice concentration
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: zs1
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: zs2
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: zs12
integer, dimension(15                    )                            :: msnage
real(fp), dimension(15                   )                            :: snwat
!
! Local variables
!
    integer            :: ddb
    integer            :: ic
    integer            :: ii
    integer            :: icxy
    integer            :: mf
    integer            :: ml
    integer            :: n
    integer            :: nm
    integer            :: nmd
    integer            :: nmu
    integer            :: ndm
    integer            :: num
    integer            :: nmf
    integer            :: nmfu
    integer            :: nml
    integer            :: nmlu
    integer            :: nmin
    real(fp)           :: depth
    real(fp)           :: flux_ib   ! flux in between snow and ice layer
    real(fp)           :: t_ib      ! temperature in between snow and ice
    real(fp)           :: t_freeze  ! freezing temperature
    real(fp)           :: tm        ! Actual time in hours after midnight January first (TIMJAN + TIMHR )
    real(fp)           :: tsi       ! surface (ice or snow) temperature
    real(fp)           :: h_old
    real(fp)           :: ice_factor
    character(20)      :: errtxt
!
    data snwat, msnage / 15*0.0_fp, 15*0/
    save snwat, msnage
!     
!! executable statements -------------------------------------------------------
!
    lundia      => gdp%gdinout%lundia
    timjan      => gdp%gdheat%timjan
    timhr       => gdp%gdinttim%timhr
    tair        => gdp%gdheat%tair
    rhum        => gdp%gdheat%rhum
    cfclou      => gdp%gdheat%cfclou
    sboltz      => gdp%gdphysco%sboltz
    rhumarr     => gdp%gdheat%rhumarr
    tairarr     => gdp%gdheat%tairarr
    clouarr     => gdp%gdheat%clouarr
    rhum_file   => gdp%gdheat%rhum_file
    tair_file   => gdp%gdheat%tair_file
    clou_file   => gdp%gdheat%clou_file
    ag          => gdp%gdphysco%ag
    rhow        => gdp%gdphysco%rhow
    drycrt      => gdp%gdnumeco%drycrt
    !
    ! INITIALISATION
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    tm = timjan + timhr
    nmin = nint(tm*60)
    !
    if ( mod(nmin,720) .eq. 0 ) then
       ! write (lundia,*) 'tijd=', tm,nmin,nst
       !
       ! KNMI model uses a time step of 12 hrs
       !
       do nm = 1, nmmax
          if (kfs(nm) == 1) then
             nmd = nm - icx
             ndm = nm - icy
             nmu = nm + icx
             num = nm + icy
             !
             if (rhum_file) then
                rhum = rhumarr(nm)
             endif
             if (tair_file) then
                tair = tairarr(nm)
             endif
             !
             ! Cloudiness in file is specified in percentages
             !
             if (clou_file) then
                cfclou = clouarr(nm)
             endif
             !
             ! =======================================================
             ! |   ON INPUT THE SUBSTANCE ICE SHOULD CONTAIN:        |
             ! |      IN ICKNMI(.,1): TEMPERATURE (TEMPW in IJS.F90) | 
             ! |      IN ICKNMI(.,2): TN, WITH A VALUE OF 0.6        |
             ! |      IN ICKNMI(.,3): EXCH0, WITH A VALUE OF 4.0     | 
             ! =======================================================
             !
             ! Printing for ice model
             !
             if (nm.eq.41) then
                write (lundia,'(a,i5,8f10.5)') 'KNMI ice before:', nst,icknmi(nm,1),icknmi(nm,2),icknmi(nm,3),h_ice(nm),h_snow(nm)
             endif    
             depth = real(dps(nm),fp) + s1(nm)
             call knmi_ice (lundia ,tm   ,w10mag(nm),cfclou   ,sboltz   ,  &
                     & anglat ,tair      ,rhum      ,precip(nm),  &
                     & h_ice(nm), h_snow(nm) ,icknmi(nm,1)   ,depth    , &
                     & icknmi(nm,2)      ,icknmi(nm,3)       ,nm       , &
                     & snwat             ,msnage    )
            if (nm.eq.41) then
               write (lundia,'(a,i5,8f10.5)') 'KNMI ice after:', nst,icknmi(nm,1),icknmi(nm,2),icknmi(nm,3),h_ice(nm),h_snow(nm)
            endif    
            !
             if (h_ice(nm) .gt. 0.0001) then
                kfsice(nm) = 1
                kspu(nm,0) = -2
                kspu(nmd,0) = -2
                kspv(nm,0) = -2
                kspv(ndm,0) = -2
             else
                kfsice(nm) = 0
                kspu(nm,0) = 0
                kspu(nm-icx,0) = 0
                kspv(nm,0) = 0
                kspv(nm-icy,0) = 0
             endif
             !
             if (h_snow(nm) .gt. 0.0001) then
                 kfssnw(nm) = 1
             else
                 kfssnw(nm) = 0
             endif
             !
             ! Copy ice thickness to PSHIP and S1 arrays
             !
             pship(nm) = 0.93_fp * a_ice(nm) * h_ice(nm) * ag * rhow
             s1(nm) = s1(nm) + a_ice(nm) * (h_old - h_ice(nm))
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
             ! if (nm .eq. 41) then
             !    write (lundia,*) 'pship:',h_ice(nm),- h_ice(nm) + h_old 
             ! endif
          endif
       enddo
    endif
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
end subroutine knmi
