subroutine z_inizm(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                 & icy       ,error     ,kcu       ,kcv       ,kcs       , &
                 & kfu       ,kfv       ,kfs       ,kfsz1     ,kfuz1     , &
                 & kfvz1     ,kfsmin    ,kfsmax    ,kfumin    ,kfumax    , &
                 & kfvmin    ,kfvmax    ,kspu      ,kspv      ,kcshyd    , &
                 & dps       ,dpu       ,dpv       ,s1        ,thick     , &
                 & hu        ,hv        ,dzu1      ,dzu0      ,dzv1      , &
                 & dzv0      ,dzs1      ,dzs0      ,zk        ,r1        , &
                 & lstsci    ,gsqs      ,qzk       ,umean     ,vmean     , &
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
!    Function: - Initiates the mask arrays for Fixed Layer Model
! Method used: Fixed Layer Approach
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                 , pointer :: lundia
    real(fp)                , pointer :: dryflc
    real(fp)                , pointer :: depini
    real(fp)                , pointer :: hdt
    integer                 , pointer :: m1_nhy
    integer                 , pointer :: m2_nhy
    integer                 , pointer :: n1_nhy
    integer                 , pointer :: n2_nhy
    logical                 , pointer :: nonhyd
    logical                 , pointer :: kfuv_from_restart
    real(fp)                , pointer :: dzmin
    real(fp)                , pointer :: zbot
    real(fp)                , pointer :: ztop
    integer  , dimension(:) , pointer :: modify_dzsuv
    logical                 , pointer :: ztbml
    logical                 , pointer :: ztbml_upd_r1
!
! Global variables
!
    integer                                          , intent(in)   :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                              !!  then computation proceeds in the X-
                                                                              !!  dir. If icx=1 then computation pro-
                                                                              !!  ceeds in the Y-dir.
    integer                                          , intent(in)   :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                         :: j      !!  Begin pointer for arrays which have
                                                                              !!  been transformed into 1D arrays.
                                                                              !!  Due to the shift in the 2nd (M-)
                                                                              !!  index, J = -2*NMAX + 1
    integer                                                         :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                          , intent(in)   :: nmmax  !  Description and declaration in dimens.igs
    integer                                                         :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)   :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)   :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)   :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kcshyd !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfumax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfvmax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfvmin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)   :: kspu   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)   :: kspv   !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(out)  :: kfsz1  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(out)  :: kfuz1  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(out)  :: kfvz1  !  Description and declaration in esm_alloc_int.f90
    logical                                          , intent(out)  :: error
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dpv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(out) :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(out) :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: vmean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(out) :: dzs0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(out) :: dzu0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: dzu1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(out) :: dzv0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: dzv1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)        :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: qzk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                         , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                                     :: zk
!
! Local variables
!
    integer        :: ddb
    integer        :: icxy       ! Help var.
    integer        :: k          ! Help var. 
    integer        :: m
    integer        :: n
    integer        :: nm         ! Help var. loops 1,nmmax and j,nmmaxj 
    integer        :: nmref
    integer        :: nmu
    integer        :: num
    integer        :: nm_pos     ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
    real(fp)       :: dpsmax
    real(fp)       :: dpsmin
    real(fp)       :: gridheight
    real(fp)       :: s1u
    real(fp)       :: s1v
    real(fp)       :: s1max
    character(300) :: errmsg
!
!! executable statements -------------------------------------------------------
!
    lundia             => gdp%gdinout%lundia
    dryflc             => gdp%gdnumeco%dryflc
    depini             => gdp%gdnumeco%depini
    hdt                => gdp%gdnumeco%hdt
    m1_nhy             => gdp%gdnonhyd%m1_nhy
    m2_nhy             => gdp%gdnonhyd%m2_nhy
    n1_nhy             => gdp%gdnonhyd%n1_nhy
    n2_nhy             => gdp%gdnonhyd%n2_nhy
    nonhyd             => gdp%gdprocs%nonhyd
    kfuv_from_restart  => gdp%gdrestart%kfuv_from_restart
    dzmin              => gdp%gdzmodel%dzmin
    zbot               => gdp%gdzmodel%zbot
    ztop               => gdp%gdzmodel%ztop
    modify_dzsuv       => gdp%gdzmodel%modify_dzsuv
    ztbml              => gdp%gdzmodel%ztbml
    ztbml_upd_r1       => gdp%gdzmodel%ztbml_upd_r1
    !
    ddb     = gdp%d%ddbound
    dzmin   = 0.1_fp*dryflc
    dpsmax  = -1.0e+32_fp
    dpsmin  =  1.0e+32_fp
    icxy    = max(icx, icy)
    nm_pos  = 1
    !
    ! initialize global arrays
    ! ASSUMED: ALL ARRAYS HAVE BEEN SET TO ZERO
    !
    kfumin =  1
    kfvmin =  1
    kfsmin =  1
    kfumax = -1
    kfvmax = -1
    kfsmax = -1
    !
    do nm = 1, nmmax
       kfs(nm) = min(1, kcs(nm))
       if (kfuv_from_restart) then
          if (kcu(nm)==0) kfu(nm) = 0
          if (kcv(nm)==0) kfv(nm) = 0
       else
          kfu(nm) = min(1, kcu(nm))
          kfv(nm) = min(1, kcv(nm))
       endif
       if (kcs(nm) == 1) then
          dpsmax = max(dpsmax, real(dps(nm),fp))
          dpsmin = min(dpsmin, real(dps(nm),fp))
       endif
    enddo
    !
    ! zbot should be lower then maximum depth (zbot<-dpsmax => dpsmax+zbot<0)
    ! otherwise an error is given
    !
    if (dpsmax+zbot > 0.0_fp) then
       write (errmsg, '(a,g10.3,a)') 'Depth value ', dpsmax, &
             & ' (m) exceeds ZBOT specified in input; change ZBOT to this value'
       call prterr(lundia, 'P004', trim(errmsg))
       error = .true.
       goto 9999
    endif
    !
    ! ztop should be higher then minimum depth (ztop>-dpsmin => dpsmin+ztop>0)
    ! otherwise an error is given and a suggestion to change these cells to permanently dry points
    !
    if (dpsmin < -ztop) then
       write (errmsg, '(a      )' ) 'One or more depth values exceed ZTOP specified in input'
       call prterr(lundia, 'P004', trim(errmsg))
       write (lundia, '(a      )' ) 'Options:'
       write (lundia, '(a,g10.3)' ) '1: Change ZTOP to above the heighest value: ', -dpsmin
       write (lundia, '(a      )' ) '2: Copy the following lines into the .dry file to make these points permanently dry:' 
       do nm = 1, nmmax
          if (kcs(nm) == 1) then
             if (real(dps(nm),fp) < -ztop) then
                call nm_to_n_and_m(nm, n, m, gdp)
                write(lundia, '(4(2x,i0))') m, n, m, n
             endif
          endif
       enddo
       write (lundia, '(a      )' ) 'End of data-block for dry points file.' 
       error = .true.
       goto 9999       
    endif
    !
    ! initialization of DZS1(0,k), DZU1(0,k) and DZV1(0,k):
    ! 1 (vertical) area to be devided: ABS(ztop - zbot)
    ! 2 distribution by using thick
    ! 3 be sure that the top layer (surface layer = kmax!) is big enough
    !   to contain all realistic water levels by adding gridheight
    ! NOTE: NMREF = 0 positions of the arrrays are ABUSED/MISUSED
    !
    nmref = 0
    gridheight = abs(ztop - zbot)
    do k = 1, kmax
       dzs1(nmref, k) = gridheight * thick(k)
       dzu1(nmref, k) = dzs1(nmref, k)
       dzv1(nmref, k) = dzs1(nmref, k)
    enddo
    !
    ! the zk array is filled with the layer thicknesses. It is no longer (august 2007) 
    ! guaranteed that zk(kmax) is above s1. This is however checked below and also in 
    ! Z_DRYCHK. There, the top layer is updated each time level, to accomodate with the
    ! water level. This can lead to a large top layer thickness and to irregular vertical
    ! grid spacing.
    !
    zk(0) = zbot
    do k = 1, kmax
       zk(k) = zk(k - 1) + dzs1(nmref, k)
    enddo
    s1max = -999.0_fp
    do nm = 1, nmmax
        if (kcs(nm) == 1) then
           s1(nm) = max(s1(nm), - real(dps(nm),fp))
           !
           ! Minimum water depth equal to dzmin
           !
           if ((s1(nm) + real(dps(nm),fp)) < depini) then
              s1(nm) = -dps(nm) + depini
           endif
           s1max = max(s1max, s1(nm))
        endif
    enddo
    !
    ! exchange s1 with neighbours
    !
    call dfexchg ( s1, 1, 1, dfloat, nm_pos, gdp )
    !
    ! ztop should be higher than maximum water level 's1max'
    ! otherwise a warning is given
    !
    if (s1max > (ztop + 0.5 * dzs1(nmref, kmax))) then
        write (errmsg, '(a,g10.3,2a)') 'Maximum water level ', s1max, &
              & ' (m); Top layer is too thick. Changing ZTOP', &
              & ' is strongly advised'
        call prterr(lundia, 'U190', trim(errmsg))
    endif
    !
    ! Set the mask
    ! arrays KFU,KFV and KFS
    ! -icx := -1 in m-direction, -icy := -1 in n-direction
    ! Determine the initial number of layers and layer thickness
    ! assuming the maximum waterlevel at a velocity point (dry)
    !
    do nm = 1, nmmax
       if (kcu(nm) /= 0) then
          nmu    = nm + icx
          s1u    = max(s1(nm), s1(nmu))
          hu(nm) = s1u + dpu(nm)
          call z_kfmnmx(j         ,kmax      ,nm         ,nmref     , &
                      & dpu(nm)   ,dzmin     ,s1u       ,kfumin     ,kfumax    , &
                      & kfu       ,zk        ,dzu1      ,gdp       )
       endif
    enddo
    do nm = 1, nmmax
       if (kcv(nm) /= 0) then
          num = nm + icy
          s1v = max(s1(nm), s1(num))
          hv(nm) = s1v + dpv(nm)
          call z_kfmnmx(j         ,kmax      ,nm         ,nmref     , &
                      & dpv(nm)   ,dzmin     ,s1v       ,kfvmin     ,kfvmax    , &
                      & kfv       ,zk        ,dzv1      ,gdp       )
       endif
    enddo
    do nm = 1, nmmax
       if (kcs(nm) /= 0) then
          call z_kfmnmx(j         ,kmax      ,nm         ,nmref     , &
                      & real(dps(nm),fp) ,dzmin     ,s1(nm)    ,kfsmin    ,kfsmax    , &
                      & kfs       ,zk        ,dzs1      ,gdp       )
       endif
    enddo
    !
    ! If requested by keyword ZTBML 
    ! (Z-model TauBottom Modified Layering)
    ! --> modify the near-bed layering to obtain smoother bottom shear stress representation in z-layer models
    !
    if (ztbml) then
       !
       ! Call with modify_dzsuv set to 1 for all 3 components, to modify both dzs1, dzu1 and dzv1
       ! (and possibly R1 and qzk)
       !
       modify_dzsuv(:) = 1
       call z_taubotmodifylayers(nmmax   ,kmax     ,lstsci       ,icx      ,icy          , & 
                               & kfs     ,kfsmin   ,kfsmax       ,dps      ,dzs1         , &
                               & kfu     ,kfumin   ,kfumax       ,dpu      ,dzu1         , &
                               & kfv     ,kfvmin   ,kfvmax       ,dpv      ,dzv1         , &
                               & r1      ,s1       ,s1           ,zk       ,modify_dzsuv , &
                               & hdt     ,gsqs     ,kfsmax       ,qzk      ,umean        , &
                               & vmean   ,dzs0     ,ztbml_upd_r1 ,gdp      )
    endif
    !
    do nm = 1, nmmax
       do k = 1, kmax
          dzu0(nm, k) = dzu1(nm, k)
          dzs0(nm, k) = dzs1(nm, k)
          dzv0(nm, k) = dzv1(nm, k)
          if (k>=kfumin(nm) .and. k<=kfumax(nm) .and. kfu(nm)==1) then
             kfuz1(nm, k) = 1
          else
             kfuz1(nm, k) = 0
          endif
          if (k>=kfvmin(nm) .and. k<=kfvmax(nm) .and. kfv(nm)==1) then
             kfvz1(nm, k) = 1
          else
             kfvz1(nm, k) = 0
          endif
          if (k>=kfsmin(nm) .and. k<=kfsmax(nm) .and. kfs(nm)==1) then
             kfsz1(nm, k) = 1
          else
             kfsz1(nm, k) = 0
          endif
          !
          ! overwrite KFUZ1 at points with gates
          !
          if (kspu(nm, 0)*kspu(nm, k)==4 .or. kspu(nm, 0)*kspu(nm, k)==10) then
             kfuz1(nm, k) = 0
          endif
          if (kspv(nm, 0)*kspv(nm, k)==4 .or. kspv(nm, 0)*kspv(nm, k)==10) then
             kfvz1(nm, k) = 0
          endif
       enddo
    enddo
    if (nonhyd) then
       !
       ! define points for non hydrostatic computation
       !
       do n = n1_nhy, n2_nhy
          do m = m1_nhy, m2_nhy
             nm = (n + ddb)*icy + (m + ddb)*icx - icxy
             !
             ! for all non hydrostatic points:
             !
             if (kcs(nm) == 1) then
                kcshyd(nm) = 1
             endif
          enddo
       enddo
       do nm = 1, nmmax
          !
          ! leave out points at or near coupling points
          !
          if (kcs(nm) == 3) then
             kcshyd(nm + icx) = 0
             kcshyd(nm - icx) = 0
             kcshyd(nm + icy) = 0
             kcshyd(nm - icy) = 0
          endif
       enddo
    endif
9999 continue
    call dfexchg ( kfuz1 , 1, kmax, dfint , nm_pos, gdp )
    call dfexchg ( kfvz1 , 1, kmax, dfint , nm_pos, gdp )
    call dfexchg ( kfsz1 , 1, kmax, dfint , nm_pos, gdp )
end subroutine z_inizm
