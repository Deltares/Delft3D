subroutine wrfous(nmax      ,mmax      ,nmaxus    ,kmax      ,lmax      , &
                & nofou     ,ifou      ,lunfou    ,dtsec     ,namcon    , &
                & kcs       ,xz        ,yz        ,xcor      ,ycor      , &
                & kfu       ,kfv       ,itdate    ,filename  ,filetype  , &
                & fougrp    ,iarrc     ,mf        ,ml        ,nf        , &
                & nl        ,gdp       )
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
!    Function: - writes results of fourier analysis to output
!                file lunfou for scalair quantities
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    use dfparall
    use dffunctionals
    use netcdf
    use datagroups
    use wrtarray, only: wrtarray_nm_2d
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                              , pointer :: mlb
    integer                              , pointer :: mub
    integer                              , pointer :: nlb
    integer                              , pointer :: nub
    integer                              , pointer :: nmaxgl
    integer                              , pointer :: mmaxgl
    integer        , dimension(:)        , pointer :: fconno
    integer        , dimension(:)        , pointer :: flayno
    integer        , dimension(:)        , pointer :: fnumcy
    integer        , dimension(:)        , pointer :: ftmsto
    integer        , dimension(:)        , pointer :: ftmstr
    integer        , dimension(:)        , pointer :: idvar
    integer                              , pointer :: iblwl
    integer                              , pointer :: ibleh
    integer                              , pointer :: iblcn
    real(fp)       , dimension(:)        , pointer :: fknfac
    real(fp)       , dimension(:)        , pointer :: foufas
    real(fp)       , dimension(:,:,:)    , pointer :: fousma
    real(fp)       , dimension(:,:,:)    , pointer :: fousmb
    real(fp)       , dimension(:)        , pointer :: fv0pu
    character(1)   , dimension(:)        , pointer :: fouelp
    character(16)  , dimension(:)        , pointer :: founam
    character(50)  , dimension(:)        , pointer :: fouvarnam
    real(fp)                             , pointer :: tzone
    real(fp)                             , pointer :: hdt
    integer                              , pointer :: nofouvar
    integer        , dimension(:,:)      , pointer :: fouref
    integer                              , pointer :: idfile
    integer                              , pointer :: ntstep
    integer                              , pointer :: lundia
!
! Global variables
!
    integer                                                                           , intent(in) :: filetype     !  File type
    integer                                                                           , intent(in) :: ifou         !!  Fourier counter
    integer                                                                                        :: itdate       !  Reference time in YYYYMMDD
    integer                                                                           , intent(in) :: kmax         !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in) :: lmax         !  Description and declaration in dimens.igs
    integer                                                                           , intent(in) :: lunfou       !!  Unit number fourier output file
    integer                                                                           , intent(in) :: mmax         !  Description and declaration in esm_alloc_int.f90
    integer                                                                                        :: nmax         !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in) :: nmaxus       !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in) :: nofou        !  Description and declaration in dimens.igs
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in) :: kcs          !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in) :: kfu          !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in) :: kfv          !  Description and declaration in esm_alloc_int.f90
    real(fp)                                                                          , intent(in) :: dtsec        !!  Integration time step [in seconds]
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in) :: xcor         !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in) :: xz           !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in) :: ycor         !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in) :: yz           !  Description and declaration in esm_alloc_real.f90
    character(20) , dimension(lmax)                                                   , intent(in) :: namcon       !  Description and declaration in esm_alloc_char.f90
    character(*)                                                                      , intent(in) :: filename     !  File name
    character(*)                                                                      , intent(in) :: fougrp       !  Group name
    !
    integer    , dimension(4,0:nproc-1)                                               , intent(in)  :: iarrc       ! array containing collected grid indices
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: mf          ! first index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: ml          ! last index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: nf          ! first index w.r.t. global grid in y-direction
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: nl          ! last index w.r.t. global grid in y-direction
!
! Local variables
!
    integer                                 :: ierror
    integer                                 :: fouvar
    integer                                 :: m            ! Loop counter over MMAX 
    integer                                 :: n            ! Loop counter over NMAXUS 
    integer                                 :: ncol         ! Number of column to write to TEKAL data file 
    integer , dimension(:,:,:), allocatable :: ibuff3
    integer , dimension(:,:,:), allocatable :: ibuff3gl
    logical                                 :: ltest        ! Help variable for atan2 function test 
    real(fp)                                :: amp          ! Fourier amplitude 
    real(fp)                                :: fas          ! Fourier phase 
    real(fp)                                :: freqnt       ! Frequency in degrees per hour 
    real(fp)                                :: shift        ! Phase shift 
    real(fp)                                :: tfasto       ! Stop time in minutes 
    real(fp)                                :: tfastr       ! Start time in minutes 
    real(fp), dimension(:,:,:), allocatable :: rbuff3
    real(fp), dimension(:,:,:), allocatable :: rbuff3gl
    real(sp)                                :: defaul       ! Default value 
    character(20)                           :: namfun       ! Local name for fourier function 
    character(4)                            :: blnm
!
!! executable statements -------------------------------------------------------
!
    mlb           => gdp%d%mlb
    mub           => gdp%d%mub
    nlb           => gdp%d%nlb
    nub           => gdp%d%nub
    mmaxgl        => gdp%gdparall%mmaxgl
    nmaxgl        => gdp%gdparall%nmaxgl
    fconno        => gdp%gdfourier%fconno
    flayno        => gdp%gdfourier%flayno
    fnumcy        => gdp%gdfourier%fnumcy
    iblwl         => gdp%gdfourier%iblwl
    ibleh         => gdp%gdfourier%ibleh
    iblcn         => gdp%gdfourier%iblcn
    ftmsto        => gdp%gdfourier%ftmsto
    ftmstr        => gdp%gdfourier%ftmstr
    idvar         => gdp%gdfourier%idvar
    fknfac        => gdp%gdfourier%fknfac
    foufas        => gdp%gdfourier%foufas
    fousma        => gdp%gdfourier%fousma
    fousmb        => gdp%gdfourier%fousmb
    fv0pu         => gdp%gdfourier%fv0pu
    fouelp        => gdp%gdfourier%fouelp
    founam        => gdp%gdfourier%founam
    fouvarnam     => gdp%gdfourier%fouvarnam
    tzone         => gdp%gdexttim%tzone
    hdt           => gdp%gdnumeco%hdt
    nofouvar      => gdp%gdfourier%nofouvar
    fouref        => gdp%gdfourier%fouref
    idfile        => gdp%gdfourier%idfile
    ntstep        => gdp%gdinttim%ntstep
    lundia        => gdp%gdinout%lundia
    !
    ! Initialize local variables
    !
    defaul = -999.0_sp
    !
    ! Frequention := 360 degree / period
    ! where period = [ (FTMSTO - FMTSTR) * DTSEC ] / [ FNUMCY * 3600 ]
    ! FOUFAS is defined in RDFOUR as
    ! FOUFAS =  2 * PI * FNUMCY / [(FTMSTO - FMTSTR) ]
    ! so FREQNT = FOUFAS * RADDEG * 3600 / DTSEC is OK
    !
    shift = ftmstr(ifou)*foufas(ifou)
    freqnt = foufas(ifou)*raddeg*3600.0_fp/dtsec
    tfastr = real(ftmstr(ifou),fp)*dtsec/60.0_fp
    tfasto = real(ftmsto(ifou),fp)*dtsec/60.0_fp
    !
    namfun = founam(ifou)
    if (founam(ifou)(:2)=='s1') then
       if (fouelp(ifou)=='e') then
          ibleh = ibleh + 1
          blnm = 'EH??'
          write (blnm(3:4), '(i2.2)') ibleh
          namfun = 'energy head'
       else
          iblwl = iblwl + 1
          blnm = 'WL??'
          write (blnm(3:4), '(i2.2)') iblwl
          namfun = 'water level'
       endif
    endif
    if (founam(ifou)(:2)=='r1') then
       iblcn = iblcn + 1
       blnm = 'CO??'
       write (blnm(3:4), '(i2.2)') iblcn
       namfun = namcon(fconno(ifou))
    endif
    !
    if (getfiletype(gdp, FILOUT_FOU) == FTYPE_NETCDF) then
       !
       ! Definition part is done in wrfou
       !
    else
       !
       ! Write information to "TEKAL" data file
       !
       if (inode == master) then
          write (lunfou, '(a,a16)') '* Results fourier analysis on: ', namfun
          !
          if (kmax>1) then
             write (lunfou, '(a,i3)') '* Layer number               : ', flayno(ifou)
          endif
          !
          write (lunfou, '(a,i0   )') '* Reference date in YYYYMMDD : ', itdate
          write (lunfou, '(a,f12.3)') '* Starttime fourier analysis : ', tfastr
          write (lunfou, '(a,f12.3)') '* Stoptime  fourier analysis : ', tfasto
          write (lunfou, '(a,i6   )') '* Number of cycles           : ', fnumcy(ifou)
          write (lunfou, '(a,f12.6)') '* Frequency [degrees/hour]   : ', freqnt
          !
          write (lunfou, '(a     )') '*'
          write (lunfou, '(a     )') '* Block definition:'
          !
          ! For GPP: description in columns 17 to 37
          !
          write (lunfou, '(a     )') '* column    1 : X-coor, zeta point'
          write (lunfou, '(a     )') '* column    2 : Y-coor, zeta point'
          write (lunfou, '(a     )') '* column    3 : X-coor, depth point'
          write (lunfou, '(a     )') '* column    4 : Y-coor, depth point'
          write (lunfou, '(a     )') '* column    5 : M-index '
          write (lunfou, '(a     )') '* column    6 : N-index '
          if (fouelp(ifou)=='x' .or. fouelp(ifou)=='e') then
             ncol = 10
             write (lunfou, '(a     )') '* column    7 : Maximum value'
             write (lunfou, '(a     )') '* column    8 : KCS'
             write (lunfou, '(a     )') '* column    9 : KFU'
             write (lunfou, '(a     )') '* column   10 : KFV'
          elseif (fouelp(ifou)=='i') then
             ncol = 10
             write (lunfou, '(a     )') '* column    7 : Minimum value'
             write (lunfou, '(a     )') '* column    8 : KCS'
             write (lunfou, '(a     )') '* column    9 : KFU'
             write (lunfou, '(a     )') '* column   10 : KFV'
          else
             ncol = 11
             write (lunfou, '(a     )') '* column    7 : Fourier amplitude'
             write (lunfou, '(a     )') '* column    8 : Fourier phase'
             write (lunfou, '(a     )') '* column    9 : KCS'
             write (lunfou, '(a     )') '* column   10 : KFU'
             write (lunfou, '(a     )') '* column   11 : KFV'
          endif
          !
          ! Write Block code and data to "TEKAL" data file
          ! Frequency is shown in GPP (20 characters total)
          !
          write (lunfou, '(a4,a5,f11.6)') blnm, ' freq', freqnt
          write (lunfou, '(4i8)') mmaxgl*nmaxgl, ncol, mmaxgl, nmaxgl
       endif
    endif
    !
    ! Write data for user defined dimensions, hence NMAXUS and MMAX
    ! First for Maximum or Minimum
    !
    if (fouelp(ifou)=='x' .or. fouelp(ifou)=='i' .or. fouelp(ifou)=='e') then
       if (getfiletype(gdp, FILOUT_FOU) == FTYPE_NETCDF) then
          allocate(rbuff3(nlb:nub,mlb:mub,1), stat = ierror)
          rbuff3 = defaul
          do n = nlb, nub
             do m = mlb, mub
                !
                ! Only write values unequal to initial min/max values (-/+1.0e+30)
                !
                if (kcs(n,m)==1 .and. comparereal(abs(fousma(n,m,ifou)),1.0e29_fp)==-1) then
                   rbuff3(n,m,1) = fousma(n,m,ifou)
                endif
             enddo
          enddo
          !
          fouvar = fouref(ifou,2)
          call wrtarray_nm_2d(idfile, filename, filetype, fougrp, 1, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rbuff3(:,:,1), trim(fouvarnam(fouvar)))
          if (ierror /= 0) goto 9999
          !
          if (fouelp(ifou)=='x' .and. founam(ifou)=='s1') then
             !
             ! Write max waterdepth too
             !
             rbuff3 = defaul
             do n = 1, nmaxus
                do m = 1, mmax
                   !
                   ! Only write values unequal to initial min/max values (-/+1.0e+30)
                   !
                   if (kcs(n,m)==1 .and. comparereal(abs(fousmb(n,m,ifou)),1.0e29_fp)==-1) then
                      rbuff3(n,m,1) = fousmb(n,m,ifou)
                   endif
                enddo
             enddo
             !
             fouvar = fouvar + 1
             call wrtarray_nm_2d(idfile, filename, filetype, fougrp, 1, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, rbuff3(:,:,1), trim(fouvarnam(fouvar)))
             if (ierror /= 0) goto 9999
          endif
          !
          deallocate(rbuff3, stat = ierror)
       else
          allocate(rbuff3(nlb:nub,mlb:mub,5), stat = ierror)
          allocate(ibuff3(nlb:nub,mlb:mub,3), stat = ierror)
          rbuff3 = defaul
          !
          do n = 1, nmaxus
             do m = 1, mmax
                !
                ! Test for active point
                ! when KCS (N,M) = 1 N > 1 and M > 1 per definition
                !
                if (kcs(n, m)==1) then
                   rbuff3(n, m, 1) = xz(n, m)
                   rbuff3(n, m, 2) = yz(n, m)
                   rbuff3(n, m, 3) = xcor(n, m)
                   rbuff3(n, m, 4) = ycor(n, m)
                   rbuff3(n, m, 5) = fousma(n, m, ifou)
                   !
                   ibuff3(n, m, 1) = kcs(n, m)
                   ibuff3(n, m, 2) = kfu(n, m)
                   ibuff3(n, m, 3) = kfv(n, m)
                else
                   fousma(n, m, ifou) = defaul
                   fousma(n, m, ifou + 1) = defaul
                   !
                   !rbuff3(n, m, 1) = defaul
                   !rbuff3(n, m, 2) = defaul
                   rbuff3(n, m, 3) = xcor(n, m)
                   rbuff3(n, m, 4) = ycor(n, m)
                   !rbuff3(n, m, 5) = defaul
                   !
                   ibuff3(n, m, 1) = 0 ! '0' instead of kcs, because TEKAL does not accept '2'
                   ibuff3(n, m, 2) = kfu(n, m)
                   ibuff3(n, m, 3) = kfv(n, m)
                endif
             enddo
          enddo
          !
          if (parll) then
             call dfgather(rbuff3, rbuff3gl, nf, nl, mf, ml, iarrc, gdp)
             call dfgather(ibuff3, ibuff3gl, nf, nl, mf, ml, iarrc, gdp)
          else 
             call dfgather_seq(rbuff3, rbuff3gl, 1-nlb, 1-mlb, nmaxgl, mmaxgl)
             call dfgather_seq(ibuff3, ibuff3gl, 1-nlb, 1-mlb, nmaxgl, mmaxgl)
          endif
          !
          if (inode == master) then
             do n = 1, nmaxgl
                do m = 1, mmaxgl
                   write (lunfou,'(4(f12.3,1x),2(i5,1x),  e14.6E3,1x ,3(i2,1x))')    &
                       & rbuff3gl(n, m, 1), rbuff3gl(n, m, 2), rbuff3gl(n, m, 3), rbuff3gl(n, m, 4), m, n,           &
                       & rbuff3gl(n, m, 5), ibuff3gl(n, m, 1), ibuff3gl(n, m, 2), ibuff3gl(n, m, 3)
                enddo
             enddo
          endif
          !
          deallocate(rbuff3, stat = ierror)
          deallocate(ibuff3, stat = ierror)
       endif
    else
       if (getfiletype(gdp, FILOUT_FOU) == FTYPE_NETCDF) then
          allocate(rbuff3(nlb:nub,mlb:mub,2), stat = ierror)
          rbuff3 = defaul
       else
          allocate(rbuff3(nlb:nub,mlb:mub,6), stat = ierror)
          allocate(ibuff3(nlb:nub,mlb:mub,3), stat = ierror)
          rbuff3 = defaul
       endif
       !
       ! Write data for user defined dimensions, hence NMAXUS and MMAX
       !
       do n = nlb, nub
          do m = mlb, mub
             ltest = (fousma(n, m, ifou)==0.0_fp .and. fousmb(n, m, ifou)==0.0_fp)
             !
             ! Test for active point and non-zero values
             ! when KCS (N,M) = 1 N > 1 and M > 1 per definition
             !
             if (kcs(n, m)==1 .and. .not.ltest) then
                fousma(n, m, ifou) = fousma(n, m, ifou)                         &
                                   & *2.0_fp/(real(ftmsto(ifou) - ftmstr(ifou),fp))
                fousmb(n, m, ifou) = fousmb(n, m, ifou)                         &
                                   & *2.0_fp/(real(ftmsto(ifou) - ftmstr(ifou),fp))
                amp = sqrt(fousma(n, m, ifou)*fousma(n, m, ifou)                &
                    & + fousmb(n, m, ifou)*fousmb(n, m, ifou))
                fas = atan2(fousmb(n, m, ifou), fousma(n, m, ifou)) + shift
                if (fnumcy(ifou)==0) then
                   amp = 0.5_fp*amp*cos(fas)
                   fas = 0.0_fp
                endif
                !
                ! Timezone correction added timezone*phase [degrees/hr].
                ! foufas       is in [rad/timestep]
                ! halftimestep is in [sec/timestep]
                ! => timezonecorr = tzone [-] * foufas [rad/timestep] * raddeg [deg/rad] * [sec/hr] / (2 * halftimestep [sec/timestep])
                !
                fas = fas*raddeg + fv0pu(ifou) - tzone*foufas(ifou)*raddeg*1800.0_fp/hdt
                !
                ! To define FAS between 0 and 360. add 720. to the MOD of
                ! FAS and re-use the MOD function
                !
                fas = mod(mod(fas, 360.0_fp) + 720.0_fp, 360.0_fp)
                amp = amp/fknfac(ifou)
                if (getfiletype(gdp, FILOUT_FOU) == FTYPE_NETCDF) then
                   rbuff3(n,m,1) = amp
                   rbuff3(n,m,2) = fas
                else
                   rbuff3(n,m,1) = xz(n, m)
                   rbuff3(n,m,2) = yz(n, m)
                   rbuff3(n,m,3) = xcor(n, m)
                   rbuff3(n,m,4) = ycor(n, m)
                   rbuff3(n,m,5) = amp
                   rbuff3(n,m,6) = fas
                   !
                   ibuff3(n,m,1) = kcs(n, m)
                   ibuff3(n,m,2) = kfu(n, m)
                   ibuff3(n,m,3) = kfv(n, m)
                endif
             else
                !
                ! Inactive point (not inside grid, can be open boundary)
                ! defaul instead of xz/yz needed for GPP
                !
                if (getfiletype(gdp, FILOUT_FOU) == FTYPE_NETCDF) then
                   !rbuff3(n,m,1) = defaul
                   !rbuff3(n,m,2) = defaul
                else
                   !rbuff3(n,m,1) = defaul
                   !rbuff3(n,m,2) = defaul
                   rbuff3(n,m,3) = xcor(n, m)
                   rbuff3(n,m,4) = ycor(n, m)
                   !rbuff3(n,m,5) = defaul
                   !rbuff3(n,m,6) = defaul
                   !
                   ibuff3(n,m,1) = 0 ! '0' instead of kcs, because TEKAL does not accept '2'
                   ibuff3(n,m,2) = kfu(n, m)
                   ibuff3(n,m,3) = kfv(n, m)
                endif
             endif
          enddo
       enddo
       if (getfiletype(gdp, FILOUT_FOU) == FTYPE_NETCDF) then
          fouvar = fouref(ifou,2)
          call wrtarray_nm_2d(idfile, filename, filetype, fougrp, 1, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rbuff3(:,:,1), trim(fouvarnam(fouvar)))
          if (ierror /= 0) goto 9999
          !
          fouvar = fouvar + 1
          call wrtarray_nm_2d(idfile, filename, filetype, fougrp, 1, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rbuff3(:,:,2), trim(fouvarnam(fouvar)))
          if (ierror /= 0) goto 9999
          !
          deallocate(rbuff3, stat = ierror)
       else
          !
          if (parll) then
             call dfgather(rbuff3, rbuff3gl, nf, nl, mf, ml, iarrc, gdp)
             call dfgather(ibuff3, ibuff3gl, nf, nl, mf, ml, iarrc, gdp)
          else 
             call dfgather_seq(rbuff3, rbuff3gl, 1-nlb, 1-mlb, nmaxgl, mmaxgl)
             call dfgather_seq(ibuff3, ibuff3gl, 1-nlb, 1-mlb, nmaxgl, mmaxgl)
          endif
          !
          if (inode == master) then
             do n = 1, nmaxgl
                do m = 1, mmaxgl
                   write (lunfou,'(4(f12.3,1x),2(i5,1x),2(e14.6E3,1x),3(i2,1x))') &
                       & rbuff3gl(n, m, 1), rbuff3gl(n, m, 2), rbuff3gl(n, m, 3), rbuff3gl(n, m, 4), m, n, &
                       & rbuff3gl(n, m, 5), rbuff3gl(n, m, 6), &
                       & ibuff3gl(n, m, 1), ibuff3gl(n, m, 2), ibuff3gl(n, m, 3)
                enddo
             enddo
          endif
          !
          deallocate(rbuff3, stat = ierror)
          deallocate(ibuff3, stat = ierror)
       endif
    endif
9999 continue
end subroutine wrfous
