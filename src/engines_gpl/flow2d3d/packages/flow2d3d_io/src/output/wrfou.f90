subroutine wrfou(nmax      ,mmax      ,nmaxus    ,kmax      ,lmax      , &
               & nofou     ,runid     ,trifil    ,dtsec     ,versio    ,namcon    , &
               & kcs       ,xz        ,yz        ,alfas     ,xcor      , &
               & ycor      ,kfu       ,kfv       ,itdate    ,gdp       )
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
!    Function: - open fourier analysis output file
!              - writes results of fourier analysis to output
!                file
!              - closes fourier analysis output file
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    use dfparall
    use netcdf
    use datagroups
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                              , pointer :: nofouvar
    integer        , dimension(:)        , pointer :: fconno
    character(1)   , dimension(:)        , pointer :: foutyp
    character(1)   , dimension(:)        , pointer :: fouelp
    character(16)  , dimension(:)        , pointer :: founam
    character(50)  , dimension(:)        , pointer :: fouvarnam
    character(50)  , dimension(:)        , pointer :: fouvarnamlong
    character(50)  , dimension(:)        , pointer :: fouvarunit
    integer        , dimension(:,:)      , pointer :: fouref
    integer        , dimension(:)        , pointer :: ftmsto
    integer        , dimension(:)        , pointer :: ftmstr
    real(fp)       , dimension(:)        , pointer :: foufas
    integer        , dimension(:)        , pointer :: flayno
    integer        , dimension(:)        , pointer :: fnumcy
    integer                              , pointer :: iblwl
    integer                              , pointer :: ibleh
    integer                              , pointer :: iblcn
    integer                              , pointer :: ibluv
    integer                              , pointer :: iblqf
    integer                              , pointer :: iblbs
    integer                              , pointer :: iblep
    integer                              , pointer :: idfile
    integer        , dimension(:)        , pointer :: idvar
    integer                              , pointer :: ntstep
    integer                              , pointer :: lundia
    integer                              , pointer :: nmaxgl
    integer                              , pointer :: mmaxgl
    integer       , dimension(:,:)       , pointer :: iarrc
    integer       , dimension(:)         , pointer :: mf
    integer       , dimension(:)         , pointer :: ml
    integer       , dimension(:)         , pointer :: nf
    integer       , dimension(:)         , pointer :: nl
    !
    integer                              , pointer :: io_prec
    logical                              , pointer :: mergemap
!
! Global variables
!
    integer                                                                          :: itdate !  Reference time in YYYYMMDD
    integer                                                                          :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                          :: lmax   !  Description and declaration in dimens.igs
    integer                                                                          :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                          :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                          :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                                          :: nofou  !  Description and declaration in dimens.igs
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(fp)                                                                         :: dtsec  !!  Integration time step [in seconds]
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: alfas  !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: xz     !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: ycor   !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              :: yz     !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                     :: runid  !!  Run identification of this simulation
    character(*)                                                       , intent(in)  :: trifil !  File name for FLOW NEFIS output files (tri"h/m"-"casl""labl".dat/def)
    character(20) , dimension(lmax)                                                  :: namcon !  Description and declaration in esm_alloc_char.f90
    character(5)                                                       , intent(in)  :: versio !!  Version nr. of the current package
!
! Local variables
!
    integer                  :: filetype
    integer                  :: iddim_n
    integer                  :: iddim_m
    !
    integer, dimension(6)    :: idatt_fou
    !
    integer                                           :: irequest
    integer                                           :: ierror
    integer                                           :: ifou         ! Local teller for fourier functions 
    integer                                           :: ivar         ! Local teller for fourier functions 
    integer                                           :: lrid         ! Length of RUNID character string 
    integer                                           :: lunfou
    integer                             , external    :: newlun
    integer                             , external    :: nc_def_var
    real(fp)                                          :: freqnt       ! Frequency in degrees per hour 
    real(fp)                                          :: tfasto       ! Stop time in minutes 
    real(fp)                                          :: tfastr       ! Start time in minutes 
    character(4)                                      :: blnm
    character(16)                                     :: fougrp
    character(20)                                     :: namfun       ! Local name for fourier function 
    character(30)                                     :: namfunlong   ! Local name for fourier function, including reference to the line in the fourier input file 
    character(256)                                    :: filename     ! fixed size version of runid, needed for character concatenation 
    character(4)                                      :: part_nr
    integer                                           :: bck_inode
    integer                                           :: bck_nproc
    logical                                           :: bck_parll
    type(dfparalltype)                  , pointer     :: bck_gdparall
!
!! executable statements -------------------------------------------------------
!
    nofouvar            => gdp%gdfourier%nofouvar
    fconno              => gdp%gdfourier%fconno
    foutyp              => gdp%gdfourier%foutyp
    fouelp              => gdp%gdfourier%fouelp
    founam              => gdp%gdfourier%founam
    fouvarnam           => gdp%gdfourier%fouvarnam
    fouvarnamlong       => gdp%gdfourier%fouvarnamlong
    fouvarunit          => gdp%gdfourier%fouvarunit
    fouref              => gdp%gdfourier%fouref
    ftmsto              => gdp%gdfourier%ftmsto
    ftmstr              => gdp%gdfourier%ftmstr
    foufas              => gdp%gdfourier%foufas
    flayno              => gdp%gdfourier%flayno
    fnumcy              => gdp%gdfourier%fnumcy
    iblwl               => gdp%gdfourier%iblwl
    ibleh               => gdp%gdfourier%ibleh
    iblcn               => gdp%gdfourier%iblcn
    ibluv               => gdp%gdfourier%ibluv
    iblqf               => gdp%gdfourier%iblqf
    iblbs               => gdp%gdfourier%iblbs
    iblep               => gdp%gdfourier%iblep
    idfile              => gdp%gdfourier%idfile
    idvar               => gdp%gdfourier%idvar
    ntstep              => gdp%gdinttim%ntstep
    lundia              => gdp%gdinout%lundia
    io_prec             => gdp%gdpostpr%io_prec
    mergemap            => gdp%gdpostpr%mergemap
    !
    fougrp = 'fou-fields'
    filetype = getfiletype(gdp, FILOUT_FOU)
    !
    if (mergemap) then
        part_nr = ''
    else
        bck_inode = inode
        bck_nproc = nproc
        bck_parll = parll
        bck_gdparall => gdp%gdparall
        !
        write(part_nr,'(A,I3.3)') '-',inode
        !
        inode = 1
        nproc = 1
        parll = .false.
        gdp%gdparall => gdp%iopartit
    endif
    !
    mf                  => gdp%gdparall%mf
    ml                  => gdp%gdparall%ml
    nf                  => gdp%gdparall%nf
    nl                  => gdp%gdparall%nl
    iarrc               => gdp%gdparall%iarrc
    mmaxgl              => gdp%gdparall%mmaxgl
    nmaxgl              => gdp%gdparall%nmaxgl
    !
    if (inode == master .and. filetype == FTYPE_NETCDF) then
       filename = trifil(1:3) // 'f' // trim(trifil(5:)) // trim(part_nr) 
       write(filename,'(2a)') trim(filename), '.nc'
       !
       ierror = nf90_open(filename, NF90_WRITE, idfile); call nc_check_err(lundia, ierror, "opening file", filename)
    endif
    !
    if (filetype == FTYPE_ASCII) then
       if (ntstep==1) goto 100
       irequest = REQUESTTYPE_DEFINE
    else
       if (ntstep==1) then
          irequest = REQUESTTYPE_DEFINE
       else
          irequest = REQUESTTYPE_WRITE
       endif
    endif
    !
    if (irequest == REQUESTTYPE_DEFINE) then
       if (filetype == FTYPE_NETCDF .and. inode == master) then
          !
          ! Assumption: wrimap has already created the map-NetCDF file
          !
          ierror = nf90_redef(idfile);    call nc_check_err(lundia, ierror, "redef file", filename)
          !
          ! Dimensions
          !
          iddim_n       = adddim(gdp, lundia, FILOUT_FOU, 'N'      , nmaxgl) ! Number of N-grid points (cell centres)
          iddim_m       = adddim(gdp, lundia, FILOUT_FOU, 'M'      , mmaxgl) ! Number of M-grid points (cell centres)
          !
          ifou  = 1
          iblwl = 0
          ibleh = 0
          iblcn = 0
          ibluv = 0
          iblqf = 0
          iblbs = 0
          iblep = 0
          do ivar=1, nofouvar
             if (ifou < nofou) then
                if (fouref(ifou+1,2) <= ivar) then
                   ifou = ifou + 1
                endif
             endif
             freqnt = foufas(ifou)*raddeg*3600.0_fp/dtsec
             tfastr = real(ftmstr(ifou),fp)*dtsec/60.0_fp
             tfasto = real(ftmsto(ifou),fp)*dtsec/60.0_fp
             !
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
             if (founam(ifou)(:2)=='u1') then
                ibluv = ibluv + 1
                blnm = 'UV??'
                write (blnm(3:4), '(i2.2)') ibluv
                namfun = 'velocity'
             endif
             if (founam(ifou)(:2)=='qx') then
                iblqf = iblqf + 1
                blnm = 'QF??'
                write (blnm(3:4), '(i2.2)') iblqf
                namfun = 'unit discharge'
             endif
             if (founam(ifou)(:2)=='ta') then
                iblbs = iblbs + 1
                blnm = 'BS??'
                write (blnm(3:4), '(i2.2)') iblbs
                namfun = 'bed stress'
             endif
             write(namfunlong,'(i3.3,2a)') fouref(ifou,1), ": ", trim(namfun)
             !
             idatt_fou(1) = addatt(gdp, lundia, FILOUT_FOU, 'layer_number', flayno(ifou) )
             idatt_fou(2) = addatt(gdp, lundia, FILOUT_FOU, 'Reference_date_in_YYYYMMDD', itdate )
             idatt_fou(3) = addatt(gdp, lundia, FILOUT_FOU, 'Starttime_fourier_analysis_in_minutes_since_reference_date', tfastr )
             idatt_fou(4) = addatt(gdp, lundia, FILOUT_FOU, 'Stoptime_fourier_analysis_in_minutes_since_reference_date', tfasto )
             idatt_fou(5) = addatt(gdp, lundia, FILOUT_FOU, 'Number_of_cycles', fnumcy(ifou) )
             idatt_fou(6) = addatt(gdp, lundia, FILOUT_FOU, 'Frequency_degrees_per_hour', freqnt )
             !
             call addelm(gdp, lundia, FILOUT_FOU, fougrp, trim(fouvarnam(ivar)), ' ', io_prec, 2, dimids=(/iddim_n, iddim_m/), &
                       & longname='Fourier analysis '//trim(namfunlong)//', '//trim(fouvarnamlong(ivar)), unit=fouvarunit(ivar), &
                       & attribs=idatt_fou, acl='z')
          enddo
          !
          call defnewgrp(idfile, FILOUT_FOU, fougrp, gdp, filename)
          !
          ! Reset the global indices
          !
          iblwl = 0
          ibleh = 0
          iblcn = 0
          ibluv = 0
          iblqf = 0
          iblbs = 0
          iblep = 0
       elseif (filetype == FTYPE_NETCDF) then ! inode /= master
          ! nothing to do
       else ! ASCII
          filename = "fourier." // trim(runid) // trim(part_nr) 
          !
          ! Open output file 'fourier.'runid on master node
          !
          if (inode == master) then
             open (newunit=lunfou, file = filename, status = 'unknown')
             !
             ! Write all requested fourier function output until IFOU > NOFOU
             !
             write (lunfou, '(a,a,a)') '*** Delft3D-FLOW utility FOUMOD *** version ',   &
                                     & versio, ' ***'
          endif
          !
          ! Continue writing the ascii file ...
          !
          irequest = REQUESTTYPE_WRITE
       endif
    endif
    !
    if (irequest == REQUESTTYPE_WRITE) then
       if (filetype == FTYPE_NETCDF .and. inode == master) then
          do ivar=1, nofouvar
             ierror = nf90_inq_varid(idfile, trim(fouvarnam(ivar)), idvar(ivar)); call nc_check_err(lundia, ierror, "inq_varid "//trim(fouvarnam(ivar))  , filename)
          enddo
       endif
       !
       ifou = 1
       !
       ! Write requested fourier function output for scalar quantity
       !
       do 
          if (ifou > nofou) exit
          !
          if (foutyp(ifou)=='s') then
             call wrfous(nmax      ,mmax      ,nmaxus    ,kmax      ,lmax      , &
                       & nofou     ,ifou      ,lunfou    ,dtsec     ,namcon    , &
                       & kcs       ,xz        ,yz        ,xcor      ,ycor      , &
                       & kfu       ,kfv       ,itdate    ,filename  ,filetype  , &
                       & fougrp    ,iarrc     ,mf        ,ml        ,nf        , &
                       & nl        ,gdp       )
             ifou = ifou + 1
          else
             !
             ! Write requested fourier function output for vector quantity
             ! and add 2 to ifou afterwards
             !
             call wrfouv(nmax      ,mmax      ,nmaxus    ,kmax      ,nofou     , &
                       & ifou      ,lunfou    ,dtsec     ,kcs       ,xz        , &
                       & yz        ,alfas     ,xcor      ,ycor      ,kfu       , &
                       & kfv       ,itdate    ,filename  ,filetype  ,fougrp    , &
                       & iarrc     ,mf        ,ml        ,nf        ,nl        , &
                       & gdp       )
             !
             ifou = ifou + 2
          endif
       enddo
    endif
    !
    ! Close fourier output file
    !
    if (filetype == FTYPE_NETCDF) then
       if (inode == master) then
          ierror = nf90_close(idfile); call nc_check_err(lundia, ierror, "closing file", filename)
       endif
    elseif (inode == master) then
       close (lunfou)
    endif
    !
100 continue
    if (.not.mergemap) then
        inode = bck_inode
        parll = bck_parll
        nproc = bck_nproc
        gdp%gdparall => bck_gdparall
    endif
end subroutine wrfou
