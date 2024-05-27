subroutine rdic(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
              & runid     ,restid    ,filic     ,fmtic     ,salin     , &
              & temp      ,const     ,secflo    ,lturi     ,lsal      , &
              & ltem      ,lstsc     ,zini      ,mmax      , &
              & nmax      ,nmaxus    ,kmax      ,lstsci    ,ltur      , &
              & namcon    ,s1        ,u1        ,v1        ,r1        , &
              & rtur1     ,decay     ,umnldf    ,vmnldf    ,kfu       , &
              & kfv       ,dpd       ,lsed      ,gdp       )
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
!    Function: - Reads RESTID from the MD-file
!              - Tests if RUNID = RESTID (they must differ)
!              - If RESTID = ' ' then reads the following records
!                from the MD-file: FILIC, FMTIC, ZETA0, U0, V0,
!                S0, T0, C01 - C0xx and I0
!              - If FILIC = non-blank then reads the initial con-
!                dition from the attribute file, otherwise
!              - Reads  ZETA0, U0, V0, S0, T0, C01 - C0xx and I0
!                and fills the related arrays with these uniform
!                values
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer       , pointer :: itis
    integer       , pointer :: i_restart
    character(16) , pointer :: rst_layer_model
    character*(10), pointer :: trans_unit      !  Unit of the variables ATR and DTR
    include 'pardef.igd'
!
! Global variables
!
    integer                                                                     :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                       , intent(in)  :: lsal   !  Description and declaration in dimens.igs
    integer                                                       , intent(in)  :: lsed   !  Description and declaration in dimens.igs
    integer                                                       , intent(in)  :: lstsc  !  Description and declaration in dimens.igs
    integer                                                                     :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                       , intent(in)  :: ltem   !  Description and declaration in dimens.igs
    integer                                                                     :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: lturi  !  Description and declaration in tricom.igs
    integer                                                                     :: lundia !  Description and declaration in inout.igs
    integer                                                                     :: lunmd  !  Description and declaration in inout.igs
    integer                                                                     :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: nrrec  !!  Pointer to the record number in the MD-file
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: kfv    !  Description and declaration in esm_alloc_int.f90
    logical                                                       , intent(in)  :: const  !  Description and declaration in procs.igs
    logical                                                                     :: error  !!  Flag=TRUE if an error is encountered
    logical                                                       , intent(in)  :: salin  !  Description and declaration in procs.igs
    logical                                                       , intent(in)  :: secflo !  Description and declaration in procs.igs
    logical                                                       , intent(in)  :: temp   !  Description and declaration in procs.igs
    real(fp)                                                                    :: zini   !!  Initial water elevation in the model
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: dpd    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: umnldf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: vmnldf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, ltur) :: rtur1  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci) :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lstsc)                                                  :: decay  !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                :: filic  !!  File name of initial condition file
    character(*)                                                                :: mdfrec !!  Standard rec. length in MD-file (300)
    character(*)                                                                :: restid !!  Run identification of the restart
                                                                                          !!  file. If RESTID = non-blank then
                                                                                          !!  current simulation will use this
                                                                                          !!  file to for setting the initial
                                                                                          !!  conditions
    character(*)                                                  , intent(in)  :: runid  !!  Run identification code for the cur-
                                                                                          !!  rent simulation (used to determine
                                                                                          !!  the names of the in- /output files
                                                                                          !!  used by the system)
    character(2)                                                  , intent(out) :: fmtic  !!  File format of initial condition file
    character(20), dimension(lstsci + ltur)                                     :: namcon !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                             :: ierr    ! Status variable for (de)allocation
    integer                             :: k       ! Help var. 
    integer                             :: l       ! Help var. 
    integer                             :: lconc   ! Number of constituents defined by user (excl. Salinity, Temperature, Secondary flow and Quantities for the Turb. models) 
    integer                             :: lenc    ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                             :: lkw     ! Length (in characters) of keyword 
    integer                             :: ll      ! Help var. 
    integer                             :: lnconc  ! Help var. for constituent 
    integer                             :: nlook   ! Help var.: nr. of data to look for in the MD-file 
    integer                             :: ntrec   ! Help. var to keep track of NRREC 
    integer , dimension(:), allocatable :: coninit ! Flag indicating whether a constituent has been initialized (0 = not initialized, 1 = initialized)
    logical                             :: defaul  ! Flag set to YES if default value may be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical                             :: found   ! FOUND=TRUE if KEYW in the MD-file was found 
    logical                             :: lerror  ! Flag=TRUE if a local error is encountered 
    logical                             :: newkw   ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical                             :: nodef   ! Flag set to YES if default value may NOT be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    real(fp)                            :: daysec  ! Number of seconds in one day
    real(fp)                            :: misval  ! Value for missing data
    real(fp)                            :: rdef    ! Help var. containing default va- lue(s) for real variable 
    real(fp), dimension(:), allocatable :: rval    ! Help array (real) where the data, recently read from the MD-file, are stored temporarily 
    real(fp), dimension(:), allocatable :: wrkini  ! Work array for reading initial conditions
    character(11)                       :: fmtdef  ! Default file format (usually=blank) 
    character(11)                       :: fmttmp  ! Help variable for file format 
    character(12)                       :: fildef  ! Default file name (usually = blank) 
    character(20)                       :: cdef    ! Default value when CHULP not found 
    character(20)                       :: chulp   ! Help var. 
    character(6)                        :: keyw    ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM)
    character(200)                      :: message
!
!! executable statements -------------------------------------------------------
!
    itis            => gdp%gdrdpara%itis
    i_restart       => gdp%gdrestart%i_restart
    rst_layer_model => gdp%gdrestart%rst_layer_model
    trans_unit      => gdp%gdpostpr%trans_unit
    !
    rdef   = 0.0
    nlook  = 1
    lerror = .false.
    newkw  = .true.
    defaul = .true.
    nodef  = .not.defaul
    fildef = ' '
    fmtdef = 'FRformatted'
    cdef   = ' '
    lconc  = lstsc - max(lsal, ltem)
    daysec = 24.*60.*60.
    misval = -999.0_fp
    !
    ! Initialize global parameters
    !
    zini  = 0.
    fmtic = 'FR'
    lturi = ltur
    !
    ! define or read names of constituents
    !
    allocate(coninit(lstsci), stat = ierr)
    coninit = 0
    lnconc  = 0
    !
    ! temporary work arrays
    !
    allocate(wrkini(kmax), stat = ierr)
    allocate(rval  (kmax), stat = ierr)
    wrkini = 0.0_fp
    rval   = 0.0_fp
    !
    ! define name of salinity  (Sub1(1:1) = 'S')
    !
    if (salin) then
       lnconc         = lnconc + 1
       namcon(lnconc) = 'Salinity'
    endif
    !
    ! define name of temperature  (Sub1(2:2) = 'T')
    !
    if (temp) then
       lnconc         = lnconc + 1
       namcon(lnconc) = 'Temperature'
    endif
    !
    ! read names of constituents, if lconc > 0 then namc <> ' '
    !
    keyw  = 'Namc  '
    ntrec = nrrec
    lenc  = 20
    do l = 1, lconc
       lnconc = lnconc + 1
       if (l < 10) then
          write (keyw(5:5), '(i1)') l
       else
          write (keyw(5:6), '(i2)') l
       endif
       namcon(lnconc) = cdef
       call prop_get_string(gdp%mdfile_ptr,'*',trim(keyw),namcon(lnconc))
       !
       ! test for namcon = ' ', which is per definition not possible
       ! because lconc is defined by namcon values <> ' '
       !
       if (namcon(lnconc) == cdef) then
          error = .true.
          call prterr(lundia    ,'V015'    ,' '       )
       endif
    enddo
    !
    ! define name of Secondary flow  (Sub1(*) = 'I')
    !
    if (secflo) then
       lnconc         = lnconc + 1
       namcon(lnconc) = 'Secondary flow'
    endif
    !
    ! define names of turbulence
    !
    if (ltur >= 1) then
       lnconc         = lnconc + 1
       namcon(lnconc) = 'Turbulent energy    '
    endif
    if (ltur == 2) then
       lnconc         = lnconc + 1
       namcon(lnconc) = 'Energy dissipation  '
    endif
    !
    ! not twice the same name
    !
    do l = 1, lnconc
       do ll = 1, l - 1
          if (namcon(ll) == namcon(l)) then
             error = .true.
             call prterr(lundia    ,'U160'    ,namcon(l) )
          endif
       enddo
    enddo
    !
    ! locate 'Restid' record for restart run-identification
    !
    restid = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'Restid_timeindex', i_restart)
    call prop_get_string(gdp%mdfile_ptr, '*', 'Restid', restid)
    if (restid /= ' ') then
       !
       ! restart from file
       ! test on consistency: runid is not restid
       !
       if (runid == restid) then
          call prterr(lundia    ,'V005'    ,' '       )
          error = .true.
       endif
       !
       ! Initialize the layer type of restart/map file. 
       ! Will be read in case of restart from a map-file, but may not be specified in the file. 
       ! For z-layer models with ZTBML, this may cause problems. 
       ! Therefore set the layering_model of the restart file to UNKNOWN. Is checked in routine CHKSET.f90.
       !
       rst_layer_model = 'UNKNOWN'
       !
       call rstfil(lundia    ,error     ,restid    ,lturi     ,mmax      , &
                 & nmaxus    ,kmax      ,lstsci    ,ltur      , &
                 & s1        ,u1        ,v1        ,r1        ,rtur1     , &
                 & umnldf    ,vmnldf    ,kfu       ,kfv       , &
                 & dpd       ,namcon    ,coninit   ,gdp       )
    endif
    !
    ! locate 'Filic' record for initial cond. in extra input file
    !
    filic = ' '
    call prop_get_string(gdp%mdfile_ptr, '*', 'Filic', filic)
    if (filic /= ' ' .and. restid == ' ') then
       !
       ! initial conditions in file
       ! locate 'Fmtic ' record for format definition of input file
       !
       fmtic = 'FR'
       call prop_get_string(gdp%mdfile_ptr, '*', 'Fmtic', fmtic)
       fmttmp = fmtic
       call filfmt(lundia    ,'Fmtic'      ,fmttmp    ,lerror    ,gdp       )
       call icfil(lundia    ,error     ,filic     ,fmttmp    ,mmax      , &
                & nmax      ,nmaxus    ,kmax      ,lstsci    ,s1        , &
                & u1        ,v1        ,r1        ,gdp       )
    else
       !
       ! no initial conditions file
       !
       if (restid == ' ') then
          !
          ! no restart file
          ! 'Zeta0 '
          !
          zini = rdef
          call prop_get(gdp%mdfile_ptr,'*','Zeta0',zini)
          !
          ! copy zini in s1
          !
          s1 = zini
          !
          ! 'U0' for all layers (or just one value)
          ! First read as string to check whether U0 specification is present
          !
          wrkini = 0.0_fp
          chulp  = ' '
          call prop_get_string(gdp%mdfile_ptr,'*','U0',chulp)
          if (chulp /= ' ') then
             rval  = misval
             keyw  = 'U0    '
             ntrec = nrrec
             nlook = 0
             call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                       & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                       & ntrec     ,lundia    ,gdp       )
             if (lerror) then
                lerror = .false.
                wrkini = rdef
             else
                if (kmax == 1) then
                   wrkini = rval(1)
                elseif (        comparereal(rval(1),misval) /= 0 &
                        & .and. comparereal(rval(2),misval) == 0  ) then
                   !
                   ! One value to be used for all layers
                   !
                   wrkini = rval(1)
                else
                   !
                   ! Expecting a value specified for each layer
                   !
                   do k = 1, kmax
                      if (comparereal(rval(k),misval) == 0) then
                         write(message,'(a,i0,a,i0,a)') 'Expecting ', kmax, &
                              & ' values to be defined for U0, but value number ', k, ' is not defined.'
                         call prterr(lundia, 'P004', message)
                         call d3stop(1, gdp)
                      endif
                      wrkini(k) = rval(k)
                   enddo
                endif
             endif
          endif
          !
          ! copy wrkini in u1
          !
          do k = 1, kmax
             u1(:, :, k) = wrkini(k)
          enddo
          !
          ! 'V0' for all layers (or just one value)
          ! First read as string to check whether V0 specification is present
          !
          wrkini = 0.0_fp
          chulp  = ' '
          call prop_get_string(gdp%mdfile_ptr,'*','V0',chulp)
          if (chulp /= ' ') then
             rval  = misval
             keyw  = 'V0    '
             ntrec = nrrec
             nlook = 0
             call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                       & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                       & ntrec     ,lundia    ,gdp       )
             if (lerror) then
                lerror = .false.
                wrkini = rdef
             else
                if (kmax == 1) then
                   wrkini = rval(1)
                elseif (        comparereal(rval(1),misval) /= 0 &
                        & .and. comparereal(rval(2),misval) == 0  ) then
                   !
                   ! One value to be used for all layers
                   !
                   wrkini = rval(1)
                else
                   !
                   ! Expecting a value specified for each layer
                   !
                   do k = 1, kmax
                      if (comparereal(rval(k),misval) == 0) then
                         write(message,'(a,i0,a,i0,a)') 'Expecting ', kmax, &
                              & ' values to be defined for V0, but value number ', k, ' is not defined.'
                         call prterr(lundia, 'P004', message)
                         call d3stop(1, gdp)
                      endif
                      wrkini(k) = rval(k)
                   enddo
                endif
             endif
          endif
          !
          ! copy wrkini in v1
          !
          do k = 1, kmax
             v1(:, :, k) = wrkini(k)
          enddo
       endif
       !
       ! lnconc is a counter for the pointer of the r1 array for all
       ! constituents.
       !
       lnconc = 0
       !
       ! Salinity
       !
       if (salin) then
          lnconc = lnconc + 1
          if (coninit(lnconc)==0) then
             !
             ! 'S0' (if salin = true) for all layers (or just one value)
             ! First read as string to check whether S0 specification is present
             !
             wrkini = 0.0_fp
             chulp  = ' '
             call prop_get_string(gdp%mdfile_ptr,'*','S0',chulp)
             if (chulp /= ' ') then
                rval  = misval
                keyw  = 'S0    '
                ntrec = nrrec
                nlook = 0
                call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                          & mdfrec    ,rval      ,rdef      ,nodef     ,nrrec     , &
                          & ntrec     ,lundia    ,gdp       )
                if (lerror) then
                   error  = .true.
                   lerror = .false.
                else
                   if (kmax == 1) then
                      wrkini = rval(1)
                   elseif (        comparereal(rval(1),misval) /= 0 &
                           & .and. comparereal(rval(2),misval) == 0  ) then
                      !
                      ! One value to be used for all layers
                      !
                      wrkini = rval(1)
                   else
                      !
                      ! Expecting a value specified for each layer
                      !
                      do k = 1, kmax
                         if (comparereal(rval(k),misval) == 0) then
                            write(message,'(a,i0,a,i0,a)') 'Expecting ', kmax, &
                                 & ' values to be defined for S0, but value number ', k, ' is not defined.'
                            call prterr(lundia, 'P004', message)
                            call d3stop(1, gdp)
                         endif
                         wrkini(k) = rval(k)
                      enddo
                   endif
                endif
             endif
             !
             ! copy wrkini in r1
             !
             do k = 1, kmax
                r1(:, :, k, lnconc) = wrkini(k)
             enddo
          endif
       endif
       !
       ! Temperature
       !
       if (temp) then
          lnconc = lnconc + 1
          if (coninit(lnconc)==0) then
             !
             ! locate and read 'T0    ' record for wrkini
             ! if temp  = .true.
             ! default value not allowed => nodef
             !
             ! Initialize WRKINI for KMAX layers
             !
             rval   = misval
             wrkini = 0.0_fp
             keyw   = 'T0    '
             ntrec  = nrrec
             nlook  = 0
             call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                       & mdfrec    ,rval      ,rdef      ,nodef     ,nrrec     , &
                       & ntrec     ,lundia    ,gdp       )
             if (lerror) then
                error  = .true.
                lerror = .false.
             else
                if (kmax == 1) then
                   wrkini = rval(1)
                elseif (        comparereal(rval(1),misval) /= 0 &
                        & .and. comparereal(rval(2),misval) == 0  ) then
                   !
                   ! One value to be used for all layers
                   !
                   wrkini = rval(1)
                else
                   !
                   ! Expecting a value specified for each layer
                   !
                   do k = 1, kmax
                      if (comparereal(rval(k),misval) == 0) then
                         write(message,'(a,i0,a,i0,a)') 'Expecting ', kmax, &
                              & ' values to be defined for T0, but value number ', k, ' is not defined.'
                         call prterr(lundia, 'P004', message)
                         call d3stop(1, gdp)
                      endif
                      wrkini(k) = rval(k)
                   enddo
                endif
             endif
             !
             ! copy wrkini in r1
             !
             do k = 1, kmax
                r1(:, :, k, lnconc) = wrkini(k)
             enddo
          endif
       endif
       !
       ! Sediment concentrations and tracers
       !
       if (const) then
          !
          ! 'C0##  ' record if const = .true.
          ! read wrkini(lmaxc ) from record where lmaxc = ltem + l
          ! First read as string to check whether C0 specification is present
          !
          keyw = 'C0?   '
          do l = 1, lconc
             lnconc = lnconc + 1
             if (coninit(lnconc)==0) then
                !
                ! Initialize WRKINI for KMAX layers
                !
                wrkini = 0.0_fp
                !
                if (l < 10) then
                   write (keyw(3:3), '(i1)') l
                else
                   write (keyw(3:4), '(i2)') l
                endif
                chulp = ' '
                call prop_get_string(gdp%mdfile_ptr,'*',trim(keyw),chulp)
                if (chulp /= ' ') then
                   rval  = misval
                   ntrec = nrrec
                   nlook = 0
                   call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                             & mdfrec    ,rval      ,rdef      ,nodef     ,nrrec     , &
                             & ntrec     ,lundia    ,gdp       )
                   if (lerror) then
                      lerror = .false.
                   else
                      if (kmax == 1) then
                         wrkini = rval(1)
                      elseif (        comparereal(rval(1),misval) /= 0 &
                              & .and. comparereal(rval(2),misval) == 0  ) then
                         !
                         ! One value to be used for all layers
                         !
                         wrkini = rval(1)
                      else
                         !
                         ! Expecting a value specified for each layer
                         !
                         do k = 1, kmax
                            if (comparereal(rval(k),misval) == 0) then
                               write(message,'(a,i0,a,i0,a,i0,a)') 'Expecting ', kmax, &
                                    & ' values to be defined for C0', l, ', but value number ', k, ' is not defined.'
                               call prterr(lundia, 'P004', message)
                               call d3stop(1, gdp)
                            endif
                            wrkini(k) = rval(k)
                         enddo
                      endif
                   endif
                endif
                !
                ! copy wrkini in r1
                !
                do k = 1, kmax
                   r1(:, :, k, lnconc) = wrkini(k)
                enddo
             endif
          enddo
       endif
       !
       ! Secondary flow
       !
       if (secflo) then
          lnconc = lnconc + 1
          if (coninit(lnconc)==0) then
             !
             ! 'I0    ' record for wrkini
             ! if secflo = .true.
             ! default value not allowed
             ! write in r1
             !
             ! Initialize WRKINI for KMAX layers
             !
             wrkini = 0.0_fp
             keyw   = 'I0    '
             ntrec  = nrrec
             nlook  = 0
             call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                       & mdfrec    ,rval      ,rdef      ,nodef     ,nrrec     , &
                       & ntrec     ,lundia    ,gdp       )
             if (lerror) then
                lerror = .false.
             else
                if (kmax == 1) then
                   wrkini = rval(1)
                elseif (        comparereal(rval(1),misval) /= 0 &
                        & .and. comparereal(rval(2),misval) == 0  ) then
                   !
                   ! One value to be used for all layers
                   !
                   wrkini = rval(1)
                else
                   !
                   ! Expecting a value specified for each layer
                   !
                   do k = 1, kmax
                      if (comparereal(rval(k),misval) == 0) then
                         write(message,'(a,i0,a,i0,a)') 'Expecting ', kmax, &
                              & ' values to be defined for I0, but value number ', k, ' is not defined.'
                         call prterr(lundia, 'P004', message)
                         call d3stop(1, gdp)
                      endif
                      wrkini(k) = rval(k)
                   enddo
                endif
             endif
             !
             ! copy wrkini in r1
             !
             do k = 1, kmax
                r1(:, :, k, lnconc) = wrkini(k)
             enddo
          endif
       endif
    endif
    !=======================================================================
    !
    ! Read decay rates For both Salinity and Temperature skipped
    !
    rdef   = 0.
    lnconc = 0
    if (lsal /= 0) lnconc = lnconc + 1
    if (ltem /= 0) lnconc = lnconc + 1
    !
    do l = 1, lconc
       !
       if (l > 99) exit
       lnconc        = lnconc + 1
       if (lnconc > lstsc) exit
       !
       decay(lnconc) = rdef
       if (l > 9) then
          keyw          = 'Dcay  '
          write (keyw(5:6), '(i2)') l
       else
          keyw          = 'Decay '
          write (keyw(6:6), '(i1)') l
       endif
       ntrec         = nrrec
       lkw           = 6
       call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                 & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                 & 'NO'      )
       lerror = .false.
       !
       ! read DECAY from record, default value allowed => defaul
       !       keywrd found ?
       !
       if (found) then
          nlook = 1
          call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          ! reading error?
          !
          if (lerror) then
             lerror        = .false.
             decay(lnconc) = rdef
          else
             !
             ! Don't allow decay rates for sediment fractions
             !
             if (lconc<=lsed) then
                call prterr(lundia    ,'U021'    ,'Decay rate not allowed for sediment quantities'       )
                error = .true.
             endif
             !
             ! Check value >= 0. and make from decay per day, decay per sec
             !
             decay(lnconc) = rval(1)
             if (decay(lnconc) < 0.) then
                call prterr(lundia    ,'U162'    ,namcon(lnconc)       )
                error = .true.
             endif
             decay(lnconc) = decay(lnconc)/daysec
          endif
       endif
    enddo
    deallocate(coninit, stat = ierr)
    deallocate(wrkini, stat = ierr)
    deallocate(rval, stat = ierr)

    if (salin .and. .not. temp .and. lconc==0) then
    !  only salinity:
       trans_unit = '[PPT M3 ]'
    else if (temp .and. .not. salin .and. lconc==0) then
    !  only temperature
       trans_unit = '[TEMP M3]'
    else if (.not. salin .and. .not. temp .and. lconc>0) then
    !  only concentrations
       trans_unit = '[  KG   ]'
    else
       trans_unit = '[ MIXED ]'
    endif
end subroutine rdic
