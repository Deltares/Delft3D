subroutine rdproc(error    ,nrrec     ,mdfrec    ,htur2d      ,salin    , &
                & temp     ,wind      ,ktemp     ,keva        ,ivapop2  , &
                & irov     ,z0v       ,sferic      ,tgfcmp   , &
                & temeqs   ,saleqs    ,wstcof    ,rhoa        ,secflo   , &
                & betac    ,equili    ,lsec      ,chzmin      ,rmincf   , &
                & rtcmod   ,couplemod ,nonhyd    ,mmax        ,nmax     , &
                & nmaxus   ,sedim     ,idensform ,solrad_read2, gdp)
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
!    Function: - Reads physical coefficients for temperature
!                model, rigid wall, tidal forces, density &
!                wind: KTEMP, FCLOU, SAREA, IVAPOP, SECCHI,
!                      STANTON, mulsta, DALTON, muldal, IROV, Z0V, TGFCMP,
!                      TEMPW, SALW, WSTRES, RHOA , SDlake, Wslake
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
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
    real(fp)                   , pointer :: cfrcon
    real(fp)                   , pointer :: cp
    real(fp)                   , pointer :: sarea
    real(fp)                   , pointer :: fclou
    real(fp)                   , pointer :: gapres
    real(fp)                   , pointer :: stanton
    real(fp)                   , pointer :: mulsta
    real(fp)                   , pointer :: dalton
    real(fp)                   , pointer :: muldal
    real(fp)                   , pointer :: qtotmx
    real(fp)                   , pointer :: lambda
    integer                    , pointer :: wslake
    integer                    , pointer :: sdlake
    integer                    , pointer :: ivapop
    integer                    , pointer :: maseva
    logical                    , pointer :: free_convec
    logical                    , pointer :: solrad_read
    integer                    , pointer :: itis
    integer                    , pointer :: nrcmp
    character(8), dimension(:) , pointer :: tgfdef
    integer                    , pointer :: lunmd
    integer                    , pointer :: lundia
    integer                    , pointer :: lunscr
    integer                    , pointer :: m1_nhy
    integer                    , pointer :: m2_nhy
    integer                    , pointer :: n1_nhy
    integer                    , pointer :: n2_nhy
    integer                    , pointer :: nhiter
    integer                    , pointer :: sleepduringwave ! Description and decleration in tricom.igs
    real(fp)                   , pointer :: epsnh
    logical                    , pointer :: l2norm
    real(fp)                   , pointer :: rel_epsnh
    real(fp)                   , pointer :: tetaq
    real(fp)                   , pointer :: tetaz
    logical                    , pointer :: flag_pp
    character(8)               , pointer :: updwl
    character(8)               , pointer :: precon
    character(8)               , pointer :: krylov
    real(fp)                   , pointer :: milu
    integer                    , pointer :: nh_level ! has value nh_weak or nh_strong
    real(fp)                   , pointer :: ti_nodal
    logical                    , pointer :: xbeach
    real(fp)                   , pointer :: tunit
    character(10)              , pointer :: tunitstr
    logical                    , pointer :: ztbml
!
! Global variables
!
    integer                  :: irov        !  Description and declaration in physco.igs
    integer                  :: idensform   !  Description and declaration in physco.igs
    integer    , intent(out) :: ivapop2     !  Copy of gdheat%ivapop for use in TDATOM/RDHEAT
    integer                  :: keva        !  Description and declaration in tricom.igs
    integer                  :: ktemp       !  Description and declaration in tricom.igs
    integer    , intent(out) :: lsec        !  Description and declaration in dimens.igs
    integer    , intent(in)  :: mmax        !  Description and declaration in esm_alloc_int.f90
    integer    , intent(in)  :: nmax        !  Description and declaration in esm_alloc_int.f90
    integer    , intent(in)  :: nmaxus      !  Description and declaration in esm_alloc_int.f90
    integer                  :: nrrec       !!  Pointer to the record number in the MD-file
    integer                  :: rtcmod      !  Description and declaration in rtc.igs
    logical    , intent(out) :: couplemod   !  Description and declaration in procs.igs
    logical    , intent(out) :: error       !!  Flag=TRUE if an error is encountered
    logical    , intent(in)  :: htur2d      !  Description and declaration in procs.igs
    logical    , intent(in)  :: nonhyd
    logical    , intent(in)  :: salin       !  Description and declaration in procs.igs
    logical    , intent(in)  :: secflo      !  Description and declaration in procs.igs
    logical    , intent(in)  :: sedim       !  Description and declaration in procs.igs
    logical                  :: sferic      !  Description and declaration in tricom.igs
    logical    , intent(out) :: solrad_read2 ! Copy of gdheat%solrad_read for use in TDATOM/RDHEAT
    logical    , intent(in)  :: temp        !  Description and declaration in procs.igs
    logical    , intent(in)  :: wind        !  Description and declaration in procs.igs
    real(fp)                 :: betac       !  Description and declaration in tricom.igs
    real(fp)   , intent(out) :: chzmin      !  Description and declaration in numeco.igs
    real(fp)                 :: rhoa        !  Description and declaration in physco.igs
    real(fp)   , intent(out) :: rmincf      !  Description and declaration in numeco.igs
    real(fp)                 :: saleqs      !  Description and declaration in tricom.igs
    real(fp)                 :: temeqs      !  Description and declaration in tricom.igs
    real(fp)                 :: z0v         !  Description and declaration in physco.igs
    real(fp), dimension(6)   :: wstcof      !  Description and declaration in physco.igs
    character(*)             :: mdfrec      !!  Standard rec. length in MD-file (300)
    character(1)             :: equili      !!  Equilibrium or advection and diffusion default = no equilibrium ('N') which means lsec = 1
    character(36)            :: tgfcmp      !  Description and declaration in tricom.igs
!
! Local variables
!
    integer                     :: idef        ! Help var. containing default value(s) for integer variable 
    integer                     :: istat
    integer                     :: k
    integer                     :: kbeg
    integer                     :: kend
    integer                     :: lenc        ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                     :: lkw         ! Actual length of KEYW 
    integer                     :: nlook       ! Help var.: nr. of data to look for in the MD-file 
    integer                     :: ntrec       ! Help. var to keep track of NRREC 
    integer, dimension(4)       :: ival        ! Value(s) for integer variable Help array (integer)
    logical                     :: defaul      ! Flag set to YES if default value may be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical                     :: found       ! FOUND=TRUE if KEYW in the MD-file was found 
    logical                     :: lerror      ! Flag=TRUE if an error is encountered 
    logical                     :: lhelp       ! Help flag
    logical                     :: lmaseva
    logical                     :: levamas
    logical                     :: newkw       ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    real(fp)                    :: rdef        ! Help var. containing default value(s) for real variable 
    real(fp)                    :: rsmall      ! Help variable 
    real(fp), dimension(6)      :: rval        ! Help array (real) where the data, recently read from the MD-file, are stored temporarily 
    real(sp)                    :: sprval      ! same, but for one single single precision value    
    real(fp)                    :: fprval      ! same, but for one single flexible precision value    
    character(1)                :: cdef        ! Help var. containing default value(s) for character variable 
    character(1)                :: chulp       ! Help variabele where the data, recently read from the MD-file, are stored temporarily 
    character(6)                :: keyw        ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
    character(12)               :: tdef        ! Help variable containing blanks 
    character(12), dimension(3) :: thulp       ! Help character variable 
    character(36)               :: tgfhlp      ! Help string to define right sequence components for Tide generating forces 
    character(256)              :: errmsg
    character(256)              :: stringval
    character(300)              :: message
!
    data thulp/3*'            '/
!
!! executable statements -------------------------------------------------------
!
    cfrcon              => gdp%gdheat%cfrcon
    cp                  => gdp%gdheat%cp
    sarea               => gdp%gdheat%sarea
    fclou               => gdp%gdheat%fclou
    gapres              => gdp%gdheat%gapres
    wslake              => gdp%gdheat%wslake
    sdlake              => gdp%gdheat%sdlake
    stanton             => gdp%gdheat%stanton
    mulsta              => gdp%gdheat%mulsta
    dalton              => gdp%gdheat%dalton
    muldal              => gdp%gdheat%muldal
    qtotmx              => gdp%gdheat%qtotmx
    lambda              => gdp%gdheat%lambda
    ivapop              => gdp%gdheat%ivapop
    maseva              => gdp%gdheat%maseva
    free_convec         => gdp%gdheat%free_convec
    solrad_read         => gdp%gdheat%solrad_read
    m1_nhy              => gdp%gdnonhyd%m1_nhy
    m2_nhy              => gdp%gdnonhyd%m2_nhy
    n1_nhy              => gdp%gdnonhyd%n1_nhy
    n2_nhy              => gdp%gdnonhyd%n2_nhy
    nhiter              => gdp%gdnonhyd%nhiter
    rel_epsnh           => gdp%gdnonhyd%rel_epsnh
    tetaq               => gdp%gdnonhyd%tetaq
    tetaz               => gdp%gdnonhyd%tetaz
    flag_pp             => gdp%gdnonhyd%flag_pp
    updwl               => gdp%gdnonhyd%updwl
    precon              => gdp%gdnonhyd%precon
    krylov              => gdp%gdnonhyd%krylov
    milu                => gdp%gdnonhyd%milu
    nh_level            => gdp%gdnonhyd%nh_level
    epsnh               => gdp%gdnonhyd%epsnh
    l2norm              => gdp%gdnonhyd%l2norm
    lunmd               => gdp%gdinout%lunmd
    lundia              => gdp%gdinout%lundia
    lunscr              => gdp%gdinout%lunscr
    nrcmp               => gdp%gdtfzeta%nrcmp
    tgfdef              => gdp%gdtfzeta%tgfdef
    itis                => gdp%gdrdpara%itis
    ti_nodal            => gdp%gdinttim%ti_nodal
    xbeach              => gdp%gdprocs%xbeach
    tunit               => gdp%gdexttim%tunit
    tunitstr            => gdp%gdexttim%tunitstr
    ztbml               => gdp%gdzmodel%ztbml
    sleepduringwave     => gdp%gdtricom%sleepduringwave
    include 'tfzeta.gdt'
    !
    ! initialize local parameters
    !
    lerror      = .false.
    newkw       = .true.
    defaul      = .true.
    cdef        = 'N'
    tdef        = ' '
    idef        = 0
    nlook       = 1
    rsmall      = 1.0e-20
    !
    ! initialize parameters that are to be read
    !
    ktemp        = 0
    fclou        = 0.0
    sarea        = 0.0
    ivapop       = 0
    ivapop2      = 0
    keva         = 0
    gapres       = 101300.0_fp
    lambda       = -1.0_fp
    qtotmx       = 0.0_fp
    solrad_read  = .false.     
    solrad_read2 = .false.
    ztbml        = .false.
    !
    stanton     = 1.30e-3
    mulsta      = 1.0_fp
    dalton      = 1.30e-3
    muldal      = 1.0_fp
    maseva      = 0
    !
    irov        = 0
    z0v         = 0.0
    !
    tgfcmp      = ' '
    !
    temeqs      = 15.0
    saleqs      = 0.0
    !
    wstcof(1)   = 0.0025_fp
    wstcof(2)   = 0.0_fp
    wstcof(3)   = 0.0025_fp
    wstcof(4)   = 50.0_fp
    wstcof(5)   = 0.0025_fp
    wstcof(6)   = 100.0_fp
    rhoa        = 1.0
    !
    ! Density formula
    !
    idensform   = 0
    !
    ! items related to secundairy flow
    !
    betac       = 0.0
    equili      = 'N'
    lsec        = 0
    chzmin      = 20.0
    rmincf      = 2.5
    !
    m1_nhy      = 0
    n1_nhy      = 0
    m2_nhy      = 0
    n2_nhy      = 0
    !
    if (parll) then
       sleepduringwave = 100
    else
       sleepduringwave = 0
    endif
    !
    ! locate and read 'Ktemp ' number of temperature model use
    ! only if temp = .true., default value allowed => idef
    !
    if (temp) then
       keyw = 'Ktemp '
       ntrec = nrrec
       call read2i(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,ival      ,idef      ,defaul    ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          ktemp = idef
       else
          ktemp = ival(1)
       endif
       !
       sprval = -1.0_sp
       call prop_get(gdp%mdfile_ptr, '*', 'Exchcf', sprval)
       lambda = real(sprval,fp)       
       if (comparereal(lambda,-1.0_fp) /= 0) then
          if (ktemp == 3) then
             write (message,'(a,f12.4)') 'Exchange coefficient (lambda) in the Excess temperature model is fixed to ', lambda
             call prterr(lundia, 'G051', trim(message))
          else
             write (message,'(a,i0)') 'Exchange coefficient (keyword Exchcf) is not used in temperature model ', ktemp
             call prterr(lundia, 'U190', trim(message))
          endif
       endif
       sprval = 0.0_sp
       call prop_get(gdp%mdfile_ptr, '*', 'Qtotmx', sprval)
       qtotmx = real(sprval,fp)       
       if (comparereal(qtotmx,0.0_fp) /= 0) then
          if (ktemp == 3) then
             write (message,'(a,f12.4)') 'Heat exchange flux (Qtot) in the Excess temperature model is limited to ', qtotmx
             call prterr(lundia, 'G051', trim(message))
          else
             write (message,'(a,i0)') 'Heat exchange flux (keyword Qtotmx) is not used in temperature model ', ktemp
             call prterr(lundia, 'U190', trim(message))
          endif
       endif
       !
       ! test constistency
       !
       if (ktemp<0 .or. ktemp>5) then
          call prterr(lundia    ,'U051'    ,' '       )
          ktemp = idef
       endif
    endif
    !
    ! Locate and read 'Gapres' Global atmosferic pressure
    ! default value allowed => default
    !
    keyw = 'Gapres'
    ntrec = nrrec
    rdef = gapres
    lkw = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          gapres = rdef
       else
          gapres = rval(1)
       endif
    else
       gapres = rdef
    endif
    !
    ! process temp, only if ktemp > 0, then per definition temp = .true.
    !
    if (ktemp>0) then
       !
       ! locate and read 'Fclou ' clouded fraction of sky
       ! only for KTEMP = 1 and 4, hence not implemented yet !!
       ! default value allowed => defaul
       !
       if (ktemp==1 .or. ktemp==4) then
          keyw = 'Fclou '
          ntrec = nrrec
          rdef = fclou
          call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          if (lerror) then
             lerror = .false.
             fclou = rdef
          else
             fclou = rval(1)
          endif
       endif
       !
       ! locate and read 'Sarea ' water surface area
       ! default value allowed => defaul
       !
       if (ktemp<=3) then
          keyw = 'Sarea '
          ntrec = nrrec
          rdef = sarea
          call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          ! reading error?
          !
          if (lerror) then
             lerror = .false.
             sarea = rdef
          else
             sarea = rval(1)
          endif
       endif
       !
       ! For "Murakami" model
       !
       if (ktemp==4) then
          !
          ! Locate and read 'Ivapop 'to determine whether vapour
          ! pressure is to be computed or to be specified by user
          ! only for KTEMP = 4;   default value allowed => defaul
          !
          keyw = 'Ivapop'
          ntrec = nrrec
          lkw = 6
          call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                    & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                    & 'NO'      )
          lerror = .false.
          !
          ! not found ?
          !
          if (found) then
             call read2i(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                       & mdfrec    ,ival      ,idef      ,defaul    ,nrrec     , &
                       & ntrec     ,lundia    ,gdp       )
             if (lerror) then
                lerror = .false.
                ivapop = idef
             else
                ivapop = ival(1)
             endif
          else
             ivapop = idef
          endif
          ivapop2 = ivapop
       endif
       !
       ! For "Ocean" model
       !
       if (ktemp==5) then
          !
          ! reset RDEF
          !
          rdef = 0.0
          !
          ! Locate and read 'Stanton'
          ! default value allowed => defaul
          !
          keyw = 'Stantn'
          ntrec = nrrec
          rdef = stanton
          lkw = 6
          call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                    & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                    & 'NO'      )
          lerror = .false.
          !
          ! not found ?
          !
          if (found) then
             call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                       & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                       & ntrec     ,lundia    ,gdp       )
             !
             ! reading error?
             !
             if (lerror) then
                lerror = .false.
                stanton = rdef
             else
                stanton = rval(1)
                write(message, '(a,e14.5)') 'Ocean heat model: Stanton number specified to be ', stanton
                call prterr(lundia, 'G051', trim(message))
             endif
          else
             stanton = rdef
             write(message, '(a,e14.5)') 'Ocean heat model: Using default Stanton number ', stanton
             call prterr(lundia, 'G051', trim(message))
          endif
          !
          ! Locate and read optional real 'mulsta'
          !
          fprval = -999.0_fp
          call prop_get(gdp%mdfile_ptr, '*', 'mulsta', fprval)
          if (comparereal(fprval, -999.0_fp) /= 0) then
             mulsta = fprval
             if (comparereal(mulsta, 0.7_fp) == -1 .or. comparereal(mulsta, 1.3_fp) == 1) then
                write(message,'(a,f12.3)') 'Stanton multiplication factor mulsta has a value outside the region [0.7,1.3]: ', mulsta
                call prterr(lundia, 'P004', trim(message))
                call d3stop(1, gdp)
             else
                write(message,'(a,f12.3)') 'Stanton multiplication factor mulsta: ', mulsta
                call prterr(lundia, 'G051', trim(message))
             endif
          endif
          !
          ! reset RDEF
          !
          rdef = 0.0
          !
          ! Locate and read 'Dalton'
          ! default value allowed => defaul
          !
          keyw = 'Dalton'
          ntrec = nrrec
          rdef = dalton
          lkw = 6
          call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                    & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                    & 'NO'      )
          lerror = .false.
          !
          ! not found ?
          !
          if (found) then
             call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                       & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                       & ntrec     ,lundia    ,gdp       )
             !
             ! reading error?
             !
             if (lerror) then
                lerror = .false.
                dalton = rdef
             else
                dalton = rval(1)
                write(message, '(a,e14.5)') 'Ocean heat model: Dalton number specified to be ', dalton
                call prterr(lundia, 'G051', trim(message))
             endif
          else
             dalton = rdef
             write(message, '(a,e14.5)') 'Ocean heat model: Using default Dalton number ', dalton
             call prterr(lundia, 'G051', trim(message))
          endif
          !
          ! Locate and read optional real 'muldal'
          !
          fprval = -999.0_fp
          call prop_get(gdp%mdfile_ptr, '*', 'muldal', fprval)
          if (comparereal(fprval, -999.0_fp) /= 0) then
             muldal = fprval
             if (comparereal(muldal, 0.7_fp) == -1 .or. comparereal(muldal, 1.3_fp) == 1) then
                write(message,'(a,f12.3)') 'Dalton multiplication factor muldal has a value outside the region [0.7,1.3]: ', muldal
                call prterr(lundia, 'P004', trim(message))
                call d3stop(1, gdp)
             else
                write(message,'(a,f12.3)') 'Dalton multiplication factor muldal: ', muldal
                call prterr(lundia, 'G051', trim(message))
             endif
          endif
          !
          ! Locate and read 'Wslake'
          ! 
          Wslake = 0
          call prop_get(gdp%mdfile_ptr, '*', 'Wslake', Wslake)
          !
          ! test constistency
          !
          if (Wslake<0 .or. Wslake>1) then
             call prterr(lundia    ,'U061'    ,' '       )
             Wslake = 0
          endif
          !
          ! Locate and read 'SDlake'
          !
          SDlake = 0
          call prop_get(gdp%mdfile_ptr, '*', 'SDlake', SDlake)
          !
          ! test constistency
          !
          if (SDlake<0 .or. SDlake>1) then
             call prterr(lundia    ,'U061'    ,' '       )
             SDlake = 0
          endif
          !
          ! Some combination checks:
          !
          ! Error: SDlake=1 is useless when Wslake=0
          !
          if (Wslake==0 .and. SDlake==1) then
             write(message,'(a,f12.3)') 'SDlake=1 is not allowed when Wslake=0'
             call prterr(lundia, 'P004', trim(message))
             call d3stop(1, gdp)
          endif
          !
          ! Error: mulsta<>1.0 is useless when SDlake=0
          ! Error: muldal<>1.0 is useless when SDlake=0
          !
          if (SDlake == 0) then
             if (comparereal(mulsta, 1.0_fp) /= 0) then
                write(message,'(a,f12.3)') 'mulsta<>1.0 is not allowed when SDlake=0'
                call prterr(lundia, 'P004', trim(message))
                call d3stop(1, gdp)
             endif
             if (comparereal(muldal, 1.0_fp) /= 0) then
                write(message,'(a,f12.3)') 'muldal<>1.0 is not allowed when SDlake=0'
                call prterr(lundia, 'P004', trim(message))
                call d3stop(1, gdp)
             endif
          endif
          !
          ! Message: SDlake=1: Stanton and Dalton values will be replaced by windcd
          !
          if (SDlake == 1) then
             write(message,'(a,f12.3)') 'SDlake=1: Stanton and Dalton values will be replaced by windCD'
             call prterr(lundia, 'G051', trim(message))
          endif
          !
          ! reset RDEF
          !
          rdef = 0.0
          !
          ! Locate and read 'cfrcon'
          ! default value is 0.14
          !
          sprval = -999.0_sp
          !
          ! free convection is switched on by default
          !
          free_convec = .true.
          call prop_get(gdp%mdfile_ptr, '*', 'cfrcon', sprval)
          cfrcon = real(sprval, fp)
          if (comparereal(cfrcon, -999.0_fp) == 0) then
             cfrcon = 0.14_fp
          elseif (comparereal(cfrcon, 0.0_fp) == 0) then
             cfrcon = 0.0_fp
             free_convec = .false.
          else
             write(message,'(a,f12.3)') 'Specified free convection coefficient cfrcon = ', cfrcon
             call prterr(lundia, 'G051', trim(message))
          endif
          !
          ! reset sprval
          !
          sprval = 0.0_sp
          !
          ! Locate and read 'SolRad' for possible specification of Nett Solar Radiation in .tem file
          ! Default value is .false.
          !
          call prop_get(gdp%mdfile_ptr, '*', 'SolRad', solrad_read)
          solrad_read2 = solrad_read
          if (solrad_read) then
             call prterr(lundia, 'G051', 'Using solar radiation specified in .tem file')
          endif
       endif
    endif
    !
    ! KEVA
    !
    ! No FilEva specified                             : keva=0
    ! FilEva    specified, QEvap="derived"            : keva=2
    ! FilEva    specified, QEvap="computed" (default) : keva=3
    ! When reading FilEva values and evap=-999.0      : keva=1
    !
    keva = -999
    stringval = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'Fileva', stringval)
    if (stringval /= ' ') then
       !
       ! FilEva specified
       !
       write(errmsg,'(2a)') 'Using precipitation/evaporation specified in file ',trim(stringval)
       call prterr(lundia, 'G051', trim(errmsg))
       !
       ! QEvap
       !
       stringval = ' '
       call prop_get(gdp%mdfile_ptr, '*', 'QEvap', stringval)
       if (stringval /= ' ') then
          call small(stringval, 999)
          select case (stringval)   
          case ('derived')
             keva = 2
             write(errmsg,'(a)') 'Evaporative heat flux is derived from the prescribed evaporative mass flux'
             call prterr(lundia, 'G051', trim(errmsg))
          case ('computed')
             keva = 3
             write(errmsg,'(a)') 'Evaporative heat flux is calculated internally, independent of the prescribed mass flux'
             call prterr(lundia, 'G051', trim(errmsg))
          case default
             write(errmsg,'(a)') 'Value of keyword "QEvap" not recognized. Expecting "derived" or "computed"'
             call prterr(lundia, 'P004', trim(errmsg))
             call d3stop(1, gdp)
          end select
       else
          !
          ! Default: QEvap = computed
          !
          if (ktemp /= 3) then
             keva = 3
             write(errmsg,'(a)') 'QEvap not specified. Evaporative heat flux is calculated internally, independent of the prescribed mass flux'
             call prterr(lundia, 'G051', trim(errmsg))
          endif
       endif
    else
       !
       ! No FilEva specified
       !
       keva = 0
    endif
    !
    ! Density formula:
    !
    ! =0: Eckart
    ! =1: UNESCO
    ! Anything else: simply default to Unesco (with a message)
    !
    stringval = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'DenFrm', stringval)
    call small(stringval, len(stringval))
    select case( stringval )
        case( 'eckart' )
            idensform = dens_Eckart
            call prterr(lundia, 'G051', 'Using Eckart density formulation')
        case( 'unesco' )
            idensform = dens_UNESCO
            call prterr(lundia, 'G051', 'Using UNESCO density formulation')
        case( 'nacl' )
            idensform = dens_NaClSol
            call prterr(lundia, 'G051', 'Using NaCl   density formulation', gdp)
        case( ' '      )
            idensform = dens_UNESCO
            call prterr(lundia, 'G051', 'Using UNESCO density formulation by default')
        case default
            write(message,'(3a)') 'Unknown density formulation ', trim(stringval),'. Use UNESCO or Eckart.'
            call prterr(lundia, 'P004', trim(message))
            error = .true.
    end select
    !
    ! MASEVA = 1: rain/evaporation in continuity equation
    ! - Always when rain/evaporation read from file (keva=1)
    ! - Defined by keyword Evamas or Maseva in case ktemp > 0
    ! - Not possible in case ktemp = 3
    !
    ! Originally parameter maseva was based on keyword Evamas
    ! In the documentation the keyword was called Maseva
    ! To be sure it works, both keywords Evamas and Maseva may be used to switch parameter maseva on
    !
    if (keva > 0) then
       maseva = 1
    elseif (ktemp>0) then
       levamas = .false.
       lmaseva = .false.
       call prop_get(gdp%mdfile_ptr, '*', 'Evamas', levamas)
       call prop_get(gdp%mdfile_ptr, '*', 'Maseva', lmaseva)
       if (levamas .or. lmaseva) then
          if (ktemp == 3) then
             maseva = 0
             call prterr(lundia, 'U164', ' ')
          else
             maseva = 1
             call prterr(lundia, 'G051', 'Evaporation taken into account in continuity equation')
          endif
       else
          maseva = 0
          call prterr(lundia, 'G051', 'Evaporation not taken into account in continuity equation')
       endif
    endif
    !
    ! process rigid wall
    ! locate and read 'Irov ' ( 0/1/2: free/part/no slip)
    ! default value allowed => defaul
    !
    keyw = 'Irov  '
    ntrec = nrrec
    lkw = 4
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       call read2i(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,ival      ,idef      ,defaul    ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          irov = idef
       else
          irov = ival(1)
       endif
       !
       ! locate and read 'Z0v   ' roughness "height" for rigid walls
       ! only if irov > 0
       ! default value allowed => defaul
       !
       if (irov==1) then
          keyw = 'Z0v   '
          ntrec = nrrec
          rdef = z0v
          call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          ! stop when reading error(z0v is used in divisions)
          !
          if (lerror .or. rval(1)<rsmall) then
             call prterr(lundia    ,'U007'    ,'value for z0v (0 not allowed)' )
             error = .true.
             z0v = rdef
          else
             z0v = rval(1)
          endif
       endif
    endif
    !
    ! HLES needs full scale slip calculation, so set irov=3 in case
    ! irov=0 and htur2d
    !
    if (irov==0 .and. htur2d) then
       irov = 3
       write (lundia, *) '*** full scale free slip calculation for HLES'
       write (lunscr, *) '*** full scale free slip calculation for HLES'
    elseif (irov==2) then
       write (lundia, *) '*** no slip defined at walls'
       write (lunscr, *) '*** no slip defined at walls'
    else
    endif
    !
    ! locate and read 'Tidfor'
    ! record containing component names for the Tide generating force
    ! default value allowed => defaul
    !
    keyw = 'Tidfor'
    ntrec = nrrec
    lkw = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       lenc = 12
       nlook = 3
       call readnc(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,thulp     ,tdef      ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       ! NLOOK can have a different value if not all elements are
       ! read; is not correct
       !
       if (nlook/=3) then
          call prterr(lundia    ,'U036'    ,keyw      )
          lerror = .false.
       elseif (lerror) then
          lerror = .false.
       else
          tgfcmp = thulp(1) // thulp(2) // thulp(3)
       endif
    endif
    !
    ! Test contents string TGFCMP for unknown components and
    ! re-define string for correct sequence components
    !
    if (tgfcmp/=' ') then
       do k = 1, mxcmp
          kbeg = (k - 1)*3 + 1
          kend = k*3
          tgfhlp(kbeg:kend) = gdp%gdtfzeta%tgfdef(k)(1:3)
       enddo
       tgfhlp(34:36) = '---'
       !
       do k = 1, mxcmp
          kbeg = (k - 1)*3 + 1
          kend = k*3
          if (index(tgfhlp, tgfcmp(kbeg:kend))==0) then
             call prterr(lundia    ,'V081'    ,tgfcmp(kbeg:kend)    )
             error = .true.
          endif
       enddo
       !
       nrcmp = 0
       do k = 1, mxcmp
          kbeg = (k - 1)*3 + 1
          kend = k*3
          tgfhlp(kbeg:kend) = '---'
          if (index(tgfcmp, gdp%gdtfzeta%tgfdef(k)(1:3))/=0) then
             tgfhlp(kbeg:kend) = gdp%gdtfzeta%tgfdef(k)(1:3)
             nrcmp = nrcmp + 1
          endif
       enddo
       !
       ! Initialize TGFCMP=TGFHLP for NRCMP <> 0 else define to ' '
       !
       tgfcmp = ' '
       if (nrcmp/=0) tgfcmp = tgfhlp
    endif
    !
    ! Initialize NRCMP = 0
    ! Will be redefined in INITGF (called by INCHKI) for TGFCMP <> ' '
    !
    nrcmp = 0
    !
    ! process density, only if SALIN = true or SEDIM = true
    !
    ! locate and read 'Tempw ' water temperature for eq. of state
    ! default value allowed => defaul
    !
    nlook = 1
    keyw = 'Tempw '
    ntrec = nrrec
    rdef = temeqs
    call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       temeqs = rdef
    else
       temeqs = rval(1)
    endif
    !
    ! locate and read 'Salw  ' salinity for equation of state
    ! only if SALIN = .false. and TEMP = .true. nessecary. will be read
    ! if TEMP = .true. without concerning the value of SALIN
    ! default value allowed => defaul
    !
    keyw = 'Salw  '
    ntrec = nrrec
    rdef = saleqs
    call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
              & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
              & ntrec     ,lundia    ,gdp       )
    !
    ! reading error?
    !
    if (lerror) then
       lerror = .false.
       saleqs = rdef
    else
       saleqs = rval(1)
    endif
    !
    ! process wind, only if WIND is true
    !
    if (wind) then
       !
       ! locate and read 'Wstres' wind stress coefficient
       ! default value allowed => defaul
       !
       keyw = 'Wstres'
       ntrec = nrrec
       nlook = 0
       rdef = wstcof(1)
       rval(1) = 999.999_fp
       rval(2) = 999.999_fp
       rval(3) = 999.999_fp
       rval(4) = 999.999_fp
       rval(5) = 999.999_fp
       rval(6) = 999.999_fp
       call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          wstcof(1) =   0.0025_fp
          wstcof(2) =   0.0000_fp
          wstcof(3) =   0.0025_fp
          wstcof(4) =  50.0000_fp
          wstcof(5) =   0.0025_fp
          wstcof(6) = 100.0000_fp
       else
          wstcof(1) = rval(1)
          !
          ! Wstres can contain only one value (version 2.03 and older)
          ! in which case RVAL (2) = 999.999 and WSTCOF(2-4) will be
          ! re-defined:  WSTCOF(2)= 0.0
          !              WSTCOF(3) = WSTCOF(1)
          !              WSTCOF(4) = 100.
          !
          if (rval(2)>999.9989_fp .and. rval(2)<999.9991_fp) then
             wstcof(2) = 0.0_fp
             wstcof(3) = wstcof(1)
             wstcof(4) = 50.0_fp
             wstcof(5) = wstcof(1)
             wstcof(6) = 100.0_fp
             call prterr(lundia, 'G051', 'Number of pivot points to convert wind speed to wind drag coef.: 1')
          elseif (rval(5)>999.9989_fp .and. rval(5)<999.9991_fp) then
             wstcof(2) = rval(2)
             wstcof(3) = rval(3)
             wstcof(4) = rval(4)
             wstcof(5) = rval(3)
             wstcof(6) = rval(4) + 1.0_fp
             call prterr(lundia, 'G051', 'Number of pivot points to convert wind speed to wind drag coef.: 2')
          else
             wstcof(2) = rval(2)
             wstcof(3) = rval(3)
             wstcof(4) = rval(4)
             wstcof(5) = rval(5)
             wstcof(6) = rval(6)
             call prterr(lundia, 'G051', 'Number of pivot points to convert wind speed to wind drag coef.: 3')
          endif
          !
          ! check that w2 <= w4 <= w6
          ! and if w2=w4 (w4=w6) then w1=w3 (w3=w5)
          ! error otherwise
          !
          if (comparereal(wstcof(2),wstcof(4)) == 0) then
             if (comparereal(wstcof(1),wstcof(3)) /= 0) then
                write(message,'(2a,f6.2,a,f6.2,2a,f8.5,a,f8.5,a)') "Discontinuity in wind stress as function of wind speed:", &
                     & "'Wstres' coeff. '2' (", wstcof(2), " m/s), is equal to coeff. '4' (", wstcof(4), " m/s), ", &
                     & "while coeff. '1' (", wstcof(1), "), is not equal to coeff. '3' (", wstcof(3), ")"
                call prterr(lundia, 'P004', trim(message))
                error = .true.
             else
                wstcof(2) =  wstcof(2) - 1.0_fp
             endif
          elseif (comparereal(wstcof(2),wstcof(4)) == 1) then
             write(message,'(a,f6.2,a,f6.2,a)') "'Wstres' coefficient '2' (", wstcof(2), &
                  & " m/s), must be smaller than or equal to coefficient '4' (", wstcof(4), " m/s)"
             call prterr(lundia, 'P004', trim(message))
             error = .true.
          endif
          if (comparereal(wstcof(4),wstcof(6)) == 0) then
             if (comparereal(wstcof(3),wstcof(5)) /= 0) then
                write(message,'(2a,f6.2,a,f6.2,2a,f8.5,a,f8.5,a)') "Discontinuity in wind stress as function of wind speed:", &
                     & "'Wstres' coeff. '4' (", wstcof(4), " m/s), is equal to coeff. '6' (", wstcof(6), " m/s), ", &
                     & "while coeff. '3' (", wstcof(3), "), is not equal to coeff. '5' (", wstcof(5), ")"
                call prterr(lundia, 'P004', trim(message))
                error = .true.
             else
                wstcof(6) =  wstcof(6) + 1.0_fp
             endif
          elseif (comparereal(wstcof(4),wstcof(6)) == 1) then
             write(message,'(a,f6.2,a,f6.2,a)') "'Wstres' coefficient '4' (", wstcof(4), &
                  & " m/s), must be smaller than or equal to coefficient '6' (", wstcof(6), " m/s)"
             call prterr(lundia, 'P004', trim(message))
             error = .true.
          endif
       endif
       !
       ! locate and read 'Rhoa  ' air density
       ! default value allowed => defaul
       !
       keyw = 'Rhoa  '
       ntrec = nrrec
       nlook = 1
       rdef = rhoa
       call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          rhoa = rdef
       else
          rhoa = rval(1)
       endif
    endif
    !
    ! process secondary flow
    !
    if (secflo) then
       !
       ! locate and read 'Betac' coupling coefficient
       ! default value allowed => defaul
       !
       keyw = 'Betac '
       ntrec = nrrec
       nlook = 1
       rdef = betac
       call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          betac = rdef
       else
          betac = rval(1)
       endif
       !
       ! locate and read 'Equili' equilibrium or advection and diffusion
       ! default = no equilibrium ('N') which means lsec = 1
       !
       keyw = 'Equili'
       ntrec = nrrec
       lenc = 1
       nlook = 0
       cdef = equili
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          equili = cdef
       else
          equili = cdef
          if (chulp=='Y' .or. chulp=='y') equili = 'Y'
       endif
       !
       ! define lsec depending of value of equili
       !
       lsec = 1
       if (equili=='Y') lsec = 2
    endif
    !
    ! locate and read 'RTCmod' run Flow together with RTC
    ! default = no ('N') which means RTCMOD = noRTC (= 0)
    !
    lhelp = .false.
    call prop_get(gdp%mdfile_ptr, '*', 'Rtcmod', lhelp)
    if (lhelp) then
       rtcmod = ibset(rtcmod,dataFromRTCToFLOW)
    endif
    !
    ! locate and read 'Waqmod' run Flow together with Couple
    ! default = no ('N') which means COUPLEMOD = .false.
    !
    lhelp = .false.
    keyw = 'Waqmod'
    ntrec = nrrec
    lkw = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       lenc = 1
       nlook = 1
       cdef = 'N'
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
       else
          !
          ! define LHELP(COUPLEMOD) if CHULP = Y/y
          !
          if (chulp=='Y' .or. chulp=='y') couplemod = .true.
       endif
    endif
    !  
    if (nonhyd) then
       if (nh_level == nh_weak) then
          call prterr(lundia, 'G051', 'Non-Hydrostatic calculation: Weak formulation (Bijvelds)')
       elseif (nh_level == nh_full) then
          call prterr(lundia, 'G051', 'Non-Hydrostatic calculation: Full formulation (Borsboom/Ullmann)')
       else
          write(message,'(a,i0)') 'Non-Hydrostatic calculation: Unexpected nh_level ', nh_level
          call prterr(lundia, 'P004', trim(message))
       endif
       !
       ! Read 'Nharea'
       !
       ival(1) = 0
       call prop_get(gdp%mdfile_ptr, '*', 'Nharea', ival, 4)
       if (ival(1) == 0) then
          !
          ! No non_hyd area specified
          ! Use complete domain and generate a warning
          !
          m1_nhy = 1
          n1_nhy = 1
          m2_nhy = mmax
          n2_nhy = nmaxus
          call prterr(lundia    ,'Z014'    ,' '       )
       else
          m1_nhy = ival(1)
          n1_nhy = ival(2)
          m2_nhy = ival(3)
          n2_nhy = ival(4)
          !
          ! Check values
          !
          ival(1) = min(max(1, m1_nhy), mmax)
          ival(3) = min(max(1, m2_nhy), mmax)
          ival(2) = min(max(1, n1_nhy), nmaxus)
          ival(4) = min(max(1, n2_nhy), nmaxus)
          if (m1_nhy/=ival(1) .or. m2_nhy/=ival(3) .or. n1_nhy/=ival(2) .or.    &
            & n2_nhy/=ival(4)) then
             error = .true.
             call prterr(lundia    ,'Z015'    ,' '       )
             goto 9999
          endif
          if (m1_nhy>m2_nhy .or. n1_nhy>n2_nhy) then
             ival(1) = m1_nhy
             ival(3) = m2_nhy
             ival(2) = n1_nhy
             ival(4) = n2_nhy
             m1_nhy = min(ival(1), ival(3))
             m2_nhy = max(ival(1), ival(3))
             n1_nhy = min(ival(2), ival(4))
             n2_nhy = max(ival(2), ival(4))
          endif
          if (m1_nhy==m2_nhy .or. n1_nhy==n2_nhy) then
             !
             ! Warning when non-hyd domain is one grid line
             !
             call prterr(lundia    ,'Z016'    ,' '       )
          endif
       endif
       !
       ! locate and read 'Nhiter'
       ! only if NONHYD = .TRUE.
       ! default value allowed => 50
       !
       keyw = 'Nhiter'
       ntrec = nrrec
       nlook = 0
       idef = 50
       call read2i(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,ival      ,idef      ,defaul    ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          nhiter = idef
          call prterr(lundia    ,'Z018'    ,' '       )
       else
          nhiter = ival(1)
          !
          ! Check value
          !
          if (nhiter>=500 .or. nhiter<=5) then
             write (errmsg, '(a6,i6,a1)') '(read:',nhiter,')'
             call prterr(lundia    ,'Z018'    ,trim(errmsg)    )
          endif
       endif
       !
       ! locate and read 'L2norm'
       ! only if NONHYD = .TRUE.
       ! default value allowed => 2
       !
       keyw = 'L2norm'
       ntrec = nrrec
       nlook = 0
       cdef = 'Y'
       lenc = 1
       l2norm = .true.
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,chulp     ,cdef      ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          call prterr(lundia    ,'Z019'    ,' '       )
       else
          !
          ! define L2NORM if CHULP = Y/y
          !
          if (chulp=='N' .or. chulp=='n') l2norm = .false.
       endif
       !
       sprval = -999.999_sp
       call prop_get(gdp%mdfile_ptr,'*','Epsnh',sprval)
       if (sprval >= 0.0_sp) then
          epsnh = real(sprval,fp)
       else
          epsnh = 1.0e-4_fp
       endif
       write (lundia,*) ' Epsnh     = ', epsnh
       !
       sprval = -999.999_sp
       call prop_get(gdp%mdfile_ptr,'*','Repsnh',sprval)
       if ( sprval >= 0.0_sp) then
          rel_epsnh = real(sprval,fp)
       else
          rel_epsnh = 1.0e-4_fp
       endif
       write (lundia,*) ' Rel_epsnh = ', rel_epsnh
       !
       sprval = -999.999_sp
       call prop_get(gdp%mdfile_ptr,'*','Tetaq',sprval)
       if ( sprval >= 0.0_sp) then
          tetaq = real(sprval,fp)
       else
          tetaq = 1.0_fp
       endif
       write (lundia,*) ' Tetaq     = ', tetaq
       !
       sprval = -999.999_sp
       call prop_get(gdp%mdfile_ptr,'*','Tetaz',sprval)
       if ( sprval >= 0.0_sp) then
          tetaz = real(sprval,fp)
       else
          tetaz = 1.0_fp
       endif
       write (lundia,*) ' Tetaz     = ', tetaz
       !
       flag_pp = .false.
       call prop_get(gdp%mdfile_ptr,'*','Flagpp',flag_pp)
       write (lundia,*) ' Flagpp    = ', flag_pp
       !
       updwl = ' '
       call prop_get(gdp%mdfile_ptr,'*','Updwl',updwl)
       call small(updwl,999)
       if ( (updwl /= 'conteq') .and.  (updwl /= 'momcor') ) then
          write (lundia,*) 'WARNING: wrong input for Updwl:', trim(updwl)
          updwl = 'conteq'
       endif
       write (lundia,*) ' Updwl     = ', updwl
       !
       precon = ' '
       call prop_get(gdp%mdfile_ptr,'*','Precon',precon)
       call small(precon,999)
       if ( (precon /= 'none') .and.  (precon /= 'diag') .and. &
          & (precon /= 'tridiag') .and.  (precon /= 'ilu') ) then
          write (lundia,*) 'WARNING: wrong input for Precon:', trim(precon)
          precon = 'tridiag'
       endif
       write (lundia,*) ' Precon    = ', precon
       !
       krylov = ' '
       call prop_get(gdp%mdfile_ptr,'*','Krylov',krylov)
       call small(krylov,999)
       if ( (krylov /= 'cg') .and.  (krylov /= 'bicgstab') ) then
          write (lundia,*) 'WARNING: wrong input for Krylov'
          krylov = 'cg'
       endif
       write (lundia,*) ' Krylov    = ', krylov
       !
       sprval = -999.999_sp
       call prop_get(gdp%mdfile_ptr,'*','Milu',sprval)
       if ( sprval >= 0.0_sp) then
          milu = real(sprval,fp)
       else
          milu = 0.95_fp
       endif
       write (lundia,*) ' Milu      = ', milu
    endif
    !
    ! read interval for update frequency nodal factors;
    ! default is 6 hours (=21600 seconds)
    !
    ti_nodal = 21600.0_fp / tunit
    call prop_get(gdp%mdfile_ptr, '*', 'NodalT', ti_nodal)
    if (comparereal(ti_nodal,21600.0_fp/tunit) /= 0) then
       write (message,'(a,e12.4,3a)') 'Updating tidal node factors every ', ti_nodal, ' [',trim(tunitstr),']'
       call prterr(lundia, 'G051', trim(message))
    endif
    !
    ! Flag to activate XBeach wave driver
    !
    call prop_get(gdp%mdfile_ptr, '*', 'XBeach', xbeach)
    !
    ! Flag to switch on modification of near-bed layering for smoother bottom shear stress representation
    ! (used in Z-model only)
    !
    call prop_get(gdp%mdfile_ptr, '*', 'Ztbml', ztbml)
    if (ztbml) then
       write (message,'(a)') 'Found Keyword Ztbml = #Y#: modifying near-bed layering to obtain '
       call prterr(lundia, 'G051', trim(message))
       write (lundia, '(a)') '            smooth bottom shear stress representation (Z-model only)'
    endif
    !
    call prop_get(gdp%mdfile_ptr, '*', 'SleepDuringWave', sleepduringwave)
 9999 continue
end subroutine rdproc
