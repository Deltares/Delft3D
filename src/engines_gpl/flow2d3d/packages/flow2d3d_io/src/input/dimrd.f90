subroutine dimrd(lunmd     ,lundia    ,error     ,runid     ,nrver     , &
               & prgnm     ,wind      ,salin     ,temp      ,sedim     , &
               & const     ,secflo    ,drogue    ,wave      ,iweflg    , &
               & htur2d    ,mudlay    , &
               & flmd2d    ,zmodel    ,nonhyd    ,roller    ,wavcmp    , &
               & culvert   ,dredge    ,cdwstruct ,snelli    ,cnstwv    , &
               & veg3d     ,waveol    ,lrdamp    ,sbkol     ,bubble    , &
               & nfl       ,nflmod    ,lfsdu     ,lfsdus1   ,gdp       )
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
!    Function: - Initialises the grid and process
!                parameters
!              - Reads the processes from the MD-file
!                and sets the relevant logical flags
!              - Reads the dimensions of the grid from
!                the MD-file
!              - Reads the type of vertical co-ordinate system
!              - Reads the open boundary definitions
!                and counts the Nr. of open boundaries
!                (NOB)
!              - Counts the number of frequencies used
!                to describe the open boundary condi-
!                tions (KC)
!              - Counts number of stations and cross-
!                sections (NOSTAT & NTRUV)
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
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: ncmax
    integer , pointer :: nmax
    integer , pointer :: mmax
    integer , pointer :: nmaxus
    integer , pointer :: kmax
    integer , pointer :: lmax
    integer , pointer :: lsts
    integer , pointer :: lstsc
    integer , pointer :: lstsci
    integer , pointer :: lsal
    integer , pointer :: lsed
    integer , pointer :: lsedtot
    integer , pointer :: ltem
    integer , pointer :: lsecfl
    integer , pointer :: lsec
    integer , pointer :: ltur
    integer , pointer :: ltur2d
    integer , pointer :: kmxdt
    integer , pointer :: npiwe
    integer , pointer :: nbub
    integer , pointer :: nxbub
    integer , pointer :: nto
    integer , pointer :: ntof
    integer , pointer :: ntoq
    integer , pointer :: ntot
    integer , pointer :: kc
    integer , pointer :: nsrc
    integer , pointer :: nsrcd
    integer , pointer :: nostat
    integer , pointer :: ntruv
    integer , pointer :: nofou
    integer , pointer :: ndro
    integer , pointer :: nsluv
    integer , pointer :: iis
    integer , pointer :: ifis
    integer , pointer :: itis
    integer , pointer :: nh_level
!
! Global variables
!
    integer                      :: lundia    !  Description and declaration in inout.igs
    integer                      :: lunmd     !  Description and declaration in inout.igs
    integer                      :: nrver     !!  Integer representative of versio
    logical                      :: bubble    !  Description and declaration in procs.igs
    logical                      :: cdwstruct !  Description and declaration in procs.igs
    logical                      :: const     !  Description and declaration in procs.igs
    logical                      :: culvert   !  Description and declaration in procs.igs    
    logical                      :: flmd2d    !  Description and declaration in procs.igs
    logical                      :: veg3d     !  Description and declaration in procs.igs
    logical                      :: dredge    !  Description and declaration in procs.igs
    logical                      :: snelli    !  Description and declaration in procs.igs
    logical                      :: cnstwv    !  Description and declaration in procs.igs
    logical                      :: drogue    !  Description and declaration in procs.igs
    logical                      :: error     !!  Flag=TRUE if an error is encountered
    logical        , intent(out) :: htur2d    !  Description and declaration in procs.igs
    logical                      :: iweflg    !  Description and declaration in procs.igs
    logical        , intent(out) :: lrdamp
    logical        , intent(out) :: lfsdu
    logical        , intent(out) :: lfsdus1
    logical                      :: mudlay    !  Description and declaration in procs.igs
    logical        , intent(out) :: nonhyd
    logical                      :: roller
    logical                      :: salin     !  Description and declaration in procs.igs
    logical                      :: secflo    !  Description and declaration in procs.igs
    logical                      :: sedim     !  Description and declaration in procs.igs
    logical                      :: temp      !  Description and declaration in procs.igs
    logical                      :: wave      !  Description and declaration in procs.igs
    integer                      :: waveol    !  Description and declaration in procs.igs
    logical                      :: wavcmp
    logical                      :: wind      !  Description and declaration in procs.igs
    logical        , intent(out) :: zmodel    !  Description and declaration in procs.igs
    logical                      :: sbkol     !  Description and declaration in procs.igs
    logical                      :: nfl       !  Description and declaration in procs.igs
    character(256)               :: nflmod    !  Near field model to apply
    character(*)                 :: runid     !!  Run identification code for the current simulation (used to determine
                                              !!  the names of the in- /output files used by the system)
    character(6)   , intent(in)  :: prgnm     !!  Help var. determining the prog. name currently active
!
! Local variables
!
    integer                                  :: idef   ! Default value for integer parameters 
    integer                                  :: iocond
    integer                                  :: lenc   ! Help variable 
    integer                                  :: lkw    ! Length of KEYWRD (max 6 characters) 
    integer                                  :: lrid   ! Length of character string runid 
    integer                                  :: luntmp ! Unit number of FILTMP 
    integer                                  :: ncdwfl ! Nr. of cdw structures
    integer                                  :: nlook  ! Nr. of values to look for in a record 
    integer                                  :: nrrec  ! Record counter keeping the track of the last record read 
    integer                                  :: ntrec  ! Current record counter. It's value is changed to detect if all records in the MD-file have been read 
    integer        , dimension(3)            :: iarray ! Help array 
    integer                       , external :: newlun
    logical                                  :: defaul ! Flag to detrmine if a default value is allowed when no value is read 
    logical                                  :: found  ! Flag is true if KEYWRD is found 
    logical                                  :: lexist ! Flag is true if bch-file exists
    logical                                  :: lerror
    logical                                  :: newkw  ! Flag to specify if the keyword to look for is a new keyword 
    logical                                  :: nodef  ! Flag to specify that no default is allowed if a value can not be found 
    character(1)                             :: ascon
    character(1)                             :: cdef
    character(1)                             :: chulp
    character(12)                            :: tkehlp
    character(12)                            :: tkemod ! Help var. to determine the specified turbulence closure model (only used if KMAX > 1) 
    character(13)                            :: q2ehlp
    character(13)                            :: q2emod
    character(256)                           :: filtmp ! General variable for attribute file name 
    character(256)                           :: filbar ! Name of file with barrier input
    character(256)                           :: filbch ! File name for the fourier openings file
    character(256)                           :: filbub ! Name of file with barrier input
    character(256)                           :: filcdw ! Name of file with fixed gate/cdw input
    character(256)                           :: fixid  ! fixed size version of runid, needed for character concatenation 
    character(300)                           :: mdfrec ! Record read from the MD-file 300 = 256 + a bit (field, =, ##, etc.) 
    character(6)                             :: keyw   ! Keyword to look for in the MD-file
    character(256)                           :: stringval
!
!! executable statements -------------------------------------------------------
!
    ncmax     => gdp%d%ncmax
    nmax      => gdp%d%nmax
    mmax      => gdp%d%mmax
    nmaxus    => gdp%d%nmaxus
    kmax      => gdp%d%kmax
    lmax      => gdp%d%lmax
    lsts      => gdp%d%lsts
    lstsc     => gdp%d%lstsc
    lstsci    => gdp%d%lstsci
    lsal      => gdp%d%lsal
    lsed      => gdp%d%lsed
    lsedtot   => gdp%d%lsedtot
    ltem      => gdp%d%ltem
    lsecfl    => gdp%d%lsecfl
    lsec      => gdp%d%lsec
    ltur      => gdp%d%ltur
    ltur2d    => gdp%d%ltur2d
    kmxdt     => gdp%d%kmxdt
    npiwe     => gdp%d%npiwe
    nbub      => gdp%d%nbub
    nxbub     => gdp%d%nxbub
    nto       => gdp%d%nto
    ntof      => gdp%d%ntof
    ntoq      => gdp%d%ntoq
    ntot      => gdp%d%ntot
    kc        => gdp%d%kc
    nsrc      => gdp%d%nsrc
    nsrcd     => gdp%d%nsrcd
    nostat    => gdp%d%nostat
    ntruv     => gdp%d%ntruv
    nofou     => gdp%d%nofou
    ndro      => gdp%d%ndro
    nsluv     => gdp%d%nsluv
    iis       => gdp%gdrdpara%iis
    ifis      => gdp%gdrdpara%ifis
    itis      => gdp%gdrdpara%itis
    nh_level  => gdp%gdnonhyd%nh_level
    !
    ! initialize local parameters
    !
    mdfrec = 'Starting ...'
    luntmp = 88
    filtmp = ' '
    idef   = 0
    nlook  = 1
    newkw  = .true.
    defaul = .true.
    nodef  = .not.defaul
    tkemod = ' '
    tkehlp = ' '
    q2emod = ' '
    q2ehlp = ' '
    nflmod = ' '
    !
    ! define length of runid and put in fixed size array
    ! size is tested in iniid
    !
    call remove_leading_spaces(runid     ,lrid      )
    fixid(1:lrid) = runid(1:lrid)
    !
    ! calculate = sign
    ! if end of file encountered (not ok first record!)
    ! if error occurred (not ok)
    !
    read (lunmd, '(a300)', iostat = iocond) mdfrec
    if (iocond /= 0) then
       if (iocond < 0) then
          call prterr(lundia    ,'G006'    ,'Md-file' )
       else
          call prterr(lundia    ,'G007'    ,'Md-file' )
       endif
       write (lundia, '(a,a)') 'RECORD: ', mdfrec(:72)
       error = .true.
       goto 9999
    endif
    iis = index(mdfrec, '=')
    if (iis == 0) then
       call prterr(lundia    ,'G007'    ,'Md-file' )
       write (lundia, '(a,a)') 'RECORD: ', mdfrec(:72)
       error = .true.
       goto 9999
    endif
    itis  = iis - 1
    ifis  = iis + 1
    nrrec = 1
    !
    ! Read version number of first record assuming first record
    ! contains keywrd <Ident>
    !
    call vermdf(mdfrec    ,nrver     ,lundia    ,error     ,gdp       )
    if (error) goto 9999
    !
    ! Read Process information and set relevant flags.
    !
    lsal    = 0
    lsecfl  = 0
    lsed    = 0
    lsedtot = 0
    lsts    = 0
    lstsc   = 0
    lstsci  = 0
    ltem    = 0
    ncmax   = 0
    bubble  = .false.
    cnstwv  = .false.
    const   = .false.
    veg3d   = .false.
    dredge  = .false.
    drogue  = .false.
    lfsdu   = .false.
    lfsdus1 = .false.
    lrdamp  = .false.
    nonhyd  = .false.
    roller  = .false.
    salin   = .false.
    secflo  = .false.
    sedim   = .false.
    snelli  = .false.
    temp    = .false.
    wavcmp  = .false.
    wave    = .false.
    waveol  = 0
    wind    = .false.
    zmodel  = .false.
    sbkol   = .false.
    nfl     = .false.
    !
    call dimpro(lunmd     ,lundia    ,error     ,nrrec     ,lsts      , &
              & lstsc     ,lstsci    ,lsal      ,ltem      ,lsed      , &
              & lsedtot   ,lsecfl    ,salin     ,temp      ,sedim     , &
              & const     ,secflo    ,wind      ,drogue    ,wave      , &
              & mudlay    ,flmd2d    ,roller    ,wavcmp    , &
              & ncmax     ,culvert   ,dredge    ,filbar    ,filcdw    , &
              & snelli    ,cnstwv    ,veg3d     ,waveol    ,filbub    , &
              & lrdamp    ,sbkol     ,bubble    ,nfl       ,nflmod    , &
              & prgnm     ,lfsdu     ,lfsdus1   ,gdp       )
    if (error) goto 9999
    !
    ! read MMAX, NMAX, KMAX
    !
    ! locate 'MNKmax' record for mmax, nmax and kmax
    ! read MMAX, NMAX, KMAX from record
    ! default value not allowed (MMAX,NMAX and KMAX must always be > 0)
    ! NMAX must always be odd, NMAXUS is used to identify the user
    ! defined NMAX
    !
    iarray = 0
    call prop_get_integers(gdp%mdfile_ptr, '*', 'MNKmax', iarray, 3)
    if (iarray(1) == 0 .or. iarray(2) == 0 .or. iarray(3) == 0) then
       error = .true.
       call prterr(lundia, 'P004', ' on reading grid dimensions')
       goto 9999
    endif
    mmax   = iarray(1)
    nmax   = iarray(2)
    nmaxus = iarray(2)
    kmax   = iarray(3)
    if (mmax<1 .or. nmax<1 .or. kmax<1) then
       error = .true.
       call prterr(lundia    ,'V001'    ,'Md-file' )
       goto 9999
    endif
    if (mod(nmax, 2)==0) then
       nmax = nmax + 1
    endif
    !
    ! If secondary flow and KMAX > 1 then inconsistency
    !
    if (secflo .and. kmax>1) then
       error = .true.
       call prterr(lundia    ,'U050'    ,'Secondary Flow'     )
       goto 9999
    endif
    !
    ! calculate NSRCD
    !
    nsrcd = 0
    call dimdis(lunmd     ,lundia    ,error     ,nrrec     ,nsrcd     , &
              & gdp       )
    if (error) goto 9999
    !
    ! calculate NXBUB and NBUB
    !
    nxbub = 0
    call dimbub(error, gdp)
    if (error) goto 9999
    !
    ! calculate NSRC (= NSRCD + NBUB)
    ! this line is also in dimbub (needed there for allocation)
    !
    nsrc = nsrcd + nbub - nxbub
    !
    ! calculate NTO
    !
    nto    = 0
    ntof   = 0
    ntoq   = 0
    ntot   = 0
    ascon  = 'N'
    filtmp = ' '
    call dimbnd(lunmd     ,lundia    ,error     ,nrrec     ,filtmp    , &
              & nto       ,ntof      ,ntoq      ,ntot      ,ascon     , &
              & gdp       )
    if (error) goto 9999
    !
    ! calculate KC if NTOF > 0
    !
    kc = 0
    if (ntof > 0) then
       if (prgnm /= 'tdatom') then
          if (ascon == 'Y') then
              !
              ! the astronomical components are no longer written to the file
              ! TMP_//runid//.bch, so we have to calculate KC
              !
              call dimbch(lunmd     ,lundia    ,error     ,nrrec     ,kc        , &
                        & gdp       )
              if (error) goto 9999
          else
              !
              ! Starting from version 2.40 the harmonic components are given
              ! in the file TMP_//runid//.bch preceded by the total number
              ! of KC unless the program is TDATOM.
              ! TDATOM generates this file and therefor it can not be read
              !
              filbch = 'TMP_' // fixid(1:lrid) // '.bch'
              inquire (file = filbch, exist = lexist)
              if (lexist) then
                  open (newunit=luntmp, file = filbch, form = 'unformatted', status = 'old')
                  read (luntmp) kc
                  close (luntmp)
              else
                  write (*,*) 'file ', filbch, ' with number of components not found.'
                  error = .true.
                  goto 9999
              endif
          endif
       !
       ! For TDATOM the components need to be read in case ascon = 'N'
       !
       elseif (ascon == 'N') then
          !
          ! calculate KC
          !
          call dimbch(lunmd     ,lundia    ,error     ,nrrec     ,kc        , &
                    & gdp       )
          if (error) goto 9999
       else
       endif
    endif
    !
    ! locate 'Tkemod' record for the specification of the 3D turb. model
    ! default value allowed = 'Algebraic ' (version 2.03 upwards)
    !
    ltur = 0
    !
    if (kmax > 1) then
       keyw  = 'Tkemod'
       newkw = .true.
       ntrec = nrrec
       nlook = 1
       lenc  = 12
       lkw   = 6
       call search(lunmd     ,error     ,newkw     ,nrrec     ,found     , &
                 & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                 & 'NO'      )
       if (found) then
          call read2c(lunmd     ,error     ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,tkemod    ,tkehlp    ,lenc      ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          if (error) goto 9999
          call small(tkemod    ,12        )
          if (tkemod == 'k-l         ') then
             ltur = 1
          elseif (tkemod == 'k-epsilon   ') then
             ltur = 2
          elseif (tkemod/='algebraic   ' .and. tkemod/='constant    ') then
             call prterr(lundia    ,'V047'    ,' '       )
          else
          endif
       endif
    endif
    lmax = lstsci + ltur
    !
    ! locate 'Q2emod' record for the specification of the 2D turb. model
    ! default value allowed = 'Constant ' (version 3.05 upwards)
    !
    ltur2d = 0
    keyw   = 'Q2emod'
    newkw  = .true.
    ntrec  = nrrec
    nlook  = 1
    lenc   = 13
    lkw    = 6
    call search(lunmd     ,error     ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    if (found) then
       call read2c(lunmd     ,error     ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,q2emod    ,q2ehlp    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       if (error) goto 9999
       call small(q2emod    ,13        )
       if (q2emod == 'constant     ') then
          ltur2d = 0
       elseif (q2emod == 'uittenbogaard') then
          ltur2d = 1
       else
          call prterr(lundia    ,'V247'    ,' '       )
       endif
    endif
    !
    ! Sigma co-ordinate or fixed coordinate (Z-model)
    !
    zmodel = .false.
    keyw   = 'Zmodel'
    ntrec  = nrrec
    lkw    = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! found ?
    !
    if (found) then
       lenc  = 1
       nlook = 1
       cdef  = 'N'
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
          ! define zmodel if CHULP = Y/y
          !
          if (chulp=='Y' .or. chulp=='y') zmodel = .true.
       endif
    endif
    !
    ! HLES horizontal viscosity calculation?
    !
    htur2d = .false.
    keyw   = 'Htur2d'
    ntrec  = nrrec
    lkw    = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! found ?
    !
    if (found) then
       lenc  = 1
       nlook = 1
       cdef  = 'N'
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
          ! define htur2d if CHULP = Y/y
          !
          if (chulp=='Y' .or. chulp=='y') htur2d = .true.
       endif
    endif
    !
    ! calculate NOSTAT
    !
    nostat = 0
    keyw   = 'Filsta'
    call dimsit(lunmd     ,lundia    ,error     ,nrrec     ,nostat    , &
              & keyw      ,gdp       )
    if (error) goto 9999
    !
    ! calculate NTRUV
    !
    ntruv = 0
    keyw  = 'Filcrs'
    call dimsit(lunmd     ,lundia    ,error     ,nrrec     ,ntruv     , &
              & keyw      ,gdp       )
    if (error) goto 9999
    !
    ! calculate NDRO
    !
    ndro = 0
    if (drogue) then
       !
       ! Check whether keyw is 'Fildro' or 'Filpar'
       !
       keyw  = 'Fildro'
       ntrec = nrrec
       nlook = 1
       lkw   = 6
       lenc  = 12
       call search(lunmd     ,error     ,newkw     ,nrrec     ,found     , &
                 & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                 & 'NO'      )
       if (.not.found) then
          !
          ! FLOW-IP does not yet support 'Fildro', but 'Filpar'
          !
          keyw = 'Filpar'
       endif
       call dimsit(lunmd     ,lundia    ,error     ,nrrec     ,ndro      , &
                 & keyw      ,gdp       )
       if (error) goto 9999
       if (ndro==0) drogue = .false.
    endif
    !
    ! calculate NOFOU
    !
    nofou  = 0
    filtmp = ' '
    call dimfou(lunmd     ,lundia    ,error     ,nrrec     ,filtmp    , &
              & nofou     ,gdp       )
    !
    ! Read IWE dimensions and re-define flag IWEFLG
    !
    kmxdt = 1
    npiwe = 1
    !
    if (ltur > 1) then
       call dimiwe(lunmd     ,lundia    ,error     ,nrrec     ,iweflg    , &
                 & kmxdt     ,kmax      ,npiwe     ,gdp       )
    endif
    if (error) goto 9999
    !
    ! Read dimensions for trachytopes
    !
    call dimtrt(lunmd     ,lundia    ,error     ,nrrec     ,gdp       )
    if (error) goto 9999
    !
    !  Read dimensions for barriers
    !
    call dimstr(lunmd     ,filbar    ,lundia    ,error     ,nrrec     , &
              & nsluv     ,gdp       )
    !
    !  Determine flag for CDW structure (0 or 1)
    !
    ncdwfl = 0
    call dimstr(lunmd     ,filcdw    ,lundia    ,error     ,nrrec     , &
              & ncdwfl    ,gdp       )
    if (error) goto 9999
    if (ncdwfl > 0) cdwstruct = .true.

    !
    ! parallel and cdwstruct disabled
    !
    if (cdwstruct .and. parll) then
       call prterr( lundia, 'P004', 'Current deflecting structure is not supported when running parallel.')
       error = .true.
       goto 9999
    endif

    !
    ! Non hydrostatic pressure
    ! default: nonhyd = .false.
    ! old format: nonhyd = true
    ! new format: nonhyd = weak
    !             nonhyd = full
    !
    stringval = ' '
    call prop_get_string(gdp%mdfile_ptr, '*', 'Nonhyd', stringval)
    if (stringval /= ' ') then
       call small(stringval,999)
       if (stringval == 'weak') then
          nonhyd   = .true.
          nh_level = nh_weak
       elseif (stringval == 'full') then
          nonhyd   = .true.
          nh_level = nh_full
       else
          call prop_get_logical(gdp%mdfile_ptr, '*', 'Nonhyd', nonhyd)
          if (nonhyd) then
             nh_level = nh_weak
             write(lundia,'(3a)') 'Keyword "Nonhyd" found in mdf-file with value ', trim(stringval), '. Interpreted as "ON: Weak formulation (Bijvelds)"'
          else
             write(lundia,'(3a)') 'Keyword "Nonhyd" found in mdf-file with value ', trim(stringval), '. Interpreted as "OFF"'
          endif
       endif
    endif
 9999 continue
end subroutine dimrd
