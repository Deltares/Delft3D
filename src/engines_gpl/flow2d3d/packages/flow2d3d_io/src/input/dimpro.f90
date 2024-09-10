subroutine dimpro(lunmd     ,lundia    ,error     ,nrrec     ,lsts      , &
                & lstsc     ,lstsci    ,lsal      ,ltem      ,lsed      , &
                & lsedtot   ,lsecfl    ,salin     ,temp      ,sedim     , &
                & const     ,secflo    ,wind      ,drogue    ,wave      , &
                & mudlay    ,flmd2d    ,roller    ,wavcmp    , &
                & ncmax     ,culvert   ,dredge    ,filbar    ,filcdw    , &
                & snelli    ,cnstwv    ,veg3d     ,waveol    ,filbub    , &
                & lrdamp    ,sbkol     ,bubble    ,nfl       ,nflmod    , &
                & prgnm     ,lfsdu     ,lfsdus1   ,gdp       )
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
!    Function: Reads the dimensions for processes from the
!              MD-file and sets the relevant logical flags
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use string_module, only: str_lower
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    character(256)                      , pointer :: culverfile
    character(256)                      , pointer :: filvg3d
    character(256)                      , pointer :: dredgefile
    real(fp)                            , pointer :: dco
    real(fp)                            , pointer :: nf_timeout
    logical                             , pointer :: tps_from_com  !  Description and declaration in procs.igs    
    logical                             , pointer :: ubot_from_com !  Description and declaration in procs.igs
    logical                             , pointer :: wlen_from_com !  Description and declaration in procs.igs
	logical                             , pointer :: add_uniqueid
    integer                             , pointer :: numdomains
    integer                             , pointer :: itis
    integer                             , pointer :: rolcorr
    character(256)                      , pointer :: sbkConfigFile
!
! Global variables
!
    integer        , intent(out) :: lsal    !  Description and declaration in dimens.igs
    integer        , intent(out) :: lsecfl  !  Description and declaration in dimens.igs
    integer                      :: lsed    !  Description and declaration in esm_alloc_int.f90
    integer                      :: lsedtot !  Description and declaration in esm_alloc_int.f90
    integer                      :: lsts    !  Description and declaration in dimens.igs
    integer                      :: lstsc   !  Description and declaration in dimens.igs
    integer                      :: lstsci  !  Description and declaration in esm_alloc_int.f90
    integer        , intent(out) :: ltem    !  Description and declaration in dimens.igs
    integer                      :: lundia  !  Description and declaration in inout.igs
    integer                      :: lunmd   !  Description and declaration in inout.igs
    integer        , intent(out) :: ncmax
    integer                      :: nrrec   !! Record counter keeping the track of the last record read
    logical        , intent(out) :: bubble  !  Description and declaration in procs.igs
    logical        , intent(out) :: const   !  Description and declaration in procs.igs
    logical        , intent(out) :: culvert !  Description and declaration in procs.igs   
    logical                      :: flmd2d  !  Description and declaration in procs.igs
    logical        , intent(out) :: veg3d   !  Description and declaration in procs.igs
    logical        , intent(out) :: dredge
    logical        , intent(out) :: drogue  !  Description and declaration in procs.igs
    logical                      :: error   !! Flag=TRUE if an error is encountered
    logical                      :: mudlay  !  Description and declaration in procs.igs
    logical        , intent(out) :: lfsdu   !  Description and declaration in procs.igs
    logical        , intent(out) :: lfsdus1 !  Description and declaration in procs.igs
    logical        , intent(out) :: lrdamp  !  Description and declaration in procs.igs
    logical        , intent(out) :: nfl     !! Flag true if Near field computations are requested
    logical                      :: roller
    logical                      :: cnstwv  !  Description and declaration in procs.igs
    logical        , intent(out) :: salin   !  Description and declaration in procs.igs
    logical                      :: sbkol   !  Description and declaration in procs.igs
    logical                      :: secflo  !  Description and declaration in procs.igs
    logical                      :: sedim   !  Description and declaration in procs.igs
    logical                      :: snelli  !  Description and declaration in procs.igs
    logical        , intent(out) :: temp    !  Description and declaration in procs.igs
    logical        , intent(out) :: wavcmp
    logical        , intent(out) :: wave    !  Description and declaration in procs.igs
    integer        , intent(out) :: waveol  !  Description and declaration in procs.igs
    logical        , intent(out) :: wind    !  Description and declaration in procs.igs
    character(6)   , intent(in)  :: prgnm   !! Help var. determining the prog. name currently active
    character(256)               :: filbar
    character(256)               :: filbub
    character(256)               :: filcdw
    character(256)               :: nflmod
    character(256)               :: filsdu  ! Temporary file name for the subsidence/uplift option   
!
! Local variables
!
    integer                   :: istof  ! Flag to detect if any constituent has been specified 
    integer                   :: lconst ! number of constituents, including sediments
    integer        , external :: newlun
    integer                   :: uw
    logical                   :: lhulp  ! Help variable to read logical from MD-file
    logical                   :: skipuniqueid !< temporary variable for reading the corresponding keyword
    character(20)             :: chulp  ! Help variable to read character from MD-file 
    character(256)            :: filrol
!
!! executable statements -------------------------------------------------------
!
    culverfile        => gdp%gdculver%culverfile
    filvg3d           => gdp%gdveg3d%filvg3d
    dredgefile        => gdp%gddredge%dredgefile
    dco               => gdp%gdnumeco%dco
    nf_timeout        => gdp%gdnfl%nf_timeout
    tps_from_com      => gdp%gdprocs%tps_from_com
    ubot_from_com     => gdp%gdprocs%ubot_from_com
    wlen_from_com     => gdp%gdprocs%wlen_from_com
    numdomains        => gdp%gdprognm%numdomains
    itis              => gdp%gdrdpara%itis
    rolcorr           => gdp%gdbetaro%rolcorr
    sbkConfigFile     => gdp%gdsobek%sbkConfigFile
    add_uniqueid      => gdp%gdnfl%add_uniqueid
    !
    ! calculate LSTSC
    ! locate 'Sub1' record for 'S'alinity, 'T'emperaure, 'I'secondary flow and 'W'ind
    !
    chulp = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'Sub1', chulp)
    !
    ! test for 'S'
    !
    istof = max(index(chulp(:4), 'S') , index(chulp(:4), 's'))
    if (istof /= 0) then
       lstsc = lstsc + 1
       lsts  = lsts + 1
       salin = .true.
       lsal  = lstsc
    endif
    !
    ! test for 'T'
    !
    istof = max(index(chulp(:4), 'T') , index(chulp(:4), 't'))
    if (istof /= 0) then
       lstsc = lstsc + 1
       lsts  = lsts + 1
       temp  = .true.
       ltem  = lstsc
    endif
    !
    ! test for 'I'
    !
    istof = max(index(chulp(:4), 'I') , index(chulp(:4), 'i'))
    if (istof /= 0) then
       secflo = .true.
    endif
    !
    ! test for 'W'
    !
    istof = max(index(chulp(:4), 'W') , index(chulp(:4), 'w'))
    if (istof /= 0) then
       wind = .true.
    endif
    !
    ! locate 'Sub2' record for 'P'articles, 'W'ave, 'C'onstituents
    !
    chulp = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'Sub2', chulp)
    !
    ! test for 'P'
    !
    istof = max(index(chulp(:3), 'P') , index(chulp(:3), 'p'))
    if (istof /= 0) then
       drogue = .true.
    endif
    !
    ! test for 'C' => locate 'Namc ' record
    ! constituent found if record = non-blanks
    !
    istof = max(index(chulp(:3), 'C') , index(chulp(:3), 'c'))
    const = istof /= 0
    !
    ! Determine number of sediments, lsed, and number of constituents (including sediments), lconst
    !
    call dimsedconst(lundia    ,error     ,sedim     ,const     , &
                   & lsed      ,lsedtot   ,lconst    ,gdp       )
    if (error) goto 9999
    lsts  = lsts  + lsed
    lstsc = lstsc + lconst
    !
    ! test for 'W'
    !
    istof = max(index(chulp(:3), 'W') , index(chulp(:3), 'w'))
    if (istof /= 0) then
       wave = .true.
    endif
    !
    ! locate 'WaveOL' for online wave computation parallel to flow, outside MOR
    ! default = no ('N') which means waveol = .false.
    !
    chulp = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'WaveOL', chulp)
    call str_lower(chulp)
    if (chulp == 'mimic') then
        waveol = 1
    else
        lhulp = .false.
        call prop_get(gdp%mdfile_ptr, '*', 'WaveOL', lhulp)
        if (lhulp) then
            waveol = 2
        else
            waveol = 0
        endif
    endif
    !
    ! Determine number for secondary flow (+LSTSCI)
    !
    lstsci = lstsc
    if (secflo) then
       lstsci = lstsci + 1
       lsecfl = lstsci
    endif
    !
    ! Determine background temperature constituents
    !
    call dimbacktemp(lundia    ,lconst    ,lstsci    ,gdp       )
    !
    ! Determine whether the fluidmud process is to be activated and set the process and dio I/O flags
    !
    call dimmud(lunmd     ,lundia    ,error     ,nrrec     ,gdp       )
    !
    ! locate and read 'Roller' run Flow together with Roller Energy
    ! default = no ('N') which means Roller = .false.
    ! rolcorr must be initialized to 2, especially when roller is not switched on
    !
    roller  = .false.
    rolcorr = 2
    call prop_get(gdp%mdfile_ptr, '*', 'Roller', roller)
    !
    if (roller) then
       ncmax  = 0
       wavcmp = .false.
       !
       ! locate 'Filwcm' record in MD-file. Filwcm contains data of wave components
       ! ncmax is used in esm_alloc_real for dimensioning arrays
       !
       filrol = ' '
       call prop_get(gdp%mdfile_ptr, '*', 'Filwcm', filrol)
       if (filrol /= ' ') then
          inquire (file = filrol, exist = wavcmp)
          if (.not. wavcmp) then
             write (lundia, '(3a)') '*** error wave component file ' // &
                                   & trim(filrol) // ' not found'
             error = .true.
             goto 9999
          else
             open (newunit=uw, file = filrol)
             call skipstarlines(uw)
             read (uw, *) ncmax
             close (uw)
          endif
       endif
    endif
    !
    ! locate 'Snellius' run Flow (Wave model no longer required)
    ! default = no ('N') which means snelli = .false.
    !
    snelli = .false.
    call prop_get(gdp%mdfile_ptr, '*', 'Snelli', snelli)
    !
    ! locate 'cnstwv' Constant wave condition set for complete domain (No Wave computation)
    ! default = no ('N') which means cnstwv = .false.
    !
    cnstwv = .false.
    call prop_get(gdp%mdfile_ptr, '*', 'Cnstwv', cnstwv)
    !
    ! locate keyword 'tpscom' for using the smoothed peak wave period TPS
    ! instead of the standard peak wave period TP (both from the COM-file).
    ! Default = .false. 
    !
    tps_from_com = .false.
    call prop_get(gdp%mdfile_ptr, '*', 'tpscom', tps_from_com)
    !
    ! locate keyword 'ubcom' for using the orbital velicity near the bottom UBOT from the COM-file
    ! instead of re-computing UBOT in FLOW based on other wave parameters. 
    ! Default = .false.
    !
    ubot_from_com = .false.
    call prop_get(gdp%mdfile_ptr, '*', 'ubcom', ubot_from_com)
    !
    ! locate keyword 'wlcom' for using the mean wave length WLEN from the COM-file
    ! instead of re-computing WLEN in FLOW based on other wave parameters. 
    ! Default = .false.
    !
    wlen_from_com = .false.
    call prop_get(gdp%mdfile_ptr, '*', 'wlcom', wlen_from_com)
    !
    ! Dredging and Dumping: get file name
    !
    dredgefile = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'Fildad', dredgefile)
    if (dredgefile /= ' ') then
       dredge = .true.
    elseif (prgnm /= 'tdatom') then
       if (numdomains > 1) then
          !
          ! Notify the dredge merge iterator that this subdomain
          ! is not interested in dredge volumes
          ! If numdomains=1, there is no dredge merge iterator
          !
          call no_dd_dredgecommunication ()
       endif
    endif
    !
    ! Culvert
    !
    culverfile = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'Filcul', culverfile)
    if (culverfile /= ' ') then
       culvert = .true.
    endif
    !
    ! Barrier data: get file name
    !
    filbar = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'Filbar', filbar)
    !
    ! Bubble screen data: get file name
    !
    filbub = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'Filbub', filbub)
    if (filbub /= ' ') then
       bubble = .true.
    endif
    !
    ! Near Field computations
    !
    nflmod            = ' '
    nfl               = .false.
    nf_timeout        = huge(nf_timeout)
    gdp%gdnfl%infile  = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'Filnfl', gdp%gdnfl%infile)
    if (gdp%gdnfl%infile /= ' ') then
       nfl    = .true.
       nflmod = 'generic'
       write (lundia, '(2a)') '*** MESSAGE COSUMO config file: ', trim(gdp%gdnfl%infile)
       !
       ! SkipUniqueId read for backward compatibility
       skipuniqueid = .false.
       call prop_get(gdp%mdfile_ptr, '*', 'SkipUniqueId', skipuniqueid)
       add_uniqueid = .not.skipuniqueid
       !
       ! Read preferred keyword
       call prop_get(gdp%mdfile_ptr, '*', 'AddUniqueId', add_uniqueid)
       !
       call prop_get(gdp%mdfile_ptr, '*', 'NfTimeout', nf_timeout)
       if (nf_timeout < huge(nf_timeout)) then
          write (lundia, '(a,f8.1,a)') '*** MESSAGE NfTimeout = ', nf_timeout, ' minutes'
       endif
    else
       add_uniqueid = .false.
    endif
    !
    !
    ! Fixed gates (CDW): get file name
    !
    filcdw = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'Filcdw', filcdw)
    !
    ! (Rigid) 3D Vegetation Model
    !
    filvg3d = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'Filpla', filvg3d)
    if (filvg3d /= ' ') then
       veg3d = .true.
    endif
    !
    ! Low Reynolds damping on viscosity/diffusivity
    !
    lrdamp = .false.
    call prop_get(gdp%mdfile_ptr, '*', 'LRdamp', lrdamp)
    !
    ! Online coupling with Sobek. Note that in the current implementation this
    ! coupling is managed by Sobek user interface.
    ! default = no ('N') which means sbkol = .false.
    ! 
    sbkol         = .false.
    sbkConfigFile = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'SbkOL', sbkConfigFile)
    if (sbkConfigFile /= ' ') then
       sbkol = .true.
    endif
    !
    ! Subsidence/Uplift 
    !
    filsdu = ' '
    lfsdu = .false.
    call prop_get(gdp%mdfile_ptr, '*', 'Filsdu', filsdu)
    if (filsdu /= ' ') then
       lfsdu = .true.
    endif
    !    
    lfsdus1 = .false.
    call prop_get(gdp%mdfile_ptr, '*', 'SduS1', lfsdus1)
    !
 9999 continue
end subroutine dimpro
