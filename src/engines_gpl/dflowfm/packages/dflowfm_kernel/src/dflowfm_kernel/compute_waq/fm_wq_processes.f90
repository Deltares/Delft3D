!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2018-2024.
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

subroutine fm_wq_processes_ini_sub()
    use m_fm_wq_processes
    use m_alloc
    use unstruc_messages
    use m_flow, only: kmx
    use m_flowgeom, only: Ndxi
    use m_flowexternalforcings
    use m_transport
    use m_partitioninfo
    use unstruc_model
    use m_flowparameters, only: jawriteDetailedTimers
    use unstruc_files, only: mdia
    use m_flowtimes
    use timers
    use m_string_utils, only: index_in_array

    use m_wind, only: jawind, jarain

    implicit none

    integer                    :: ierr_sub            !< error status
    integer                    :: ierr_eho            !< error status
    character(256)             :: cerr                !< error message

    ! Other
    integer( 4)              :: nosys_eho, notot_eho, nocons_eho
    integer( 4)              :: i

    integer :: janew, iex, ierr
    integer :: kk, k, kb, kt, ktmax, kdum

    logical :: Lsub, Leho, Lstt, Lpdf, Lopl, Lblm, Lallocated

    integer(4), save         :: ithndl = 0

    call mess(LEVEL_INFO, 'Initialising water quality processes')

    jawriteDetailedTimers = 1
    if (timon) call timstrt( "fm_wq_processes_ini_sub", ithndl )

    if (kmx>0) then
     wqbot3D_output = md_wqbot3D_output
    else
     wqbot3D_output = 0
    end if
    ibflag = 0

    substance_file = md_subfile
    his_output_file = md_ehofile
    proc_def_file = md_pdffile
    proc_dllso_file = md_oplfile
    bloom_file = md_blmfile
    statistics_file = md_sttfile

    ! check if substance file exists
    inquire(file=substance_file,exist=Lsub)
    if ( .not.Lsub) then
     call mess(LEVEL_ERROR, 'Substance file does not exist: ', trim(substance_file))
    end if

    ! check if additional history output file exists
    if (his_output_file /= ' ') then
     inquire(file=his_output_file,exist=Leho)
     if ( .not.Leho) then
        call mess(LEVEL_ERROR, 'Additional history output file specified, but does not exist: ', trim(his_output_file))
     end if
    else
     Leho = .false.
    endif

    ! check if statistics file exists
    if (statistics_file /= ' ') then
     inquire(file=statistics_file,exist=Lstt)
     if ( .not.Lstt) then
        call mess(LEVEL_ERROR, 'Statistics file does not exist: ', trim(statistics_file))
     end if
    end if

    !     check if proc_def file exists
    if (proc_def_file /= ' ') then
     inquire(file=proc_def_file,exist=Lpdf)
     if ( .not.Lpdf) then
        call mess(LEVEL_ERROR, 'Process library file does not exist: ', trim(proc_def_file))
     end if
    else
     call mess(LEVEL_ERROR, 'No process library file specified. Use commandline argument --processlibrary "<path>/<name>"')
    endif

    ! check if open process dll/so file exists
    if (proc_dllso_file /= ' ') then
     inquire(file=proc_dllso_file,exist=Lopl)
     if ( .not.Lopl) then
        call mess(LEVEL_ERROR, 'Open process library dll/so file does not exist: ', trim(proc_dllso_file))
     end if
    endif

    ! check if bloom file exists
    if (bloom_file /= ' ') then
     inquire(file=bloom_file,exist=Lblm)
     if ( .not.Lblm) then
        call mess(LEVEL_ERROR, 'BLOOM species definition file specified, but does not exist: ', trim(bloom_file))
     end if
    end if

    ! water column definition
    if ( kmx > 0 ) then
     call getkbotktopmax(1,kbx,kt,kdum)
     call getkbotktopmax(Ndxi,kdum,kt,ktx)
     noseg = ktx-kbx+1 ! includes D-Flow FM dummy layer
     noq1 = 0
     noq2 = 0
     noq3 = noseg - 2*Ndxi + 1
     noq4 = 0

    ! allocate vertical exchanges array
     call realloc(iexpnt, [4, noq3], keepExisting=.false., fill=0)


    ! set vertical exchanges
     iex = 0
     do kk=1,Ndxi
        call getkbotktopmax(kk,kb,kt,ktmax)
        do k=ktmax,kb+1,-1
           iex = iex+1
           iexpnt(1,iex) = k - kbx+1
           iexpnt(2,iex) = k-1 - kbx+1
        end do
     end do
    else
     kbx = 1
     ktx = Ndxi
     noseg = Ndxi
     noq1 = 0
     noq2 = 0
     noq3 = 0
     noq4 = 0

    ! allocate vertical exchanges array (dummy)
     call realloc(iexpnt, [4, noq3], keepExisting=.false., fill=0)
    end if

    ! allocate array that indicates if processes are active based on volume and depth criteria
    call realloc(wqactive, ktx, keepExisting=.false., fill=.true.)

    ! allocate array that indicates if processes are active based on 'ProcessesInactive' parameter
    call realloc(wqdoproc, ktx, keepExisting=.false., fill=.true.)

    ! allocate iknmrk array used by processes that indicates if processes are active, and position in the watercolumn, dynamically set every timestep
    call realloc(iknmrk, noseg, keepExisting=.false., fill=0)

    ! allocate and fill array that can tell processes if they are in the current domain when running in parallel
    call realloc(wqmydomain, noseg, keepExisting=.false., fill=.true.)
    if(jampi == 1) then
    do kk=1,Ndxi
       call getkbotktopmax(kk,kb,kt,ktmax)
       do k=kb,ktmax
          wqmydomain(k-kbx+1) = idomain(kk) == my_rank
       enddo
    enddo
    endif

    !    store the number of segments and number of layers for access
    !    in the processes library
    call store_noseg_nolay( noseg, kmx )

    ! ======================
    ! Start initialising WAQ
    ! ======================

    !     Read the substance file for the process defintion
    !     Reset number of messages
    ierr=0

    call mess(LEVEL_INFO, 'Opening substance file: ', trim(substance_file))
    call setmlu(mdia)
    Lallocated = .false.

    if ( .not. allocated(syname_sub) ) then
      allocate( syname_sub(0) )
    endif
    if ( .not. allocated(syunit_sub) ) then
      allocate( syunit_sub(0) )
    endif
    if ( .not. allocated(coname_sub) ) then
      allocate( coname_sub(0) )
    endif
    if ( .not. allocated(covalue_sub) ) then
      allocate( covalue_sub(0) )
    endif
    if ( .not. allocated(ouname_sub) ) then
      allocate( ouname_sub(0) )
    endif
    if ( .not. allocated(oudesc_sub) ) then
      allocate( oudesc_sub(0) )
    endif
    if ( .not. allocated(syname_eho) ) then
      allocate( syname_eho(0) )
    endif
    if ( .not. allocated(syunit_eho) ) then
      allocate( syunit_eho(0) )
    endif
    if ( .not. allocated(coname_eho) ) then
      allocate( coname_eho(0) )
    endif
    if ( .not. allocated(covalue_eho) ) then
      allocate( covalue_eho(0) )
    endif
    if ( .not. allocated(ouname_eho) ) then
      allocate( ouname_eho(0) )
    endif
    if ( .not. allocated(oudesc_eho) ) then
      allocate( oudesc_eho(0) )
    endif
    if ( .not. allocated(syname) ) then
      allocate( syname(0) )
    endif
    if ( .not. allocated(coname) ) then
      allocate( coname(0) )
    endif
    if ( .not. allocated(funame) ) then
      allocate( funame(0) )
    endif
    if ( .not. allocated(paname) ) then
      allocate( paname(0) )
    endif
    if ( .not. allocated(sfunname) ) then
      allocate( sfunname(0) )
    endif
    if ( .not. allocated(diname) ) then
      allocate( diname(0) )
    endif
    if ( .not. allocated(vename) ) then
      allocate( vename(0) )
    endif
    if ( .not. allocated(flxdmp) ) then
      allocate( flxdmp(0,0,0) )
    endif
    if ( .not. allocated(idpnew) ) then
      allocate( idpnew(0) )
    endif
    if ( .not. allocated(dispnw) ) then
      allocate( dispnw(0,0) )
    endif
    if ( .not. allocated(dspx) ) then
      allocate( dspx(0) )
    endif
    if ( .not. allocated(dsto) ) then
      allocate( dsto(0) )
    endif
    if ( .not. allocated(velonw) ) then
      allocate( velonw(0,0) )
    endif
    if ( .not. allocated(velx) ) then
      allocate( velx(0,0) )
    endif
    if ( .not. allocated(vsto) ) then
      allocate( vsto(0) )
    endif

    call read_substances(Lallocated,substance_file,nosys,notot,nocons,noout_sub,syname_sub,syunit_sub,coname_sub, &
              covalue_sub,ouname_sub,oudesc_sub,ierr_sub,cerr)
    if (ierr_sub /= 0) call mess(LEVEL_ERROR, cerr)
    call realloc (syname_sub, notot, keepExisting=.false., fill=' ')
    call realloc (syunit_sub, notot, keepExisting=.false., fill=' ')
    call realloc (coname_sub, nocons, keepExisting=.false., fill=' ')
    call realloc (covalue_sub, nocons, keepExisting=.false., fill=0.0e0)
    call realloc (ouname_sub, noout_sub, keepExisting=.false., fill=' ')
    call realloc (oudesc_sub, noout_sub, keepExisting=.false., fill=' ')
    Lallocated = .true.
    call read_substances(Lallocated,substance_file,nosys,notot,nocons,noout_sub,syname_sub,syunit_sub,coname_sub, &
              covalue_sub,ouname_sub,oudesc_sub,ierr_sub,cerr)
    if (ierr_sub /= 0) call mess(LEVEL_ERROR, cerr)
    noout_map = noout_sub

    if (Leho) then
     call mess(LEVEL_INFO, 'Opening extra history output file: ', trim(his_output_file))
     Lallocated = .false.
     call read_substances(Lallocated,his_output_file,nosys_eho,notot_eho,nocons_eho,noout_eho,syname_eho,syunit_eho,coname_eho, &
                 covalue_eho,ouname_eho,oudesc_eho,ierr_eho,cerr)
     if (ierr_eho /= 0) call mess(LEVEL_ERROR, cerr)
     if (nosys_eho==0 .and. notot_eho==0 .and. nocons_eho==0) then
        call realloc (ouname_eho, noout_eho, keepExisting=.false., fill=' ')
        call realloc (oudesc_eho, noout_eho, keepExisting=.false., fill=' ')
        Lallocated = .true.
        call read_substances(Lallocated,his_output_file,nosys_eho,notot_eho,nocons_eho,noout_eho,syname_eho,syunit_eho,coname_eho, &
                    covalue_eho,ouname_eho,oudesc_eho,ierr_eho,cerr)
        if (ierr_eho /= 0) call mess(LEVEL_ERROR, cerr)
     else
        ! Error: extra history outputfile contains other definitions that output alone!
     endif
    else
     ! Warning: extra history outputfile does not exist!
     noout_eho = 0
    end if

    !     The active substances should be initialised as 'constituents' in DFM.
    !     Initial concentration (fields), boundary conditions and additional (waste) loads
    !     should be specified in DFM
    call realloc(syname, notot)
    call realloc(syunit, notot)
    do i = 1, notot
      syname(i) = syname_sub(i)
      syunit(i) = syunit_sub(i)
    end do

    call realloc(amass, [notot, noseg], keepExisting=.false., fill=0.0d0)       !< mass array to be updated

    !     add corresponding tracers and bottom substances, if not already defined by initial and/or boundary conditions
    transformcoef = 0.0_hp
    call realloc(isys2trac,notot,keepExisting=.false.,fill=0)
    do i=1,nosys
     call add_bndtracer(trim(syname_sub(i)), syunit(i), isys2trac(i), janew)
    end do
    call realloc(isys2wqbot,notot,keepExisting=.false.,fill=0)
    do i=nosys+1,notot
     call add_wqbot(trim(syname_sub(i)), syunit(i), isys2wqbot(i), janew)
    end do

    !     Additional  data that comes from DFM should be added to the parameter/function/segment function list before the wq_processes_initialise call

    !     No spatial parameters for now, they should come from DFM
    nopa = 0
    call realloc(paname, nopa)

    !      Use functions to set 2D (or 0D variables) from DFM per column
    nofun = 0
    call realloc(funame, nofun)

    nosfun = 0
    call realloc(sfunname, nofun)

    call dfm_waq_initexternalforcings(ierr)
    if (ierr /= 0) then
     call mess(LEVEL_ERROR, 'Error reading water quality processes external forcings from ext-file')
    endif
    nosfunext = nosfun

    jawaqproc = 1 ! substances succesfully initiated

    if ( timon ) call timstop ( ithndl )
end subroutine fm_wq_processes_ini_sub

subroutine fm_wq_processes_ini_proc()
    use m_fm_wq_processes
    use m_wq_processes_initialise
    use m_wq_processes_pmsa_size
    use bloom_data_vtrans
    use m_alloc
    use unstruc_messages
    use m_flow, only: kmx
    use m_flowgeom, only: Ndxi, ba
    use m_flowparameters, only: jasal, jatem, jawave, jawaveSwartDelwaq
    use m_flowexternalforcings
    use m_transport
    use m_partitioninfo
    use unstruc_model
    use unstruc_files
    use m_flowtimes
    use timers
    use m_wind, only: jawind, jarain
    use date_time_utils, only : compute_reference_day

    implicit none

    type(procespropcoll)     :: statprocesdef   !< the statistical proces definition
    integer  ( 4), parameter :: nomult = 0      !< number of multiple substances
    integer  ( 4)            :: imultp(2,nomult)!< multiple substance administration
    type(t_dlwq_item)        :: constants       !< delwaq constants list
    integer                  :: nocons_used     !< number of constants actually used
    integer                  :: noinfo          !< count of informative message
    integer                  :: nowarn          !< count of warnings
    integer                  :: ierr, ierr2     !< error count

    integer( 4)              :: i, j, ip, icon, ipar, ifun, isfun, ivar
    integer                  :: ipoifmlayer, ipoifmktop, ipoifmkbot
    integer( 4)              :: refdayNr      ! reference day number, varying from 1 till 365

    integer :: iex
    integer :: kk, k, kb, kt, ktmax

    integer :: lunlsp

    integer(4), save         :: ithndl = 0

    character*20,parameter   :: ctauflow = 'tauflow'
    character*20,parameter   :: ctau = 'tau'
    character*20,parameter   :: cvelocity = 'velocity'
    character*20,parameter   :: csalinity = 'salinity'
    character*20,parameter   :: ctemperatureflow = 'tempflow'
    character*20,parameter   :: ctemperature = 'temp'
    character*20,parameter   :: cwind = 'vwind'
    character*20,parameter   :: cwinddir = 'winddir'
    character*20,parameter   :: cfetchl = 'fetch'
    character*20,parameter   :: cfetchd = 'initdepth'
    character*20,parameter   :: cirradiation = 'radsurf'
    character*20,parameter   :: crain = 'rain'
    character*20,parameter   :: cvtrans = 'ACTIVE_vtrans'
    character*20,parameter   :: cvertdisp = 'ACTIVE_vertdisp'
    character*10,parameter   :: cbloom = 'd40blo'
    character*20,parameter   :: cdoprocesses = 'DoProcesses'
    character*20,parameter   :: cprocessesinactive = 'ProcessesInactive'

    character*20,parameter   :: cWaveH = 'WaveHeight'
    character*20,parameter   :: cWaveL = 'WaveLength'
    character*20,parameter   :: cWaveT = 'WavePeriod'

    if (timon) call timstrt( "fm_wq_processes_ini_proc", ithndl )

    !     try to open the lsp-file for logging output
    proc_log_file = defaultfilename('wq_lsp')
    open (newunit=lunlsp , file=proc_log_file, status='unknown', iostat=ierr)
    if (ierr /= 0) then
     call mess(LEVEL_ERROR, 'Could not open processes log file: ', trim(proc_log_file))
    end if
    call setmlu(lunlsp)
    !     Reset number of messages
    noinfo=0
    nowarn=0
    ierr = 0

    !     Use segment functions to set 3D (or 2D variables per column) from DFM (e.g. salinity or temperature)
    call mess(LEVEL_INFO, '==========================================================================')
    call mess(LEVEL_INFO, 'Data from hydrodynamics available for water quality')
    call mess(LEVEL_INFO, '--------------------------------------------------------------------------')
    nosfun = nosfun+1
    isfsurf = nosfun
    call realloc(sfunname, nosfun, keepExisting=.true., fill='surf')
    call mess(LEVEL_INFO, '''horizontal surface'' connected as ''surf'' (by default)')

    icon = index_in_array(ctauflow,coname_sub)
    if (icon>0) then
     nosfun = nosfun+1
     isftau = nosfun
     call realloc(sfunname, nosfun, keepExisting=.true., fill='tauflow')
     call mess(LEVEL_INFO, '''bottom shear stress'' connected as ''tauflow''')
    else
     icon = index_in_array(ctau,coname_sub)
     if (icon>0) then
        nosfun = nosfun+1
        isftau = nosfun
        call realloc(sfunname, nosfun, keepExisting=.true., fill='tau')
        call mess(LEVEL_INFO, '''bottom shear stress'' connected as ''tau''')
     else
        call mess(LEVEL_INFO, '''bottom shear stress'' not connected, because ''tauflow'' or ''tau'' are not in the sub-file.')
        isftau = 0
     endif
    end if
    if (isftau > 0) then
     if (jawaveSwartDelwaq == 0) then
        call mess(LEVEL_INFO, 'jawaveSwartDelwaq == 0 so tau/tauflow = taucur or tau from wave-current interaction if waves activated')
     else if (jawaveSwartDelwaq == 1) then
        call mess(LEVEL_INFO, 'jawaveSwartDelwaq == 1 so tau/tauflow = taucur + ftauw*tauwave')
     else if (jawaveSwartDelwaq == 2) then
        call mess(LEVEL_INFO, 'jawaveSwartDelwaq == 2 so tau/tauflow = taubxu')
     endif
    endif

    icon = index_in_array(cvelocity,coname_sub)
    if (icon>0) then
     nosfun = nosfun+1
     isfvel = nosfun
     call realloc(sfunname, nosfun, keepExisting=.true., fill='velocity')
     call mess(LEVEL_INFO, '''flow element center velocity'' connected as ''velocity''')
    else
     call mess(LEVEL_INFO, '''flow element center velocity'' not connected, because ''velocity'' is not in the sub-file.')
     isfvel = 0
    end if

    icon = index_in_array(cWaveH,coname_sub)
    isfwaveheight = 0
    if (icon>0) then
     nosfun = nosfun+1
     isfwaveheight = nosfun
     call realloc(sfunname, nosfun, keepExisting=.true., fill='WaveHeight')  ! this must be the name used in waq, process input param/in sub file.
     call mess(LEVEL_INFO, '''flow element center WaveHeight'' connected as ''WaveHeight''')
    else
     call mess(LEVEL_INFO, '''flow element center WaveHeight'' not connected, because ''WaveHeight'' is not in the sub-file.')
     isfwaveheight = 0
    end if

    icon = index_in_array(cWaveL,coname_sub)
    isfwavelength = 0
    if (icon>0) then
     nosfun = nosfun+1
     isfwavelength = nosfun
     call realloc(sfunname, nosfun, keepExisting=.true., fill='WaveLength')  ! this must be the name used in waq, process input param/in sub file.
     call mess(LEVEL_INFO, '''flow element center WaveLength'' connected as ''WaveLength''')
    else
     call mess(LEVEL_INFO, '''flow element center WaveLength'' not connected, because ''WaveLength'' is not in the sub-file.')
     isfwavelength = 0
    end if

    icon = index_in_array(cWaveT,coname_sub)
    isfwaveperiod = 0
    if (icon>0) then
     nosfun = nosfun+1
     isfwaveperiod = nosfun
     call realloc(sfunname, nosfun, keepExisting=.true., fill='WavePeriod')  ! this must be the name used in waq, process input param/in sub file.
     call mess(LEVEL_INFO, '''flow element center WavePeriod'' connected as ''WavePeriod''')
    else
     call mess(LEVEL_INFO, '''flow element center WavePeriod'' not connected, because ''WavePeriod'' is not in the sub-file.')
     isfwaveperiod = 0
    end if


    icon = index_in_array(csalinity,coname_sub)
    isfsal = 0
    if ( jasal == 1 ) then
     if (icon>0) then
        nosfun = nosfun+1
        isfsal = nosfun
        call realloc(sfunname, nosfun, keepExisting=.true., fill='salinity')
        call mess(LEVEL_INFO, '''salinity'' connected as ''salinity''')
     else
        call mess(LEVEL_INFO, '''salinity'' not connected, because ''salinity'' is not in the sub-file.')
     end if
    else
     if (icon>0) then
        call mess(LEVEL_INFO, '''salinity'' is the sub-file but ''salinity'' is not in the hydrodynamic model.')
     endif
    end if

    icon = index_in_array(ctemperatureflow,coname_sub)
    isftem = 0
    if ( jatem >= 1 ) then
     if (icon > 0) then
        nosfun = nosfun+1
        isftem = nosfun
        call realloc(sfunname, nosfun, keepExisting=.true., fill='tempflow')
        call mess(LEVEL_INFO, '''temperature'' connected as ''tempflow''')
     else
        icon = index_in_array(ctemperature,coname_sub)
        if (icon>0) then
           nosfun = nosfun+1
           isftem = nosfun
           call realloc(sfunname, nosfun, keepExisting=.true., fill='temp')
           call mess(LEVEL_INFO, '''temperature'' connected as ''temp''')
        else
           call mess(LEVEL_INFO, '''temperature'' not connected, because ''tempflow'' or ''temp'' are not in the sub-file.')
        endif
     end if
    else
     if (icon>0) then
        call mess(LEVEL_INFO, '''tempflow'' is the sub-file but ''temperature'' is not in the hydrodynamic model.')
     endif
     icon = index_in_array(ctemperature,coname_sub)
     if (icon>0) then
        call mess(LEVEL_INFO, '''temp'' is the sub-file but ''temperature'' is not in the hydrodynamic model.')
     endif
    end if

    icon = index_in_array(cwind,coname_sub)
    isfvwind = 0
    if ( jawind >= 1 ) then
     if (icon>0) then
        nosfun = nosfun+1
        isfvwind = nosfun
        call realloc(sfunname, nosfun, keepExisting=.true., fill='vwind')
        call mess(LEVEL_INFO, '''wind velocity magnitude'' connected as ''vwind''')
     else
        call mess(LEVEL_INFO, '''wind velocity magnitude'' not connected, because ''vwind'' is not in the sub-file.')
     end if
    else
     if (icon>0) then
        call mess(LEVEL_INFO, '''vwind'' is the sub-file but ''wind velocity'' is not in the hydrodynamic model.')
     endif
    end if

    icon = index_in_array(cwinddir,coname_sub)
    isfwinddir = 0
    if ( jawind >= 1 ) then
     if (icon>0) then
        nosfun = nosfun+1
        isfwinddir = nosfun
        call realloc(sfunname, nosfun, keepExisting=.true., fill='winddir')
        call mess(LEVEL_INFO, '''wind direction'' connected as ''winddir''')
     else
        call mess(LEVEL_INFO, '''wind direction'' not connected, because ''winddir'' is not in the sub-file.')
     end if
    else
     if (icon>0) then
        call mess(LEVEL_INFO, '''winddir'' is the sub-file but ''wind direction'' is not in the hydrodynamic model.')
     endif
    end if

    icon = index_in_array(cfetchl,coname_sub)
    if (icon==0) then
     icon = index_in_array(cfetchd,coname_sub)
    end if
    isffetchl = 0
    isffetchd = 0
    if ( jawave == 1 .or. jawave == 2 ) then  ! copied from "set_external_forcings", call to "tauwavefetch"
     if (icon>0) then
        nosfun = nosfun+1
        isffetchl = nosfun
        call realloc(sfunname, nosfun, keepExisting=.true., fill='fetch')
        call mess(LEVEL_INFO, '''fetch length'' connected as ''fetch''')
        nosfun = nosfun+1
        isffetchd = nosfun
        call realloc(sfunname, nosfun, keepExisting=.true., fill='initdepth')
        call mess(LEVEL_INFO, '''fetch depth'' connected as ''initdepth''')
     else
        call mess(LEVEL_INFO, '''fetch length'' and ''fetch depth'' not connected, because neither ''fetch'' or ''initdepth'' is in the sub-file.')
     end if
    else
     if (icon>0) then
        call mess(LEVEL_INFO, '''fetch'' or ''initdepth'' is the sub-file but ''fetch length''/''fetch depth'' are not in the hydrodynamic model.')
     endif
    end if

    icon = index_in_array(cirradiation,coname_sub)
    isfradsurf = 0
    if ( jasol == 1 .and. jatem > 1 ) then
     if (icon>0) then
        nosfun = nosfun+1
        isfradsurf = nosfun
        call realloc(sfunname, nosfun, keepExisting=.true., fill='radsurf')
        call mess(LEVEL_INFO, '''solar radiation'' connected as ''radsurf''')
     else
        call mess(LEVEL_INFO, '''solar radiation'' not connected, because ''radsurf'' is not in the sub-file.')
     end if
    else
     if (icon>0) then
        call mess(LEVEL_INFO, '''radsurf'' is the sub-file but ''solar radiation'' is not in the hydrodynamic model.')
     endif
    end if

    icon = index_in_array(crain,coname_sub)
    isfrain = 0
    if ( jarain == 1 ) then
     if (icon>0) then
        nosfun = nosfun+1
        isfrain = nosfun
        call realloc(sfunname, nosfun, keepExisting=.true., fill='rain')
        call mess(LEVEL_INFO, '''rain'' (mm/day) connected as ''rain'' (mm/h)')
     else
        call mess(LEVEL_INFO, '''rain'' not connected, because ''rain'' is not in the sub-file.')
     end if
    else
     if (icon>0) then
        call mess(LEVEL_INFO, '''rain'' is the sub-file but ''rain'' is not in the hydrodynamic model.')
     endif
    end if

    !     data only needed for vtrans
    icon = index_in_array(cvtrans,coname_sub)
    if (icon>0) then
     icon = index_in_array(cvertdisp,coname_sub)
     if (icon<=0) then
        call mess(LEVEL_ERROR, 'The water quality process ''vtrans'' requires the process ''vertdisp'' to be active.')
     else if (kmx==0) then
        call mess(LEVEL_WARN, 'The water quality process ''vtrans'' was not switched on because the model is not 3D.')
     else
        fm_vtrans = .true.
        nosfun = nosfun+1
        isfvertdisper = nosfun
        call realloc(sfunname, nosfun, keepExisting=.true., fill='VertDisper')
        nosfun = nosfun+1
        isffmlayer = nosfun
        call realloc(sfunname, nosfun, keepExisting=.true., fill='FMLayer')
        nosfun = nosfun+1
        isffmktop = nosfun
        call realloc(sfunname, nosfun, keepExisting=.true., fill='FMkTop')
        nosfun = nosfun+1
        isffmkbot = nosfun
        call realloc(sfunname, nosfun, keepExisting=.true., fill='FMkBot')
        call mess(LEVEL_INFO, 'Found process ''vtrans'', added ''VertDisper'', ''FMLayer'', ''FMkTop'' and ''FMkBot''')
     end if
    end if
    call mess(LEVEL_INFO, '--------------------------------------------------------------------------')

    noconm = nocons + 1000
    call realloc(coname, noconm)
    ierr2 = dlwq_init_item(constants)
    ierr2 = dlwq_resize(constants,noconm)

    !     Skip constants from the sub-file that will be added by DFM as parameter/function/segment function
    nocons_used = 0
    do i = 1, nocons
     ipar = index_in_array(coname_sub(i),paname)
     if (ipar>0) then
        call mess(LEVEL_INFO, 'Water quality constant replaced by spatial parameter: ', coname_sub(i))
     endif
     ifun = index_in_array(coname_sub(i),funame)
     if (ifun>0) then
        call mess(LEVEL_INFO, 'Water quality constant replaced by temporal function: ', coname_sub(i))
     endif
     isfun = index_in_array(coname_sub(i),sfunname)
     if (isfun>0) then
        call mess(LEVEL_INFO, 'Water quality constant replaced by segment function: ', coname_sub(i))
     endif
     if (ipar<0 .and. ifun<0 .and. isfun<0) then
        nocons_used = nocons_used + 1
        coname(nocons_used) = coname_sub(i)
        constants%ipnt(nocons_used) = i
        constants%name(nocons_used) = coname_sub(i)
        constants%constant(nocons_used) = covalue_sub(i)
     end if
    end do
    nocons = nocons_used
    constants%no_item = nocons

    !     Set the required output data.
    !     -> When outputs are not specified, they might end up in A(1), a 'black hole' location that is constantly overwritten
    noout_user = noout_map + noout_eho
    noout = noout_user

    allocate(outputs%names(noout_user))
    allocate(outputs%std_var_name(noout_user))
    allocate(outputs%pointers(noout_user))
    allocate(outputs%units(noout_user))
    allocate(outputs%description(noout_user))

    outputs%cursize  = noout_user
    do i = 1, noout_sub
      outputs%names(i) = ouname_sub(i)
      outputs%std_var_name(i) = ' '
      outputs%units(i) = ' '
      outputs%description(i) = trim(oudesc_sub(i))//' ('//trim(ouname_sub(i))//') '
      outputs%pointers(i) = -1
    enddo
    do j = 1, noout_eho
      i = noout_map+j
      outputs%names(i) = ouname_eho(j)
      outputs%std_var_name(i) = ' '
      outputs%units(i) = ' '
      outputs%description(i) = trim(oudesc_eho(j))//' ('//trim(ouname_eho(j))//') '
      outputs%pointers(i) = -1
    enddo

    deallocate (coname_sub)
    deallocate (covalue_sub)
    deallocate (ouname_sub)

    !     calculation timers need to be known for the statistical processes (start time/stop time)
    isfact = 1
    itfact = 86400
    itstrt_process = nint(tstart_user)
    if (ti_waqproc > 0) then
     itstop_process = floor(tstop_user/ti_waqproc + 0.001d0)*ti_waqproc
    else
     itstop_process = tstop_user
    endif
    otime = dble(julrefdat)-0.5d0 !refdate_mjd

    !     Compute refday needed for daylight process
    call compute_reference_day(refdat, refdayNr)


    !     Finally, evaluate the processes using the proces library
    !     --------------------------------------------------------
    bloom_output_file = defaultfilename('bloom')

    call mess(LEVEL_INFO, 'Initialising water quality processes.')
    call wq_processes_initialise ( lunlsp, proc_def_file, proc_dllso_file, bloom_file, bloom_output_file, statistics_file, statprocesdef, outputs, &
                                 nomult, imultp, constants, refdayNr, noinfo, nowarn, ierr)
    call mess(LEVEL_INFO, 'Number of warnings during initialisation of the processes : ', nowarn)
    call mess(LEVEL_INFO, 'Number of errors during initialisation of the processes   : ', ierr)
    if (ierr  /=  0) then
     call mess(LEVEL_ERROR, 'Something went wrong during initialisation of the processes. Check the lsp-file: ', trim(proc_log_file))
    endif
    call mess(LEVEL_INFO, 'Water quality processes initialisation was successful')
    call mess(LEVEL_INFO, '==========================================================================')

    !     proces fractional step multiplier is 1 for all
    prondt = 1

    !     Allocate the work arrays for the pointers
    call realloc(ipmsa, nipmsa, keepExisting=.false., fill=0)
    call realloc(increm,nipmsa, keepExisting=.false., fill=0)

    !     allocate deriv and velonw arrays
    call realloc(deriv, [noseg, notot], keepExisting=.false., fill=0.0d0 )      !< Model derivatives (= stochi(notot ,noflux) * flux(noflux, noseg))
    call realloc(velonw, [nveln, noq3], keepExisting=.false., fill=0.0 )      !< New velocity array

    !     Determine size of a array from process system and noseg/noq3, and allocate it
    call wq_processes_pmsa_size( lunlsp, noseg, noq3, sizepmsa )
    !     And actually allocate and zero the A array
    call realloc(pmsa, sizepmsa , keepExisting=.false., fill=0.0)

    !     constants from the substance file
    ip = arrpoi(iicons)
    do i = 1, nocons
      pmsa(ip+i-1) = constants%constant(i)
    end do

    !     defaults from the proces library
    ip = arrpoi(iidefa)
    do i = 1, nodef
      pmsa(ip+i-1) = defaul(i)
    end do

    !     parameters from ext-file
    !     spatially varying constants provided trough the ext-file that remain fixed during the run
    !      -> fill using abcdabcdabcd partern
    if (nopa > 0) then
     ip = arrpoi(iiparm)
     do k=0,ktx-kbx
        do j = 1,nopa
           pmsa(ip) = painp(j,k+kbx)
           ip = ip + 1
        end do
     end do
    end if

    !      functions from DFM, one value for the whole system
    !      -> can be used for timeseries that are updated by DFM, e.g. global irradiation
    !      -> or can be used for 2D values when calculating only one water column
    !      -> updated in waqfil

    !     exchange areas
    ip = arrpoi(iiarea)
    iex = 0
    do kk=1,Ndxi
     call getkbotktopmax(kk,kb,kt,ktmax)
     do k=ktmax,kb+1,-1
        pmsa(ip+iex) = ba(kk)
        iex = iex+1
     end do
    end do

    if ( isffmlayer > 0 ) then
     ipoifmlayer = arrpoi(iisfun) + (isffmlayer-1)*noseg
     ipoifmktop  = arrpoi(iisfun) + (isffmktop-1)*noseg
     ipoifmkbot  = arrpoi(iisfun) + (isffmkbot-1)*noseg
     do kk=1,Ndxi
        call getkbotktopmax(kk,kb,kt,ktmax)
        do k=kb,ktmax
           pmsa(ipoifmlayer + k-kbx) = ktmax - k + 1
           pmsa(ipoifmktop  + k-kbx) = ktmax - kbx + 1
           pmsa(ipoifmkbot  + k-kbx) = kb - kbx + 1
        end do
     end do
    end if

    ipbloo = index_in_array(cbloom,pronam)
    if (ipbloo > 0) then
      ioffbl = prvpnt(ipbloo)
      write ( lunlsp, * ) ' MESSAGE: Bloom fractional step switched on'
    else
      ipbloo = 0
      ioffbl = 0
    endif

    call realloc(waqoutputs, [noout, noseg], keepExisting=.false., fill = -999.0d0)
    call realloc(outvar,noout,keepExisting=.false.,fill=0)
    do j=1,noout
      ivar = index_in_array(outputs%names(j),varnam)
      outvar(j) = ivar
    enddo

    ! If there is a parameter 'doprocesses', mask the area where processes are active by setting wqdoproc to .true./.false. (default=.true.)
    ipar = index_in_array(cdoprocesses,paname)
    if (ipar>0) then
     call mess(LEVEL_WARN, 'Found parameter ''DoProcesses'', but this is not used any more.')
     call mess(LEVEL_WARN, 'Use ''ProcessesInactive'' with non-zero values to set processes to inactive instead')
    endif

    ! If there is a parameter 'ProcessesInactive', mask the area where processes are active by setting wqdoproc to .true./.false. (default=.true.)
    ipar = index_in_array(cprocessesinactive,paname)
    if (ipar>0) then
     call mess(LEVEL_INFO, 'Found parameter ''ProcessesInactive''. Water quality processes are switched off for segments where ProcessesInactive <> 0.0')
     do k=kbx,ktx
        wqdoproc(k) = comparereal(painp(ipar,k),0.0e0)==0
     end do
    endif

    jawaqproc = 2 ! processes succesfully initiated

    if ( timon ) call timstop ( ithndl )
    return
end subroutine fm_wq_processes_ini_proc

!! @return Integer result status (0 if successful)
subroutine dfm_waq_initexternalforcings(iresult)
    use unstruc_boundaries
    use m_alloc
    use m_flowexternalforcings
    use m_flowparameters
    use m_flowtimes
    use m_flowgeom
    use m_partitioninfo
    use unstruc_model
    use unstruc_messages
    use timespace
    use m_flow
    use m_meteo
    use m_ec_instance
    use dfm_error
    use processes_input
    use m_fm_wq_processes
    use timers
    use unstruc_files, only: resolvePath

    implicit none
    integer, intent (out)         :: iresult

    character(len=256)            :: filename, sourcemask
    integer                       :: kb, k, ja, method, kk, kt, lenqidnam, ipa, ifun, isfun, imna
    integer                       :: klocal, waqseg2D, waqseglay
    character (len=NAMTRACLEN)    :: qidnam
    character (len=20)            :: waqinput
    double precision, allocatable :: viuh(:)            ! temporary variable
    integer, external             :: findname

    integer(4), save         :: ithndl = 0

    if (timon) call timstrt( "dfm_waq_initexternalforcings", ithndl )

    iresult = DFM_NOERR

    success = .true.    ! default if no valid providers are present in *.ext file (m_flowexternalforcings::success)

    if ( .not. allocated(paname) ) then
       allocate( paname(0) )
    endif
    if ( .not. allocated(funame) ) then
       allocate( funame(0) )
    endif
    if ( .not. allocated(sfunname) ) then
       allocate( sfunname(0) )
    endif
    if ( .not. allocated(monname) ) then
       allocate( monname(0) )
    endif
    if ( .not. allocated(mondef) ) then
       allocate( mondef(0,0) )
    endif

    call settimespacerefdat(refdat, julrefdat, Tzone, Timjan)

    if (mext /= 0) then
      ja = 1

      do while (ja  ==  1)                                ! read *.ext file

         call delpol()                                    ! remove a possibly existing polygon
         call readprovider(mext,qid,filename,filetype,method,operand,transformcoef,ja,sourcemask)
         if (ja == 1) then
            call resolvePath(filename, md_extfile_dir)
            call mess(LEVEL_INFO, 'External Forcing or Initialising '''//trim(qid)//''' from file '''//trim(filename)//'''.')
            ! Initialize success to be .false.
            success = .false.

            qidnam = qid
            call get_waqinputname(qid, waqinput, qidnam)
            lenqidnam = len_trim(qidnam)
            if (filetype == 7 .and. method == 4) then
               method = 5                                   ! upward compatible fix
            endif

            if (qid(1:12) == 'waqparameter') then
               ipa = findname(nopa, paname, waqinput)

               if ( ipa == 0 ) then
                  nopa = nopa + 1
                  ipa = nopa
                  call realloc(paname, nopa, keepExisting=.true., fill=waqinput)
                  call realloc(painp, [nopa, Ndkx], keepExisting=.true., fill=0.0)
               end if
               call realloc(viuh, Ndkx, keepExisting=.false., fill=dmiss)

                !  copy existing parameter values (if they existed) in temp array
               do kk=1,Ndxi
                  call getkbotktop(kk,kb,kt)
                  viuh(kk) = painp(ipa,kk)
                  do k=kb,kb+kmxn(kk)-1
                     viuh(k) = painp(ipa,k)
                  end do
               end do

                ! will only fill 2D part of viuh
               success = timespaceinitialfield(xz, yz, viuh, Ndx, filename, filetype, method, operand, transformcoef, 2)

               if (success) then
                  do kk = 1,Ndxi
                     if (viuh(kk)  /=  dmiss) then
                        painp(ipa,kk) = viuh(kk)
                        call getkbotktop(kk,kb,kt)
                        do k=kb,kb+kmxn(kk)-1
                           painp(ipa,k) = painp(ipa,kk)
                        end do
                     endif
                  enddo
               endif
               deallocate(viuh)

            elseif (qid(1:16) == 'waqsegmentnumber') then
               ipa = findname(nopa, paname, waqinput)

               if ( ipa == 0 ) then
                  nopa = nopa + 1
                  ipa = nopa
                  call realloc(paname, nopa, keepExisting=.true., fill=waqinput)
                  call realloc(painp, [nopa, Ndkx], keepExisting=.true., fill=0.0)
               end if
               call realloc(viuh, Ndkx, keepExisting=.false., fill=dmiss)

            ! copy existing parameter values (if they existed) in temp array
               do kk=1,Ndxi
                  call getkbotktop(kk,kb,kt)
                  viuh(kk) = painp(ipa,kk)
                  do k=kb,kb+kmxn(kk)-1
                     viuh(k) = painp(ipa,k)
                  end do
               end do

                ! will only fill 2D part of viuh
               success = timespaceinitialfield(xz, yz, viuh, Ndx, filename, filetype, method, operand, transformcoef, 2)

               if (success) then
                  do kk = 1,Ndxi
                     if (viuh(kk)  /=  dmiss) then
                        if (jampi == 0) then
                           waqseg2D = mod(int(viuh(kk))-1, Ndxi) + 1
                           waqseglay = (int(viuh(kk))-1) / Ndxi + 1
                        else
                           waqseg2D = mod(int(viuh(kk))-1, Nglobal_s) + 1
                           waqseglay = (int(viuh(kk))-1) / Nglobal_s + 1
                        endif
                        klocal = global_to_local( waqseg2D )
                        if(klocal >= 1.and.klocal <= Ndxi.and.waqseglay >= 1.and.waqseglay <= kmx) then
                           call getkbotktop(klocal,kb,kt)
                           painp(ipa,kk) = max(kb,kb+kmxn(kk)-waqseglay)-kbx + 1
                        else
                           painp(ipa,kk) = -999.0
                        endif
                        call getkbotktop(kk,kb,kt)
                        do k=kb,kb+kmxn(kk)-1
                           painp(ipa,k) = painp(ipa,kk)
                        end do
                     endif
                  enddo
               endif
               deallocate(viuh)

            else if (qid(1:11) == 'waqfunction') then

               ifun = findname(nofun, funame, waqinput)

               if ( ifun == 0 ) then
                  nofun = nofun + 1
                  call realloc(funame, nofun, keepExisting=.true., fill=waqinput)
                  call reallocP(funinp, [nofun, 1], keepExisting=.true., fill=0.0d0)
               end if
               success = .true.

            else if (qid(1:18) == 'waqsegmentfunction') then

               isfun = findname(nosfun, sfunname, waqinput)

               if ( isfun == 0 ) then
                  nosfun = nosfun + 1
                  call realloc(sfunname, nosfun, keepExisting=.true., fill=waqinput)
                  call reallocP(sfuninp, [nosfun, Ndkx], keepExisting=.true., fill=0.0d0)
               end if
               success = .true.

            else if (qid(1:17) == 'waqmonitoringarea') then
               imna = findname(nomon, monname, waqinput)

               if ( imna == 0 ) then
                  nomon = nomon + 1
                  imna = nomon
                  call realloc(monname, nomon, keepExisting=.true., fill=waqinput)
                  call realloc(mondef, [nomon, Ndkx], keepExisting=.true., fill=2)
               end if

               call realloc(viuh, Ndkx, keepExisting=.false., fill=dmiss)

                ! will only fill 2D part of viuh
               success = timespaceinitialfield(xz, yz, viuh, Ndx, filename, filetype, method, operand, transformcoef, 2)

               if (success) then
                  do kk=1,Ndxi
                     if (viuh(kk) /= dmiss) then
                        mondef(imna,kk) = 1
                        call getkbotktop(kk,kb,kt)
                        do k=kb,kb+kmxn(kk)-1
                           mondef(imna,k) = 1
                        end do
                     endif
                  end do
               endif
               deallocate(viuh)
            else
            ! just accept any other keyword as success, they are evaluated again in unstruc.F90
               success = .true.
            endif

        endif

      enddo

    endif ! read mext file

    if (loglevel_StdOut == LEVEL_DEBUG .and. associated(ecInstancePtr)) then
      call ecInstancePrintState(ecInstancePtr,callback_msg,LEVEL_DEBUG)
    endif

    if (.not. success) then
      iresult = DFM_EXTFORCERROR
    end if

    if (mext /= 0) then
      rewind(mext) ! rewind ext file
    end if

    if ( timon ) call timstop ( ithndl )
    return

contains

   function findloc( array, value )
        ! Private version of findloc - this intrinsic is new in Fortran 2008
        ! and not supported by gfortran 4.9.1
       integer, dimension(:), intent(in) :: array
       integer, intent(in)               :: value

       integer, dimension(1)             :: findloc

       integer                           :: i

       findloc = 0

       do i = 1,size(array)
           if ( array(i) == value ) then
               findloc = i
               exit
           endif
       enddo
   end function findloc


   integer function global_to_local( global_number )
       !> Internal function: Translate the global segment number to the
       !! number used within the current domain
       use m_partitioninfo, only: jampi, iglobal_s
       use timers

       integer, intent(in) :: global_number !< Global segment number to be translated

       integer, save       :: previous_global = -1
       integer, save       :: previous_local  = -1
       integer             :: i(1)

       integer(4), save         :: ithndl = 0

       if (timon) call timstrt( "global_to_local", ithndl )

       if (jampi==0) then
          global_to_local = global_number
       else
          global_to_local = dmiss
          if ( global_number == previous_global ) then
              global_to_local = previous_local
          else
              i = findloc(iglobal_s, value = global_number)
              if ( i(1)  >  0 ) then
                 global_to_local = i(1)
                 previous_global = global_number
                 previous_local  = i(1)
              endif
          endif
       endif
       if ( timon ) call timstop ( ithndl )
   end function global_to_local

end subroutine dfm_waq_initexternalforcings


subroutine get_waqinputname(qid, inputname, qidname)
    !> Convert qid (from .ext file) to waq input name (split in generic qidname and specific input name).
    !! If the input qid is not waq input name, then the same qid is returned (and no waq input name)
    implicit none

    character(len=*), intent(in)    :: qid       !< Original quantityid, e.g., 'waqfunctionradsurf'.
    character(len=*), intent(inout) :: inputname !< The trimmed waq input name, e.g., 'fluor'.
    character(len=*), intent(inout) :: qidname   !< The base input name for further use in external file analisys, e.g., 'tracerbnd'.

    character(len=256)              :: qidloc    !< Original quantityid, e.g., 'waqfunctionradsurf'.

    inputname = ''
    qidloc = qid
    if ( qidloc(1:13) == 'initialwaqbot' ) then
     qidname = qidloc(1:13)
     if ( len_trim(qidloc) > 13 ) then
        inputname = trim(qidloc(14:))
     end if
    else if ( qidloc(1:11) == 'waqfunction' ) then
     qidname = qidloc(1:11)
     if ( len_trim(qidloc) > 11 ) then
        inputname = trim(qidloc(12:))
     end if
    else if ( qidloc(1:16) == 'waqsegmentnumber' ) then
     qidname = qidloc(1:16)
     if ( len_trim(qidloc) > 16 ) then
        inputname = trim(qidloc(17:))
     end if
    else if ( qidloc(1:18) == 'waqsegmentfunction' ) then
     qidname = qidloc(1:18)
     if ( len_trim(qidloc) > 18 ) then
        inputname = trim(qidloc(19:))
     end if
    else if (qidloc(1:12) == 'waqparameter' ) then
     qidname = qidloc(1:12)
     if ( len_trim(qidloc) > 12 ) then
        inputname = trim(qidloc(13:))
     end if
    else if (qidloc(1:18) == 'waqmassbalancearea' ) then
     qidname = qidloc(1:18)
     if ( len_trim(qidloc) > 18 ) then
        inputname = trim(qidloc(19:))
     end if
    else if (qidloc(1:17) == 'waqmonitoringarea' ) then
     qidname = qidloc(1:17)
     if ( len_trim(qidloc) > 17 ) then
        inputname = trim(qidloc(18:))
     end if
    end if

    return
end subroutine get_waqinputname


subroutine add_wqbot(wqbotnam, wqbotunit, iwqbot, janew)
    !> add waq bottom substance
    use m_flowgeom
    use m_flow, only: Ndkx
    use m_flowexternalforcings, only: numtracers, trnames
    use m_fm_wq_processes
    use m_alloc
    use m_missing
    use unstruc_messages
    implicit none

    character(len=*), intent(in)  :: wqbotnam
    character(len=20), intent(in) :: wqbotunit
    integer,          intent(out) :: iwqbot
    integer,          intent(out) :: janew

    integer,          external    :: findname

    integer                       :: itrac

    if ( .not. allocated(wqbotnames) ) then
     allocate( wqbotnames(0) )
    endif
    if ( .not. allocated(wqbot) ) then
     allocate( wqbot(0,0) )
    endif
    if ( .not. allocated(trnames) ) then
     allocate( trnames(0) )
    endif

    iwqbot = findname(numwqbots, wqbotnames, wqbotnam)
    itrac = findname(numtracers, trnames, wqbotnam)

    if ( itrac /= 0 ) then
     call mess(LEVEL_ERROR, 'add_wqbot: water quality bottom variable named '''//trim(wqbotnam)//''' already exists as a tracer.')
    endif

    janew = 0
    if ( iwqbot == 0 ) then
     janew = 1
    !     add bottom substance

     numwqbots = numwqbots+1
     iwqbot = numwqbots

    !     realloc
     call realloc(wqbotnames, numwqbots, keepExisting=.true., fill=trim(wqbotnam))
     call realloc(wqbotunits, numwqbots, keepExisting=.true., fill=wqbotunit)
     call realloc(wqbot, (/ numwqbots, Ndkx /), keepExisting=.true., fill=0.0d0)
    end if
end subroutine add_wqbot

subroutine fm_wq_processes_step(dt,time)
    use m_fm_wq_processes
    use m_wq_processes_proces
    use m_mass_balance_areas
    use unstruc_model, only: md_flux_int
    use m_flow, only: vol1
    use timers

    implicit none

    double precision, intent(in) :: dt   !< timestep for waq in seconds
    double precision, intent(in) :: time !< time     for waq in seconds

    integer                      :: ipoiconc

    integer                      :: ipoivol, ipoisurf, ipoiarea
    integer                      :: ipoivelx, ipoidefa, ipoiflux

    integer(4), save :: ithand0 = 0
    integer(4), save :: ithand1 = 0
    integer(4), save :: ithand2 = 0

    if ( jawaqproc  ==  0 ) then
     return
    endif

    if ( timon ) call timstrt ( "fm_wq_processes_step", ithand0 )
    flux_int = md_flux_int

    !     copy data from D-FlowFM to WAQ
    if ( timon ) call timstrt ( "copy_data_from_fm_to_wq_processes", ithand1 )
    call copy_data_from_fm_to_wq_processes(time)
    if ( timon ) call timstop ( ithand1 )

    ipoiconc = arrpoi(iiconc)
    ipoivol  = arrpoi(iivol)
    ipoivelx = arrpoi(iivelx)
    ipoidefa = arrpoi(iidefa)
    ipoiflux = arrpoi(iiflux)
    ipoisurf = arrpoi(iisfun) + (isfsurf-1)*noseg
    ipoiarea = arrpoi(iiarea)

    pmsa(ipoidefa+1) = time

    call wq_processes_proces (notot , noseg , pmsa(ipoiconc), vol1(kbx:ktx-kbx), time  , dt    , deriv , ndmpar, &
                            nproc , nflux , ipmsa , prvnio, promnr, iflux , increm, pmsa(ipoiflux), flxdmp, stochi, &
                            ibflag, ipbloo, ioffbl, amass , nosys , isfact, itfact , iexpnt, iknmrk, noq1  , &
                            noq2  , noq3  , noq4  , pmsa(ipoiarea), ndspn , idpnew, dispnw, ndspx , dspx  , &
                            dsto  , nveln , ivpnw , velonw, nvelx , pmsa(ipoivelx), vsto  , mbadefdomain(kbx:ktx), &
                            pmsa(ipoidefa), prondt, prvvar, prvtyp, vararr, varidx, arrpoi, arrknd, arrdm1, &
                            arrdm2, novar , pmsa  , nomba , pronam, prvpnt, nodef , pmsa(ipoisurf), flux_int)

    ! copy data from WAQ to D-FlowFM
    if ( timon ) call timstrt ( "copy_data_from_wq_processes_to_fm", ithand2 )
    call copy_data_from_wq_processes_to_fm(dt, time)
    if ( timon ) call timstop ( ithand2 )

    if ( timon ) call timstop ( ithand0 )
    return
end subroutine fm_wq_processes_step


subroutine copy_data_from_fm_to_wq_processes(time)
    !  copy data from D-FlowFM to WAQ
    use m_flowgeom,       only: Ndxi, ba
    use m_flow,           only: vol1, ucx, ucy
    use m_flowtimes,      only: irefdate, tunit
    use m_flowparameters, only: flowWithoutWaves, jawaveswartdelwaq
    use m_fm_wq_processes
    use m_transport,      only: constituents, itemp, isalt
    use m_sferic,         only: twopi, rd2dg
    use m_wind
    use m_meteo
    use processes_input
    use m_waves,          only: fetch, nwf, hwav, rlabda, twav
    use unstruc_messages
    implicit none

    double precision, intent(in) :: time !< time     for waq in seconds

    double precision :: taucurc, czc
    double precision :: u10, dir, wdir, FetchL, FetchD

    integer          :: isys, iconst, iwqbot
    integer          :: ipoisurf, ipoitau, ipoivel
    integer          :: ipoivol, ipoiconc, ipoisal, ipoitem
    integer          :: ipoivwind, ipoiwinddir, ipoifetchl, ipoifetchd, ipoiradsurf, ipoirain, ipoivertdisper, ipoileng
    integer          :: ipoiwaveheight, ipoiwavelength, ipoiwaveperiod
    integer          :: ip, ifun, isfun
    integer          :: kk, k, kb, kt, ktmax, ktwq
    integer          :: L

    logical, save    :: first = .true.

    if (nofun>0) then
     do ifun=1,nofun
        success = ec_gettimespacevalue(ecInstancePtr, item_waqfun(ifun), irefdate, tzone, tunit, time)
        if (.not.success) then
           call mess(LEVEL_ERROR, 'Error reading data for function: ', trim(funame(ifun)))
        endif
     end do
     ip = arrpoi(iifunc)
     do ifun=1,nofun
        pmsa(ip+ifun-1) = funinp(ifun,1)
     end do
    end if

    if (nosfunext>0) then
     do isfun=1,nosfunext
        success = ec_gettimespacevalue(ecInstancePtr, item_waqsfun(isfun), irefdate, tzone, tunit, time)
        if (.not.success) then
           call mess(LEVEL_ERROR, 'Error reading data for segment function: ', trim(sfunname(isfun)))
        endif
     end do
     do isfun=1,nosfunext
        ip = arrpoi(iisfun) + (isfun-1)*noseg
        do kk=1,Ndxi
           call getkbotktopmax(kk,kb,kt,ktmax)
           do k=kb,ktmax
              pmsa(ip + k-kbx) = sfuninp(isfun, kk)
           end do
        end do
     end do
    end if

    ipoisurf = arrpoi(iisfun) + (isfsurf-1)*noseg
    do kk=1,Ndxi
     call getkbotktopmax(kk,kb,kt,ktmax)
     do k=kb,ktmax
        pmsa(ipoisurf + k-kbx) = ba(kk)
     end do
    end do

    ipoivol = arrpoi(iivol)
    do k=0,ktx-kbx
     pmsa(ipoivol + k) = vol1(k+kbx)
    end do

    if (isftau > 0) then
     ipoitau  = arrpoi(iisfun) + (isftau-1)*noseg
     if (jawave==0 .or. flowWithoutWaves) then
        call gettaus(1,2)
     else
        call gettauswave(jawaveswartdelwaq)
     endif
     do kk=1,Ndxi
        call getkbotktop(kk,kb,kt)
        pmsa(ipoitau+kb-kbx) = taus(kk)
     end do
    end if

    if (isfvel > 0) then
     ipoivel  = arrpoi(iisfun) + (isfvel-1)*noseg
     do kk=1,Ndxi
        call getkbotktopmax(kk,kb,kt,ktmax)
        do k=kb,ktmax
           pmsa(ipoivel  + k-kbx) = sqrt(ucx(k)**2 + ucy(k)**2)
        end do
     end do
    endif

    ! get wave params from dflow-fm and put to PMSA array.
    if (isfwaveheight > 0) then
     ipoiwaveheight  = arrpoi(iisfun) + (isfwaveheight-1)*noseg
     do kk=1,Ndxi
        call getkbotktopmax(kk,kb,kt,ktmax)
        do k=kb,ktmax
           pmsa(ipoiwaveheight  + k-kbx) = hwav(kk)*sqrt(2.0_fp)
        end do
     end do
    endif
    if (isfwavelength > 0) then
     ipoiwavelength  = arrpoi(iisfun) + (isfwavelength-1)*noseg
     do kk=1,Ndxi
        call getkbotktopmax(kk,kb,kt,ktmax)
        do k=kb,ktmax
           pmsa(ipoiwavelength  + k-kbx) = rlabda(kk)
        end do
     end do
    endif
    if (isfwaveperiod > 0) then
     ipoiwaveperiod  = arrpoi(iisfun) + (isfwaveperiod-1)*noseg
     do kk=1,Ndxi
        call getkbotktopmax(kk,kb,kt,ktmax)
        do k=kb,ktmax
           pmsa(ipoiwaveperiod  + k-kbx) = twav(kk)
        end do
     end do
    endif


    if ( isfsal > 0 ) then
     ipoisal = arrpoi(iisfun) + (isfsal-1)*noseg
     do k=0,ktx-kbx
        pmsa(ipoisal + k) = constituents(isalt,k+kbx)
     end do
    end if

    if ( isftem > 0 ) then
     ipoitem = arrpoi(iisfun) + (isftem-1)*noseg
     do k=0,ktx-kbx
        pmsa(ipoitem + k) = constituents(itemp, k+kbx)
     end do
    end if

    ! copy 2D arrays for wind velocity magnitude, fetch length, solar radiation and rain to 3D waq arrays, fill over whole column (safety)
    if ( isfvwind > 0 ) then
     ipoivwind = arrpoi(iisfun) + (isfvwind-1)*noseg
     if(jawind == 1) then
        do kk=1,Ndxi
           call getkbotktopmax(kk,kb,kt,ktmax)
           ! apparently wind is available at edges only, so just take the 1st edge
           call getlink1(kk,L)
           u10 = sqrt( wx(L)*wx(L) + wy(L)*wy(L) )
           pmsa(ipoivwind + kb-kbx : ipoivwind + ktmax-kbx) = u10
        end do
     else
        do k=0,ktx-kbx
           pmsa(ipoivwind + k) = windsp
        end do
     end if
    end if

    if ( isfwinddir > 0 ) then
     ipoiwinddir = arrpoi(iisfun) + (isfwinddir-1)*noseg
     if(jawind == 1) then
        do kk=1,Ndxi
           call getkbotktopmax(kk,kb,kt,ktmax)
           ! apparently wind is available at edges only, so just take the 1st edge
           call getlink1(kk,L)
           dir = atan2(wy(L), wx(L))
           if (dir < 0d0) dir = dir + twopi
           wdir = 270.0d0 - dir*rd2dg ! from rad to degree
           if (wdir < 0d0) wdir = wdir + 360.0d0
           pmsa(ipoiwinddir + kb-kbx : ipoiwinddir + ktmax-kbx) = wdir
        end do
     else
        do k=0,ktx-kbx
           pmsa(ipoiwinddir + k) = winddir
        end do
     end if
    end if

    if ( isffetchl > 0 ) then   ! note: no fetch without wind
     ipoifetchl = arrpoi(iisfun) + (isffetchl-1)*noseg
     ipoifetchd = arrpoi(iisfun) + (isffetchd-1)*noseg
     do kk=1,Ndxi
        call getkbotktopmax(kk,kb,kt,ktmax)
        call getfetch(kk,U10,FetchL,FetchD)
        pmsa(ipoifetchl + kb-kbx : ipoifetchl + ktmax-kbx) = FetchL
        pmsa(ipoifetchd + kb-kbx : ipoifetchd + ktmax-kbx) = FetchD
     end do
    end if

    if ( isfradsurf > 0 ) then
     ipoiradsurf = arrpoi(iisfun) + (isfradsurf-1)*noseg
     do kk=1,Ndxi
        call getkbotktopmax(kk,kb,kt,ktmax)
        call getkbotktop(kk,kb,kt)
        pmsa(ipoiradsurf + kb-kbx : ipoiradsurf + ktmax-kbx) = qrad(kk)
     end do
    end if

    if ( isfrain > 0 ) then
     ipoirain = arrpoi(iisfun) + (isfrain-1)*noseg
     do kk=1,Ndxi
        call getkbotktopmax(kk,kb,kt,ktmax)
        pmsa(ipoirain + kb-kbx : ipoirain + ktmax-kbx) = rain(kk)/24.0d0 ! rain: mm/day => mm/h
     end do
    end if

    if ( isfvertdisper > 0 ) then
     ipoivertdisper = arrpoi(iisfun) + (isfvertdisper-1)*noseg
     do kk=1,Ndxi
        call getkbotktopmax(kk,kb,kt,ktmax)
        do k=kb,ktmax
           pmsa(ipoivertdisper + k-kbx) = vicwws(k-1)
        end do
     end do
    ! set vertical dispersion lengths
     ipoileng =  arrpoi(iileng)
     do kk=1,Ndxi
        call getkbotktopmax(kk,kb,kt,ktmax)
        do k=ktmax,kb+1,-1
           pmsa(ipoileng) = 0.5d0*(zws(k)-zws(k-1))
           ipoileng = ipoileng + 1
           pmsa(ipoileng) = 0.5d0*(zws(k-1)-zws(k-2))
           ipoileng = ipoileng + 1
        end do
     end do

    end if

    ! determine cells where processes can be active above a minimum volume and depth (the switch wqdoproc can overrule this)
    do kk=1,Ndxi
     call getkbotktopmax(kk,kb,kt,ktmax)
     do k=kb,ktmax
        wqactive(k) = vol1(k) > waq_vol_dry_thr .and. (vol1(k)/ba(kk)) > waq_dep_dry_thr
     enddo
    enddo

    ! fill concentrations
    ipoiconc = arrpoi(iiconc)
    do k=kbx,ktx
     do isys=1,nosys
        iconst = isys2const(isys)
        pmsa(ipoiconc+(k-kbx)*notot+isys-1) = constituents(iconst,k)
     end do
    end do

    ! fill masses (transported)
    do k=kbx,ktx
     if (wqactive(k)) then
        do isys=1,nosys
           iconst = isys2const(isys)
           amass(isys,k-kbx+1) = constituents(iconst,k)*vol1(k)
        end do
     else
        do isys=1,nosys
           iconst = isys2const(isys)
           amass(isys,k-kbx+1) = 0.0d0
        end do
     endif
    end do

    ! fill concentrations and masses (not transported, only first time)
    if (first) then
     first = .false.
     if (notot>nosys) then
     do kk=1,Ndxi
        call getkbotktopmax(kk,kb,kt,ktmax)
        do k=kb,ktmax
           do isys=nosys+1,notot
              iwqbot = isys2wqbot(isys)
              pmsa(ipoiconc+(k-kbx)*(notot)+isys-1) = wqbot(iwqbot,k)
              amass(isys,k-kbx+1) = wqbot(iwqbot,k)*ba(kk)
           end do
        end do
     end do
     end if
    end if

    ! set iknmrk array
    if(kmx > 0) then
     do kk=1,Ndxi
        call getkbotktopmax(kk,kb,kt,ktmax)
        if (.not.wqdoproc(kb)) then
           ! whole column is inactive
           do k=kb,ktmax
              iknmrk(k-kbx+1) = IKNMRK_INACTIVE
           end do
        else
           ktwq = 0
           do k=ktmax,kb,-1
              if (wqactive(k)) then
                 ! first active segment from the top
                 ktwq = k
                 exit
              else
                 ! set inactive segments from the top as inactive
                 iknmrk(k-kbx+1) = IKNMRK_INACTIVE
              end if
           enddo
           if (ktwq > 0) then
              if ( kb == ktwq ) then
                 ! only one active segment
                 iknmrk(ktwq-kbx+1) = IKNMRK_ACTIVE_TOPBOT
              else
                 ! ktwq is the top active segment
                 iknmrk(ktwq-kbx+1) = IKNMRK_ACTIVE_TOP
                 do k=kb+1,ktwq-1
                    iknmrk(k-kbx+1) = IKNMRK_ACTIVE_MIDDLE
                 end do
                 iknmrk(kb-kbx+1) = IKNMRK_ACTIVE_BOTTOM
              end if
           end if
        end if
     end do
    else
     do k=1,Ndxi
        if (wqactive(k).and.wqdoproc(k)) then
           iknmrk(k) = IKNMRK_ACTIVE_TOPBOT
        else
           iknmrk(k) = IKNMRK_INACTIVE
        end if
     end do
    end if
    return
end subroutine copy_data_from_fm_to_wq_processes


subroutine copy_data_from_wq_processes_to_fm(dt, tim)
    !  copy data from WAQ to D-FlowFM
    use m_missing,        only: dmiss
    use m_flowgeom,       only: Ndxi, ba
    use m_flow,           only: vol1
    use m_flowtimes
    use m_flowparameters, only: eps10
    use m_fm_wq_processes
    use m_transport,      only: constituents
    use unstruc_messages
    use precision_basics
    use timers

    implicit none

    double precision, intent(in) :: dt
    double precision, intent(in) :: tim

    integer          :: isys, iconst, iwqbot
    integer          :: ivar, iarr, iv_idx
    integer          :: iarknd, ip_arr, idim1, idim2
    integer          :: incr
    integer          :: i, j, ip
    integer          :: kk, k, kb, kt, ktmax
    logical          :: copyoutput

    integer(4), save :: ithand1 = 0
    integer(4), save :: ithand2 = 0
    integer(4), save :: ithand3 = 0

    ! fill concentrations (transported)
    if ( timon ) call timstrt ( "copy_const", ithand1 )
    do kk=1,Ndxi
     call getkbotktopmax(kk,kb,kt,ktmax)
     do k=kb,kt
        if (wqactive(k)) then
           do isys=1,nosys
              iconst = isys2const(isys)
              constituents(iconst,k) = amass(isys,k-kbx+1) / vol1(k)
           end do
        end if
     end do
    end do
    if ( timon ) call timstop ( ithand1 )

    ! Ouputs to waq outputs array (only when his or map outputs will be written within the next timestep,
    ! and during first timestep)
    copyoutput = .false.
    if (comparereal(tim, tstart_user, eps10) == 0) then
     copyoutput = .true.
    endif
    if (ti_his > 0) then
     if (comparereal(tim+dt-2.0_hp*eps10, time_his, eps10)>= 0) then
        copyoutput = .true.
     endif
    endif
    if (ti_map > 0 .or. ti_mpt(1) > 0) then
    if (comparereal(tim+dt-2.0_hp*eps10, time_map, eps10) >= 0) then
        copyoutput = .true.
     endif
    endif

    if (copyoutput) then
    ! copy additional output
     if ( timon ) call timstrt ( "copy_output", ithand2 )
     waqoutputs=dmiss
     noout = outputs%cursize
     do j = 1, noout
        ivar   = outvar(j)  ! which variable is it
        if (ivar > 0) then
           iarr   = vararr(ivar)         ! which array in pmsa
           iv_idx = varidx(ivar)         ! which index within the array
           iarknd = arrknd(iarr)         ! which type of array (increm is 0, dim1 or 1)
           ip_arr = arrpoi(iarr)         ! start point of the array in pmsa
           idim1  = arrdm1(iarr)         ! dimension in the 1e direction
           idim2  = arrdm2(iarr)         ! dimension in the 2e direction
           if ( iarknd  ==  1 ) then
              ip = ip_arr + iv_idx - 1
              incr = 0
           elseif ( iarknd  ==  2 ) then
              ip = ip_arr + iv_idx - 1
              incr = idim1
           elseif ( iarknd  ==  3 ) then
              ip = ip_arr + (iv_idx-1)*idim1
              incr = 1
           endif
           do i = 1, noseg
              waqoutputs(j,i) = pmsa(ip)
              ip = ip + incr
           enddo
        endif
     enddo
     if ( timon ) call timstop ( ithand2 )
    endif

    ! Copy wqbot data (when his or map, but also when rst or mba outputs will be written within the next timestep, and during first timestep)
    if (ti_rst > 0) then
    if (comparereal(tim+dt-2.0_hp*eps10, time_rst, eps10) >= 0) then
        copyoutput = .true.
     endif
    endif
    if (ti_mba > 0) then
    if (comparereal(tim+dt-2.0_hp*eps10, time_mba, eps10) >= 0) then
        copyoutput = .true.
     endif
    endif

    if (copyoutput) then
    ! copy concentrations (not transported)
     if ( timon ) call timstrt ( "copy_wqbot", ithand3 )
     if (notot>nosys) then
        do kk=1,Ndxi
           call getkbotktopmax(kk,kb,kt,ktmax)
           do k=kb,kt
              do isys=nosys+1,notot
                 iwqbot = isys2wqbot(isys)
                 wqbot(iwqbot,k) = amass(isys,k-kbx+1) / ba(kk)
              end do
           end do
        end do
     end if
     if ( timon ) call timstop ( ithand3 )
    end if
    return
end subroutine copy_data_from_wq_processes_to_fm

logical function wq_processes_mydomain(iseg)

   use m_fm_wq_processes
   use m_partitioninfo

   implicit none

   integer :: iseg
   if(jampi == 1) then
      if(iseg > 0 .and. iseg <= noseg) then
         wq_processes_mydomain = wqmydomain(iseg)
      else
         wq_processes_mydomain = .false.
      endif
   else
      wq_processes_mydomain = .true.
   endif

end function wq_processes_mydomain

logical function reduce_sum_wq_processes(size_wq_processes_data, wq_processes_data)

    use m_partitioninfo

    implicit none

    integer             :: size_wq_processes_data
    real                :: wq_processes_data(size_wq_processes_data)

    double precision    :: mpi_wq_processes_data(size_wq_processes_data)
    double precision    :: mpi_wq_processes_data_reduce(size_wq_processes_data)

    if (jampi==1) then
      mpi_wq_processes_data = dble(wq_processes_data)
      call reduce_double_sum(size_wq_processes_data, mpi_wq_processes_data, mpi_wq_processes_data_reduce)
      wq_processes_data = sngl(mpi_wq_processes_data_reduce)
    end if

    reduce_sum_wq_processes = .true.

end function reduce_sum_wq_processes

logical function reduce_int_max_wq_processes(wq_processes_data)

    use m_partitioninfo

    implicit none

    integer             :: wq_processes_data

    if (jampi==1) then
      call reduce_int1_max(wq_processes_data)
    end if

    reduce_int_max_wq_processes = .true.

end function reduce_int_max_wq_processes


subroutine default_fm_wq_processes()
    !> defaults for process library (WAQ)
    use m_fm_wq_processes
    use unstruc_model
    implicit none

    jawaqproc = 0
    md_subfile = ''
    md_ehofile = ''
    md_sttfile = ''
    md_thetav_waq = 0d0
    md_dt_waqproc = 0d0

    return
end subroutine default_fm_wq_processes
