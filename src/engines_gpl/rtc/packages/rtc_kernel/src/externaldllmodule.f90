!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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

   Module ExternalDLLModule

!     Make the RTC data available to the Fortran evaluation module
!     This is achieved by letting the pointers sobekh, sobekq, ...
!     point to the relevant array sections.
!
!     In the routine eval_measures these will then be available as
!     individual arrays, making the analogy with the Matlab
!     implementation as close as possible.
!
!     Note:
!     - No provisions for version 2.06 support
!     - No support for water quality yet
!
    use parametermodule

    Implicit None

    integer, parameter :: dp = kind(1.0d0)

    integer, parameter :: isobekh      = 1     ! Index of: water level
    integer, parameter :: isobekq      = 2     ! Index of: discharge at reach segment
    integer, parameter :: isobeksa     = 3     ! Index of: surface area
    integer, parameter :: isobekwd     = 4     ! Index of: water depth
    integer, parameter :: isobekcl     = 5     ! Index of: crest level of structure
    integer, parameter :: isobekcw     = 6     ! Index of: crest width of structure
    integer, parameter :: isobekgl     = 7     ! Index of: gate lower edge (orifice)
    integer, parameter :: isobekgo     = 8     ! Index of: opening height (orifice)
    integer, parameter :: isobekfa     = 9     ! Index of: structure flow area
    integer, parameter :: isobekqs     = 10    ! Index of: discharge structure
    integer, parameter :: isobekvs     = 11    ! Index of: velocity at structure
    integer, parameter :: isobekhu     = 12    ! Index of: water level up
    integer, parameter :: isobekhd     = 13    ! Index of: water level down
    integer, parameter :: isobekdh     = 14    ! Index of: head over structure
    integer, parameter :: isobekpd     = 15    ! Index of: pressure difference over structure
    integer, parameter :: isobekpc     = 16    ! Index of: pump capacity(pump)
    integer, parameter :: isobek1d2dh  = 1     ! Index of: 2D water level
    integer, parameter :: isobek1d2dwd = 4     ! Index of: 2D water depth
    integer, parameter :: isobek1d2dbl = 17    ! Index of: 2D bed level
    integer, parameter :: isobek1d2du  = 18    ! Index of: U-velocity
    integer, parameter :: isobek1d2dv  = 19    ! Index of: V-velocity
    integer, parameter :: isobek1d2dc  = 20    ! Index of: Abs velocity
    integer, parameter :: isobekvr     = 21    ! Index of: reach segment velocity
    integer, parameter :: irrh         = 1     ! Index of: RR water level or ground water level
    integer, parameter :: irainh       = 1     ! Index of: precipitation
!   integer, parameter :: irainhpredict= 1     ! Index of: precipitation, predicted - NOT YET
    integer, parameter :: isobekc      = 1     ! Index of: optional, controlled by RTC
    integer, parameter :: isobeks      = 1     ! Index of: set point of SOBEK-flow controller
    integer, parameter :: irrslowon    = 1     ! Index of: RR-pump, switch on low capacity
    integer, parameter :: irrslowoff   = 1     ! Index of: RR-pump, switch off low capacity
    integer, parameter :: irrshighon   = 1     ! Index of: RR-pump, switch on high capacity
    integer, parameter :: irrshighoff  = 1     ! Index of: RR-pump, switch off high capacity
    integer, parameter :: id3dh        = 1     ! Index of: water level
    integer, parameter :: id3dsal      = 2     ! Index of: salinity

    real(dp), dimension(:), pointer :: sobekh
    real(dp), dimension(:), pointer :: sobekq
    real(dp), dimension(:), pointer :: sobeksa
    real(dp), dimension(:), pointer :: sobekwd
    real(dp), dimension(:), pointer :: sobekcl
    real(dp), dimension(:), pointer :: sobekcw
    real(dp), dimension(:), pointer :: sobekgl
    real(dp), dimension(:), pointer :: sobekgo
    real(dp), dimension(:), pointer :: sobekfa
    real(dp), dimension(:), pointer :: sobekqs
    real(dp), dimension(:), pointer :: sobekvs
    real(dp), dimension(:), pointer :: sobekhu
    real(dp), dimension(:), pointer :: sobekhd
    real(dp), dimension(:), pointer :: sobekdh
    real(dp), dimension(:), pointer :: sobekpd
    real(dp), dimension(:), pointer :: sobekpc
    real(dp), dimension(:), pointer :: sobek1d2dh
    real(dp), dimension(:), pointer :: sobek1d2dwd
    real(dp), dimension(:), pointer :: sobek1d2dbl
    real(dp), dimension(:), pointer :: sobek1d2du
    real(dp), dimension(:), pointer :: sobek1d2dv
    real(dp), dimension(:), pointer :: sobek1d2dc
    real(dp), dimension(:), pointer :: sobekvr
    real(dp), dimension(:), pointer :: rrh
    real(dp), dimension(:), pointer :: rainh
    real(dp), dimension(:), pointer :: sobekc
    real(dp), dimension(:), pointer :: sobeks
    real(dp), dimension(:), pointer :: rrslowon
    real(dp), dimension(:), pointer :: rrslowoff
    real(dp), dimension(:), pointer :: rrshighon
    real(dp), dimension(:), pointer :: rrshighoff
    real(dp), dimension(:), pointer :: d3dh
    real(dp), dimension(:), pointer :: d3dsal
    real(dp), dimension(:,:), pointer :: sobekwq

    real(dp), pointer, save :: dllsobekh(:) , dllsobekq (:), dllsobeksa(:), dllsobekwd(:), &
                               dllsobekcl(:), dllsobekcw(:), dllsobekgl(:), dllsobekgo(:), &
                               dllsobekfa(:), dllsobekqs(:), dllsobekvs(:), dllsobekhu(:), &
                               dllsobekhd(:), dllsobekdh(:), dllsobekpd(:), dllsobekpc(:), &
                               dllsobek1d2dbl(:), dllsobek1d2du(:), dllsobek1d2dv(:), dllsobek1d2dc(:), &
                               dllsobekvr(:)
    real(dp), pointer, save :: dllrrh(:), dllrainh(:)
    real(dp), pointer, save :: dlld3dh(:), dlld3dsal(:)
    real(dp), pointer, save :: dllsobekc(:), dllsobeks(:)
    real(dp), pointer, save :: dllsobekwq(:,:)

    character(len=CharIdLength), pointer, save :: dllidsbr(:)
    character(len=CharIdLength), pointer, save :: dllidd3b(:)
    character(len=CharIdLength), pointer, save :: dllidpre(:)
    character(len=CharIdLength), pointer, save :: dllidd3d(:)
    character(len=CharIdLength), pointer, save :: dlllocidwq(:)
    character(len=CharIdLength), pointer, save :: dllparidwq(:)
    character(len=CharIdLength), pointer, save :: dllmeasid(:)


    integer, save      :: lulog, lusum

    integer            :: dll_handle, dll_test
    Character(256)     :: dll_function

 contains

! set_pointers --
!     Set the pointers to the various relevant array sections
!
! Arguments:
!     alrsbk            All results of SOBEK (current time)
!     alrs3b            All results of SOBEK-RR (current time)
!     alrspr            All results of "Rain" module (current time)
!     id_sbk            IDs for SOBEK locations
!     id_d3b            IDs for SOBEK_RR locations
!     id_swq            IDs for SOBEK_WQ locations
!     id_pre            IDs for "Rain" locations
!
! Or should this be an include file?
!

subroutine set_pointers( alrsbk, alrs3b, alrswq, alrspr, alrs3d, dcvval, paraid )
    ! AM: Not needed now:, id_sbk, id_d3b, id_swq, id_pre )
    real(dp), dimension(:,:), target :: alrsbk
    real(dp), dimension(:,:), target :: alrs3b
    real(dp), dimension(:,:,:), target :: alrswq
    real(dp), dimension(:,:), target :: alrspr
    real(dp), dimension(:,:), target :: alrs3d
    real(dp), dimension(:),   target :: dcvval
    character(len=*), dimension(:)   :: paraid
   !character(len=*), dimension(:), target :: id_sbk
   !character(len=*), dimension(:), target :: id_swq
   !character(len=*), dimension(:), target :: id_d3b
   !character(len=*), dimension(:), target :: id_pre

    integer :: i
    integer :: start_m

    sobekh        => alrsbk(:,isobekh)
    sobekq        => alrsbk(:,isobekq)
    sobeksa       => alrsbk(:,isobeksa)
    sobekwd       => alrsbk(:,isobekwd)
    sobekcl       => alrsbk(:,isobekcl)
    sobekcw       => alrsbk(:,isobekcw)
    sobekgl       => alrsbk(:,isobekgl)
    sobekgo       => alrsbk(:,isobekgo)
    sobekfa       => alrsbk(:,isobekfa)
    sobekqs       => alrsbk(:,isobekqs)
    sobekvs       => alrsbk(:,isobekvs)
    sobekhu       => alrsbk(:,isobekhu)
    sobekhd       => alrsbk(:,isobekhd)
    sobekdh       => alrsbk(:,isobekdh)
    sobekpd       => alrsbk(:,isobekpd)
    sobekpc       => alrsbk(:,isobekpc)
    sobek1d2dh    => alrsbk(:,isobek1d2dh)
    sobek1d2dwd   => alrsbk(:,isobek1d2dwd)
    sobek1d2dbl   => alrsbk(:,isobek1d2dbl)
    sobek1d2du    => alrsbk(:,isobek1d2du)
    sobek1d2dv    => alrsbk(:,isobek1d2dv)
    sobek1d2dc    => alrsbk(:,isobek1d2dc)
    sobekvr       => alrsbk(:,isobekvr)
    rrh           => alrs3b(:,irrh)
    rainh         => alrspr(:,irainh)
    d3dh          => alrs3d(:,id3dh)
    d3dsal        => alrs3d(:,id3dsal)
    sobekwq       => alrswq(:,:,ntims)

    !
    ! Decision variables: position is shifted
    !
!   sobekc        => ...(:)    <= AM: it seems this is not used!

    start_m = 1
    do i = 1,size(paraid)
        if ( index(paraid(i), 'Dll' ) == 1 ) then
            start_m = i
            exit
        endif
    enddo


    sobeks        => dcvval(start_m:)
!   rrslowon      => input(irrslowon,:)
!   rrslowoff     => input(irrslowoff,:)
!   rrshighon     => input(irrshighon,:)
!   rrshighoff    => input(irrshighoff,:)

    ! Something with the location-IDs

end subroutine set_pointers


! location_index --
!     Find the index of the location ID
!
! Arguments:
!     name            Name (ID) of the location
!     listids         List of IDs for the given type
!
!
! Result:
!     Index in the local arrays
!
integer function name_index(name, listids)
    character(len=*)               :: name
    character(len=*), dimension(:) :: listids

    character(len=len(name))       :: name_low

    integer :: i

    name_index = -1
    name_low   = lowercase(name)
    do i = 1,size(listids)
        if ( name_low == lowercase(listids(i)) ) then
            name_index = i
            exit
        endif
    enddo
end function name_index


! lowercase --
!     Return a string in lower case
!
! Arguments:
!     string          String to be converted
!
! Result:
!     Strign converted to lower case
!
function lowercase(string)
    character(len=*)               :: string
    character(len=len(string))     :: lowercase

    integer :: i
    integer :: k
    character(len=26)              :: upcase = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=26)              :: locase = 'abcdefghijklmnopqrstuvwxyz'

    do i = 1,len(string)
        k = index( upcase, string(i:i) )
        if ( k > 0 ) then
            lowercase(i:i) = locase(k:k)
        else
            lowercase(i:i) = string(i:i)
        endif
    enddo
end function lowercase

!
! free_lun --
!     Determine a free LU-number for reading/writing files
!
! Arguments:
!     lun             New LU-number
!
! Note:
!     Use this routine because we do not want to hinder RTC
!
subroutine free_lun( lun )

    integer, intent(out) :: lun

    integer :: i
    logical :: opend

    lun = -1

    do i = 10,99
        inquire( i, opened = opend )
        if ( .not. opend ) then
            lun = i
            exit
        endif
    enddo

end subroutine free_lun



subroutine eval_measures( sobekfirst, sobekdate, sobektime, sobekcomptimestepsize, &
                          id_sbr, id_slc, id_d3b, id_swq, id_pre, measbp )


    integer, intent(in)  :: sobekfirst
    integer, intent(in)  :: sobekdate
    integer, intent(in)  :: sobektime
    integer, intent(in)  :: sobekcomptimestepsize

    character(len=*), dimension(:) :: id_sbr
    double precision, dimension(:) :: id_slc   ! AM: strange type
    character(len=*), dimension(:) :: id_swq
    character(len=*), dimension(:) :: id_d3b
    character(len=*), dimension(:) :: id_pre
    character(len=*), dimension(:) :: measbp

    real(dp) :: pi = 3.14159265359d0

    real(dp) :: evaluate_breaching, D_aquifer, L_Piping, d_toplayer,     &
                h_hinterDefault, h_dike, tanalphai, gamma_sat, gamma_w,  &
                k_permeability, d70, nu, eta, theta, gamma_g,            &
                cg, te, KStrickler, m0, mh, mp, me, B0, f1, f2, T0, uc,  &
                Z_p, Z_h, Z_o, Z_e, h, hup, hdown

    real(dp) :: crestvelocity, crestflow, h_hinter, h_river, h_dikering
    real(dp) :: newlevel, newwidth
    real(dp) :: hmax

    real(dp), save :: v0_piping        = 0.5/3600.0  ! speed crest level lowering first phase: 0.5 m/h
    integer, save  :: PipingFirstPhase = 3*3600      ! first phase lasts 3 hours

    integer, save :: no_dikebreach_locations
    real(dp), dimension(:),   pointer, save        :: zmin
    real(dp), dimension(:,:), pointer, save        :: proboxlocinfo
    character(len=60), dimension(:), pointer, save :: location_id


    real(dp), dimension(:), pointer, save          :: crestlevel
    real(dp), dimension(:), pointer, save          :: crestwidth
    real(dp), dimension(:), pointer, save          :: qmaxpos
    real(dp), dimension(:), pointer, save          :: qmaxneg
    integer, dimension(:), pointer, save           :: Failed
    integer, dimension(:), pointer, save           :: DateFailed
    integer, dimension(:), pointer, save           :: TimeFailed
    integer, dimension(:), pointer, save           :: SecondsSinceTimeZmin
    integer, dimension(:), pointer, save           :: SecondsSinceStartPiping
    integer, dimension(:), pointer, save           :: FailedDueToHP
    integer, dimension(:), pointer, save           :: FailedDueToOE
    integer, dimension(:), pointer, save           :: FailedFrom

    ! The moments of failure are taken as double precision reals to ease the printing
    real(kind=kind(1.0d00)), dimension(:), pointer, save           :: TimeZminReached
    real(kind=kind(1.0d00)), dimension(:), pointer, save           :: TimeFailed_HP
    real(kind=kind(1.0d00)), dimension(:), pointer, save           :: TimeFailed_OE

    ! Translate codes into strings
    character(len=10), dimension(0:1), save :: hasfailed = (/ 'No failure', 'Failure   ' /)
    character(len=10), dimension(0:4), save :: hp_type   = (/ 'N/A       ', 'Heave&Pipe',&
                                                              'HeaveNoPip', 'Pot.Piping',&
                                                              'Neither   '               /)
    character(len=10), dimension(0:3), save :: oe_type   = (/ 'N/A       ', 'Overf&Eros',&
                                                              'OverfNoEro', 'No overfl.' /)
    character(len=10), dimension(0:2), save :: from_type = (/ 'N/A       ', 'From river',&
                                                              'FromHinter'               /)

    character(len=15), dimension(3)         :: dates
    character(len=60)                       :: scenid
    logical                                 :: terminate

    integer  :: idx_level, idx_width

    integer  :: i, id, ir, idike
!!  integer, save :: lulog

    integer, save :: RtcMatlab_Timestep
    integer, save :: prev_time
    integer, save :: curr_time
    logical, save :: print_data
    integer, parameter :: print_step = 10 ! Print at this interval, that is: each hour

    if ( sobekfirst == 1 ) then
        RtcMatlab_Timestep = SobekCompTimestepSize
        prev_time          = 0 - print_step - 1
        curr_time          = 0

        !
        ! Open input file and read it
        !
        call read_info( '..\fixed\arjen\proboxgeneral.inp', proboxlocinfo, location_id, no_dikebreach_locations, &
                        scenid )
        call read_zmin( '..\fixed\arjen\zmin.dat', zmin, no_dikebreach_locations )

        allocate( Failed(1:no_dikebreach_locations) )
        allocate( DateFailed(1:no_dikebreach_locations) )
        allocate( TimeFailed(1:no_dikebreach_locations) )
        allocate( SecondsSinceTimeZmin(1:no_dikebreach_locations) )
        allocate( SecondsSinceStartPiping(1:no_dikebreach_locations) )
        allocate( TimeFailed_HP(1:no_dikebreach_locations) )
        allocate( TimeFailed_OE(1:no_dikebreach_locations) )
        allocate( TimeZminReached(1:no_dikebreach_locations) )
        allocate( FailedDueToHP(1:no_dikebreach_locations) )
        allocate( FailedDueToOE(1:no_dikebreach_locations) )
        allocate( FailedFrom(1:no_dikebreach_locations) )

        allocate( crestlevel(1:no_dikebreach_locations) )
        allocate( crestwidth(1:no_dikebreach_locations) )
        allocate( qmaxpos(1:no_dikebreach_locations) )
        allocate( qmaxneg(1:no_dikebreach_locations) )

        Failed             = 0                        ! failure already occured? 0=no, 1=yes
        DateFailed         = 0                        ! date of failure
        TimeFailed         = 0                        ! time of failure
        DateFailed         = DateFailed -999
        TimeFailed         = TimeFailed -999
        SecondsSinceTimeZmin = 0                      ! seconds since time of reaching zmin
        SecondsSinceStartPiping = 0                   ! seconds since start of piping
        TimeFailed_HP      = -999d0
        TimeFailed_OE      = -999d0
        TimeZminReached    = -999d0

        Failed             = 0                        ! Initialise these arrays once
        FailedDueToHP      = 0
        FailedDueToOE      = 0
        FailedFrom         = 0

        ! Get the crest level and initial width directy from the probox information
        crestlevel    = proboxlocinfo(:,6)
        crestwidth    = proboxlocinfo(:,23)
        qmaxpos       = 0.0
        qmaxneg       = 0.0

        ! Detailed log file
        call free_lun( lulog )
        open( lulog, file = '..\fixed\arjen\systeemwerking.txt' )

        call report_info( proboxlocinfo, zmin, location_id, scenid )

        write(lulog,'(a)')'location  failed         date    time  from       piping    &
            &overflow              zmin          start            start     crest     crest  max.flow  max.flow'
        write(lulog,'(a)')'                             failure              & heave   &
            && erosion          reached   piping&heave   overfl&eros.     level     width  positive  negative'
    endif

    ! Summary file - opened each time step
    call free_lun( lusum )
    open( lusum, file = '..\fixed\arjen\samenvatting.txt' )
    write(lusum,'(2a)')'Scenario: ',trim(scenid)
    write(lusum,'(a)')'location  failed         date    time  from       piping    &
       &overflow              zmin          start            start     crest     crest  max.flow  max.flow'
    write(lusum,'(a)')'                             failure              & heave   &
       && erosion          reached   piping&heave   overfl&eros.     level     width  positive  negative'

    print_data = .false.
    if ( abs(curr_time-prev_time) >= print_step ) then
        print_data = .true.
        prev_time  = curr_time
        write(lulog,'(a,2i8)' ) 'Time: ', sobekdate, sobektime
    endif
    curr_time = curr_time + RtcMatlab_Timestep
    terminate = .false.

    do idike = 1, no_dikebreach_locations

        i  = name_index( trim(location_id(idike))              , id_sbr )
        ir = name_index( trim(location_id(idike))//"_river"    , id_sbr )
        id = name_index( trim(location_id(idike))//"_dikering" , id_sbr )
        write(lulog,*) ' idike i ir id ', idike, i, ir, id

        evaluate_breaching = proboxlocinfo (idike,1)     ! 0 = no evaluation, 1=only heave/piping, 2=only overflow/erosion 3=evaluation all mechanisms

        if ( i .lt. 0 .or. ir .lt. 0 .or. id .lt. 0 ) then
            if ( sobekfirst == 1 .and. evaluate_breaching > 0 ) then
                write(*,*) 'Error finding a location!'
                write(*,*) 'Dike breach location: ', idike
                write(*,*) i, trim(location_id(idike))
                write(*,*) ir, trim(location_id(idike))//"_river"
                write(*,*) id, trim(location_id(idike))//"_dikering"
                terminate = .true.
            endif
            cycle ! Simply skip this location
        endif

        !
        ! Get the current data for this location
        !
        crestvelocity = sobekvs(i)
        crestflow     = sobekqs(i)

        qmaxpos(idike) = max( qmaxpos(idike), crestflow )
        qmaxneg(idike) = min( qmaxneg(idike), crestflow )

        h_river       = sobek1d2dh(ir)
        h_dikering    = sobek1d2dh(id)

        !
        ! Get the parameters for this location
        !
       !evaluate_breaching = proboxlocinfo (idike,1)     ! 0 = no evaluation, 1=only heave/piping, 2=only overflow/erosion 3=evaluation all mechanisms
        D_aquifer          = proboxlocinfo (idike,2)     ! aquifer thickness [m]
        L_Piping           = proboxlocinfo (idike,3)     ! piping length [m]
        d_toplayer         = proboxlocinfo (idike,4)     ! thickness impermeable top layer [m]
        h_hinterDefault    = proboxlocinfo (idike,5)     ! hydraulic head hinterland [m+NAP]
        h_dike             = proboxlocinfo (idike,6)     ! dike height [m+NAP]
        tanalphai          = proboxlocinfo (idike,7)     ! tan of the inner slope angle [-]

        ! Heave
        gamma_sat          = proboxlocinfo (idike,8)     ! saturated volumetric soil weight [kN/m3]
        gamma_w            = proboxlocinfo (idike,9)     ! volumetric weight water [kN/m3]

        ! Piping
        k_permeability     = proboxlocinfo (idike,10)     ! specific permeability [m/s]
        d70                = proboxlocinfo (idike,11)     ! 70th percentile grain distribution [m]
        nu                 = proboxlocinfo (idike,12)     ! kinematic viscosity [m2/s]
        eta                = proboxlocinfo (idike,13)     ! White constant (sleepkrachtfactor) [-]
        theta              = proboxlocinfo (idike,14)     ! rolling resistance angle [rad]
        gamma_g            = proboxlocinfo (idike,15)     ! volumetric weight of the grains [kN/m3]

        ! Erosion inner slope
        cg                 = proboxlocinfo (idike,16)     ! grass quality coefficient [-]
        te                 = proboxlocinfo (idike,17)     ! critical velocity exceedance duration [h]
        KStrickler         = proboxlocinfo (idike,18)     ! roughness coefficient by Strickler [m]

        !  Model factors
        m0                 = proboxlocinfo (idike,19)     ! model uncertainty critical head difference for heave [-]
        mh                 = proboxlocinfo (idike,20)     ! model uncertainty damping head difference (assymetric) [-]
        mp                 = proboxlocinfo (idike,21)     ! model uncertainty Sellmeijer [-]
        me                 = proboxlocinfo (idike,22)     ! model uncertainty erosion inner slope [-]

        ! Breach growth parameters
        B0                 = proboxlocinfo (idike,23)     ! Initial width of dike breach
        f1                 = proboxlocinfo (idike,24)     ! constant factor f1
        f2                 = proboxlocinfo (idike,25)     ! constant factor f2
        T0                 = proboxlocinfo (idike,26)     ! time period for lowering from initial crest level to minimum crest level in hours
        uc                 = proboxlocinfo (idike,27)     ! constant critical flow velociy sediment/soil [m/s]  SHOULD BE > 0 !!!!

        ! zmin already read from separate file   ! minimum crest level optional via Sobek1D2DBL van omliggende history stations

        Z_p                = 0                        ! Z_p piping
        Z_h                = 0                        ! Z_h heave
        Z_o                = 0                        ! Z_o overflow
        Z_e                = 0                        ! Z_e erosion

        h             = max( h_river, h_hinterdefault )
        h_hinter      = max( h_dikering, h_hinterdefault )

        !
        ! Check for inconsistency
        !
        if ( h_dike < zmin(idike) ) then
             write(*,*) 'ERROR: Water level "hinterland" below bottom - ', location_id(idike)
             write(*,*) 'h_dike: ', h_dike
             write(*,*) 'zmin:   ', zmin(idike)
             terminate = .true.
        endif

        !
        ! Determine if any of the failure two mechanisms occurs
        !
        if ( evaluate_breaching == 0 ) then
            cycle ! Skip the location: no failure mechanism to be evaluated
        endif

        !
        ! Determine - at each time - if a new mechanism has become
        ! active. But careful: once a mechanism has been active for a time,
        ! the associated information should not be overwritten.
        !
        write(lulog,*) ' Call failure for location idike=', idike
        write(lulog,*) '   i iriver idikering =',i, ir, id

        ! changed to match original Matlab implementation
        if (Failed(idike) .eq. 0) then
           call failure( h, D_aquifer,L_Piping,d_toplayer,h_hinter,h_dike, &
                         tanalphai,gamma_sat,gamma_w, &
                         k_permeability,d70,nu,eta,theta,gamma_g, &
                         cg,te,KStrickler,m0,mh,mp,me, evaluate_breaching, &
                         Z_h, Z_p, Z_o, Z_e, Failed(idike), FailedDueToHP(idike), &
                         FailedDueToOE(idike), FailedFrom(idike) )

           write(lulog,*) 'Location:     ',location_id(idike)
           write(lulog,*) '      h_dike: ', h_dike
           write(lulog,*) '  crestlevel: ', crestlevel(idike)
           write(lulog,*) '        zmin: ', zmin(idike)
           write(lulog,*) '     h_river: ', h_river
           write(lulog,*) '    h_hinter: ', h_hinter
           write(lulog,*) '   FailedHP : ', FailedDueToHP(idike)
           write(lulog,*) '   FailedOE : ', FailedDueToOE(idike)
           write(lulog,*) '  FailedFrom: ', FailedFrom(idike)

           if ( Failed(idike) == 1 ) then
               ! GP added to match original Matlab implementation
               ! lowering crestlevel, dikeheigt, as result of initial dike damage
               if (TimeFailed_HP(idike) <= -999.0d0 .and. TimeFailed_OE(idike) <= -999.0d0) then
                 hmax = max (h_river, h_dikering)
                 if (hmax > h_dike) hmax = h_dike
                 CrestLevel(idike) = max ( hmax - 0.1, zmin(idike) + 0.05)
                 h_dike = CrestLevel (idike)
                 ! addition to match original Matlab implementation
                 proboxlocinfo(idike,6) = CrestLevel (idike)
               endif
           endif

        endif

        if ( Failed(idike) == 1 ) then

            !
            ! Register the _first_ time the dike failed
            !
            if ( FailedDueToHP(idike) == 1 .and. TimeFailed_HP(idike) /= -999.0d0 ) then
                TimeFailed_HP(idike) = sobekdate + sobektime / 1.0d5
            endif
            if ( FailedDueToOE(idike) == 1 .and. TimeFailed_OE(idike) /= -999.0d0 ) then
                TimeFailed_OE(idike) = sobekdate + sobektime / 1.0d5
            endif

            ! hup:   highest water level not corrected for "h_hinterDefault"
            ! hdown: lowest water level not corrected for "h_hinterDefault"

            hup   = max (h_river, h_dikering) ! hup highest
            hdown = min (h_river, h_dikering)
            call verheyvdknaap (B0, f1, f2, h_dike, PipingFirstPhase, v0_piping, &
                                CrestLevel(idike), CrestWidth(idike), zmin(idike), T0, uc, &
                                CrestVelocity, hup, hdown, RtcMatlab_Timestep, &
                                SecondsSinceTimeZmin(idike), SecondsSinceStartPiping(idike), &
                                FailedDueToHP(idike), FailedDueToOE(idike), &
                                newLevel, newWidth)

            CrestLevel(idike) = newLevel
            CrestWidth(idike) = newWidth

            if ( CrestLevel(idike) <= zmin(idike) ) then
                SecondsSinceTimeZmin(idike) = SecondsSinceTimeZmin(idike) + RtcMatlab_Timestep
                if ( TimeZminReached(idike) /= -999.0d0 ) then
                    TimeZminReached = sobekdate + sobektime/1.0d5
                endif
            endif
        endif

        !
        ! Store the results
        !
        idx_level = name_index( trim(location_id(idike)) // '_Crestlevel', measbp)
        idx_width = name_index( trim(location_id(idike)) // '_Crestwidth', measbp)
        write(lulog,*) ' idike idx_level idx_width ', idike, idx_level, idx_width

        if ( idx_level < 0 .or. idx_width < 0 ) then
            write(*,*) 'Problem with ', trim(id_sbr(i))
            if ( idx_level < 0 ) write(*,*) '    Measure ', trim(location_id(idike)) // '_Crestlevel not found'
            if ( idx_width < 0 ) write(*,*) '    Measure ', trim(location_id(idike)) // '_Crestwidth not found'
            write(*,*) 'Available measures:'
            write(*,'(3x,a)') measbp
            terminate = .true.
        endif
!        sobekc(idx_level) = 1          <== Not sure if this is used!
!        sobekc(idx_width) = 1
        sobeks(idx_level) = crestlevel(idike)
        sobeks(idx_width) = crestwidth(idike)


        dates(1) = datetime(TimeZminReached(idike))
        dates(2) = datetime(TimeFailed_HP(idike))
        dates(3) = datetime(TimeFailed_OE(idike))

       !write(lulog,'(2a11,2i8,3a11,3a15,6f10.2)') &
        if ( print_data ) then
            write(lulog,'(2a11,2i8,3a11,3a15,4f10.2,2f12.7)') &
                location_id(idike), &
                hasfailed(Failed(idike)), &
                DateFailed(idike), TimeFailed(idike), &
                from_type(FailedFrom(idike)), &
                hp_type(FailedDueToHP(idike)), &
                oe_type(FailedDueToOE(idike)), &
                dates, &
                CrestLevel(idike), CrestWidth(idike), &
                Qmaxpos(idike), Qmaxneg(idike), &
                h_river, h_dikering
        endif

        write(lusum,'(2a11,2i8,3a11,3a15,4f10.2)') &
            location_id(idike), &
            hasfailed(Failed(idike)), &
            DateFailed(idike), TimeFailed(idike), &
            from_type(FailedFrom(idike)), &
            hp_type(FailedDueToHP(idike)), &
            oe_type(FailedDueToOE(idike)), &
            dates, &
            CrestLevel(idike), CrestWidth(idike), &
            Qmaxpos(idike), Qmaxneg(idike)

    enddo

    close( lusum )

    !
    ! We have scanned the whole input now, we were either successful or not
    ! If not, simply stop.
    !
    if ( terminate ) stop

end subroutine eval_measures


! datetime --
!    Format the date and time in a pleasant way
!
character(len=15) function datetime( timevalue )
     real(kind=kind(1.0d0)) :: timevalue

     write( datetime, '(f15.6)' ) timevalue
     datetime(9:9) = ' '
end function datetime


! sellmeijer --
!    Critical head difference for piping according to Sellmeijer
!
real(dp) function sellmeijer(D,L,d70,k,nu,eta,theta,gamma_g,gamma_w)
    real(dp) :: D,L,d70,k,nu,eta,theta,gamma_g,gamma_w

    real(dp) :: kappa, alpha, c, dh_cp

    ! Auxiliary variables / coefficients:
    kappa = nu / 9.81 * k                    ! intrinsic conductivity [m2]
    c = eta * d70 * (1/(kappa*L))**(1.0/3.0) ! coefficient [m^(2/3)]
    alpha = (D/L) ** (0.28 / ((D/L)**2.8-1)) ! coefficient [-]

    ! Sellmeijer formula:
    dh_cp = alpha * c * L * (gamma_g/gamma_w-1) * &
        (0.68 - 0.1 * log(c)) * tan(theta)   ! critical head difference

    ! Alternative formulation (slightly different results -> check constants!)
    ! beta = 0.28 / ((D/L)^2.8-1)
    ! dh_cp = D^beta * L^(1-beta) * (g*d70^3/nu/k/L)^(1/3) * ...
    ! (0.25 - 0.037*log((g*d70^3/nu/k/L)^(1/3)))

    sellmeijer = dh_cp

end function sellmeijer


! heave --
!     Critical head difference for heave
!
real(dp) function heave(gamma_sat,d,gamma_w)
    real(dp) :: gamma_sat,d,gamma_w

    ! Heave formula:
    heave = (gamma_sat - gamma_w) / gamma_w * d  ! critical head difference

end function heave


! verheyvdknaap --
!    Routine for implementing Verhey-vdKnaap
!
subroutine VerheyvdKnaapArjenMarkus (B0, f1, f2, h_dike, PipingFirstPhase, v0_piping, &
    CrestLevel, CrestWidth, zmin, T0, uc, &
    uact, hup, hdown, RtcMatlab_Timestep, SecondsSinceTimeZmin, SecondsSinceStartPiping, &
    FailedDueToHP, FailedDueToOE, newlevel, newwidth )

    real(dp) :: B0, f1, f2, h_dike, CrestLevel, CrestWidth, zmin, T0, uc, &
                uact, hup, hdown, newlevel, newwidth,  v0_piping
    integer  :: RtcMatlab_Timestep, SecondsSinceTimeZmin, SecondsSinceStartPiping
    integer  :: FailedDueToHP, FailedDueToOE, PipingFirstPhase

    real(dp) :: help1, help2, help3, help4, hdown2, dbdt

    ! gravitation constant
    real(dp), parameter :: g     = 9.81d0

    write(lulog,*)  ' Start VerheyvdKnaap with input data'
    write(lulog,*)  ' B0     = ', B0
    write(lulog,*)  ' f1     = ', f1
    write(lulog,*)  ' f2     = ', f2
    write(lulog,*)  ' h_dike = ', h_dike
    write(lulog,*)  ' PipingFirstPhase    = ', PipingFirstPhase
    write(lulog,*)  ' v0_piping = ', v0_piping
    write(lulog,*)  ' CrestLevel= ', CrestLevel
    write(lulog,*)  ' CrestWidth= ', CrestWidth
    write(lulog,*)  ' zmin      = ', zmin
    write(lulog,*)  ' T0        = ', T0
    write(lulog,*)  ' uc        = ', uc
    write(lulog,*)  ' uact      = ', uact
    write(lulog,*)  ' hup       = ', hup
    write(lulog,*)  ' hdown     = ', hdown
    write(lulog,*)  ' RtcTimestep=', RtcMatlab_Timestep
    write(lulog,*)  ' SecondsSinceTimeZmin= ',   SecondsSinceTimeZmin
    write(lulog,*)  ' SecondsSinceStartPiping=', SecondsSinceStartPiping
    write(lulog,*)  ' newlevel  = ', newlevel
    write(lulog,*)  ' newwidth  = ', newwidth
    !
    ! If the crest level has not reached the minimum level yet, then
    ! we need to consider the mechanisms that reduce the level in
    ! parallel (that is: they add up)
    !
    if ( CrestLevel > zmin ) then
        !
        ! Piping and heave:
        ! - Two phases distinguished in dike breach growth due to piping and heave
        !
        if ( FailedDueToHP == 1 ) then
            SecondsSinceStartPiping = SecondsSinceStartPiping + RtcMatlab_Timestep
            if ( SecondsSinceStartPiping <= PipingFirstPhase ) then
                CrestLevel = CrestLevel - v0_piping * RtcMatlab_Timestep
            else
                CrestLevel = CrestLevel - (h_dike-zmin) / T0 * RtcMatlab_Timestep / 3600.0
            endif
        endif
        write(lulog,*)  ' Computed VerheyvdKnaap piping/heave'
        write(lulog,*)  ' CrestLevel ', CrestLevel
        write(lulog,*)  ' CrestWidth ', CrestWidth

        !
        ! Overflow and erosion:
        ! Just a single phase - detect that it is happening
        !
        if ( FailedDueToOE == 1 ) then
            CrestLevel = CrestLevel - (h_dike-zmin) / T0 * RtcMatlab_Timestep / 3600.0
        endif

        !
        ! During this period the crest width is fixed (and the crest level can not be lower
        ! than zmin)
        !
        if ( CrestLevel <= zmin ) then
            CrestLevel = zmin
        endif
        CrestWidth = B0
    endif

    write(lulog,*)  ' Computed VerheyvdKnaap overflow/erosion'
    write(lulog,*)  ' CrestLevel ', CrestLevel
    write(lulog,*)  ' CrestWidth ', CrestWidth
    !
    ! If the crest level has reached the minimum level, the crest width can increase,
    ! but this is independent of the precise mechanism. If two mechanisms are active,
    ! the effects do _not_ add up
    !
    ! Growth of dike breach: only if velocity exceeds critical value
    !
    if ( CrestLevel <= zmin .and. SecondsSinceTimeZmin > 0 .and. abs(uact) > uc ) then
        dbdt = 0
        if ( hup < zmin .and. hdown < zmin ) then
            ! no change of CrestWidth
            dbdt = 0
        else
            if ( hup == hdown) then
                dbdt = 0
            else
                hdown2 = max (hdown, zmin)
                help1 = f1 * f2  / log(10.0)
                help2 = (g * (hup-hdown2)) ** 1.5
                help3 = f2 * g / uc
                help4 = help3 * (SecondsSinceTimeZmin/3600.0) + 1
                dbdt  = help1 * help2 / help4 / uc / uc
            endif
        endif
        dbdt = max (0.0, dbdt)
        CrestWidth = CrestWidth + dbdt * RtcMatlab_Timestep / 3600.0
    endif

    newlevel = CrestLevel
    newwidth = CrestWidth

    write(lulog,*)  ' Computed VerheyvdKnaap breach growth'
    write(lulog,*)  ' CrestLevel ', CrestLevel
    write(lulog,*)  ' CrestWidth ', CrestWidth

end subroutine verheyvdknaapArjenMarkus

! this routine matches the original Matlab implementation
subroutine VerheyvdKnaap (B0, f1, f2, h_dike, PipingFirstPhase, v0_piping, &
    CrestLevel, CrestWidth, zmin, T0, uc, &
    uact, hup, hdown, RtcMatlab_Timestep, SecondsSinceTimeZmin, SecondsSinceStartPiping, &
    FailedDueToHP, FailedDueToOE, newlevel, newwidth )


    real(dp) :: B0, f1, f2, h_dike, CrestLevel, CrestWidth, zmin, T0, uc, &
                uact, hup, hdown, newlevel, newwidth,  v0_piping
    integer  :: RtcMatlab_Timestep, SecondsSinceTimeZmin, SecondsSinceStartPiping
    integer  :: FailedDueToHP, FailedDueToOE, PipingFirstPhase

    real(dp) :: help1, help2, help3, help4, hdown2, dbdt

    ! gravitation constant
    real(dp), parameter :: g     = 9.81d0

    write(lulog,*)  ' Start VerheyvdKnaap with input data'
    write(lulog,*)  ' B0     = ', B0
    write(lulog,*)  ' f1     = ', f1
    write(lulog,*)  ' f2     = ', f2
    write(lulog,*)  ' h_dike = ', h_dike
    write(lulog,*)  ' PipingFirstPhase    = ', PipingFirstPhase
    write(lulog,*)  ' v0_piping = ', v0_piping
    write(lulog,*)  ' CrestLevel= ', CrestLevel
    write(lulog,*)  ' CrestWidth= ', CrestWidth
    write(lulog,*)  ' zmin      = ', zmin
    write(lulog,*)  ' T0        = ', T0
    write(lulog,*)  ' uc        = ', uc
    write(lulog,*)  ' uact      = ', uact
    write(lulog,*)  ' hup       = ', hup
    write(lulog,*)  ' hdown     = ', hdown
    write(lulog,*)  ' RtcTimestep=', RtcMatlab_Timestep
    write(lulog,*)  ' SecondsSinceTimeZmin= ',   SecondsSinceTimeZmin
    write(lulog,*)  ' SecondsSinceStartPiping=', SecondsSinceStartPiping
    write(lulog,*)  ' newlevel  = ', newlevel
    write(lulog,*)  ' newwidth  = ', newwidth
    !
    ! lowering of dike: constant speed going down from CrestLevel to zmin in T0 hours
    !
    if ( (abs(uact) > uc) .and. (CrestLevel > zmin ) ) then
        CrestLevel = max (zmin, (CrestLevel - (h_dike-zmin) / T0 * RtcMatlab_Timestep / 3600.0 ))
    endif
    write(lulog,*) ' Computed Verhey/vdKnaap piping/heave'
    write(lulog,*) ' CrestLevel =', Crestlevel
    write(lulog,*) ' CrestWidth =', Crestwidth

    ! Growth of dike breach: only if velocity exceeds critical value

    if ( (CrestLevel <= zmin) .and. (SecondsSinceTimeZmin > 0) ) then
        if ( hup < zmin .and. hdown < zmin ) then
            ! no change of CrestWidth
            dbdt = 0
        else
            if ( hup == hdown) then
                dbdt = 0
            else
                hdown2 = max (hdown, zmin)
                help1 = f1 * f2  / log(10.0)
                help2 = (g * (hup-hdown2)) ** 1.5
                help3 = f2 * g / uc
                help4 = help3 * (SecondsSinceTimeZmin/3600.0) + 1
                dbdt  = help1 * help2 / help4 / uc / uc
            endif
        endif
        dbdt = max (0.0, dbdt)
        CrestWidth = CrestWidth + dbdt;
    endif

    write(lulog,*) ' Computed Verhey/vdKnaap overflow/erosion'
    write(lulog,*) ' CrestLevel =', Crestlevel
    write(lulog,*) ' CrestWidth =', Crestwidth
    newlevel = CrestLevel
    newwidth = CrestWidth

end subroutine verheyvdknaap



! failure --
!     Test routine for implementation of dike failure mechanisms
!     (for probabilistic calculation scheme Delft Cluster)
!
subroutine failure(hriver,DD,L,d_d,hhinter,h_d,tanalphai,gamma_sat,gamma_w, &
    k_p,d70,nu,eta,theta,gamma_g,cg,te,KK,m0,mh,mp,me, evaluate_breaching, &
    Z_h, Z_p , Z_o, Z_e, fail, FailedDueToHP, FailedDueToOE, failedfrom )

    real(dp) :: hriver,DD,L,d_d,hhinter,h_d,tanalphai,gamma_sat,gamma_w, &
                k_p,d70,nu,eta,theta,gamma_g,cg,te,KK,m0,mh,mp,me, evaluate_breaching, &
                Z_h, Z_p , Z_o, Z_e
    integer  :: fail, FailedDueToHP, FailedDueToOE, failedfrom

    logical  :: evaluate_heaveandpiping, evaluate_overflowanderosion
    real(dp) :: dh_ch, dh_cp, v_c, q_c, dh_e, h, h_hinter

    write(lulog,*) ' Call failure with the following parameters'
    write(lulog,*) ' hriver    = ',hriver
    write(lulog,*) ' D_aquifer = ',DD
    write(lulog,*) ' L         = ',L
    write(lulog,*) ' d_toplayer= ',d_d
    write(lulog,*) ' hhinter   = ',hhinter
    write(lulog,*) ' h_dike    = ',h_d
    write(lulog,*) ' hriver    = ',hriver
    write(lulog,*) ' tanalphai = ',tanalphai
    write(lulog,*) ' gammasat  = ',gamma_sat
    write(lulog,*) ' gamma_w   = ',gamma_w
    write(lulog,*) ' k_permeab = ',k_p
    write(lulog,*) ' d70       = ',d70
    write(lulog,*) ' nu        = ',nu
    write(lulog,*) ' eta       = ',eta
    write(lulog,*) ' theta     = ',theta
    write(lulog,*) ' gamma_g   = ',gamma_g
    write(lulog,*) ' cg        = ',cg
    write(lulog,*) ' te        = ',te
    write(lulog,*) ' KStrickler= ',KK
    write(lulog,*) ' m0        = ',m0
    write(lulog,*) ' mh        = ',mh
    write(lulog,*) ' mp        = ',mp
    write(lulog,*) ' me        = ',me
    write(lulog,*) ' eval      = ',evaluate_breaching
    write(lulog,*) ' Z_h       = ',Z_h
    write(lulog,*) ' Z_p       = ',Z_p
    write(lulog,*) ' Z_o       = ',Z_o
    write(lulog,*) ' Z_e       = ',Z_e

    h        = max (hriver, hhinter)
    h_hinter = min (hriver, hhinter)

    ! Failure due to heave and piping

    evaluate_heaveandpiping = (evaluate_breaching == 3) .or. (evaluate_breaching == 1)
    evaluate_overflowanderosion = (evaluate_breaching == 3) .or. (evaluate_breaching == 2)

!   if ( evaluate_heaveandpiping .and. FailedDueToHP /= 1 ) then
    if ( evaluate_heaveandpiping ) then
        Z_h = 0
        Z_p = 0

        ! Heave:
        dh_ch = heave(gamma_sat,d_d,gamma_w) ! critical head difference for heave
        Z_h = m0*dh_ch - mh*(h-h_hinter)

        write(lulog,*)  ' Computed heave '
        write(lulog,*)  ' dh_ch =', dh_ch
        write(lulog,*)  ' Z_h   =', Z_h

        ! Piping
        dh_cp = sellmeijer(DD,L,d70,k_p,nu,eta,theta,gamma_g,gamma_w)  ! critical head difference
        Z_p = mp*dh_cp - (h-0.3*d_d-h_hinter)

        write(lulog,*)  ' Computed piping Sellmeijer'
        write(lulog,*)  ' dh_cp =', dh_cp
        write(lulog,*)  ' Z_p   =', Z_p

        !
        ! Evaluation of failure criteria
        !
        if (Z_h < 0) then ! heave occurs
            if (Z_p < 0) then ! piping occurs
                fail = 1
                failedDueToHP = 1 ! heave and piping
            else
                failedDueToHP = 2 ! heave, but no piping
            endif
        elseif (Z_p < 0) then ! piping could occur for less top layer thickness
            failedDueToHP = 3 ! no heave, but potential piping
        else
            failedDueToHP = 4 ! neither heave, nor potential piping
        endif
    endif


    ! Failure due to overflow and erosion of inner slope

!   if ( evaluate_overflowanderosion .and. FailedDueToOE /= 1 ) then
    if ( evaluate_overflowanderosion ) then
        Z_o = 0
        Z_e = 0

        ! Overflow
        Z_o = h_d - h  ! water level exceeds dike height

        write(lulog,*)  ' Computed overflow '
        write(lulog,*)  ' Z_o   =', Z_o

        ! Erosion inner slope
        v_c = 3.8 * (cg / 6e-5) ** (2./3.0) / (1+0.8*log10(te))  ! critical flow velocity [m/s]    !! 6e+5 of 6e-5??
        q_c = v_c**(2.5) * KK**(0.25) / 125 / tanalphai**(0.75)  ! critical discharge [m3/s]
        dh_e = (q_c**2 / 0.36 / 9.81)**(1.0/3.0)  ! critical level difference [m]
        Z_e = h_d + me * dh_e - h           ! overflow discharge exceeds critical value

        write(lulog,*)  ' Computed erosion  '
        write(lulog,*)  ' v_c   =', v_c
        write(lulog,*)  ' q_c   =', q_c
        write(lulog,*)  ' dh_e  =', dh_e
        write(lulog,*)  ' Z_e   =', Z_e

        ! Evaluation of failure criteria
        if (Z_o < 0) then ! overflow occurs
            if (Z_e < 0) then ! erosion occurs
                fail = 1
                FailedDueToOE = 1 ! overflow and erosion
            else
                FailedDueToOE = 2 ! overflow, but no erosion
            endif
        else
            FailedDueToOE = 3 ! no overflow
        endif
    endif

    !
    ! Register from which side the dike failure was caused
    !
    if ( fail == 1 .and. failedfrom == 0 ) then
        if (hriver >= hhinter) then
            failedfrom = 1
        endif
        if (hriver <  hhinter) then
            failedfrom = 2
        endif
    endif

end subroutine failure


! read_info --
!     Read the probox information file
!
! Arguments:
!     filename           Name of the file containing the information
!     proboxinfo         Array with all parameters
!     location_id        Array with the names of the locations
!     no_locs            Number of locations
!     scenid             Scenario ID
!
! Note:
!     We rely on the Fortran run-time for throwing errors while
!     reading the files. A they are generated, there should be much
!     of a problem.
!
subroutine read_info( filename, proboxinfo, location_id, no_locs, scenid )
    character(len=*)                        :: filename
    real(dp), dimension(:,:), pointer       :: proboxinfo
    character(len=*), dimension(:), pointer :: location_id
    integer                                 :: no_locs
    character(len=*)                        :: scenid

    integer                           :: lun
    integer                           :: i
    character(len=40)                 :: scenario

    call free_lun( lun )
    open( lun, file = filename, status = 'old' )

    read( lun, * ) scenid
    read( lun, * ) no_locs

    allocate( proboxinfo(no_locs,27) )
    allocate( location_id(no_locs) )


    do i = 1,no_locs
        read( lun, '(a)' ) location_id(i)
    enddo

    do i = 1,no_locs
        read( lun, * ) proboxinfo(i,:)
    enddo

    close( lun )

end subroutine read_info


! read_zmin --
!     Read the probox information file
!
! Arguments:
!     filename           Name of the file containing the information
!     zmin               Array with minimum heights
!     no_locs            Number of locations
!
subroutine read_zmin( filename, zmin, no_locs )
    character(len=*)                :: filename
    real(dp), dimension(:), pointer :: zmin
    integer                         :: no_locs

    integer                         :: lun
    integer                         :: i

    call free_lun( lun )
    open( lun, file = filename, status = 'old' )

    allocate( zmin(no_locs) )

    do i = 1,no_locs
        read( lun, * ) zmin(i)
    enddo

    close( lun )

end subroutine read_zmin


! report_info --
!     Echo the probox information and such to the detailed file
!
! Arguments:
!     proboxinfo         Array with parameters per location
!     zmin               Array with minimum heights
!     location_id        Names of the locations
!     scenid             Name of the scenario
!
subroutine report_info( proboxlocinfo, zmin, location_id, scenid )
    real(dp), dimension(:,:)       :: proboxlocinfo
    real(dp), dimension(:)         :: zmin
    character(len=*), dimension(:) :: location_id
    character(len=*)               :: scenid

    integer                        :: i
    integer                        :: j
    integer                        :: eval

    write( lulog, * ) 'Scenario: ', trim(scenid)

    write( lulog, * ) 'Probox parameters:'
    write( lulog, '(20x,5x,26a12)' ) &
        'D_aquifer',&
        'L_Piping',&
        'd_toplayer',&
        'h_hinterDefault',&
        'h_dike',&
        'tanalphai',&
        'gamma_sat',&
        'gamma_w',&
        'k_permeability',&
        'd70',&
        'nu',&
        'eta',&
        'theta',&
        'gamma_g',&
        'cg',&
        'te',&
        'KStrickler',&
        'm0',&
        'mh',&
        'mp',&
        'me',&
        'B0',&
        'f1',&
        'f2',&
        'T0',&
        'uc'

    do i = 1,size(location_id)
        eval = proboxlocinfo(i,1)
        if ( eval > 0 ) then
            write( lulog, '(a20,i5,26g12.4)' ) trim(location_id(i)), eval, &
                (proboxlocinfo(i,j) ,j=2,27 )
        endif
    enddo

    write( lulog, * ) ' '
end subroutine report_info


End Module ExternalDLLModule
