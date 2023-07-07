!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

! test_waq_omi_lib.f90 --
!     Test program to check that DELWAQ2 works correctly via the OpenMI/Delta-Shell
!
program test_kalman0

    use m_waq_multi_instance
    use m_openda_quantities

    implicit none

    integer, parameter :: nmodes = 2


    logical :: success
    logical :: varying_volume

    integer :: integration_method
    integer :: i

    integer, dimension(4,4) :: pointers = &
        reshape((/ -1, 1, 0, 2, &
                    1, 2, 3,-2, &
                    2, 3, 1,-2, &
                    3,-2, 2, 0  /), (/ 4,4 /))
    integer, dimension(4)   :: number_exchanges = (/ 4, 0, 0, 0 /)
    real, dimension(3)      :: salvalue
    real, dimension(3)      :: tmpvalue
    real, dimension(3)      :: oxyvalue
    real, dimension(3)      :: volume
    real, dimension(3)      :: volume0
    real, dimension(4)      :: flow
    real, dimension(4)      :: area
    real                    :: dvol_dt



    integer                    :: test_starttime
    integer                    :: test_endtime
    integer                    :: test_tstep
    integer                    :: delwaq_timestep
    double precision           :: endTime

    logical, external :: SETINTEGRATIONOPTIONS, SETSIMULATIONTIMES,    &
                         DEFINEWQDISPERSION,    DEFINEWQPROCESSES,     &
                         SETCURRENTVALUESCALARINIT, DEFINEDISCHARGELOCATIONS, &
                         SETINITIALVOLUME,   &
                         DEFINEWQSCHEMATISATION, SETFLOWDATA,   &
                         SETWASTELOADVALUES, SETBOUNDARYCONDITIONS, &
                         GETCURRENTVALUE, &
                         SETOUTPUTTIMERS, DEFINEMONITORINGLOCATIONS

    integer, external :: MODELFINALIZE, MODELINITIALIZE, MODELINITIALIZE_BY_ID, &
                         GETWQCURRENTTIME, MODELPERFORMTIMESTEP


    integer, external :: CREATE_INSTANCE, STORE_CURRENT_INSTANCE,   SE_GET_EXCHANGE_ITEM_ID, &
                         SE_SET_NOISE_FOR_TIME_SPAN, SE_GET_VALUES_FOR_TIME_SPAN


    integer                              :: result
    integer                              :: run, imode
    character(len=16), dimension(nmodes) :: fnaam
    integer, dimension(nmodes)           :: runlun

    integer , dimension(nmodes)          :: instance_handles
    double precision                     :: currentTime
    integer                              :: retval, ierr

    integer                              :: forc_id
    integer                              :: pred_id
    double precision, dimension(nmodes)  :: noisevals   ! a scalar per mode

    double precision, dimension(:), allocatable :: predvals
    logical                              :: baddnoise
    character*20                        :: runidname

    !===========================================================
    !===========================================================
    print *,'initializing...'

!
!   === INITIALIZE ALL MODES  ===
!===========================================================
!   output files
    do run = 1, nmodes
        write(fnaam(run),'(A8,I4.4,A4)') 'uitvoer_',run,'.txt'
        open(newunit=runlun(run), file=fnaam(run))
    enddo

!==========================================================

! open arjen-ws.inp

    !runidname = 'arjen-ws'
    !runidname = 'arjen-ws'

    call getarg(1,runidname)


    result = MODELINITIALIZE_BY_ID(runidName)

    do run = 1, nmodes
        ! create the instance handles. In addition, the corresponding instance states
        ! are initially filled.

        instance_handles(run) = CREATE_INSTANCE()

    enddo



    delwaq_timestep = 0


!==================================================================
!     !---- now propagate all modes in each timestep! -----
!==================================================================
    do while (delwaq_timestep < 10)

        delwaq_timestep = delwaq_timestep + 1

        do run = 1,nmodes

            print * ,' --1 NOW PROPAGATING MODE: ',run

    !       ----------------------------------
    !       select the instance
            call Select_Instance(instance_handles(run))



            result = ModelPerformTimeStep()


            result = GETWQCURRENTTIME(currentTime)

    !  ------------------------------ get monitor values START------------------------



    !

        enddo   ! run over modes

        print *, '==== END of TIMESTEP(s) for ALL modes, time: ',currentTime,' ===== '



    enddo  ! while time loop)


! export states
    do run = 1, nmodes
       call Select_Instance(instance_handles(run))
       call export_current_instance(.false.)
    enddo

   !  ====== FINALIZATION  =========
!===========================================================
     ! perform the finalization with one instantiation only
     ! The one now in memory will do fine.
    result = ModelFinalize()



    print *,'end of everything'



   goto 9999

! ******************************************************
! ********************************************************
! *******************************************************
!-------------------------------------------
! hieronder:OUD
!--------------------------------------------

    varying_volume     = .true.
    integration_method = 1

    success = SETINTEGRATIONOPTIONS(integration_method, .true., .true., .true., .false., .false.)

    !
    ! Schematisation:
    ! Three segments of 10x1 km, 10 m deep - volume = 1.0e8 m3
    ! Cross sections: 1 km x 10 m - area = 1.0e4 m2
    ! Flow rate: 0.2 m/s x area = 0.2e4 m3/s
    ! Lengths: 5 km on both sides
    ! Residence time: 1.0e8/0.2e4 = 50000 s
    !

    test_starttime = 0
    test_endtime = 1000000
    test_tstep = 10000
    success = SetSimulationTimes( test_starttime, test_endtime,test_tstep )

    success = SetOutputTimers(1, test_starttime, test_endtime, test_tstep)

    success = DefineWQSchematisation( 3, pointers, number_exchanges )

    success = DefineWQDispersion( (/ 100.0, 0.0, 0.0 /), &
                                                              (/ 5000.0, 5000.0, 5000.0, 5000.0, 5000.0, 5000.0, 5000.0, 5000.0 /) )

    success = DefineWQProcesses( (/ 'Salinity', 'Temperature', 'OXY' /), 3, 3, &
                                                             (/ 'OXYSAT' /), 1,   &
                                                             (/ 'SaturOXY', 'ReaerOXY' /), 2)

!  at this moment, noseg and notot are known. Therefore the state can be defined!


    success = SetCurrentValueScalarInit( 'Salinity',    30.0 )
    success = SetCurrentValueScalarInit( 'Temperature', 20.0 )
    success = SetCurrentValueScalarInit( 'OXY',          3.0 )

    success = DefineDischargeLocations( (/1, 2/), 2 )


    success = DefineMonitoringLocations( (/1, 2 ,3/), (/ 'Links', 'Midden', 'Rechts' /), 3)

    !
    ! Important: for initialising the mass per segment
    !
    volume  = 1.0e8
    volume0 = volume
    success = SetInitialVolume( volume )

    result  = ModelInitialize()
! now the module variable dlwqd%rbuf has been filled with concentrations


    !
    ! Now we can define the volumes, flows and areas
    ! As well as boundary conditions and waste loads
    !
    area    = 1.0e4
    flow    = 0.2e4

    if ( varying_volume ) then
        dvol_dt = 0.2e4
        flow    = (/ 0.8e4, 0.6e4, 0.4e4, 0.2e4 /)
    endif


    success = SetFlowData( volume, area, flow )
    success = SetWasteLoadValues( 1, (/ 1.0, 0.0, 1000000.0, 0.0 /) )
    success = SetBoundaryConditions( 1, (/ 30.0, 10.0, 3.0 /) )
    success = SetBoundaryConditions( 2, (/  0.0, 20.0, 3.0 /) )

!====================================================


    do run = 1, nmodes

        ! create the instance handles. In addition, the corresponding instance states
        ! are initially filled.

        instance_handles(run) = CREATE_INSTANCE()

    enddo

    forc_id = SE_GET_EXCHANGE_ITEM_ID(accepting, 'bnd1', 'Temperature')
    pred_id = SE_GET_EXCHANGE_ITEM_ID(providing, 'rechts','Temperature')
    noisevals = 1.0  ! initialized factor
    ! for the moment, we assume that predictions are scalars
    allocate(predvals(1))


    delwaq_timestep = 0


!==================================================================
!     !---- now propagate all modes in each timestep! -----
!==================================================================
    do while (delwaq_timestep < 10)

        delwaq_timestep = delwaq_timestep + 1

        do run = 1,nmodes

            print * ,' -- NOW PROPAGATING MODE: ',run

    !       ----------------------------------
    !       select the instance
            call Select_Instance(instance_handles(run))

            result = GETWQCURRENTTIME(currentTime)
            if ( varying_volume ) then
                !!WRONG!   volume  = volume + dvol_dt * test_tstep
                ! Vortech: replaced this as a function of currenttime to avoid
                ! problems with multiple instances
                volume = volume0 + (currentTime * dvol_dt)
                success = SetFlowData( volume, area, flow )
            endif
            ! onderstaande twee regels zijn te vergelijken met inlezen van tijdreeks
            ! het moet dus elke tijdstap gebeuren.
            ! alternatief is dat ze in de (pseudo)state komen!
            ! weglaten betekent dat aangepaste forcings verknoeid blijven.
            success = SetBoundaryConditions( 1, (/ 30.0, 10.0, 3.0 /) )
            success = SetBoundaryConditions( 2, (/  0.0, 20.0, 3.0 /) )


            !--------------------------
            !   forcings START
            !--------------------------
            baddnoise = (run .eq. 1 .and. .false.)  ! only add noise for non-background modes
            if (baddnoise) then
                if (mod(delwaq_timestep,3).eq.0) then ! throw a dice for each run, but not each timestep
                    noisevals(run) = 1.0 + 0.2*sin(10.0*run + 20.0*delwaq_timestep )  ! pseudo-random number
                    print *,'for MODE ',run, ' noise adjusted to ',noisevals(run)
                endif
            else
                ! do nothing    : that means: set identity!!!
                noisevals(run) = 1.0

            endif
            ierr = SE_SET_NOISE_FOR_TIME_SPAN(forc_id, currentTime, endTime,  &
                                oper_multiply, 1,noisevals(run))

         !--------------------------
         !   forcings END
         !-------------------------



            result = ModelPerformTimeStep()


            result = GETWQCURRENTTIME(currentTime)

    !  ------------------------------ get monitor values START------------------------
            success = GetCurrentValue( 'Salinity',    salvalue )
            success = GetCurrentValue( 'Temperature', tmpvalue )
            success = GetCurrentValue( 'OXY',         oxyvalue )
            write(*,'(i5,3e12.4)') i, salvalue
            write(*,'(5x,3e12.4)')    tmpvalue
            write(*,'(5x,3e12.4)')    oxyvalue

     ! monitor values to exchange items:
            ierr = SE_GET_VALUES_FOR_TIME_SPAN(pred_id, currentTime, endTime, 1,predvals)
            print *,'providing exchange item  : ',predvals(1)



    !  ------------------------------ get monitor values END ------------------------
            write(runlun(run),'(i5,3e12.4)') delwaq_timestep, salvalue
            write(runlun(run),'(5x,3e12.4)')    tmpvalue
            write(runlun(run),'(5x,3e12.4)')    oxyvalue


        enddo   ! run over modes

        print *, '==== END of TIMESTEP(s) for ALL modes, time: ',currentTime,' ===== '

    ! This call is necessary to save the last instance
        retval = Store_Current_Instance(0)


    enddo  ! while time loop)


! export states
    do run = 1, nmodes
       call Select_Instance(instance_handles(run))
       call export_current_instance(.false.)
    enddo

   !  ====== FINALIZATION  =========
!===========================================================
     ! perform the finalization with one instantiation only
     ! The one now in memory will do fine.
    success = ModelFinalize()


!===================================================

! now close the output files
    do run = 1,nmodes
        close(runlun(run))
    enddo


9999 continue

end program test_kalman0
