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
    use waq_omi_constants

    implicit none

    integer, parameter :: nmodes = 2

    logical :: success

    integer :: i
    integer :: k

    real, dimension(:), allocatable :: ctrvalue

    integer                         :: test_starttime
    integer                         :: test_endtime
    integer                         :: test_tstep
    integer                         :: delwaq_timestep

    logical, external :: SETINTEGRATIONOPTIONS, SETSIMULATIONTIMES,    &
                         DEFINEWQDISPERSION,    DEFINEWQPROCESSES,     &
                         SETCURRENTVALUESCALARINIT, DEFINEDISCHARGELOCATIONS, &
                         SETINITIALVOLUME,  MODELINITIALIZE_BY_ID,   &
                         DEFINEWQSCHEMATISATION, SETFLOWDATA,   &
                         SETWASTELOADVALUES, SETBOUNDARYCONDITIONS, &
                         MODELPERFORMTIMESTEP, GETCURRENTVALUE, &
                         MODELFINALIZE, SETOUTPUTTIMERS, DEFINEMONITORINGLOCATIONS

    integer, external :: CREATE_INSTANCE, STORE_CURRENT_INSTANCE, GETSUBSTANCEID, &
                         GETLOCATIONID, SET_VALUES, GET_VALUES

    integer                              :: run, imode
    character(len=16), dimension(nmodes) :: fnaam
    integer, dimension(nmodes)           :: runlun

    integer , dimension(nmodes)          :: instance_handles
    double precision                     :: currentTime
    integer                              :: retval, ierr, status

    integer                              :: forc_id_loc
    integer                              :: forc_id_sub
    integer                              :: forc_id_subtype
    integer                              :: forc_id_loctype

    integer                              :: pred_id_loc
    integer                              :: pred_id_sub
    integer                              :: pred_id_subtype
    integer                              :: pred_id_loctype

    double precision, dimension(nmodes)  :: noisevals   ! a scalar per mode

    double precision, dimension(:), allocatable :: predvals
    logical                              :: baddnoise
    character(len=200)                   :: runid

    !
    ! Get the run-ID or quit
    !
    runid = 'arjen-ws'

    if ( runid == ' ' ) then
        write(*,*) 'Please specify the run-ID!'
        stop
    endif

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

    success = ModelInitialize_By_Id(runid)

    allocate( ctrvalue(1736) )

    do run = 1, nmodes

        ! create the instance handles. In addition, the corresponding instance states
        ! are initially filled.

        instance_handles(run) = CREATE_INSTANCE()

    enddo

    forc_id_loc     = 56
    forc_id_sub     = GetSubstanceId( 'cTR1' )
    forc_id_subtype = ODA_PAR_TYPE_SUBSTANCE
    forc_id_loctype = ODA_LOC_TYPE_SEGMENT

  !  pred_id_loc     = GetLocationId( ODA_LOC_TYPE_MONITOR, '(31,4)' )
    pred_id_loc     = 56
    pred_id_sub     = GetSubstanceId( 'cTR1' )
    pred_id_subtype = ODA_PAR_TYPE_SUBSTANCE
    pred_id_loctype = ODA_LOC_TYPE_SEGMENT

   !forc_id = SE_GET_EXCHANGE_ITEM_ID(accepting, '56', 'cTR1')      ! Segment number
   !pred_id = SE_GET_EXCHANGE_ITEM_ID(providing, '(31,4)','cTR1')   ! Name of monitoring location/area

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

            call GETWQCURRENTTIME(currentTime)

            !--------------------------
            !   forcings START
            !--------------------------
            baddnoise = (run .eq. 1 .and. .true.)  ! only add noise for non-background modes
            if (baddnoise) then
                if (mod(delwaq_timestep,3).eq.0) then ! throw a dice for each run, but not each timestep
                    noisevals(run) = 1.0 + 0.2*sin(10.0*run + 20.0*delwaq_timestep )  ! pseudo-random number
                    print *,'for MODE ',run, ' noise adjusted to ',noisevals(run)
                endif
            else
                ! do nothing    : that means: set identity!!!
                noisevals(run) = 1.0

            endif
!!            ierr = Set_Values(forc_id_subtype, forc_id_sub, forc_id_loctype, forc_id_loc,  &
!!                                oper_add, 1,noisevals(run))

         !--------------------------
         !   forcings END
         !-------------------------


            do k = 1,4
                success = ModelPerformTimeStep()
            enddo

            call GETWQCURRENTTIME(currentTime)

    !  ------------------------------ get monitor values START------------------------
            success = GetCurrentValue( 'cTR1',    ctrvalue )
            write(*,'(i5,5e12.4)') i, (ctrvalue(k), k = 54,58)

     ! monitor values to exchange items:
            ierr = Get_Values(pred_id_subtype, pred_id_sub, pred_id_loctype, pred_id_loc, 1, predvals)
            print *,'providing exchange item  : ',predvals(1)



    !  ------------------------------ get monitor values END ------------------------
            write(runlun(run),'(i5,5e12.4)') delwaq_timestep, (ctrvalue(k), k = 54,58)


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


end program test_kalman0
