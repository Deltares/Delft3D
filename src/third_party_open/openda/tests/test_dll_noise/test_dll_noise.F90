
!
!  PROGRAM: test_dll_noise
!
!  PURPOSE:  Testing of two (or more) instances of the same model using the flow2d3d_openda dll functions. 
!            parameter timesteps_for_each_instance:
!               = infinity: the instances are performed sequentially.
!                           This is the easiest test
!               = one     : each instance performs one time step; so all instances
!                            proceed more or less in parallel. This is called the kalman0-test
!                            If instances perform the same, the restart/state swapping is perfect.
!               = (in between) : Most realistic for filtering purposes. Example: observations
!                  available each hour, while model timestep is 7.5m. So value of this parameter is 8.
!
!
!            Also, testing the effect of noise on certain exchange items.
!
!            This program should be used with the test rot in the same directory,
!            because of the required combinations of exchange-items. For other model tests,
!            different exchange-items should be chosen.
!
!  Arguments:
!   1) runid (for example, rot in 01-general )


!
!****************************************************************************

    program test_dll_noise
    
    use m_openda_quantities   
            
    implicit none

    !---
    
    !number of instances:
    integer, parameter :: nmodes = 4
    
    !sea explanation above
    integer, parameter :: Timesteps_for_each_instance = 1 

    integer, external :: SE_INITIALIZE_OPENDA, SE_PERFORMTIMESTEP, SE_FINALIZE_OPENDA, &
    SE_GETTIMEHORIZON, SE_GETDELTAT
    
    integer, external :: SE_CREATE_INSTANCE
    
    integer, external :: SE_GET_EXCHANGE_ITEM_COUNT, SE_GET_EXCHANGE_ITEM_ID_II, &
        SE_GET_VALUES_COUNT_FOR_TIME_SPAN, SE_GET_VALUES_FOR_TIME_SPAN, &
        SE_SET_VALUES_COUNT_FOR_TIME_SPAN, SE_SET_NOISE_FOR_TIME_SPAN, &
        SE_GET_EXCHANGE_ITEM_ID_CI, SE_STORE_CURRENT_INSTANCE,SE_GET_INSTANCE_CORE_STATE, &
        SE_GET_INSTANCE_SIZE, SE_SET_INSTANCE_CORE_STATE

    integer :: D3D_returnValue, D3D_Timestep
    character(len=256) :: schemID = ''
    integer  :: modelHandle  ! handle to current computation
    double precision :: currentTime, startTime, endTime, deltaT
    integer :: interval, run, idebug,len1
    integer :: ierr
    logical ::  doappend

    integer , dimension(nmodes) :: instance_handles

 !   voor exchange-items
    integer:: ei_count, pred_id, forc_id, act_count
    double precision :: noisevals(nmodes)   ! a scalar per mode
    double precision, dimension(:), allocatable :: predvals
    logical :: baddnoise
    integer :: itimesteps
 !-----------------------
 
    call GetArg(1, schemID)
    
    idebug = 1

    print *,'initializing...'

!
!   === INITIALIZE ALL MODES  === 
    
    D3D_returnValue = SE_INITIALIZE_OPENDA('D3D_FLOW', schemID)

    if ( D3D_returnValue /= 0 ) then
         print *, 'Result SE_INITIALIZE function NOT OK, D3D_returnValue: ', D3D_returnValue
         stop
    endif

    do run = 1, nmodes
       ! create the instance handles. In addition, the corresponding instance states
       ! are initially filled.
       !
       instance_handles(run) = SE_CREATE_INSTANCE()
    enddo

    D3D_returnValue = SE_GETTIMEHORIZON('D3D_FLOW', schemID , startTime, endTime)
    if ( D3D_returnValue /= 0 ) then
         print *, 'Result SE_GETTIMEHORIZON function NOT OK, D3D_returnValue: ', D3D_returnValue
         stop
    endif    

     call SE_GETCURRENTTIME('D3D_FLOW', schemID , currentTime)

     D3D_Timestep = 0

! Vraag nu exchange-items op
     ei_count = SE_GET_EXCHANGE_ITEM_COUNT()
     print *,' Total number of exchange items : ',ei_count



!-------------------------------------------------TEST: ROT--------------------------    
! gebruik vanaf nu wind  voor ACCEPTING
! NB merk  op dat 'locatie wind' en 1e monitorpunt beide locatie 1 hebben, maar het betreft verschillende
!    objecten!
     pred_id = SE_GET_EXCHANGE_ITEM_ID_CI('eiland',waterlevel)
     forc_id = SE_GET_EXCHANGE_ITEM_ID_II(1,windu)    
!--------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------
    
     noisevals = 1.0  ! initialized factor
    
 !    print *,'number of EIs;  id to be used for providing; and accepting : ',ei_count,pred_id, forc_id


! =========== END OF INITIALIZATION ====================

     do while (currentTime < endTime - 1.0D-10)

           D3D_Timestep = D3D_Timestep + 1

   !---- now propagate all modes in this timestep! -----
        do run = 1, nmodes

!           print * ,' -- NOW PROPAGATING MODE: ',run
 
!           ----------------------------------
           ! select the instance
           call SE_Select_Instance(instance_handles(run))        
  !        ---------------------------------
           !D3d ACCEPTS exchange items (MULTIPLIER-corrections on wind fields)
           ! this simulates noise on forcings
           ! Must be done before performtimestep, for each mode! 
 
           act_count = SE_GET_VALUES_COUNT_FOR_TIME_SPAN(run, forc_id, currentTime, endTime)                  
           ! we assume that act_count is always 1. 
                     
           baddnoise = (run .ne. 1 .and. .true.)  ! only add noise for non-background modes, if at all
           if (baddnoise) then
              if (mod(D3D_Timestep,2).eq.0) then ! throw a dice for each run, but not each timestep
                 noisevals(run) = 1.0 + 0.2*sin(10.0*run + 20.0*D3D_Timestep )  ! pseudo-random number
                 print *,'for MODE ',run, ' noise adjusted to ',noisevals(run)
              endif
           else
              ! do nothing    : that means: set identity!!!
              noisevals(run) = 1.0 
           endif
           !ierr = SE_SET_NOISE_FOR_TIME_SPAN(forc_id, currentTime, endTime, oper_multiply, act_count,noisevals(run))
                     
           !-------------------------------------------------- 
           ! subloop: perform several timesteps for each instance 
           !-------------------------------------------------- 
           itimesteps = 0
           do  while ((itimesteps < Timesteps_for_each_instance) .and. (currentTime < endTime - 1.0D-10))  
              itimesteps = itimesteps + 1
              D3D_returnValue = SE_PERFORMTIMESTEP('D3D_FLOW', schemID,1)
              if ( D3D_returnValue == 0 ) then ! check res == 0

!  ------------------------------ get monitor values START------------------------
             
             !   D3d PROVIDES exchange items (monitor points)                  
                 act_count = SE_GET_VALUES_COUNT_FOR_TIME_SPAN(run, pred_id, currentTime, endTime)                  
                 allocate(predvals(act_count), stat=ierr)
                 if (ierr .ne. 0) print *,'ERROR allocating predvals'
                 predvals = -999.0
                 ierr = SE_GET_VALUES_FOR_TIME_SPAN(pred_id, currentTime, endTime, act_count,predvals)
                 print *,'water level monitored : ',predvals(1)
                 deallocate(predvals)
             
!  ------------------------------ get monitor values END------------------------

                
                call SE_GETCURRENTTIME('D3D_FLOW', schemID , currentTime)
             else
                print *, 'Result SE_PERFORMTIMESTEP function NOT OK, D3D_returnValue: ', D3D_returnValue
             endif           
           enddo  ! end of subloop: several performtimesteps per mode
           
        enddo   ! run over modes

        ! This call is necessary to save the last instance
        ierr = SE_Store_Current_Instance(0)
           
        print *, '==== END of TIMESTEP(s) for ALL modes, time: ',currentTime,' ===== '
     enddo    ! while (currentTime < endTime - 1.0D-10)

            
   !  ====== FINALIZATION  =========   

    !first: dump final state (for exporting in append mode)
     doappend = .false.
     do run = 1,nmodes 
        call SE_Select_Instance(instance_handles(run))
        call SE_Export_Current_Instance(doappend)
     enddo
     
  !  Perform the finalization with one instantiation only     
  !  We need the first instance, since we want the output variables  
  !  of this instance. Therefore: a select_instance is needed first.
     call SE_Select_Instance(instance_handles(1))
    

     D3D_returnValue = SE_FINALIZE_OPENDA('D3D_FLOW', schemID)
     if ( D3D_returnValue == 0 ) then ! check res == 0
          print *, 'Result SE_FINALIZE_OPENDA function OK'
     else
          print *, 'Result SE_FINALIZE_OPENDA function NOT OK, D3D_returnValue: ', D3D_returnValue
     endif
      

    end program test_dll_noise

