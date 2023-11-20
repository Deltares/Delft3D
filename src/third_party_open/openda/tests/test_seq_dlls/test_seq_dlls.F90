!  test_seq_dlls
!
!  PROGRAM: test_seq_dlls
!
!  PURPOSE:  Test the main dll functions. Two (different) models can be run sequentially.
!  Results should be the same as standalone runs with d_hydro.exe.
!  Further, the number of instances (modes) and the number of timesteps per mode can be altered.
!  Timesteps_for_each_instance    = 1: instances are swapped after each timestep
!                                 = n: instances are swapped after n-th timestep   
! 
!   The following tests can (should) be performed:
!   1) test if a model can run in dll and gives the same results as d_hydro.exe 


!  Arguments:
!   1) runid (for example, rot in 01-general )
!   2) time_pe : timesteps_for_each_instance 

!
!****************************************************************************

    subroutine test_one_dll(schemID, time_pe)
    
    use m_openda_quantities   
            
    implicit none

    ! arguments
    character(len=*) :: schemID 
    integer          :: time_pe

    !number of instances:
    integer, parameter :: nmodes = 2

    !---
    integer :: timesteps_for_each_instance
    
    integer, external :: SE_INITIALIZE_OPENDA, SE_PERFORMTIMESTEP, SE_FINALIZE_OPENDA, &
    SE_GETTIMEHORIZON, SE_GETDELTAT
    
    integer, external :: SE_CREATE_INSTANCE
    
    ! note: most of these funtions are not yet used here.
    integer, external :: SE_GET_EXCHANGE_ITEM_COUNT, SE_GET_EXCHANGE_ITEM_ID_II, &
        SE_GET_VALUES_COUNT_FOR_TIME_SPAN, SE_GET_VALUES_FOR_TIME_SPAN, &
        SE_SET_VALUES_COUNT_FOR_TIME_SPAN, SE_SET_NOISE_FOR_TIME_SPAN, &
        SE_GET_EXCHANGE_ITEM_ID_CI, SE_STORE_CURRENT_INSTANCE,SE_GET_INSTANCE_CORE_STATE, &
        SE_GET_INSTANCE_SIZE, SE_SET_INSTANCE_CORE_STATE

    integer :: D3D_returnValue, D3D_Timestep

    double precision :: currentTime, startTime, endTime, deltaT
    integer :: interval, run, idebug,len1
    integer :: ierr
    logical ::  doappend

    integer , dimension(nmodes) :: instance_handles


    integer :: itimesteps
 !-----------------------------------------------------------------------
 ! body: 
 !-----------------------------------------------------------------------
    !see explanation above
    Timesteps_for_each_instance = time_pe
    
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

       instance_handles(run) = SE_CREATE_INSTANCE()

    enddo
    

    D3D_returnValue = SE_GETTIMEHORIZON('D3D_FLOW', schemID , startTime, endTime)
    if ( D3D_returnValue /= 0 ) then
         print *, 'Result SE_GETTIMEHORIZON function NOT OK, D3D_returnValue: ', D3D_returnValue
         stop
    endif    

     call SE_GETCURRENTTIME('D3D_FLOW', schemID , currentTime)

     D3D_Timestep = 0
  
! =========== END OF INITIALIZATION ====================

     do while (currentTime < endTime - 1.0D-10)

           D3D_Timestep = D3D_Timestep + 1

   !---- now propagate all modes in this timestep! -----
        do run = 1,nmodes

           print * ,' -- NOW PROPAGATING MODE: ',run
 
!           ----------------------------------
           ! select the instance
           call SE_Select_Instance(instance_handles(run))        
  !        ---------------------------------
          
           itimesteps = 0
           
           call SE_GETCURRENTTIME('D3D_FLOW', schemID , currentTime)
           
           do  while ((itimesteps < Timesteps_for_each_instance) .and. (currentTime < endTime - 1.0D-10))  
             itimesteps = itimesteps + 1


             D3D_returnValue = SE_PERFORMTIMESTEP('D3D_FLOW', schemID,1)
             if ( D3D_returnValue == 0 ) then ! check res == 0


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
     
     ! perform the finalization with one instantiation only     
     ! We need the first instance, since we want the output variables  
     ! of this instance. Therefore: a select_instance is needed first.
      call SE_Select_Instance(instance_handles(1))

     D3D_returnValue = SE_FINALIZE_OPENDA('D3D_FLOW', schemID)
     if ( D3D_returnValue == 0 ) then ! check res == 0
          print *, 'Result SE_FINALIZE_OPENDA function OK'
     else
          print *, 'Result SE_FINALIZE_OPENDA function NOT OK, D3D_returnValue: ', D3D_returnValue
     endif
      

    

    end subroutine test_one_dll

!--------------------------------------------------------------------
