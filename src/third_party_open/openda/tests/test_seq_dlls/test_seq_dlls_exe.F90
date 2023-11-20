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


!  Arguments:
!   1) runid1 (for example, rot in 01-general )
!   2) runid2. If only one argument, runid2=runid1.
!   3) time_pe1. Number of timesteps per instance for run runid1.
!   4) time_pe2. Number of timesteps per instance for run runid2.
!   If less than four arguments, default values of 100000 for both time_pe are chosen. 

!   The following tests can (should) be performed:
!   1) test if a model can run in dll and gives the same results as d_hydro.exe 
!       >  test_seq_dll.exe runid1 runid1 100000 100000
!       (compare map- and his- files with references)
!   2) test if this model can be restarted each time step 
!       >   test_seq_dll.exe runid1 runid1 1 1
!       (compare map- and his- files with references)
!       (compare state dumps of both instances)
!   3) test if different models can be run dll-wise.
!      >   test_seq_dll.exe runid1 runid2 100000 100000
!       (compare map- and his- files fo both models with references)
!
!****************************************************************************


   program test_seq_dlls
   
   implicit none

   character(len=256) :: schemID1 = ''
   character(len=256) :: schemID2 = ''
   integer            :: nargs
   character(len=10)  :: arg3
   character(len=10)  :: arg4    
   integer            :: time_pe1
   integer            :: time_pe2
   
   nargs = iargc()
   if (nargs .eq. 1) then  ! repeat the same model
      call GetArg(1, schemID1)
      schemID2 = schemID1
   elseif (nargs .ge. 2) then
      call GetArg(1, schemID1)
      call GetArg(2, schemID2)
   else
      print *,'at least one argument (runid) required'        
      stop
   endif
   time_pe1 = 100000
   time_pe2 = 100000
   if (nargs .eq. 4) then
      call GetArg(3, arg3)
      call GetArg(4, arg4)
      read(arg3,'(I4)') time_pe1   
      read(arg4,'(I4)') time_pe2   
   endif
   

   print *,'running model ',schemID1,' with 2 instances and ',time_pe1, 'instance swap timesteps' 
   call test_one_dll(schemID1, time_pe1)
   
   print *,'running model ',schemID2,' with 2 instances and ',time_pe2, 'instance swap timesteps' 
   call test_one_dll(schemID2, time_pe2)
   
   
   end program test_seq_dlls
