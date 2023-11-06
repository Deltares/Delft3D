!  PROGRAM: test_seq_dlls
!
!  PURPOSE:  Test the main flow2d3d_openda dll functions. Two (different) models can be run sequentially.
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
!     (compare map- and his- files for both models with references)

----------------------------------------------------

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
!   1) runid (for example, rot in 01-general 

The following can be tested:
1) baddnoise = FALSE : alle states are the same.
2) baddnoise = TRUE: instances 2..n differ from background instance 1 and are all pairwise different.



