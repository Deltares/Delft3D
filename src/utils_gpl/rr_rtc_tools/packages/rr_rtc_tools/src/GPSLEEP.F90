subroutine GPSLEEP (sleeptime)

! *********************************************************************
! *** OPERATIONAL RIVER BASIN WATER MANAGEMENT MODEL
! **********************************************************************
! *** Sleep routine: wait sleeptime milliseconds
! **********************************************************************
   integer  ::  sleeptime

! Define the conversion factor to milliseconds, which is platform dependent
#if (defined(HAVE_CONFIG_H))
    call sleep(sleeptime)
#else
    call SLEEPQQ(sleeptime)
#endif

end subroutine
