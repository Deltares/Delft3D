!
!       swell wave height for FEWS
!
        IVTYPE = POS_FHSWE
        IF (OQPROC(IVTYPE)) THEN
          ETOT = 0.
          FSWELL = PI2 * OUTPAR(5)
!            integration over [fmin,fmax]                                 40.87
             FMIN = 0.                                                    40.87
             FMAX = FSWELL                                                40.87
             ECS  = 1.                                                    40.87
             ETOT = SwanIntgratSpc(0. , FMIN, FMAX, SPCSIG, SPCDIR(1,1),  40.87
     &                             WK , ECS , 0.  , 0.    , ACLOC      ,  40.87
     &                             1  )                                   40.87
          IF (ETOT .GE. 0.) THEN                                          30.00
            VOQ(IP,VOQR(IVTYPE)) = 4.*SQRT(ETOT)
          ELSE
            VOQ(IP,VOQR(IVTYPE)) = 0.                                     40.86
          ENDIF
          IF (ITEST.GE.100) THEN                                          40.00
            WRITE(PRINTF, 223) IP, OVSNAM(IVTYPE), VOQ(IP,VOQR(IVTYPE))   40.00
 223        FORMAT(' SWOEXA: POINT ', I5, 2X, A, 1X, E12.4)
          ENDIF
        ENDIF 

!
!       swell wave period TM01 for FEWS
!
        IVTYPE = POS_FTM01
        IF (OQPROC(IVTYPE)) THEN
          ETOT = 0.
          FSWELL = PI2 * OUTPAR(5)
!             integration over [fmin,fmax]                                40.87
              FMIN = 0                                                    40.87
              FMAX = FSWELL                                               40.87
              ECS  = 1.                                                   40.87
              APTOT=SwanIntgratSpc(0. , FMIN, FMAX, SPCSIG, SPCDIR(1,1),  40.87
     &                             WK , ECS , 0.  , 0.    , ACLOC      ,  40.87
     &                             1  )                                   40.87
              EPTOT=SwanIntgratSpc(1. , FMIN, FMAX, SPCSIG, SPCDIR(1,1),  40.87
     &                             WK , ECS , 0.  , 0.    , ACLOC      ,  40.87
     &                             1  )                                   40.87

           IF (EPTOT.GT.0.) THEN
             TPER = 2.*PI * APTOT / EPTOT
             VOQ(IP,VOQR(IVTYPE)) = TPER
           ELSE
             VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
           ENDIF
          IF (ITEST.GE.100) THEN                                          40.00
            WRITE(PRINTF, 224) IP, OVSNAM(IVTYPE), VOQ(IP,VOQR(IVTYPE))   40.00
 224        FORMAT(' SWOEXA: POINT ', I5, 2X, A, 1X, E12.4)
          ENDIF
        ENDIF

!
!       swell wave DIR for FEWS
!
        IVTYPE = POS_FDIR
        IF (OQPROC(IVTYPE)) THEN
          ETOT = 0.
          FSWELL = PI2 * OUTPAR(5)
!             integration over [fmin,fmax]                                40.87
              FMIN = 0                                                    40.87
              FMAX = FSWELL                                               40.87
              ECS  = 1.                                                   40.87
              ETOT= SwanIntgratSpc(0. , FMIN, FMAX, SPCSIG, SPCDIR(1,1),  40.87
     &                             WK , ECS , 0.  , 0.    , ACLOC      ,  40.87
     &                             1  )                                   40.87
              EEX = SwanIntgratSpc(0., FMIN, FMAX, SPCSIG, SPCDIR(1,1),   40.87
     &                             WK, SPCDIR(1,2), 0., 0., ACLOC, 1)     40.87
              EEY = SwanIntgratSpc(0., FMIN, FMAX, SPCSIG, SPCDIR(1,1),   40.87
     &                             WK, SPCDIR(1,3), 0., 0., ACLOC, 1)     40.87

           IF (ETOT.GT.0.) THEN
              IF (BNAUT) THEN                                             30.70
                 DIRDEG = ATAN2(EEY,EEX) * 180./PI                        10.15
              ELSE
                 DIRDEG = (ALCQ + ATAN2(EEY,EEX)) * 180./PI               10.15
              ENDIF
              IF (DIRDEG.LT.0.) DIRDEG = DIRDEG + 360.                    10.15
!
!             *** Convert (if necessary) from nautical degrees ***        32.01
!             *** to cartesian degrees                         ***        32.01
!
              VOQ(IP,VOQR(IVTYPE)) = DEGCNV( DIRDEG )                     32.01
!
           ELSE
              VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
           ENDIF
        ENDIF
