
      subroutine estmorfini (dtf, nstep, lestmorf, nstart, juer)
C ----------------------------------------------------------------------
C
C       Funktie : initialiseer voor bereken karakteristieke waterstanden
C                 en eb- en vloedvolumes.
C-----------------------------------------------------------------------

      logical    lestmorf
      integer    nstep,   nstart  ,juer
      DOUBLE PRECISION    dtf
     
      CHARACTER 
     & record *78

      INTEGER
     & iost ,
     & nofgetij
 
      REAL
     & GetijPeriode 
     
     

          lestmorf = .true. 
C         Haal de lengte van de getij-periode
          OPEN( 210, FILE = 'estmorf.inp', STATUS = 'OLD', 
     &          IOSTAT = iost)
          IF ( iost .NE. 0 ) THEN
             lestmorf = .false.          
c            STOP 'Error opening file = estmorf.inp'
             return
          ENDIF
          
          READ( 210, '(A)', IOSTAT = iost) record
          DO WHILE( record(:10) .NE. 'INPUT USER' 
     &       .AND. ( iost .EQ. 0 ) )
            READ( 210, '(A)', IOSTAT = iost) record
          ENDDO
          IF ( iost .NE. 0 ) THEN
             WRITE (juer,*) 'Error in hidden feature'//
     +             ' (reading file = estmorf.inp)' 
             STOP  'Error reading file = estmorf.inp'
          ENDIF
          READ( 210, *) ! delt
          READ( 210, *) ! Morflng
          READ( 210, *) ! Dtmorf
          READ( 210, *) ! Dtiter
          READ( 210, *) ! Niter
          READ( 210, *) GetijPeriode
          CLOSE( 210)

          nofgetij = INT( Getijperiode / dtf )
          nstart = nstep - nofgetij + 1

          WRITE( *,'(A,I10,A)')
     &      'Getijperiode   = ', INT( GetijPeriode), ' seconden'
          WRITE( *,'(A,I10,A)')
     &      'Runtijdstap    = ', INT( dtf)         , ' seconden'
          WRITE( *,'(A,I10,A)')
     &      'Runlengte      = ', nstep       , ' stappen'
          WRITE( *,'(A,I10,A)')
     &      'Getijperiode   = ', nofgetij    , ' stappen'
          WRITE( *,'(A,I10,A)')
     &      'Start bij stap = ', nstart

          WRITE( *, '(A)') ' '

      end
      
C ======================================================================
      SUBROUTINE EstMorf
     & ( mode, branch, nbran, ngrid, waterstand, debiet, dtf)
C ----------------------------------------------------------------------

C       Funktie : Bereken karakteristieke waterstanden
C                 Bereken eb- en vloedvolumes.

C DATA ---------------------------------------------------- arguments --   

      INTEGER
     & mode   ,   ! 0 = accumuleren, 1 = schrijven in file
     & branch ,     
     & nbran  ,
     & ngrid

      double precision
     & waterstand ,
     & debiet

      DOUBLE PRECISION
     & dtf

      DIMENSION
     & branch    ( 4, nbran ) ,
     & waterstand( ngrid, 3  ) ,
     & debiet    ( ngrid, 3  )

C DATA -------------------------------------------------------- local --

      LOGICAL
     & initlz

      INTEGER
     & i, istat,
     & ibrn, igrd

      REAL
     & q, Hgem

       REAL , allocatable , save , dimension (:) ::
     & HAmin, HAmax, HBmin, HBmax, QApos, QAneg, QBpos, QBneg 

      SAVE    initlz 
      
      DATA
     & initlz  / .TRUE. /

C BEGIN ================================================================

        IF ( initlz ) THEN
          initlz = .FALSE.

          ALLOCATE( HAmin( nbran), STAT = istat)
          IF ( istat .EQ. 0 ) ALLOCATE( HAmax( nbran), STAT = istat)
          IF ( istat .EQ. 0 ) ALLOCATE( HBmin( nbran), STAT = istat)
          IF ( istat .EQ. 0 ) ALLOCATE( HBmax( nbran), STAT = istat)
          IF ( istat .EQ. 0 ) ALLOCATE( QApos( nbran), STAT = istat)
          IF ( istat .EQ. 0 ) ALLOCATE( QAneg( nbran), STAT = istat)
          IF ( istat .EQ. 0 ) ALLOCATE( QBpos( nbran), STAT = istat)
          IF ( istat .EQ. 0 ) ALLOCATE( QBneg( nbran), STAT = istat)
          IF ( istat .NE. 0 ) STOP 'Error Morf memory allocation'

C         Initialize
          DO i = 1, nbran

            HAmax( i) = -9999.
            HAmin( i) =  9999.
            HBmax( i) = -9999.
            HBmin( i) =  9999.

            QApos( i) =     0.
            QAneg( i) =     0.
            QBpos( i) =     0.
            QBneg( i) =     0.

          ENDDO

        ENDIF

        IF ( mode .EQ. 0 ) THEN ! accumuleren

          DO ibrn = 1, nbran

            igrd = branch( 3, ibrn)  ! A-kant
          
            HAmin( ibrn) = MIN( HAmin( ibrn), waterstand( igrd, 3)) 
            HAmax( ibrn) = MAX( HAmax( ibrn), waterstand( igrd, 3)) 

            Q = debiet( igrd, 3)
            IF ( Q .GE. 0. ) THEN
              QApos( ibrn) = QApos( ibrn) + Q * dtf
            ELSE
              QAneg( ibrn) = QAneg( ibrn) - Q * dtf
            ENDIF

            igrd = branch( 4, ibrn)  ! B-kant

            HBmin( ibrn) = MIN( HBmin( ibrn), waterstand( igrd, 3)) 
            HBmax( ibrn) = MAX( HBmax( ibrn), waterstand( igrd, 3)) 

            Q = debiet( igrd, 3)
            IF ( Q .GE. 0. ) THEN
              QBpos( ibrn) = QBpos( ibrn) + Q * dtf
            ELSE
              QBneg( ibrn) = QBneg( ibrn) - Q * dtf
            ENDIF

          ENDDO

        ELSE ! schrijven

          OPEN ( 215, FILE = 'flow.out')
          WRITE( 215, '(''* aantal vakken = '',I4)')
          WRITE( 215, '(''*  Hmin  Hgem  Hmax       Qpos       Qneg'')')
          WRITE( 215, '(''* ----- ----- ----- ---------- ----------'')')

          DO ibrn = 1, nbran

            WRITE( 215, *) ibrn, ( branch( i, ibrn), i=1,4)

            Hgem = ( HAmin( ibrn) + HAmax( ibrn) ) / 2.
            WRITE( 215, '(3(1x,F8.5), 5(1x,E14.6))')
     &        HAmin( ibrn), Hgem, HAmax( ibrn),
     &        QApos( ibrn), QAneg( ibrn)

            Hgem = ( HBmin( ibrn) + HBmax( ibrn) ) / 2.
            WRITE( 215, '(3(1x,F8.5), 5(1x,E14.6))')
     &        HBmin( ibrn), Hgem, HBmax( ibrn),
     &        QBpos( ibrn), QBneg( ibrn)

          ENDDO
          CLOSE( 215)
         
        ENDIF

      END
C                                                                     72       
