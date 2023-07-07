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

      SUBROUTINE DLWQS1 ( LUNREP       , NPOS     ,
     +                    CCHAR        , VRSION   ,
     +                    ILUN         , LCH      ,
     +                    LSTACK       , IOUTPT   ,
     +                    DTFLG1       , DTFLG3   ,
     +                    StatProcesDef, AllItems ,
     +                    NOINFO       , NOWARN   ,
     +                    IERR         )

!       Deltares Software Centre

!>\file
!>                          Defines process steering for statistical output processing
!>\par  Description:
!>                          This routine deals with the set up of statistical processes

!      Created            : somewhere 2002 by Jan van Beek

!      Modified           :

!      Subroutines called :

!      Logical units      :

      use m_zoek
      use timers       !   performance timers
      use           stasub
      use           dhralloc
      use           ProcesSet

      implicit      none

!     kind           function         name                Descriptipon

      integer  ( 4), intent(in   ) :: lunrep            !< unit nr of output report file
      integer  ( 4), intent(in   ) :: npos              !< significant line length of input file
      character( 1), intent(in   ) :: cchar             !< comment character
      real     ( 4), intent(in   ) :: vrsion            !< version number of this input
      integer  ( 4), intent(inout) :: ilun (*)          !< unitnumber include stack
      character( *), intent(inout) :: lch  (*)          !< filename include stack for input
      integer  ( 4), intent(in   ) :: lstack            !< include file stack size
      integer  ( 4), intent(  out) :: ioutpt            !< flag for more or less output
      logical      , intent(in   ) :: dtflg1            !< 'date'-format 1st timescale
      logical      , intent(in   ) :: dtflg3            !< 'date'-format (F;ddmmhhss,T;yydddhh)
      type(ProcesPropColl)         :: StatProcesDef     !< the statistical proces definition
      type(ItemPropColl)           :: AllItems          !< all items of the proces system
      integer      , intent(inout) :: noinfo            !< count of informative message
      integer  ( 4), intent(inout) :: nowarn            !< cumulative warning count
      integer  ( 4), intent(inout) :: ierr              !< cumulative error   count

      type(ProcesProp)      :: aProcesProp         ! one statistical proces definition

      INTEGER     , POINTER :: STA_NO_IN(:)
      INTEGER     , POINTER :: STA_NO_OUT(:)
      INTEGER     , POINTER :: STA_SWITR(:)
      CHARACTER*20, POINTER :: STA_IN_NAM(:)
      CHARACTER*50, POINTER :: STA_IN_TXT(:)
      REAL        , POINTER :: STA_IN_DEF(:)
      CHARACTER*20, POINTER :: STA_OUT_NAM(:)
      CHARACTER*50, POINTER :: STA_OUT_TXT(:)
      CHARACTER*10, POINTER :: STA_MODNAM(:)
!
      INTEGER       NKEY  , IPOSR , NSPROC
      CHARACTER*20, POINTER :: KEYNAM(:)
      CHARACTER*20, POINTER :: KEYVAL(:)
      CHARACTER*20, ALLOCATABLE :: KEYNAM2(:)
      CHARACTER*20, ALLOCATABLE :: KEYVAL2(:)
      INTEGER     , POINTER :: NOKEY(:)
!
      INTEGER       NPERIOD
      CHARACTER*20, POINTER :: PERNAM(:)
      CHARACTER*20, POINTER :: PERSFX(:)
      INTEGER     , POINTER :: PSTART(:)
      INTEGER     , POINTER :: PSTOP(:)
!
      INTEGER       NSVAI , NSVAO , ISWITR
      CHARACTER*20, POINTER :: VAINAM(:)
      CHARACTER*50, POINTER :: VAITXT(:)
      REAL        , POINTER :: VAIDEF(:)
      CHARACTER*20, POINTER :: VAONAM(:)
      CHARACTER*50, POINTER :: VAOTXT(:)
!
      INTEGER       IKSTAT, ISTAT , IKEY   , IFOUND, IERR_ALLOC,
     +              NOSTAT, ISPROC, IPERIOD, IRET  , IHULP1    ,
     +              IHULP2
      CHARACTER*20  KEY
      CHARACTER*4   CH4
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwqs1", ithndl )
!
      WRITE(LUNREP,2000)
      IPOSR = 0
      CALL RDSTAT ( LUNREP , IPOSR  , NPOS   , CCHAR  , VRSION ,
     +              ILUN   , LCH    , LSTACK , IOUTPT , DTFLG1 ,
     +              DTFLG3 , IERR   , NOSTAT , NKEY   , NOKEY  ,
     +              KEYNAM , KEYVAL , NPERIOD, PERNAM , PERSFX ,
     +              PSTART , PSTOP  )
!
!     set number of statistical processes (some once , some per period )
!
      IKSTAT = 1
      NSPROC = 0
      DO ISTAT = 1 , NOSTAT
         KEY='OUTPUT-OPERATION'
         CALL ZOEK(KEY,NOKEY(ISTAT),KEYNAM(IKSTAT:),20,IKEY)
         IF ( IKEY .GT. 0 ) THEN
            IKEY = IKSTAT + IKEY - 1
            KEY='STADAY'
            CALL ZOEK(KEY,1,KEYVAL(IKEY:),20,IFOUND)
            IF ( IFOUND .GT. 0 ) THEN
               NSPROC = NSPROC + 1
               GOTO 10
            ENDIF
            KEY='STADPT'
            CALL ZOEK(KEY,1,KEYVAL(IKEY:),20,IFOUND)
            IF ( IFOUND .GT. 0 ) THEN
               NSPROC = NSPROC + 1
               GOTO 10
            ENDIF
            KEY='STADSC'
            CALL ZOEK(KEY,1,KEYVAL(IKEY:),20,IFOUND)
            IF ( IFOUND .GT. 0 ) THEN
               NSPROC = NSPROC + NPERIOD
               GOTO 10
            ENDIF
            KEY='STAGEO'
            CALL ZOEK(KEY,1,KEYVAL(IKEY:),20,IFOUND)
            IF ( IFOUND .GT. 0 ) THEN
               NSPROC = NSPROC + NPERIOD
               GOTO 10
            ENDIF
            KEY='STAPRC'
            CALL ZOEK(KEY,1,KEYVAL(IKEY:),20,IFOUND)
            IF ( IFOUND .GT. 0 ) THEN
               NSPROC = NSPROC + NPERIOD
               GOTO 10
            ENDIF
            KEY='STAQTL'
            CALL ZOEK(KEY,1,KEYVAL(IKEY:),20,IFOUND)
            IF ( IFOUND .GT. 0 ) THEN
               NSPROC = NSPROC + NPERIOD
               GOTO 10
            ENDIF
   10       CONTINUE
         ENDIF
         IKSTAT = IKSTAT + NOKEY(ISTAT)
      ENDDO
!
      ALLOCATE (STA_NO_IN(NSPROC))
      ALLOCATE(STA_NO_OUT(NSPROC))
      ALLOCATE(STA_SWITR(NSPROC))
      ALLOCATE(STA_MODNAM(NSPROC))
!
!     emergency solution
!
      ALLOCATE(KEYNAM2(NKEY),KEYVAL2(NKEY))
      KEYNAM2=KEYNAM
      KEYVAL2=KEYVAL
!
!     report on periods
!
      WRITE(LUNREP,'(A,I6)') 'Number of periods defined:',NPERIOD
      WRITE(LUNREP,*)
      DO IPERIOD = 1 , NPERIOD
         WRITE(LUNREP,'(3A)') 'PERIOD [',PERNAM(IPERIOD),']'
         WRITE(LUNREP,'(3A)') 'SUFFIX [',PERSFX(IPERIOD),']'
         IF ( DTFLG1 ) THEN
            IHULP1 = PSTART(IPERIOD)
            IHULP2 = PSTOP(IPERIOD)
            WRITE(LUNREP,2020)
     *           IHULP1/31536000       , MOD(IHULP1,31536000)/86400,
     *           MOD(IHULP1,86400)/3600, MOD(IHULP1,3600)/60       ,
     *           MOD(IHULP1,60)        ,
     *           IHULP2/31536000       , MOD(IHULP2,31536000)/86400,
     *           MOD(IHULP2,86400)/3600, MOD(IHULP2,3600)/60       ,
     *           MOD(IHULP2,60)
         ELSE
            WRITE(LUNREP,2030) PSTART(IPERIOD),PSTOP(IPERIOD)
         ENDIF
      ENDDO
!
!     loop over the ouput operations, setup administration and report
!
      IKSTAT = 1
      ISPROC = 0
      DO ISTAT = 1 , NOSTAT
         WRITE(LUNREP,*)
         DO IKEY = 1 , NOKEY(ISTAT)
            WRITE(LUNREP,'(A,1X,A)') KEYNAM(IKSTAT+IKEY-1),
     +                               KEYVAL(IKSTAT+IKEY-1)
         ENDDO
         KEY='OUTPUT-OPERATION'
         CALL ZOEK(KEY,NOKEY(ISTAT),KEYNAM(IKSTAT:),20,IKEY)
         IF ( IKEY .GT. 0 ) THEN
            IKEY = IKSTAT + IKEY - 1
            KEY='STADAY'
            CALL ZOEK(KEY,1,KEYVAL(IKEY:),20,IFOUND)
            IF ( IFOUND .GT. 0 ) THEN
               ISPROC = ISPROC + 1
               CALL SETDAY ( LUNREP         , NOKEY(ISTAT)   ,
     +                       KEYNAM2(IKSTAT), KEYVAL2(IKSTAT),
     +                       DTFLG1         , DTFLG3         ,
     +                       ISPROC         , aProcesProp    ,
     +                       AllItems       , IERR           ,
     +                       NOWARN         )

               iret = ProcesPropCollAdd( StatProcesDef , aProcesProp )
               GOTO 100
            ENDIF
            KEY='STADPT'
            CALL ZOEK(KEY,1,KEYVAL(IKEY:),20,IFOUND)
            IF ( IFOUND .GT. 0 ) THEN
               ISPROC = ISPROC + 1
               CALL SETDPT ( LUNREP         , NOKEY(ISTAT)   ,
     +                       KEYNAM2(IKSTAT), KEYVAL2(IKSTAT),
     +                       ISPROC         , aProcesProp    ,
     +                       AllItems       , IERR           ,
     +                       NOWARN         )
               iret = ProcesPropCollAdd( StatProcesDef , aProcesProp )
               GOTO 100
            ENDIF
            KEY='STADSC'
            CALL ZOEK(KEY,1,KEYVAL(IKEY:),20,IFOUND)
            IF ( IFOUND .GT. 0 ) THEN
               DO IPERIOD = 1 , NPERIOD
                  WRITE(LUNREP,'(3A)') 'For period [',PERNAM(IPERIOD),']:'
                  ISPROC = ISPROC + 1
                  CALL SETDSC ( LUNREP         , NOKEY(ISTAT)   ,
     +                          KEYNAM2(IKSTAT), KEYVAL2(IKSTAT),
     +                          PERNAM(IPERIOD), PERSFX(IPERIOD),
     +                          PSTART(IPERIOD), PSTOP(IPERIOD) ,
     +                          ISPROC         , aProcesProp    ,
     +                          AllItems       , IERR           ,
     +                          NOWARN         )
                  iret = ProcesPropCollAdd( StatProcesDef , aProcesProp )
               ENDDO
               GOTO 100
            ENDIF
            KEY='STAGEO'
            CALL ZOEK(KEY,1,KEYVAL(IKEY:),20,IFOUND)
            IF ( IFOUND .GT. 0 ) THEN
               DO IPERIOD = 1 , NPERIOD
                  ISPROC = ISPROC + 1
                  WRITE(LUNREP,'(3A)') 'For period [',PERNAM(IPERIOD),']:'
                  CALL SETGEO ( LUNREP         , NOKEY(ISTAT)   ,
     +                          KEYNAM2(IKSTAT), KEYVAL2(IKSTAT),
     +                          PERNAM(IPERIOD), PERSFX(IPERIOD),
     +                          PSTART(IPERIOD), PSTOP(IPERIOD) ,
     +                          ISPROC         , aProcesProp    ,
     +                          AllItems       , IERR           ,
     +                          NOWARN         )
                  iret = ProcesPropCollAdd( StatProcesDef , aProcesProp )
               ENDDO
               GOTO 100
            ENDIF
            KEY='STAPRC'
            CALL ZOEK(KEY,1,KEYVAL(IKEY:),20,IFOUND)
            IF ( IFOUND .GT. 0 ) THEN
               DO IPERIOD = 1 , NPERIOD
                  ISPROC = ISPROC + 1
                  WRITE(LUNREP,'(3A)') 'For period [',PERNAM(IPERIOD),']:'
                  CALL SETPRC ( LUNREP         , NOKEY(ISTAT)   ,
     +                          KEYNAM2(IKSTAT), KEYVAL2(IKSTAT),
     +                          PERNAM(IPERIOD), PERSFX(IPERIOD),
     +                          PSTART(IPERIOD), PSTOP(IPERIOD) ,
     +                          ISPROC         , aProcesProp    ,
     +                          AllItems       , IERR           ,
     +                          NOWARN         )
                  iret = ProcesPropCollAdd( StatProcesDef , aProcesProp )
               ENDDO
               GOTO 100
            ENDIF
            KEY='STAQTL'
            CALL ZOEK(KEY,1,KEYVAL(IKEY:),20,IFOUND)
            IF ( IFOUND .GT. 0 ) THEN
               DO IPERIOD = 1 , NPERIOD
                  ISPROC = ISPROC + 1
                  WRITE(LUNREP,'(3A)') 'For period [',PERNAM(IPERIOD),']:'
                  CALL SETQTL ( LUNREP         , NOKEY(ISTAT)   ,
     +                          KEYNAM2(IKSTAT), KEYVAL2(IKSTAT),
     +                          PERNAM(IPERIOD), PERSFX(IPERIOD),
     +                          PSTART(IPERIOD), PSTOP(IPERIOD) ,
     +                          ISPROC         , aProcesProp    ,
     +                          AllItems       , IERR           ,
     +                          NOWARN         )
                  iret = ProcesPropCollAdd( StatProcesDef , aProcesProp )
               ENDDO
               GOTO 100
            ENDIF
            WRITE(LUNREP,*)
     +      'ERROR unrecognised operation:',KEYVAL(IKEY:)
            IERR = IERR + 1
  100       CONTINUE
         ELSE
            WRITE(LUNREP,*)
     +      'ERROR no operation defined for output-operation'
            IERR = IERR + 1
         ENDIF
         WRITE(LUNREP,'(A)') 'END-OUTPUT-OPERATION'
         IKSTAT = IKSTAT + NOKEY(ISTAT)
      ENDDO
!
      DEALLOCATE (KEYNAM,KEYVAL,NOKEY,STAT=IERR_ALLOC)
      DEALLOCATE (KEYNAM2,KEYVAL2,STAT=IERR_ALLOC)
      NSPROC = ISPROC
!
  500 CONTINUE
      IF(VRSION.GE.0.0) THEN
         WRITE ( LUNREP , 3000 ) 10
      END IF
!
      if ( timon ) call timstop( ithndl )
      RETURN
 2000 FORMAT ( /,' Output operations')
 2010 FORMAT ( /,' ERROR. DLWQS1 allocating memory :',I8)
 2020 FORMAT ( ' Start of period :',I2,'Y-',I3,'D-',I2,'H-',I2,
     *           'M-',I2,'S.',
     *        /' End of period   :',I2,'Y-',I3,'D-',I2,'H-',I2,
     *           'M-',I2,'S.')
 2030 FORMAT (  ' Start of period :        ' ,I10 ,
     &         /' End of period   :        ' ,I10 )
 3000 FORMAT (/1X, 59('*'),' B L O C K -',I2,' ',5('*')/)
      END
