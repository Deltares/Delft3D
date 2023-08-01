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

!     Module DELWAQ2:
!     - Encapsulate the interface of DELWQ2 and DLWQI0:
!       A, J and C are now pointers to arrays
!
      MODULE DELWAQ2
      use m_dlwqnq
      use m_dlwqnp
      use m_dlwqno
      use m_dlwqnm
      use m_dlwqnj
      use m_dlwqni
      use m_dlwqnh
      use m_dlwqng
      use m_dlwqnf
      use m_dlwqne
      use m_dlwqnd
      use m_dlwqnc
      use m_dlwqnb
      use m_dlwqna
      use m_dlwqn9
      use m_dlwqn8
      use m_dlwqn7
      use m_dlwqn6
      use m_dlwqn5
      use m_dlwqn4
      use m_dlwqn3
      use m_dlwqn3
      use m_dlwqn2
      use m_dlwqn1
      use m_dlwqn0
      use m_startup_screen
      use m_srstop
      use m_monsys
      use m_gkwini
      use m_getcom
      use m_open_waq_files


      CONTAINS

      SUBROUTINE DELWQ2 ( A, J, C, IMAXA, IMAXI, IMAXC, INIT,
     &                    ACTION, DLWQD                     )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!            DELWAQ - Deltares WAter Quality programme
!
!                     Version 4.30           june 1997
!                     Replaces:
!                     Version 4.22           feb 1997
!                     Version 4.21           jan 1997
!                     Version 4.20           may 1996
!                     Version 4.01           may 1994
!                     Version 4.00           september 1993
!                     Release 3.05           november 1991.
!                     Release 3.0 - RWS-DIV, june 1988.
!                     Release 2.0 of july     1984.
!                     Release 1.0 of december 1981.
!
!     INFORMATION   : Deltares
!                     L. Postma,
!                     Rotterdamse weg 185,
!                     P.O. Box 177,
!                     2600 MH Delft,
!                     Netherlands.
!                     telephone (31) 15-569353
!                     telefax   (31) 15-619674
!
!     FUNCTION      : Performs the waterquality simulations on a
!                     consistent set of binary intermediate files.
!
!     LOGICAL UNITS : LUNIN  , input , binary common-block file
!                       *    , output, user console file
!
!     SUBROUTINES CALLED : DLWQI0, initialises the system
!                          DLWQN1, first   integration procedure
!                          DLWQN2, second  integration procedure
!                          DLWQN3, third   integration procedure
!                          DLWQN4, fourth  integration procedure
!                          DLWQN5, fifth   integration procedure
!                          DLWQN6, sixth   integration procedure
!                          DLWQN7, seventh integration procedure
!                          DLWQN8, eighth  integration procedure
!                          DLWQN9, nineth  integration procedure
!                          DLWQNB, tenth   integration procedure
!                          DLWQNC, 11th    integration procedure
!                          DLWQND, 12th    integration procedure
!                          DLWQNE, 13th    integration procedure
!                          DLWQNA, 14th    integration procedure
!                          DLWQNF, 15th    integration procedure
!                          DLWQNG, 16th    integration procedure
!                          DLWQNH, 17th    integration procedure
!                          DLWQNI, 18th    integration procedure
!                          DLWQNJ, 19+20   integration procedure
!                          SRSTOP, stops execution
!                          open_waq_files, opens files
!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH  DESCRIPTION
!     ---------------------------------------------------------
!     A       REAL     IMAXA   real      workspace array
!     J       INTEGER  IMAXJ   integer   workspace array
!     C       CHAR*20  IMAXC   character workspace array
!     DLWQD   TYPE(..) 1       derived type for persistent storage
!     IMAXA   INTEGER  1       maximum real      workspace array
!     IMAXI   INTEGER  1       maximum integer   workspace array
!     IMAXC   INTEGER  1       maximum character workspace array
!     INIT    LOGICAL  1       if T boot the system if F no initialisation
!     ACTION  INTEGER  1       indication of the action to be performed
!
      use grids
      USE DLWQI0_MOD
      USE Timers
      use delwaq2_data
      use m_actions
      use m_sysn          ! System characteristics
      use m_sysi          ! Timer characteristics
      use m_sysa          ! Pointers in real array workspace
      use m_sysj          ! Pointers in integer array workspace
      use m_sysc          ! Pointers in character array workspace
      use m_dhgnam

      implicit none

!
!     Declaration of arguments
!
      INTEGER       IMAXA , IMAXI , IMAXC
      INTEGER, DIMENSION(:), POINTER          :: J
      REAL, DIMENSION(:), POINTER             :: A
      CHARACTER(LEN=*), DIMENSION(:), POINTER :: C
      LOGICAL                                 :: INIT
      LOGICAL                                 :: exists
      INTEGER                                 :: ACTION
      TYPE(DELWAQ_DATA), TARGET               :: DLWQD
      type(GridPointerColl), pointer          :: GridPs               ! collection of all grid definitions


!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH  DESCRIPTION
!     ---------------------------------------------------------
!     LUNIN   INTEGER  1       unit nummer of the common BOOT-file
!     IPAGE   INTEGER  1       pagelength for output in lines
!     NLUN    INTEGER  1       number of unit numbers
!     LCHMAX  INTEGER  1       length file names
!
!
!     Local declarations
!
      INTEGER            ::   LUNIN
      INTEGER, PARAMETER ::   IPAGE  =    64
      INTEGER, PARAMETER ::   NLUN   =    50
      INTEGER, PARAMETER ::   LCHMAX =   255
!
!           input structure for boot-file
!
!
      INTEGER, SAVE            :: LUN(NLUN)
      CHARACTER*(LCHMAX), SAVE :: LCHAR(NLUN)
      integer, save            :: filtype(nlun)
      CHARACTER*(LCHMAX), SAVE :: RUNID
      LOGICAL, SAVE            :: INIT2        = .TRUE. ! To suppress the start-up screen

      logical                  :: lfound
      integer                  :: idummy, ierr2
      real                     :: rdummy
      CHARACTER                :: cdummy
      CHARACTER*2              :: C2
!
      integer(4), save         :: ithndl = 0
!
!     Local variables
!
      INTEGER, SAVE            :: INDX
      INTEGER                  :: IERR
      INTEGER                  :: IMR
      INTEGER                  :: IMI
      INTEGER                  :: IMC
      INTEGER                  :: ILUN
      INTEGER                  :: IERRD
      INTEGER                  :: K

!
      IF ( INIT ) THEN
         call timini ( )
         ! for openda-usage, where multiple instances are launched,
         ! the time module does not work correctly.
         if ( dlwqd%set_timer ) timon = .true.
         timon = .true.
         if (timon) call timstrt( "delwaq2", ithndl )
!
!        boot the system; read dimensions of sysn from delwaq03.wrk-file
!
         CALL DHGNAM(RUNID,'.mon')
         LCHAR(1) = TRIM(RUNID)//'-delwaq03.wrk'

!
!        produce a user-friendly message if the 03 work file is missing,
!        an indication that DELWAQ1 was not able to complete its job properly.
!
         inquire( file = lchar(1), exist = exists )
         if ( .not. exists ) then
             write( *, '(a)'  ) 'DELWAQ2 cannot run - the system work file is missing'
             write( *, '(2a)' ) '    File name: ', trim(lchar(1))
             write( *, '(2a)' ) '    Please check if DELWAQ1 ran correctly'
             call srstop( 1 )
         endif

!
!        the file does exist, so continue processing
!
         CALL open_waq_files ( LUNIN , LCHAR(1), 1     , 2     , IERR  )
         IF ( IERR .GT. 0 ) GOTO 999
         READ  ( LUNIN )   IN
         READ  ( LUNIN )   II
         READ  ( LUNIN )   IMR     , IMI  , IMC
         READ  ( LUNIN ) ( LUN    (K), K = 1, NOLUN )
         READ  ( LUNIN ) ( LCHAR  (K), K = 1, NOLUN )
         READ  ( LUNIN ) ( filtype(K), K = 1, NOLUN )
         DO ILUN = 1, NOLUN
            CLOSE ( LUN(ILUN) )
         END DO
         close(lunin)


         CALL open_waq_files ( LUN(19) , LCHAR(19) , 19    , 1    , IERRD  )
         CALL SETMLU ( LUN(19) )

!           Show startup screen
!
            IF ( INIT2 ) THEN
               INIT2 = .FALSE.
               CALL startup_screen (LUN(19))
            ENDIF

            IF (ACTION .EQ. ACTION_FULLCOMPUTATION) THEN
               WRITE(*,*)
               WRITE(*,'(A9,A)') '  runid: ',TRIM(RUNID)
               WRITE(*,*)
            ENDIF


! collaborative call to i0
!
         IERR = 0
         gridps => dlwqd%gridps
         call dlwqi0 ( nlun   , a      , j      , c      , imaxa  ,
     &                 imaxi  , imaxc  , ipage  , lun    , lchar  ,
     &                 filtype, gridps , dlwqd  , ierr   )
!

         IF ( IERR .GT. 0 ) GOTO 992
!
!        end of initialisation
!
         WRITE ( * , *)
         WRITE ( * , * ) ' SIMULATION STARTED '
         WRITE ( * , * )
         WRITE ( * , * ) ' INTEGRATION ROUTINE =', intsrt

      ENDIF

!
!     Store the local persistent variables
!
      DLWQD%II = II
      DLWQD%IN = IN



!         branch to the appropriate integration option

      select case ( intsrt )

         case (  0 )     !      not transport, just processes
            call dlwqn0 ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case (  1 )     !      backward in space and time
            call dlwqn1 ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case (  2 )     !      modified 2nd order Runge Kutta
            call dlwqn2 ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case (  3 )     !      2nd order Lax Wendroff
            call dlwqn3 ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case (  4 )     !      Aternating direction implicit
            call dlwqn4 ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case (  5 )     !      Flux corrected transport
            call dlwqn5 ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case (  6 )     !      Direct steady state, backward differences in space
            call dlwqn6 ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case (  7 )     !      Direct steady state, central differences in space
            call dlwqn7 ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case (  8 )     !      Iteratively steady state, backward differences in space
            call dlwqn8 ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case (  9 )     !      Iteratively steady state, central differences in space
            call dlwqn9 ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 10 )     !      Fully implicit, direct method, upwind
            call dlwqnb ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 11 )     !      Horizontal explicit upwind, vertical implicit central
            call dlwqnc ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 12 )     !      Horizontal explicit FCT   , vertical implicit central
            call dlwqnd ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 13 )     !      Horizontal explicit upwind, vertical implicit upwind
            call dlwqne ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 14 )     !      Horizontal explicit FCT   , vertical implicit upwind
            call dlwqna ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 15 )     !      GMRES, horizontal upwind, vertical upwind
            call dlwqnf ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 16 )     !      GMRES, horizontal upwind, vertical central
            call dlwqng ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 17 )     !      stationary GMRES, horizontal upwind, vertical upwind
            call dlwqnh ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 18 )     !      stationary GMRES, horizontal upwind, vertical central
            call dlwqni ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 19 )     !      TRISULA-ADI 1 (vertically upwind)
            call dlwqnj ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 20 )     !      TRISULA-ADI 2 (vertically central)
            call dlwqnj ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 21 )     !      Self adjusting teta method (limiter Salezac)
            call dlwqnm ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 22 )     !      Self adjusting teta method (limiter Boris and Book)
            call dlwqnm ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 23 )     !      Leonards QUICKEST
            call dlwqno ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 24 )     !      Local flexible time step method by Leonard Postma
            call dlwqnp ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case ( 25 )     !      Special for emission module
            call dlwqnq ( a , j , c , lun , lchar, action, dlwqd, gridps )

         case default
            goto 990

      end select

      IF ( ACTION == ACTION_FINALISATION    .OR.
     &     ACTION == ACTION_FULLCOMPUTATION        ) THEN

!     print timer-results
!     Note: removed printing of timers to monitoring file



          if ( timon ) then
             call timstop ( ithndl )
             call timdump ( TRIM(RUNID)//'-timers.out' )
             call timfinalize()
          endif

      endif

      return

  990 WRITE ( * , * ) ' ERROR: INTEGRATION OPTION NOT IMPLEMENTED'
      CALL SRSTOP(1)
  991 WRITE ( * , * ) ' ERROR: INTEGRATION OPTION NOT IMPLEMENTED in online mode'
      CALL SRSTOP(1)
  992 WRITE ( * , * ) ' ERROR : INITIALISATION FAILED'
      CALL SRSTOP(1)
  999 WRITE ( * , * ) ' ERROR: NO VALID SET OF MODEL-INTERMEDIATE-FILES'
      CALL SRSTOP(1)
      END SUBROUTINE DELWQ2

      END MODULE DELWAQ2
