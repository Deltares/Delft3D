!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
    use m_waq_precision
    use m_integration_scheme_25
    use m_integration_scheme_24
    use m_integration_scheme_23
    use m_integration_scheme_21_22
    use m_integration_scheme_18
    use m_integration_scheme_17
    use m_integration_scheme_16
    use m_integration_scheme_15
    use m_integration_scheme_13
    use m_integration_scheme_12
    use m_integration_scheme_11
    use m_integration_scheme_14
    use m_integration_scheme_7
    use m_integration_scheme_6
    use m_integration_scheme_5
    use m_integration_scheme_1
    use m_integration_scheme_0
    use m_startup_screen
    use m_srstop
    use m_monsys
    use m_cli_utils, only : retrieve_command_argument
    use m_open_waq_files


CONTAINS

    SUBROUTINE DELWQ2 (buffer, IMAXA, IMAXI, IMAXC, INIT, &
            ACTION, DLWQD)
        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !            DELWAQ - Deltares WAter Quality programme
        !
        !     FUNCTION      : Performs the waterquality simulations on a
        !                     consistent set of binary intermediate files.
        !
        !     LOGICAL UNITS : LUNIN  , input , binary common-block file
        !                       *    , output, user console file
        !
        !     SUBROUTINES CALLED : DLWQI0, initialises the system
        !                          integration_scheme_1, first   integration procedure
        !                          integration_scheme_5, fifth   integration procedure
        !                          integration_scheme_6, sixth   integration procedure
        !                          integration_scheme_7, seventh integration procedure
        !                          integration_scheme_11, 11th    integration procedure
        !                          integration_scheme_12, 12th    integration procedure
        !                          integration_scheme_13, 13th    integration procedure
        !                          integration_scheme_14, 14th    integration procedure
        !                          integration_scheme_15, 15th    integration procedure
        !                          integration_scheme_16, 16th    integration procedure
        !                          integration_scheme_17, 17th    integration procedure
        !                          integration_scheme_18, 18th    integration procedure
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
        use dlwqgrid_mod
        USE DLWQI0_MOD
        USE Timers
        use delwaq2_data
        use m_waq_data_buffer
        use m_actions
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace
        use m_sysj          ! Pointers in integer array workspace
        use m_sysc          ! Pointers in character array workspace
        use m_cli_utils, only : get_input_filename

        implicit none

        !
        !     Declaration of arguments
        !
        type(waq_data_buffer), target :: buffer
        INTEGER(kind = int_wp) :: IMAXA, IMAXI, IMAXC
        LOGICAL :: INIT
        LOGICAL :: exists
        INTEGER(kind = int_wp) :: ACTION
        TYPE(DELWAQ_DATA), TARGET :: DLWQD
        type(GridPointerColl), pointer, save :: GridPs               ! collection of all grid definitions


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
        INTEGER(kind = int_wp) :: LUNIN
        INTEGER(kind = int_wp), PARAMETER :: IPAGE = 64
        INTEGER(kind = int_wp), PARAMETER :: NLUN = 50
        INTEGER(kind = int_wp), PARAMETER :: LCHMAX = 255
        !
        !           input structure for boot-file
        !
        !
        INTEGER(kind = int_wp), SAVE :: LUN(NLUN)
        CHARACTER*(LCHMAX), SAVE :: LCHAR(NLUN)
        integer(kind = int_wp), save :: filtype(nlun)
        CHARACTER*(LCHMAX), SAVE :: RUNID
        LOGICAL, SAVE :: INIT2 = .TRUE. ! To suppress the start-up screen

        logical :: lfound
        integer(kind = int_wp) :: idummy, ierr2
        real(kind = real_wp) :: rdummy
        CHARACTER :: cdummy
        CHARACTER*2 :: C2
        !
        integer(kind = int_wp), save :: ithndl = 0
        !
        !     Local variables
        !
        INTEGER(kind = int_wp), SAVE :: INDX
        INTEGER(kind = int_wp) :: IERR
        INTEGER(kind = int_wp) :: IMR
        INTEGER(kind = int_wp) :: IMI
        INTEGER(kind = int_wp) :: IMC
        INTEGER(kind = int_wp) :: ILUN
        INTEGER(kind = int_wp) :: IERRD
        INTEGER(kind = int_wp) :: K

        !
        IF (INIT) THEN
            call timini ()
            ! for openda-usage, where multiple instances are launched,
            ! the time module does not work correctly.
            if (dlwqd%set_timer) timon = .true.
            timon = .true.
            if (timon) call timstrt("delwaq2", ithndl)
            !
            !        boot the system; read dimensions of sysn from delwaq03.wrk-file
            !
            CALL get_input_filename(RUNID, '.mon')
            LCHAR(1) = TRIM(RUNID) // '-delwaq03.wrk'

            !
            !        produce a user-friendly message if the 03 work file is missing,
            !        an indication that DELWAQ1 was not able to complete its job properly.
            !
            inquire(file = lchar(1), exist = exists)
            if (.not. exists) then
                write(*, '(a)') 'DELWAQ2 cannot run - the system work file is missing'
                write(*, '(2a)') '    File name: ', trim(lchar(1))
                write(*, '(2a)') '    Please check if DELWAQ1 ran correctly'
                call srstop(1)
            endif

            !
            !        the file does exist, so continue processing
            !
            CALL open_waq_files (LUNIN, LCHAR(1), 1, 2, IERR)
            IF (IERR > 0) GOTO 999
            READ  (LUNIN)   IN
            READ  (LUNIN)   II
            READ  (LUNIN)   IMR, IMI, IMC
            READ  (LUNIN) (LUN    (K), K = 1, NOLUN)
            READ  (LUNIN) (LCHAR  (K), K = 1, NOLUN)
            READ  (LUNIN) (filtype(K), K = 1, NOLUN)
            DO ILUN = 1, NOLUN
                CLOSE (LUN(ILUN))
            END DO
            close(lunin)

            CALL open_waq_files (LUN(19), LCHAR(19), 19, 1, IERRD)
            CALL SETMLU (LUN(19))

            !           Show startup screen
            !
            IF (INIT2) THEN
                INIT2 = .FALSE.
                CALL startup_screen (LUN(19))
            ENDIF

            IF (ACTION == ACTION_FULLCOMPUTATION) THEN
                WRITE(*, *)
                WRITE(*, '(A9,A)') '  runid: ', TRIM(RUNID)
                WRITE(*, *)
            ENDIF


            ! collaborative call to i0
            !
            IERR = 0
            gridps => dlwqd%gridps
            call dlwqi0 (buffer, nlun, imaxa, &
                    imaxi, imaxc, ipage, lun, lchar, &
                    filtype, gridps, dlwqd, ierr)
            !

            IF (IERR > 0) GOTO 992
            !
            !        end of initialisation
            !
            WRITE (*, *)
            WRITE (*, *) ' SIMULATION STARTED '
            WRITE (*, *)
            WRITE (*, *) ' INTEGRATION ROUTINE =', intsrt

        ENDIF

        !
        !     Store the local persistent variables
        !
        DLWQD%II = II
        DLWQD%IN = IN



        !         branch to the appropriate integration option

        select case (intsrt)

        case (0)     !      not transport, just processes
            call integration_scheme_0 (buffer, lun, lchar, action, dlwqd, gridps)

        case (1)     !      backward in space and time
            call integration_scheme_1 (buffer, lun, lchar, action, dlwqd, gridps)

        case (2, 3, 4) ! deprecated
            goto 991

        case (5)     !      Flux corrected transport
            call integration_scheme_5 (buffer, lun, lchar, action, dlwqd, gridps)

        case (6)     !      Direct steady state, backward differences in space
            call integration_scheme_6 (buffer, lun, lchar, action, dlwqd, gridps)

        case (7)     !      Direct steady state, central differences in space
            call integration_scheme_7 (buffer, lun, lchar, action, dlwqd, gridps)

        case (8, 9, 10) ! deprecated
            goto 991

        case (11)     !      Horizontal explicit upwind, vertical implicit central
            call integration_scheme_11 (buffer, lun, lchar, action, dlwqd, gridps)

        case (12)     !      Horizontal explicit FCT   , vertical implicit central
            call integration_scheme_12 (buffer, lun, lchar, action, dlwqd, gridps)

        case (13)     !      Horizontal explicit upwind, vertical implicit upwind
            call integration_scheme_13 (buffer, lun, lchar, action, dlwqd, gridps)

        case (14)     !      Horizontal explicit FCT   , vertical implicit upwind
            call integration_scheme_14 (buffer, lun, lchar, action, dlwqd, gridps)

        case (15)     !      GMRES, horizontal upwind, vertical upwind
            call integration_scheme_15 (buffer, lun, lchar, action, dlwqd, gridps)

        case (16)     !      GMRES, horizontal upwind, vertical central
            call integration_scheme_16 (buffer, lun, lchar, action, dlwqd, gridps)

        case (17)     !      stationary GMRES, horizontal upwind, vertical upwind
            call integration_scheme_17 (buffer, lun, lchar, action, dlwqd, gridps)

        case (18)     !      stationary GMRES, horizontal upwind, vertical central
            call integration_scheme_18 (buffer, lun, lchar, action, dlwqd, gridps)

        case (19, 20) ! deprecated
            goto 991

        case (21)     !      Self adjusting teta method (limiter Salezac)
            call integration_scheme_21_22 (buffer, lun, lchar, action, dlwqd, gridps)

        case (22)     !      Self adjusting teta method (limiter Boris and Book)
            call integration_scheme_21_22 (buffer, lun, lchar, action, dlwqd, gridps)

        case (23)     !      Leonards QUICKEST
            call integration_scheme_23 (buffer, lun, lchar, action, dlwqd, gridps)

        case (24)     !      Local flexible time step method by Leonard Postma
            call integration_scheme_24 (buffer, lun, lchar, action, dlwqd, gridps)

        case (25)     !      Special for emission module
            call integration_scheme_25 (buffer, lun, lchar, action, dlwqd, gridps)

        case default
            goto 990

        end select

        IF (ACTION == ACTION_FINALISATION    .OR. &
                ACTION == ACTION_FULLCOMPUTATION) THEN

            !     print timer-results

            if (timon) then
                call timstop (ithndl)
                call timdump (TRIM(RUNID) // '-timers.out')
                call timfinalize()
            endif

        endif

        return

        990 WRITE (*, *) ' ERROR: INTEGRATION OPTION NOT IMPLEMENTED'
        CALL SRSTOP(1)
        991 WRITE (*, *) ' ERROR: INTEGRATION OPTION DEPRECATED'
        CALL SRSTOP(1)
        992 WRITE (*, *) ' ERROR : INITIALISATION FAILED'
        CALL SRSTOP(1)
        999 WRITE (*, *) ' ERROR: NO VALID SET OF MODEL-INTERMEDIATE-FILES'
        CALL SRSTOP(1)
    END SUBROUTINE DELWQ2

END MODULE DELWAQ2
