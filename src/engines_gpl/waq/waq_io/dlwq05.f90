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
      module m_dlwq05
      use m_waq_precision
      use m_string_utils
      use m_opt0
      use m_dlwq5a
      use m_error_status

      implicit none

      contains


      SUBROUTINE DLWQ05 ( LUN    , LCHAR  , filtype, CAR    , IAR    , & 
                         RAR    , NRFTOT , NRHARM , NOBND  , NOSYS  , & 
                         NOTOT  , NOBTYP , IRMAX  , IIMAX  , DTFLG1 , & 
                         IWIDTH , INTSRT , DTFLG3 , SNAME  , & 
                         ICMAX  , IOUTPT , status   )

!       Deltares Software Centre

!>\file
!>                          Reads all inputs associated with open boundaries
!>\par  Description:
!>                          This routine reads:
!>                             - the boundary ID's (and names and types for modern files)
!>                             - the Tatcher-Harleman time lags
!>                             - the Open boundary concentrations


!      Subroutines called : CONVER
!                           OPT0
!                           CHECK
!                           CNVTIM
!                           RDTOK1 tokenized data reading

!      Logical units      : LUN(27) = unit stripped DELWAQ input file
!                           LUN(29) = unit formatted output file
!                           LUN( 2) = unit intermediate file (system)
!                           LUN( 3) = unit intermediate file (harmonics)
!                           LUN( 4) = unit intermediate file (pointers)
!                           LUN(14) = unit intermediate file (boundaries)

      use m_conver
      use m_check
      use m_srstop
      use m_cli_utils, only : retrieve_command_argument
      use rd_token     !   for the reading of tokens
      use timers       !   performance timers
      use date_time_utils, only : convert_relative_time

!     kind           function         name                Descriptipon

      integer(kind=int_wp), intent(in   ) :: irmax              !< size of the real workspace
      integer(kind=int_wp), intent(inout) :: lun(:)             !< array with unit numbers
      character( *),        intent(inout) :: lchar(:)           !< array with file names of the files
      integer(kind=int_wp), intent(inout) :: filtype(*)         !< type of binary file
      character( *),        intent(inout) :: car(:)             !< character workspace
      integer(kind=int_wp), intent(inout) :: iar(:)             !< integer workspace ( dump locations at entrance )
      real(kind=real_wp),   intent(inout) :: rar(irmax)         !< real    workspace
      integer(kind=int_wp), intent(inout) :: nrftot(*)          !< number of function items
      integer(kind=int_wp), intent(inout) :: nrharm(*)          !< number of harmonic items
      integer(kind=int_wp), intent(inout) :: nobnd              !< number of open model boundaries
      integer(kind=int_wp), intent(in   ) :: notot              !< total number of substances
      integer(kind=int_wp), intent(inout) :: nosys              !< number of transported substances
      integer(kind=int_wp), intent(  out) :: nobtyp             !< number of open model boundary types
      integer(kind=int_wp), intent(in   ) :: iimax              !< size of the integer workspace
      logical,              intent(in   ) :: dtflg1             !< 'date'-format 1st timescale
      integer(kind=int_wp), intent(in   ) :: iwidth             !< width of the output file
      integer(kind=int_wp), intent(in   ) :: intsrt             !< integration option
      logical,              intent(in   ) :: dtflg3             !< 'date'-format (F;ddmmhhss,T;yydddhh)
      character(20),        intent(inout) :: sname(:)           !< array with substance names
      integer(kind=int_wp), intent(in   ) :: icmax              !< size of the character workspace
      integer(kind=int_wp), intent(in   ) :: ioutpt             !< flag for more or less output

      integer(kind=int_wp) :: idef

      type(error_status) :: status !< current error status
 !
      CHARACTER*1   CDUMMY
      CHARACTER*255 CHULP
      LOGICAL       DISPER
      CHARACTER(LEN=20) , ALLOCATABLE   :: BNDID(:)             ! boundary id's 20 character
      CHARACTER(LEN=40) , ALLOCATABLE   :: BNDNAME(:)           ! boundary names
      CHARACTER(LEN=20) , ALLOCATABLE   :: BNDTYPE(:)           ! boundary types
      CHARACTER(LEN=256), ALLOCATABLE   :: BNDID_LONG(:)        ! array to buffer the non truncated boundary id's
      CHARACTER(LEN=256), ALLOCATABLE   :: BNDTYPE_LONG(:)      ! array to buffer the non truncated boundary types
      INTEGER(kind=int_wp), ALLOCATABLE ::  IBNDTYPE(:)         ! index boundary type
      real(kind=dp), allocatable        ::  drar(:)             !  double precission workspace (very large !lp)
      logical                         :: no_id_check            ! command line argument to skip double ID check
      real(kind=real_wp) ::  rdummy                  ! dummy real in argument list
      integer(kind=int_wp) ::  idummy                  ! dummy integer in argument list
      integer(kind=int_wp) ::  VOLUME
      integer(kind=int_wp) ::  ithndl = 0
      integer(kind=int_wp) ::  k, I, IERR_ALLOC
      integer(kind=int_wp) ::  ifact, lunwr, ierr2, iwar2, ifound, ityp2
      integer(kind=int_wp) ::  iaropt, nover, mxover, ibnd, it, nosubs
      integer(kind=int_wp) ::  ierrh, ihulp, rhulp, ifound2, l, itype
      if (timon) call timstrt( "dlwq05", ithndl )
!
!     Init
!
      DISPER  = .FALSE.
      VOLUME  = 0
      IFACT   =  1
      LUNUT   = LUN(29)
      LUNWR   = LUN( 2)
      IPOSR   =  0
      IERR2   =  0
      Iwar2   =  0
!
      IF ( NOBND == 0 ) THEN
         WRITE ( LUNUT , 2000 )
         IFOUND = GETTOKEN ( CHULP, IDUMMY, RDUMMY, ITYPE, IERR2 )
         IF (IERR2 == 2) THEN
            GOTO 175
         ELSE IF (ITYPE==2 .AND. IDUMMY==0) THEN
            WRITE ( LUNUT , 2120 )
            GOTO 170
         ENDIF
         WRITE ( LUNUT , 2001 )
         call status%increase_error_count()
         GOTO 175
      ENDIF
!
!     read boundary names, from version 4.9 on names are ID's
!                                              names are 40 characters
!                                              types are 20 characters
!
      ALLOCATE(BNDID(NOBND),BNDNAME(NOBND),BNDTYPE(NOBND),BNDID_LONG(NOBND),BNDTYPE_LONG(NOBND),IBNDTYPE(NOBND),STAT=IERR_ALLOC)
      IF ( IERR_ALLOC /= 0 ) THEN
         WRITE ( LUNUT , 2300 ) IERR_ALLOC
         WRITE ( LUNUT , 2310 ) NOBND
         CALL SRSTOP(1)
      ENDIF
      NOBTYP = 0
      IF ( IOUTPT < 3 ) THEN
         WRITE ( LUNUT , 2005 )
      ELSE
         IF ( IWIDTH == 5 ) THEN
            WRITE ( LUNUT , 2010 )
         ELSE
            WRITE ( LUNUT , 2020 )
         ENDIF
      ENDIF
      IERR2  =  0

      DO I = 1 , NOBND
         IBNDTYPE(I)     =  0
         BNDID_LONG(I)   = ' '
         BNDNAME(I)      = ' '
         BNDTYPE_LONG(I) = ' '

         ! read ID, do not truncate yet

         ITYPE = 1
         CALL RDTOK1 ( LUNUT , ILUN  , LCH          , LSTACK, CCHAR , & 
                      IPOSR , NPOS  , BNDID_LONG(I), IHULP , RHULP , & 
                      ITYPE , IERR2 )
         IF ( IERR2 > 0 ) GOTO 170


         ! read also name and type

         ITYPE = 1
         CALL RDTOK1 ( LUNUT , ILUN , LCH       , LSTACK, CCHAR , & 
                      IPOSR , NPOS , BNDNAME(I), IHULP , RHULP , & 
                      ITYPE , IERR2)
         IF ( IERR2 > 0 ) GOTO 170
         ITYPE = 1
         CALL RDTOK1 ( LUNUT , ILUN , LCH            , LSTACK, CCHAR , & 
                      IPOSR , NPOS , BNDTYPE_LONG(I), IHULP , RHULP , & 
                      ITYPE , IERR2)
         IF ( IERR2 > 0 ) GOTO 170


         IF ( BNDID_LONG(I)  == ' ' ) WRITE ( BNDID_LONG(I), '(''Boundary-ID'',I7)' ) I
         IF ( BNDNAME(I)     == ' ' ) WRITE ( BNDNAME(I), '(''Boundary name '',I7)' ) I
         IF ( BNDTYPE_LONG(I)== ' ' ) BNDTYPE_LONG(I) = 'Boundary type 1'

         BNDID(I)   = BNDID_LONG(I)
         BNDTYPE(I) = BNDTYPE_LONG(I)

         IF ( IOUTPT >= 3 ) THEN
            IF ( IWIDTH == 5 ) THEN
               WRITE ( LUNUT, 2030 ) BNDID(I),BNDNAME(I),BNDTYPE(I)
            ELSE
               WRITE ( LUNUT, 2040 ) I,BNDID(I),BNDNAME(I),BNDTYPE(I)
            ENDIF
         ENDIF

         ! check for unique ID, error if non-truncated ID is unique otherwise warning, can be skipped if input is generated

         call retrieve_command_argument ( '-no_id_check' , 0    , no_id_check, idummy, rdummy, cdummy, ierr2)
         if ( .not. no_id_check ) then
            IFOUND = index_in_array(BNDID(I),BNDID(:I-1))
            IF ( IFOUND >= 0 ) THEN
               IFOUND2 = index_in_array(BNDID_LONG(I),BNDID_LONG(:I-1))
               IF ( IFOUND == IFOUND2 ) THEN
                  WRITE(LUNUT,2270) BNDID(I)
                  call status%increase_warning_count()
               ELSE
                  WRITE(LUNUT,2280) BNDID(I)
                  call status%increase_error_count()
               ENDIF
            ENDIF
         endif

         ! check if truncated type and non truncated type give the same number

         ITYPE = index_in_array(BNDTYPE(I),BNDTYPE(:nobtyp))
         ITYP2 = index_in_array(BNDTYPE_LONG(I),BNDTYPE_LONG(:nobtyp))
         IF ( ITYPE /= ITYP2 ) THEN
            WRITE(LUNUT,2290) TRIM(BNDTYPE_LONG(I))
            call status%increase_error_count()
         ENDIF

         ! if type found set type, otherwise add type

         IF ( ITYPE > 0 ) THEN
            IBNDTYPE(I) = ITYPE
         ELSE
            NOBTYP = NOBTYP + 1
            BNDTYPE(NOBTYP)      = BNDTYPE(I)
            BNDTYPE_LONG(NOBTYP) = BNDTYPE_LONG(I)
            IBNDTYPE(I)          = NOBTYP
         ENDIF

         ! write ID and name to system file

         WRITE ( LUNWR )  BNDID(I), BNDNAME(I)

      end do

      WRITE ( LUNUT ,   *  )
      WRITE ( LUNUT , 2060 ) NOBTYP
      IF ( IOUTPT < 2 ) THEN
         WRITE ( LUNUT , 2065 )
      ELSE
         WRITE ( LUNUT , 2066 )
         DO I = 1 , NOBTYP
            WRITE ( LUNUT , 2070 ) I, BNDTYPE(I)
         end do
         WRITE ( LUNUT ,   *  )
      ENDIF
      WRITE ( LUNWR )  ( BNDTYPE(I) , I = 1, NOBTYP )
      WRITE ( LUNWR )  ( IBNDTYPE(I), I = 1, NOBND  )
      DEALLOCATE(BNDNAME,BNDID_LONG,BNDTYPE_LONG,IBNDTYPE)
!
!     dummy time lags
!
      IF ( NOSYS == 0 ) THEN
         WRITE ( LUNWR ) ( 0 , I=1, NOBND )
         WRITE ( LUNUT , 2090 )
         GOTO 170
      ENDIF
!
!        Read time lags
!        Note:
!        We may encounter strings instead, in that case skip
!        until we find an integer
!
      ITYPE = 2
      CALL RDTOK1 ( LUNUT , ILUN  , LCH   , LSTACK, CCHAR , & 
                   IPOSR , NPOS  , CDUMMY, IAROPT, RHULP , & 
                   ITYPE , IERR2 )
      IF ( IERR2 > 0 ) THEN
         WRITE ( LUNUT , 2101 )
         ITYPE = 1
         DO
            CALL RDTOK1 ( LUNUT , ILUN  , LCH   , LSTACK, CCHAR , & 
                         IPOSR , NPOS  , CDUMMY, IAROPT, RHULP , & 
                         ITYPE , IERR2 )
            READ( CDUMMY, *, IOSTAT = IERR2 ) IAROPT
            IF ( IERR2 == 0 ) THEN
               EXIT
            ENDIF
         ENDDO
      ENDIF

      WRITE ( LUNUT , 2100 ) IAROPT
      GOTO ( 60 , 70 , 110 ) IAROPT+1
      WRITE ( LUNUT , 2110 )
      call status%increase_error_count()
      GOTO 160
!
!        no time lags
!
   60 WRITE ( LUNUT , 2120 )
      WRITE ( LUNWR ) ( 0 , I=1, NOBND )
      GOTO 160
!
!       time lags constant without defaults
!
   70 WRITE ( LUNUT , 2130 )
      IF ( IIMAX < NOBND ) THEN
        WRITE ( LUNUT , 2140 ) NOBND,IIMAX,NOBND-IIMAX
        DO K = 1, NOBND
          ITYPE = 2
          CALL RDTOK1 ( LUNUT , ILUN  , LCH   , LSTACK, CCHAR , & 
                       IPOSR , NPOS  , CDUMMY, IDUMMY, RHULP , & 
                       ITYPE , IERR2 )
          IF ( IERR2 > 0 ) GOTO 170
        end do
        call status%increase_error_count()
        GOTO 160
      ENDIF
      DO K = 1, NOBND
         ITYPE = 2
         CALL RDTOK1 ( LUNUT , ILUN  , LCH   , LSTACK, CCHAR , & 
                      IPOSR , NPOS  , CDUMMY, IAR(K), RHULP , & 
                      ITYPE , IERR2 )
         IF ( IERR2 > 0 ) GOTO 170
      end do
      IF ( IOUTPT < 3 ) THEN
         WRITE ( LUNUT , 2145 )
      ELSE
         WRITE ( LUNUT , 2150 )
      ENDIF
      IF ( DTFLG1 ) THEN
           CALL CONVER ( IAR   , NOBND , IFACT , DTFLG1 , DTFLG3 )
           IF ( IOUTPT >= 3 ) WRITE ( LUNUT , 2160 ) &
              ( IAR(K)/31536000       , MOD(IAR(K),31536000)/86400, & 
                MOD(IAR(K),86400)/3600, MOD(IAR(K),3600)/60       , & 
                MOD(IAR(K),60)        ,      K = 1 , NOBND        )
      ELSE
           IF ( IOUTPT >= 3 ) WRITE ( LUNUT , 2170 ) &
              ( IAR(K),                      K = 1 , NOBND        )
      ENDIF
      DO I=1,NOBND
      IF ( IAR(I) < 0 ) THEN
           WRITE ( LUNUT , 2180 ) IAR(I)
           call status%increase_error_count()
      ENDIF
      end do
      WRITE ( LUNWR ) ( IAR(K) , K=1,NOBND )
      GOTO 160
!
!       time lags constant with defaults
!
  110 WRITE ( LUNUT , 2190 )
      ITYPE = 2
      CALL RDTOK1 ( LUNUT , ILUN  , LCH   , LSTACK, CCHAR , & 
                   IPOSR , NPOS  , CDUMMY, IDEF  , RHULP , & 
                   ITYPE , IERR2 )
      IF ( IERR2 > 0 ) GOTO 170
      IF ( IDEF < 0 ) THEN
         WRITE ( LUNUT , 2180 ) IDEF
         call status%increase_error_count()
      ENDIF
!            fill the array with the default
      DO I = 1,MIN(IIMAX,NOBND)
         IAR(I) = IDEF
      end do
!            nr of overridings
      ITYPE = 2
      CALL RDTOK1 ( LUNUT , ILUN  , LCH   , LSTACK, CCHAR , & 
                   IPOSR , NPOS  , CDUMMY, NOVER , RHULP , & 
                   ITYPE , IERR2 )
      IF ( IERR2 > 0 ) GOTO 170
      IF ( DTFLG1 ) THEN
         call convert_relative_time ( idef , IFACT , DTFLG1, DTFLG3 )
         WRITE ( LUNUT , 2210 ) & 
                idef /31536000       , MOD(idef ,31536000)/86400, & 
                MOD(idef ,86400)/3600, MOD(idef ,3600)/60       , & 
                MOD(idef ,60)        , NOVER
      ELSE
           WRITE ( LUNUT , 2220 ) IDEF, NOVER
      ENDIF
      MXOVER = IIMAX - NOBND
!            overridings
      DO K = 1, MIN( NOVER, MXOVER)
         ITYPE = 2
         CALL RDTOK1 ( LUNUT , ILUN  , LCH   , LSTACK      , CCHAR , & 
                      IPOSR , NPOS  , CDUMMY, IAR(K+NOBND), RHULP , & 
                      ITYPE , IERR2 )
         IF ( IERR2 > 0 ) GOTO 170
         IBND  = MAX(1,MIN(IABS(IAR(K+NOBND)),NOBND))
         ITYPE = 2
         CALL RDTOK1 ( LUNUT , ILUN  , LCH   , LSTACK   , CCHAR , & 
                      IPOSR , NPOS  , CDUMMY, IAR(IBND), RHULP , & 
                      ITYPE , IERR2 )
         IF ( IERR2 > 0 ) GOTO 170
      end do
!
      DO K = 1, NOVER-MXOVER
         ITYPE = 2
         CALL RDTOK1 ( LUNUT , ILUN  , LCH   , LSTACK, CCHAR , & 
                      IPOSR , NPOS  , CDUMMY, IDUMMY, RHULP , & 
                      ITYPE , IERR2 )
         IF ( IERR2 > 0 ) GOTO 170
         ITYPE = 2
         CALL RDTOK1 ( LUNUT , ILUN  , LCH   , LSTACK, CCHAR , & 
                      IPOSR , NPOS  , CDUMMY, IDUMMY, RHULP , & 
                      ITYPE , IERR2 )
         IF ( IERR2 > 0 ) GOTO 170
      end do
      IF ( NOVER > MXOVER ) THEN
         WRITE ( LUNUT , 2200 ) NOBND,NOVER,IIMAX,NOBND+NOVER-IIMAX
         call status%increase_error_count()
         GOTO 160
      ENDIF
      IF ( DTFLG1 ) & 
        CALL CONVER ( IAR   , NOBND , IFACT , DTFLG1 , DTFLG3 )
      IF ( NOVER > 0 .AND. IOUTPT >= 3 ) WRITE ( LUNUT , 2230 )
      DO I=1, NOVER
         IBND = IABS( IAR(I+NOBND) )
         IF ( IBND > NOBND .OR. IBND == 0 ) THEN
              WRITE ( LUNUT , 2180 ) IAR(I+NOBND)
              call status%increase_error_count()
         ELSEIF ( IOUTPT >= 3 ) THEN
            IT =   IAR (IBND)
            IF ( DTFLG1 ) THEN
               WRITE ( LUNUT , 2240 ) IBND  , & 
                      IT/31536000       , MOD(IT,31536000)/86400, & 
                      MOD(IT,86400)/3600, MOD(IT,3600)/60       , & 
                      MOD(IT,60)
            ELSE
               WRITE ( LUNUT , 2250 )  IBND  ,IT
            ENDIF
         ENDIF
      end do
      WRITE ( LUNWR ) ( IAR(K) , K=1,NOBND )
!
!        Read boundary concentrations
!
!
!           This IF block stands for the new input processing
!
  160 WRITE ( LUNUT , 2260 )
      K = NOBND+1
      L = NOBND+NOBTYP+1
      allocate( drar(irmax) )             ! this array is 100 mb lp
       call dlwq5a ( lun    , lchar  , 14     , iwidth , icmax  , & 
                   car    , iimax  , iar    , irmax  , rar    , & 
                   sname  , bndid  , bndtype(1:nobtyp), nobnd  , nosys  , & 
                   nobtyp , drar   , dtflg1 , dtflg3 , & 
                   ioutpt , ierr2  , status)
      deallocate( drar )
      deallocate( bndid, bndtype )

      IF ( IERR2 ==  0 ) goto 180
      IF ( IERR2 >  0 ) THEN
        call status%increase_error_count_with(ierr2)
         IERR2 = 0
         GOTO 170
      ENDIF

      IF ( INTSRT == 6 .OR. INTSRT == 7 ) THEN
         NOSUBS = NOTOT
      ELSE
         NOSUBS = NOSYS
      ENDIF
!          IERRH = -1 signals OPT0 that it is boundaries to deal with
      IERRH = -1
      call opt0   ( lun    , 14     , 0        , 0        , nobnd  , & 
                   nosubs , nosubs , nrftot(8), nrharm(8), ifact  , & 
                   dtflg1 , disper , volume   , iwidth   , lchar  , & 
                   filtype, dtflg3 , ioutpt   , ierrh  , & 
                   status   , .false.)
      call status%increase_error_count_with(ierrh)
!
      IERR2 = 0
  170 CONTINUE
      IF ( ALLOCATED(BNDID  ) ) DEALLOCATE(BNDID  )
      IF ( ALLOCATED(BNDTYPE) ) DEALLOCATE(BNDTYPE)
      IF ( IERR2 > 0 ) call status%increase_error_count()
      IF ( IERR2 == 3 ) CALL SRSTOP(1)
  175 call check  ( chulp  , iwidth , 5      , ierr2  , status)
  180 if ( timon ) call timstop( ithndl )
      RETURN
!
!       Output formats
!
 2000 FORMAT ( //,' No boundary conditions' )
 2001 FORMAT ( //,' ERROR: Without boundary conditions only optional specification of zero time lags allowed!' )
 2005 FORMAT (  /,' Names of open boundaries are printed for', & 
                 ' output option 3 and higher !' )
 2010 FORMAT (  /,' Boundary-IDs:       boundary-names:    ',20X, & 
                 ' boundary-types:',/  )
 2020 FORMAT (  /,' Bound nr:      boundary-IDs:       boundary-names:', & 
                 '                         boundary-types:',/  )
 2030 FORMAT (   A20,A40,A20   )
 2040 FORMAT (   I7,9X,A20,A40,A20 )
 2060 FORMAT (    ' Number of different boundary types: ', I4  )
 2065 FORMAT (    ' Boundary types printed for output option is 2', & 
                 ' or higher !' )
 2066 FORMAT (    ' Type:  Type-string' )
 2070 FORMAT (      I6, 2X, A20  )
 2090 FORMAT ( //,' No active systems; no boundary conditions' )
 2100 FORMAT (  /,' Time lag option:',I3 )
 2101 FORMAT (  /,' Note: Skipping superfluous boundary names' )
 2110 FORMAT (    ' ERROR: Option not implemented')
 2120 FORMAT (    ' No time lags' )
 2130 FORMAT (    ' Constant time lags without defaults' )
 2140 FORMAT ( /,' ERROR the number of boundaries (',I7,') exceeds', & 
                ' the maximum (',I7,').', & 
               /' The maximum is limited by INTEGER array space', & 
               /' Consult your system manager to obtain ',I7,' words', & 
                ' of additional storage.' )
 2145 FORMAT (    ' Values will be printed for output option 3', & 
                 ' and higher !' )
 2150 FORMAT (    ' Values :'  )
 2160 FORMAT (     4(3X,I2,'Y-',I3,'D-',I2,'H-',I2,'M-',I2,'S ') )
 2170 FORMAT (     3X,10I7 )
 2180 FORMAT (    ' ERROR, invalid time lag:',I10)
 2190 FORMAT (    ' Constant time lags with defaults' )
 2200 FORMAT ( /,' ERROR the number of boundaries (',I7,') plus the', & 
               /' number of overridings (',I7,')  exceeds', & 
                ' the maximum (',I7,').', & 
               /' The maximum is limited by INTEGER array space', & 
               /' Consult your system manager to obtain ',I7,' words', & 
                ' of additional storage.' )
 2210 FORMAT (    ' Default value     :', & 
                   I2,'Y-',I3,'D-',I2,'H-',I2,'M-',I2,'S ' & 
               /,' Number of overridings :',I4 )
 2220 FORMAT (    ' Default value     :',I7, & 
               /,' Number of overridings :',I4 )
 2230 FORMAT (    ' Number       values',/ )
 2240 FORMAT (      I6,7X,I2,'Y-',I3,'D-',I2,'H-',I2,'M-',I2,'S ')
 2250 FORMAT (      I6,7X,I7)
 2260 FORMAT (  /,' Boundary concentrations' )
 2270 FORMAT (    ' WARNING: boundary ID is not unique:',A)
 2280 FORMAT (    ' ERROR: truncated boundary ID is not unique:',A)
 2290 FORMAT (    ' ERROR: truncated boundary type is not unique:',A)
 2300 FORMAT (    ' ERROR: allocating boundary arrays:',I7)
 2310 FORMAT (    ' number of boundaries             :',I7)
!
      END

      end module m_dlwq05
