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

      SUBROUTINE DLWQ5B ( LUNUT  , IPOSR  , NPOS   , CCHAR  , CAR    ,
     *                    IAR    , ICMAX  , IIMAX  , ANAME  , ATYPE  ,
     *                    NTITM  , NTTYPE , NOITM  , NOITS  , CHKFLG ,
     *                    CALLR  , ILUN   , LCH    , LSTACK , VRSION ,
     *                    ITYPE  , RAR    , NCONST , ITMNR  , CHULP  ,
     *                                      IOUTPT , IERR   , iwar   )
!
!
!     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED            : May '97  by L. Postma
!
!     MODIFIED           :
!
!     FUNCTION           : Item name retrieval
!
!     SUBROUTINES CALLED : RDTOK1 - reading tokenized input
!
!     LOGICAL UNITS      : LUNUT   = unit formatted output file
!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     IPOSR   INTEGER    1         IN/OUT  Start position on input line
!     NPOS    INTEGER    1         INPUT   nr of significant characters
!     CCHAR   CHAR*1     1         INPUT   comment character
!     CAR     CHARACTER  *         OUTPUT  character workspace
!     IAR     INTEGER  IIMAX       OUTPUT  integer   workspace
!     ICMAX   INTEGER    1         INPUT   max. char workspace dimension
!     IIMAX   INTEGER    1         INPUT   max. int. workspace dimension
!     ANAME   CHAR*20    *         INPUT   ID's of the boundaries/wastes
!     ATYPE   CHAR*20    *         INPUT   Types of the boundaries/wastes
!     NTITM   INTEGER    1         INPUT   number of bounds/wastes
!     NTTYPE  INTEGER    1         INPUT   number of bound/waste types
!     NOITM   INTEGER    1         OUTPUT  number of items read
!     NOITS   INTEGER    1         OUTPUT  number of items for SCALE
!     CHKFLG  INTEGER    1         INPUT   check on input or add items
!     CALLR   CHAR*(6)   1         INPUT   calling subject
!     ILUN    INTEGER   LSTACK     IN/OUT  unitnumb include stack
!     LCH     CHAR*(*)  LSTACK     IN/OUT  file name stack, 4 deep
!     LSTACK  INTEGER    1         INPUT   include file stack size
!     VRSION  REAL       1         INPUT   Input file version number
!     ITYPE   INTEGER    1         OUTPUT  Type of the token at exit
!     RAR     REAL       *         OUTPUT  Array with real values
!     NCONST  REAL       *         OUTPUT  Number of those values
!     CHULP   CHAR*(*)   1         OUTPUT  Input string at end of routine
!     IOUTPT  INTEGER    1         INPUT   Output file option
!     IERR    INTEGER    1         OUTPUT  Error indicator
!     iwar    INTEGER    1         OUTPUT  Cumulative warning count
!
!
      use m_zoek
      use m_movint
      use m_movchr
      use timers       !   performance timers

      INTEGER       ICMAX   , IIMAX    , CHKFLG
      CHARACTER*(*) CAR(*)  , ANAME(*) , ATYPE(*) , LCH(LSTACK) ,
     *              CHULP
      CHARACTER*1   CCHAR*1 , CALLR*10
      DIMENSION     IAR(*)  , ILUN(LSTACK), RAR(*)
      LOGICAL       USEFOR, SETNAM, COMPUT, SIGNON
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq5b", ithndl )

!
!          some initialisations
!
      USEFOR = .FALSE.
      SETNAM = .FALSE.
      COMPUT = .FALSE.
      SIGNON = .FALSE.
      NOITM  = 0
      NOITS  = 0
      ITMNR  = 0
      IOFF   = 0
      IOFFC  = 0
      IOFFI  = 0
      NCONST = 0
!
!          Get a token string (and return if something else was found)
!
   10 ITYPE = -3
      IF ( SIGNON .OR. ( USEFOR .AND. SETNAM ) ) ITYPE = 0
      CALL RDTOK1 ( LUNUT  , ILUN   , LCH    , LSTACK , CCHAR  ,
     *              IPOSR  , NPOS   , CHULP  , IHULP  , RHULP  ,
     *                                         ITYPE  , IERR   )
      IF ( IERR .NE. 0 ) goto 9999
!
!          A keyword was met
!
      IF ( IABS(ITYPE) .EQ. 1 .AND.
     *         (  CHULP(1: 5) .EQ. 'BLOCK'        .OR.
     *            CHULP(1: 6) .EQ. 'LINEAR'       .OR.
     *            CHULP(1: 4) .EQ. 'ITEM'         .OR.
     *            CHULP(1:13) .EQ. 'IDENTICALITEM'.OR.
     *            CHULP(1:12) .EQ. 'USEDATA_ITEM' .OR.
     *            CHULP(1: 7) .EQ. 'FORITEM'      .OR.
     *            CHULP(1: 9) .EQ. 'DATA_ITEM'    .OR.
     *            CHULP(1: 6) .EQ. 'CONCEN'       .OR.
     *            CHULP(1: 6) .EQ. 'DATA'         .OR.
     *            CHULP(1:10) .EQ. 'TIME_DELAY'   .OR.
     *            CHULP(1: 8) .EQ. 'ODS_FILE'     .OR.
     *            CHULP(1:11) .EQ. 'BINARY_FILE'  .OR.
     *            CHULP(1: 8) .EQ. 'ABSOLUTE'     .OR.
     *            CHULP(1: 4) .EQ. 'TIME'         .OR.
     *            CHULP(1: 9) .EQ. 'HARMONICS'    .OR.
     *            CHULP(1: 8) .EQ. 'FOURIERS'     .OR.
     *            CHULP(1: 5) .EQ. 'SCALE'        .OR.
     *            CHULP(1: 8) .EQ. 'DEFAULTS'     .OR.
     *            CHULP(1: 3) .EQ. 'ALL'          .OR.
     *            CHULP(1: 8) .EQ. 'SEGMENTS'     .OR.
     *            CHULP(1: 9) .EQ. 'CONSTANTS'    .OR.
     *            CHULP(1:10) .EQ. 'PARAMETERS'   .OR.
     *            CHULP(1: 9) .EQ. 'FUNCTIONS'    .OR.
     *            CHULP(1:13) .EQ. 'SEG_FUNCTIONS'     )   ) THEN
         IF ( USEFOR ) THEN
            WRITE ( LUNUT , 1035 ) CHULP
            GOTO 40
         ELSE
            goto 9999
         ENDIF
      ENDIF
!
!          Computations
!
      IF ( IABS(ITYPE) .EQ. 1 .AND.
     *     ( CHULP .EQ.  '*'  .OR. CHULP .EQ.  '/'  .OR.
     *       CHULP .EQ.  '+'  .OR. CHULP .EQ.  '-'  .OR.
     *       CHULP .EQ. 'MIN' .OR. CHULP .EQ. 'MAX'      ) ) THEN
         IF ( .NOT. COMPUT ) THEN
            WRITE ( LUNUT , 1070 )
            GOTO 40
         ENDIF
         IF ( SIGNON ) THEN
            WRITE ( LUNUT , 1080 )
            GOTO 40
         ENDIF
         NOITM = NOITM + 1
         NOITS = NOITS + 1
         CALL MOVINT ( IAR   , ITMNR+NOITM , ITMNR+NOITM*2 )
         IAR(ITMNR+NOITM+NOITM) = 0
         IF ( CHULP .EQ.  '*'  ) IAR(ITMNR+NOITM) = -1000000
         IF ( CHULP .EQ.  '/'  ) IAR(ITMNR+NOITM) = -10000000
         IF ( CHULP .EQ.  '+'  ) IAR(ITMNR+NOITM) = -100000000
         IF ( CHULP .EQ.  '-'  ) IAR(ITMNR+NOITM) = -1000000000
         IF ( CHULP .EQ. 'MIN' ) IAR(ITMNR+NOITM) = -1100000000
         IF ( CHULP .EQ. 'MAX' ) IAR(ITMNR+NOITM) = -1200000000
         SIGNON = .TRUE.
         GOTO 10
      ENDIF
!
!          An item used in computations
!
      IF ( IABS(ITYPE) .EQ. 1 .AND. SIGNON ) THEN
         DO 15 I = 1 , ITMNR-1
            IF ( IAR(I) .EQ. -1300000000 ) GOTO 15
            CALL ZOEK ( CHULP, 1,CAR(I+IOFF),20,IFOUND)
            IF ( IFOUND .EQ. 1 ) THEN
               NOITS = NOITS - 1
               I2 = IAR(ITMNR+NOITM)
               IF ( I2 .EQ. -1000000 )    WRITE(LUNUT,1120)I,CHULP
               IF ( I2 .EQ. -10000000 )   WRITE(LUNUT,1110)I,CHULP
               IF ( I2 .EQ. -100000000 )  WRITE(LUNUT,1100)I,CHULP
               IF ( I2 .EQ. -1000000000 ) WRITE(LUNUT,1090)I,CHULP
               IF ( I2 .EQ. -1100000000 ) WRITE(LUNUT,1092)I,CHULP
               IF ( I2 .EQ. -1200000000 ) WRITE(LUNUT,1094)I,CHULP
               IAR(ITMNR+NOITM) = I2 + I
               CAR(ITMNR+NOITM+IOFF) = '&$&$SYSTEM_NAME&$&$!'
               SIGNON = .FALSE.
               GOTO 10
            ENDIF
   15    CONTINUE
         I2 = IAR(ITMNR+NOITM)
         IF ( I2 .EQ. -1000000 )    WRITE(LUNUT,1130)CHULP
         IF ( I2 .EQ. -10000000 )   WRITE(LUNUT,1140)CHULP
         IF ( I2 .EQ. -100000000 )  WRITE(LUNUT,1150)CHULP
         IF ( I2 .EQ. -1000000000 ) WRITE(LUNUT,1160)CHULP
         IF ( I2 .EQ. -1100000000 ) WRITE(LUNUT,1162)CHULP
         IF ( I2 .EQ. -1200000000 ) WRITE(LUNUT,1164)CHULP
         IAR ( ITMNR+NOITM+NOITM) = NOITS
         CAR ( ITMNR+NOITM+IOFF ) = CHULP
         SIGNON = .FALSE.
         GOTO 10
      ENDIF
!
!          A number is used in computations
!
      IF ( IABS(ITYPE) .EQ. 2 .OR. IABS(ITYPE) .EQ. 3 ) THEN
         IF ( SETNAM .OR. SIGNON ) THEN
            NCONST = NCONST + 1
            RAR(NCONST) = RHULP
            NOITS = NOITS - 1
            I2 = IAR(ITMNR+NOITM)
            CAR(ITMNR+NOITM+IOFF) = '&$&$SYSTEM_NAME&$&$!'
            IF ( SIGNON ) THEN
               IF ( I2 .EQ. -1000000 )    WRITE(LUNUT,1170)RHULP
               IF ( I2 .EQ. -10000000 )   WRITE(LUNUT,1180)RHULP
               IF ( I2 .EQ. -100000000 )  WRITE(LUNUT,1190)RHULP
               IF ( I2 .EQ. -1000000000 ) WRITE(LUNUT,1200)RHULP
               IF ( I2 .EQ. -1100000000 ) WRITE(LUNUT,1210)RHULP
               IF ( I2 .EQ. -1200000000 ) WRITE(LUNUT,1220)RHULP
               IAR(ITMNR+NOITM) = I2 - NCONST
               SIGNON = .FALSE.
            ENDIF
            IF ( SETNAM ) THEN
               NAMSET = IAR( ITMNR )
               IF ( NAMSET .GT. 0 .AND. IOUTPT .GE. 3 ) THEN
                  WRITE ( LUNUT , 1001 ) CALLR, ITMNR, CALLR, NAMSET ,
     *                                   ANAME(NAMSET) , RHULP
               ELSEIF ( NAMSET .EQ. 0 .AND. IOUTPT .GE. 3  ) THEN
                  WRITE ( LUNUT , 1001 ) CALLR, ITMNR, CALLR, NAMSET ,
     *                                   'FLOW'        , RHULP
               ELSEIF (NAMSET .EQ. -1300000000 .AND. IOUTPT .GE. 3) THEN
                  WRITE ( LUNUT , 1001 ) CALLR, ITMNR, CALLR, NAMSET ,
     *                                   'Ignored'     , RHULP
               ELSEIF ( IOUTPT .GE. 3 ) THEN
                  WRITE ( LUNUT , 1011 ) CALLR, ITMNR, CALLR,-NAMSET ,
     *                                   ATYPE(-NAMSET) , RHULP
               ENDIF
               IAR(ITMNR+NOITM) =  -NCONST
               IAR(ITMNR+NOITM+NOITM) = 0
               USEFOR = .FALSE.
               SETNAM = .FALSE.
               COMPUT = .TRUE.
            ENDIF
            GOTO 10
         ENDIF
      ENDIF
!
!          A local redirection of the name of an item or substance
!
      IF ( IABS(ITYPE) .EQ. 1 .AND. CHULP .EQ. 'USEFOR') THEN
         IF ( USEFOR ) THEN
            WRITE ( LUNUT , 1035 ) CHULP
            GOTO 40
         ELSE
            USEFOR = .TRUE.
            SETNAM = .FALSE.
            GOTO 10
         ENDIF
      ENDIF
!
!          Getting the items of this block
!                        NOITM  is the order number in the series
!                        NAMSET is the ID number of NOITMth name
!                        ANAME/ATYPE(NAMSET) is the corresponding
!                        reserved name or type
!                        CHULP is the name that should be used.
!                        IARR(ITMNR) stores NAMSET
!
      IF ( ITYPE .EQ. 1 ) THEN
         IF ( USEFOR .AND. SETNAM ) THEN
            NAMSET = IAR( ITMNR )
            IF ( NAMSET .GT. 0 .AND. IOUTPT .GE. 3 ) THEN
               WRITE ( LUNUT , 1000 ) CALLR , ITMNR , CALLR , NAMSET ,
     *                                ANAME(NAMSET) , CHULP
            ELSEIF ( NAMSET .EQ. 0 .AND. IOUTPT .GE. 3  ) THEN
               WRITE ( LUNUT , 1000 ) CALLR , ITMNR , CALLR , NAMSET ,
     *                                'FLOW'        , CHULP
            ELSEIF ( NAMSET .EQ. -1300000000 .AND. IOUTPT .GE. 3  ) THEN
               WRITE ( LUNUT , 1000 ) CALLR , ITMNR , CALLR , NAMSET ,
     *                                'Ignored'     , CHULP
            ELSEIF ( IOUTPT .GE. 3 ) THEN
               WRITE ( LUNUT , 1010 ) CALLR , ITMNR , CALLR ,-NAMSET ,
     *                                ATYPE(-NAMSET) , CHULP
            ENDIF
            IAR ( ITMNR + NOITM + NOITM) = NOITS
            CAR ( ITMNR + NOITM + IOFF ) = CHULP
            USEFOR = .FALSE.
            SETNAM = .FALSE.
!                     it is now possible to compute
            COMPUT = .TRUE.
            GOTO 10
         ENDIF
!
!              fill in a string value if an empty string is provided
!
         IF ( CHKFLG      .EQ. -1 .AND.
     *        CHULP(1:20) .EQ. '                    ' ) THEN
            CHULP = 'Item-'
            WRITE ( CHULP(6:12) , '(I7)' ) NOITM+1
         ENDIF
!
!              FLOW is only valid as CONCENTR. and item number is 0
!
         CALL ZOEK(CHULP,1,(/'FLOW                '/),20,IFOUND)
         IF ( IFOUND .EQ. 1 .AND. CALLR .EQ. 'CONCENTR. ' ) THEN
            NOITM = NOITM + 1
            NOITS = NOITS + 1
            ITMNR = ITMNR + 1
            ICM = ITMNR + NOITM + IOFF
            CALL MOVINT ( IAR   , ITMNR       , ITMNR+NOITM*2 )
            CALL MOVINT ( IAR   , ITMNR+NOITM , ITMNR+NOITM*2 )
            CALL MOVCHR ( CAR   , ITMNR+IOFF  , ICM   )
            IAR ( ITMNR ) =  0
            IAR ( ITMNR + NOITM ) = ITMNR
            IAR ( ITMNR + NOITM + NOITM ) = NOITS
            CAR ( ITMNR + IOFF  ) = CHULP
            CAR ( ITMNR + NOITM + IOFF ) = CHULP
            IF ( USEFOR ) SETNAM = .TRUE.
            IF ( IOUTPT .GE. 3 .AND. .NOT. USEFOR )
     *      WRITE ( LUNUT , 1020 ) CALLR , ITMNR , CALLR , 0 , 'FLOW'
            GOTO 10
         ENDIF
!
!              CHULP equals an item-NAME
!
         CALL ZOEK(CHULP,NTITM,ANAME,20,I2)
         IF ( I2 .GE. 1 ) THEN
            NOITM = NOITM + 1
            NOITS = NOITS + 1
            ITMNR = ITMNR + 1
            ICM = ITMNR + NOITM + IOFF
            CALL MOVINT ( IAR   , ITMNR       , ITMNR+NOITM*2 )
            CALL MOVINT ( IAR   , ITMNR+NOITM , ITMNR+NOITM*2 )
            CALL MOVCHR ( CAR   , ITMNR+IOFF  , ICM   )
            IAR ( ITMNR ) =  I2
            IAR ( ITMNR + NOITM ) = ITMNR
            IAR ( ITMNR + NOITM + NOITM ) = NOITS
            CAR ( ITMNR + IOFF  ) = CHULP
            CAR ( ITMNR + NOITM + IOFF ) = CHULP
            IF ( USEFOR ) SETNAM = .TRUE.
            IF ( IOUTPT .GE. 3 .AND. .NOT. USEFOR )
     *      WRITE ( LUNUT , 1020 ) CALLR, ITMNR, CALLR, I2, ANAME(I2)
            GOTO 10
         ENDIF
!
!              CHULP equals an item-TYPE. IAR now is negative.
!
         CALL ZOEK(CHULP,NTTYPE,ATYPE,20,I2)
         IF ( I2 .GE. 1 ) THEN
            NOITM = NOITM + 1
            NOITS = NOITS + 1
            ITMNR = ITMNR + 1
            ICM = ITMNR + NOITM + IOFF
            CALL MOVINT ( IAR   , ITMNR       , ITMNR+NOITM*2 )
            CALL MOVINT ( IAR   , ITMNR+NOITM , ITMNR+NOITM*2 )
            CALL MOVCHR ( CAR   , ITMNR+IOFF  , ICM   )
            IAR ( ITMNR ) = -I2
            IAR ( ITMNR + NOITM ) = ITMNR
            IAR ( ITMNR + NOITM + NOITM ) = NOITS
            CAR ( ITMNR + IOFF  ) = CHULP
            CAR ( ITMNR + NOITM + IOFF ) = CHULP
            IF ( USEFOR ) SETNAM = .TRUE.
            IF ( IOUTPT .GE. 3 .AND. .NOT. USEFOR )
     *      WRITE ( LUNUT , 1030 ) CALLR, ITMNR, CALLR, I2, ATYPE(I2)
            GOTO 10
         ENDIF
!
!              If only existing names or types are allowed then
!                     this is the place for an error massage
!              JVB stick to just a warning keep on reading IAR = 0?, or used for flow??
!
         IF ( CHKFLG .EQ. 1 ) THEN
            NOITM = NOITM + 1
            NOITS = NOITS + 1
            ITMNR = ITMNR + 1
            ICM = ITMNR + NOITM + IOFF
            CALL MOVINT ( IAR   , ITMNR       , ITMNR+NOITM*2 )
            CALL MOVINT ( IAR   , ITMNR+NOITM , ITMNR+NOITM*2 )
            CALL MOVCHR ( CAR   , ITMNR+IOFF  , ICM   )
            IAR ( ITMNR ) = -1300000000
            IAR ( ITMNR + NOITM ) = 1300000000
            IAR ( ITMNR + NOITM + NOITM ) = NOITS
            CAR ( ITMNR + IOFF  ) = CHULP
            CAR ( ITMNR + NOITM + IOFF ) = CHULP
            IF ( USEFOR ) SETNAM = .TRUE.
            WRITE ( LUNUT , 1040 ) CALLR, ITMNR, CHULP
            iwar = iwar + 1
            GOTO 10
CJVB        WRITE ( LUNUT , 1050 ) CHULP
CJVB        GOTO 40
         ELSE
!
!              Now a new name is added to the list of names
!                     the rest is moved upward since it is all 1 array
!
            NTITM = NTITM + 1
            IOFF  = IOFF  + 1
            ICM   = ICMAX + NTITM
            CALL MOVCHR ( ANAME , NTITM  , ICM  )
            ANAME(NTITM) = CHULP
!              Plus normal procedure
            NOITM = NOITM + 1
            NOITS = NOITS + 1
            ITMNR = ITMNR + 1
            ICM = ITMNR + NOITM + IOFF
            CALL MOVINT ( IAR   , ITMNR       , ITMNR+NOITM*2 )
            CALL MOVINT ( IAR   , ITMNR+NOITM , ITMNR+NOITM*2 )
            CALL MOVCHR ( CAR   , ITMNR+IOFF  , ICM   )
            IAR ( ITMNR ) = NTITM
            IAR ( ITMNR + NOITM ) = ITMNR
            IAR ( ITMNR + NOITM + NOITM ) = NOITS
            CAR ( ITMNR + IOFF  ) = CHULP
            CAR ( ITMNR + NOITM + IOFF ) = CHULP
            IF ( USEFOR ) SETNAM = .TRUE.
            IF ( IOUTPT .GE. 3 .AND. .NOT. USEFOR )
     *                   WRITE ( LUNUT , 1020 ) CALLR, ITMNR, CALLR,
     *                                          NTITM, ANAME(NTITM)
            GOTO 10
         ENDIF
      ENDIF
!
!              No item name was given, but an item number
!
      IF ( ITYPE .EQ. 2 ) THEN
         IF ( IHULP .LE.  NTITM .AND. IHULP .GE. -NTTYPE ) THEN
            NOITM = NOITM + 1
            NOITS = NOITS + 1
            ITMNR = ITMNR + 1
            ICM = ITMNR + NOITM + IOFF
            CALL MOVINT ( IAR   , ITMNR       , ITMNR+NOITM*2 )
            CALL MOVINT ( IAR   , ITMNR+NOITM , ITMNR+NOITM*2 )
            CALL MOVCHR ( CAR   , ITMNR+IOFF  , ICM   )
            IAR ( ITMNR ) = IHULP
            IAR ( ITMNR + NOITM ) = ITMNR
            IAR ( ITMNR + NOITM + NOITM ) = NOITS
            IF ( CALLR .EQ. 'segment' ) THEN
               IF ( IHULP .LE. 0 ) THEN
                  WRITE ( LUNUT , 1060 ) IHULP
                  GOTO 40
               ENDIF
               IF ( IOUTPT .GE. 3 .AND. .NOT. USEFOR )
     *              WRITE ( LUNUT , 1015 ) CALLR, ITMNR, CALLR,  IHULP
               WRITE ( CHULP , '(''Segment '',I8)' ) IHULP
            ELSEIF ( IHULP .EQ. 0 .AND. CALLR .NE. 'CONCENTR. ' ) THEN
               WRITE ( LUNUT , 1060 ) IHULP
               GOTO 40
            ELSEIF ( IHULP .GT. 0 ) THEN
               IF ( IOUTPT .GE. 3 .AND. .NOT. USEFOR )
     *              WRITE ( LUNUT , 1020 ) CALLR, ITMNR, CALLR,  IHULP,
     *                                                   ANAME(  IHULP )
               CHULP = ANAME( IHULP)
            ELSEIF ( IHULP .EQ. 0 .AND. CALLR .EQ. 'CONCENTR. ' ) THEN
               IF ( IOUTPT .GE. 3 .AND. .NOT. USEFOR )
     *         WRITE ( LUNUT , 1020 ) CALLR, ITMNR, CALLR, IHULP,
     *                                                    'FLOW'
               CHULP = 'FLOW'
            ELSE
               IF ( IOUTPT .GE. 3 .AND. .NOT. USEFOR )
     *         WRITE ( LUNUT , 1030 ) CALLR, ITMNR, CALLR, -IHULP,
     *                                                   ATYPE( -IHULP )
               CHULP = ATYPE(-IHULP)
            ENDIF
            CAR ( ITMNR + IOFF  ) = CHULP
            CAR ( ITMNR + NOITM + IOFF ) = CHULP
            IF ( USEFOR ) SETNAM = .TRUE.
            GOTO 10
         ELSE
            WRITE ( LUNUT , 1060 ) IHULP
            GOTO 40
         ENDIF
      ENDIF
!
   40 IERR = 1
 9999 if (timon) call timstop( ithndl )
      RETURN
!
 1000 FORMAT (  ' Input ',A,' nr:',I5,' is ',A,' nr:',I5,' with ID  : ',
     *          A20,' and local substitution: ',A20 )
 1001 FORMAT (  ' Input ',A,' nr:',I5,' is ',A,' nr:',I5,' with ID  : ',
     *          A20,' and local substitution: ',E15.6 )
 1010 FORMAT (  ' Input ',A,' nr:',I5,' is ',A,' type:',I5,
     *          ' with type: ',A20,' and local substitution: ',A20 )
 1011 FORMAT (  ' Input ',A,' nr:',I5,' is ',A,' type:',I5,
     *          ' with type: ',A20,' and local substitution: ',E15.6 )
 1015 FORMAT (  ' Input ',A,' nr:',I5,' is ',A,' nr:',I5 )
 1020 FORMAT (  ' Input ',A,' nr:',I5,' is ',A,' nr:',I5,' with ID  : ',
     *          A20 )
 1030 FORMAT (  ' Input ',A,' nr:',I5,' is ',A,' nr:',I5,' with type: ',
     *          A20 )
 1035 FORMAT (  ' ERROR: no reserved keyword expected: ', A20 )
 1040 FORMAT (  ' WARNING: Input ',A,' nr:',I5,' with name: ',A20,
     *          ' is not a valid ID, data ignored' )
 1050 FORMAT ( /' ERROR: string is no valid item ID: ',A )
 1060 FORMAT (  ' ERROR: number: ',I5,' is not a valid item number !' )
 1070 FORMAT (  ' ERROR: multiplication is only allowed in USEFOR',
     *          ' context !')
 1080 FORMAT (  ' ERROR: arithmetics should be separated by items !')
 1090 FORMAT (  ' Subtracted by item nr: ',I6,' Name: ',A20 )
 1092 FORMAT (  ' Minimum value is item nr: ',I6,' Name: ',A20 )
 1094 FORMAT (  ' Maximum value is item nr: ',I6,' Name: ',A20 )
 1100 FORMAT (  ' Summed with item nr: ',I6,' Name: ',A20 )
 1110 FORMAT (  ' Divided by item nr: ',I6,' Name: ',A20 )
 1120 FORMAT (  ' Multiplied by item nr: ',I6,' Name: ',A20 )
 1130 FORMAT (  ' Multiplied by local substitution: ',A20 )
 1140 FORMAT (  ' Divided by local substitution: ',A20 )
 1150 FORMAT (  ' Summed with local substitution: ',A20 )
 1160 FORMAT (  ' Subtracted by local substitution: ',A20 )
 1162 FORMAT (  ' Minimum value is local substitution: ',A20 )
 1164 FORMAT (  ' Maximum value is local substitution: ',A20 )
 1169 FORMAT (  ' Substituted by: ',E15.6 )
 1170 FORMAT (  ' Multiplied by: ',E15.6 )
 1180 FORMAT (  ' Divided by: ',E15.6 )
 1190 FORMAT (  ' Summed with: ',E15.6 )
 1200 FORMAT (  ' Subtracted by: ',E15.6 )
 1210 FORMAT (  ' Minimum value is: ',E15.6 )
 1220 FORMAT (  ' Maximum value is: ',E15.6 )
!
      END
