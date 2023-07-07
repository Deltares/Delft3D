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

      SUBROUTINE DLWQ5C ( FNAME  , LUNUT  , CAR    , IAR    , RAR    ,
     *                    ICMAX  , IIMAX  , IRMAX  , DRAR   , NOITM  ,
     *                    NODIM  , IORDER , SCALE  , ITMNR  , IDMNR  ,
     *                             AMISS  , NOBRK  , IERR   , iwar   )
!
!
!     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED            : May '97  by L. Postma
!
!     MODIFIED           :
!
!     FUNCTION           : Boundary and waste data new style
!                          Data retrieval from an ODS file
!
!     SUBROUTINES CALLED : CONVER - converting times of breakpoints
!
!     LOGICAL UNITS      : LUNUT   = report file
!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     FNAME   CHAR*(*)   1         INPUT   filename of the ODS file
!     CAR     CHARACTER  *         LOCAL   character workspace
!     IAR     INTEGER  IIMAX       LOCAL   integer   workspace
!     RAR     REAL     IRMAX       LOCAL   real      workspace
!     ICMAX   INTEGER    1         INPUT   max. char workspace dimension
!     IIMAX   INTEGER    1         INPUT   max. int. workspace dimension
!     IRMAX   INTEGER    1         INPUT   max. real workspace dimension
!     DRAR    REAL*8     1         IN/OUT  Double precision workspace
!     NOITM   INTEGER    1         INPUT   number of bounds/wastes
!     NODIM   INTEGER    1         INPUT   number of concentrations
!     IORDER  INTEGER    1         INPUT   Order of the input
!     SCALE   LOGICAL    1         INPUT   True if scale values are stored
!     IOFFC   INTEGER    1         INPUT   Offset of the concentrations    IOFFI   INTEGER    1         INPUT   Offset in the integer array
!     IOFFI   INTEGER    1         INPUT   Offset of the items             IOFFI   INTEGER    1         INPUT   Offset in the integer array
!     AMISS   REAL       1         INPUT   Missing value indicator
!     NOBRK   INTEGER    1         OUTPUT  Number of time steps found
!     IERR    INTEGER    1         OUTPUT  error flag
!     Iwar    INTEGER    1         in/out  cumulative warning count
!
!     IN THE COMMON BLOCK:
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     block sysi.inc
!     ITSTRT  INTEGER    1         INPUT   Simulation start time ( scu )
!     ITSTOP  INTEGER    1         INPUT   Simulation stop time ( scu )
!     ISFACT  INTEGER    1         INPUT   system clock in seconds
!     OTIME   REAL*8     1         INPUT   Julian offset of the real time
!
!     The map of the Character array is:
!     First NOITM + NODIM entries the names of ITEMS and SUBSTANCES
!         IF IORDER = 1 then ITEMS first IF 2 then CONCENTRATIONS first
!     The rest of the array is free working space for this routine
!         Next NOITM + NODIM entries reserved for names of values to be
!                                                             retrieved
!         Further locations is workspace
!
!     The map of the Integer array is:
!     First NOITM + NODIM entries the names of ITEMS and SUBSTANCES
!         IF IORDER = 1 then ITEMS first IF 2 then CONCENTRATIONS first
!     Then NOBRK time breakpoints to be read in eg in this routine
!     The rest of the array is free working space
!         Initially next NOITM + NODIM entries reserved for numbers of
!              locations or substances as stored in the character array.
!              In the corresponding location below NOITM + NODIM a
!                                  reference is made to this location.
!         Initially next NOITM + NODIM entries reserved for numbers of
!              locations or substances as stored in the ODS file for
!                                  retrieval
!         Initially next NOBRK values are for retrieved time values
!
!     The map of the Integer array is:
!     IF (SCALE) First NODIM entries the scale factors
!     Then the matrix of values to be read in eg in this routine
!
      use m_zoek
      use timers       !   performance timers
      use m_sysi          ! Timer characteristics


      INTEGER       ICMAX  , IIMAX  , IRMAX
      CHARACTER*(*) CAR(*) , FNAME
      DIMENSION     IAR(*) , RAR(*)
      LOGICAL       SCALE
      REAL*8        DRAR(*)
      CHARACTER     CFILE(3)*256
      real amiss
!
!     Local declarations
!
      DIMENSION     LOC(3)
      REAL*8        AFACT    , A1    , A2    , D_BEG    , D_END , DUMMY
      CHARACTER*3   CDUMMY
!
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwq5c", ithndl )
!
!     Array offsets
!
      NOTTT = ITMNR + NOITM + IDMNR + NODIM
      IF ( IORDER .EQ. 1 ) THEN
         IOFFA = ITMNR
         IOFFB = ITMNR + NOITM + IDMNR
         IOFFC = 0
         IOFFD = ITMNR + NOITM
         NSCLE = NODIM
      ENDIF
      IF ( IORDER .EQ. 2 ) THEN
         IOFFA = IDMNR + NODIM + ITMNR
         IOFFB = IDMNR
         IOFFC = IDMNR + NODIM
         IOFFD = 0
         NSCLE = NOITM
      ENDIF
!
!        write the ODS file name
!
      WRITE ( LUNUT , 1000 ) FNAME
!
!        get the dimensions of the ODS file
!
      CFILE(1) = FNAME
      CFILE(3) = ' '
      K1   = NOTTT + 1
      CALL GETDIM ( CFILE  , 0      , CDUMMY , 0      , 0       ,
     *                       0      , IAR(K1), IERROR , CFILE(3))
      NSUBS = IAR(K1)
      NLOCS = IAR(K1+1)
      NTIMS = IAR(K1+2)
!
!             Deal with locations ( J for characters, K for integers )
!
      J1 = NOTTT + 1
      J2 = J1    + 1
      K1 = NOTTT + NOITM + 1
      J3 = J2 + NLOCS
      K2 = K1 + NLOCS
!
!             See if storage is available
!
      K3 = MIN ( (IIMAX-K2) , (ICMAX-J3) )
      IF ( K3 .LT. NLOCS ) THEN
         WRITE ( LUNUT , 1010 ) K3, NLOCS
         IERR = 1
         GOTO 510
      ENDIF
!
!             Get the available locations
!
      CAR(J1)  = '*'
      CALL GETLOC ( CFILE  , 0  , CAR(J1), 1      , 0       ,
     *              0      , K3 , CAR(J2), IAR(K1), IAR(K2) ,
     *                            NOLOC  , IERROR , CFILE(3))
!
!             Fill an array with wanted locations
!
      NOIT2 = 0
      NOITV = 0
      DO 20 J = 1 , NOITM
         IF ( CAR(IOFFA+J) .EQ. '&$&$SYSTEM_NAME&$&$!' ) THEN
            NOIT2 = NOIT2 + 1
            NOITV = NOITV + 1
            CAR(IOFFA+NOIT2) = CAR(IOFFA+J)
            CAR(IOFFC+NOIT2) = CAR(IOFFC+J)
            IAR(IOFFA+NOIT2) = IAR(IOFFA+J)
            IAR(IOFFC+NOIT2) = IAR(IOFFC+J)
            IAR(NOTTT+NOIT2) = -1
            IF ( SCALE .AND. IORDER .EQ. 2 ) RAR(NOIT2) = RAR(J)
            GOTO 20
         ENDIF
         CALL ZOEK(CAR(IOFFA+J),NOLOC,CAR(J1+1),20,I)
         IF ( I .GE. 1 ) THEN
            NOIT2 = NOIT2 + 1
            CAR(IOFFA+NOIT2) = CAR(IOFFA+J)
            CAR(IOFFC+NOIT2) = CAR(IOFFC+J)
            IAR(IOFFA+NOIT2) = IAR(IOFFA+J)
            IAR(IOFFC+NOIT2) = IAR(IOFFC+J)
            IAR(NOTTT+NOIT2) = I
            GOTO 20
         ENDIF
         WRITE ( LUNUT , 1070 ) IAR(IOFFA+J), CAR(IOFFA+J)
         iwar = iwar + 1
         IF (   IAR(IOFFA+J  ) .LT. 0 .OR.
     *        ( IAR(IOFFA+J+1) .LT. 0 .AND. J .NE. NOITM ) ) THEN
            WRITE ( LUNUT , 1080 )
            IERR = 2
            GOTO 510
         ENDIF
   20 CONTINUE
!
!  Compact the pointers for unresolved externals
!
      ISHFT = NOITM - NOIT2
      IF ( IORDER .EQ. 1 ) THEN
         LTOT = IDMNR + NODIM
      ELSE
         LTOT = 0
      ENDIF
      DO 30 I = IOFFA+NOIT2+1,IOFFA+NOIT2+LTOT+NOIT2
         CAR(I) = CAR(I+ISHFT)
         IAR(I) = IAR(I+ISHFT)
   30 CONTINUE
      DO 40 I = IOFFC+NOIT2+1,IOFFC+NOIT2+LTOT+NOIT2*2
         CAR(I) = CAR(I+ISHFT)
         IAR(I) = IAR(I+ISHFT)
   40 CONTINUE
      NOTTT = NOTTT - ISHFT*2
      ITMNR = ITMNR - ISHFT
      NOITM = NOITM - ISHFT
      IF ( IORDER .EQ. 1 ) THEN
         IOFFB = ITMNR + NOITM + IDMNR
         IOFFD = ITMNR + NOITM
         NSHFT = 0
      ELSE
         NSHFT = ITMNR + NOITM
      ENDIF
!
!             Deal with substances
!
      J1 = NOTTT + 1
      J2 = J1    + 1
      K1 = NOTTT + NOITM + 1
      J3 = J2 + NSUBS
      K2 = K1 + NSUBS
!
!             See if storage is available
!
      K3 = MIN ( (IIMAX-K2) , (ICMAX-J3) )
      IF ( K3 .LT. NSUBS ) THEN
         WRITE ( LUNUT , 1010 ) K3, NSUBS
         IERR = 1
         GOTO 510
      ENDIF
!
!             Get the available substances
!
      CALL GETPAR ( CFILE  , 0      , CAR(J1), 1      , 0       ,
     *              0      , K3     , 0      , CAR(J2), CAR(J3) ,
     *              IAR(K1), IAR(K2), NOPAR  , IERROR , CFILE(3))
!
!             Fill an array with wanted substances
!
      ICNT = 0
      K5   = NOTTT + NOITM
      NITM = NODIM
      DO 60 J = 1 , NITM
         K = J - ICNT
         IAR(K5+K) = 0
         IF ( CAR(IOFFB+J) .EQ. '&$&$SYSTEM_NAME&$&$!' ) GOTO 60
         CALL ZOEK(CAR(IOFFB+K),NOPAR,CAR(J1+1),20,I)
         IF ( I .GE. 1 ) THEN
            IAR(K5+K) = I
            GOTO 60
         ENDIF
         CALL DLWQ5H ( LUNUT  , IAR    , ITMNR  , NOITM  , IDMNR  ,
     *                 NODIM  , IORDER , CAR    , K5     , IOFFB  ,
     *                          NSHFT  , IOFFD  , K      , ICNT   )
         iwar = iwar + 1
         IF ( J + ICNT .GE. NITM ) GOTO 70
   60 CONTINUE
   70 K1 = K1 + NODIM
!
!             Get the time values
!
      K3 = IIMAX-K1
!  first NODIM real*4 can be scale values
      K2 = 1
      IF ( SCALE ) K2 = NSCLE/2 + 2
      K5 = K2 + 3
      K4 =  ( IRMAX - K5*2 ) / 2
!  see if there is space enough
      K4 = MIN ( K3 , K4 )
!
!             See if storage is available
!
      IF ( K4 .LT. NTIMS ) THEN
         WRITE ( LUNUT , 1010 ) K4, NTIMS
         IERR = 1
         GOTO 510
      ENDIF
      AFACT = ISFACT/864.0D+02
      IF ( ISFACT .LT. 0 ) AFACT = -1.0D+00/ISFACT/864.0D+02
!
!             Get the available time values
!
      DRAR(K2  ) = 0
      CALL GETTME ( CFILE  , 0      , DRAR(K2), 1      , 0       ,
     *              0      , K4     , DRAR(K5), IAR(K1), NOBRK   ,
     *                                          IERROR , CFILE(3))
!
!  see if the found time values are within the range
!
      IF ( NOBRK .GE. 1 ) THEN
         WRITE ( LUNUT , 1020 )
         A1 = DELTIM + ITSTRT*AFACT
         A2 = DELTIM + ITSTOP*AFACT
         I1 = 1
         I2 = 1
         DO 80 I = 1 , NOBRK
            IF ( DRAR(K5+I-1) .LE. A1 ) I1 = I
            IF ( DRAR(K5+I-1) .LT. A2 ) I2 = I
   80    CONTINUE
         IF ( I2 .NE. NOBRK ) I2 = I2 + 1
         K6 = K5+NOBRK-1
         IF ( DRAR(K6) .LT. A1 ) I2 = 1
!  errors and warnings
         IF ( DRAR(K5) .GT. A1 ) THEN
            CALL GREGOR ( DRAR(K5), IY1, IM1, ID1, IH1, IN1, IS1, DUMMY)
            CALL GREGOR ( A1      , IY2, IM2, ID2, IH2, IN2, IS2, DUMMY)
            WRITE ( LUNUT , 1030 )  IY1, IM1, ID1, IH1, IN1, IS1,
     *                              IY2, IM2, ID2, IH2, IN2, IS2
            iwar = iwar + 1
         ENDIF
         IF ( DRAR(K6) .LT. A2 ) THEN
            CALL GREGOR ( DRAR(K6), IY1, IM1, ID1, IH1, IN1, IS1, DUMMY)
            CALL GREGOR ( A2      , IY2, IM2, ID2, IH2, IN2, IS2, DUMMY)
            WRITE ( LUNUT , 1040 )  IY1, IM1, ID1, IH1, IN1, IS1,
     *                              IY2, IM2, ID2, IH2, IN2, IS2
            iwar = iwar + 1
         ENDIF
         NOBRK = I2-I1+1
      ENDIF
      WRITE ( LUNUT , 1050 ) NOBRK
      IF ( NOBRK .EQ. 1 )    WRITE ( LUNUT , 1060 )
!      times are converted to DELWAQ times
      DO 90 I = I1,I2
         A2 = DRAR(K5+I-1) - DELTIM
         IAR(K1+I-I1) = A2/AFACT + 0.5
   90 CONTINUE
!      See if enough space is available
!           nr substances  nr locations for retrieval
      NT1 = NODIM*NOITM
!           nr substances  nr locations for storage
      NT2 = NODIM+NOITM
!           real retrieval space + 1
      IS  =  NT1*NOBRK + 1
      IF ( SCALE ) IS = IS + NSCLE
!           convert for double precission,
!           NOBRK is max number of retrievals per invocation
      IS2 = (IS + NOBRK+1)/2+1
!           then the offset increases
      MAXD   = IRMAX - IS
      IF ( MAXD .LT. NOBRK ) THEN
         WRITE ( LUNUT , 1010 ) IS + NOBRK, IRMAX
         IERR = 1
         GOTO 510
      ENDIF
!     set the time margins for retrieval
!
!     JVB, the endtime can be overwritten here
!
!      DRAR(IS2  ) = DRAR(K5+I1-1) - AFACT/2.0
!      DRAR(IS2+1) = DRAR(K5+I2-1) + AFACT/2.0
      D_BEG = DRAR(K5+I1-1) - AFACT/2.0
      D_END = DRAR(K5+I2-1) + AFACT/2.0
      DRAR(IS2  ) = D_BEG
      DRAR(IS2+1) = D_END
CJVB
!
!             Get the data themselves
!
      LOC(3) =  1
      IGS = 1
      IG  = 1
      IF ( SCALE ) IG = IG + NSCLE
      DO 120 I = 1 , NODIM
! this should correspond with the found substance numbers
         KP = IAR(NOTTT+NOITM+I)
         IF ( KP .LT. 0 ) GOTO 120
         IF ( IORDER .EQ. 1 ) THEN
            IG  = IGS
            IGS = IGS + 1
            IF ( SCALE ) IG = IG + NSCLE
         ENDIF
         DO 110 J = 1 , NOITM
! this should correspond with the found location numbers
            KL = IAR(NOTTT+J)
            IF ( KL .LE. 0 ) GOTO 95
            LOC(1) = KL
            LOC(2) = KL
            CALL GETMAT ( CFILE , 0 , KP   , LOC     , DRAR(IS2),
     *                    AMISS , 0 , MAXD , RAR(IS) , IERROR   ,
     *                                                 CFILE(3) )
   95       IG2 = IG
! this loop is per location, so skip the amount of substances if IORDER is 1
            IF ( IORDER .EQ. 1 ) THEN
               IG = IG + NODIM
            ELSE
               IG = IG + 1
            ENDIF
            DO 100 K = 0 , NOBRK-1
               RAR(IG2) = RAR(IS+K)
! skip a full matrix further, because this is this substance for all
!                                                        breakpoints
               IG2 = IG2 + NT1
  100       CONTINUE
  110    CONTINUE
  120 CONTINUE
      DO 130 I = 1,NSCLE
         IAR(NOTTT+I) = I
  130 CONTINUE
      DO 140 I = 1,NOBRK
         IAR(NOTTT+NSCLE+I) = IAR(K1+I-1)
  140 CONTINUE
!
  510 CONTINUE
      if (timon) call timstop( ithndl )
      RETURN
!
!      formats
!
 1000 FORMAT (  ' DATA will be retrieved from ODS-file: ',A )
 1010 FORMAT (  ' ERROR: Insufficient memory ! Available:',I10,
     *                                            ', needed:',I10,' !' )
 1020 FORMAT (  ' This block consists of a time function.' )
 1030 FORMAT (  ' WARNING: file start time   : ',
     *                      I4,'.',I2,'.',I2,' ',I2,':',I2,':',I2,/
     *          ' after simulation start time: ',
     *                      I4,'.',I2,'.',I2,' ',I2,':',I2,':',I2,' !' )
 1040 FORMAT (  ' WARNING: file stop  time   : ',
     *                      I4,'.',I2,'.',I2,' ',I2,':',I2,':',I2,/
     *          ' before simulation stop time: ',
     *                      I4,'.',I2,'.',I2,' ',I2,':',I2,':',I2,' !' )
 1050 FORMAT (  ' Number of valid time steps found: ',I6 )
 1060 FORMAT (  ' This block consists of constant data.' )
 1070 FORMAT (  ' WARNING: location : ',I8,' not found. Name is: ',A )
 1080 FORMAT (  ' ERROR  : location is used in a computation',
     *          ' that will become corrupted !' )
!
      END
