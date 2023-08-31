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
      module m_dlwqn9
      use m_hsurf
      use m_dlwqtr
      use m_dlwqo2


      implicit none

      contains


      subroutine dlwqn9 ( a     , j     , c     , lun   , lchar  ,
     &                    action, dlwqd , gridps)

!       Deltares Software Centre

!>\file
!>                         Performs iterative steady state computation
!>                         central in space.

!     CREATED            : june 1988 by L. Postma
!
!
!     LOGICAL UNITS      : LUN(19) , output, monitoring file
!                          LUN(20) , output, formatted dump file
!                          LUN(21) , output, unformatted hist. file
!                          LUN(22) , output, unformatted dump file
!                          LUN(23) , output, restart file
!
!     SUBROUTINES CALLED : DLWQTR, user transport routine
!                          DLWQ13, system postpro-dump routine
!                          DLWQ80, convert water quality processes
!                          DLWQ81, add waste loads
!                          DLWQ83, set iteration step and check
!                          DLWQ90, do the transport
!                          open_waq_files, opens files
!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH   FUNC.  DESCRIPTION
!     ---------------------------------------------------------
!     A       REAL       *      LOCAL  real      workspace array
!     J       INTEGER    *      LOCAL  integer   workspace array
!     C       CHARACTER  *      LOCAL  character workspace array
!     LUN     INTEGER    *      INPUT  array with unit numbers
!     LCHAR   CHARACTER  *      INPUT  filenames
!
!     Declaration of arguments
!
      use m_dlwq90
      use m_dlwq83
      use m_dlwq81
      use m_dlwq80
      use m_dlwq65
      use m_dlwq41
      use m_dlwq13
      use m_zero
      use m_fileutils
      use grids
      use timers
      use delwaq2_data
      use m_openda_exchange_items, only : get_openda_buffer
      use waqmem          ! module with the more recently added arrays
      use m_actions
      use m_sysn          ! System characteristics
      use m_sysi          ! Timer characteristics
      use m_sysa          ! Pointers in real array workspace
      use m_sysj          ! Pointers in integer array workspace
      use m_sysc          ! Pointers in character array workspace

      implicit none

!
!     Declaration of arguments
!
      REAL, DIMENSION(*)          :: A
      INTEGER, DIMENSION(*)       :: J
      INTEGER, DIMENSION(*)       :: LUN
      CHARACTER*(*), DIMENSION(*) :: C
      CHARACTER*(*), DIMENSION(*) :: LCHAR
      INTEGER                     :: ACTION
      TYPE(DELWAQ_DATA)           :: DLWQD
      type(GridPointerColl)       :: GridPs               ! collection off all grid definitions


!
!     Local declarations
!
      LOGICAL          IMFLAG , IDFLAG , IHFLAG , CONVER
      LOGICAL          OPFLAG , LDUMMY , LSTREC , LREWIN

      INTEGER          ITIME
      INTEGER          ITIMEL
      INTEGER          IAFLAG
      INTEGER          IBFLAG
      INTEGER          ISYS
      INTEGER          ICSYS
      INTEGER          NSYS
      INTEGER          INWTYP
      INTEGER          ISTEP
      INTEGER          I
      INTEGER          :: NOSSS
      INTEGER          :: NOQTT

      REAL             ASTOP
      INTEGER         sindex

      integer       :: ithandl


      !
      ! Distinguishing the actions is superfluous:
      ! there is only one step
      !
      IF ( ACTION == ACTION_INITIALISATION .OR.
     &     ACTION == ACTION_FINALISATION        ) THEN
          RETURN
      ENDIF

!
!          some initialisation
!
      ithandl = 0
      if ( timon ) call timstrt ( "dlwqn9", ithandl )

      ITIMEL = ITSTRT
      ITIME  = ITSTRT + IDT
      IHFLAG = .FALSE.
      IAFLAG = 0
      IBFLAG = 0
      IF ( MOD(INTOPT,16) .GE. 8 ) IBFLAG = 1
      CALL ZERO ( A(IMAS2) , NOTOT*5 )
      LDUMMY = .FALSE.
      LSTREC = .FALSE.
      nosss  = noseg + nseg2
      NOQTT  = NOQ + NOQ4
      inwtyp = intyp + nobnd

!        Determine the volumes and areas that ran dry,
!        They cannot have explicit processes during this time step

         call hsurf  ( noseg    , nopa     , c(ipnam) , a(iparm) , nosfun   ,
     &                 c(isfna) , a(isfun) , surface  , lun(19)  )
         call dryfld ( noseg    , nosss    , nolay    , a(ivol)  , noq1+noq2,
     &                 a(iarea) , nocons   , c(icnam) , a(icons) , sindex   ,
     &                 surface  , j(iknmr) , iknmkv   )
!
!          make closure error correction
!
      IF ( IDT.EQ.0 ) THEN
         CALL ZERO ( A(IVOL2), NOSEG )
      ELSE IF ( J(INRH2+1).GE.0 .AND. IVFLAG.EQ.0 ) THEN
         CALL DLWQ41 ( LUN     , ITIME   , ITIMEL  , A(IHARM), A(IFARR),
     *                 J(INRHA), J(INRH2), J(INRFT), NOSEG   , A(IVOL2),
     *                 J(IBULK), LCHAR   , ftype   , ISFLAG  , IVFLAG  ,
     *                 LDUMMY  , J(INISP), A(INRSP), J(INTYP), J(IWORK),
     *                 LSTREC  , LREWIN  , A(IVOLL), dlwqd   )
         CALL DLWQ65 ( A(IVOL2), A(IVOL) , IDT     , NOSEG   )
      ELSE
         CALL ZERO   ( A(IVOL2) , NOSEG )
         WRITE ( LUN(19), 1000 )
      ENDIF
!
      ASTOP  = 1.0
      DO 10 I = 1,IMSTOP
      ASTOP  = ASTOP/10.0
   10 CONTINUE
      CALL ZERO ( A(ITIMR) , NOTOT*NOSEG )
!
!======================= iteration loop =============================
!
!          do the user transport processes
!
      DO  20 ISTEP = 1,ITSTOP
      CALL DLWQTR ( NOTOT   , NOSYS   , NOSEG   , NOQ     , NOQ1    ,
     *              NOQ2    , NOQ3    , NOPA    , NOSFUN  , NODISP  ,
     *              NOVELO  , J(IXPNT), A(IVOL) , A(IAREA), A(IFLOW),
     *              A(ILENG), A(ICONC), A(IDISP), A(ICONS), A(IPARM),
     *              A(IFUNC), A(ISFUN), A(IDIFF), A(IVELO), ITSTRT  ,
     *              IDT     , C(ISNAM), NOCONS  , NOFUN   , C(ICNAM),
     *              C(IPNAM), C(IFNAM), C(ISFNA), LDUMMY  , ILFLAG  )
!
!          do the user water quality processes
!
      CALL DLWQ80 ( A(IDERV), NOTOT   , NOSEG   , ITFACT  , A(ITIMR),
     *              A(IMASS), A(IMAS2), IAFLAG  , A(IDMPS), INTOPT  ,
     *              J(ISDMP))
!
!          add the waste loads
!
      CALL DLWQ81 ( A(IWSTE), J(IWAST), NOWST   , NOTOT   , A(ICONC),
     *              A(IDERV), A(ITIMR), A(IVOL) , A(IMAS2), IAFLAG  ,
     *              A(IDMPS), NDMPS   , INTOPT  , J(ISDMP), NOSYS   )
!
!          do the transport itself
!
      CALL DLWQ90 ( A(IDISP), A(IDIFF), A(IAREA), A(IFLOW), A(ILENG),
     *              A(IVELO), A(ICONC), A(IBOUN), J(IXPNT), NOSYS   ,
     *              NOTOT   , NOQ1    , NOQ2    , NOQ     , NODISP  ,
     *              NOVELO  , J(IDPNT), J(IVPNT), A(IDERV), A(ITIMR),
     *              A(IVOL) , INTOPT  , A(IMAS2), IAFLAG  , ILFLAG  ,
     *              A(IDMPQ), NDMPQ   , J(IQDMP))
!
!          set an iteration step and check convergence
!
      CALL DLWQ83 ( A(ICONC), A(IMASS), A(IDERV), A(IVOL) , A(IVOL2),
     *              A(ITIMR), NOSYS   , NOTOT   , NOSEG   , ISTEP   ,
     *                                  ASTOP   , CONVER  , LUN(19) )
!
      IF ( CONVER .AND. IHFLAG ) GOTO 30
      IF ( CONVER .OR. ISTEP .EQ. ITSTOP-1 ) THEN
           IAFLAG = 1
           IHFLAG = .TRUE.
      ENDIF
   20 CONTINUE
!
!     Call OUTPUT system
!
   30 CONTINUE
      CALL DLWQO2 ( NOTOT   , NOSEG   , NOPA    , NOSFUN  , ITSTRT  ,
     +              C(IMNAM), C(ISNAM), C(IDNAM), J(IDUMP), NODUMP  ,
     +              A(ICONC), A(ICONS), A(IPARM), A(IFUNC), A(ISFUN),
     +              A(IVOL) , NOCONS  , NOFUN   , 1       , NOUTP   ,
     +              LCHAR   , LUN     , J(IIOUT), J(IIOPO), A(IRIOB),
     +              C(IOSNM), C(IOUNI), C(IODSC), C(ISSNM), C(ISUNI), C(ISDSC),
     +              C(IONAM), NX      , NY      , J(IGRID), C(IEDIT),
     +              NOSYS   , A(IBOUN), J(ILP)  , A(IDERV), A(IMAS2),
     +              A(ISMAS), NFLUX   , A(IFLXI), ISFLAG  , IAFLAG  ,
     +              IBFLAG  , IMSTRT  , IMSTOP  , IMSTEP  , IDSTRT  ,
     +              IDSTOP  , IDSTEP  , IHSTRT  , IHSTOP  , IHSTEP  ,
     +              IMFLAG  , IDFLAG  , IHFLAG  , NOLOC   , A(IPLOC),
     +              NODEF   , A(IDEFA), ITSTRT  , ITSTOP  , NDMPAR  ,
     +              C(IDANA), NDMPQ   , NDMPS   , J(IQDMP), J(ISDMP),
     +              J(IPDMP), A(IDMPQ), A(IDMPS), A(IFLXD), NTDMPQ  ,
     +              C(ICBUF), NORAAI  , NTRAAQ  , J(IORAA), J(NQRAA),
     +              J(IQRAA), A(ITRRA), C(IRNAM), A(ISTOC), NOGRID  ,
     +              NOVAR   , J(IVARR), J(IVIDX), J(IVTDA), J(IVDAG),
     +              J(IAKND), J(IAPOI), J(IADM1), J(IADM2), J(IVSET),
     +              J(IGNOS), J(IGSEG), A       , NOBND   , NOBTYP  ,
     +              C(IBTYP), J(INTYP), C(ICNAM), NOQ     , J(IXPNT),
     +              INTOPT  , C(IPNAM), C(IFNAM), C(ISFNA), J(IDMPB),
     +              NOWST   , NOWTYP  , C(IWTYP), J(IWAST), J(INWTYP),
     +              A(IWDMP), iknmkv  , isegcol )
!
!          close files, except monitor file
!
      DO 40 I=1 , 22
         INQUIRE ( LUN(I) , OPENED = OPFLAG )
         IF ( OPFLAG .AND. I .NE. 19 ) THEN
            CLOSE ( LUN(I) )
         ENDIF
   40 CONTINUE
!
!          close files, except monitor file
!
      call CloseHydroFiles( dlwqd%collcoll )
      call close_files( lun )
!
!          write restart file
!
      CALL DLWQ13 ( LUN      , LCHAR , A(ICONC) , ITSTRT, C(IMNAM) ,
     *              C(ISNAM) , NOTOT , NOSEG    )
!
!          output formats
!
 1000 FORMAT ( 'No closure error corrections !' )
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      end module m_dlwqn9
