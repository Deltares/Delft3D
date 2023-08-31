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
      module m_dlwqnb
      use m_zercum
      use m_setset
      use m_proint
      use m_proces
      use m_hsurf
      use m_dlwqtr
      use m_dlwqt0
      use m_dlwqo2


      implicit none

      contains


      subroutine dlwqnb ( a     , j     , c     , lun   , lchar  ,
     &                    action, dlwqd , gridps)

!       Deltares Software Centre

!>\file
!>                         1st order upwind, fully implicit direct method (10)
!>
!>                         Performs time dependent integration. Upwind 1st order
!>                         in space. Fully implicit with a direct method in time.\n
!>                         Matrices become very large in 3D and method unworkable. In 2D
!>                         the method can be used. In 1D the method outperforms the
!>                         iterative methods.

!     CREATED            : april 1992 by J.v.Gils
!
!     LOGICAL UNITS      : LUN(19) , output, monitoring file
!                          LUN(20) , output, formatted dump file
!                          LUN(21) , output, unformatted hist. file
!                          LUN(22) , output, unformatted dump file
!                          LUN(23) , output, unformatted dump file
!
!     SUBROUTINES CALLED : DLWQTR, user transport routine
!                          PROCES, DELWAQ proces system
!                          DLWQO2, DELWAQ output system
!                          DLWQ13, system postpro-dump routine
!                          DLWQ14, scales waterquality
!                          DLWQ15, wasteload routine
!                          DLWQ17, boundary routine
!                          DLWQ41, updates volumes
!                          DLWQT0, updates other time dependent items
!                          DLWQ62, adds transport to matrix and rhs
!                          DELMAT, inverts the matrix
!                          DLWQB1, initializes matrix and rhs
!                          DLWQB2, checks matrix
!                          DLWQB3, computes volumes
!                          DLWQB4, computation of mass array
!                          DLWQB5, performs mass balance computation
!                          DLWQB6, updates right hand side
!                          DLWQB7, adds open boundaries to deriv
!                          DLWQB8, restores conc array
!                          MOVE,   copies one array to another
!                          PROINT, integration of fluxes
!                          open_waq_files, opens files
!                          ZERCUM, zero's the cummulative array's
!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH   FUNC.  DESCRIPTION
!     ---------------------------------------------------------
!     A       REAL       *      LOCAL  real      workspace array
!     J       INTEGER    *      LOCAL  integer   workspace array
!     C       CHARACTER  *      LOCAL  character workspace array
!     LUN     INTEGER    *      INPUT  array with unit numbers
!     LCHAR   CHAR*(*)   *      INPUT  filenames
!
!     Declaration of arguments
!
      use m_dlwqce
      use m_dlwqb9
      use m_dlwqb8
      use m_dlwqb7
      use m_dlwqb6
      use m_dlwqb5
      use m_dlwqb4
      use m_dlwqb3
      use m_dlwqb2
      use m_dlwqb1
      use m_dlwq41
      use m_dlwq17
      use m_dlwq15
      use m_dlwq14
      use m_dlwq13
      use m_delpar01
      use m_delmat
      use m_move
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
      use m_dlwqdata_save_restore

      implicit none

!
!     Declaration of arguments
!
      REAL, DIMENSION(*)             :: A
      INTEGER, DIMENSION(*)          :: J
      INTEGER, DIMENSION(*)          :: LUN
      CHARACTER*(*), DIMENSION(*)    :: C
      CHARACTER*(*), DIMENSION(*)    :: LCHAR
      INTEGER                        :: ACTION
      TYPE(DELWAQ_DATA), TARGET      :: DLWQD
      type(GridPointerColl)          :: GridPs               ! collection of all grid definitions


!
!     Local declarations
!
      LOGICAL         IMFLAG , IDFLAG , IHFLAG
      LOGICAL         UPDATE , LREWIN
      INTEGER         NSTEP
      INTEGER         I
      INTEGER         IBND
      INTEGER         ISYS
      INTEGER         NSYS
      INTEGER         IDDEF
      INTEGER         IVDEF
      REAL            RDUMMY(1)
      INTEGER         LAATST
      INTEGER         sindex




!
!     SPECIAL REMARKS    : MASS-ARRAY IS USED FOR RHS VECTOR!!
!
!     This option is a mix of option 1 (discretization of transport
!     in space) and option 6 (matrix inversion to perform implicit
!     integration in time.
!     The processes part is integrated EXPLICITLY, in order to allow
!     for any complexity of the processes.
!     Strictly speaking, a loop over substances should be added
!     (see below, DLWQB1). To anticipate this, the method uses an
!     extra VOLUME-array (IVOL2), and uses the AMASS-array (IMASS)
!     for the rhs-matrix, in stead of the DERIV-array as in method 6.
!     (JvG, May 8 1992)

      if ( action == ACTION_FINALISATION ) then
          call dlwqdata_restore(dlwqd)
          if ( timon ) call timstrt ( "dlwqnb", ithandl )
          goto 50
      endif

      IF ( ACTION == ACTION_INITIALISATION  .OR.
     &     ACTION == ACTION_FULLCOMPUTATION        ) THEN

!
!          some initialisation
!
          ithandl = 0
          ITIME   = ITSTRT
          NSTEP   = (ITSTOP-ITSTRT)/IDT
          IFFLAG  = 0
          IAFLAG  = 0
          IBFLAG  = 0
          IF ( MOD(INTOPT,16) .GE. 8 ) IBFLAG = 1
          IF ( NDSPN .EQ. 0 ) THEN
             NDDIM = NODISP
          ELSE
             NDDIM = NDSPN
          ENDIF
          IF ( NVELN .EQ. 0 ) THEN
             NVDIM = NOVELO
          ELSE
             NVDIM = NVELN
          ENDIF
          LSTREC = ICFLAG .EQ. 1
          nosss  = noseg + nseg2
          NOQTT  = NOQ + NOQ4
          inwtyp = intyp + nobnd

          UPDATR = .TRUE.

!
!          initialize second volume array with the first one
!
          CALL MOVE   ( A(IVOL ), A(IVOL2) , NOSEG   )
      ENDIF

!
!     Save/restore the local persistent variables,
!     if the computation is split up in steps
!
!     Note: the handle to the timer (ithandl) needs to be
!     properly initialised and restored
!
      IF ( ACTION == ACTION_INITIALISATION ) THEN
          if ( timon ) call timstrt ( "dlwqnb", ithandl )
          call dlwqdata_save(dlwqd)
          if ( timon ) call timstop ( ithandl )
          RETURN
      ENDIF

      IF ( ACTION == ACTION_SINGLESTEP ) THEN
          call dlwqdata_restore(dlwqd)
      ENDIF

      if ( timon ) call timstrt ( "dlwqnb", ithandl )

!======================= simulation loop ============================

   10 continue

!        Determine the volumes and areas that ran dry at start of time step

         call hsurf  ( noseg    , nopa     , c(ipnam) , a(iparm) , nosfun   ,
     &                 c(isfna) , a(isfun) , surface  , lun(19)  )
         call dryfld ( noseg    , nosss    , nolay    , a(ivol)  , noq1+noq2,
     &                 a(iarea) , nocons   , c(icnam) , a(icons) , surface  ,
     &                 j(iknmr) , iknmkv   )

!        user transport processes

         update = updatr
         call dlwqtr ( notot    , nosys    , nosss    , noq      , noq1     ,
     &                 noq2     , noq3     , nopa     , nosfun   , nodisp   ,
     &                 novelo   , j(ixpnt) , a(ivol)  , a(iarea) , a(iflow) ,
     &                 a(ileng) , a(iconc) , a(idisp) , a(icons) , a(iparm) ,
     &                 a(ifunc) , a(isfun) , a(idiff) , a(ivelo) , itime    ,
     &                 idt      , c(isnam) , nocons   , nofun    , c(icnam) ,
     &                 c(ipnam) , c(ifnam) , c(isfna) , update   , ilflag   )
         if ( update ) updatr = .true.

!jvb     Temporary ? set the variables grid-setting for the DELWAQ variables

         call setset ( lun(19)  , nocons   , nopa     , nofun    , nosfun   ,
     &                 nosys    , notot    , nodisp   , novelo   , nodef    ,
     &                 noloc    , ndspx    , nvelx    , nlocx    , nflux    ,
     &                 nopred   , novar    , nogrid   , j(ivset) )

!        return conc and take-over from previous step or initial condition,
!        and do particle tracking of this step (will be back-coupled next call)

         call delpar01( itime   , noseg    , nolay    , noq      , nosys    ,
     &                  notot   , a(ivol)  , surface  , a(iflow) , c(isnam) ,
     &                  nosfun  , c(isfna) , a(isfun) , a(imass) , a(iconc) ,
     &                  iaflag  , intopt   , ndmps    , j(isdmp) , a(idmps) ,
     &                  a(imas2))

!          call PROCES subsystem

         call proces ( notot    , nosss    , a(iconc) , a(ivol)  , itime    ,
     &                 idt      , a(iderv) , ndmpar   , nproc    , nflux    ,
     &                 j(iipms) , j(insva) , j(iimod) , j(iiflu) , j(iipss) ,
     &                 a(iflux) , a(iflxd) , a(istoc) , ibflag   , ipbloo   ,
     &                 ioffbl   ,  a(imass) , nosys    ,
     &                 itfact   , a(imas2) , iaflag   , intopt   , a(iflxi) ,
     &                 j(ixpnt) , iknmkv   , noq1     , noq2     , noq3     ,
     &                 noq4     , ndspn    , j(idpnw) , a(idnew) , nodisp   ,
     &                 j(idpnt) , a(idiff) , ndspx    , a(idspx) , a(idsto) ,
     &                 nveln    , j(ivpnw) , a(ivnew) , novelo   , j(ivpnt) ,
     &                 a(ivelo) , nvelx    , a(ivelx) , a(ivsto) , a(idmps) ,
     &                 j(isdmp) , j(ipdmp) , ntdmpq   , a(idefa) , j(ipndt) ,
     &                 j(ipgrd) , j(ipvar) , j(iptyp) , j(ivarr) , j(ividx) ,
     &                 j(ivtda) , j(ivdag) , j(ivtag) , j(ivagg) , j(iapoi) ,
     &                 j(iaknd) , j(iadm1) , j(iadm2) , j(ivset) , j(ignos) ,
     &                 j(igseg) , novar    , a        , nogrid   , ndmps    ,
     &                 c(iprna) , intsrt   ,
     &                 j(iprvpt), j(iprdon), nrref    , j(ipror) , nodef    ,
     &                 surface  , lun(19)  )

!          set new boundaries

         if ( itime .ge. 0   ) then
          ! first: adjust boundaries by OpenDA
            if ( dlwqd%inopenda ) then
               do ibnd = 1,nobnd
                  do isys = 1,nosys
                     call get_openda_buffer(isys,ibnd, 1,1,
     &                               A(ibset+(ibnd-1)*nosys + isys-1))
                  enddo
               enddo
            endif

            CALL DLWQ17 ( A(IBSET), A(IBSAV), J(IBPNT), NOBND   , NOSYS   ,
     *                    NOTOT   , IDT     , A(ICONC), A(IFLOW), A(IBOUN))
         endif
!
!     Call OUTPUT system
!
      CALL DLWQO2 ( NOTOT   , NOSEG   , NOPA    , NOSFUN  , ITIME   ,
     +              C(IMNAM), C(ISNAM), C(IDNAM), J(IDUMP), NODUMP  ,
     +              A(ICONC), A(ICONS), A(IPARM), A(IFUNC), A(ISFUN),
     +              A(IVOL) , NOCONS  , NOFUN   , IDT     , NOUTP   ,
     +              LCHAR   , LUN     , J(IIOUT), J(IIOPO), A(IRIOB),
     +              C(IOSNM), C(IOUNI), C(IODSC), C(ISSNM), C(ISUNI), C(ISDSC),
     +              C(IONAM), NX      , NY      , J(IGRID), C(IEDIT),
     +              NOSYS   , A(IBOUN), J(ILP)  , A(IMASS), A(IMAS2),
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

!        zero cummulative array's

         if ( imflag .or. ( ihflag .and. noraai .gt. 0 ) ) then
            call zercum ( notot   , nosys   , nflux   , ndmpar  , ndmpq   ,
     &                    ndmps   , a(ismas), a(iflxi), a(imas2), a(iflxd),
     &                    a(idmpq), a(idmps), noraai  , imflag  , ihflag  ,
     &                    a(itrra), ibflag  , nowst   , a(iwdmp))
         endif

!          simulation done ?

         if ( itime .lt. 0      ) goto 9999
         if ( itime .ge. itstop ) goto 50

!          restore conc-array from mass array

         call dlwqb8 ( nosys    , notot    , nototp   , noseg    , a(ivol ) ,
     &                 surface  , a(imass) , a(iconc) )

!        add processes

         call dlwq14 ( a(iderv) , notot    , nosss    , itfact   , a(imas2) ,
     &                 idt      , iaflag   , a(idmps) , intopt   , j(isdmp) )
!
!          get new volumes
!
      ITIMEL = ITIME
      ITIME  = ITIME + IDT
      IF ( IVFLAG .EQ. 1 ) THEN
!
!          computation of volumes for computed volumes only
!
         CALL MOVE   ( A(IVOL) , A(IVOL2), NOSEG   )
         CALL DLWQB3 ( A(IAREA), A(IFLOW), A(IVNEW), J(IXPNT), NOTOT   ,
     *                 NOQ     , NVDIM   , J(IVPNW), A(IVOL2), INTOPT  ,
     *                 A(IMAS2), IDT     , IAFLAG  , NOSYS   , A(IDMPQ),
     *                 NDMPQ   , J(IQDMP))
         UPDATR = .TRUE.
      ELSE
!
!          read new volumes from files
!
         CALL DLWQ41 ( LUN     , ITIME   , ITIMEL  , A(IHARM), A(IFARR),
     *                 J(INRHA), J(INRH2), J(INRFT), NOSEG   , A(IVOL2),
     *                 J(IBULK), LCHAR   , ftype   , ISFLAG  , IVFLAG  ,
     *                 UPDATE  , J(INISP), A(INRSP), J(INTYP), J(IWORK),
     *                 LSTREC  , LREWIN  , A(IVOLL), dlwqd   )
         IF ( UPDATE ) UPDATR = .TRUE.
      ENDIF

!        update the info on dry volumes with the new volumes

         call dryfle ( noseg    , nosss    , a(ivol2) , nolay    , nocons   ,
     &                 c(icnam) , a(icons) , surface  , j(iknmr) , iknmkv   )

!          add the waste loads

         call dlwq15 ( nosys     , notot    , noseg    , noq      , nowst    ,
     &                 nowtyp    , ndmps    , intopt   , idt      , itime    ,
     &                 iaflag    , c(isnam) , a(iconc) , a(ivol)  , a(ivol2) ,
     &                 a(iflow ) , j(ixpnt) , c(iwsid) , c(iwnam) , c(iwtyp) ,
     &                 j(inwtyp) , j(iwast) , iwstkind , a(iwste) , a(iderv) ,
     &                 iknmkv    , nopa     , c(ipnam) , a(iparm) , nosfun   ,
     &                 c(isfna ) , a(isfun) , j(isdmp) , a(idmps) , a(imas2) ,
     &                 a(iwdmp)  , 1        , notot     )

!          Here we implement a loop that inverts the same matrix
!          for series of subsequent substances having the same
!          additional VELO and DISPER array. (JvG, April 24, 1993).

!          Start of loop

         isys = 0
         nsys = 1
   20    continue

!          Compute range of substances for present cycle

         isys = isys + nsys
         iddef = j(idpnw+isys-1)
         ivdef = j(ivpnw+isys-1)
         do i = isys+1,nosys
            if (j(idpnw+i-1).ne.iddef.or.j(ivpnw+i-1).ne.ivdef) goto 40
         enddo
         i = nosys + 1
   40    nsys = i - isys
         if ( .not.( isys .eq. 1 .and. nsys .eq. nosys ) ) updatr = .true.

      IF ( UPDATR ) THEN
         UPDATR = .FALSE.

!          zero the matrix, initialize diagonal and compute RHS

         call dlwqb1 ( notot   , noseg   , a(ivol) , a(ivol2), a(iconc),
     &                 a(iderv), isys    , nsys    , jtrack  , a(itimr),
     &                 rhs     , idt     )
!
!          do the transport itself (Attn.: NOTOT replaced by NOSYS)
!
         CALL DLWQB9 ( A(IDISP), A(IDNEW), A(IAREA), A(IFLOW), A(ILENG),
     *                 A(IVNEW), A(IBOUN), J(IXPNT), NOSYS   , ISYS    ,
     *                 NSYS    , NOQ1    , NOQ2    , NOQ     , NDDIM   ,
     *                 NVDIM   , J(IDPNW), J(IVPNW), rhs     , A(ITIMR),
     *                 JTRACK  , INTOPT  , ILFLAG  )
!
!          invert the matrix
!
         CALL DELMAT ( NOSEG   , JTRACK  , JTRACK  , NSYS    , A(ITIMR),
     *                                               rhs     ,    0    )
      ELSE
!          compute RHS
         CALL DLWQB6 ( A(ICONC), A(IDERV), NOSEG   , NOTOT   , A(IVOL) ,
     *                           IDT     , 1       , rhs     , NOSYS   )
!
!          do the transport itself, only accross boundaries
!
         CALL DLWQB7 ( A(IDISP), A(IDNEW), A(IAREA), A(IFLOW), A(ILENG),
     *                 A(IVNEW), A(IBOUN), J(IXPNT), NOSYS   , 1       ,
     *                 NOSYS   , NOQ1    , NOQ2    , NOQ     , NDDIM   ,
     *                 NVDIM   , J(IDPNW), J(IVPNW), rhs     , INTOPT  ,
     *                                                         ILFLAG  )
!
!          calculate the concentration with known matrix
!
         CALL DELMAT ( NOSEG   , JTRACK  , JTRACK  , NOSYS   , A(ITIMR),
     *                                               rhs     ,    2    )
      ENDIF
!
!          store results from RHS in concentration matrix
!
      CALL DLWQB2 ( A(ICONC), rhs     , NOSEG   , NOTOT   , ISYS    ,
     *              NSYS    )
!
!          Back for new cycle if last substance does not equal NOSYS
!
      IF ( (ISYS+NSYS-1) .NE. NOSYS ) GOTO 20
!
!          mass balance of transport
!
      CALL DLWQB5 ( A(IDISP), A(IDNEW), A(IAREA), A(IFLOW), A(ILENG),
     *              A(IVNEW), A(ICONC), A(IBOUN), J(IXPNT), NOSYS   ,
     *              NOTOT   , NOQ1    , NOQ2    , NOQ     , NDDIM   ,
     *              NVDIM   , J(IDPNW), J(IVPNW), INTOPT  , A(IMAS2),
     *              ILFLAG  , A(IDMPQ), NDMPQ   , IDT     , J(IQDMP))

!          update mass array, explicit step for passive substances

      call dlwqb4 ( nosys   , notot   , nototp  , noseg   , a(ivol2),
     &              surface , a(imass), a(iconc), a(iderv), idt     )
!
!          replace old by new volumes
!
      CALL MOVE   ( A(IVOL2), A(IVOL) , NOSEG   )
!
!          calculate closure error
!
      IF ( LREWIN .AND. LSTREC ) THEN
         CALL DLWQCE ( A(IMASS), A(IVOLL), A(IVOL2), NOSYS , NOTOT ,
     +                 NOSEG   , LUN(19) )
         CALL MOVE   ( A(IVOLL), A(IVOL) , NOSEG   )
      ENDIF
!
!          integrate the fluxes at dump segments fill ASMASS with mass
!
      IF ( IBFLAG .GT. 0 ) THEN
         CALL PROINT ( NFLUX   , NDMPAR  , IDT     , ITFACT  , A(IFLXD),
     +                 A(IFLXI), J(ISDMP), J(IPDMP), NTDMPQ  )
      ENDIF

      call dlwqt0 ( lun      , itime    , itimel   , a(iharm) , a(ifarr) ,
     &              j(inrha) , j(inrh2) , j(inrft) , idt      , a(ivol)  ,
     &              a(idiff) , a(iarea) , a(iflow) , a(ivelo) , a(ileng) ,
     &              a(iwste) , a(ibset) , a(icons) , a(iparm) , a(ifunc) ,
     &              a(isfun) , j(ibulk) , lchar    , c(ilunt) , ftype    ,
     &              intsrt   , isflag   , ifflag   , ivflag   , ilflag   ,
     &              update   , j(iktim) , j(iknmr) , j(inisp) , a(inrsp) ,
     &              j(intyp) , j(iwork) , .false.  , ldummy   , rdummy   ,
     &              .false.   , gridps   , dlwqd    )
      if ( update ) updatr = .true.

!          end of time loop

      IF ( ACTION == ACTION_FULLCOMPUTATION ) THEN
          GOTO 10
      ENDIF

   50 CONTINUE

      IF ( ACTION == ACTION_FINALISATION    .OR.
     &     ACTION == ACTION_FULLCOMPUTATION      ) THEN
!
!
!          close files, except monitor file
!
          call CloseHydroFiles( dlwqd%collcoll )
          call close_files( lun )
!
!          write restart file
!
          CALL DLWQ13 ( LUN      , LCHAR , A(ICONC) , ITIME , C(IMNAM) ,
     *                  C(ISNAM) , NOTOT , NOSEG    )
      ENDIF

 9999 if ( timon ) call timstop ( ithandl )

      dlwqd%iaflag = iaflag
      dlwqd%itime = itime

      RETURN
      END SUBROUTINE

      end module m_dlwqnb
