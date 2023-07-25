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
      module m_dlwqnj
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


      subroutine dlwqnj ( a     , j     , c     , lun   , lchar  ,
     &                    action, dlwqd , gridps)

!       Deltares Software Centre

!>\file
!>                         ADI solver of Delft3D-FLOW method (19 & 20)
!>
!>                         Performs time dependent integration according to
!>                         the Alternate Direction Implicit method of
!>                         Delft3D-FLOW (difu.f90).
!>                         Is implemented as:
!>                            - method 19 upwind discretisation of the vertical
!>                            - method 20 central discretisation of the vertical
!>                            .
!>                         Method 20 allows for the use of a Forester filter to
!>                         warantee monotoneous behaviour.

!     CREATED            : december 1995 by E. de Goede
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
!                          DLWQ41, update volumes
!                          DLWQT0, update other time functions
!                          PROINT, integration of fluxes
!                          open_waq_files, opens files
!                          SRSTOP, stops execution
!
! ROUTINES MBT TRISULA-TRANSPORTSCHEMA:
!                          DLBACK, back conversion to DELWAQ arrays
!                          DLCONV, conversion to TRISULA arrays
!                          DLDIFU, performs time step
!                          DLFLUX, computes fluxes for mass balance
!                          DLINIT, initializes TRISULA arrays
!                          DLMASB, updates mass balance
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
      use m_dlwqf8
      use m_dlwqd2
      use m_dlwqce
      use m_dlwqb3
      use m_dlwq41
      use m_dlwq17
      use m_dlwq15
      use m_dlwq14
      use m_dlwq13
      use m_dlmasb
      use m_dlinit
      use m_dlflux
      use m_dldifu
      use m_dlconv
      use m_dlback
      use m_delpar01
      use m_move
      use m_fileutils
      use grids
      use timers
      use waqmem                         ! Global memory with allocatable GMRES arrays
      use delwaq2_data
      use m_openda_exchange_items, only : get_openda_buffer
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
      REAL, DIMENSION(*)          :: A
      INTEGER, DIMENSION(*)       :: J
      INTEGER, DIMENSION(*)       :: LUN
      CHARACTER*(*), DIMENSION(*) :: C
      CHARACTER*(*), DIMENSION(*) :: LCHAR
      INTEGER                     :: ACTION
      TYPE(DELWAQ_DATA), TARGET   :: DLWQD
      type(GridPointerColl)       :: GridPs               ! collection of all grid definitions


!
!     Local declarations
!
      REAL             RDUMMY(1)
      LOGICAL          IMFLAG , IDFLAG , IHFLAG
      LOGICAL          LREWIN , LDUMM2
      INTEGER          NSTEP
      INTEGER          ICREEP
      INTEGER          ICENTR
      INTEGER          IZ
      INTEGER          IDUMMY(1)
      REAL             RDT

      INTEGER          IBND
      INTEGER           ISYS

      real             dsdksi(1,1), dsdeta(1,1), dtdksi(1,1), dtdeta(1,1)
      real             rbnd(1)
      real             ws(1)
      real             adummy(1)
      real             kmxsed(1)
      logical          eqmbc
      character*4      sedtyp(2)
      INTEGER          sindex

      !
      ! Variables local to this method
      !
      integer       :: i
      logical, save :: ifirst = .true.
      integer, save :: jstart
      integer, save :: nmmaxj
      real, save    :: vicmol
      real, save    :: eps




! ====================================================================
! SOME REMARKS:
!
! IN TRISULA QXK=FLUX IN X-DIRECTION, QYK=FLUX IN Y-DIRECTION
! IN DELWAQ FIRST DIRECTION=Y-DIRECTION; SECOND DIRECTION=X-DIRECTION
!
! IN TRISULA Z-DIRECTION IS POSITIVE UPWARDS; IN DELWAQ POSITIVE DOWNWARDS
! ====================================================================
!
      if ( action == ACTION_FINALISATION ) then
          call dlwqdata_restore(dlwqd)
          if ( timon ) call timstrt ( "dlwqnj", ithandl )
          goto 20
      endif

      IF ( ACTION == ACTION_INITIALISATION  .OR.
     &     ACTION == ACTION_FULLCOMPUTATION        ) THEN

!          some initialisation

         ithandl = 0
         ITIME   = ITSTRT
         NSTEP   = (ITSTOP-ITSTRT)/IDT
         IFFLAG  = 0
         IAFLAG  = 0
         IBFLAG  = 0
         IF ( MOD(INTOPT,16) .GE. 8 ) IBFLAG = 1
         LDUMMY = .FALSE.
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
         LSTREC   = ICFLAG .EQ. 1
         nosss    = noseg + nseg2
         noqtt    = noq + noq4
         inwtyp   = intyp + nobnd
         LLENG    = ILENG+NOQ1+NOQ2
         FORESTER = BTEST(INTOPT,6)
         NOWARN   = 0

!          initialize second volume array with the first one

!        This statement caused a stack overflow with a very large model
!        Use an explicit loop instead
!         a( ivol2 : ivol2+noseg ) = a( ivol : ivol+noseg )
         do i = 0,noseg-1
             a(ivol2+i) = a(ivol+i)
         enddo

!          initialize constant arrays for D3D-FLOW solver

         call dlinit ( lun(19)  , noseg    , noq1     , noq2     , noq      ,
     &                 a(ivol)  , nopa     , c(ipnam) , a(iparm) , ilflag   ,
     &                 a(ileng) , flowpnt  , nmax     , mmax     , kmax     ,
     &                 j(ilgra) , gsqs     , a(iguv)  , a(igvu)  , cell_x   ,
     &                 cell_y   , guu      , gvv      , j(ikcs)  , thick    ,
     &                 sig      )
         hu = 0.0
         hv = 0.0
      ENDIF


!
!     Save/restore the local persistent variables,
!     if the computation is split up in steps
!
!     Note: the handle to the timer (ithandl) needs to be
!     properly initialised and restored
!
      IF ( ACTION == ACTION_INITIALISATION ) THEN
          if ( timon ) call timstrt ( "dlwqnj", ithandl )
          call dlwqdata_save(dlwqd)
          if ( timon ) call timstop ( ithandl )
          RETURN
      ENDIF

      IF ( ACTION == ACTION_SINGLESTEP ) THEN
          call dlwqdata_restore(dlwqd)
      ENDIF

      ICREEP   = 0
      IF ( BTEST(INTOPT,7) ) ICREEP = 1
      ICENTR   = 0
      IF ( INTSRT .NE. 19 ) ICENTR = 1

      if ( timon ) call timstrt ( "dlwqnj", ithandl )

!======================= simulation loop ============================

   10 continue

!        Determine the volumes and areas that ran dry at start of time step

         call hsurf  ( noseg    , nopa     , c(ipnam) , a(iparm) , nosfun   ,
     &                 c(isfna) , a(isfun) , surface  , lun(19)  )
         call dryfld ( noseg    , nosss    , nolay    , a(ivol)  , noq1+noq2,
     &                 a(iarea) , nocons   , c(icnam) , a(icons) , surface  ,
     &                 j(iknmr) , iknmkv   )

!          user transport processes

         call dlwqtr ( notot    , nosys    , nosss    , noq      , noq1     ,
     &                 noq2     , noq3     , nopa     , nosfun   , nodisp   ,
     &                 novelo   , j(ixpnt) , a(ivol)  , a(iarea) , a(iflow) ,
     &                 a(ileng) , a(iconc) , a(idisp) , a(icons) , a(iparm) ,
     &                 a(ifunc) , a(isfun) , a(idiff) , a(ivelo) , itime    ,
     &                 idt      , c(isnam) , nocons   , nofun    , c(icnam) ,
     &                 c(ipnam) , c(ifnam) , c(isfna) , ldummy   , ilflag   )

!jvb  Temporary ? set the variables grid-setting for the DELWAQ variables

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
!           first: adjust boundaries by OpenDA
            if ( dlwqd%inopenda ) then
               do ibnd = 1,nobnd
                  do isys = 1,nosys
                     call get_openda_buffer(isys,ibnd, 1,1,
     &                               A(ibset+(ibnd-1)*nosys + isys-1))
                  enddo
               enddo
            endif
            call dlwq17 ( a(ibset), a(ibsav), j(ibpnt), nobnd   , nosys   ,
     &                    notot   , idt     , a(iconc), a(iflow), a(iboun))
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

!          zero cummulative array's

         if ( imflag .or. ( ihflag .and. noraai .gt. 0 ) ) then
            call zercum ( notot   , nosys   , nflux   , ndmpar  , ndmpq   ,
     &                    ndmps   , a(ismas), a(iflxi), a(imas2), a(iflxd),
     &                    a(idmpq), a(idmps), noraai  , imflag  , ihflag  ,
     &                    a(itrra), ibflag  , nowst   , a(iwdmp))
         endif

!          simulation done ?

         if ( itime .lt. 0      ) goto 9999
         if ( itime .ge. itstop ) goto 20

      write (lun(19),*) '  '
      write (lun(19),*) '==========================================='
      iz = (itime - itstrt) / idt + 1
      write (lun(19),*) ' time step no. :',iz
!
!     INITIALIZATION OF TRISULA ARRAYS
!
      IF ( IFIRST ) THEN
         jstart = 1 - 2 * NMAX
         nmmaxj = ( 2 + MMAX ) * NMAX
         kadu   = 1
         kadv   = 1
         kcu    = 0
         r11    = 0.0
         sour   = 0.0
         sink   = 0.0
         s0     = 0.0
         s1     = 0.0
         sigdif = 1.0
         sigmol = 1.0
         vicmol = 0.0
         dicuv  = 0.0
         dicww  = 0.0
         IFIRST = .FALSE.
         eqmbc  = .false.
         eps    = 1.0e-20
      ENDIF

!        add processes

         call dlwq14 ( a(iderv), notot   , nosss   , itfact  , a(imas2),
     &                 idt     , iaflag  , a(idmps), intopt  , j(isdmp))

!        get new volumes

         itimel = itime
         itime  = itime + idt
         select case ( ivflag )
            case ( 1 )                 !     computation of volumes for computed volumes only
               call move   ( a(ivol) , a(ivol2), noseg   )
               call dlwqb3 ( a(iarea), a(iflow), a(ivnew), j(ixpnt), notot   ,
     &                       noq     , nvdim   , j(ivpnw), a(ivol2), intopt  ,
     &                       a(imas2), idt     , iaflag  , nosys   , a(idmpq),
     &                       ndmpq   , j(iqdmp))
               updatr = .true.
            case ( 2 )                 !     the fraudulent computation option
               call dlwq41 ( lun     , itime   , itimel  , a(iharm), a(ifarr),
     &                       j(inrha), j(inrh2), j(inrft), noseg   , a(ivoll),
     &                       j(ibulk), lchar   , ftype   , isflag  , ivflag  ,
     &                       updatr  , j(inisp), a(inrsp), j(intyp), j(iwork),
     &                       lstrec  , lrewin  , a(ivol2), dlwqd   )
               call dlwqf8 ( noseg   , noq     , j(ixpnt), idt     , iknmkv  ,
     &                       a(ivol ), a(iflow), a(ivoll), a(ivol2))
               updatr = .true.
               lrewin = .true.
               lstrec = .true.
            case default               !     read new volumes from files
               call dlwq41 ( lun     , itime   , itimel  , a(iharm), a(ifarr),
     &                       j(inrha), j(inrh2), j(inrft), noseg   , a(ivol2),
     &                       j(ibulk), lchar   , ftype   , isflag  , ivflag  ,
     &                       updatr  , j(inisp), a(inrsp), j(intyp), j(iwork),
     &                       lstrec  , lrewin  , a(ivoll), dlwqd   )
         end select

!        update the info on dry volumes with the new volumes

         call dryfle ( noseg    , nosss    , a(ivol2) , nolay    , nocons   ,
     &                 c(icnam) , a(icons) , surface  , j(iknmr) , iknmkv   )

!        add the waste loads

         call dlwq15 ( nosys    , notot    , noseg    , noq      , nowst    ,
     &                 nowtyp   , ndmps    , intopt   , idt      , itime    ,
     &                 iaflag   , c(isnam) , a(iconc) , a(ivol)  , a(ivol2) ,
     &                 a(iflow ), j(ixpnt) , c(iwsid) , c(iwnam) , c(iwtyp) ,
     &                 j(inwtyp), j(iwast) , iwstkind , a(iwste) , a(iderv) ,
     &                 iknmkv   , nopa     , c(ipnam) , a(iparm) , nosfun   ,
     &                 c(isfna ), a(isfun) , j(isdmp) , a(idmps) , a(imas2) ,
     &                 a(iwdmp) , 1        , notot    )

!        conversion of Delwaq arrays to FLOW arrays

         call dlconv ( noseg    , noq1     , noq2     , noq3     , noq      ,
     &                 nobnd    , nosys    , notot    , intsrt   , intopt   ,
     &                 ilflag   , nvdim    , nddim    , j(ivpnw) , j(idpnw) ,
     &                 a(ivol)  , a(ivol2) , a(iarea) , a(iflow) , a(ileng) ,
     &                 a(ivnew) , a(idisp) , a(idnew) , a(iconc) , a(iboun) ,
     &                 a(iderv) , cellpnt  , flowpnt  , nmax     , mmax     ,
     &                 kmax     , j(ilgra) , gsqs     , a(iguv)  , a(igvu)  ,
     &                 thick    , a(ivola) , a(ivolb) , dps      , s1       ,
     &                 a(iddkl) , a(ir1)   , a(iqxk)  , a(iqyk)  , a(iqzk)  ,
     &                 a(idifx) , a(idify) , a(idifz) , dicuv    , dicww    ,
     &                 areau    , areav    , a(iaakl) , a(ibbkl) , a(icckl) ,
     &                 j(ikcs)  , j(ikfu)  , j(ikfv)  , j(ikfs)  )

      rdt = idt * 1.0
      if ( mod((itime-itstrt)/idt,2) .eq. 1 ) then

         write ( lun(19), * ) 'implicit in x-direction'
         call dldifu ( icreep   , rdt      , lun(19)  , 0        , nmax     ,
     &                 1        , jstart   , nmmaxj   , nmax*mmax, kmax     ,
     &                 nosys    , nosys    , 0        , 0        , 0        ,
     &                 0        , 0        , 0        , 0        , idummy   ,
     &                 j(ikcs)  , kcu      , j(ikfs)  , j(ikfu)  , j(ikfv)  ,
     &                 kadu     , kadv     , s0       , s1       , hu       ,
     &                 hv       , dps      , a(iqxk)  , a(iqyk)  , a(iqzk)  ,
     &                 guu      , gvv      , a(iguv)  , a(igvu)  , gsqs     ,
     &                 rbnd     , sigdif   , sigmol   , a(ir1)   , r11      ,
     &                 sour     , sink     , ws       , sedtyp   , thick    ,
     &                 sig      , dicuv    , dicww    , dsdksi   , dsdeta   ,
     &                 dtdksi   , dtdeta   , a(iaak)  , a(ibbk)  , a(icck)  ,
     &                 a(ibd3x) , a(ibddx) , a(ibdx)  , a(ibux)  , a(ibuux) ,
     &                 a(ibu3x) , a(iwrk1) , a(iwrk2) , areau    , areav    ,
     &                 a(iaakl) , a(ibbkl) , a(icckl) , a(iddkl) , kmxsed   ,
     &                 eqmbc    , eqmbc    , adummy   , a(ivola) , a(ivolb) ,
     &                 rscale   , adummy   , eps      , vicmol   , a(idifx) ,
     &                 a(idify) , icentr   , dfluxx   , dfluxy   )

!        Computes fluxes for mass balance

         call dlflux ( jstart   , nmmaxj   , nmax*mmax, kmax     , nosys    ,
     &                 notot    , nmax     , 1        , intsrt   , icreep   ,
     &                 j(ikfu)  , j(ikfv)  , j(ikfs)  , j(ikcs)  , kadu     ,
     &                 kadv     , a(iqxk)  , a(iqyk)  , a(iqzk)  , a(idifx) ,
     &                 a(idify) , a(idifz) , a(ir1)   , r11      , a(iguv)  ,
     &                 a(igvu)  , dicww    , areau    , areav    , gsqs     ,
     &                 s0       , dps      , thick    , sigdif   , sigmol   ,
     &                 vicmol   , dfluxx   , dfluxy   , a(iaakl) , a(ibbkl) ,
     &                 a(icckl) )

      else

         write ( lun(19), * ) 'implicit in y-direction'
         call dldifu ( icreep   , rdt      , lun(19)  , 0        , 1        ,
     &                 nmax     , jstart   , nmmaxj   , nmax*mmax, kmax     ,
     &                 nosys    , nosys    , 0        , 0        , 0        ,
     &                 0        , 0        , 0        , 0        , idummy   ,
     &                 j(ikcs)  , kcu      , j(ikfs)  , j(ikfv)  , j(ikfu)  ,
     &                 kadv     , kadu     , s0       , s1       , hv       ,
     &                 hu       , dps      , a(iqyk)  , a(iqxk)  , a(iqzk)  ,
     &                 gvv      , guu      , a(igvu)  , a(iguv)  , gsqs     ,
     &                 rbnd     , sigdif   , sigmol   , a(ir1)   , r11      ,
     &                 sour     , sink     , ws       , sedtyp   , thick    ,
     &                 sig      , dicuv    , dicww    , dsdksi   , dsdeta   ,
     &                 dtdksi   , dtdeta   , a(iaak)  , a(ibbk)  , a(icck)  ,
     &                 a(ibd3x) , a(ibddx) , a(ibdx)  , a(ibux)  , a(ibuux) ,
     &                 a(ibu3x) , a(iwrk1) , a(iwrk2) , areav    , areau    ,
     &                 a(iaakl) , a(ibbkl) , a(icckl) , a(iddkl) , kmxsed   ,
     &                 eqmbc    , eqmbc    , adummy   , a(ivola) , a(ivolb) ,
     &                 rscale   , adummy   , eps      , vicmol   , a(idify) ,
     &                 a(idifx) , icentr   , dfluxx   , dfluxy              )

!        Computes fluxes for mass balance

         call dlflux ( jstart   , nmmaxj   , nmax*mmax, kmax     , nosys    ,
     &                 notot    , 1        , nmax     , intsrt   , icreep   ,
     &                 j(ikfv)  , j(ikfu)  , j(ikfs)  , j(ikcs)  , kadv     ,
     &                 kadu     , a(iqyk)  , a(iqxk)  , a(iqzk)  , a(idify) ,
     &                 a(idifx) , a(idifz) , a(ir1)   , r11      , a(igvu)  ,
     &                 a(iguv)  , dicww    , areav    , areau    , gsqs     ,
     &                 s0       , dps      , thick    , sigdif   , sigmol   ,
     &                 vicmol   , dfluxy   , dfluxx   , a(ibbkl) , a(iaakl) ,
     &                 a(icckl) )

      endif

!        Computations for mass balance

      call dlmasb ( rdt      , nmax     , mmax     , kmax     , noq1     ,
     &              noq2     , noq3     , noq      , nosys    , notot    ,
     &              j(ilgra) , flowpnt  , a(iaakl) , a(ibbkl) , a(icckl) ,
     &              a(iarea) , a(iguv)  , a(igvu)  , dps      , thick    ,
     &              a(ir1)   , r11      , nvdim    , j(ivpnw) , a(ivnew) ,
     &              nddim    , j(idpnw) , a(idnew) , iaflag   , a(imas2) ,
     &              ndmpq    , j(iqdmp) , a(idmpq) )

!        Back conversion

      call dlback ( rdt      , nmax     , mmax     , kmax     , j(ilgra) ,
     &              noseg    , nosys    , notot    , gsqs     , r11      ,
     &              a(ivol)  , a(ivol2) , a(iconc) , a(imass) , a(iderv) )
!
!       Forester filter on the vertical
!
      IF ( FORESTER .AND. INTSRT .EQ. 19 ) THEN
         CALL DLWQD2 ( LUN(19) , NOSYS   , NOTOT   , NOSEG   , NOQ3    ,
     *                 KMAX    , A(ICONC), A(LLENG), NOWARN  )
      ENDIF

!     calculate closure error
         if ( lrewin .and. lstrec ) then
            call dlwqce ( a(imass), a(ivoll), a(ivol2), nosys , notot ,
     &                    noseg   , lun(19) )
            call move   ( a(ivoll), a(ivol) , noseg   )
         else
!     replace old by new volumes
            call move   ( a(ivol2), a(ivol) , noseg   )
         endif

!     integrate the fluxes at dump segments fill asmass with mass
         if ( ibflag .gt. 0 ) then
            call proint ( nflux   , ndmpar  , idt     , itfact  , a(iflxd),
     &                    a(iflxi), j(isdmp), j(ipdmp), ntdmpq  )
         endif

!          new time values, volumes excluded

         call dlwqt0 ( lun      , itime    , itimel   , a(iharm) , a(ifarr) ,
     &                 j(inrha) , j(inrh2) , j(inrft) , idt      , a(ivol)  ,
     &                 a(idiff) , a(iarea) , a(iflow) , a(ivelo) , a(ileng) ,
     &                 a(iwste) , a(ibset) , a(icons) , a(iparm) , a(ifunc) ,
     &                 a(isfun) , j(ibulk) , lchar    , c(ilunt) , ftype    ,
     &                 intsrt   , isflag   , ifflag   , ivflag   , ilflag   ,
     &                 ldumm2   , j(iktim) , j(iknmr) , j(inisp) , a(inrsp) ,
     &                 j(intyp) , j(iwork) , .false.  , ldummy   , rdummy   ,
     &                 .false.   , gridps   , dlwqd    )

!          end of loop

      IF ( ACTION == ACTION_FULLCOMPUTATION ) THEN
          GOTO 10
      ENDIF

   20 CONTINUE

      IF ( ACTION == ACTION_FINALISATION    .OR.
     &     ACTION == ACTION_FULLCOMPUTATION      ) THEN
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

      END

      end module m_dlwqnj
