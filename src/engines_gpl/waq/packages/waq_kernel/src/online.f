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
      module m_online

      implicit none

      contains


      SUBROUTINE SRWSHL(ITIME, A, J, C)
      use timers
      use delwaq2_data
      use m_sysn          ! System characteristics
      use m_sysi          ! Timer characteristics
      use m_sysa          ! Pointers in real array workspace
      use m_sysj          ! Pointers in integer array workspace
      use m_sysc          ! Pointers in character array workspace
      implicit none

      real,             dimension(*) :: a
      integer,          dimension(*) :: j
      character(len=*), dimension(*) :: c

      integer    itime

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "srwshl", ithandl )
!

      CALL SRWINT ( ITIME   , ITSTRT  , ITSTOP  ,
     j              NOSYS   , NOTOT   , NOSEG   ,
     +              NOBND   , NOWST   , NOCONS  , NOPA    , NOFUN   ,
     +              NOSFUN  , NOQ     ,
     J              J(iwast-1),
     J              J(IXPNT-1),
     +              A(ICONS-1),
     J              A(IPARM-1),
     J              A(IBSET-1),
     J              A(IWSTE-1),
     +              A(IFUNC-1),
     J              A(ISFUN-1),
     J              A(ICONC-1),
     J              C(IBNID-1),
     J              C(ISNAM-1),
     J              C(IWSID-1),
     +              C(ICNAM-1),
     J              C(IPNAM-1),
     J              C(IFNAM-1),
     J              C(ISFNA-1),
     J              C(IMNAM-1) )
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
      SUBROUTINE SRWINT ( ITIME , ITSTRT, ITSTOP,
     J                    NOSYS , NOTOT , NOSEG ,
     +                    NOBND , NOWST , NOCONS, NOPA  , NOFUN ,
     +                    NOSFUN, NOQ   , IWASTE, IPOINT, CONS  ,
     +                    PARAM , BNDSET, WASTE , FUNC  , SEGFUN,
     +                    CONC  , BNDNAM, SYSNAM, WANAME, CONAME,
     +                    PANAME, FUNAME, SFNAME, MONAME)
      use timers

!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     FUNCTION           : Interface for SRW communications
!
!                          26-9-2001, WRITE OUTPUT FOR LATERALS
!                                     USE BOUNDARY STREAMS ONLY
!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH   FUNC.  DESCRIPTION
!     ---------------------------------------------------------
!     ITIME   INTEGER    1      INPUT  Simulation time (s)
!     ITSTRT  INTEGER    1      INPUT  Simulation start time (s)
!     NOSYS   INTEGER    1      INPUT  Nr. of active substances
!     NOTOT   INTEGER    1      INPUT  Total nr. of substances
!     NOSEG   INTEGER    1      INPUT  Nr of segments
!     NOBND   INTEGER    1      INPUT  Nr of boundaries
!     NOWST   INTEGER    1      INPUT  Nr of waste loads
!     NOCONS  INTEGER    1      INPUT  Nr of constants
!     NOPA    INTEGER    1      INPUT  Nr of parameters (f(x))
!     NOFUN   INTEGER    1      INPUT  Nr of functions (f(t))
!     NOSFUN  INTEGER    1      INPUT  Nr of segment functions (f(x,t))
!     NOQ     INTEGER    1      INPUT  Nr of exchanges
!     IPOINT  INTEGER    ??,*   INPUT  Definition of exchanges
!     CONS    REAL       *      INPUT  Constants
!     PARAM   REAL       *      INPUT  Parameters
!     BNDSET  REAL       *      INPUT  Boundary concentrations
!     WASTE   REAL       *      INPUT  Wasteloads
!     FUNC    REAL       *      INPUT  Functions
!     SEGFUN  REAL       *      INPUT  Segment functions
!     BNDNAM  CHARACTER  *      INPUT  Boundary names
!     SYSNAM  CHARACTER  *      INPUT  Substances names
!     WANAME  CHARACTER  *      INPUT  Wasteload names
!     CONAME  CHARACTER  *      INPUT  Constant names
!     PANAME  CHARACTER  *      INPUT  Parameter names
!     FUNAME  CHARACTER  *      INPUT  Function names
!     SFNAME  CHARACTER  *      INPUT  Segment function names
!     MONAME  CHARACTER  *      INPUT  Model identification strings

!     Delft-IO for SRW

      use m_dio_plt


!     DELWAQ variables from argument list
!
      INTEGER      ITIME , ITSTRT, NOSYS , NOTOT , NOSEG ,
     +             NOBND , NOWST , NOCONS, NOPA  , NOFUN ,
     +             NOSFUN, NOQ   , ITSTOP
      INTEGER      IPOINT( 4,NOQ ), IWASTE(NOWST)
      REAL         CONS(*)        , PARAM(NOPA,*)   ,
     +             WASTE(NOTOT+1,*), BNDSET(NOSYS,*),
     +             FUNC(*)        , SEGFUN(NOSEG,*) ,
     +             CONC(NOTOT,*)
      CHARACTER*20 BNDNAM(*)      , SYSNAM(*)       ,
     +             WANAME(*)      , CONAME(*)       ,
     +             PANAME(*)      , FUNAME(*)       ,
     +             SFNAME(*)
      CHARACTER*40 MONAME(4)

!     Local variables

      INTEGER      isys  , iseg  , ibnd  , iwst  , icons ,
     +             ifun  , ipa   , isf   , iloc  , ivar  , ioq

      INTEGER      PROCID, Nr_Times
      character*20 Times(1)
      INTEGER      NrLocBoundIn,  NrVarBoundIn
      INTEGER      NrLocBoundOut, NrVarBoundOut
      character*20, allocatable, save : :
     j             LocBoundIn(:), VarBoundIn(:),
     j             LocBoundOut(:), VarBoundOut(:)
      real, allocatable, save : :
     j             Values(:,:)
      integer, allocatable, save : :
     j             dynbnd(:,:,:), dynwst(:,:,:),
     j             bn2seg(:)

      save         Procid, NrLocBoundIn,  NrVarBoundIn,
     j             NrLocBoundOut, NrVarBoundOut

      real         misval
      parameter   (misval = -9999.999)
      character*20 start_date, actual_date

!     Variables related to DIO calls

      integer srwBoundOutSet,    srwBoundInSet
      integer srwBoundInStream, srwBoundOutStream
      integer srwStatus

      save srwBoundOutSet, srwBoundInSet

      data procid / 0 /
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "online", ithandl )

!***********************************************************************
!     System initialisation
!***********************************************************************

      IF ( ITSTRT .EQ. ITIME ) THEN

!         Allocate arrays
          allocate (
     j              LocBoundIn(nobnd+nowst), VarBoundIn(nosys),
     j              LocBoundOut(nobnd+nowst), VarBoundOut(nosys),
     j              Values(nosys,nobnd+nowst),
     j              dynbnd(nosys,nobnd,2), dynwst(nosys,nowst,2),
     j              bn2seg(nobnd)
     j             )

!***********************************************************************
!         Inquire which locations and variables will be input
!
!         In the current version ONLY the active state variables
!         at the model boundaries and at the waste loads are potential
!         input

!         Initialise mapping structure to Delwaq
          do ibnd = 1,nobnd
              do isys = 1,nosys
                  dynbnd(isys,ibnd,1) = 0
                  dynbnd(isys,ibnd,2) = 0
              enddo
          enddo
          do iwst = 1,nowst
              do isys = 1,nosys
                  dynwst(isys,iwst,1) = 0
                  dynwst(isys,iwst,2) = 0
              enddo
          enddo

!         Open Data Stream

          srwBoundInStream = dioCreateStreamSynched (
     j                       dio_Binary_stream,
     j                       'BoundToDelwaq',
     j                       'r' )


!         Open Data Set and Collect Info

          srwBoundInSet = dioGetPltDatasetInfo (
     j                       srwBoundInStream,
     j                       'IncomingBoundaries',
     j                       NrVarBoundIn, VarBoundIn,
     j                       NrLocBoundIn, LocBoundIn,
     +                       Nr_Times, Times )


          if ( srwBoundInSet .gt. 0 ) then
!             SRW run
              procid = 1

              call map_input (
     j                        NrVarBoundIn, VarBoundIn,
     j                        NrLocBoundIn, LocBoundIn,
     j                        nosys , sysnam, nobnd , bndnam, dynbnd )
              call map_input (
     j                        NrVarBoundIn, VarBoundIn,
     j                        NrLocBoundIn, LocBoundIn,
     j                        nosys , sysnam, nowst , waname, dynwst )
          else
!             Not an SRW run
              procid = 0
              goto 9999 !   return
          endif

!***********************************************************************
!         Define which locations and variables will be output
!
!         In the current version ONLY the active state variables
!         at the model boundaries and laterals are output.

          NrVarBoundOut = nosys
          NrLocBoundOut = nobnd+nowst
          do ibnd = 1,nobnd
              LocBoundOut(ibnd) = bndnam(ibnd)
          enddo
          do iwst = 1,nowst
              LocBoundOut(nobnd+iwst) = waname(iwst)
          enddo
          do isys = 1,nosys
              VarBoundOut(isys) = sysnam(isys)
          enddo

!         Open data stream

          srwBoundOutStream = dioCreateStreamSynched (
     j                          dio_Binary_stream,
     j                          'BoundFromDelwaq',
     j                          'w')

          srwBoundOutSet = dioDefinePltDataset (
     j                          srwBoundOutStream,
     j                          'OutGoingBoundaries',
     j                          Dio_Plt_Real,
     j                          NrVarBoundOut,VarBoundOut,
     j                          NrLocBoundOut,LocBoundOut)

!         Map boundaries to segments

          do ioq=1,noq
              if ( ipoint(1,ioq).lt.0 .and. ipoint(2,ioq).gt.0 ) then
                  bn2seg(-ipoint(1,ioq)) = ipoint(2,ioq)
              endif
              if ( ipoint(2,ioq).lt.0 .and. ipoint(1,ioq).gt.0 ) then
                  bn2seg(-ipoint(2,ioq)) = ipoint(1,ioq)
              endif
          enddo

!***********************************************************************
!         Retrieve start date
          start_date = moname(4)(1:20)

!     End of initialisation
      ENDIF

!***********************************************************************
!     Actions within DELWAQ's time loop
!***********************************************************************

      if ( procid .le. 0 ) goto 9999  !    return

!***********************************************************************
!     Fill output array

      do ibnd = 1,nobnd
          iseg = bn2seg(ibnd)
          do isys = 1,nosys
              values(isys,ibnd) = conc(isys,iseg)
          enddo
      enddo
      do iwst = 1,nowst
          iseg = iwaste(iwst)
          do isys = 1,nosys
              values(isys,nobnd+iwst) = conc(isys,iseg)
          enddo
      enddo

!***********************************************************************
!     What time is it?
!     actual_date = find_date_from_start (start_date,(itime-itstrt))

!***********************************************************************
!     Send output
      call DioPutPltDataSetReals (srwBoundOutSet,times(1),
     j               NrVarBoundOut, NrLocBoundOut,  values)

!***********************************************************************
!     Collect Input (and implicitly get permission to proceed)
!     Jos van Gils, 27-4-2001, NOT FOR LAST TIME STEP!
!***********************************************************************
!     Store input in Delwaq structures

      if ( itime .lt. itstop ) then


      srwStatus = dioGetPltDataSetReals (srwBoundInSet,times(1),
     j            NrVarBoundIn,NrLocBoundIn,values)


      if ( srwStatus .gt. 0 ) then
          do ibnd = 1,nobnd
              do isys = 1,nosys
                  iloc = dynbnd(isys,ibnd,1)
                  ivar = dynbnd(isys,ibnd,2)
                  if (iloc.gt.0.and.ivar.gt.0) then
                      if ( abs(values(ivar,iloc)-misval) .gt.
     j                     abs(0.001*misval) )
     j                bndset(isys,ibnd) = values(ivar,iloc)
                  endif
              enddo
          enddo
          do iwst = 1,nowst
              do isys = 1,nosys
                  iloc = dynwst(isys,iwst,1)
                  ivar = dynwst(isys,iwst,2)
                  if (iloc.gt.0.and.ivar.gt.0) then
                      if ( abs(values(ivar,iloc)-misval) .gt.
     j                     abs(0.001*misval) )
     j                waste(1+isys,iwst) = values(ivar,iloc)
!     ......... gaat dit goed? praten we niet over g/s ipv g/m3????????
                  endif
              enddo
          enddo
      endif

      endif

 9999 if ( timon ) call timstop ( ithandl )
      RETURN
      END

      subroutine map_input (Nvarin, varin, Nlocin, locin,
     j                      Nvar  , var  , nloc  , loc  , map )
      use m_zoek
      use timers

      integer           Nvarin, Nlocin, Nvar  , nloc
      integer           map(nvar,nloc,2)
      character*(*)     varin(nvarin), locin(nlocin),
     j                  var(nvar), loc(nloc)

      integer           ilocin, ivarin, iloc   , ivar
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "map_input", ithandl )

      do ilocin = 1,nlocin
          call zoek (Locin(ilocin),nloc,loc,20,iloc)
          do ivarin = 1,nvarin
              call zoek (Varin(ivarin),nvar,var,20,ivar)
              if ( iloc .ge. 1 .and. ivar .ge. 1 ) then
                  map(ivar,iloc,1) = ilocin
                  map(ivar,iloc,2) = ivarin
              endif
          enddo
      enddo

      if ( timon ) call timstop ( ithandl )
      return
      end

      SUBROUTINE RTCSHL(ITIME, A, J, C)
      use timers
      use delwaq2_data
      use m_sysn          ! System characteristics
      use m_sysi          ! Timer characteristics
      use m_sysa          ! Pointers in real array workspace
      use m_sysj          ! Pointers in integer array workspace
      use m_sysc          ! Pointers in character array workspace
      implicit none

      real,             dimension(*) :: a
      integer,          dimension(*) :: j
      character(len=*), dimension(*) :: c


      integer    itime
!
      REAL PROLOC
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "rtcshl", ithandl )

      call rtcint (NOCONS,
     j             NOPA  ,
     j             NOFUN ,
     j             NOSFUN,
     j             NOTOT ,
     J             A(ICONC),
     J             A(ISFUN),
     +             A(IFUNC),
     J             A(IPARM),
     +             A(ICONS),
     j             IDT   ,
     j             ITIME ,
     +             A(IVOL),
     j             NOSEG ,
     j             NOSYS ,
     j             NDMPAR,
     J             J(IPDMP),
     +             A(IBOUN),
     j             NOLOC ,
     j             A(IPLOC),
     j             NODEF ,
     +             A(IDEFA),
     j             NTDMPQ,
     J             C(IDANA),
     J             C(ISNAM),
     J             C(IPNAM),
     J             C(ISFNA),
     J             C(IFNAM),
     &             J(IIOUT),
     &             J(IIOPO),
     &             C(IONAM))
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
      subroutine rtcint (NOCONS, NOPA  ,
     +                   NOFUN , NOSFUN, NOTOT , CONC  , SEGFUN,
     +                   FUNC  , PARAM , CONS  , IDT   , ITIME ,
     +                   VOLUME, NOSEG , NOSYS , NDMPAR, IPDMP ,
     +                   BOUND , NOLOC , PROLOC, NODEF , DEFAUL,
     +                   NTDMPQ, DANAM , SYNAME, paname, sfname,
     +                   funame, IOUTPS, IOPOIN, OUNAM )
      use m_fiosub
      use m_gkwini
      use m_getcom
      use m_open_waq_files
      use timers


      INTEGER    NOCONS, NOPA  , NOFUN , NOSFUN,
     +           NOTOT , IDT   , ITIME , NOSEG , NOSYS ,
     +           NDMPAR, NOLOC , NODEF , NTDMPQ
      INTEGER    IPDMP(*), IOUTPS(7,*), IOPOIN(*)
      REAL       CONC(NOTOT,*),
     +           SEGFUN(NOSEG,*), FUNC(*)      ,
     +           PARAM(*)       , CONS(*)      ,
     +           VOLUME(*)      , BOUND(*)     ,
     +           PROLOC(*)      , DEFAUL(*)
      CHARACTER*20 DANAM(*), SYNAME(*), OUNAM(*)
      character(len=20), intent(in   ) :: paname(*) ! parameter names
      character(len=20), intent(in   ) :: sfname(*) ! segment function names
      character(len=20), intent(in   ) :: funame(*) ! function names

!     Interface to RTC

!     Writes concentration values for dump areas

!     Call to DELWAQ routine from output subsystem
!     NCOUT  is number of output substances (=0 or NOTOT)
!     NRVAR  is number of extra output variables
!     OUTVAL is a buffer of at least (NOTOT+NRVAR)*NDMPAR reals
!     IOPOIN is not used if NRVAR = 0

      real, allocatable, save : : outval(:)
      integer nrvar, ncout, io_rtc, isys, idmp, nrvar2
      logical first, rewine
      character*40 moname(4)
      character*255 filnam
      character*255 inifil
      logical       lfound
      integer       idummy, ierr2
      real          rdummy
      integer       k1, idum

      save    first
      save    filnam
      data    first   /.true./
      data    rewine  /.true./
      data    moname /'Interface from Delwaq to RTC',
     j                'Concentrations for current time step',
     j                ' ',
     j                ' '/
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "rtcint", ithandl )

      !
      ! Pointers into the output system arrays
      ! - use the history file
      !
      k1     = 1 + ioutps(4,1) + ioutps(4,2)
      nrvar  = ioutps(4,3)
      nrvar2 = nrvar / 2

      if ( first ) then
          allocate ( outval(ndmpar*(notot+nrvar2)) )
          filnam = ' '
          call getcom ( '-i'  , 3    , lfound, idummy, rdummy,
     +                  inifil, ierr2)
          if ( lfound ) then
             if ( ierr2.ne. 0 ) then
                inifil = ' '
             endif
          else
             inifil = 'delwaq.ini'
          endif
          open(newunit=io_rtc,file=inifil,status='old',err=123)
          call gkwini(io_rtc,'SimulationOptions',
     j                       'FilenameRTCOutput',filnam)
          close (io_rtc)
  123     continue
          if ( filnam .eq. ' ' ) filnam = 'wq2rtc.his'
      endif

      ncout  = notot
      CALL FIOSUB       (OUTVAL, IOPOIN(K1), NRVAR2 , NOCONS, NOPA  ,
     +                   NOFUN , NOSFUN, NOTOT , CONC  , SEGFUN,
     +                   FUNC  , PARAM , CONS  , IDT   , ITIME ,
     +                   VOLUME, NOSEG , NOSYS , NDMPAR, IPDMP ,
     +                   BOUND , NOLOC , PROLOC, NODEF , DEFAUL,
     +                   NCOUT , NTDMPQ, paname, sfname, funame,
     +                   danam )

      if ( first .or. rewine ) then
          CALL open_waq_files ( IO_RTC, FILNAM, 21    , 1     , IDUM  )

          write ( io_rtc ) moname
          write ( io_rtc ) notot+nrvar2, ndmpar
          write ( io_rtc ) (syname(isys),isys=1,notot),
     &                     (ounam(k1+isys-1),isys=1,nrvar2)
          write ( io_rtc ) (idmp,danam(idmp),idmp=1,ndmpar)
      endif

      write ( io_rtc ) itime
      write ( io_rtc ) (outval(isys),isys=1,(notot+nrvar2)*ndmpar)

      if ( rewine ) close ( io_rtc )

      first = .false.

      if ( timon ) call timstop ( ithandl )
      return
      end


      end module m_online
