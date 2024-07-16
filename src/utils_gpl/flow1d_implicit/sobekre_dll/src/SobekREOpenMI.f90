!******************************************************************
! 
!  SOBEKRE OpenMI ModelEngine interface
!
!
!  Copyright(c) RIZA 2004
!
!  Programmer Jan Noort (Sepra B.V.)
!
!******************************************************************

MODULE SobekRE_OpenMI

!******************************************************************
!
!  SobekRE_OpenMI contains the OPENMI interface for SOBEKRE 
!
!  Exported Functions in SobekRE_OpenMI 
!
!  SobekRE_Initialize         Initialises a SOBEK run
!  SobekRE_GetNsteps          Returns the total number of time steps 
!  SobekRE_PerformTimestep    Computes a SOBEK time step. SOBEK has to be 
!                             initialised
!  SobekRE_GetCurrentTime     Returns the current time of the simulation
!  SobekRE_GetInputTime       Returns the time for which new input is required
!  SobekRE_GetSimulationSpan  Returns the begin time and end time of the SOBEK 
!                             simulation
!  SobekRE_KeepCurrentState   Save current state to NEFIS files
!  SobekRE_RestoreState       Replace indicated state to sobeksim files
!  SobekRe_DeleteState       Not implemented yet
!  SobekRE_Finalize           Ends the SOBEK run, closes files, etc.
!  SobekRE_GetError           Returns the description of the last error
!  
!  Local functions/subroutines in SobekRE_OpenMI
!  
!  defineExchangeitems        Defines the exchange items in ROB
!  DefExItBound               Defines the exchange items in ROB on boundaries
!  DefExItLat                 Defines the exchange items in ROB on Laterals
!  DefExItBranch              Defines the exchange items in ROB on Branches
!  DefExItStruct              Defines the exchange items in ROB on structures
!  ExchangeItemsToRob         Puts the computed exchange items in ROB
!  PutBoundaries              Puts the computed exchange items on boundaries in ROB
!  PutBoundariesSalt          Puts the computed exchange items for salt on boundaries in ROB
!  PutLaterals                Puts the computed exchange items on laterals in ROB
!  PutLateralsSalt            Puts the computed exchange items for salt on laterals in ROB
!  PutBranches                Puts the computed exchange items on branches in ROB
!  GetBoundaries              Gets the boundary condions from ROB and puts them in the
!                             SOBEK boundaries (HSTAT and QSTAT)
!  GetLaterals                Gets the Laterals from ROB and puts them in the
!                             SOBEK lateral conditions (QLAT)
!  OpenMIactive               Sets the lOpenMI flag. This flag indicates whether SOBEK is
!                             run in OpenMI or not.
!  SetError                   Sets an error
!
!******************************************************************

   use frob

!******************************************************************
!
!  Definition of quantities
!
   character(len=ROBIDLEN)     :: Quantity_Level      = 'Water Level'
   character(len=ROBIDLEN)     :: Quantity_Discharge  = 'Discharge'
   character(len=ROBIDLEN)     :: Quantity_Salinity      = 'Salt concentration'
   character(len=ROBIDLEN)     :: Quantity_Chezy      = 'Chezy Coefficient'
   character(len=ROBIDLEN)     :: Quantity_Area       = 'Area'
   character(len=ROBIDLEN)     :: Quantity_Width      = 'Width'
   character(len=ROBIDLEN)     :: Quantity_Depth      = 'Average Depth'
   character(len=ROBIDLEN)     :: Quantity_HyRad      = 'Hydraulic Radius'
   character(len=ROBIDLEN)     :: Quantity_Vel        = 'Average Velocity'
   character(len=ROBIDLEN)     :: Quantity_Froude     = 'Froude Number'
   character(len=ROBIDLEN)     :: Quantity_Crest      = 'Crest Height'

!******************************************************************
!
!  Global variables
!
   logical         , private   :: CheckDone           = .false.
   logical         , public    :: InitDone            = .false.
   logical         , private   :: lOpenMI             = .false.
   character(len=*), parameter :: modelID             = 'SobekRE'               
   character(len=*), parameter :: modelDescription    = 'Sobek River and Estuaries'
   integer         , private   :: istep               = 0
   real(kind=8)    , private   :: deltat              = 86400.0
   real(kind=8),     private   :: julian_start
   integer         , private   :: lubtc,  lustat, juer,   ker    
   integer         , private   :: itim(2),nstep,  wqagst, restrt
   integer         , private   :: itp,    ifp,    ipc,    ipcs,   idtm
   integer         , private   :: sbkrel(2), step
   integer         , private   :: juresi ,jufrou ,juresd,  justrd, jusold
   integer         , private   :: inocon ,itstat(4) ,juscr,lurtn,  jugraut,jugralg

   logical         , private   :: lauto,  lflow,  lkalm,  lsalt,  lsedt
   logical         , private   :: lgrad, lmorp,  lwqin,  ldlwq,  laux
   logical         , private   :: lrivr,  lestu,  lrest,  ldebug
   logical         , private   :: lbatch, steady, lwstat, newres
   logical         , private   :: lfrou
   logical         , private   :: lmoza, lgrwt

   double precision, private   :: dtf
   real            , private   :: frobuf(8)
   real            , private   :: strcpu
   double precision, private, parameter   :: julian2modifiedjulian_shift = -2400000.5d0
   
   real, allocatable           :: slat(:)
   
   character*256 filnam
                
   TYPE tMsg
      INTEGER (KIND(8))    :: Code        ! (Fout)code
      CHARACTER (LEN=1000) :: Text        ! (Fout) tekst
      INTEGER (KIND(1))    :: MsgType     ! 0 = niet fataal, 1 = fataal (Error)
   END TYPE tMsg

   type(tMsg)              :: Msg

   
   CONTAINS

   ! SobekRE_Initialize
   ! - Initialiseer applicatie
   !
   function SobekRE_Initialize(ArgString, ArgCount) result(rv)
      !DEC$ ATTRIBUTES DLLEXPORT :: SobekRE_Initialize
      !DEC$ ATTRIBUTES ALIAS : 'SobekRE_Initialize' :: SobekRE_Initialize
      
      ! Parameters

      implicit none
      integer,intent(in)            :: ArgCount
      character(len=*)  ,intent(in) :: ArgString
      integer                       :: rv

      rv = 0
      
      ! OpenMI is actief:
      write(*,*) 'SobekRE_Initialize(', trim(ArgString), ')'

      lOpenMI = .true.
      filnam = ArgString

      ! Initialise SOBEK 

      call soini1(sbkrel, itstat, lbatch, lfrou, ldebug, lurtn, juscr,  &
                  filnam, ker)

      if (ker.ne.3) then
         call soini2(sbkrel, itstat, lfrou , ldebug, juscr , juer,   &
                     juresi, jufrou, juresd, justrd, jusold, itim,   &
                     nstep , itp,    ifp,    ipc,    ipcs,   wqagst, &
                     restrt, idtm,   lustat, jugraut,jugralg,inocon, &
                     frobuf, strcpu, dtf,    lauto,  lflow,  lkalm,  &
                     lsalt,  lsedt,  lmorp,  lgrad,  lwqin,  ldlwq,  &
                     laux,   lmoza,  lgrwt,  lrivr,  lestu,          &
                     steady, lwstat, newres, lrest,  filnam, ker)
         if (ker.eq.3) then
            rv = -1
         endif
      else
         rv = -1
      endif

      ! Store the start time for use in GetTimeSpan

      if (rv==0) rv = SobekRE_GetCurrentTime(julian_start)

      ! Initialize ROB

      if (rv==0) rv = FRobInitialize()
      
      ! Define the exchange Items

      if (rv==0) rv = defineExchangeitems()
      
      if(rv.ne.0) then
         call SetError('Error initialising SOBEK', 1, 1)
      endif
      InitDone = .true.

   end function SobekRE_Initialize

   !
   !  Determine the total number of time steps
   !
   function SobekRE_GetNsteps(out_nsteps) result(rv)
      !DEC$ ATTRIBUTES DLLEXPORT :: SobekRE_GetNsteps
      !DEC$ ATTRIBUTES ALIAS : 'SobekRE_GetNsteps' :: SobekRE_GetNsteps
      
      implicit none
      integer                                            :: rv, out_nsteps
      
      if (lOpenMI) then
         out_nsteps = nstep
         rv = 0
      else
         rv = -1
         call SetError('Error SOBEK Not initialised', 2, 1)
      endif
   end function SobekRE_GetNsteps


   !
   ! Compute timestep
   !
   function SobekRE_PerformTimestep() result(rv)
      !DEC$ ATTRIBUTES DLLEXPORT :: SobekRE_PerformTimestep
      !DEC$ ATTRIBUTES ALIAS : 'SobekRE_PerformTimestep' :: SobekRE_PerformTimestep
      implicit none
      include '..\include\memplf90.i'
 
      integer :: rv

      integer :: nstru, strtyp, strunm, strpar
      integer :: gtipnt, gtcpnt, gtrpnt
      
      rv = 0

      ! Perform a SOBEK time step

!      if (initdone .and. OpenMIactive()) then
!      integer   nstru, strtyp, strunm
!      integer  gtipnt, gtrpnt, gtcpnt
!      external gtipnt, gtrpnt, gtcpnt
         ! Fill structure changes

         nstru    = ip(gtipnt('NSTRU'))
         strtyp   =    gtipnt('STRTYP')         
         strunm   =    gtcpnt('STRUNM')         
         strpar   =    gtrpnt('STRPAR')  

         call GetStruct(ip(strtyp), cp(strunm), rp(strpar), nstru)
 !     endif
 
      call sosimc ( lflow  ,lkalm  ,lsalt  ,lsedt  ,lmorp  ,  &
                    lgrad  ,lwqin  ,lrivr  ,lestu  ,lmoza  ,  &
                    lgrwt  ,wqagst ,restrt ,newres ,  &
                    itim   ,nstep  ,dtf    ,steady ,lwstat ,  &
                    lustat ,itp    ,ifp    ,ipcs   ,idtm   ,  &
                    juer   ,juresi ,jufrou ,juresd ,justrd ,  &
                    ker    ,ldebug ,inocon ,jusold ,lfrou  ,  &
                    itstat ,juscr  ,frobuf ,jugraut,jugralg,step)

      ! Put the exchange items in ROB

      rv = ExchangeItemsToRob()

      if (ker.ne.0) then
         rv = -1      
         call SetError('Error during performtimestep', 1, 1)
      else if(rv.ne.0) then
         call SetError('Error putting data in ROB', 1, 1)
      endif
   end function SobekRE_PerformTimestep

   ! Returns the current time of the simulation. Format:
   ! (YYYYMMDD.HHMMSS)
   !
   function SobekRE_GetCurrentTime(julian) result(rv)
      !DEC$ ATTRIBUTES DLLEXPORT :: SobekRE_GetCurrentTime
      !DEC$ ATTRIBUTES ALIAS : 'SobekRE_GetCurrentTime' :: SobekRE_GetCurrentTime

      use Calendar
      implicit none
      
      ! Parameters
      
      real(8),intent(out)  :: julian
      integer              :: rv

      ! Local variables

      integer    :: time, year, month, day, date, hours, minutes, seconds

      ! itim(1) contains YYYYMMDD (year, month, day)    
      ! itim(2) contains HHMMSSHH (hour,minute,second, hundredth of a second)
      !
      date     = itim(1)
      year     = date/10000
      date     = date - year*10000
      month    = date/100
      day      = date - month*100

      time     = itim(2)
      hours    = time/1000000
      time     = time-hours*1000000
      minutes  = time/10000
      seconds  = time-minutes*10000

      julian   = ToJulian(year,month,day,hours,minutes,seconds,1)
      rv = 0
   
   end function SobekRE_GetCurrentTime

   ! Returns the time for which new input is required
   ! (YYYYMMDD.HHMMSS)
   !
   function SobekRE_GetInputTime(julian) result(rv)
      !DEC$ ATTRIBUTES DLLEXPORT :: SobekRE_GetInputTime
      !DEC$ ATTRIBUTES ALIAS : 'SobekRE_GetInputTime' :: SobekRE_GetInputTime
      implicit none
      real(8),intent(out) :: julian
      integer :: rv

      rv = SobekRE_GetCurrentTime(julian)
      julian = julian+dtf/86400d0

   end function SobekRE_GetInputTime

   !  Returns the begin time and end time of the SOBEK simulation
   !
   function SobekRE_GetSimulationSpan(julian_strt, julian_end) result(rv)
      !DEC$ ATTRIBUTES DLLEXPORT :: SobekRE_GetSimulationSpan
      !DEC$ ATTRIBUTES ALIAS : 'SobekRE_GetSimulationSpan' :: SobekRE_GetSimulationSpan
      implicit none
      real(8),intent(out) :: julian_strt, julian_end
      integer :: rv
      rv = -1
      if ( (nstep.eq.0) .or. (dtf.eq.0) ) then
         call SetError('Error number of timesteps or time step not set', 3, 1)
      else
         julian_strt= julian_start
         julian_end = julian_start+nstep*dtf/86400d0
         rv = 0
      endif
   end function SobekRE_GetSimulationSpan
   
   !  Save current State to NEFIS files 
   !
   function SobekRE_KeepCurrentState( state_id ) result(rv)
      !DEC$ ATTRIBUTES DLLEXPORT :: SobekRE_KeepCurrentState
      !DEC$ ATTRIBUTES ALIAS : 'SobekRE_KeepCurrentState' :: SobekRE_KeepCurrentState
      implicit none

      integer                           :: rv
      character*(*),intent(in)          :: state_id

      ! Local variables of KeepCurrentState

      character*(256)   :: defdesc, datdesc
      integer           :: ncontr, nstru, arexop
      integer           :: nentri, errr, i
      integer           :: ndim  (10)
      integer           :: lagstm, nlags
      integer           :: len
      character*16      :: grnamf
      character*16      :: nameel(10) , quanel(10), unitel(10) , nameac(11)
      character*64      :: descel(10)
      logical           :: inikeep, newkeep

!     Definition of elements.

      data (ndim(i)  ,descel(i)   ,nameel(i) ,quanel(i) , &
            unitel(i),i=1,10) /                           &
      1,'Restart time step'              ,'ISTEP'  ,'t'  ,'-'    , &
      1,'Water levels on t=n+1'          ,'H2'     ,'h'  ,'m'    , & 
      1,'Discharges on t=n+1'            ,'Q2'     ,'Q'  ,'m3/s' , & 
      2,'Structure history'              ,'STRHIS' ,'<>' ,'-'    , & 
      2,'Control history'                ,'CONHIS' ,'<>' ,'-'    , & 
      2,'Aggregated flows WQ-interface'  ,'QAGGR'  ,'Q'  ,'m3/s' , & 
      1,'Aggregated lateral discharges'  ,'QLAGGR' ,'Q'  ,'m3/s' , & 
      2,'Status variable for extra area' ,'AREXCN' ,'<>' ,'-'    , & 
      2,'Time lag in discharges        ' ,'BUFLAG' ,'<>' ,'-'    , & 
      3,'Groundwater history           ' ,'GRHIS'  ,'<>' ,'-'    /

      data  grnamf   / 'FLOW-KEP-GROUP' /

      ! Extra LOCAL parameters for NEFIS

      integer       :: fd_nefis_rst
      integer       :: ncelst
      integer       :: juer
      integer       :: contrl, conhis, strhis, qaggr , qlaggr
      integer       :: arexcn, grhis, buflag

      character*16  :: tijd

      real          :: curtim

      character*1   :: coding

      ! EXTERNAL FUNCTIONS

      integer, external   :: gtipnt, gtrpnt, gtdpnt, clsnef

!     Include memory pool

      include '..\include\memplf90.i'
      include '..\include\ccomp.i'

      !
      ! Purpose of routine
      !
      ! KeepState of waterlevels, discharges and all restart variables
     
      ! Body

      rv = 0

      ! Create a "Keepstate-file" and write to that (new) "KeepState"-file
      ! Afterwards the user is responsible for deleting it

      ! Create a name in which the time is present

      write(tijd( 1: 16 ),'(2i8)') itim(1),itim(2)

      ! Check for space in the time:

      do len=1 ,16
         if ( tijd(len:len) == ' ' ) tijd(len:len) = '0'
      enddo 

      len = index(state_id,'TIME=') + 4

      defdesc = state_id(1:len)//tijd//'.stf'
      datdesc = state_id(1:len)//tijd//'.sta'

      ! Initialize a "Keepstate-file"
      ! Be sure that old files with the same name have been deleted:
     
      open  ( 31, file    =  datdesc )
      close ( 31, status  = 'delete' )
      open  ( 31, file    =  defdesc )
      close ( 31, status  = 'delete' )

      ! Open files with the given names for WVC keep state

      call soofil ( coding , fd_nefis_rst   , &
                    defdesc, datdesc, juer   , errr )

      ! File has been created and does exist: 
      ! use old restart routines to write info to these files

      nentri = 10

      ! Determine constants and addresses:

      ncontr = ip(gtipnt ('NCONTR'))
      ngrid  = ip(gtipnt ('NGRID' ))
      nstru  = ip(gtipnt ('NSTRU' ))
      nqlat  = ip(gtipnt ('NQLAT' ))
      arexop =    gtipnt ('AREXOP')
      lagstm = ip(gtipnt ('LAGSTM'))
      nlags  = ip(gtipnt ('NLAGS' ))

      ! File is always a "new" NEFIS file:

      newkeep = .true.
      inikeep = .false.
                
      call FLDFST(fd_nefis_rst  , grnamf , nentri ,               &
                  nstru     , ncontr     , nqlat  , ngrid  ,  &
                  lwqin     , ip(arexop) , ndim   , nameel ,  &
                  quanel    , unitel     , descel ,           &
                  newkeep   , inikeep    , nameac ,           &
                  lagstm    , nlags      , lgrwt  , errr   )
      if (errr.ne.0) goto 1000

      ! Next call old Restart routines for writing data to NEFIS
     
      hpack  = gtdpnt('HPACK')
      qpack  = gtdpnt('QPACK')

      ! Determine present time

      curtim = sngl( time )

      ! Determine pointers to arrays in pool:

      contrl = gtrpnt('CONTRL')
      conhis = gtrpnt('CONHIS')
      strhis = gtrpnt('STRHIS')
      qaggr  = gtrpnt('QAGGR' )
      qlaggr = gtrpnt('QLAGGR')
      arexcn = gtipnt('AREXCN')
      grhis  = gtrpnt('GRHIS' )
      buflag = gtrpnt('BUFLAG')

      ! Write values to the data file according to the definition file:

      ncelst = 1

      call FLWRST(fd_nefis_rst , grnamf , nentri ,                &
                  nstru  ,ncontr ,ngrid   , lwqin  , ncelst , &
                  itim   ,curtim ,nameel  , dp(hpack)       , &
                  dp(qpack)  , rp(contrl) , rp(conhis)      , &
                  rp(strhis) , rp(qaggr)  , rp(qlaggr)      , &
                  ip(arexop) , ip(arexcn) , lagstm , nlags  , &
                  rp(buflag) , lgrwt      , rp(grhis)       , &
                  errr  )
      if (errr.ne.0) goto 1000

      goto 1010
1000  continue
      rv = -1
1010  continue

      ! Close new created NEFIS files
      errr = clsnef(fd_nefis_rst)
      if (errr /= 0) rv=-1

   end function SobekRE_KeepCurrentState

   !  Replace indicated State to sobeksim files 
   !
   function SobekRE_RestoreState( state_id, datum, tijd ) result(rv)
      !DEC$ ATTRIBUTES DLLEXPORT :: SobekRE_RestoreState
      !DEC$ ATTRIBUTES ALIAS : 'SobekRE_RestoreState' :: SobekRE_RestoreState
      implicit none

      integer                           :: rv
     character*(*),intent(in)          :: state_id
     integer, intent (in)              :: datum, tijd

      !  Local Variables of RestoreState

      integer          :: fd_nefis_rst
      integer          :: ncontr, nstru, arexop
      integer          :: nentri, errr  , i
      integer          :: lagstm, nlags
      integer          :: ncelst, len
      integer          :: juer
      integer          :: conhis, strhis, qaggr , qlaggr
      integer          :: arexcn, grhis, buflag

      character*1      :: coding
      character*(256)  :: defdesc, datdesc
      character*16     :: grnamf
      character*16     :: nameel(10)

!     Definition of elements.

      data (nameel(i), i=1,10) /   &
      'ISTEP'  , &
      'H2'     , & 
      'Q2'     , & 
      'STRHIS' , & 
      'CONHIS' , & 
      'QAGGR'  , & 
      'QLAGGR' , & 
      'AREXCN' , & 
      'BUFLAG' , & 
      'GRHIS'     /

      data  grnamf   / 'FLOW-KEP-GROUP' /

     ! EXTERNAL FUNCTIONS

      integer, external     :: gtipnt, gtrpnt, gtdpnt, clsnef

      ! Include memory pool

      include '..\include\memplf90.i'
     include '..\include\ccomp.i'

     ! Body

      rv = 0

      write(*,*) 'Current time = ',itim(1),itim(2)

     itim(1) = datum
     itim(2) = tijd
       
      ! Use the two given names (i.e. defdesc and datdesc):

      len = index(state_id,'TIME=') + 20

      defdesc = state_id(1:len)//'.stf'
      datdesc = state_id(1:len)//'.sta'

     ! Open the NEFIS file with the restore timestep results

      call soofil ( coding , fd_nefis_rst   , &
                    defdesc, datdesc, juer   , errr )

      ! Do not check itim while it is not known. It is the responsibility of the user
      ! that submitted file and time as given by user are the same

     if (errr.ne.0) goto 1000

      ncelst =  1
      nentri = 10

!     Determine constants and addresses:

      ncontr  = ip(gtipnt ('NCONTR'))
      ngrid   = ip(gtipnt ('NGRID' ))
      nstru   = ip(gtipnt ('NSTRU' ))
      arexop  =    gtipnt ('AREXOP')
      hpack   =    gtdpnt ('HPACK' )
      qpack   =    gtdpnt ('QPACK' )
      conhis  =    gtrpnt ('CONHIS')
      strhis  =    gtrpnt ('STRHIS')
      qaggr   =    gtrpnt ('QAGGR' )
      qlaggr  =    gtrpnt ('QLAGGR')
      arexcn  =    gtipnt ('AREXCN')
      grhis   =    gtrpnt ('GRHIS' )
      buflag  =    gtrpnt ('BUFLAG')
      lagstm  = ip(gtipnt ('LAGSTM'))
      nlags   = ip(gtipnt ('NLAGS' ))

      call FLREST (fd_nefis_rst, grnamf     , nentri ,    &
                   nstru      , ncontr     , ngrid      , lwqin  ,    &
               ncelst     , nameel     , dp(hpack)  , dp(qpack),  & 
               rp(conhis) , rp(strhis) , rp(qaggr)  , rp(qlaggr), &
               ip(arexop) , ip(arexcn) , lagstm     , nlags     , &
               rp(buflag) , lgrwt      , rp(grhis)  , errr  )
      if (errr.ne.0) goto 1000

      ! After using the results close the NEFIS files

      errr = clsnef(fd_nefis_rst)

     if (errr /= 0) rv=-1

      goto 1010

1000  continue
      rv = -1
1010  continue
   end function SobekRE_RestoreState

   ! Schrijven naar disk
   ! Memory cleanup
   !
   function SobekRE_Finalize() result(rv)
      !DEC$ ATTRIBUTES DLLEXPORT :: SobekRE_Finalize
      !DEC$ ATTRIBUTES ALIAS : 'SobekRE_Finalize' :: SobekRE_Finalize
      implicit none
      integer                   :: rv
      call SOSIMf (lflow  ,lsalt  ,lmorp  ,lgrad  ,lwqin  ,             &
                   ldlwq  ,laux   ,newres ,itim   ,             &
                   nstep  ,dtf    ,juer   ,ker    ,ldebug )
      call sofin(frobuf, strcpu, lfrou, lwstat, juer, inocon,           &
                 lustat, lurtn, itstat, ker)

      call FRobFinalize
      rv = 0
   end function SobekRE_Finalize

   !  Returns the description of the last error
   !
   function SobekRE_GetError( errorCode, errorDescription, errorType)result(rv)
      !DEC$ ATTRIBUTES DLLEXPORT :: SobekRE_GetError
      !DEC$ ATTRIBUTES ALIAS : 'SobekRE_GetError' :: SobekRE_GetError

      implicit none
      integer              ,intent(out)      :: errorCode
      character (len=*)    ,intent(out)      :: errorDescription
      integer              ,intent(out)      :: errorType
      integer                                :: rv
      
      if (len_trim(Msg%Text) > 0 ) then
         errorDescription      = Msg%Text
         errorCode             = Msg%Code
         errorType             = Msg%MsgType
         rv                    = 0
      else
         errorDescription      = 'unknown error'
         errorCode             = 0
         errorType             = 0
         rv                    = -1
      endif
   end function SobekRE_GetError

   !  Defines the exchange items in ROB
   !
   function defineExchangeitems result(rv)
      
      implicit none
      include '..\include\memplf90.i'

      integer           :: rv
      integer           :: nnode, node, nodenm, gridid, nbran, ngrid, nstru
      integer           :: nqlat, qlatid, branch, strtyp, strunm
      integer           :: gtipnt, gtcpnt
!

!
      rv = FRobModelAdd (modelID, modelDescription)
!
!    Create all units
!
      rv = FRobUnitAdd ('m',           'meter',                      1.0D0, 0.0D0 )
      if (rv==0) rv = FRobUnitAdd ('m1/2s-1',     'm1/2s-1',                    1.0D0, 0.0D0 )
      if (rv==0) rv = FRobUnitAdd ('m3/s',        'cubic meter per second',     1.0D0, 0.0D0 )
      if (rv==0) rv = FRobUnitAdd ('kg/m3',       'kilogram per cubic meter',   1.0D3, 0.0D0 )
      if (rv==0) rv = FRobUnitAdd ('m2',          'square meter',               1.0D0, 0.0D0 )
      if (rv==0) rv = FRobUnitAdd ('m/s',         'meters per second',          1.0D0, 0.0D0 )
      if (rv==0) rv = FRobUnitAdd ('-',           'dimensionless',              1.0D0, 0.0D0 )
      if (rv.ne.0) then
         call SetError('Error creating units', 10, 1)
      else

!        Create all quantities
!
         if (rv==0) rv = FRobQuantityAdd (Quantity_Level    , Quantity_Level     , 'm')
         if (rv==0) rv = FRobQuantityAdd (Quantity_Discharge  , Quantity_Discharge , 'm3/s')
         if (rv==0) rv = FRobQuantityAdd (Quantity_Salinity , Quantity_Salinity  , 'kg/m3')
         if (rv==0) rv = FRobQuantityAdd (Quantity_Chezy    , Quantity_Chezy     , 'm1/2s-1')
         if (rv==0) rv = FRobQuantityAdd (Quantity_Area        , Quantity_Area      , 'm2')
         if (rv==0) rv = FRobQuantityAdd (Quantity_Width    , Quantity_Width     , 'm')
         if (rv==0) rv = FRobQuantityAdd (Quantity_Depth    , Quantity_Depth     , 'm')
         if (rv==0) rv = FRobQuantityAdd (Quantity_HyRad    , Quantity_HyRad     , 'm')
         if (rv==0) rv = FRobQuantityAdd (Quantity_Vel         , Quantity_Vel       , 'm/s')
         if (rv==0) rv = FRobQuantityAdd (Quantity_Froude      , Quantity_Froude    , '-')
         if (rv==0) rv = FRobQuantityAdd (Quantity_Crest    , Quantity_Crest     , 'm')
         if (rv.ne.0) then
            call SetError('Error creating Quantities ', 11, 1)
         endif
      endif

      if (rv==0) then

         ! Define boundary exchange items

         nnode  = ip (gtipnt ( 'NNODE' ))
         node   =     gtipnt ( 'NODE'  )
         nodenm =     gtcpnt ( 'NODENM')
         rv = DefExItBound(ip(node), cp(NodeNm), nnode)
         if (rv.ne.0) then
            call SetError('Error creating Exchange items on boundaries', 12, 1)
         endif
      endif

      if (rv==0) then

         ! Define lateral exchange items on branches

         nqlat  = ip (gtipnt   ( 'NQLAT'))
         qlatid = max(1,gtcpnt ('QLATNM'))
         allocate(slat(nqlat))
         rv = DefExItLat(cp(qlatid), nqlat)
         if (rv.ne.0) then
            call SetError('Error creating Exchange items on Laterals', 13, 1)
         endif
      endif

      if (rv==0) then
      
         ! Define exchange items on branches

         branch =    gtipnt( 'BRANCH' )
         GridId =    gtcpnt( 'GRIDNM' )
         nbran  = ip(gtipnt( 'NBRAN' ))
         nGrid  = ip(gtipnt( 'NGRID' ))
         rv = DefExitBranch(ip(branch), cp(GridId), nbran, nGrid)
         if (rv.ne.0) then
            call SetError('Error creating Exchange items on Branches', 14, 1)
         endif
      endif

      if (rv==0) then
      
         ! Define exchange items on structures

         nstru    = ip(gtipnt('NSTRU'))
         strtyp   =    gtipnt('STRTYP')         
         strunm   =    gtcpnt('STRUNM')         

         rv = DefExItStruct(ip(strtyp), cp(strunm), nstru)
         if (rv.ne.0) then
            call SetError('Error creating Exchange items on structures', 15, 1)
         endif
      endif

   end function

   !  Defines the exchange items in ROB on boundaries
   !
   function DefExItBound(node, NodeId, nnode)  result(rv)

      use frob
    
      implicit none

      ! Parameters

      integer, intent(in)        :: nnode
      integer, intent(in)        :: node(4,nnode)
      integer                    :: rv
      character*40, intent(in)   :: NodeId(nnode)
      
      ! Local variables

      integer                    :: nx, istat

      include '..\include\sobconf90.i'

      rv = 0

      do nx = 1, nnode
         istat  = node(3,nx)
         if (node(1,nx) .eq. chbou) then

            ! H-boundary

            if (rv==0) rv = FRobElementSetAddIDBasedSingle (ModelId, NodeId(nx), 'H-boundary at '//NodeId(nx))
            if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Level,     NodeId(nx), PROVIDING_ROLE)
            if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Discharge, NodeId(nx), PROVIDING_ROLE)
            if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Level,     NodeId(nx), ACCEPTING_ROLE)
            if (lsalt) then
               if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Salinity,     NodeId(nx), PROVIDING_ROLE)

!               if ((rv==0) .and. ( (sbdpar(1,ibn)==csbusr) .or. (sbdpar(1,ibn)==csbthh) ) ) then
                  rv = fRobExchangeItemAdd(ModelId, Quantity_Salinity,     NodeId(nx), ACCEPTING_ROLE)
!`              endif

            endif

         else if (node(1,nx) .eq. cqbou) then

            ! Q-boundary

            if (rv==0) rv = FRobElementSetAddIDBasedSingle (ModelId, NodeId(nx), 'Q-boundary at '//NodeId(nx))
            if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Level,     NodeId(nx), PROVIDING_ROLE)
            if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Discharge, NodeId(nx), PROVIDING_ROLE)
            if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Discharge, NodeId(nx), ACCEPTING_ROLE)
            if (lsalt) then
               if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Salinity,     NodeId(nx), PROVIDING_ROLE)
               if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Salinity,     NodeId(nx), ACCEPTING_ROLE)
            endif

         else if (node(1,nx) .eq. cqhbou) then

            ! QH-boundary

            if (rv==0) rv = FRobElementSetAddIDBasedSingle (ModelId, NodeId(nx), 'QH-boundary at '//NodeId(nx))
            if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Level,     NodeId(nx), PROVIDING_ROLE)
            if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Discharge, NodeId(nx), PROVIDING_ROLE)
            if (lsalt) then
               if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Salinity,     NodeId(nx), PROVIDING_ROLE)
            endif

         else if (node(1,nx) .eq. chqbou) then

            ! HQ-boundary

            if (rv==0) rv = FRobElementSetAddIDBasedSingle (ModelId, NodeId(nx), 'HQ-boundary at '//NodeId(nx))
            if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Level,     NodeId(nx), PROVIDING_ROLE)
            if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Discharge, NodeId(nx), PROVIDING_ROLE)
            if (lsalt) then
               if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Salinity,     NodeId(nx), ACCEPTING_ROLE)
               if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Salinity,     NodeId(nx), PROVIDING_ROLE)
            endif

         endif
      end do 

   end function DefExItBound

   !  Defines the exchange items in ROB on Laterals
   !
   function DefExItLat(qlatid, nqlat) result(rv)
      implicit none

      ! parameters

      integer, intent(in)        :: nqlat
      integer                    :: rv
      character*40, intent(in)   :: QlatId(nqlat)
      
      ! Local variables

      integer                    :: i

      rv = 0
      
      do i = 1, nqlat
         if (rv==0) rv = FRobElementSetAddIDBasedSingle (ModelId, QlatId(i), 'Lateral at '//QlatId(i))
         if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Level,     QlatId(i), PROVIDING_ROLE)
         if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Discharge, QlatId(i), PROVIDING_ROLE)
         if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Discharge, QlatId(i), ACCEPTING_ROLE)
         if (lsalt) then
            if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Salinity,     QlatId(i), ACCEPTING_ROLE)
            if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Salinity,     QlatId(i), PROVIDING_ROLE)
         endif
      end do
      
   end function DefExItLat

   !  Defines the exchange items in ROB on Branches
   !
   function DefExitBranch(branch, GridId, nbran, nGrid) result(rv)
   
      use frob
      
      implicit none

      !  Parameters

      integer, intent(in)        :: nbran, nGrid
      integer, intent(in)        :: branch(4,nbran)
      character*40, intent(in)   :: GridId(nGrid)

      !  Local variables

      integer                    :: rv, ibran, lbrnam, i1, i2
      character*40               :: branam

      rv = 0

      do ibran = 1, nbran
         call getbrn(ibran, branam, lbrnam)
         i1 = branch(3, ibran)
         i2 = branch(4, ibran)

         if (rv==0) rv = FRobElementSetAddIDBasedMultiple(modelID, branam, 'gridpoints on branch '//branam, GridId(i1:i2))
         if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Level,     branam, PROVIDING_ROLE)
         if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Discharge,  branam, PROVIDING_ROLE)
         if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Chezy,      branam, PROVIDING_ROLE)
         if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Area,    branam, PROVIDING_ROLE) 
         if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Width,      branam, PROVIDING_ROLE)
         if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Depth,      branam, PROVIDING_ROLE)
         if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_HyRad,      branam, PROVIDING_ROLE)
         if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Vel,        branam, PROVIDING_ROLE)
         if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Froude,     branam, PROVIDING_ROLE)

         if (lsalt) then
            if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Salinity,     branam, PROVIDING_ROLE)
         endif
      end do
      
   end function DefExitBranch
        
   !  Defines the exchange items in ROB on structures
   !
   function DefExItStruct(strtyp, strunm, nstru) result(rv)
      implicit none
      include '..\include\sobconf90.i'
      integer                    :: rv, i
      integer,       intent(in)  :: nstru         
      integer,       intent(in)  :: strtyp(10,nstru) 
      character*40,  intent(in)  :: strunm(nstru)

      rv = 0
      
      
      do i = 1, nstru
         if ( (strtyp(1,i).eq.csweir) .or. (strtyp(1,i).eq.caweir) ) then
            if (rv==0) rv = FRobElementSetAddIDBasedSingle (ModelId, strunm(i), 'Structure '//strunm(i))
            if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Crest,     strunm(i), PROVIDING_ROLE)
            if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Crest,     strunm(i), ACCEPTING_ROLE)
            if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Width,     strunm(i), PROVIDING_ROLE)
            if (rv==0) rv = fRobExchangeItemAdd(ModelId, Quantity_Width,     strunm(i), ACCEPTING_ROLE)
         endif
      enddo
   end function DefExItStruct


   ! Puts the computed exchange items in ROB
   !
   function ExchangeItemsToRob() result(rv)
      implicit none
      include '..\include\memplf90.i'
      integer           :: rv

      ! Local variables

      integer           :: nnode, node, nodenm, gridid, nbran, ngrid, nstru
      integer           :: nqlat, qlatid, branch, h2, q2, qlat, qltpar
      integer           :: ct, waoft, r, csa, sbdscr, nboun, sbdpar, strpar
      integer           :: strtyp, strunm
      integer           :: gtipnt, gtcpnt, gtrpnt

      ! Quantities

      ngrid = ip(gtipnt ('NGRID' ))
      h2    =    gtrpnt ('HPACK' ) + ngrid * 2
      q2    =    gtrpnt ('QPACK' ) + ngrid * 2
      qlat  =    gtrpnt ('QLAT'  )
      if (lsalt) then
         csa   =    gtrpnt('CSA'   )
         sbdscr=    gtrpnt('SBDSCR')      
         sbdpar=    gtrpnt('SBDPAR')      
      else
         csa   = 1
         sbdscr= 1
         sbdpar= 1
      endif
      
      ! Put boundary exchange items

      nboun  = ip (gtipnt ( 'NBOUN' ))
      nnode  = ip (gtipnt ( 'NNODE' ))
      node   =     gtipnt ( 'NODE'  )
      nodenm =     gtcpnt ( 'NODENM')
      rv = PutBoundaries(rp(h2), rp(q2), rp(csa), rp(sbdscr), rp(sbdpar), ip(node),             &
                          cp(nodenm), ngrid, nnode, nboun)
      if (rv.ne.0) then
         call SetError('Error putting Exchange items for boundaries in Rob', 21, 1)
      endif

      if (rv==0) then

         ! Put lateral exchange items on branches

         nqlat  = ip (gtipnt ( 'NQLAT' ))
         qlatid = max(1,gtcpnt ('QLATNM'))
         qltpar =     gtrpnt ( 'QLTPAR')
         rv = PutLaterals(rp(h2), rp(qlat), rp(csa), cp(qlatid), rp(qltpar), ngrid, nqlat)
         if (rv.ne.0) then
            call SetError('Error putting Exchange items for Laterals in Rob', 22, 1)
         endif
      endif

      if (rv==0) then

         ! Put exchange items on branches

         branch =    gtipnt( 'BRANCH' )
         nbran  = ip(gtipnt( 'NBRAN' ))
         GridId =    gtcpnt( 'GRIDNM' )
         ct     =    gtrpnt( 'CPACK'  )
         waoft  =    gtrpnt( 'WAOFT'  )
         r      =    gtrpnt( 'RPACK'  )
         rv = PutBranches(rp(h2), rp(q2), rp(csa), ip(branch), rp(ct), rp(waoft), rp(r),        &
                          ngrid, nbran)
         if (rv.ne.0) then
            call SetError('Error putting Exchange items for Branches in Rob', 23, 1)
         endif
      endif

      if (rv==0) then

         nstru    = ip(gtipnt('NSTRU'))
         strtyp   =    gtipnt('STRTYP')         
         strunm   =    gtcpnt('STRUNM')         
         strpar   =    gtrpnt('STRPAR')  

         rv = PutStruct(ip(strtyp), cp(strunm), rp(strpar), nstru)

      endif

   end function ExchangeItemsToRob
      
   !  Puts the computed exchange items on boundaries in ROB
   !
   function PutBoundaries(h2, q2, csa, sbdscr, sbdpar, node, nodenm, ngrid, nnode,              &
                          nboun) result(rv)
      implicit none
      include '..\include\sobconf90.i'

      ! Parameters

      integer,       intent(in)        :: nnode, ngrid, nboun
      integer,       intent(in)        :: node(4,nnode)
      integer                          :: rv
      character*40,  intent(in)        :: Nodenm(nnode)
      real,          intent(in)        :: h2(ngrid), q2(ngrid), csa(ngrid), sbdscr(3,nboun)
      real,          intent(in)        :: sbdpar(5,nboun)

      ! Local variables

      integer                          :: inode, istat, iopt
      real                             :: con(1)

      rv = 0
      
      do inode = 1, nnode
         istat  = node(3,inode)
         if ( (node(1,inode) .eq. chbou) .or. (node(1,inode) .eq. cqbou) .or.                   &
              (node(1,inode) .eq. cqhbou).or. (node(1,inode) .eq. chqbou) ) then

            if (rv==0) rv = FRobPutReal (modelID, '', Quantity_Level,     nodenm(inode),        &
                              PROVIDING_ROLE, 1, h2(node(2,inode)) )
            if (rv==0) rv = FRobPutReal (modelID, '', Quantity_Discharge, nodenm(inode),        &  
                              PROVIDING_ROLE, 1, q2(node(2,inode)) )     
            if (lsalt .and. (rv==0) ) then
               iopt = int(sbdpar(3,istat))
               if (iopt .eq. csbusr .or. iopt .eq. csbthh) then
                  con(1) = sbdscr(3,istat)
               else 
                  con(1) = csa(node(2,inode))
               endif

               rv = FRobPutReal (modelID, '', Quantity_Salinity, nodenm(inode),                 &  
                                 PROVIDING_ROLE, 1, con )
            endif
         endif
      end do 

   end function PutBoundaries

   !  Put the computed exchange items on laterals in ROB
   !
   function PutLaterals(h2, qlat, csa, qlatid, qltpar, ngrid, nqlat) result(rv)
      implicit none
      include '..\include\sobconf90.i'

      ! Parameters

      integer,       intent(in)        :: ngrid, nqlat
      real,          intent(in)        :: qltpar(9, nqlat)
      integer                          :: rv
      character*40,  intent(in)        :: qlatid(nqlat)
      real,          intent(in)        :: h2(ngrid), qlat(nqlat), csa(ngrid)

      ! Local variables

      real                             :: h(1)
      integer                          :: i, igrid

      rv = 0

      do i = 1, nqlat

         ! loop over all laterals

         if (qltpar(4,i) .eq. cpd1gp) then
            h(1) = h2(qltpar(5,i))
         else

            ! Compute average level

            h(1) = 0.0
            do igrid = qltpar(5,i), qltpar(6,i)
               h(1) = h(1) + h2(igrid)
            enddo
            h(1) = h(1)/(qltpar(6,i)-qltpar(5,i)+1)
         endif
         if (rv==0) rv = FRobPutReal (modelID, '', Quantity_Level, QlatId(i),            &
                                      PROVIDING_ROLE, 1, h )
         if (rv==0) rv = FRobPutReal (modelID, '', Quantity_Discharge, QlatId(i),        &
                                      PROVIDING_ROLE, 1, qlat(i) )
         if (lsalt) then
            if (rv==0) rv = FRobPutReal (modelID, '', Quantity_Salinity, QlatId(i),            &
                                         PROVIDING_ROLE, 1, slat(i) )
            slat(i) = 500
            if (rv==0) rv = FRobPutReal (modelID, '', Quantity_Salinity, QlatId(i),            &
                                         ACCEPTING_ROLE, 1, slat(i) )
         endif
      enddo

   end function PutLaterals
   
   function PutBranches(h2, q2, csa, branch, ct, waoft, r, ngrid, nbran) result(rv)
      implicit none
      include '..\include\memplf90.i'

      ! Parameters

      integer,       intent(in)        :: ngrid, nbran
      integer,       intent(in)        :: branch(4, nbran)
      real,          intent(in)        :: ct(ngrid,4), waoft(ngrid,18), r(ngrid, 4)
      real,          intent(in)        :: h2(ngrid), q2(ngrid), csa(ngrid)
      integer                          :: rv

      ! Local variables

      integer                          :: ibran, lbrnam, numpnt, ixpar, i, i1, i2
      real, allocatable                :: help(:)
      real                             :: sorpar, froude, velocity, depth
      integer                          :: gtrpnt
      real                             :: g
      character*40                     :: branam

      rv = 0

      allocate(help(ngrid))
      ixpar  = gtrpnt ( 'FLWPAR' )
      g      = sorpar ( rp(ixpar), 1 )

      do ibran = 1, nbran
         call getbrn(ibran, branam, lbrnam)

         i1 = branch(3, ibran)
         i2 = branch(4, ibran)
         numpnt = i2 - i1 + 1

         if (rv==0) rv = FRobPutReal(modelID,'',Quantity_Level,      branam,PROVIDING_ROLE,numpnt,h2(i1:i2))
         if (rv==0) rv = FRobPutReal(modelID,'',Quantity_Discharge,  branam,PROVIDING_ROLE,numpnt,q2(i1:i2))
         if (rv==0) rv = FRobPutReal(modelID,'',Quantity_Chezy,      branam,PROVIDING_ROLE,numpnt,ct(i1:i2,1))
         if (rv==0) rv = FRobPutReal(modelID,'',Quantity_Area,       branam,PROVIDING_ROLE,numpnt,waoft(i1:i2,3))
         if (rv==0) rv = FRobPutReal(modelID,'',Quantity_Width,      branam,PROVIDING_ROLE,numpnt,waoft(i1:i2,1))
         do i = i1, i2
            help(i) = depth(waoft(i,3), waoft(i,1))
         enddo
         if (rv==0) rv = FRobPutReal(modelID,'',Quantity_Depth,      branam,PROVIDING_ROLE,numpnt,help(i1:i2))
         if (rv==0) rv = FRobPutReal(modelID,'',Quantity_HyRad,      branam,PROVIDING_ROLE,numpnt,r(i1:i2,1))
         do i = i1, i2
            help(i) = velocity(q2(i), waoft(i,1))
         enddo
         if (rv==0) rv = FRobPutReal(modelID,'',Quantity_Vel,        branam,PROVIDING_ROLE,numpnt,help(i1:i2))
         do i = i1, i2
            help(i) = froude(q2(i), waoft(i,3), waoft(i,1), g)
         enddo
         if (rv==0) rv = FRobPutReal(modelID,'',Quantity_Froude,     branam,PROVIDING_ROLE,numpnt,help(i1:i2))

         if (lsalt) then
            if (rv==0) rv = FRobPutReal(modelID,'',Quantity_Salinity,branam,PROVIDING_ROLE,numpnt,csa(i1:i2))
         endif
      end do

      deallocate(help)

   end function PutBranches

   function PutStruct(strtyp, strunm, strpar, nstru) result(rv)

      include '..\include\sobconf90.i'
      integer nrcoefs, dmstrpar
      common /dimens/nrcoefs,dmstrpar

      integer                          :: rv
      integer,       intent(in)        :: nstru
      integer,       intent(in)        :: strtyp(10, nstru)
      real,          intent(in)        :: strpar(dmstrpar, nstru)
      character*40,  intent(in)        :: strunm(nstru)

      integer                          :: i
      rv = 0

      do i = 1, nstru
         if ( (strtyp(1,i).eq.csweir) .or. (strtyp(1,i).eq.caweir) ) then
            if (rv==0) rv = FRobPutReal(modelID,'',Quantity_Crest,      strunm(i),PROVIDING_ROLE, 1,strpar(1,i))
            if (rv==0) rv = FRobPutReal(modelID,'',Quantity_Width,      strunm(i),PROVIDING_ROLE, 1,strpar(2,i))
         endif
      enddo

   end function PutStruct

   !  Gets the boundary condions from ROB and puts them in the
   !  SOBEK boundaries (HSTAT and QSTAT)
   !
   function GetBoundaries(hstat, qstat, node, nodenm, nhstat, nqstat, nnode) result(rv)
      implicit none
      include '..\include\sobconf90.i'

      ! Parameters

      integer,       intent(in)        :: nnode, nqstat, nhstat
      integer,       intent(in)        :: node(4,nnode)
      integer                          :: rv
      character*40,  intent(in)        :: Nodenm(nnode)
      real,          intent(inout)     :: hstat(nhstat), qstat(nqstat)

      ! Local variables

      integer                          :: iboun, istat
      real(8)                          :: Values(1)

      rv = 0

      if (InitDone) then
         do iboun = 1, nnode
            istat  = node(3,iboun)
            if (node(1,iboun) .eq. chbou) then
               if (rv==0) rv = FRobGetDouble (modelID, '', Quantity_Level, nodenm(iboun),        &
                                   ACCEPTING_ROLE, 1, Values )
               if (Values(1).ne.MISSING_VALUE) then
                  hstat(istat) = Values(1)
               endif
            else if (node(1,iboun) .eq. cqbou) then
               if (rv==0) rv = FRobGetDouble (modelID, '', Quantity_Discharge, nodenm(iboun),        &  
                                   ACCEPTING_ROLE, 1, Values )     
               if (Values(1).ne.MISSING_VALUE) then
                  qstat(istat) = Values(1)
               endif
            endif
         end do 
      endif

   end function GetBoundaries

   !  Puts the computed exchange items for salt on boundaries in ROB
   !
   subroutine GetBoundariesSalt(nodenm, iboun, nboun, con) 
      implicit none

      ! parameters
      integer,       intent(in)        :: iboun, nboun
      character*40,  intent(in)        :: nodenm(nboun)
      real,          intent(inout)     :: con

      ! Local variables
      real(8)                          :: Values(1)
      integer                          :: rv


      rv = FRobGetDouble (modelID, '', Quantity_Salinity, nodenm(iboun),        &  
                          ACCEPTING_ROLE, 1, Values )
      if (Values(1).ne.MISSING_VALUE) then
         con = Values(1)
      endif

   end subroutine GetBoundariesSalt


   !  Gets the Laterals from ROB and puts them in the
   !  SOBEK lateral conditions (QLAT)
   !
   function GetLaterals(qlat, qlatid, qltpar, nqlat) result(rv)
      implicit none
      include '..\include\sobconf90.i'

      ! Parameters

      integer,       intent(in)        :: nqlat
      real,          intent(in)        :: qltpar(9, nqlat)
      integer                          :: rv
      character*40,  intent(in)        :: qlatid(nqlat)
      real,          intent(inout)     :: qlat(nqlat)

      ! Local variables

      integer                          :: i
      real(8)                          :: Values(1)

      rv = 0

      if (InitDone) then

         do i = 1, nqlat

            ! loop over all laterals

            if (rv==0) rv = FRobGetDouble (modelID, '', Quantity_Discharge, QlatId(i),        &
                                ACCEPTING_ROLE, 1, Values )
            if (Values(1).ne.MISSING_VALUE) then
               qlat(i) = Values(1)
            endif
         enddo
      endif

   end function GetLaterals
   
   !  Puts the computed exchange items for salt on laterals in ROB
   !
   subroutine GetLateralsSalt(qlatid, qstat, nqlat, cstat) 
      implicit none

      ! parameters
      integer,       intent(in)        :: nqlat, qstat
      character*40,  intent(in)        :: qlatid(nqlat)
      real,          intent(inout)     :: cstat

      ! Local variables
      real(8)                          :: Values(1)
      integer                          :: rv

      rv = FRobGetDouble (modelID, '', Quantity_Salinity, QlatId(qstat), ACCEPTING_ROLE, 1, Values )

      if (Values(1).ne.MISSING_VALUE) then
         cstat = Values(1)
      endif

   end subroutine GetLateralsSalt

   subroutine GetStruct(strtyp, strunm, strpar, nstru)
      implicit none
      include '..\include\sobconf90.i'
      integer nrcoefs, dmstrpar
      common /dimens/nrcoefs,dmstrpar

      ! parameters
      integer,       intent(in)  :: nstru         
      integer,       intent(in)  :: strtyp(10,nstru) 
      character*40,  intent(in)  :: strunm(nstru)
      real,          intent(out) :: strpar(dmstrpar, nstru)

      integer                    :: i,rv
      real(8)                    :: Values(1)
      
      do i = 1, nstru
         if ( (strtyp(1,i).eq.csweir) .or. (strtyp(1,i).eq.caweir) ) then

            rv = FRobGetDouble (modelID, '', Quantity_Crest, strunm(i), ACCEPTING_ROLE, 1, Values )

            if (Values(1).ne.MISSING_VALUE) then
               strpar(1,i) = Values(1)
            endif

            rv = FRobGetDouble (modelID, '', Quantity_Width, strunm(i), ACCEPTING_ROLE, 1, Values )

            if (Values(1).ne.MISSING_VALUE) then
               strpar(2,i) = Values(1)
            endif

         endif
      enddo
   end subroutine GetStruct


   !  Sets the lOpenMI flag. This flag indicates whether SOBEK is
   !  run in OpenMI or not.
   !
   function OpenMIactive()
      logical           :: OpenMIactive
      OpenMIactive = lOpenMI
   end function OpenMIactive

   !  Sets an error
   !
   subroutine SetError(Message, iCode, iType)

      character(len=*), intent(in)        :: Message        ! (Fout) tekst
      integer, intent(in)                 :: iCode, iType

      Msg%Text    = Message
      Msg%Code    = iCode
      Msg%MsgType = iType
      
   end subroutine Seterror



! @ToJulian
!
! Conversion of Gregorian calendar date to Julian date for years AD 1801-2099
! Where
! YYYY is the year (1801 <= YYYY <= 2099)
! MM is the month (1 <= MM <= 12)
! DD is the day of the month (1 <= DD <= 31)
! HH hour of day (0-23)
! MM minute (0-60)
! SS second (0-60)
!
! @Result : huge(JD) in case of errors (invalid date or date outside valid range) else JD
!
! @Param  TO       : (valid) date occurence
! @param  Modified : when present, indicates conversioj to modified Julian
!
function ToJulian(YYYY,MM,DD,HH,MN,SS,Modified) result(JD)

  ! source : http://aa.usno.navy.mil/data/docs/JulianDate.html
  !
  ! Conversion of Gregorian calendar date to Julian date for years AD 1801-2099 can be carried out with the 
  ! following formula:
  ! JD = 367K -  <(7(K+<(M+9)/12>))/4>  +  <(275M)/9>  + I + 1721013.5 + UT/24 - 0.5sign(100K+M-190002.5) + 0.5
  ! where K is the year (1801 <= K <= 2099), M is the month (1 <= M <= 12), I is the day of the month (1 <= I <= 31),
  ! and UT is the universal time in hours ("<=" means "less than or equal to"). The last two terms in the formula
  ! add up to zero for all dates after 1900 February 28, so these two terms can be omitted for subsequent dates. 
  ! This formula makes use of the sign and truncation functions described below:
  !
  ! The last two terms in the formula add up to zero for all dates after 1900 February 28,
  !
  ! The formula given above was taken from the U.S. Naval Observatory's no-longer-published Almanac for Computers 
  ! for year 1990.

  real,parameter              :: UT_shift = 0.D+0  ! Afwijking t.o.v. UT in uren
  integer,intent(in),optional :: Modified
  integer,intent(in)          :: YYYY,MM,DD,HH,MN,SS
  real(kind=8)                :: UT  ! UT is the universal time in hours
  real(kind=8)                :: JD, x5
  real(kind=8),parameter      :: julian_2_modified_julian_shift = - 2400000.5D+0

  JD = huge(JD)

  if(YYYY >=1801 .and. YYYY < 2099) then
    UT = real(HH,8) + real(MN,8)/60. + real(SS,8)/3600.

    !       367K                       <(7(K+<(M+9)/12>))/4>                                 <(275M)/9>                         I + 1721013.5+ UT/24
    JD = 367.D+0 * YYYY   -  int(7.D+0 * (YYYY + int( (MM+9.D+0)/12.D+0) )/4.D+0)   +  int( (275.D+0 * MM)/9.D+0)  +  real(DD,8) + 1721013.5D+0 + UT/24.

    ! Date before 1900,march 01
    if(JD < 2415079.50000000D+0) then
      ! 0.5sign(100K+M-190002.5) + 0.5
      x5 = 0.5D+0 * sign(1.D+0,100.D+0*YYYY+MM-190002.5D+0) + 0.5D+0
      JD = JD - x5
    endif

    if(Present(Modified)) JD = JD + julian_2_modified_julian_shift
  endif

end function ToJulian
   
end module
