!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------

    Module LocationDataModule

    Use ParameterModule
    Use DH_Alloc

    Implicit None

! Constants

! Variables

! *** Delft_3B Node data
!
! *** ID_D3B ( ) = unieke knoop id
! *** NAME_D3B ( ) = naam
!
! *** ID_3B  (  ) = knoop id in HIS file        (character)
! *** BBBLOC (  ) = conversie array van HIS file id naar LOC file id
! *** IDPAR3 (  ) = id van parameters           (character)
! *** RES3B  (  ) = resultaten van 3B per id en parameter (real)
! ***                 voor huidige tijdstap
! ***               parameter 1 = waterstand
! *** ALRS3B (  ) = resultaten van 3B per id en parameter en tijdstap
! ***   (i3B, ipar, it) i3B =lokatie
! ***                   ipar=parameter
! ***                   it  =tijdstap  (ntims=huidige, ntims-1=vorige etc.)
!

      CHARACTER(len=CharIdLength), Pointer, Save :: ID_D3B (:), ID_3B(:), NAME_D3B(:)
      CHARACTER(len=CharIdLength), Pointer, Save :: IDPAR3 (:)
      Double Precision, Pointer, Save				 :: RES3B  (:,:), ALRS3B(:,:,:)
      Integer, Pointer, Save                     :: BBBLOC (:)


! *** Node data
! *** ID_SBK (  ) = Sobek_id                    (character)
! *** NAME_SBK (  ) = Sobek_name                (character)
! *** ID_SBF (  ) = Sobek_id from HIS file      (character)
! *** ID_SBR (  ) = branch carrier id           (character)
! ***                 or node/reach/struct id
! *** ID_SLC (  ) =    >= 0 ==> location on branch        (real),
!                        -1 ==> location is node
!                        -2 ==> location is reach
!                        -3 ==> location is struct
!                        -4 ==> location is measurement location
! *** IDPARS (  ) = id van parameters           (character)
! *** SBKLOC (  ) = conversie array van HIS file id naar LOC file id
! *** RESSBK (  ) = resultaten van sobek per id en parameter (real)
! ***                 voor huidige tijdstap
! ***               parameter 1 = waterstand
! ***               parameter 2 = waterdiepte
! ***               parameter 3 = debiet
! ***               parameter 4 = diepte
! *** ALRSBK (  ) = resultaten van sobek per id en parameter en tijdstap
! ***   (isbk, ipar, it) isbk=lokatie
! ***                    ipar=parameter
! ***                    it  =tijdstap  (ntims=huidige, ntims-1=vorige etc.)

      CHARACTER(len=CharIdLength), Pointer, Save :: ID_SBF (:), ID_SBK (:), ID_SBR (:), NAME_SBK(:)
      CHARACTER(len=CharIdLength), Pointer, Save :: IDPARS (:)
      Double Precision, Pointer, Save            :: ID_SLC (:)
      Double Precision, Pointer, Save            :: RESSBK (:,:), ALRSBK(:,:,:)
      Integer, Pointer, Save                     :: SbkLOC (:)


! *** Results from HIS file dummy array: must be a real array and not double precision
      Real, Pointer, Save                        :: RESRead(:,:)

      CHARACTER(len=CharIdLength), Pointer, Save :: ID_D3DHIS(:), ID_D3D (:), NAME_D3D(:)
      CHARACTER(len=CharIdLength), Pointer, Save :: IDPAR3D (:)
      Double Precision, Pointer, Save				 :: RESD3D (:,:), ALRS3D(:,:,:)
      Integer, Pointer, Save                     ::  D3DLOC (:)


! *** Precipitation data
!
! *** ID_PRE ( )  = unieke id
! *** NAME_PRE ( )= naam
! *** ID_PR  (  ) = Sobek_id from HIS file      (character)
! *** IDPARP (  ) = id van parameters           (character)
! *** RESPRE (  ) = resultaten van Precipitation per id en parameter (real)
! ***                 voor huidige tijdstap
! *** ALRSPR (  ) = resultaten van Precipitation per id en parameter en tijdstap
! ***   (ipre, ipar, it) ipre=lokatie
! ***                    ipar=parameter
! ***                    it  =tijdstap  (ntims=huidige, ntims-1=vorige etc.)


      CHARACTER(len=CharIdLength), Pointer, Save :: ID_PRE (:), ID_PR(:), NAME_PRE(:)
      CHARACTER(len=CharIdLength), Pointer, Save :: IDPARP (:)
      Double Precision, Pointer, Save            :: RESPRE (:,:), ALRSPR(:,:,:)


! *** External data   NEXT (wind) + NEXTH (HIS file) locations
!
! *** ID_EXT ( ) = unieke id
! *** NAME_EXT ( )= naam
! *** ID_EX  (  ) = Sobek_id from HIS file      (character)
! *** IDPARE (  ) = id van parameters           (character)
! *** HisFileExt (  ) = HIS file name
! *** HisParExt (  ) = HIS parameter id
! *** HisLocExt (  ) = HIS location id
! *** HisDataSet(  ) =  ( ,1) geeft His Dataset index for index IExtH
! ***                      2) geeft His parameter index
! ***                      3) geeft His lokatie index
! *** HisDataSetFile (  ) =  geeft His Dataset filename
! *** HisDataSetTimes  =  ( , ) geeft His Dataset Julian timesteps for all HIS files
! *** HisDataSetNParLocTimes =  ( , ) geeft His Dataset aantal parameters, locaties,tijdstappen
! *** FirstTimeDataSet =  ( ) geeft laatste gebruikte index voor de tijdstappen in de HIS file
! *** RESEXT (  ) = resultaten van Extern per id en parameter (real)
! ***                 voor huidige tijdstap
! *** ALRSEX (  ) = resultaten van Extern per id en parameter en tijdstap
! ***   (iext, ipar, it) iext=lokatie
! ***                    ipar=parameter
! ***                    it  =tijdstap  (ntims=huidige, ntims-1=vorige etc.)


      CHARACTER(len=CharIdLength), Pointer, Save :: ID_EXT (:), ID_EX(:), NAME_EXT(:)
      CHARACTER(len=CharIdLength), Pointer, Save :: IDPARE (:)
      CHARACTER(len=CharIdLength), Pointer, Save :: HisFileExt (:), HisParExt(:), HisLocExt(:)
      CHARACTER(len=CharIdLength), Pointer, Save :: HisDataSetFile (:)
      Integer, Pointer, Save                     :: HisDataSet (:,:), FirstTimeDataSet(:), HisDataSetNParLocTimes(:,:)
      Double Precision, Pointer, Save            :: RESEXT (:,:), ALRSEX(:,:,:), HisDataSetTimes(:,:)


! *** Bui data
!
! *** EVTIMS      = duur van de buien in bui-tijdstappen
! *** NRSRAI      = regenval tijdstapgrootte in seconden
! *** BuiDat      = array met buidata voor alle events, stations en tijdstappen
! *** EvStrt      = start data buien     jaar/maand/dag/uur/minuut/seconde
! *** EvDura      = duur van de buien in dagen/uren/minuten/seconden
! *** NameStat    = namen rainfall stations
! *** Loc2Stat    = conversie van lokaties naar stations

      INTEGER      NRSRAI

      Double Precision, Pointer, Save            :: BUIDAT (:,:,:)
      INTEGER, Pointer, Save                     :: EVSTRT (:,:), EVDURA(:,:), EVTIMS(:)
      INTEGER, Pointer, Save                     :: Loc2Stat(:)
      CHARACTER(len=CharIdLength), Pointer, Save :: NAMESTAT(:)


! *** Wind data
!
! *** CONSWV= logical, constant wind velocity
! *** VALCWV= value constant wind velocity
! *** CONSWD= logical, constant wind direction
! *** VALCWD= value constant wind direction
! *** NTIMW = aantal tijdstappen in wind file
! *** WNDDIR= wind direction array
! *** WNDVEL= wind velocity array
! *** WindInterpVel= true means linear interpolation velocity ; false means block data (default=block data)
! *** WindInterpDir= true means linear interpolation direction; false means block data (default=block data)
! *** WindTable  = WindTable Nr in case of use of the NewTablesModule
! ***                 (iwind,1) velocity table
! ***                 (iwind,2) direction table

      INTEGER                                    :: NTIMW, NTIMWD, NTIMWV
      LOGICAL, Pointer, Save                     :: CONSWD(:), CONSWV(:), WindInterpVel(:), WindInterpDir(:)
      Double Precision, Pointer, Save				 :: VALCWD(:), VALCWV(:)

      Double Precision, Pointer, Save            :: WNDDIR (:,:,:)
      Double Precision, Pointer, Save            :: WNDVEL (:,:,:)
      INTEGER, Pointer, Save                     :: WindLoc2Stat(:), WindTable(:,:)
      CHARACTER(len=CharIdLength), Pointer, Save :: WindNameStat(:)
! indx 1 = lokatie
! indx 2 = volgnr. tijd
! indx 3 = tijd + value
!  1=tijd als timnew (10000*year+100*month+1*day+.01*hour etc)
!  2= value
      Integer, Pointer, Save ::  SearchFromIndex(:,:), SearchToIndex(:,:)
!  Search From indices en search to indices for wind arrays
!  :,1 voor wind direction, I,2 voor wind velocity


! *** WQ data
! *** ID_SWQ (  ) = Sobek_id                    (character)
! *** ID_WQF (  ) = Sobek_id from HIS file      (character)
! *** IDPARQ (  ) = id van parameters           (character)
! *** WQLOC  (  ) = conversie array van HIS file id naar LOC file id
! *** RESWQ  (  ) = resultaten van sobek per id en parameter (real)
! ***                 voor huidige tijdstap
! ***               parameter 1,2,3 etc = afh. parameters in WQ HIS file
! *** ALRSWQ (  ) = resultaten van sobek per id en parameter en tijdstap
! ***   (isobwq, ipar, it) isobwq=lokatie
! ***                      ipar=parameter
! ***                      it  =tijdstap  (ntims=huidige, ntims-1=vorige etc.)

      CHARACTER(len=CharIdLength), Pointer, Save :: ID_WQF (:), ID_SWQ (:)
      CHARACTER(len=CharIdLength), Pointer, Save :: IDPARQ (:)
      Double Precision, Pointer, Save            :: RESWQ (:,:), ALRSWQ(:,:,:)
      Integer, Pointer, Save                     :: WQLOC (:)

!     D3DFlow Data
!     ------------
! BARACT      I*4  NSLUV           Array with active row of
!                                  barrier table
! BARHJU     DP*4  NSLUV           Array with Julian dat/time for
!                                  actual high for current table period
! BARHVL     DP*4  NSLUV           Array with barrier value for
!                                  actual high of current table period
! BARINT      L*4  NSLUV           Logical for interpolation method,
!                                  True is interpolate, false is block
! BARJUL     DP*4  NSLUV*......    Array with Julian dat/time as result
!                                  from DP function JULIAN
! BARLJU     DP*4  NSLUV           Array with Julian dat/time for
!                                  actual low of current table period
! BARLVL     DP*4  NSLUV           Array with barrier value for
!                                  actual low of current table period
! BARMAX      I*4  NSLUV           Array with number of rows of
!                                  barrier table
!                                  actual low of current table period
! BARVAL      R*4  NSLUV*......    Array with values from data file
!

      Integer,          Pointer, Save :: BARACT(:)
      Double Precision, Pointer, Save :: BARHJU(:)
      Real,             Pointer, Save :: BARHVL(:)
      Logical,          Pointer, Save :: BARINT(:)
      Double Precision, Pointer, Save :: BARJUL(:,:)
      Double Precision, Pointer, Save :: BARLJU(:)
      Real,             Pointer, Save :: BARLVL(:)
      Integer,          Pointer, Save :: BARMAX(:)
      Real,             Pointer, Save :: BARVAL(:,:)


    Contains

      Function ALLOC3DBAR(nsluv, maxtab, outuni)  result(RetVal)
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    ZWS
!
!             Module: SUBROUTINE ALLOC3DBAR
!           Function: Allocates the array's for the barriers
!        Method used:
!               Date: 01-11-2001
!         Programmer: J. Zeekant
!-----------------------------------------------------------------------
!   Calling routines:              RDBAR
!-----------------------------------------------------------------------
!   Called  routines:              ERRMSG
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! MAXTAB  I   I*4                  Length of the longest table
! NSLUV   I   I*4                  Number of U- and V-Barriers
! OUTUNI  I   I*4                  Unit of error output.
!-----------------------------------------------------------------------
!    Common variables:
!    -----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BARACT      I*4  NSLUV           Array with active row of
!                                  barrier table
! BARHJU     DP*4  NSLUV           Array with Julian dat/time for
!                                  actual high for current table period
! BARHVL     DP*4  NSLUV           Array with barrier value for
!                                  actual high of current table period
! BARINT      L*4  NSLUV           Logical for interpolation method,
!                                  True is interpolate, false is block
! BARJUL     DP*4  NSLUV*......    Array with Julian dat/time as result
!                                  from DP function JULIAN
! BARLJU     DP*4  NSLUV           Array with Julian dat/time for
!                                  actual low of current table period
! BARLVL     DP*4  NSLUV           Array with barrier value for
!                                  actual low of current table period
! BARMAX      I*4  NSLUV           Array with number of rows of
!                                  barrier table
! BARVAL      R*4  NSLUV*......    Array with values from data file
!-----------------------------------------------------------------------
! RetVal      Integer              Return value (0=ok, <>0 is error)
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! I           I*4                  Loop counter
!-----------------------------------------------------------------------

      integer :: RetVal

      integer maxtab, nsluv, outuni

      Logical Success

      RetVal = 0
!
!     Allocate the array's for the barriers
!

      success = DH_AllocInit(nsluv, BarAct,0)
!     allocate (baract(nsluv), STAT=RetVal)
!---- Initialise this array to 0
!     baract = 0

      success = success .and. DH_AllocInit(nsluv, Barhju,-1D0)
!     allocate (barhju(nsluv), STAT=RetVal)
!---- Initialise this array to -1
!     barhju = -1.0

      success = success .and. DH_AllocInit(nsluv, Barhvl,0.)
!     allocate (barhvl(nsluv), STAT=RetVal)

      success = success .and. DH_AllocInit(nsluv, Barint,.false.)
!     allocate (barint(nsluv), STAT=allerr)

      success = success .and. DH_AllocInit(nsluv,maxtab,barjul,-1D0)
!     allocate (barjul(nsluv, maxtab), STAT=allerr)
!---- Initialise this array to -1
!     barjul = -1.0

      success = success .and. DH_AllocInit(nsluv, Barlju,-1D0)
!     allocate (barlju(nsluv), STAT=allerr)
!---- Initialise this array to -1
!     barlju = -1.0

      success = success .and. DH_AllocInit(nsluv, Barlvl,0.)
!     allocate (barlvl(nsluv), STAT=allerr)
!     was originally not initialized

      success = success .and. DH_AllocInit(nsluv, Barmax,0)
!     allocate (barmax(nsluv), STAT=allerr)
!     was originally not initialized

      success = success .and. DH_AllocInit(nsluv,maxtab, Barval,0.)
!     allocate (barval(nsluv, maxtab), STAT=allerr)
!     was originally not initialized
      if (.not. success) then
        call ERRMSG (946, 0, 'ALLOC3DBAR', 'Allocation error 3DBarrier arrays', outuni)
        RetVal = 946
      endif

      Return
      End Function ALLOC3DBAR

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      Function AllocLocationArrays (Iout1) result(RetVal)

      Integer RetVal

      Integer Iout1
      Logical Success

      RetVal = 0

! RR data
      success = DH_AllocInit(ND3B, ID_D3B,'')
      success = success .and. DH_AllocInit(ND3B, Name_D3B,'')
      success = success .and. DH_AllocInit(NLocHis, ID_3B,'')
      success = success .and. DH_AllocInit(NLocHis, BBBLoc,0)
      success = success .and. DH_AllocInit(NPar3, IdPar3,'')
      success = success .and. DH_AllocInit(ND3B, NPar3, Res3B, 0D0)
      success = success .and. DH_AllocInit(ND3B, NPar3, Ntims, AlRs3B,0D0)
      If (.not. success) then
         Call ErrMsg (929, 1, ' AllocLocationArrays', ' ', IOUT1)
         RetVal = 929
         Return
      Endif


! Sobek CF data
      success = DH_AllocInit(NSbk, ID_Sbk,' ')
      success = success .and. DH_AllocInit(NSbk, NAME_SBK,' ')
      success = success .and. DH_AllocInit(NLocHis, ID_SBF,' ')
      success = success .and. DH_AllocInit(NSbk, ID_SBR,' ')
      success = success .and. DH_AllocInit(NSbk, ID_SLC,0D0)
      success = success .and. DH_AllocInit(NLocHis, SbkLoc,0)
      success = success .and. DH_AllocInit(NParS, IdParS,' ')
      success = success .and. DH_AllocInit(NSBK, NParS, ResSbk, 0D0)
      success = success .and. DH_AllocInit(NSBK, NParS, Ntims, AlRSBK,0D0)
      If (.not. success) then
         Call ErrMsg (929, 2, ' AllocLocationArrays', ' ', IOUT1)
         RetVal = 929
         Return
      Endif
!    ID_SBF = ' '
!    ID_SBK = ' '
!    ID_SBR = ' '
!    IDPARS = ' '


! Altijd NPars=NParQ, want aantal parameters WQ is veel groter dan aantal parameters Sobek (NPars=16) en dan # parameters in 3B His file (NPar3=2)
! Make sure the next declaration is consistent with the ResRead declaration in the routine RDHIS
      success = DH_AllocInit(NLocHis, NParQ, ResRead, 0E0)
!     Allocate  (   RESRead (NLocHis, 500), STAT=Allocation_Error )

! Precipitation data
      success = success .and. DH_AllocInit(NPRE, ID_PR,'')
      success = success .and. DH_AllocInit(NPRE, ID_PRE,'')
      success = success .and. DH_AllocInit(NPRE, NAME_PRE,'')
      success = success .and. DH_AllocInit(NParP, IdParP,'')
      success = success .and. DH_AllocInit(NPRE, NParP, ResPre, 0D0)
      success = success .and. DH_AllocInit(NPRE, NParP, Ntims, AlRSPR,0D0)
      If (.not. success) then
         Call ErrMsg (929, 3, ' AllocLocationArrays', ' ', IOUT1)
         RetVal = 929
         Return
      Endif

! External/wind data
      success = DH_AllocInit(NEXT+NEXTH, ID_EXT,' ')
      success = success .and. DH_AllocInit(NEXT+NEXTH, NAME_EXT,' ')
      success = success .and. DH_AllocInit(NEXT+NEXTH, ID_EX,' ')
      success = success .and. DH_AllocInit(NParE, IdParE,' ')
      success = success .and. DH_AllocInit(NEXT+NEXTH, NParE, ResExt, 0D0)
      success = success .and. DH_AllocInit(NEXT+NEXTH, NParE, NTims, AlRSEX,0D0)
      success = success .and. DH_AllocInit(NEXTH, HisFileExt,' ')
      success = success .and. DH_AllocInit(NEXTH, HisParExt,' ')
      success = success .and. DH_AllocInit(NEXTH, HisLocExt,' ')
      success = success .and. DH_AllocInit(NEXTH, 3, HisDataSet,0)
      success = success .and. DH_AllocInit(NEXTH, HisDataSetFile,' ')
      success = success .and. DH_AllocInit(NEXTH, FirstTimeDataSet,0)
      success = success .and. DH_AllocInit(NEXTH, 3, HisDataSetNParLocTimes,0)
      success = success .and. DH_AllocInit(NEXTH, NTimHis, HisDataSetTimes,0D0)
      If (.not. success)  then
         Call ErrMsg (929, 4, ' AllocLocationArrays', ' ', IOUT1)
         RetVal = 929
         Return
      Endif

      success = success .and. DH_AllocInit(NEVNT, NSTAT, NTIM, BUIDAT,0D0)
      success = success .and. DH_AllocInit(NEVNT, 6, EVSTRT,0)
      success = success .and. DH_AllocInit(NEVNT, 4, EVDURA,0)
      success = success .and. DH_AllocInit(NEVNT, EVTIMS,0)
      success = success .and. DH_AllocInit(NPRE, Loc2Stat,0)
      success = success .and. DH_AllocInit(NSTAT, NameStat,' ')

      success = success .and. DH_AllocInit(NWind, ConsWD,.false.)
      success = success .and. DH_AllocInit(NWind, ConsWV,.false.)
      success = success .and. DH_AllocInit(NWind, WindInterpVel,.false.)
      success = success .and. DH_AllocInit(NWind, WindInterpDir,.false.)
      success = success .and. DH_AllocInit(NWind, ValCWD, 0D0)
      success = success .and. DH_AllocInit(NWind, ValCWV, 0D0)
      success = success .and. DH_AllocInit(NWind, 2, WindTable, 0)
      success = success .and. DH_AllocInit(NExt, WindLoc2Stat, 0)
      success = success .and. DH_AllocInit(NWind, WindNameStat, ' ')
      If (.not. success)  then
         Call ErrMsg (929, 5, ' AllocLocationArrays', ' ', IOUT1)
         RetVal = 929
         Return
      Endif


! WQ data
      success = success .and. DH_AllocInit(NLocHis, ID_WQF, ' ')
      success = success .and. DH_AllocInit(NLocHis, ID_SWQ, ' ')
      success = success .and. DH_AllocInit(NPARQ  , IDPARQ, ' ')
      success = success .and. DH_AllocInit(NLocHis, WQLOC , 0)
      success = success .and. DH_AllocInit(NSWQ, NParQ, ResWq, 0D0)
      success = success .and. DH_AllocInit(NSWQ, NParQ, NTims, AlRSWQ,0D0)
      If (.not. success)  then
         Call ErrMsg (929, 6, ' AllocLocationArrays', ' ', IOUT1)
         RetVal = 929
         Return
      Endif

! D3DFlow data
      success = DH_AllocInit(N3D, ID_D3D,'')
      success = success .and. DH_AllocInit(N3D, Name_D3D,'')
      success = success .and. DH_AllocInit(NLocHis, ID_D3DHIS,'')
      success = success .and. DH_AllocInit(NLocHis, D3DLoc,0)
      success = success .and. DH_AllocInit(NPar3D, IdPar3D,'')
      success = success .and. DH_AllocInit(N3D, NPar3D, ResD3D, 0D0)
      success = success .and. DH_AllocInit(N3D, NPar3D, Ntims, AlRs3D,0D0)
      If (.not. success) then
         Call ErrMsg (929, 1, ' AllocLocationArrays', ' ', IOUT1)
         RetVal = 929
         Return
      Endif


      Return
      End Function AllocLocationArrays



      Function AllocWindArrays (Iout1)  result(RetVal)

      integer :: RetVal

      logical  success
      Integer  Iout1

      RetVal = 0

      success = DH_AllocInit(NLocHis, ID_WQF, ' ')
      success = success .and. DH_AllocInit(NWind, NTimW, 2, WndDir, 0D0)
      success = success .and. DH_AllocInit(NWind, NTimW, 2, WndVel, 0D0)
      success = success .and. DH_AllocInit(NWind, 2, SearchFromIndex, 0)
      success = success .and. DH_AllocInit(NWind, 2, SearchToIndex, 0)

!     Allocate ( WNDDIR (NWIND, NTIMW, 2), STAT=Allocation_Error )
!     Allocate ( WNDVEL (NWIND, NTIMW, 2), STAT=Allocation_Error )
!     Allocate ( SearchFromIndex(NWIND,2), STAT=Allocation_Error )
!     Allocate ( SearchToIndex  (NWIND,2), STAT=Allocation_Error )

      If (.not. success)  then
         Call ErrMsg (929, 5, ' AllocLocationArrays', ' ', IOUT1)
         RetVal = 929
         Return
      Endif

      Return
      End Function AllocWindArrays



      Function RDL_SB (IDEBUG, IN, IOUT1) result(RetVal)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RTC version 1.0.                     Date: June 1997
! *********************************************************************
! *** Last update: June   1997       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Inlezen lokatie file: Sobek_id (incl. 1D2D), en lokatie
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************

      Use NewTables
      Use ParseToken
      Use ReadLib

      Integer RetVal

      INTEGER    NHLP
      PARAMETER (NHLP=10)
!
      LOGICAL      ENDFIL  !ALLOW, FOUND
!      INTEGER      IDUM(NHLP)
      REAL          RLOC  ! RDUM(NHLP),
!      Double Precision Ddum(NHLP)
      CHARACTER(len=CharIdLength) :: ID, Name, Branch, Node, Reach, Struct
      CHARACTER(len=CharIdLength) :: MeasLoc, HistoryLoc, DamBreakLoc !CDUM(NHLP)
      CHARACTER*999               :: STRING
      INTEGER                     :: IDEBUG, IOUT1, IN, ISBK, IECODE
!
! Additional variables for NewTables and ParseToken
      Character*4    SearchString
      Integer        ScanToTk, IStart, ReturnIndx, NumberOfTokens
      Logical        ParseTokenReadCaseSensitive, ParseTokenSearchCaseSensitive, ReadError, success
      Type (TokenArray) RecordData

      RetVal = 0

      IStart   = 1     ! Scan from token 1
      ScanToTk = 999   ! Scan up to Token 999
      ParseTokenReadCaseSensitive = .true.      ! no conversion to upper case; ParseToken fills array in original case
      ParseTokenSearchCaseSensitive = .false.   ! find keywords case-insensitive
! end of additional variables ParseToken


      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' RDL_SB')
!      ALLOW = .FALSE.
!
! *********************************************************************
! *** skip header of file
! *********************************************************************
!
      CALL SKPCOM (IN, ENDFIL,'RTC')
!
! *********************************************************************
! *** read data
! *** format:     SBKO id 'id_id' nm 'name' bi 'branchid' lc 0.0 sbko
! ***
! *** additions for coupling with Matlab:        (March 1998, T2233)
! ***
! ***>=0  SBKO id 'branch_loc_id' nm 'name' bi 'branchid' lc 0.0 sbko
! ***-1   SBKO id 'meetid' nm 'name' in 'Node_id' sbko
! ***-2   SBKO id 'meetid' nm 'name' ir 'Reachsegment_id' sbko
! ***-3   SBKO id 'meetid' nm 'name' is 'Structure_id' sbko
! ***-4   SBKO id 'meetid' nm 'name' ml 'Measurement location id' sbko
! ***-5   SBKO id 'meetid' nm 'name' hl '1D2D HistoryStation_id' sbko
! ***-5   SBKO id 'meetid' nm 'name' db '1D2D DamBreak_id' sbko
! *********************************************************************
!
      String = ' '
      Isbk = 0
      Do While (.not. endfil)
        READ(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
        IF (IDEBUG .GT. 0) WRITE(IDEBUG,*)  STRING(1:len_trim(String))
! skip regel als hij niet begint met juist keyword
        IF (STRING(1:4) .EQ. 'SBKO') Then
!         Read data using ParseToken
          BackSpace(IN)
          SearchString = 'SBKO'
          ReadError = .false.
          Success = GetRecord (In, SearchString, Endfil, Idebug, Iout1)
          If (Endfil .or. .not. success) then
             Call ErrMsg (974, 0, 'Rdl_Sb', ' Unexpected end of Sobek locations file ',IOUT1)
             RetVal = 974
             Return
          Endif
          String = ' '
          Success = GetStringFromBuffer (String)
          If (.not. Success) goto 150
          Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
          If (.not. Success) goto 150
          BRANCH = ' '
          Node   = ' '
          Reach  = ' '
          Struct = ' '
! check dimensies
          ISbk = ISbk + 1
          IF (ISBK .GT. NSBK) then
             CALL ERRMSG (913, 0, 'Rdl_sb',' NSBK Sobek lokaties',IOUT1)
             RetVal = 913
             Return
          Endif
! Read data
! get obligatory id
          if (Getkey ('id', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Id = RecordData%Token(ReturnIndx+1)
          else
              ReadError = .true.
          endif
! optional name
          Name = ' '
          if (Getkey ('nm', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Name = RecordData%Token(ReturnIndx+1)
          endif
! Sobek location via bi lc, in, ir, is, ml, hl or db keyword
          If (Getkey ('bi', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Branch = RecordData%Token(ReturnIndx+1)
              if (Getkey ('lc', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                  Read (RecordData%Token(ReturnIndx+1),*,Err=991)   Rloc
              else
                  ReadError = .true.
              endif
              ID_SBR (ISBK) = BRANCH
              ID_SLC (ISBK) = RLOC
          ELSEIF (Getkey ('in', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Node = RecordData%Token(ReturnIndx+1)
              ID_SBR (ISBK) = Node
              ID_SLC (ISBK) = -1
          ELSEIF (Getkey ('ir', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Reach = RecordData%Token(ReturnIndx+1)
              ID_SBR (ISBK) = Reach
              ID_SLC (ISBK) = -2
          ELSEIF (Getkey ('is', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Struct = RecordData%Token(ReturnIndx+1)
              ID_SBR (ISBK) = Struct
              ID_SLC (ISBK) = -3
          ELSEIF (Getkey ('ml', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              MeasLoc = RecordData%Token(ReturnIndx+1)
              ID_SBR (ISBK) = MeasLoc
              ID_SLC (ISBK) = -4
          ELSEIF (Getkey ('hl', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              HistoryLoc = RecordData%Token(ReturnIndx+1)
              ID_SBR (ISBK) = HistoryLoc
              ID_SLC (ISBK) = -5
          ELSEIF (Getkey ('db', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              DamBreakLoc = RecordData%Token(ReturnIndx+1)
              ID_SBR (ISBK) = DamBreakLoc
              ID_SLC (ISBK) = -5
          Else
              ReadError = .true.
!             No branch, node, reachseg or struct found
              CALL ERRMSG (912, 0, ' bi in ir is ml hl bd',' Read Sobek lokaties',IOUT1)
              RetVal = 912
              Return
          Endif

          IF (IDEBUG .GT. 0) THEN
              WRITE(IDEBUG,*) ' Echo sbk_id',ID(1:len_trim(Id))
              WRITE(IDEBUG,*) BRANCH(1:len_trim(Branch)), RLOC, Node(1:len_trim(Node)), &
                                  Reach(1:len_trim(Reach)), Struct(1:len_trim(Struct))
          ENDIF
! additional error handling
          Goto 992
  991     Continue
          ReadError = .true.
  992     Continue
          IF (ReadError) then
             Call ErrMsg (974,0,'Rdl_Sb',' Read error during reading SBKO location record',IOUT1)
             RetVal = 974
             Return
          Endif
          ID_SBK (ISBK) = ID
          NAME_SBK (ISBK) = Name
        ENDIF
   21   Continue
        CALL SKPCOM (IN, ENDFIL,'RTC')
      EndDo
      NSOBEK = Max(0, ISBK)
      IF (IDEBUG .GT. 0) THEN
        DO ISBK=1, NSOBEK
           WRITE(IDEBUG,*) ID_SBK(ISBK)(1:len_trim(Id_Sbk(Isbk))), ID_SBR(ISBK)(1:len_trim(Id_Sbr(Isbk))), ID_SLC(ISBK)
        ENDDO
      ENDIF
      GOTO 999
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 CONTINUE
      CALL ERRMSG (902, IECODE, 'Rdl_sb', ' Sobek_id file', IOUT1)
      RetVal = 902

! *********************************************************************
! *** end of file
! *********************************************************************
!
  999 CONTINUE

      RETURN
      End Function Rdl_sb




      Function RDL_3B (IDEBUG, IN, IOUT1) result(RetVal)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RTC version 1.0.                    Date: June  1997
! *********************************************************************
! *** Last update: June  1997        By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Inlezen 3B lokaties: Delft_3B_id
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************

      Use NewTables
      Use ParseToken
      Use ReadLib

      Integer RetVal

      INTEGER    NHLP
      PARAMETER (NHLP=10)

      INTEGER      IDEBUG, IN, IOUT1, ID3B, IECODE
!      INTEGER      IDUM(NHLP)
!      REAL         RDUM(NHLP)
!      Double Precision Ddum(NHLP)
      LOGICAL      ENDFIL
!      LOGICAL      ALLOW, FOUND

      CHARACTER(len=CharIdLength) :: IDNODE, Name  !, CDUM(NHLP)
      CHARACTER*999               :: STRING

! Additional variables for NewTables and ParseToken
      Character*4    SearchString
      Integer        ScanToTk, IStart, ReturnIndx, NumberOfTokens
      Logical        ParseTokenReadCaseSensitive, ParseTokenSearchCaseSensitive, ReadError, Success
      Type (TokenArray) RecordData

      RetVal = 0

      IStart   = 1     ! Scan from token 1
      ScanToTk = 999   ! Scan up to Token 999
      ParseTokenReadCaseSensitive = .true.      ! no conversion to upper case; ParseToken fills array in original case
      ParseTokenSearchCaseSensitive = .false.   ! find keywords case-insensitive
! end of additional variables ParseToken

      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' RDL_3B')
!     ALLOW = .FALSE.

! *********************************************************************
! *** skip header of file
! *********************************************************************

      CALL SKPCOM (IN, ENDFIL,'RTC')

! *********************************************************************
! *** read data
! *** format:     3BO id 'id_id' nm 'name' 3bo
! *********************************************************************

      String = ' '
      ND3BID=0
      ID3B=0
      Endfil = .false.
      Do While (.not. endfil)
         READ(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
! skip regel als hij niet begint met juist keyword
         IF (STRING(1:3) .EQ. '3BO') THEN
!          Read data using ParseToken
           BackSpace(IN)
           SearchString = '3BO'
           ReadError = .false.
           Success = GetRecord (In, SearchString, Endfil, Idebug, Iout1)
           If (Endfil .or. .not. success) then
             Call ErrMsg (974, 0, 'Rdl_3B', ' Unexpected end of RR locations file ',IOUT1)
             RetVal = 974
             Return
           Endif
           String = ' '
           Success = GetStringFromBuffer (String)
           If (.not. Success) goto 150
           Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
           If (.not. Success) goto 150
           ID3B = ID3B + 1
! check dimensies
           IF (ID3B .GT. ND3B) then
             Call ERRMSG (913, 0, 'Rdl_3b',' ND3B 3B-lokaties',IOUT1)
             RetVal = 913
             Return
           Endif
! end check
           IF (IDEBUG .GT. 0) WRITE(IDEBUG,*)  STRING(1:len_trim(String))
! get id
           if (Getkey ('id', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               IdNode = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
! get optional name
           if (Getkey ('nm', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Name = RecordData%Token(ReturnIndx+1)
           endif
           IF (ReadError) then
             Call ErrMsg (974,0,'Rdl_3B',' Read error during reading 3BO location record',IOUT1)
             RetVal = 974
             Return
           Endif
           IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Echo 3B_id',IDNODE(1:len_trim(IdNode))
           ID_D3B (ID3B) = IDNODE
           NAME_D3B (ID3B) = Name
         Endif
   21    CONTINUE
         CALL SKPCOM (IN, ENDFIL,'RTC')
      EndDo
      ND3BID = Max(0, ID3B)
      IF (IDEBUG .GT. 0) THEN
        DO ID3B=1,ND3BID
           WRITE(IDEBUG,*) ID_D3B(ID3B)(1:len_trim(Id_D3b(Id3B)))
        ENDDO
      ENDIF
      GOTO 999
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 CONTINUE
      CALL ERRMSG (902, IECODE, 'Rdl_3B', ' Delft3B_id file', IOUT1)
      RetVal = 902

! *********************************************************************
! *** end of file
! *********************************************************************
!
  999 CONTINUE

      RETURN
      END Function Rdl_3B


      Function RDL_3D (IDEBUG, IN, IOUT1) result(RetVal)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RTC version 1.0.                     Date: June 1997
! *********************************************************************
! *** Last update: Feb 2008          By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Inlezen lokatie file: D3D_id (incl. 1D2D), en lokatie
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************

      Use NewTables
      Use ParseToken

      Integer RetVal

      INTEGER    NHLP
      PARAMETER (NHLP=10)
!
      LOGICAL       ENDFIL
      CHARACTER(len=CharIdLength) :: ID, Name
      CHARACTER*999               :: STRING
      INTEGER                     :: IDEBUG, IOUT1, IN, ID3D, IECODE
!
! Additional variables for NewTables and ParseToken
      Character*4    SearchString
      Integer        ScanToTk, IStart, ReturnIndx, NumberOfTokens
      Logical        ParseTokenReadCaseSensitive, ParseTokenSearchCaseSensitive, ReadError, success
      Type (TokenArray) RecordData

      RetVal = 0

      IStart   = 1     ! Scan from token 1
      ScanToTk = 999   ! Scan up to Token 999
      ParseTokenReadCaseSensitive = .true.      ! no conversion to upper case; ParseToken fills array in original case
      ParseTokenSearchCaseSensitive = .false.   ! find keywords case-insensitive
! end of additional variables ParseToken


      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' RDL_3D')
!
! *********************************************************************
! *** skip header of file
! *********************************************************************
!
      CALL SKPCOM (IN, ENDFIL,'RTC')
!
! *********************************************************************
! *** read data
! *** format:     D3DO id 'id_id' nm 'name' d3do
! *********************************************************************
!
      String = ' '
      ID3D = 0
      Do While (.not. endfil)
        READ(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
        IF (IDEBUG .GT. 0) WRITE(IDEBUG,*)  STRING(1:len_trim(String))
! skip regel als hij niet begint met juist keyword
        IF (STRING(1:4) .EQ. 'D3DO') Then
!         Read data using ParseToken
          BackSpace(IN)
          SearchString = 'D3DO'
          ReadError = .false.
          Success = GetRecord (In, SearchString, Endfil, Idebug, Iout1)
          If (Endfil .or. .not. success) then
             Call ErrMsg (974, 0, 'Rdl_3D', ' Unexpected end of Sobek locations file ',IOUT1)
             RetVal = 974
             Return
          Endif
          String = ' '
          Success = GetStringFromBuffer (String)
          If (.not. Success) goto 150
          Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
          If (.not. Success) goto 150
! check dimensies
          ID3D = ID3D + 1
          IF (ID3D .GT. N3D) then
             CALL ERRMSG (913, 0, 'Rdl_3D',' N3D D3DFlow locations',IOUT1)
             RetVal = 913
             Return
          Endif
! Read data
! get obligatory id
          if (Getkey ('id', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Id = RecordData%Token(ReturnIndx+1)
          else
              ReadError = .true.
          endif
! optional name
          Name = ' '
          if (Getkey ('nm', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
              Name = RecordData%Token(ReturnIndx+1)
          endif

          IF (IDEBUG .GT. 0) THEN
              WRITE(IDEBUG,*) ' Echo sbk_id',ID(1:len_trim(Id))
          ENDIF
! additional error handling
          Goto 992
  991     Continue
          ReadError = .true.
  992     Continue
          IF (ReadError) then
             Call ErrMsg (974,0,'Rdl_3D',' Read error during reading D3DO location record',IOUT1)
             RetVal = 974
             Return
          Endif
          ID_D3D (ID3D) = ID
          NAME_D3D (ID3D) = Name
        ENDIF
   21   Continue
        CALL SKPCOM (IN, ENDFIL,'RTC')
      EndDo
      ND3D = Max(0, ID3D)
      IF (IDEBUG .GT. 0) THEN
        DO ID3D=1, ND3D
           WRITE(IDEBUG,*) ID_D3D(ID3D)(1:len_trim(Id_D3D(ID3D)))
        ENDDO
      ENDIF
      GOTO 999
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 CONTINUE
      CALL ERRMSG (902, IECODE, 'Rdl_3D', ' D3DFlow_id file', IOUT1)
      RetVal = 902

! *********************************************************************
! *** end of file
! *********************************************************************
!
  999 CONTINUE

      RETURN
      End Function Rdl_3D




      Function RDL_PR (IDEBUG, IN, IOUT1) result(RetVal)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RTC version 1.0.                    Date: June  1997
! *********************************************************************
! *** Last update: June  1997        By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Inlezen neerslag lokaties: Precipitation id's
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************

      Use NewTables
      Use ParseToken
      Use ReadLib

      Integer RetVal

      INTEGER    NHLP
      PARAMETER (NHLP=10)
!
!      INTEGER      IDUM(NHLP)
!      REAL         RDUM(NHLP)
!      Double Precision Ddum(NHLP)
      LOGICAL      ENDFIL
!      LOGICAL      ALLOW, FOUND
      CHARACTER(len=CharIdLength) :: IDNODE, Name  ! CDUM(NHLP)
      CHARACTER*999               :: STRING
      INTEGER                     :: IDEBUG, IN, IOUT1, IPRECP, IPRE, IECODE

! Additional variables for NewTables and ParseToken
      Character*4    SearchString
      Integer        ScanToTk, IStart, ReturnIndx, NumberOfTokens
      Logical        ParseTokenReadCaseSensitive, ParseTokenSearchCaseSensitive, ReadError, Success
      Type (TokenArray) RecordData

      RetVal = 0

      IStart   = 1     ! Scan from token 1
      ScanToTk = 999   ! Scan up to Token 999
      ParseTokenReadCaseSensitive = .true.      ! no conversion to upper case; ParseToken fills array in original case
      ParseTokenSearchCaseSensitive = .false.   ! find keywords case-insensitive
! end of additional variables ParseToken
!
      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' RDL_PR')
!      ALLOW = .FALSE.
!
! *********************************************************************
! *** skip header of file
! *********************************************************************
!
      CALL SKPCOM (IN, ENDFIL,'RTC')
!     ARS 12430: Precipitation file is allowed to be empty, no check on premature end-of-file
!     IF (ENDFIL) CALL ERRMSG (911, 0, 'Rdl_PR', ' Precipitation id file', IOUT1)
!
! *********************************************************************
! *** read data
! *** format:     PREC id 'id_id' nm 'name' prec
! *********************************************************************
!
      String = ' '
      NPRECP=0
      IPrecP=0
      Do While (.not. endfil)
         IF (ENDFIL) GOTO 21
         READ(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
! skip regel als hij niet begint met juist keyword
         IF (STRING(1:4) .EQ. 'PREC') Then
!          Read data using ParseToken
           BackSpace(IN)
           SearchString = 'PREC'
           ReadError = .false.
           Success = GetRecord (In, SearchString, Endfil, Idebug, Iout1)
           If (Endfil .or. .not. success) then
             Call ErrMsg (974, 0, 'Rdl_Pre', ' Unexpected end of Precipitations locations file ',IOUT1)
             RetVal = 974
             Return
           Endif
           String = ' '
           Success = GetStringFromBuffer (String)
           If (.not. Success) goto 150
           Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
           If (.not. Success) goto 150
           IPrecP = IPrecP + 1
! check dimensies
           IF (IPRECP .GT. NPRE) then
             CALL ERRMSG (913, 0,'Rdl_pr',' NPRE neerslaglokaties',IOUT1)
             RetVal = 913
             Return
           Endif
! end check
           IF (IDEBUG .GT. 0) WRITE(IDEBUG,*)  STRING(1:len_trim(String))
! get id
           if (Getkey ('id', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               IdNode = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
! get optional name
           if (Getkey ('nm', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Name = RecordData%Token(ReturnIndx+1)
           endif
!          IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Echo neerslag_id',IDNODE(1:len_trim(IdNode))
           ID_PRE (IPRECP) = IDNODE
           NAME_PRE (IPRECP) = Name
         Endif
   21    CONTINUE
         CALL SKPCOM (IN, ENDFIL,'RTC')
      EndDo
      NPRECP = Max(0, IPRECP)

      IF (IDEBUG .GT. 0) THEN
        DO IPRE=1,NPRECP
           WRITE(IDEBUG,*) ID_PRE(IPRE)(1:len_trim(ID_Pre(Ipre)))
        ENDDO
      ENDIF
      GOTO 999
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 CONTINUE
      CALL ERRMSG (902, IECODE, 'Rdl_pr', ' neerslag_id file', IOUT1)
      RetVal = 902

! *********************************************************************
! *** end of file
! *********************************************************************
!
  999 CONTINUE

      RETURN
      END Function Rdl_Pr


      Function RDL_EX (IDEBUG, IN, IOUT1) result(RetVal)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RTC version 1.0.                    Date: June  1997
! *********************************************************************
! *** Last update: June  1997        By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Inlezen externe lokaties: lokatie_id
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************

      Use NewTables
      Use ParseToken
      Use FileModule
      Use ReadLib
      use Dio_Plt_Rw

      Integer :: RetVal

      INTEGER    NHLP
      PARAMETER (NHLP=10)
!
      INTEGER      IDEBUG, IN, IOUT1, IEXT, JExt,IECODE
!      INTEGER      IDUM(NHLP)
!      REAL         RDUM(NHLP)
!      Double Precision Ddum(NHLP)
      LOGICAL      ENDFIL, CaseSensitive
!      LOGICAL      ALLOW, FOUND
      CHARACTER(len=CharIdLength) :: IDNODE, Name, HisFile, HisParameter, HisLocation   ! CDUM(NHLP)
      CHARACTER*999               :: STRING

! Additional variables for NewTables and ParseToken
      Character*4    SearchString
      Integer        ScanToTk, IStart, ReturnIndx, NumberOfTokens
      Logical        ParseTokenReadCaseSensitive, ParseTokenSearchCaseSensitive, ReadError, Success
      Type (TokenArray) RecordData

      Integer        InTest

      RetVal = 0

      IStart   = 1     ! Scan from token 1
      ScanToTk = 999   ! Scan up to Token 999
      ParseTokenReadCaseSensitive = .true.      ! no conversion to upper case; ParseToken fills array in original case
      ParseTokenSearchCaseSensitive = .false.   ! find keywords case-insensitive
! end of additional variables ParseToken
!
      CaseSensitive = .false.

      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' RDL_EX')
!      ALLOW = .FALSE.
!
! *********************************************************************
! *** skip header of file
! *********************************************************************
!
      CALL SKPCOM (IN, ENDFIL,'RTC')
!     ARS 12430: External file is allowed to be empty, no check on premature end-of-file
!     IF (ENDFIL) CALL ERRMSG (911, 0, 'Rdl_Ex', ' External id file', IOUT1)
!
! *********************************************************************
! *** read data   EXT records (wind prediction)
! *** format:     EXT id 'id_id' nm 'name' ext
! *********************************************************************
!
      String = ' '
      NEXTD=0
      IExt=0
      Do While (.not. endfil)
         IF (ENDFIL) GOTO 21
         READ(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
! skip regel als hij niet begint met juist keyword
         IF (STRING(1:3) .EQ. 'EXT') Then
!          Read data using ParseToken
           BackSpace(IN)
           SearchString = 'EXT'
           ReadError = .false.
           Success = GetRecord (In, SearchString, Endfil, Idebug, Iout1)
           If (Endfil .or. .not. success) then
             Call ErrMsg (974, 0, 'Rdl_Ext', ' Unexpected end of External locations file ',IOUT1)
             RetVal = 974
             Return
           Endif
           String = ' '
           Success = GetStringFromBuffer (String)
           If (.not. Success) goto 150
           Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
           If (.not. Success) goto 150
           IExt = IExt + 1
! check dimensies
           IF (IEXT .GT. NEXT) then
              CALL ERRMSG (913, 0,'Rdl_ex',' NEXT externe lokaties',IOUT1)
              RetVal = 913
              Return
           Endif
! end check
           IF (IDEBUG .GT. 0) WRITE(IDEBUG,*)  STRING(1:len_trim(String))
! get id
           if (Getkey ('id', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               IdNode = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
! get optional name
           if (Getkey ('nm', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Name = RecordData%Token(ReturnIndx+1)
           endif
           IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Echo externe_id',IDNODE (1:len_trim(IdNode))
           ID_EXT (IEXT) = IDNODE
           NAME_EXT (IEXT) = Name
         Endif
   21    CONTINUE
         CALL SKPCOM (IN, ENDFIL,'RTC')
      EndDo
      NEXTD = Max(0, IEXT)

      IF (IDEBUG .GT. 0) THEN
        WRITE(IDEBUG,*) ' External locations', NExtD
        DO IEXT=1,NEXTD
           WRITE(IDEBUG,*) ID_EXT(IEXT) (1:len_trim(Id_Ext(Iext)))
        ENDDO
      ENDIF

! *********************************************************************
! *** read data HEXT records (data from external HIS files)
! *** format:    HEXT id 'id_id' nm 'name' hf 'Hisfilename' hp 'hisparamaterid' hl 'hislocationid' hext
! *********************************************************************
!
      Rewind(in)
      Endfil = .false.
      String = ' '
      NEXTHD=0
      IExt=0
      Do While (.not. endfil)
         IF (ENDFIL) GOTO 221
         READ(IN,'(A)',END=221,ERR=150,IOSTAT=IECODE)  STRING
! skip regel als hij niet begint met juist keyword
         IF (STRING(1:4) .EQ. 'HEXT') Then
!          Read data using ParseToken
           BackSpace(IN)
           SearchString = 'HEXT'
           ReadError = .false.
           Success = GetRecord (In, SearchString, Endfil, Idebug, Iout1)
           If (Endfil .or. .not. success) then
              Call ErrMsg (974, 0, 'Rdl_Ext', ' Unexpected end of External locations file ',IOUT1)
              RetVal = 974
              Return
           Endif
           String = ' '
           Success = GetStringFromBuffer (String)
           If (.not. Success) goto 150
           Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
           If (.not. Success) goto 150
           IExt = IExt + 1
! check dimensies
           IF (IEXT .GT. NEXTH) then
              CALL ERRMSG (913, 0,'Rdl_ex',' NEXTH externe lokaties His file',IOUT1)
              RetVal = 913
              Return
           Endif
           IF (IDEBUG .GT. 0) WRITE(IDEBUG,*)  STRING(1:len_trim(String))
! get id
           if (Getkey ('id', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               IdNode = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
! get optional name
           if (Getkey ('nm', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               Name = RecordData%Token(ReturnIndx+1)
           endif
           if (Getkey ('hf', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               HisFile = RecordData%Token(ReturnIndx+1)
!              test whether referenced input file exists!
               Intest = DioNewLun()
               Call OpenFl(intest,Hisfile, Iout1,2)
               Close(intest)
           else
               ReadError = .true.
           endif
           if (Getkey ('hp', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               HisParameter = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
           if (Getkey ('hl', IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
               HisLocation = RecordData%Token(ReturnIndx+1)
           else
               ReadError = .true.
           endif
           IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Echo externe_id',IDNODE (1:len_trim(IdNode))
           ID_EXT (NEXT+IEXT) = IDNODE
           NAME_EXT (NEXT+IEXT) = Name
           HisFileExt (IEXT) = HisFile
           HisParExt  (IEXT) = HisParameter
           HisLocExt  (IEXT) = HisLocation
         Endif
  221    CONTINUE
         CALL SKPCOM (IN, ENDFIL,'RTC')
      EndDo
      NEXTHD = Max(0, IEXT)

      IF (IDEBUG .GT. 0) THEN
        WRITE(IDEBUG,*) ' External locations - HIS file coupling; number of locations=', NExtHD
        DO IEXT=1,NEXTHD
           WRITE(IDEBUG,'(I3,1X,A,1X,A,1X,A,1X,A)') IExt, ID_EXT(NEXT+IEXT) (1:len_trim(Id_Ext(NEXT+Iext))), &
                                        HisFileExt(IEXT) (1:len_trim(HisFileExt(Iext))), &
                                        HisParExt(IEXT) (1:len_trim(HisParExt(Iext))), &
                                        HisLocExt(IEXT) (1:len_trim(HisLocExt(Iext)))
        ENDDO
      ENDIF
! Determine HisDataSet
      NExtHDataSet = 0
      Do Iext=1,NextHD
         Do Jext=1,IExt-1
            If (StringComp(HisFileExt(IExt), HisFileExt(JExt), CaseSensitive)) then
                 HisDataSet(Iext,1) = HisDataSet(Jext,1)
                 goto 901
            Endif
         Enddo
         NExtHDataSet = NExtHDataSet + 1
         HisDataSet(Iext,1) = NExtHDataSet
!                     (.,2) = parameter index and
!                     (.,3) = location index  are to be determined later!!
         HisDataSetFile(NextHDataSet) = HisFileExt(IExt)
  901    Continue
      Enddo
      IF (IDEBUG .GT. 0) THEN
        WRITE(IDEBUG,*) ' External locations - HIS file coupling; number of locations=', NExtHD
        DO IEXT=1,NEXTHD
           WRITE(IDEBUG,'(I3,1X,A,1X,A,1X,A,1X,A,I4,A)') IExt, ID_EXT(NEXT+IEXT) (1:Len_trim(Id_Ext(NEXT+Iext))), &
                                        HisFileExt(IEXT) (1:Len_trim(HisFileExt(Iext))), &
                                        HisParExt(IEXT) (1:Len_trim(HisParExt(Iext))), &
                                        HisLocExt(IEXT) (1:Len_trim(HisLocExt(Iext))), &
                                        HisDataSet(iext,1), HisDataSetFile(HisDataSet(iext,1))
        ENDDO
      ENDIF

      GOTO 999
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 CONTINUE
      CALL ERRMSG (902, IECODE, 'Rdl_ex', ' external_id file', IOUT1)
      RetVal = 902

! *********************************************************************
! *** end of file
! *********************************************************************
!
  999 CONTINUE

      RETURN
      END Function Rdl_Ex



      Function RDL_WQ (IDEBUG, IN, IOUT1) result(RetVal)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RTC version 1.0.                     Date: June 1997
! *********************************************************************
! *** Last update: March 2001        By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Inlezen lokatie file with monitoring locations for WQ
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IN     = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************
!
      Use ReadLib

      Integer :: RetVal

      LOGICAL      ENDFIL
      INTEGER      POS1
      CHARACTER*999 STRING
      INTEGER       IDEBUG, IOUT1, IN, ISobWq, IECODE

      RetVal = 0

      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' RDL_WQ')
!
! *********************************************************************
! *** skip header of file
! *********************************************************************
!
      CALL SKPCOM (IN, ENDFIL,'RTC')
!
! *********************************************************************
! *** read data WQ monitoring stations. ID is given in record after string 'Name:'
! *********************************************************************
!
      String = ' '
      ISobWq = 0
      Do While (.not. endfil)
        READ(IN,'(A)',END=21,ERR=150,IOSTAT=IECODE)  STRING
        IF (IDEBUG .GT. 0) WRITE(IDEBUG,*)  STRING(1:len_trim(String))
! skip regel als hij het keyword 'Name: 'niet bevat
        POS1 = Index (String, 'Name: ')
        IF (POS1 .GT. 0) Then
! check dimensies
           ISobWq = ISobWq + 1
           IF (ISobWQ .GT. NSWQ)  then
              CALL ERRMSG (913, 0, 'Rdl_wq',' NSWQ Sobek-WQ lokaties',IOUT1)
              RetVal = 913
              Return
           Endif
! Read data
           ID_SWQ (ISobWQ) = String(Pos1+6:)
        ENDIF
   21   Continue
        CALL SKPCOM (IN, ENDFIL,'RTC')
      EndDo
      NSOBWQ = Max(0, ISobWQ)
      IF (IDEBUG .GT. 0) THEN
        Write(Idebug,*) ' WQ ids read'
        DO ISobWQ=1, NSOBWQ
           WRITE(IDEBUG,*) ID_SWQ(ISobWq) (1:len_trim(Id_Swq(ISobWq)))
        ENDDO
      ENDIF
      GOTO 999
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 CONTINUE
      CALL ERRMSG (902, IECODE, 'Rdl_wq', ' Sobek_wq id file', IOUT1)
      RetVal = 902

! *********************************************************************
! *** end of file
! *********************************************************************
!
  999 CONTINUE

      RETURN
      End Function Rdl_Wq



      Function CheckIdsMatlab_oldversion (IdString, Iout1, fullstring)  result(RetVal)

      use ReadLib

      Integer :: RetVal

      character, parameter :: strUNDERSCORE = '_'
      character, parameter :: strBlank=' '
      character, parameter :: strMinus='-'
      character, parameter :: strPlus ='+'
      character, parameter :: strMultiply ='*'
      character, parameter :: strDivide='/'
      character, parameter :: strEqual='='
      character, parameter :: strNotEqual='#'
      character, parameter :: strGreater='>'
      character, parameter :: strSmaller='<'
		
      character(len=*)            :: IdString
      character(len=CharIdLength) :: TmpString
      Integer                     :: iOut1, Idebug, ipos
      Logical                     :: fullstring

      RetVal = 0
      idebug = iout1
!      write(Idebug,*) ' Entry of CheckIdsMatlab', IdString(1:)

         if (CntStr('=',IdString) .ge. 2) then
            Call ErrMsg (999, 1, ' CheckIdsMatlab', ' =sign in ids will not work when using Matlab', IOUT1)
            RetVal = 999
!           stop ' = sign in ids will not work when using Matlab'
         endif
         iPos = INDEX(IdString, '=')
         if (fullstring) then
            TmpString = IdString(1:)
         else
            TmpString = IdString(1:iPos)
         Endif
         CALL RPLSTR (TmpString, strBLANK,  strUNDERSCORE)
         CALL RPLSTR (TmpString, strMinus,  strUNDERSCORE)
         CALL RPLSTR (TmpString, strPlus ,  strUNDERSCORE)
         CALL RPLSTR (TmpString, strMultiply, strUNDERSCORE)
         CALL RPLSTR (TmpString, strDivide, strUNDERSCORE)
!        CALL RPLSTR (TmpString, strEqual,  strUNDERSCORE)
         CALL RPLSTR (TmpString, strGreater, strUNDERSCORE)
         CALL RPLSTR (TmpString, strSmaller, strUNDERSCORE)
         CALL RPLSTR (TmpString, strNotEqual,strUNDERSCORE)
         if (FullString) then
            IdString(1:)=TmpString(1:)
         else
            IdString(1:iPos)=TmpString(1:IPos)
         Endif

!      write(Idebug,*) ' Exit of CheckIdsMatlab', IdString(1:)


      RETURN
      End Function CheckIdsMatlab_oldversion



      Function CheckIdsMatlab (IdString, Iout1, fullstring)  result(RetVal)
!     new version
!     allows only characters between a-z, A-Z, digits 0-9, and underscores
!     all other characters are replaced by underscores
!     Forbidden characters are e.g. -,+,=,%,&,#, etc.

!     General routine, independent of ASCII or EBCDIC character codes
!     Only assumption: a-z, A-Z, 0-9 are in that order in the character sequence file

!    Additionally: also , is allowed (string separator in Matlab, used in getting multiple strings from Matlab)


      use ReadLib

      Integer :: RetVal

      character, parameter :: strUNDERSCORE = '_'
      character, parameter :: strComma      = ','
		
      character(len=*)            :: IdString
      character(len=CharIdLength) :: TmpString
      character(len=1)				 :: alower,zlower,Aupper,Zupper
      Integer                     :: iOut1, Idebug, ipos, i,j, ialow, izlow, iaup, izup, iund, i0, i9, icomma
      Logical                     :: fullstring

      RetVal = 0
      idebug = iout1

      aLower = 'a'
      zLower = 'z'
      AUpper = 'A'
      ZUpper = 'Z'
      i0    =  ichar('0')
      i9    =  ichar('9')
      ialow =  ichar(ALower)
      iaup  =  ichar(Aupper)
      izlow =  ichar(Zlower)
      izup  =  ichar(Zupper)
      Iund  =  ichar(StrUNDERSCORE)
      Icomma=  ichar(StrComma)

         iPos = INDEX(IdString, '=')-1
         if (fullstring) then
            TmpString = IdString(1:)
         else
            TmpString = IdString(1:iPos)
         Endif
         Do i=1, len_trim(TmpString)
            j = ichar(TmpString(i:i))
            if (j .ne. iund .and. j .ne. icomma .and. .not. (j .ge. ialow .and. j .le. izlow) .and. &
                 .not. (j .ge. iaup .and. j .le. izup) .and. .not. (j .ge. i0 .and. j .le. i9)) then
                 CALL RPLSTR (TmpString, Tmpstring(i:i), strUNDERSCORE)
            endif
         Enddo

         if (FullString) then
            IdString(1:)=TmpString(1:)
         else
            IdString(1:iPos)=TmpString(1:IPos)
         Endif


      RETURN
      End Function CheckIdsMatlab


    End Module LocationDataModule
