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

    Module MeasureModule

    Use ParameterModule
    Use DH_Alloc

    Implicit None

! Constants

! Variables

! *** Maatregel data mbt 3B (3B measure data)
!
! *** MSID3B   = 3B-measure id
! *** MSBP3B   = 3B-beslisparameter id
! *** MSON3B   = aanslagpeil van de maatregel
! *** MSOFF3B  = afslagpeil van de maatregel
! *** ONCH3B   = 3B-measure check aanslagpeil (<,=,>)
! *** OFCH3B   = 3B-measure check afslagpeil (<,=,>)

! *** LCID3B   = 3B-lokatie id gerelateerd aan maatregel (id in 3BML record)
! *** LCMS3B   = gerelateerde maatregel (uit 3BML record, verwijst naar MLST record)
! *** LCPR3B   = 3B-prioriteit per 3B lokatie

! *** IXMS3P   = index van de bij de 3B-measure (MLST) horende beslisparameter
! *** IXMS3B   = index van de bij de 3B-lokatie (3BML) horende 3B maatregel  MS3BID
! *** IXID3B   = index van de bij de 3B-lokatie (3BML) horende unieke id uit ID3BML
! *** ID3BML   = unieke set van 3B id's in maatregelen (=3B lokatie id's uit 3BML records zonder dubbelen)
! *** MSSTAT   = status van de maatregel; 0=uit; >0: aan
! *** MSSTA0   = vorige status van de maatregel;
! *** D3BSTA   = status gemaal van D3Bknoopmaatregel voor alle lokaties uit N3MLOC
! ***              0 = volle capaciteit
! ***              1 = gekort tot nul.
! *** MS3BST   = gezette points van de unieke 3B id's in maatregelen
! ***            ( ,1) = status gemaal van per D3Bknoop voor alle unieke lokaties uit N3MLOC)
! ***                    (0 = geen maalstop, 1 = maalstop)
! ***            ( ,2) = aantal maatregelen dat actief is
! *** NrSbkDllmeasures
! *** NrRRDllmeasures


      Logical                                    :: OldFormatMeasureFiles
      CHARACTER(len=CharIdLength), Pointer, Save :: MSID3B(:), MSBP3B(:)
      CHARACTER*1, Pointer, Save                 :: ONCH3B(:), OFCH3B(:)
      Double Precision, Pointer, Save            :: MSON3B(:), MSOFF3B(:)

      CHARACTER(len=CharIdLength), Pointer, Save :: LCID3B(:), LCMS3B(:), LcName3B(:)
      INTEGER, Pointer, Save                     :: LCPR3B(:)

      INTEGER, Pointer, Save                     :: IXMS3P (:), IXMS3B(:), IXID3B(:)
      CHARACTER(len=CharIdLength), Pointer, Save :: ID3BML (:), Descr3BML(:)

      INTEGER, Pointer, Save                     :: MSSTAT (:), MSSTA0(:)
      INTEGER, Pointer, Save                     :: D3BSTA (:)
      Double Precision, Pointer, Save            :: MS3BST(:,:)
      INTEGER                                    :: NrSbkDllMeasures, NrRRDllMeasures

! *** Maatregel data mbt Sobek (Sobek measure data)
!
! *** MEASID   = measure id (Sobek-controller id waarop de maatregel geldt)
! *** MEASName = measure name (Sobek-controller name/description waarop de maatregel geldt)
! *** MEASIDMatlab = measure Matlab id used in communication with Matlab
! *** MEASNV   = number of measure check values  (n dim.)
! *** MEASBP   = beslisparameter id
! *** MEASNBP  = n beslisparameter id's (n dim.)
! *** MEASPR   = prioriteit
! *** MEASTY   = measure type
! *** MEASCV   = measure check value
! *** MEASNCV  = measure n check values (n dim.)
! *** MEASCH   = measure check (<,=,>)
! **  MEASNCH  = measure check (<,=,>)(n dim.)
! *** MEASSP   = measure set point
! *** MEASNSP  = measure n set points (n dim.)
! *** MEASCP   = measure check parameter
! *** MEASNCP  = measure check parameters (n dim.)
! *** MEASCSP  = measure set point parameter
! *** INITSP   = sobek-structure initial set point, als geen maatregel actief is
! *** MSB_ON   = logical die aangeeft of Sobek-maatregel actief is.
!
! *** IXMSBP   = index van de bij de measure horende beslisparameter
! *** IXMSNBP  = index van de bij de measure horende beslisparameters (n dim.)
! *** IXMSCP   = index van de bij de measure horende check beslisparameter
! *** IXMSNCP  = index van de bij de measure horende check beslisparameters (n dim.)
! *** IXMSSP   = index van de bij de measure horende setpoint beslisparameter
! *** IXMSSB   = index van de bij de measure horende Sobek lokatie in MSSBID
! *** MSSBID   = unieke set van Sobek id's in maatregelen (=maatregel-id's zonder dubbelen)
! *** MSSBST   = gezette points van de unieke Sobek id's in maatregelen
! *** MeasMissingValue = missing  value of measures; if result=missing value then the measure is neglected

      CHARACTER(len=CharIdLength), Pointer, Save :: MEASID(:), MeasIdMatlab(:), MeasName(:), &
                                                    MEASBP(:), MSSBID(:), MsSbDescr(:),MEASCP(:), MEASCSP(:)
      CHARACTER*1, Pointer, Save                 :: MEASCH(:)
      CHARACTER(len=CharIdLength), Pointer, Save :: MEASNBP(:,:), MEASNCP(:,:), MEASNCH(:,:)
      LOGICAL, Pointer, Save                     :: MSB_ON(:)
      Double Precision, Pointer, Save            :: MEASCV(:), MEASSP(:), INITSP(:), MSSBST(:)
      Double Precision, Pointer, Save            :: MeasMissingValue(:)
      Double Precision, Pointer, Save            :: MEASNCV(:,:), MEASNSP(:,:)
      INTEGER, Pointer, Save                     :: MEASTY(:), MEASPR(:), IXMSBP(:), IXMSNBP(:,:), IXMSSB(:),&
                                                   MEASNV(:), IXMSCP(:), IXMSNCP(:,:), IXMSSP(:)

      CHARACTER(len=CharIdLength), Pointer, Save :: D3BPara(:), SbkPara(:)

! Originele MSDat.CMN is unused
!
! *** Maatregel data (measure data)
!
! *** MSON   ( , ) = aanslagpeilen van de maatregel
! *** MSOFF  ( , ) = afslagpeilen van de maatregel
! *** MSOPT  ( )   = optie (keuze parameter waarop gestuurd wordt)
! *** MSID3B ( , ) = koppeling van maatregelindex aan D3B id
! *** MSD3B  ( , ) = koppeling van delft_3B id aan sbk_id en prioriteit
!                    ( ,1) = interne sobek index
!                    ( ,2) = prioriteit
! *** MSSTAT (   ) = status van de maatregel; 0=uit; >0:  i=op prioriteit IPRI
! *** MSSTA0 (   ) = vorige status van de maatregel;

!      Double Precision, Pointer, Save ::         MSON (NSBK,NPRI), MSOFF (NSBK,NPRI)
!      INTEGER, Pointer, Save ::      MSOPT(NSBK), MSID3B(ND3B), MSD3B (ND3B,2), MSSTAT(NSBK), MSSTA0(NSBK)

! D3BPara = Parameters in HIS file to 3B
! SbkPara = Parameters in HIS file to Sobek


   Contains

    Function AllocMeasureArrays (Iout1) result(RetVal)

      Integer RetVal

      Integer Iout1
      Logical Success

      RetVal = 0

      Success = DH_AllocInit (NParaHis,D3BPara,'')
      Success = Success .and. DH_AllocInit (1,SbkPara,'')
      Success = Success .and. DH_AllocInit (N3Mes,MsId3B,'')
      Success = Success .and. DH_AllocInit (N3Mes,MsBp3B,'')
      Success = Success .and. DH_AllocInit (N3Mes,OnCh3B,'')
      Success = Success .and. DH_AllocInit (N3Mes,OfCh3B,'')
      Success = Success .and. DH_AllocInit (N3Mes,MsOn3B,0D0)
      Success = Success .and. DH_AllocInit (N3Mes,MsOff3B,0D0)
      Success = Success .and. DH_AllocInit (N3Loc,LcId3B,'')
      Success = Success .and. DH_AllocInit (N3Loc,LcName3B,'')
      Success = Success .and. DH_AllocInit (N3Loc,LcMs3B,'')
      Success = Success .and. DH_AllocInit (N3Loc,LcPr3B,0)
!     Allocate ( D3BPara(NParaHis), SbkPara(1))
!     Allocate ( MSID3B(N3MES), MSBP3B(N3MES), STAT=Allocation_Error )
!     Allocate ( ONCH3B(N3MES), OFCH3B(N3MES), STAT=Allocation_Error )
!     Allocate ( MSON3B(N3MES), MSOFF3B(N3MES) , STAT=Allocation_Error )
!     Allocate ( LCID3B(N3LOC), LCMS3B(N3LOC),  STAT=Allocation_Error )
!     Allocate ( LCPR3B(N3LOC),  STAT=Allocation_Error )

      Success = Success .and. DH_AllocInit (N3Mes,IxMs3P,0)
      Success = Success .and. DH_AllocInit (N3Loc,IxMs3B,0)
      Success = Success .and. DH_AllocInit (N3Loc,IxId3B,0)
      Success = Success .and. DH_AllocInit (N3Loc,Id3BML,'')
      Success = Success .and. DH_AllocInit (N3Loc,Descr3BML,'')
      Success = Success .and. DH_AllocInit (N3Mes,MsStat,0)
      Success = Success .and. DH_AllocInit (N3Mes,MsSta0,0)
      Success = Success .and. DH_AllocInit (N3Loc,D3BSta,0)
      Success = Success .and. DH_AllocInit (N3Loc,NParaHis,Ms3BST,0D0)
!     Allocate ( IXMS3P (N3MES), IXMS3B(N3LOC), IXID3B(N3LOC), STAT=Allocation_Error )
!     Allocate ( ID3BML (N3LOC), STAT=Allocation_Error )
!     Allocate ( MSSTAT (N3MES), MSSTA0(N3MES), STAT=Allocation_Error )
!     Allocate ( D3BSTA (N3LOC), STAT=Allocation_Error )
!     Allocate ( MS3BST(N3LOC,NParaHis), STAT=Allocation_Error )
      If (.not. success)  then
         Call ErrMsg (929, 1, ' AllocMeasureArrays', ' ', IOUT1)
         RetVal = 929
         Return
      Endif

! ARS 7623: MSSBID(NSMES) ipv (NSBK)
      Success = DH_AllocInit (NSMes,MeasId,'')
      Success = Success .and. DH_AllocInit (NSMes,MeasName,'')
      Success = Success .and. DH_AllocInit (NSMes,MeasIdMatlab,'')
      Success = Success .and. DH_AllocInit (NSMes,MeasMissingValue,0D0)
      Success = Success .and. DH_AllocInit (NSMes,MeasBp,'')
      Success = Success .and. DH_AllocInit (NSMes,MSSbId,'')
      Success = Success .and. DH_AllocInit (NSMes,MsSbDescr,'')
      Success = Success .and. DH_AllocInit (NSMes,MeasCP,'')
      Success = Success .and. DH_AllocInit (NSMes,MeasCSP,'')
      Success = Success .and. DH_AllocInit (NSMes,MeasCH,'')
      Success = Success .and. DH_AllocInit (NSCV,NSMes,MeasNBP,'')
      Success = Success .and. DH_AllocInit (NSCV,NSMes,MeasNCP,'')
      Success = Success .and. DH_AllocInit (NSCV,NSMes,MeasNCH,'')
      Success = Success .and. DH_AllocInit (NSMes,MSB_ON,.false.)

      Success = Success .and. DH_AllocInit (NSMes,MeasCV,0D0)
      Success = Success .and. DH_AllocInit (NSMes,MeasSP,0D0)
      Success = Success .and. DH_AllocInit (NSMes,INITSP,0D0)
      Success = Success .and. DH_AllocInit (NSMes,MSSBST,0D0)
      Success = Success .and. DH_AllocInit (NSCV,NSMes,MeasNCV,0D0)
      Success = Success .and. DH_AllocInit (NSCV,NSMes,MeasNSP,0D0)
      Success = Success .and. DH_AllocInit (NSMes,MeasTY,0)
      Success = Success .and. DH_AllocInit (NSMes,MeasPr,0)
      Success = Success .and. DH_AllocInit (NSMes,IxMsBP,0)
      Success = Success .and. DH_AllocInit (NSCV,NSMes,IxMSNBP,0)
      Success = Success .and. DH_AllocInit (NSMes,IxMSSB,0)
      Success = Success .and. DH_AllocInit (NSMes,MeasNV,0)
      Success = Success .and. DH_AllocInit (NSMes,IxMsCP,0)
      Success = Success .and. DH_AllocInit (NSCV,NSMes,IxMSNCP,0)
      Success = Success .and. DH_AllocInit (NSMes,IxMsSp,0)

!     Allocate ( MEASID(NSMES), MEASBP(NSMES), MSSBID(NSMES), &
!                MEASCP(NSMES), MEASCSP(NSMES), STAT=Allocation_Error )
!     Allocate ( MEASCH(NSMES), STAT=Allocation_Error )
!     Allocate ( MEASNBP(NSCV,NSMES), MEASNCP(NSCV,NSMES), &
!                MEASNCH(NSCV,NSMES), STAT=Allocation_Error )
!     Allocate ( MSB_ON(NSMES), STAT=Allocation_Error )
! ARS 7623:  MSSBST(NSMES) ipv (NSBK)
!     Allocate ( MEASCV(NSMES), MEASSP(NSMES), INITSP(NSMES), &
!                MSSBST(NSMES), STAT=Allocation_Error )
!     Allocate ( MEASNCV(NSCV,NSMES), MEASNSP(NSCV,NSMES), STAT=Allocation_Error )
!     Allocate ( MEASTY(NSMES), MEASPR(NSMES), IXMSBP(NSMES), IXMSNBP(NSCV,NSMES), &
!                IXMSSB(NSMES), MEASNV(NSMES), IXMSCP(NSMES), IXMSNCP(NSCV,NSMES), &
!                IXMSSP(NSMES), STAT=Allocation_Error )
      If (.not. success)  then
         Call ErrMsg (929, 1, ' AllocMeasureArrays', ' ', IOUT1)
         RetVal = 929
         Return
      Endif

! Originele MSDat.CMN is unused
!      Allocate ( MSON (NSBK,NPRI), MSOFF (NSBK,NPRI) )
!      Allocate ( MSOPT(NSBK), MSID3B(ND3B), MSD3B (ND3B,2), MSSTAT(NSBK), MSSTA0(NSBK) )

    Return
    End Function AllocMeasureArrays

  End Module MeasureModule
