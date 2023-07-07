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

      SUBROUTINE WRICHA ( LU    , NTRANS, VARNAM, NNOTRA, IOUTP ,
     J                    NSPEC , NAMSPE, NTOX  , NAMTOX)
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            :
!
!     V0.02  240394  Jos van Gils  Modify for hybrid coupling
!     V0.01  040894  Jos van Gils  First version
!
!     FUNCTION            : Writes PDF for charon call
!
!     SUBROUTINES CALLED  : -
!
!     FILES               : -
!
!     COMMON BLOCKS       : -
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!
!     Declarations
!
      CHARACTER*12 FILNAM
      CHARACTER*6  MODNAM, PRONAM
      CHARACTER*50 PRODES
      INTEGER      LU    , NINPUT, I     , NOUTPU, NTRANS, NSPEC ,
     J             NTOX  , NNOTRA
      PARAMETER   (NINPUT = 5, NOUTPU = 8)
      CHARACTER*50 INPDES(NINPUT), OUTDES(NOUTPU), FLXDES
      CHARACTER*10 INPNAM(NINPUT), OUTNAM(NOUTPU), VARNAM(NTRANS)
      CHARACTER*6  NAMSPE(NSPEC ), NAMTOX(NTOX)
      REAL         INPVAL(NINPUT)
      INTEGER      IOUTP(NSPEC)

      DATA MODNAM /'D40CHA'/
      DATA PRONAM /'CHARON'/
      DATA PRODES /'CHARON chemical equilibrium module                '/
      DATA INPNAM /'TimMultCh ',
     J             'Delt      ',
     J             'TEMP      ',
     J             'IM1       ',
     J             'transporte'/
      DATA INPDES /'ratio bloom/delwaq timesteps                   (-)',
     J             'DELWAQ timestep                                (d)',
     J             'ambient water temperature                     (øC)',
     J             'suspended solids                            (g/m3)',
     J             'element of transport vector                 (g/m3)'/
      DATA INPVAL / 1.0, -999.,  15., 10., -999. /
      DATA OUTNAM /'pH        ',
     J             'Alka      ',
     J             'NH4       ',
     J             'PO4       ',
     J             'ssssss    ',
     J             'QmmIM1    ',
     J             'FrmmDis   ',
     J             'KdmmCEC   '/
      DATA OUTDES /'pH                                             (-)',
     J             'alkalinity                            (MoleOH-/m3)',
     J             'total ammonium CHARON                       (g/m3)',
     J             'total phosphates CHARON                     (g/m3)',
     J             'species                                     (g/m3)',
     J             'quality of solid phase                     (g/gDM)',
     J             'dissolved fraction                             (-)',
     J             'partition coefficient                      (m3/kg)'/
      DATA FLXDES /'derivative of el. transport vector        (g/m3/d)'/

!     Construct filename and open file

      WRITE ( FILNAM , '(A6,''.pdf  '')' ) PRONAM
      OPEN ( LU , FILE=FILNAM )

!     PROCESS NAME

      WRITE (LU,1000) PRONAM,PRODES

!     Module name

      WRITE (LU,1010) MODNAM

!     Value of TRswitch

      WRITE (LU,1011)

!     Input parameters

      WRITE (LU,1020) NINPUT-1 + NTRANS
!     Singular input quantities
      DO 10 I = 1,NINPUT-1
   10 WRITE (LU,1030) INPNAM(I), INPVAL(I), INPDES(I)
!     Transported vector
      DO 11 I = 1,NTRANS
          WRITE (INPNAM(NINPUT),'(A10)') VARNAM(I)
   11 WRITE (LU,1030) INPNAM(NINPUT), INPVAL(NINPUT), INPDES(NINPUT)
      WRITE (LU,1021)

!     Output parameters

      WRITE (LU,1040) NOUTPU-4 + NNOTRA + NTOX*3
!     Singular output quantities
      DO 20 I = 1,NOUTPU-4
   20 WRITE (LU,1050) OUTNAM(I), OUTDES(I)
!     CHARON compounds
      DO 21 I = 1,NSPEC
          IF ( IOUTP(I) .GT. 0 ) THEN
              WRITE (OUTNAM(NOUTPU-3)( 1: 6),'(A6)') NAMSPE(I)
              WRITE (LU,1050) OUTNAM(NOUTPU-3), OUTDES(NOUTPU-3)
          ENDIF
   21 CONTINUE
!     Concentration of solid metals
      DO 22 I = 1,NTOX
          WRITE (OUTNAM(NOUTPU-2)( 2: 3),'(A2)') NAMTOX(I)(1:2)
   22 WRITE (LU,1050) OUTNAM(NOUTPU-2), OUTDES(NOUTPU-2)
!     Dissolved fraction of metals
      DO 23 I = 1,NTOX
          WRITE (OUTNAM(NOUTPU-1)( 3: 4),'(A2)') NAMTOX(I)(1:2)
   23 WRITE (LU,1050) OUTNAM(NOUTPU-1), OUTDES(NOUTPU-1)
!     Kd metals
      DO 24 I = 1,NTOX
          WRITE (OUTNAM(NOUTPU)( 3: 4),'(A2)') NAMTOX(I)(1:2)
   24 WRITE (LU,1050) OUTNAM(NOUTPU), OUTDES(NOUTPU)
      WRITE (LU,1041)

!     Fluxes

      WRITE (LU,1060) NTRANS
      DO 31 I = 1,NTRANS
   31 WRITE (LU,1051) VARNAM(I)(1:6),VARNAM(I)(8:10),FLXDES

!     Basic stochiometry

      WRITE (LU,1070) NTRANS
      DO 41 I = 1,NTRANS
   41 WRITE (LU,1080) VARNAM(I),VARNAM(I)(1:6),VARNAM(I)(8:10)

!     End lines: close file

      WRITE (LU,1081)
      WRITE (LU,1082)
      WRITE (LU,1090) FILNAM(1:6)
      CLOSE ( LU )

      RETURN
 1000 FORMAT (A6,24X,A50)
 1010 FORMAT (A6,6X,'; Name of module')
 1011 FORMAT ('123       ; value of TRswitch')
 1020 FORMAT (I3,9X,'; Number of input variables on segment level')
 1021 FORMAT ('0         ; Number of input variables on exchange level')
 1030 FORMAT (A10,F10.3,10X,A50)
 1040 FORMAT (I3,9X,'; Number of output variables on segment level')
 1041 FORMAT ('0         ; Number of output variables on exchange level')
 1050 FORMAT (A10,20X,A50)
 1051 FORMAT ('c',A6,A3,20X,A50)
 1060 FORMAT (I3,9X,'; Number of fluxes')
 1070 FORMAT (I3,9X,'; Basic stochiometry')
 1080 FORMAT (A10,2X,'c',A6,A3,2X,'  1.00')
 1081 FORMAT ('0         ; Number of basic stochiometry terms dispersion')
 1082 FORMAT ('0         ; Number of basic stochiometry terms velocity')
 1090 FORMAT ('END ',A6)
      END
