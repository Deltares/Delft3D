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

      Function CH3BMEAS (IDEBUG, IOUT1) result(RetVal)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RTC version 1.0.                 Date: June 1997
! *********************************************************************
! *** Last update: June 1997         By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Carry out 3B/Maalstop measures
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IOUT1  = file unit number of output file with messages
! ***  RetVal = Return value; 0=ok, >0 = foutcode
! *********************************************************************

      Use ParameterModule
      Use LocationDataModule
      Use DecisionModule
      Use MeasureModule
!
      implicit none

      Integer :: RetVal

      LOGICAL CHKTRUE
      Double precision RVAL, RVal1, Rval2, Rval3
      INTEGER IDEBUG, IOUT1, IMEAS, IPRIOR, I3LOC, INDX, I, iMatLoc
!
      RetVal = 0
!
      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' Ch3bmeas')
!
! *********************************************************************
! *** Carry out 3B/Maalstop measures per maatregel
! *********************************************************************
!
      DO IMEAS = 1,N3MEAS
         RVAL  = DCVVAL(IXMS3P(IMEAS),1)

! ***   Check aanslag
         CHKTRUE = .FALSE.
         IF (ONCH3B(IMEAS) .EQ. '<') THEN
            CHKTRUE = (RVAL .LT. MSON3B(IMEAS))
         ELSEIF (ONCH3B(IMEAS) .EQ. '=') THEN
            CHKTRUE = (ABS (RVAL-MSON3B(IMEAS)) .LT. .0000001)
         ELSEIF (ONCH3B(IMEAS) .EQ. '>') THEN
            CHKTRUE = (RVAL .GT. MSON3B(IMEAS))
         ELSE
            CALL ERRMSG (920,0, ONCH3B(IMEAS),' 3B-measure file', IOUT1)
            RetVal = 920
            Return
         ENDIF

         IF (CHKTRUE) MSSTAT (IMEAS) = 1
         IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Aanslag 3B',IMEAS, MSSTAT(IMEAS)


! ***   Check afslag
         CHKTRUE = .FALSE.
         IF (OFCH3B(IMEAS) .EQ. '<') THEN
            CHKTRUE = (RVAL .LT. MSOFF3B(IMEAS))
         ELSEIF (OFCH3B(IMEAS) .EQ. '=') THEN
            CHKTRUE = (ABS (RVAL-MSOFF3B(IMEAS)) .LT. .0000001)
         ELSEIF (OFCH3B(IMEAS) .EQ. '>') THEN
            CHKTRUE = (RVAL .GT. MSOFF3B(IMEAS))
         ELSE
            CALL ERRMSG (920,0, OFCH3B(IMEAS), ' 3B-measure file', IOUT1)
            RetVal = 920
            Return
         ENDIF

         IF (CHKTRUE) MSSTAT (IMEAS) = 0
         IF (IDEBUG .GT. 0) WRITE(IDEBUG,*) ' Afslag 3B',IMEAS, MSSTAT(IMEAS)

         IF (IDEBUG .GT. 0) THEN
            WRITE (IDEBUG,*) ' Measure  ',MSID3B(IMEAS), IMEAS
            WRITE (IDEBUG,*) ' Beslispar',MSBP3B(IMEAS)
            WRITE (IDEBUG,*) '   index  ',IXMS3P(IMEAS)
            WRITE (IDEBUG,*) '   value  ',DCVVAL(IXMS3P(IMEAS),1)
            WRITE (IDEBUG,*) ' Check on ',MSON3B(IMEAS)
            WRITE (IDEBUG,*) ' Check    ',ONCH3B(IMEAS)
            WRITE (IDEBUG,*) ' Check of ',MSOFF3B(IMEAS)
            WRITE (IDEBUG,*) ' Check    ',OFCH3B(IMEAS)
            WRITE (IDEBUG,*) ' Status   ',MSSTAT(IMEAS)
         ENDIF
      ENDDO
!
! *********************************************************************
! *** Bepaal per 3B-lokatie wat er gebeurt
! *********************************************************************
!

      IF (IDEBUG .GT. 0) Write(Idebug,*) ' Check Matlab measures',Low3Pri

      DO IPRIOR = LOW3PRI, 1, -1
         DO I3LOC = 1,N3MLOC
            D3BSTA (I3LOC) = 0
            IF (LCPR3B(I3LOC) .EQ. IPRIOR) THEN
               IMEAS = IXMS3B(I3LOC)
!              WRITE(*,*) ' Ch3BMeas I3Loc IMeas =', I3Loc, imeas
               If (Imeas .ne. 0) then
                  D3BSTA (I3LOC) = MSSTAT(IMEAS)
               Else
!                 do nothing, D3BSTA (I3LOC) unchanged
               Endif
               INDX = IXID3B(I3LOC)
!              MS3BST (INDX) = MAX (MS3BST(INDX), FLOAT(D3BSTA(I3LOC)))
               MS3BST (INDX,2) = MS3BST(INDX,2) + D3BSTA(I3LOC)
!              Possible Matlab decisions at lower priority may be overruled
               MS3BST (INDX,3) = 0
               IF (IDEBUG .GT. 0) THEN
                 WRITE (IDEBUG,*) ' 3B-id    ',LCID3B(I3LOC), I3LOC
                 if (imeas .gt. 0) WRITE (IDEBUG,*) ' Measure  ',MSID3B(IMEAS), IMEAS
                 WRITE (IDEBUG,*) ' Priority ',LCPR3B(I3LOC)
                 if (Imeas .gt. 0) WRITE (IDEBUG,*) ' Status msstat',MSSTAT(IMEAS)
                 WRITE (IDEBUG,*) ' Status D3Bsta',D3BSTA(I3LOC)
                 WRITE (IDEBUG,*) ' Unieke index ',INDX
                 WRITE (IDEBUG,*) ' Status MS3BST',MS3BST(INDX,2)
               ENDIF
            ENDIF
         ENDDO
! Matlab measures
         DO I3LOC = N3MLOC+1,N3MLoc+N3MatLoc
            IF (LCPR3B(I3LOC) .EQ. IPRIOR) THEN
               IMatloc = I3Loc-N3MLoc
               IMEAS = N3MEAS + 4*(IMatLoc-1)
               RVAL  = DCVVAL(IXMS3P(IMEAS+1),1)
               RVAL1 = DCVVAL(IXMS3P(IMEAS+2),1)
               RVAL2 = DCVVAL(IXMS3P(IMEAS+3),1)
               RVAL3 = DCVVAL(IXMS3P(IMEAS+4),1)
               INDX = IXID3B(I3LOC)
!              MS3BST (INDX,2) = MS3BST(INDX,2) + 1
!              Set Matlab=on and set switch on/off levels
               MS3BST (INDX,3) = 1
               MS3BST (INDX,4) = Rval
               MS3BST (INDX,5) = Rval1
               MS3BST (INDX,6) = Rval2
               MS3BST (INDX,7) = Rval3
               IF (IDEBUG .GT. 0) THEN
                 WRITE (IDEBUG,*) ' Matlab measure imeas n3meas ', imeas, N3meas
                 WRITE (IDEBUG,*) ' I3Loc, N3MLoc N3Matloc ', I3Loc, N3MLoc, N3Matloc
                 WRITE (IDEBUG,*) ' 3B-id    ',LCID3B(I3LOC), I3LOC
                 if (IMeas .gt. 0) WRITE (IDEBUG,*) ' Measure  ',MSID3B(IMEAS), IMEAS
                 WRITE (IDEBUG,*) ' Priority ',LCPR3B(I3LOC)
                 if (Imeas .gt. 0) WRITE (IDEBUG,*) ' Status msstat',MSSTAT(IMEAS)
                 WRITE (IDEBUG,*) ' Status D3Bsta',D3BSTA(I3LOC)
                 WRITE (IDEBUG,*) ' Unieke index IxId3B',INDX
                 WRITE (IDEBUG,*) ' Status MS3BST',(MS3BST(INDX,I),I=2,7)
                 WRITE (IDEBUG,*) ' ipara value', IxMs3p(imeas+1), Dcvval(IxMs3p(Imeas+1),1)
                 WRITE (IDEBUG,*) ' ipara value', IxMs3p(imeas+2), Dcvval(IxMs3p(Imeas+2),1)
                 WRITE (IDEBUG,*) ' ipara value', IxMs3p(imeas+3), Dcvval(IxMs3p(Imeas+3),1)
                 WRITE (IDEBUG,*) ' ipara value', IxMs3p(imeas+4), Dcvval(IxMs3p(Imeas+4),1)
               ENDIF
            ENDIF
         ENDDO
      ENDDO

! *********************************************************************
! *** Zet maatregel uit(0) of aan (1) op basis van aantal active maatregelen
! *********************************************************************

      DO INDX = 1,N3BMS
         IF (MS3BST(INDX,2) .GT. 0)  MS3BST(INDX,1) = 1
      ENDDO


      RETURN
      END Function Ch3BMeas
