!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
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
!  
!  

      SUBROUTINE PDFNEF ( LUNREP, SERIAL, VERSIO, IERROR)
!     
!          Deltares
!     
!          CREATED            : june 1999 by Jan van Beek
!     
!          FUNCTION           : Convert process definition tables to NEFIS format
!     
!          LOGICAL UNITS      :
!     
!          SUBROUTINES CALLED :
!     
!          ARGUMENTS
!     
!          NAME    TYPE     LENGTH     FUNCT.  DESCRIPTION
!          ----    -----    ------     ------- -----------
!          LUNREP       INT    1       I       Unit number report file
!          IERROR       INT    1       O       Error
!     
!          IMPLICIT NONE for extra compiler checks
      
      use m_obtain_number_decimals
!     
!          Common declarations
!     
      use m_waqpb_data

!     
      IMPLICIT NONE
!     
!          Declaration of arguments
!     
      INTEGER       LUNREP      , SERIAL     , & 
                   IERROR      , lunfil
      REAL          VERSIO
      character(len=10) num_decimals_version_char
      integer       num_decimals_version
!     
!          Declaration of file identification group
!     
      REAL          VFFORM
      CHARACTER*20  RUNDAT
      CHARACTER*40  FFORM      , CONTEN      , & 
                   SOURCE
      CHARACTER*40  REMARK(4)
!     
!          Local variables
!     
!          DEFNAM  CHAR*255    1       LOCAL   name definition file
!          DATNAM  CHAR*255    1       LOCAL   name data file
!     
      INTEGER       IC              , IP          , & 
                   ITEL
      INTEGER       DEFFDS
      CHARACTER*1   CODING
      CHARACTER*2   ACCESS
      CHARACTER*255 DEFNAM          , DATNAM
!     
!          External NEFIS Functions
!     
      INTEGER   CLSNEF & 
              ,CRENEF
      EXTERNAL  CLSNEF & 
              ,CRENEF
!     
!          Initialize proces definition file
!     
      DEFNAM = 'proc_def.def'
      DATNAM = 'proc_def.dat'
      WRITE(LUNREP,*) 'opening NEFIS DEF file:',DEFNAM
      WRITE(LUNREP,*) 'opening NEFIS DAT file:',DEFNAM
!     
!          delete existing NEFIS files
!     
      CALL delete_file ( DEFNAM, IERROR )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*)'ERROR deleting existing NEFIS DEF file:',DEFNAM
         WRITE(LUNREP,*)'ERROR number:',IERROR
         GOTO 900
      ENDIF
      CALL delete_file ( DATNAM, IERROR )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*)'ERROR deleting existing NEFIS DAT file:',DATNAM
         WRITE(LUNREP,*)'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Open NEFIS file
!     
      ACCESS = 'U'
      CODING = 'N'
      IERROR = CRENEF(DEFFDS, DATNAM, DEFNAM, CODING, ACCESS)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR opening NEFIS file:',TRIM(DATNAM)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Table P1 (SUBSTANCE GROUPS)
!     
      CALL WR_TABP1 ( DEFFDS      , & 
                     NSGRP       , SGRPID      , & 
                     SGRPNM      , LUNREP      , & 
                     IERROR      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing table P1'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Table P2 (ITEMS)
!     
      CALL WR_TABP2 ( DEFFDS      , & 
                     NITEM       , ITEMID      , & 
                     ITEMNM      , ITEMUN      , & 
                     ITEMDE      , ITEMAG      , & 
                     ITEMDA      , ITEMGR      , & 
                     ITEMSE      , ITEMWK      , &
                     LUNREP      , IERROR      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing table P2'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Table P3 (PROCESS MODULES)
!     
      CALL WR_TABP3 ( DEFFDS      , & 
                     NFORT       , FORTID      , & 
                     LUNREP      , IERROR      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing table P3'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Table P4 (PROCESSES)
!     
      CALL WR_TABP4 ( DEFFDS      , & 
                     NPROC       , PROCID      , & 
                     PROCNM      , PROCFO      , & 
                     PROCCO      , LUNREP      , & 
                     IERROR      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing table P4'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Table P5 (CONFIGURATIONS)
!     
      CALL WR_TABP5 ( DEFFDS      , & 
                     NCONF       , CONFID      , & 
                     CONFNM      , LUNREP      , & 
                     IERROR      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing table P5'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Table R1 (CONFIGURATIONS-PROCESSES)
!     
      ITEL = 1
      DO IP = 1 , NPROC
         DO IC = 1 , NCONF
            IF ( CONPRO(IC,IP) ) THEN
               ICNPRO(ITEL) = 1
            ELSE
               ICNPRO(ITEL) = 0
            ENDIF
            ITEL = ITEL + 1
         ENDDO
      ENDDO
      CALL WR_TABR1 ( DEFFDS      , & 
                     NCONF       , NPROC       , & 
                     ICNPRO      , LUNREP      , & 
                     IERROR      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing table R1'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Table R2 (CONFIGURATIONS-SUBSTANCES)
!     
      CALL WR_TABR2 ( DEFFDS      , & 
                     NCNSB       , R2_CID      , & 
                     R2_SID      , LUNREP      , & 
                     IERROR      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing table R2'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Table R3 (INPUT ITEMS)
!     
      CALL WR_TABR3 ( DEFFDS      , & 
                     NINPU       , INPUPR      , & 
                     INPUIT      , INPUNM      , & 
                     INPUDE      , INPUDO      , & 
                     INPUSX      , LUNREP      , & 
                     IERROR      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing table R3'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Table R4 (OUTPUT ITEMS)
!     
      CALL WR_TABR4 ( DEFFDS      , & 
                     NOUTP       , OUTPPR      , & 
                     OUTPIT      , OUTPNM      , & 
                     OUTPDO      , OUTPSX      , & 
                     LUNREP      , IERROR      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing table R4'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Table R5 (OUTPUT FLUXES)
!     
      CALL WR_TABR5 ( DEFFDS      , & 
                     NOUTF       , OUTFPR      , & 
                     OUTFFL      , OUTFNM      , & 
                     OUTFDO      , LUNREP      , & 
                     IERROR      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing table R5'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Table R6 (FLUX-SUBSTANCE)
!     
      CALL WR_TABR6 ( DEFFDS      , & 
                     NSTOC       , STOCFL      , & 
                     STOCSU      , STOCSC      , & 
                     LUNREP      , IERROR      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing table R6'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Table R7 (VELOCITY-SUBSTANCE)
!     
      CALL WR_TABR7 ( DEFFDS      , & 
                     NVELO       , VELOIT      , & 
                     VELOSU      , VELOSC      , & 
                     LUNREP      , IERROR      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing table R7'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Table R8 (DISPERSION-SUBSTANCE)
!     
      CALL WR_TABR8 ( DEFFDS      , & 
                     NDISP       , DISPIT      , & 
                     DISPSU      , DISPSC      , & 
                     LUNREP      , IERROR      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing table R8'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Table R9 (CONFIGURATIONS-MODELLED VARIABLES)
!     
      CALL WR_TABR9 ( DEFFDS      , & 
                     NMODV       , MODVCI      , & 
                     MODVIT      , LUNREP      , & 
                     IERROR      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing table R9'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Table M1 (old_items)
!     
      CALL WR_TABM1 ( DEFFDS      , & 
                     n_old_items, & 
                     old_items_old_name, & 
                     old_items_new_name, & 
                     old_items_old_default, & 
                     old_items_configuration, & 
                     old_items_serial, & 
                     old_items_action_type, & 
                     LUNREP      , & 
                     IERROR      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing table M1'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Indices group
!     
      CALL WR_INDICES( DEFFDS      , & 
                      R2_IIN      , NCNSB       , & 
                      INPUII      , NINPU       , & 
                      INPUPI      , OUTPII      , & 
                      NOUTP       , OUTPPI      , & 
                      LUNREP      , IERROR      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing INDICES'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Last FILE IDENTIFICATION GROUP
!     
      FFORM  = 'DELWAQ PROCESS DEFINITION FILE'
      VFFORM = 2.00
      CONTEN = 'DELWAQ PROCESS DEFINITION FILE'
      CALL DATTIM(RUNDAT)
      SOURCE = 'Deltares'
      REMARK = ' '
      REMARK(1) = '@(#)Deltares, DELWAQ Process Definition '
      
      ! obtain number of decimals of version number
      num_decimals_version = obtain_num_decimals_version(versio)
      write(num_decimals_version_char,'(I10)') num_decimals_version
      
      WRITE(REMARK(2), '(A12,F5.'//num_decimals_version_char//',A1,I10,A2,A10)') & 
       'File Version',VERSIO, '.', SERIAL, ', ', RUNDAT(1:10)
      REMARK(3) = RUNDAT(11:20)
      REMARK(4) = ' '
      CALL WR_FILID ( DEFFDS, FFORM , VFFORM, CONTEN, & 
                     VERSIO, SERIAL, RUNDAT, SOURCE, REMARK, & 
                     LUNREP, IERROR)

      open(newunit=lunfil, file='filid.pptex')
      write(lunfil,'(a30,'' colsep '', a20, '' \\'')') & 
              'Creation date', rundat
      write(lunfil,'(a30,'' colsep '', i10, '' \\'')') & 
              'File serial number', serial
      write(lunfil,'(a30,'' colsep '', f12.3, '' \\'')') & 
              'Version processes library', versio
      close(lunfil)

      open(newunit=lunfil, file='conf_name.pptex')
      do ic = 1, nconf
          write(lunfil,'(a10,'' colsep '', a50, '' \\'')') & 
                   confid(ic), confnm(ic)
      enddo
      close(lunfil)

      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing file identification group'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Close files
!     
      IERROR = CLSNEF(DEFFDS)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR closing nefis file'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
      WRITE(LUNREP,*) 'closing NEFIS file'
      WRITE(LUNREP,*) 'NEFIS file written successfully'
!     
  900 CONTINUE
      RETURN
!     
      END
