!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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

      SUBROUTINE WR_TABR7 ( DEFFDS      , & 
                           NO_VSTO     , R7_VID      , & 
                           R7_SID      , R7_SCAL     , & 
                           LUNREP      , IERROR      )
!     
!          Deltares
!     
!          CREATED            :  june 1999 by Jan van Beek
!     
!          FUNCTION           :  Write TABLE_R7 group to NEFIS file
!     
!          FILES              :  NEFIS file assumed opened
!     
!          SUBROUTINES CALLED :
!     
!          ARGUMENTS
!     
!          NAME    TYPE     LENGTH     FUNCT.  DESCRIPTION
!          ----    -----    ------     ------- -----------
!          DEFFDS       INT    2993    I/O     Definition file descriptor
!          DATFDS       INT    999     I/O     Data file descriptor
!          NO_VSTO      INT            I       number of rows in table r3
!          R7_VID       CHA*10 NO_VSTO I       velocity identification
!          R7_SID       CHA*10 NO_VSTO I       substance identification
!          R7_SCAL      REAL   NO_VSTO I       scale factor
!          LUNREP       INT    1       I       Unit number report file
!          IERROR       INT    1       O       Error
!     
!          IMPLICIT NONE for extra compiler checks
!          SAVE to keep the group definition intact
!     
      IMPLICIT NONE
      SAVE
!     
!          declaration of arguments
!     
      INTEGER       NO_VSTO     , LUNREP     , & 
                   IERROR
      INTEGER       DEFFDS
      CHARACTER(len=10)  R7_VID      (NO_VSTO)
      CHARACTER(len=10)  R7_SID      (NO_VSTO)
      REAL          R7_SCAL(NO_VSTO)
!     
!          Local variables
!     
!          GRPNAM  CHAR*16     1       LOCAL   group name (table)
!          NELEMS  INTEGER     1       LOCAL   number of elements in group (=cell)
!          ELMNMS  CHAR*16  NELEMS     LOCAL   name of elements on file
!          ELMTPS  CHAR*16  NELEMS     LOCAL   type of elements
!          ELMDMS  INTEGER  6,NELEMS   LOCAL   dimension of elements
!          NBYTSG  INTEGER  NELEMS     LOCAL   length of elements (bytes)
!     
      INTEGER       NELEMS
      PARAMETER   ( NELEMS = 4 )
!     
      INTEGER       I               , IELM
      INTEGER       ELMDMS(2,NELEMS), NBYTSG(NELEMS), & 
                   UINDEX(3)
      CHARACTER(len=16)  GRPNAM
      CHARACTER(len=16)  ELMNMS(NELEMS), ELMTPS(NELEMS)
      CHARACTER(len=64)  ELMDES(NELEMS)
!     
!          External NEFIS Functions
!     
      INTEGER   CREDAT & 
              ,DEFCEL & 
              ,DEFELM & 
              ,DEFGRP & 
              ,FLSDAT & 
              ,FLSDEF & 
              ,PUTELS & 
              ,PUTELT
      EXTERNAL  CREDAT & 
              ,DEFCEL & 
              ,DEFELM & 
              ,DEFGRP & 
              ,FLSDAT & 
              ,FLSDEF & 
              ,PUTELS & 
              ,PUTELT
!     
!          element names
!     
      DATA  GRPNAM  /'TABLE_R7'/
      DATA & 
      (ELMNMS(I),ELMTPS(I),NBYTSG(I),ELMDMS(1,I),ELMDMS(2,I),ELMDES(I), & 
       I = 1 , NELEMS) & 
     /'NO_VSTO'  ,'INTEGER'  , 4,1,1,'number of rows in table R7'     , & 
      'R7_VID'   ,'CHARACTER',10,1,0,'velocity identification'        , & 
      'R7_SID'   ,'CHARACTER',10,1,0,'substance identification'       , & 
      'R7_SCAL'  ,'REAL'     , 4,1,0,'scale factor'                   /
!     
!          Set dimension of table
!     
      DO IELM = 2 , NELEMS
         ELMDMS(2,IELM) = NO_VSTO
      ENDDO
!     
!          Define elements
!     
      WRITE(LUNREP,*) ' WRITING GROUP:',GRPNAM
      DO IELM = 1 , NELEMS
         IERROR = DEFELM (DEFFDS        , ELMNMS(IELM)  , & 
                         ELMTPS(IELM)  , NBYTSG(IELM)  , & 
                         ' '           , ' '           , & 
                         ELMDES(IELM)  , ELMDMS(1,IELM), & 
                         ELMDMS(2,IELM))
         IF ( IERROR .NE. 0 ) THEN
            WRITE(LUNREP,*) 'ERROR defining element:',ELMNMS(IELM)
            WRITE(LUNREP,*) 'ERROR number:',IERROR
            GOTO 900
         ENDIF
      ENDDO
!     
!          Define group
!     
      IERROR = DEFCEL (DEFFDS, GRPNAM, NELEMS, ELMNMS)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR defining cell for group',GRPNAM
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      IERROR = DEFGRP (DEFFDS, GRPNAM, GRPNAM, 1, 1, 1)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR defining group',GRPNAM
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      IERROR = CREDAT (DEFFDS, GRPNAM, GRPNAM)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR creating data',GRPNAM
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      IERROR = FLSDEF(DEFFDS)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR flushing definition file'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      IERROR = FLSDAT(DEFFDS)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR flushing data file'
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
!          Nu het schrijven
!     
      UINDEX(1) = 1
      UINDEX(2) = 1
      UINDEX(3) = 1
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(1)
      IERROR = PUTELT (DEFFDS , & 
                      GRPNAM , ELMNMS(1), & 
                      UINDEX , 1        , & 
                      NO_VSTO           )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(1)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(2)
      IERROR = PUTELS (DEFFDS , & 
                      GRPNAM , ELMNMS(2), & 
                      UINDEX , 1        , & 
                      R7_VID            )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(2)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(3)
      IERROR = PUTELS (DEFFDS , & 
                      GRPNAM , ELMNMS(3), & 
                      UINDEX , 1        , & 
                      R7_SID            )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(3)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(4)
      IERROR = PUTELT (DEFFDS , & 
                      GRPNAM , ELMNMS(4), & 
                      UINDEX , 1        , & 
                      R7_SCAL           )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(4)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
  900 CONTINUE
      RETURN
!     
      END
