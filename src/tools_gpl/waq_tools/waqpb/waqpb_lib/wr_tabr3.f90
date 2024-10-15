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

      SUBROUTINE WR_TABR3 ( DEFFDS      , & 
                           NO_INPU     , R3_PID      , & 
                           R3_IID      , R3_NUMB     , & 
                           R3_DEFY     , R3_DOC      , & 
                           R3_SEX      , LUNREP      , & 
                           IERROR      )
!     
!          Deltares
!     
!          CREATED            :  june 1999 by Jan van Beek
!     
!          FUNCTION           :  Write TABLE_R3 group to NEFIS file
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
!          NO_INPU      INT            I       number of rows in table r3
!          R3_PID       CHA*10 NO_INPU I       process identification
!          R3_IID       CHA*10 NO_INPU I       item identification
!          R3_NUMB      INT    NO_INPU I       serial number
!          R3_DEFY      CHA*1  NO_INPU I       default y/n
!          R3_DOC       CHA*1  NO_INPU I       documented y/n
!          R3_SEX       INT    NO_INPU I       segment or exchange
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
      INTEGER       NO_INPU     , LUNREP     , & 
                   IERROR
      INTEGER       DEFFDS
      CHARACTER(len=10)  R3_PID      (NO_INPU)
      CHARACTER(len=10)  R3_IID      (NO_INPU)
      INTEGER       R3_NUMB(NO_INPU)
      CHARACTER(len=1)   R3_DEFY(NO_INPU)
      CHARACTER(len=1)   R3_DOC (NO_INPU)
      INTEGER       R3_SEX (NO_INPU)
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
      PARAMETER   ( NELEMS = 7 )
!     
      INTEGER       I               , IELM
      INTEGER       ELMDMS(2,NELEMS), NBYTSG(NELEMS), & 
                   UINDEX(3)
      CHARACTER(len=16)  GRPNAM
      CHARACTER(len=16)  ELMNMS(NELEMS)  , ELMTPS(NELEMS)
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
      DATA  GRPNAM  /'TABLE_R3'/
      DATA & 
      (ELMNMS(I),ELMTPS(I),NBYTSG(I),ELMDMS(1,I),ELMDMS(2,I),ELMDES(I), & 
       I = 1 , NELEMS) & 
     /'NO_INPU'  ,'INTEGER'  , 4,1,1,'number of rows  in table R3'    , & 
      'R3_PID'   ,'CHARACTER',10,1,0,'configuration identification'   , & 
      'R3_IID'   ,'CHARACTER',10,1,0,'configuration identification'   , & 
      'R3_NUMB'  ,'INTEGER'  , 4,1,0,'serial number in process'       , & 
      'R3_DEFY'  ,'CHARACTER', 1,1,0,'use default yes/no'             , & 
      'R3_DOC'   ,'CHARACTER', 1,1,0,'documented yes/no'              , & 
      'R3_SEX'   ,'INTEGER'  , 4,1,0,'segment/exchange indication'    /
!     
!          Set dimension of table
!     
      DO IELM = 2 , NELEMS
         ELMDMS(2,IELM) = NO_INPU
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
                      NO_INPU           )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(1)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(2)
      IERROR = PUTELS (DEFFDS , & 
                      GRPNAM , ELMNMS(2), & 
                      UINDEX , 1        , & 
                      R3_PID            )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(2)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(3)
      IERROR = PUTELS (DEFFDS , & 
                      GRPNAM , ELMNMS(3), & 
                      UINDEX , 1        , & 
                      R3_IID            )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(3)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(4)
      IERROR = PUTELT (DEFFDS , & 
                      GRPNAM , ELMNMS(4), & 
                      UINDEX , 1        , & 
                      R3_NUMB           )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(4)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(5)
      IERROR = PUTELS (DEFFDS , & 
                      GRPNAM , ELMNMS(5), & 
                      UINDEX , 1        , & 
                      R3_DEFY           )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(5)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(6)
      IERROR = PUTELS (DEFFDS , & 
                      GRPNAM , ELMNMS(6), & 
                      UINDEX , 1        , & 
                      R3_DOC            )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(6)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(7)
      IERROR = PUTELT (DEFFDS , & 
                      GRPNAM , ELMNMS(7), & 
                      UINDEX , 1        , & 
                      R3_SEX            )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(7)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!     
  900 CONTINUE
      RETURN
!     
      END
