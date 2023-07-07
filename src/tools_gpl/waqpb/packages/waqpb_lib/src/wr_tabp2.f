!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
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

      SUBROUTINE WR_TABP2 ( DEFFDS      ,
     +                      NO_ITEM     , ITEM_ID     ,
     +                      ITEM_NAME   , ITEM_UNIT   ,
     +                      ITEM_DEFAULT, ITEM_AGGREGA,
     +                      ITEM_DISAGGR, ITEM_GROUPID,
     +                      ITEM_SEGX   , ITEM_WK     ,
     +                      LUNREP      , IERROR      )
C
C     Deltares
C
C     CREATED            :  june 1999 by Jan van Beek
C
C     FUNCTION           :  Write TABLE_P2 group to NEFIS file
C
C     FILES              :  NEFIS file assumed opened
C
C     SUBROUTINES CALLED :
C
C     ARGUMENTS
C
C     NAME    TYPE     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     DEFFDS       INT    2993    I/O     Definition file descriptor
C     DATFDS       INT    999     I/O     Data file descriptor
C     NO_ITEM      INT            I       number of items
C     ITEM_ID      CHA*10 NO_ITEM I       unique item identification
C     ITEM_NAME    CHA*50 NO_ITEM I       item name
C     ITEM_UNIT    CHA*20 NO_ITEM I       unit
C     ITEM_DEFAULT REA    NO_ITEM I       default value
C     ITEM_AGGREGA CHA*10 NO_ITEM I       variable used for aggregation
C     ITEM_DISAGGR CHA*10 NO_ITEM I       variable used for dis-aggregation
C     ITEM_GROUPID CHA*30 NO_ITEM I       substance group ID
C     ITEM_SEGX    CHA*1  NO_ITEM I       segment / exchange indication
C     ITEM_WK      CHA*1  NO_ITEM I       active / inactive indication
C     LUNREP       INT    1       I       Unit number report file
C     IERROR       INT    1       O       Error
C
C     IMPLICIT NONE for extra compiler checks
C     SAVE to keep the group definition intact
C
      IMPLICIT NONE
      SAVE
C
C     declaration of arguments
C
      INTEGER       NO_ITEM     , LUNREP     ,
     +              IERROR
      INTEGER       DEFFDS
      CHARACTER*10  ITEM_ID     (NO_ITEM)
      CHARACTER*50  ITEM_NAME   (NO_ITEM)
      CHARACTER*20  ITEM_UNIT   (NO_ITEM)
      REAL          ITEM_DEFAULT(NO_ITEM)
      CHARACTER*10  ITEM_AGGREGA(NO_ITEM)
      CHARACTER*10  ITEM_DISAGGR(NO_ITEM)
      CHARACTER*30  ITEM_GROUPID(NO_ITEM)
      CHARACTER*1   ITEM_SEGX   (NO_ITEM)
      CHARACTER*1   ITEM_WK     (NO_ITEM)
C
C     Local variables
C
C     GRPNAM  CHAR*16     1       LOCAL   group name (table)
C     NELEMS  INTEGER     1       LOCAL   number of elements in group (=cell)
C     ELMNMS  CHAR*16  NELEMS     LOCAL   name of elements on file
C     ELMTPS  CHAR*16  NELEMS     LOCAL   type of elements
C     ELMDMS  INTEGER  6,NELEMS   LOCAL   dimension of elements
C     NBYTSG  INTEGER  NELEMS     LOCAL   length of elements (bytes)
C
      INTEGER       NELEMS
      PARAMETER   ( NELEMS = 10 )
C
      INTEGER       I               , IELM
      INTEGER       ELMDMS(2,NELEMS), NBYTSG(NELEMS),
     +              UINDEX(3)
      CHARACTER*16  GRPNAM
      CHARACTER*16  ELMNMS(NELEMS)  , ELMTPS(NELEMS)
      CHARACTER*64  ELMDES(NELEMS)
C
C     External NEFIS Functions
C
      INTEGER   CREDAT
     +         ,DEFCEL
     +         ,DEFELM
     +         ,DEFGRP
     +         ,FLSDAT
     +         ,FLSDEF
     +         ,PUTELS
     +         ,PUTELT
      EXTERNAL  CREDAT
     +         ,DEFCEL
     +         ,DEFELM
     +         ,DEFGRP
     +         ,FLSDAT
     +         ,FLSDEF
     +         ,PUTELS
     +         ,PUTELT
C
C     element names
C
      DATA  GRPNAM  /'TABLE_P2'/
      DATA
     + (ELMNMS(I),ELMTPS(I),NBYTSG(I),ELMDMS(1,I),ELMDMS(2,I),ELMDES(I),
     +  I = 1 , NELEMS)
     +/'NO_ITEM','INTEGER'  , 4,1,1,'number of items'                  ,
     + 'ITEM_ID','CHARACTER',10,1,0,'unique item identification'       ,
     + 'ITEM_NM','CHARACTER',50,1,0,'item name'                        ,
     + 'UNIT'   ,'CHARACTER',20,1,0,'unit'                             ,
     + 'DEFAULT','REAL'     , 4,1,0,'default value'                    ,
     + 'AGGREGA','CHARACTER',10,1,0,'variable used for aggregation    ',
     + 'DISAGGR','CHARACTER',10,1,0,'variable used for dis-aggregation',
     + 'GROUPID','CHARACTER',30,1,0,'substance group ID               ',
     + 'SEG_EXC','CHARACTER', 1,1,0,'segment / exchange indication    ',
     + 'WK'     ,'CHARACTER', 1,1,0,'active / inactive indication     '/
C
C     Set dimension of table
C
      DO IELM = 2 , NELEMS
         ELMDMS(2,IELM) = NO_ITEM
      ENDDO
C
C     Define elements
C
      WRITE(LUNREP,*) ' WRITING GROUP:',GRPNAM
      DO IELM = 1 , NELEMS
         IERROR = DEFELM (DEFFDS        , ELMNMS(IELM)  ,
     +                    ELMTPS(IELM)  , NBYTSG(IELM)  ,
     +                    ' '           , ' '           ,
     +                    ELMDES(IELM)  , ELMDMS(1,IELM),
     +                    ELMDMS(2,IELM))
         IF ( IERROR .NE. 0 ) THEN
            WRITE(LUNREP,*) 'ERROR defining element:',ELMNMS(IELM)
            WRITE(LUNREP,*) 'ERROR number:',IERROR
            GOTO 900
         ENDIF
      ENDDO
C
C     Define group
C
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
C
C     Nu het schrijven
C
      UINDEX(1) = 1
      UINDEX(2) = 1
      UINDEX(3) = 1
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(1)
      IERROR = PUTELT (DEFFDS ,
     +                 GRPNAM , ELMNMS(1),
     +                 UINDEX , 1        ,
     +                 NO_ITEM           )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(1)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(2)
      IERROR = PUTELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(2),
     +                 UINDEX , 1        ,
     +                 ITEM_ID           )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(2)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(3)
      IERROR = PUTELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(3),
     +                 UINDEX , 1        ,
     +                 ITEM_NAME         )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(3)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(4)
      IERROR = PUTELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(4),
     +                 UINDEX , 1        ,
     +                 ITEM_UNIT         )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(4)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(5)
      IERROR = PUTELT (DEFFDS ,
     +                 GRPNAM , ELMNMS(5),
     +                 UINDEX , 1        ,
     +                 ITEM_DEFAULT      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(5)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(6)
      IERROR = PUTELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(6),
     +                 UINDEX , 1        ,
     +                 ITEM_DISAGGR      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(6)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(7)
      IERROR = PUTELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(7),
     +                 UINDEX , 1        ,
     +                 ITEM_AGGREGA      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(7)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(8)
      IERROR = PUTELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(8),
     +                 UINDEX , 1        ,
     +                 ITEM_GROUPID      )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(8)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(9)
      IERROR = PUTELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(9),
     +                 UINDEX , 1        ,
     +                 ITEM_SEGX         )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(9)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(10)
      IERROR = PUTELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(10),
     +                 UINDEX , 1         ,
     +                 ITEM_WK            )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(10)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
C
  900 CONTINUE
      RETURN
C
      END
