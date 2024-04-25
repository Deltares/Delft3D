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

      SUBROUTINE WR_TABM1 ( DEFFDS      , & 
                           n_old_items, & 
                           old_items_old_name, & 
                           old_items_new_name, & 
                           old_items_old_default, & 
                           old_items_configuration, & 
                           old_items_serial, & 
                           old_items_action_type, & 
                           LUNREP      , IERROR      )
!     
!          Deltares
!     
!          CREATED            :  july 2012 by Jan van Beek
!     
!          FUNCTION           :  Write TABLE_M1 group to NEFIS file
!     
!          FILES              :  NEFIS file assumed opened
!     
!          IMPLICIT NONE for extra compiler checks
!          SAVE to keep the group definition intact
!     
      IMPLICIT NONE
      SAVE
!     
!          declaration of arguments
!     
      integer            :: deffds                                       ! nefis file descriptor
      integer            :: n_old_items                                  ! number of old items
      character*10       :: old_items_old_name(n_old_items)              ! old name (if equal to new name then use old_default if target serial is less then
      character*10       :: old_items_new_name(n_old_items)              ! new name
      real               :: old_items_old_default(n_old_items)           ! old default value
      character*10       :: old_items_configuration(n_old_items)         ! (only use this new name if a specific configuration is used?)
      integer            :: old_items_serial(n_old_items)                ! the proces definition serial number up to where this old name, old default was used
      integer            :: old_items_action_type(n_old_items)           ! process rename, process parameter rename, default value change
      integer            :: lunrep                                       ! report file
      integer            :: ierror                                       ! error
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
      CHARACTER*16  GRPNAM
      CHARACTER*16  ELMNMS(NELEMS)  , ELMTPS(NELEMS)
      CHARACTER*64  ELMDES(NELEMS)
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
      DATA  GRPNAM  /'TABLE_M1'/
      DATA & 
      (ELMNMS(I),ELMTPS(I),NBYTSG(I),ELMDMS(1,I),ELMDMS(2,I),ELMDES(I), & 
       I = 1 , NELEMS) & 
     /'n_old_items'  ,'INTEGER'  , 4,1,1,'number of old items', & 
      'old_name'     ,'CHARACTER',10,1,0,'old name', & 
      'new_name'     ,'CHARACTER',10,1,0,'new name', & 
      'old_default'  ,'REAL'     , 4,1,0,'old default value', & 
      'configuration','CHARACTER',10,1,0,'configuration', & 
      'serial'       ,'INTEGER'  , 4,1,0,'serial number when changed', & 
      'action_type'  ,'INTEGER'  , 4,1,0,'type of change'/
!     
!          Set dimension of table
!     
      DO IELM = 2 , NELEMS
         ELMDMS(2,IELM) = n_old_items
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
                      n_old_items       )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(1)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(2)
      IERROR = PUTELS (DEFFDS , & 
                      GRPNAM , ELMNMS(2), & 
                      UINDEX , 1        , & 
                      old_items_old_name)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(2)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(3)
      IERROR = PUTELS (DEFFDS , & 
                      GRPNAM , ELMNMS(3), & 
                      UINDEX , 1        , & 
                      old_items_new_name)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(3)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(4)
      IERROR = PUTELS (DEFFDS , & 
                      GRPNAM , ELMNMS(4), & 
                      UINDEX , 1        , & 
                      old_items_old_default)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(4)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(5)
      IERROR = PUTELT (DEFFDS , & 
                      GRPNAM , ELMNMS(5), & 
                      UINDEX , 1        , & 
                      old_items_configuration)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(5)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(6)
      IERROR = PUTELS (DEFFDS , & 
                      GRPNAM , ELMNMS(6), & 
                      UINDEX , 1        , & 
                      old_items_serial)
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR writing element',ELMNMS(6)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      WRITE(LUNREP,*) ' WRITING ELEMENT:',ELMNMS(7)
      IERROR = PUTELS (DEFFDS , & 
                      GRPNAM , ELMNMS(7), & 
                      UINDEX , 1        , & 
                      old_items_action_type)
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
