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
      module m_rd_tabm1
      use m_waq_precision


      implicit none

      contains

      SUBROUTINE RD_TABM1 ( DEFFDS      , & 
                           n_old_items_max, & 
                           n_old_items, & 
                           old_items_old_name, & 
                           old_items_new_name, & 
                           old_items_old_default, & 
                           old_items_configuration, & 
                           old_items_serial, & 
                           old_items_action_type, & 
                           LUNREP      , IERROR      )
!
!     Deltares
!
!     CREATED            :  Aug 2012 by Jan van Beek
!
!     FUNCTION           :  Read TABLE_M1 (old_items) group from NEFIS file
!
!     FILES              :  NEFIS file assumed opened
!
!     IMPLICIT NONE for extra compiler checks
!     SAVE to keep the group definition intact
!
      IMPLICIT NONE
      SAVE
!
!     declaration of arguments
!
      integer(kind=int_wp) ::deffds                                       ! nefis file descriptor
      integer(kind=int_wp) ::n_old_items_max                              ! maximum number of old items
      integer(kind=int_wp) ::n_old_items                                  ! number of old items
      character(len=10)       :: old_items_old_name(n_old_items_max)          ! old name (if equal to new name then use old_default if target serial is less then
      character(len=10)       :: old_items_new_name(n_old_items_max)          ! new name
      real(kind=real_wp) ::old_items_old_default(n_old_items_max)       ! old default value
      character(len=10)       :: old_items_configuration(n_old_items_max)     ! (only use this new name if a specific configuration is used?)
      integer(kind=int_wp) ::old_items_serial(n_old_items_max)            ! the proces definition serial number up to where this old name, old default was used
      integer(kind=int_wp) ::old_items_action_type(n_old_items_max)       ! process rename, process parameter rename, default value change
      integer(kind=int_wp) ::lunrep                                       ! report file
      integer(kind=int_wp) ::ierror                                       ! error
!
!     Local variables
!
!     GRPNAM  CHAR*16     1       LOCAL   group name (table)
!     NELEMS  INTEGER     1       LOCAL   number of elements in group (=cell)
!     ELMNMS  CHAR*16  NELEMS     LOCAL   name of elements on file
!     ELMTPS  CHAR*16  NELEMS     LOCAL   type of elements
!     ELMDMS  INTEGER  6,NELEMS   LOCAL   dimension of elements
!     NBYTSG  INTEGER  NELEMS     LOCAL   length of elements (bytes)
!
      INTEGER(kind=int_wp) ::NELEMS
      PARAMETER   ( NELEMS = 7 )
!
      INTEGER(kind=int_wp) ::I               , IELM          , & 
                   BUFLEN
      INTEGER(kind=int_wp) ::ELMDMS(2,NELEMS), NBYTSG(NELEMS), & 
                   UINDEX(3)
      character(len=16)  GRPNAM
      character(len=16)  ELMNMS(NELEMS)  , ELMTPS(NELEMS)
      character(len=64)  ELMDES(NELEMS)
!
!     External NEFIS Functions
!
      INTEGER(kind=int_wp) ::GETELS & 
              ,GETELT
      EXTERNAL  GETELS & 
              ,GETELT
!
!     element names
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
!     Read all elements
!
      UINDEX(1) = 1
      UINDEX(2) = 1
      UINDEX(3) = 1

      BUFLEN = NBYTSG(1)*ELMDMS(2,1)
      IERROR = GETELT (DEFFDS , & 
                      GRPNAM , ELMNMS(1), & 
                      UINDEX , 1        , & 
                      buflen , n_old_items)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(1)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      IF ( n_old_items > n_old_items_max ) THEN
         WRITE(LUNREP,*) 'ERROR reading group',GRPNAM
         WRITE(LUNREP,*) 'Actual number of items:',n_old_items
         WRITE(LUNREP,*) 'greater than maximum:',n_old_items_max
         IERROR = 1
         GOTO 900
      ENDIF
!
!     Set dimension of table
!
      DO IELM = 2 , NELEMS
         ELMDMS(2,IELM) = n_old_items
      ENDDO

      BUFLEN = NBYTSG(2)*ELMDMS(2,2)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(2), & 
                      UINDEX , 1        , & 
                      BUFLEN , & 
                      old_items_old_name)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(2)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF

      BUFLEN = NBYTSG(3)*ELMDMS(2,3)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(3), & 
                      UINDEX , 1        , & 
                      BUFLEN , & 
                      old_items_new_name)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(3)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF

      BUFLEN = NBYTSG(4)*ELMDMS(2,4)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(4), & 
                      UINDEX , 1        , & 
                      BUFLEN , & 
                      old_items_old_default)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(4)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF

      BUFLEN = NBYTSG(5)*ELMDMS(2,5)
      IERROR = GETELT (DEFFDS , & 
                      GRPNAM , ELMNMS(5), & 
                      UINDEX , 1        , & 
                      BUFLEN , & 
                      old_items_configuration)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(5)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF

      BUFLEN = NBYTSG(6)*ELMDMS(2,6)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(6), & 
                      UINDEX , 1        , & 
                      BUFLEN , & 
                      old_items_serial)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(6)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF

      BUFLEN = NBYTSG(7)*ELMDMS(2,7)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(7), & 
                      UINDEX , 1        , & 
                      BUFLEN , & 
                      old_items_action_type)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(7)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!
  900 CONTINUE
      RETURN
!
      END

      end module m_rd_tabm1
