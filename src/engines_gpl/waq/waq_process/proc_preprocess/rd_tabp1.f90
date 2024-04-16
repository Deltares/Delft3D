!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
      module m_rd_tabp1
      use m_waq_precision


      implicit none

      contains


      SUBROUTINE RD_TABP1 ( DEFFDS      , & 
                           NO_SGRP_MAX , NO_SGRP     , & 
                           SGRP_ID     , SGRP_NAME   , & 
                           LUNREP      , IERROR      )
!
!     Deltares
!
!     CREATED            :  june 1999 by Jan van Beek
!
!     FUNCTION           :  Read TABLE_P1 group from NEFIS file
!
!     FILES              :  NEFIS file assumed opened
!
!     SUBROUTINES CALLED :
!
!     ARGUMENTS
!
!     NAME         TYPE     LENGTH       FUNCT.  DESCRIPTION
!     ----         -----    ------       ------- -----------
!     DEFFDS       INT      2993         I/O     Definition file descriptor
!     DATFDS       INT      999          I/O     Data file descriptor
!     NO_SGRP_MAX  INT      1            I       number of substance groups
!     NO_SGRP      INT      1            I       number of substance groups
!     ITEM_ID      CHA*30   NO_SGRP_MAX  I       unique substance group identification
!     ITEM_NAME    CHA*50   NO_SGRP_MAX  I       substance group name
!     LUNREP       INT      1            I       Unit number report file
!     IERROR       INT      1            O       Error
!
!     IMPLICIT NONE for extra compiler checks
!     SAVE to keep the group definition intact
!
      IMPLICIT NONE
      SAVE
!
!     declaration of arguments
!
      INTEGER(kind=int_wp) ::NO_SGRP_MAX , NO_SGRP     , & 
                   LUNREP      , IERROR
      INTEGER(kind=int_wp) ::DEFFDS
      character(len=30)  SGRP_ID     (NO_SGRP_MAX)
      character(len=50)  SGRP_NAME   (NO_SGRP_MAX)
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
      PARAMETER   ( NELEMS = 3 )
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
      DATA  GRPNAM  /'TABLE_P1'/
      DATA & 
      (ELMNMS(I),ELMTPS(I),NBYTSG(I),ELMDMS(1,I),ELMDMS(2,I),ELMDES(I), & 
       I = 1 , NELEMS) & 
     /'NO_SGRP'  ,'INTEGER'  , 4,1,1,'number of substance groups'     , & 
      'SGRP_ID'  ,'CHARACTER',30,1,0,'unique group identification'    , & 
      'SGRP_NAME','CHARACTER',50,1,0,'substance group name'           /
!
!     Read group
!
      UINDEX(1) = 1
      UINDEX(2) = 1
      UINDEX(3) = 1

      BUFLEN = NBYTSG(1)*ELMDMS(2,1)
      IERROR = GETELT (DEFFDS , & 
                      GRPNAM , ELMNMS(1), & 
                      UINDEX , 1        , & 
                      BUFLEN , NO_SGRP  )
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(1)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      IF ( NO_SGRP > NO_SGRP_MAX ) THEN
         WRITE(LUNREP,*) 'ERROR reading group',GRPNAM
         WRITE(LUNREP,*) 'Actual number of substance groups:',NO_SGRP
         WRITE(LUNREP,*) 'greater than maximum:',NO_SGRP_MAX
         IERROR = 1
         GOTO 900
      ENDIF
!
!     Set dimension of table
!
      DO IELM = 2 , NELEMS
         ELMDMS(2,IELM) = NO_SGRP
      ENDDO

      BUFLEN = NBYTSG(2)*ELMDMS(2,2)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(2), & 
                      UINDEX , 1        , & 
                      BUFLEN , SGRP_ID  )
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(2)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF

      BUFLEN = NBYTSG(3)*ELMDMS(2,3)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(3), & 
                      UINDEX , 1        , & 
                      BUFLEN , SGRP_NAME)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(3)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!
  900 CONTINUE
      RETURN
!
      END

      end module m_rd_tabp1
