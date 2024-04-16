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
      module m_rd_filid
      use m_waq_precision


      implicit none

      contains


      SUBROUTINE RD_FILID ( DEFFDS,         FFORM , VFFORM, CONTEN, & 
                           VERSIO, SERIAL, RUNDAT, SOURCE, REMARK, & 
                           LUNREP, IERROR)
!
!     Deltares
!
!     CREATED            :  june 1999 by Jan van Beek
!
!     FUNCTION           :  Read File Identification group from NEFIS file
!
!     FILES              :  NEFIS file assumed opened
!
!     SUBROUTINES CALLED :
!
!     ARGUMENTS
!
!     NAME    TYPE     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     DEFFDS  INT      2993       I/O     Definition file descriptor
!     DATFDS  INT       999       I/O     Data file descriptor
!     FFORM   CH*40       1       O       File Format
!     VFFORM  REAL        1       O       Version File Format
!     CONTEN  CH*40       1       O       File contents
!     VERSIO  REAL        1       O       Version number process library
!     SERIAL  INT         1       O       File serial number
!     RUNDAT  CH*20       1       O       Creation date
!     SOURCE  CH*40       1       O       Source data
!     REMARK  CH*40       4       O       Remarks
!     LUNREP  INT         1       I       Unit number report file
!     IERROR  INT         1       O       Error
!
!     IMPLICIT NONE for extra compiler checks
!     SAVE to keep the group definition intact
!
      IMPLICIT NONE
      SAVE
!
!     declaration of arguments
!
      INTEGER(kind=int_wp) ::SERIAL      , LUNREP     , & 
                   IERROR
      INTEGER(kind=int_wp) ::DEFFDS
      REAL(kind=real_wp) ::VFFORM      , VERSIO
      character(len=20)  RUNDAT
      character(len=40)  FFORM       , CONTEN     , & 
                   SOURCE
      character(len=40)  REMARK(4)
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
      PARAMETER   ( NELEMS = 8 )
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
      DATA  GRPNAM  /'FILE_ID'/
      DATA & 
      (ELMNMS(I),ELMTPS(I),NBYTSG(I),ELMDMS(1,I),ELMDMS(2,I),ELMDES(I), & 
       I = 1 , NELEMS) & 
     /'FFORM ', 'CHARACTER', 40, 1, 1,'File Format                   ', & 
      'VFFORM', 'REAL'     ,  4, 1, 1,'Version File Format           ', & 
      'CONTEN', 'CHARACTER', 40, 1, 1,'File contents                 ', & 
      'VERSIO', 'REAL'     ,  4, 1, 1,'Version number process library', & 
      'SERIAL', 'INTEGER'  ,  4, 1, 1,'File serial number            ', & 
      'RUNDAT', 'CHARACTER', 20, 1, 1,'Creation date                 ', & 
      'SOURCE', 'CHARACTER', 40, 1, 1,'Source data                   ', & 
      'REMARK', 'CHARACTER', 40, 1, 4,'Remarks                       '/
!
!     Read group
!
      UINDEX(1) = 1
      UINDEX(2) = 1
      UINDEX(3) = 1

      BUFLEN = NBYTSG(1)*ELMDMS(2,1)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(1), & 
                      UINDEX , 1        , & 
                      BUFLEN , FFORM    )
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(1)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF

      BUFLEN = NBYTSG(2)*ELMDMS(2,2)
      IERROR = GETELT (DEFFDS , & 
                      GRPNAM , ELMNMS(2), & 
                      UINDEX , 1        , & 
                      BUFLEN , VFFORM   )
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(2)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF

      BUFLEN = NBYTSG(3)*ELMDMS(2,3)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(3), & 
                      UINDEX , 1        , & 
                      BUFLEN , CONTEN   )
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(3)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF

      BUFLEN = NBYTSG(4)*ELMDMS(2,4)
      IERROR = GETELT (DEFFDS , & 
                      GRPNAM , ELMNMS(4), & 
                      UINDEX , 1        , & 
                      BUFLEN , VERSIO   )
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(4)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF

      BUFLEN = NBYTSG(5)*ELMDMS(2,5)
      IERROR = GETELT (DEFFDS , & 
                      GRPNAM , ELMNMS(5), & 
                      UINDEX , 1        , & 
                      BUFLEN , SERIAL   )
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(5)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF

      BUFLEN = NBYTSG(6)*ELMDMS(2,6)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(6), & 
                      UINDEX , 1        , & 
                      BUFLEN , RUNDAT   )
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(6)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF

      BUFLEN = NBYTSG(7)*ELMDMS(2,7)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(7), & 
                      UINDEX , 1        , & 
                      BUFLEN , SOURCE   )
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(7)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF

      BUFLEN = NBYTSG(8)*ELMDMS(2,8)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(8), & 
                      UINDEX , 1        , & 
                      BUFLEN , REMARK   )
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(8)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!
  900 CONTINUE
      RETURN
!
      END

      end module m_rd_filid
