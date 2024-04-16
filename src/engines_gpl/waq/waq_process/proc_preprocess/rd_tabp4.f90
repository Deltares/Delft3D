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
      module m_rd_tabp4
      use m_waq_precision


      implicit none

      contains


      SUBROUTINE RD_TABP4 ( DEFFDS      , & 
                           NO_PROC_MAX , NO_PROC     , & 
                           PROC_ID     , PROC_NAME   , & 
                           PROC_FORT   , PROC_TRCO   , & 
                           LUNREP      , IERROR      )
!
!     Deltares
!
!     CREATED            :  june 1999 by Jan van Beek
!
!     FUNCTION           :  Read TABLE_P4 group from NEFIS file
!
!     FILES              :  NEFIS file assumed opened
!
!     SUBROUTINES CALLED :
!
!     ARGUMENTS
!
!     NAME    TYPE     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     DEFFDS       INT    2993    I/O     Definition file descriptor
!     DATFDS       INT    999     I/O     Data file descriptor
!     NO_PROC_MAX  INT            I       maximum number of processes
!     NO_PROC      INT            I       number of processes
!     PROC_ID      CHA*10 NO_PROC I       unique process identification
!     PROC_NAME    CHA*50 NO_PROC I       process name
!     PROC_FORT    CHA*10 NO_PROC I       reference to fortran routine
!     PROC_TRCO    INT    NO_PROC I       transport code (123)
!     LUNREP       INT    1       I       Unit number report file
!     IERROR       INT    1       O       Error
!
!     IMPLICIT NONE for extra compiler checks
!     SAVE to keep the group definition intact
!
      IMPLICIT NONE
      SAVE
!
!     declaration of arguments
!
      INTEGER(kind=int_wp) ::NO_PROC_MAX , NO_PROC     , & 
                   LUNREP      , IERROR
      INTEGER(kind=int_wp) ::DEFFDS
      character(len=10)  PROC_ID     (NO_PROC_MAX)
      character(len=50)  PROC_NAME   (NO_PROC_MAX)
      character(len=10)  PROC_FORT   (NO_PROC_MAX)
      INTEGER(kind=int_wp) ::PROC_TRCO   (NO_PROC_MAX)
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
      PARAMETER   ( NELEMS = 5 )
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
      DATA  GRPNAM  /'TABLE_P4'/
      DATA & 
      (ELMNMS(I),ELMTPS(I),NBYTSG(I),ELMDMS(1,I),ELMDMS(2,I),ELMDES(I), & 
       I = 1 , NELEMS) & 
     /'NO_PROC'  ,'INTEGER'  , 4,1,1,'number of processes'            , & 
      'PROC_ID'  ,'CHARACTER',10,1,0,'unique process identification'  , & 
      'PROC_NAME','CHARACTER',50,1,0,'process name'                   , & 
      'PROC_FORT','CHARACTER',10,1,0,'fortran subroutine'             , & 
      'PROC_TRCO','INTEGER'  , 4,1,0,'transport code (123)'           /
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
                      BUFLEN , NO_PROC  )
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(1)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      IF ( NO_PROC > NO_PROC_MAX ) THEN
         WRITE(LUNREP,*) 'ERROR reading group',GRPNAM
         WRITE(LUNREP,*) 'Actual number of processes:',NO_PROC
         WRITE(LUNREP,*) 'greater than maximum:',NO_PROC_MAX
         IERROR = 1
         GOTO 900
      ENDIF
!
!     Set dimension of table
!
      DO IELM = 2 , NELEMS
         ELMDMS(2,IELM) = NO_PROC
      ENDDO

      BUFLEN = NBYTSG(2)*ELMDMS(2,2)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(2), & 
                      UINDEX , 1        , & 
                      BUFLEN , PROC_ID  )
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(2)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF

      BUFLEN = NBYTSG(3)*ELMDMS(2,3)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(3), & 
                      UINDEX , 1        , & 
                      BUFLEN , PROC_NAME)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(3)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF

      BUFLEN = NBYTSG(4)*ELMDMS(2,4)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(4), & 
                      UINDEX , 1        , & 
                      BUFLEN , PROC_FORT)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(4)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF

      BUFLEN = NBYTSG(5)*ELMDMS(2,5)
      IERROR = GETELT (DEFFDS , & 
                      GRPNAM , ELMNMS(5), & 
                      UINDEX , 1        , & 
                      BUFLEN , PROC_TRCO)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(5)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!
  900 CONTINUE
      RETURN
!
      END

      end module m_rd_tabp4
