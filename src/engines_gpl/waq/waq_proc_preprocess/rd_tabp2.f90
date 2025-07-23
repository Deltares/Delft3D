!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
      module m_rd_tabp2
      use m_waq_precision


      implicit none

      contains


      SUBROUTINE RD_TABP2 ( DEFFDS      , & 
                           NO_ITEM_MAX , NO_ITEM     , & 
                           ITEM_ID     , ITEM_NAME   , & 
                           ITEM_UNIT   , ITEM_DEFAULT, & 
                           ITEM_AGGREGA, ITEM_DISAGGR, & 
                           ITEM_GROUPID, ITEM_SEGX   , & 
                           ITEM_WK     , ITEM_SN     , & 
                           ITEM_SU     , LUNREP      , & 
                           IERROR      )
!
!     Deltares
!
!     CREATED            :  june 1999 by Jan van Beek
!
!     FUNCTION           :  Read TABLE_P2 group from NEFIS file
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
!     NO_ITEM_MAX  INT            I       maximum number of items
!     NO_ITEM      INT            0       number of items
!     ITEM_ID      CHA*10 NO_ITEM 0       unique item identification
!     ITEM_NAME    CHA*50 NO_ITEM 0       item name
!     ITEM_UNIT    CHA*20 NO_ITEM 0       unit
!     ITEM_DEFAULT REA    NO_ITEM 0       default value
!     ITEM_AGGREGA CHA*10 NO_ITEM 0       variable used for aggregation
!     ITEM_DISAGGR CHA*10 NO_ITEM 0       variable used for dis-aggregation
!     ITEM_GROUPID CHA*30 NO_ITEM 0       subtance group ID
!     ITEM_SEGX    CHA*1  NO_ITEM 0       segment / exchange indication
!     ITEM_WK      CHA*1  NO_ITEM 0       active / inactive indication
!     LUNREP       INT    1       I       Unit number report file
!     IERROR       INT    1       0       Error
!
!     IMPLICIT NONE for extra compiler checks
!     SAVE to keep the group definition intact
!
      IMPLICIT NONE
      SAVE
!
!     declaration of arguments
!
      INTEGER(kind=int_wp) ::NO_ITEM_MAX , NO_ITEM     , & 
                   LUNREP      , IERROR
      INTEGER(kind=int_wp) ::DEFFDS
      character(len=10)  ITEM_ID     (NO_ITEM_MAX)
      character(len=50)  ITEM_NAME   (NO_ITEM_MAX)
      character(len=20)  ITEM_UNIT   (NO_ITEM_MAX)
      REAL(kind=real_wp) ::ITEM_DEFAULT(NO_ITEM_MAX)
      character(len=10)  ITEM_AGGREGA(NO_ITEM_MAX)
      character(len=10)  ITEM_DISAGGR(NO_ITEM_MAX)
      character(len=30)  ITEM_GROUPID(NO_ITEM_MAX)
      character(len=1)   ITEM_SEGX   (NO_ITEM_MAX)
      character(len=1)   ITEM_WK     (NO_ITEM_MAX)
      character(len=100) ITEM_SN     (NO_ITEM_MAX)
      character(len=40)  ITEM_SU     (NO_ITEM_MAX)
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
      INTEGER(kind=int_wp) ::NELEMS, INQNELEMS
      PARAMETER   ( NELEMS = 12 )
!
      INTEGER(kind=int_wp) ::I               , IELM          , & 
                   BUFLEN
      INTEGER(kind=int_wp) ::ELMDMS(2,NELEMS), NBYTSG(NELEMS), & 
                   UINDEX(3)
      character(len=16)  GRPNAM
      character(len=16)  ELMNMS(NELEMS)  , INQELMNMS(NELEMS),  ELMTPS(NELEMS)
      character(len=64)  ELMDES(NELEMS)
      LOGICAL       NETCDFSTD
!
!     External NEFIS Functions
!
      INTEGER(kind=int_wp) ::INQCEL & 
              ,GETELS & 
              ,GETELT
      EXTERNAL  GETELS & 
              ,GETELT
!
!     element names
!
      DATA  GRPNAM  /'TABLE_P2'/
      DATA & 
      (ELMNMS(I),ELMTPS(I),NBYTSG(I),ELMDMS(1,I),ELMDMS(2,I),ELMDES(I), & 
       I = 1 , NELEMS) & 
     /'NO_ITEM','INTEGER'  ,  4,1,1,'number of items'                  , & 
      'ITEM_ID','CHARACTER', 10,1,0,'unique item identification'       , & 
      'ITEM_NM','CHARACTER', 50,1,0,'item name'                        , & 
      'UNIT'   ,'CHARACTER', 20,1,0,'unit'                             , & 
      'DEFAULT','REAL'     ,  4,1,0,'default value'                    , & 
      'AGGREGA','CHARACTER', 10,1,0,'variable used for aggregation    ', & 
      'DISAGGR','CHARACTER', 10,1,0,'variable used for dis-aggregation', & 
      'GROUPID','CHARACTER', 30,1,0,'subtance group ID                ', & 
      'SEG_EXC','CHARACTER',  1,1,0,'segment / exchange indication    ', & 
      'WK'     ,'CHARACTER',  1,1,0,'active / inactive indication     ', & 
      'ITEM_SN','CHARACTER',100,1,0,'netcdf standard name             ', & 
      'ITEM_SU','CHARACTER', 40,1,0,'netcdf standard unit             '/
!
!     Check if netcdf standard names and units are present on the proc_def file
!
      INQNELEMS = 12
      IERROR = INQCEL (DEFFDS , GRPNAM, INQNELEMS, INQELMNMS)
      IF (INQNELEMS==NELEMS) THEN
          NETCDFSTD = .TRUE.
      ELSE
          NETCDFSTD = .FALSE.
      ENDIF
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
                      buflen , NO_ITEM  )
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element ',ELMNMS(1)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      IF ( NO_ITEM > NO_ITEM_MAX ) THEN
         WRITE(LUNREP,*) 'ERROR reading group ',GRPNAM
         WRITE(LUNREP,*) 'Actual number of items: ',NO_ITEM
         WRITE(LUNREP,*) 'greater than maximum: ',NO_ITEM_MAX
         IERROR = 1
         GOTO 900
      ENDIF
!
!     Set dimension of table
!
      DO IELM = 2 , NELEMS
         ELMDMS(2,IELM) = NO_ITEM
      ENDDO
      BUFLEN = NBYTSG(2)*ELMDMS(2,2)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(2), & 
                      UINDEX , 1        , & 
                      BUFLEN , ITEM_ID  )
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element ',ELMNMS(2)
         WRITE(LUNREP,*) 'ERROR number: ',IERROR
         GOTO 900
      ENDIF
      BUFLEN = NBYTSG(3)*ELMDMS(2,3)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(3), & 
                      UINDEX , 1        , & 
                      BUFLEN , ITEM_NAME)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element ',ELMNMS(3)
         WRITE(LUNREP,*) 'ERROR number: ',IERROR
         GOTO 900
      ENDIF
      BUFLEN = NBYTSG(4)*ELMDMS(2,4)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(4), & 
                      UINDEX , 1        , & 
                      BUFLEN , ITEM_UNIT)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element ',ELMNMS(4)
         WRITE(LUNREP,*) 'ERROR number: ',IERROR
         GOTO 900
      ENDIF
      BUFLEN = NBYTSG(5)*ELMDMS(2,5)
      IERROR = GETELT (DEFFDS , & 
                      GRPNAM , ELMNMS(5), & 
                      UINDEX , 1        , & 
                      BUFLEN , ITEM_DEFAULT)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element ',ELMNMS(5)
         WRITE(LUNREP,*) 'ERROR number: ',IERROR
         GOTO 900
      ENDIF
      BUFLEN = NBYTSG(6)*ELMDMS(2,6)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(6), & 
                      UINDEX , 1        , & 
                      BUFLEN , ITEM_DISAGGR)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element ',ELMNMS(6)
         WRITE(LUNREP,*) 'ERROR number: ',IERROR
         GOTO 900
      ENDIF
      BUFLEN = NBYTSG(7)*ELMDMS(2,7)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(7), & 
                      UINDEX , 1        , & 
                      BUFLEN , ITEM_AGGREGA)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element ',ELMNMS(7)
         WRITE(LUNREP,*) 'ERROR number: ',IERROR
         GOTO 900
      ENDIF
      BUFLEN = NBYTSG(8)*ELMDMS(2,8)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(8), & 
                      UINDEX , 1        , & 
                      BUFLEN , ITEM_GROUPID)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element ',ELMNMS(8)
         WRITE(LUNREP,*) 'ERROR number: ',IERROR
         GOTO 900
      ENDIF
      BUFLEN = NBYTSG(9)*ELMDMS(2,9)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(9), & 
                      UINDEX , 1        , & 
                      BUFLEN , ITEM_SEGX)
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element ',ELMNMS(9)
         WRITE(LUNREP,*) 'ERROR number: ',IERROR
         GOTO 900
      ENDIF
      BUFLEN = NBYTSG(10)*ELMDMS(2,10)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(10), & 
                      UINDEX , 1         , & 
                      BUFLEN , ITEM_WK  )
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element ',ELMNMS(10)
         WRITE(LUNREP,*) 'ERROR number: ',IERROR
         GOTO 900
      ENDIF
      IF (.NOT. NETCDFSTD) THEN
         WRITE(LUNREP,'(/A/)') ' Info: This processes definition file does not contain standard names and units for NetCDF files.'
         ITEM_SN = ' '
         ITEM_SU = ' '
         GOTO 900
      ENDIF
      BUFLEN = NBYTSG(11)*ELMDMS(2,11)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(11), & 
                      UINDEX , 1         , & 
                      BUFLEN , ITEM_SN  )
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element ',ELMNMS(11)
         WRITE(LUNREP,*) 'ERROR number: ',IERROR
         GOTO 900
      ENDIF
      BUFLEN = NBYTSG(12)*ELMDMS(2,12)
      IERROR = GETELS (DEFFDS , & 
                      GRPNAM , ELMNMS(12), & 
                      UINDEX , 1         , & 
                      BUFLEN , ITEM_SU  )
      IF ( IERROR /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element ',ELMNMS(12)
         WRITE(LUNREP,*) 'ERROR number: ',IERROR
         GOTO 900
      ENDIF
!
  900 CONTINUE
      RETURN
!
      END

      end module m_rd_tabp2
