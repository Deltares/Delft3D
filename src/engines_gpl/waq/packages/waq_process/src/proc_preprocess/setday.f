!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

      SUBROUTINE SETDAY ( LUNREP     , NOKEY      ,
     +                    KEYNAM     , KEYVAL     ,
     +                    DTFLG1     , DTFLG3     ,
     +                    IPROC      , aProcesProp,
     +                    AllItems   , IERR       ,
     +                    NOWARN     )
!
!     Deltares
!
!     CREATED:            : februari 2002 by Jan van Beek
!
!     FUNCTION            : Sets io list for statistical routine STADAY
!
!     SUBROUTINES CALLED  : SRSTOP, stops execution
!                           ZOEK  , finds string in character array
!                           DLWQ0T, converts absolute time to system time (seconds)
!
!
!     PARAMETERS          :
!
!     NAME    KIND      LENGTH  FUNCT.  DESCRIPTION
!     ----    -----     ------  ------- -----------
!     LUNREP  INTEGER        1  INPUT   unit number report file
!     NOKEY   INTEGER        1  INPUT   number of keywords for this process
!     KEYNAM  CHAR*20    NOKEY  INPUT   keyword name
!     KEYVAL  CHAR*20    NOKEY  INPUT   keyword value
!     aProcesProp               OUTPUT  properties for this proces
!     AllItems                  INPUT   all items known to the proces system
!     IERR    INTEGER        1  IN/OUT  cummulative error count
!     NOWARN  INTEGER        1  IN/OUT  cummulative warning count
!
      use m_zoek
      use m_srstop
      use m_dhslen
      USE ProcesSet
      use timers       !   performance timers
      use m_cnvper
      use m_cnvtim
!
      IMPLICIT NONE
!
!     Declaration of arguments
!
      INTEGER       LUNREP, NOKEY , IPROC , IERR  , NOWARN
      LOGICAL       DTFLG1 , DTFLG3
      CHARACTER*20  KEYNAM(NOKEY), KEYVAL(NOKEY)
      type(ProcesProp)      :: aProcesProp         ! output statistical proces definition
      type(ItemPropColl)    :: AllItems            ! all items of the proces system
!
!     Local declarations
!
      INTEGER       IERR_ALLOC, IKEY  , ISLEN     , IERR2 , IRET
      integer       istart , iperiod
      INTEGER,      ALLOCATABLE :: ISUSED(:)
      CHARACTER*20  KEY       , SUFFIX  , NAME
      REAL          PERIOD
      type(ItemProp)        :: aItemProp            ! one item
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "setday", ithndl )
!
!     init
!
      ALLOCATE(ISUSED(NOKEY),STAT=IERR_ALLOC)
      IF ( IERR_ALLOC .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR allocating buffer array:',IERR_ALLOC
         WRITE(LUNREP,*) 'in routine SETDAY_3, buffer length:',NOKEY
         WRITE(*,*) 'ERROR allocating buffer array:',IERR_ALLOC
         CALL SRSTOP(1)
      ENDIF
      ISUSED = 0
      KEY='OUTPUT-OPERATION'
      CALL ZOEK(KEY,NOKEY,KEYNAM,20,IKEY)
      IF ( IKEY .GT. 0 ) THEN
         ISUSED(IKEY) = 1
      ENDIF
!
!     Fill the Propces Properties
!
      aProcesProp%name       = 'STADAY'
      WRITE(aProcesProp%name(7:10),'(I4.4)') IPROC
      aProcesProp%routine    = 'STADAY'
      aProcesProp%text       = 'periodic average'
      aProcesProp%swtransp   = 123
      aProcesProp%type       = PROCESTYPE_OUTPUT
      aProcesProp%no_input      = 7
      aProcesProp%no_output     = 3
      aProcesProp%no_FluxOutput = 0
      aProcesProp%no_FluxStochi = 0
      aProcesProp%no_DispStochi = 0
      aProcesProp%no_VeloStochi = 0
      ALLOCATE(aProcesProp%input_item(aProcesProp%no_input),
     +         aProcesProp%output_item(aProcesProp%no_output),
     +         STAT=IERR_ALLOC)
      IF ( IERR_ALLOC .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR allocating IOitem array:',IERR_ALLOC
         WRITE(LUNREP,*) 'in routine SETDAY_1, array length:',aProcesProp%no_input,aProcesProp%no_output
         WRITE(*,*) 'ERROR allocating array:',IERR_ALLOC
         CALL SRSTOP(1)
      ENDIF
!
      KEY='SUBSTANCE'
      CALL ZOEK(KEY,NOKEY,KEYNAM,20,IKEY)
      IF ( IKEY .LE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR no parameter specified for statistics'
         IERR = IERR + 1
      ELSE
         ISUSED(IKEY) = 1
         aProcesProp%input_item(1)%name=KEYVAL(IKEY)
         aProcesProp%input_item(1)%type=IOTYPE_SEGMENT_INPUT
         aProcesProp%input_item(1)%actdef=-999.
         aProcesProp%input_item(1)%indx  = 1
         aProcesProp%input_item(1)%ip_val= 0
         aItemProp%name = KEYVAL(IKEY)
         iret = ItemPropCollFind( AllItems, aItemProp )
         if ( iret .le. 0 ) then
            aItemProp%text    = 'input parameter for statistics'
            aItemProp%default = -999.
            aItemProp%waqtype = WAQTYPE_NONE
            iret = ItemPropCollAdd( AllItems, aItemProp )
         endif
         aProcesProp%input_item(1)%item=>AllItems%ItemPropPnts(iret)%pnt
      ENDIF
!
      KEY = 'TINIT'
      CALL ZOEK(KEY,NOKEY,KEYNAM,20,IKEY)
      IF ( IKEY .LE. 0 ) THEN
         istart = 0
      ELSE
         ISUSED(IKEY) = 1
         READ(KEYVAL(IKEY),'(I20.0)',IOSTAT=IERR2) istart
         IF ( IERR2 .NE. 0 ) THEN
            CALL DLWQ0T( KEYVAL(IKEY), istart, .FALSE., .FALSE., IERR2)
            IF ( IERR2 .NE. 0 ) THEN
               WRITE(LUNREP,*)'ERROR interpreting start time:',
     +                         KEYVAL(IKEY)
               IERR = IERR + 1
            ENDIF
         ELSE
            CALL CNVTIM ( istart, 1     , DTFLG1 , DTFLG3 )
         ENDIF
      ENDIF
      aItemProp%name    = KEY(1:10)//aProcesProp%name(1:10)
      aItemProp%default = istart
      aItemProp%text    = 'start time for statistics'
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%input_item(2)%name=aItemProp%name
      aProcesProp%input_item(2)%type=IOTYPE_SEGMENT_INPUT
      aProcesProp%input_item(2)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(2)%actdef=istart
      aProcesProp%input_item(2)%indx  = 2
      aProcesProp%input_item(2)%ip_val  = 0
!
      KEY = 'PERIOD'
      CALL ZOEK(KEY,NOKEY,KEYNAM,20,IKEY)
      IF ( IKEY .LE. 0 ) THEN
         iperiod = 86400.
      ELSE
         ISUSED(IKEY) = 1
         READ(KEYVAL(IKEY),'(I20.0)',IOSTAT=IERR2) iperiod
         IF ( IERR2 .NE. 0 ) THEN
            CALL CNVPER( KEYVAL(IKEY), iperiod, .FALSE., .FALSE., IERR2)
            IF ( IERR2 .NE. 0 ) THEN
               WRITE(LUNREP,*)'ERROR interpreting period:',KEYVAL(IKEY)
               IERR = IERR + 1
            ENDIF
         ELSE
            CALL CNVTIM ( iperiod, 1     , DTFLG1 , DTFLG3 )
         ENDIF
      ENDIF
      aItemProp%name    = KEY(1:10)//aProcesProp%name(1:10)
      aItemProp%default = iperiod
      aItemProp%text    = 'period of time averaged output'
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%input_item(3)%name=aItemProp%name
      aProcesProp%input_item(3)%type=IOTYPE_SEGMENT_INPUT
      aProcesProp%input_item(3)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(3)%actdef=iperiod
      aProcesProp%input_item(3)%indx  = 3
      aProcesProp%input_item(3)%ip_val  = 0
!
      aItemProp%name    = 'ITIME'
      iret = ItemPropCollFind( AllItems, aItemProp )
      if ( iret .le. 0 ) then
         aItemProp%default = -999.
         aItemProp%text    = 'time in calculation'
         aItemProp%waqtype = WAQTYPE_DEFAULT
         iret = ItemPropCollAdd( AllItems, aItemProp )
      endif
      aProcesProp%input_item(4)%name=aItemProp%name
      aProcesProp%input_item(4)%type=IOTYPE_SEGMENT_INPUT
      aProcesProp%input_item(4)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(4)%actdef=-999.
      aProcesProp%input_item(4)%indx  = 4
      aProcesProp%input_item(4)%ip_val  = 0
!
      aItemProp%name    = 'IDT'
      iret = ItemPropCollFind( AllItems, aItemProp )
      if ( iret .le. 0 ) then
         aItemProp%default = -999.
         aItemProp%text    = 'time step'
         aItemProp%waqtype = WAQTYPE_DEFAULT
         iret = ItemPropCollAdd( AllItems, aItemProp )
      endif
      aProcesProp%input_item(5)%name=aItemProp%name
      aProcesProp%input_item(5)%type=IOTYPE_SEGMENT_INPUT
      aProcesProp%input_item(5)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(5)%actdef=-999.
      aProcesProp%input_item(5)%indx  = 5
      aProcesProp%input_item(5)%ip_val  = 0
!
      aItemProp%name    = 'TCOUNT    '//aProcesProp%name(1:10)
      aItemProp%default = 0.0
      aItemProp%text    = 'time step counter'
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%input_item(6)%name=aItemProp%name
      aProcesProp%input_item(6)%type=IOTYPE_SEGMENT_WORK
      aProcesProp%input_item(6)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(6)%actdef=0.0
      aProcesProp%input_item(6)%indx  = 6
      aProcesProp%input_item(6)%ip_val  = 0
!
      KEY = 'SUFFIX'
      CALL ZOEK(KEY,NOKEY,KEYNAM,20,IKEY)
      IF ( IKEY .LE. 0 ) THEN
         SUFFIX = ' '
      ELSE
         SUFFIX = KEYVAL(IKEY)
         ISUSED(IKEY) = 1
      ENDIF
      CALL DHSLEN(SUFFIX,ISLEN)
!
      IF (SUFFIX(1:ISLEN) .NE. ' ' ) THEN
         aItemProp%name    = SUFFIX(1:ISLEN)//'_'//aProcesProp%input_item(1)%name
      ELSE
         aItemProp%name    = 'TAVG_'//aProcesProp%input_item(1)%name
      ENDIF
      aItemProp%default = -999.
      aItemProp%text    = 'periodic average '//aProcesProp%input_item(1)%name
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%output_item(1)%name=aItemProp%name
      aProcesProp%output_item(1)%type=IOTYPE_SEGMENT_OUTPUT
      aProcesProp%output_item(1)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(1)%indx= 1
      aProcesProp%output_item(1)%ip_val= 0
      WRITE(LUNREP,2000) 'Statistical output named [',aItemProp%name,
     +                   '] created with periodic average from [',aProcesProp%input_item(1)%name,']'
!
!     work array in input and in output
!
      IF (SUFFIX(1:ISLEN) .NE. ' ' ) THEN
         aItemProp%name    = 'T_'//SUFFIX(1:ISLEN)//'_'//aProcesProp%input_item(1)%name
      ELSE
         aItemProp%name    = 'T_'//aProcesProp%input_item(1)%name
      ENDIF
      aItemProp%default = -999.
      aItemProp%text    = 'work array '//aProcesProp%input_item(1)%name
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%output_item(2)%name=aItemProp%name
      aProcesProp%output_item(2)%type=IOTYPE_SEGMENT_WORK
      aProcesProp%output_item(2)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(2)%indx= 2
      aProcesProp%output_item(2)%ip_val= 0
      aProcesProp%input_item(7)%name=aItemProp%name
      aProcesProp%input_item(7)%type=IOTYPE_SEGMENT_WORK
      aProcesProp%input_item(7)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(7)%actdef=-999.
      aProcesProp%input_item(7)%indx  = 7
      aProcesProp%input_item(7)%ip_val  = 0

      ! Add the companion for the TCOUNT input item
      aItemProp%name    = 'TCOUNT    '//aProcesProp%name(1:10)
      aItemProp%default = -999.
      aItemProp%text    = 'time step counter (work array)'
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%output_item(3)%name=aItemProp%name
      aProcesProp%output_item(3)%type=IOTYPE_SEGMENT_OUTPUT
      aProcesProp%output_item(3)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(3)%indx= 3
      aProcesProp%output_item(3)%ip_val= 0

!
!     check the use of the key words
!
      DO IKEY = 1 , NOKEY
         IF ( ISUSED(IKEY) .EQ. 0 ) THEN
            NOWARN = NOWARN + 1
            WRITE(LUNREP,*) 'WARNING: keyword not used'
            WRITE(LUNREP,*) 'key   :',KEYNAM(IKEY)
            WRITE(LUNREP,*) 'value :',KEYVAL(IKEY)
         ENDIF
      ENDDO
!
      DEALLOCATE (ISUSED)
!
      if (timon) call timstop( ithndl )
      RETURN
 2000 FORMAT(5A)
      END
