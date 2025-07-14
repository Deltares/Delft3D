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
      module m_setday
      use m_waq_precision
      use m_string_utils
      USE ProcesSet
      use m_error_status

      implicit none

      contains


      SUBROUTINE SETDAY ( LUNREP     , NOKEY      , &
                         KEYNAM     , KEYVAL     , &
                         is_date_format     , is_yyddhh_format     , &
                         IPROC      , aProcesProp, &
                         AllItems   , status )
!
!     Deltares
!
!     CREATED:            : februari 2002 by Jan van Beek
!
!     FUNCTION            : Sets io list for statistical routine STADAY
!
!     SUBROUTINES CALLED  : stop_with_error, stops execution
!                           ZOEK  , finds string in character array
!                           convert_string_to_time_offset, converts absolute time to system time (seconds)
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
!
      use m_logger_helper, only : stop_with_error
      use m_string_manipulation, only : get_trimmed_length

      use timers       !   performance timers
      use date_time_utils, only : convert_string_to_time_offset, convert_period_to_timer, convert_relative_time
!
      IMPLICIT NONE
!
!     Declaration of arguments
!
      INTEGER(kind=int_wp) ::LUNREP, NOKEY , IPROC , item_ind
      LOGICAL       is_date_format , is_yyddhh_format
      character(len=20)  KEYNAM(NOKEY), KEYVAL(NOKEY)
      type(ProcesProp)      :: aProcesProp         ! output statistical proces definition
      type(ItemPropColl)    :: AllItems            ! all items of the proces system

      type(error_status), intent(inout) :: status !< current error status
!
!     Local declarations
!
      INTEGER(kind=int_wp) ::IERR_ALLOC, IKEY  , ISLEN     , IERR2 , IRET
      integer(kind=int_wp) ::istart , iperiod
      INTEGER(kind=int_wp),      ALLOCATABLE  ::ISUSED(:)
      character(len=20)  SUFFIX  , NAME, item_name
      character(len=50)  item_desc
      REAL(kind=real_wp) ::PERIOD, default_value
      type(ItemProp)        :: aItemProp            ! one item
      integer(kind=int_wp) ::ithndl = 0
      if (timon) call timstrt( "setday", ithndl )
!
!     init
!
      ALLOCATE(ISUSED(NOKEY),STAT=IERR_ALLOC)
      IF ( IERR_ALLOC /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR allocating buffer array:',IERR_ALLOC
         WRITE(LUNREP,*) 'in routine SETDAY_3, buffer length:',NOKEY
         WRITE(*,*) 'ERROR allocating buffer array:',IERR_ALLOC
         CALL stop_with_error()
      ENDIF
      ISUSED = 0

      IKEY = index_in_array('OUTPUT-OPERATION',KEYNAM)
      IF ( IKEY > 0 ) THEN
         ISUSED(IKEY) = 1
      ENDIF
!
!     Fill the Proces Properties
!
      aProcesProp%name       = 'STADAY'
      WRITE(aProcesProp%name(7:10),'(I4.4)') IPROC
      aProcesProp%routine    = 'STADAY'
      aProcesProp%text       = 'periodic average, periodic minimum, periodic maximum'
      aProcesProp%swtransp   = 123
      aProcesProp%type       = PROCESTYPE_OUTPUT
      aProcesProp%no_input      = 9
      aProcesProp%no_output     = 7
      aProcesProp%no_FluxOutput = 0
      aProcesProp%no_FluxStochi = 0
      aProcesProp%no_DispStochi = 0
      aProcesProp%no_VeloStochi = 0
      ALLOCATE(aProcesProp%input_item(aProcesProp%no_input), &
              aProcesProp%output_item(aProcesProp%no_output), &
              STAT=IERR_ALLOC)
      IF ( IERR_ALLOC /= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR allocating IOitem array:',IERR_ALLOC
         WRITE(LUNREP,*) 'in routine SETDAY_1, array length:',aProcesProp%no_input,aProcesProp%no_output
         WRITE(*,*) 'ERROR allocating array:',IERR_ALLOC
         CALL stop_with_error()
      ENDIF
!
      IKEY = index_in_array('SUBSTANCE',KEYNAM)
      IF ( IKEY <= 0 ) THEN
         WRITE(LUNREP,*) 'ERROR no parameter specified for statistics'
         call status%increase_error_count()
      ELSE
         ISUSED(IKEY) = 1
         aProcesProp%input_item(1)%name=KEYVAL(IKEY)
         aProcesProp%input_item(1)%type=IOTYPE_SEGMENT_INPUT
         aProcesProp%input_item(1)%actdef=-999.
         aProcesProp%input_item(1)%indx  = 1
         aProcesProp%input_item(1)%ip_val= 0
         aItemProp%name = KEYVAL(IKEY)
         iret = ItemPropCollFind( AllItems, aItemProp )
         if ( iret <= 0 ) then
            aItemProp%text    = 'input parameter for statistics'
            aItemProp%default = -999.
            aItemProp%waqtype = WAQTYPE_NONE
            iret = ItemPropCollAdd( AllItems, aItemProp )
         endif
         aProcesProp%input_item(1)%item=>AllItems%ItemPropPnts(iret)%pnt
      ENDIF

      IKEY = index_in_array('TINIT',KEYNAM)
      IF ( IKEY <= 0 ) THEN
         istart = 0
      ELSE
         ISUSED(IKEY) = 1
         READ(KEYVAL(IKEY),'(I20.0)',IOSTAT=IERR2) istart
         IF ( IERR2 /= 0 ) THEN
            CALL convert_string_to_time_offset( KEYVAL(IKEY), istart, .FALSE., .FALSE., IERR2)
            IF ( IERR2 /= 0 ) THEN
               WRITE(LUNREP,*)'ERROR interpreting start time:', &
                              KEYVAL(IKEY)
               call status%increase_error_count()
            ENDIF
         ELSE
            call convert_relative_time(istart, 1     , is_date_format , is_yyddhh_format )
         ENDIF
      ENDIF

      item_desc = 'start time for statistics'
      item_ind = 2
      item_name = 'TINIT'//aProcesProp%name(1:10)
      call update_process_properties(AllItems, aProcesProp, aItemProp, real(istart), item_desc, item_ind, item_name, &
      IOTYPE_SEGMENT_INPUT)
!
      IKEY = index_in_array('PERIOD',KEYNAM)
      IF ( IKEY <= 0 ) THEN
         iperiod = 86400.
      ELSE
         ISUSED(IKEY) = 1
         READ(KEYVAL(IKEY),'(I20.0)',IOSTAT=IERR2) iperiod
         IF ( IERR2 /= 0 ) THEN
            CALL convert_period_to_timer(KEYVAL(IKEY), iperiod, .FALSE., .FALSE., IERR2)
            IF ( IERR2 /= 0 ) THEN
               WRITE(LUNREP,*)'ERROR interpreting period:',KEYVAL(IKEY)
               call status%increase_error_count()
            ENDIF
         ELSE
            call convert_relative_time ( iperiod, 1     , is_date_format , is_yyddhh_format )
         ENDIF
      ENDIF

      item_desc = 'period of time averaged output'
      item_ind = 3
      item_name = 'PERIOD'//aProcesProp%name(1:10)
      call update_process_properties(AllItems, aProcesProp, aItemProp, real(iperiod), item_desc, item_ind,item_name, &
      IOTYPE_SEGMENT_INPUT)
!
      aItemProp%name    = 'ITIME'
      iret = ItemPropCollFind( AllItems, aItemProp )
      if ( iret <= 0 ) then
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
      item_desc = 'time step'
      item_ind = 5
      item_name =  'IDT'

      aItemProp%name = item_name
      iret = ItemPropCollFind( AllItems, aItemProp )
      if ( iret <= 0 ) then
         aItemProp%default = -999.
         aItemProp%text    = 'time step'
         aItemProp%waqtype = WAQTYPE_DEFAULT
         iret = ItemPropCollAdd( AllItems, aItemProp )
      endif

      aProcesProp%input_item(item_ind)%name=item_name
      aProcesProp%input_item(item_ind)%type=IOTYPE_SEGMENT_INPUT
      aProcesProp%input_item(item_ind)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(item_ind)%actdef=-999.
      aProcesProp%input_item(item_ind)%indx  = item_ind
      aProcesProp%input_item(item_ind)%ip_val  = 0
!
      item_desc = 'time step counter'
      item_ind = 6
      item_name =  'TCOUNT    '//aProcesProp%name(1:10)
      call update_process_properties(AllItems, aProcesProp, aItemProp, 0.0, item_desc, item_ind, item_name, IOTYPE_SEGMENT_WORK)
!
      IKEY = index_in_array('SUFFIX',KEYNAM)
      IF ( IKEY <= 0 ) THEN
         SUFFIX = ' '
      ELSE
         SUFFIX = KEYVAL(IKEY)
         ISUSED(IKEY) = 1
      ENDIF
      CALL get_trimmed_length(SUFFIX,ISLEN)
!
      IF (SUFFIX(1:ISLEN) /= ' ' ) THEN
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
      WRITE(LUNREP,2000) 'Statistical output named [',aItemProp%name, &
                        '] created with periodic average from [',aProcesProp%input_item(1)%name,']'
!
      !     work array in input and in output
      IF (SUFFIX(1:ISLEN) /= ' ' ) THEN
         aItemProp%name    = SUFFIX(1:ISLEN)//'_'//aProcesProp%input_item(1)%name
      ELSE
         aItemProp%name    = 'TMIN_'//aProcesProp%input_item(1)%name
      ENDIF
      aItemProp%default = -999.
      aItemProp%text    = 'periodic minimum '//aProcesProp%input_item(1)%name
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%output_item(2)%name=aItemProp%name
      aProcesProp%output_item(2)%type=IOTYPE_SEGMENT_OUTPUT
      aProcesProp%output_item(2)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(2)%indx= 2
      aProcesProp%output_item(2)%ip_val= 0
      WRITE(LUNREP,2000) 'Statistical output named [',aItemProp%name, &
                       '] created with periodic minimum from [',aProcesProp%input_item(1)%name,']'
!
      IF (SUFFIX(1:ISLEN) /= ' ' ) THEN
         aItemProp%name    = SUFFIX(1:ISLEN)//'_'//aProcesProp%input_item(1)%name
      ELSE
         aItemProp%name    = 'TMAX_'//aProcesProp%input_item(1)%name
      ENDIF
      aItemProp%default = -999.
      aItemProp%text    = 'periodic maximum '//aProcesProp%input_item(1)%name
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%output_item(3)%name=aItemProp%name
      aProcesProp%output_item(3)%type=IOTYPE_SEGMENT_OUTPUT
      aProcesProp%output_item(3)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(3)%indx= 3
      aProcesProp%output_item(3)%ip_val= 0
      WRITE(LUNREP,2000) 'Statistical output named [',aItemProp%name, &
                       '] created with periodic maximum from [',aProcesProp%input_item(1)%name,']'
!
!     work array in input and in output
!
      IF (SUFFIX(1:ISLEN) /= ' ' ) THEN
         aItemProp%name    = 'T_'//SUFFIX(1:ISLEN)//'_'//aProcesProp%input_item(1)%name
      ELSE
         aItemProp%name    = 'T_'//aProcesProp%input_item(1)%name
      ENDIF
      aItemProp%default = -999.
      aItemProp%text    = 'work array '//aProcesProp%input_item(1)%name
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%output_item(4)%name=aItemProp%name
      aProcesProp%output_item(4)%type=IOTYPE_SEGMENT_WORK
      aProcesProp%output_item(4)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(4)%indx= 4
      aProcesProp%output_item(4)%ip_val= 0
      aProcesProp%input_item(7)%name=aItemProp%name
      aProcesProp%input_item(7)%type=IOTYPE_SEGMENT_WORK
      aProcesProp%input_item(7)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(7)%actdef=-999.
      aProcesProp%input_item(7)%indx  = 7
      aProcesProp%input_item(7)%ip_val  = 0
!
!     work array in input and in output
!
      IF (SUFFIX(1:ISLEN) /= ' ' ) THEN
         aItemProp%name    = 'MINDYN_'//SUFFIX(1:ISLEN)//'_'//aProcesProp%input_item(1)%name
      ELSE
         aItemProp%name    = 'MINDYN_'//aProcesProp%input_item(1)%name
      ENDIF
      aItemProp%default = -999.
      aItemProp%text    = 'work array '//aProcesProp%input_item(1)%name
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%output_item(5)%name=aItemProp%name
      aProcesProp%output_item(5)%type=IOTYPE_SEGMENT_WORK
      aProcesProp%output_item(5)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(5)%indx= 5
      aProcesProp%output_item(5)%ip_val= 0
      aProcesProp%input_item(8)%name=aItemProp%name
      aProcesProp%input_item(8)%type=IOTYPE_SEGMENT_WORK
      aProcesProp%input_item(8)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(8)%actdef=-999.
      aProcesProp%input_item(8)%indx  = 8
      aProcesProp%input_item(8)%ip_val  = 0

!
!     work array in input and in output
!
      IF (SUFFIX(1:ISLEN) /= ' ' ) THEN
         aItemProp%name    = 'MAXDYN_'//SUFFIX(1:ISLEN)//'_'//aProcesProp%input_item(1)%name
      ELSE
         aItemProp%name    = 'MAXDYN_'//aProcesProp%input_item(1)%name
      ENDIF
      aItemProp%default = -999.
      aItemProp%text    = 'work array '//aProcesProp%input_item(1)%name
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%output_item(6)%name=aItemProp%name
      aProcesProp%output_item(6)%type=IOTYPE_SEGMENT_WORK
      aProcesProp%output_item(6)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(6)%indx= 6
      aProcesProp%output_item(6)%ip_val= 0
      aProcesProp%input_item(9)%name=aItemProp%name
      aProcesProp%input_item(9)%type=IOTYPE_SEGMENT_WORK
      aProcesProp%input_item(9)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(9)%actdef=-999.
      aProcesProp%input_item(9)%indx  = 9
      aProcesProp%input_item(9)%ip_val  = 0

      ! Add the companion for the TCOUNT input item
      aItemProp%name    = 'TCOUNT    '//aProcesProp%name(1:10)
      aItemProp%default = -999.
      aItemProp%text    = 'time step counter (work array)'
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd( AllItems, aItemProp )
      aProcesProp%output_item(7)%name=aItemProp%name
      aProcesProp%output_item(7)%type=IOTYPE_SEGMENT_OUTPUT
      aProcesProp%output_item(7)%item=>AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(7)%indx= 7
      aProcesProp%output_item(7)%ip_val= 0

!
!     check the use of the key words
!
      DO IKEY = 1 , NOKEY
         IF ( ISUSED(IKEY) == 0 ) THEN
            call status%increase_warning_count()
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

      SUBROUTINE update_process_properties(all_items, process_prop, item_prop, default_value, item_desc, item_ind, item_name, &
      item_type)
!
!     FUNCTION            : Update process properties
!
!     SUBROUTINES CALLED  :
!
!
!     PARAMETERS          :
!
!     NAME    KIND      LENGTH  FUNCT.  DESCRIPTION
!     ----    -----     ------  ------- -----------
!     all_items                  IN/OUT  all items known to the proces system
!     process_prop               IN/OUT  properties for this proces
!     item_prop                  IN/OUT  one item
!     default_value              INPUT   keyword value
!     item_desc                  INPUT   item description
!     item_ind                   INPUT   item index
!     item_name                  INPUT   item name
!

         TYPE(ItemPropColl), INTENT(IN) :: all_items  ! all items of the proces system
         TYPE(ItemProp), INTENT(OUT) :: item_prop  ! one item
         TYPE(ProcesProp), INTENT(OUT) :: process_prop ! output statistical proces definition
         CHARACTER(LEN=20), INTENT(IN) :: item_name
         CHARACTER(LEN=50), INTENT(IN) :: item_desc
         INTEGER(kind=int_wp), INTENT(IN)  ::item_ind, item_type
         INTEGER(kind=int_wp) ::iret
         REAL(kind=real_wp), INTENT(IN)  ::default_value

         item_prop%name    = item_name
         item_prop%default = default_value
         item_prop%text    = item_desc
         item_prop%waqtype = WAQTYPE_NONE

         iret = ItemPropCollAdd( all_items, item_prop )
         process_prop%input_item(item_ind)%name = item_name
         process_prop%input_item(item_ind)%type = item_type
         process_prop%input_item(item_ind)%item => all_items%ItemPropPnts(iret)%pnt
         process_prop%input_item(item_ind)%actdef = default_value
         process_prop%input_item(item_ind)%indx  = item_ind
         process_prop%input_item(item_ind)%ip_val  = 0

      END SUBROUTINE update_process_properties

      end module m_setday
