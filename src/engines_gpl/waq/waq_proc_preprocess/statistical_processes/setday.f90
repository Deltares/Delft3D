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
module m_setday
   use m_waq_precision
   use m_string_utils
   use ProcesSet
   use m_error_status

   implicit none

contains

   subroutine SETDAY(LUNREP, NOKEY, &
                     KEYNAM, KEYVAL, &
                     is_date_format, is_yyddhh_format, &
                     IPROC, aProcesProp, &
                     AllItems, status)
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
      use m_logger_helper, only: stop_with_error
      use m_string_manipulation, only: get_trimmed_length

      use timers !   performance timers
      use date_time_utils, only: convert_string_to_time_offset, convert_period_to_timer, convert_relative_time
!
      implicit none
!
!     Declaration of arguments
!
      integer(kind=int_wp) :: LUNREP, NOKEY, IPROC, item_ind
      logical is_date_format, is_yyddhh_format
      character(len=20) KEYNAM(NOKEY), KEYVAL(NOKEY)
      type(ProcesProp) :: aProcesProp ! output statistical proces definition
      type(ItemPropColl) :: AllItems ! all items of the proces system

      type(error_status), intent(inout) :: status !< current error status
!
!     Local declarations
!
      integer(kind=int_wp) :: IERR_ALLOC, IKEY, ISLEN, IERR2, IRET
      integer(kind=int_wp) :: istart, iperiod
      integer(kind=int_wp), allocatable :: ISUSED(:)
      character(len=20) SUFFIX, NAME, item_name
      character(len=50) item_desc
      real(kind=real_wp) :: PERIOD, default_value
      type(ItemProp) :: aItemProp ! one item
      integer(kind=int_wp) :: ithndl = 0
      if (timon) call timstrt("setday", ithndl)
!
!     init
!
      allocate (ISUSED(NOKEY), STAT=IERR_ALLOC)
      if (IERR_ALLOC /= 0) then
         write (LUNREP, *) 'ERROR allocating buffer array:', IERR_ALLOC
         write (LUNREP, *) 'in routine SETDAY_3, buffer length:', NOKEY
         write (*, *) 'ERROR allocating buffer array:', IERR_ALLOC
         call stop_with_error()
      end if
      ISUSED = 0

      IKEY = index_in_array('OUTPUT-OPERATION', KEYNAM)
      if (IKEY > 0) then
         ISUSED(IKEY) = 1
      end if
!
!     Fill the Proces Properties
!
      aProcesProp%name = 'STADAY'
      write (aProcesProp%name(7:10), '(I4.4)') IPROC
      aProcesProp%routine = 'STADAY'
      aProcesProp%text = 'periodic average, periodic minimum, periodic maximum'
      aProcesProp%swtransp = 123
      aProcesProp%type = PROCESTYPE_OUTPUT
      aProcesProp%no_input = 9
      aProcesProp%no_output = 7
      aProcesProp%no_FluxOutput = 0
      aProcesProp%no_FluxStochi = 0
      aProcesProp%no_DispStochi = 0
      aProcesProp%no_VeloStochi = 0
      allocate (aProcesProp%input_item(aProcesProp%no_input), &
                aProcesProp%output_item(aProcesProp%no_output), &
                STAT=IERR_ALLOC)
      if (IERR_ALLOC /= 0) then
         write (LUNREP, *) 'ERROR allocating IOitem array:', IERR_ALLOC
         write (LUNREP, *) 'in routine SETDAY_1, array length:', aProcesProp%no_input, aProcesProp%no_output
         write (*, *) 'ERROR allocating array:', IERR_ALLOC
         call stop_with_error()
      end if
!
      IKEY = index_in_array('SUBSTANCE', KEYNAM)
      if (IKEY <= 0) then
         write (LUNREP, *) 'ERROR no parameter specified for statistics'
         call status%increase_error_count()
      else
         ISUSED(IKEY) = 1
         aProcesProp%input_item(1)%name = KEYVAL(IKEY)
         aProcesProp%input_item(1)%type = IOTYPE_SEGMENT_INPUT
         aProcesProp%input_item(1)%actdef = -999.
         aProcesProp%input_item(1)%indx = 1
         aProcesProp%input_item(1)%ip_val = 0
         aItemProp%name = KEYVAL(IKEY)
         iret = ItemPropCollFind(AllItems, aItemProp)
         if (iret <= 0) then
            aItemProp%text = 'input parameter for statistics'
            aItemProp%default = -999.
            aItemProp%waqtype = WAQTYPE_NONE
            iret = ItemPropCollAdd(AllItems, aItemProp)
         end if
         aProcesProp%input_item(1)%item => AllItems%ItemPropPnts(iret)%pnt
      end if

      IKEY = index_in_array('TINIT', KEYNAM)
      if (IKEY <= 0) then
         istart = 0
      else
         ISUSED(IKEY) = 1
         read (KEYVAL(IKEY), '(I20.0)', IOSTAT=IERR2) istart
         if (IERR2 /= 0) then
            call convert_string_to_time_offset(KEYVAL(IKEY), istart, .false., .false., IERR2)
            if (IERR2 /= 0) then
               write (LUNREP, *) 'ERROR interpreting start time:', &
                  KEYVAL(IKEY)
               call status%increase_error_count()
            end if
         else
            call convert_relative_time(istart, 1, is_date_format, is_yyddhh_format)
         end if
      end if

      item_desc = 'start time for statistics'
      item_ind = 2
      item_name = 'TINIT'//aProcesProp%name(1:10)
      call update_process_properties(AllItems, aProcesProp, aItemProp, real(istart), item_desc, item_ind, item_name, &
                                     IOTYPE_SEGMENT_INPUT)
!
      IKEY = index_in_array('PERIOD', KEYNAM)
      if (IKEY <= 0) then
         iperiod = 86400.
      else
         ISUSED(IKEY) = 1
         read (KEYVAL(IKEY), '(I20.0)', IOSTAT=IERR2) iperiod
         if (IERR2 /= 0) then
            call convert_period_to_timer(KEYVAL(IKEY), iperiod, .false., .false., IERR2)
            if (IERR2 /= 0) then
               write (LUNREP, *) 'ERROR interpreting period:', KEYVAL(IKEY)
               call status%increase_error_count()
            end if
         else
            call convert_relative_time(iperiod, 1, is_date_format, is_yyddhh_format)
         end if
      end if

      item_desc = 'period of time averaged output'
      item_ind = 3
      item_name = 'PERIOD'//aProcesProp%name(1:10)
      call update_process_properties(AllItems, aProcesProp, aItemProp, real(iperiod), item_desc, item_ind, item_name, &
                                     IOTYPE_SEGMENT_INPUT)
!
      aItemProp%name = 'ITIME'
      iret = ItemPropCollFind(AllItems, aItemProp)
      if (iret <= 0) then
         aItemProp%default = -999.
         aItemProp%text = 'time in calculation'
         aItemProp%waqtype = WAQTYPE_DEFAULT
         iret = ItemPropCollAdd(AllItems, aItemProp)
      end if
      aProcesProp%input_item(4)%name = aItemProp%name
      aProcesProp%input_item(4)%type = IOTYPE_SEGMENT_INPUT
      aProcesProp%input_item(4)%item => AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(4)%actdef = -999.
      aProcesProp%input_item(4)%indx = 4
      aProcesProp%input_item(4)%ip_val = 0
!
      item_desc = 'time step'
      item_ind = 5
      item_name = 'IDT'

      aItemProp%name = item_name
      iret = ItemPropCollFind(AllItems, aItemProp)
      if (iret <= 0) then
         aItemProp%default = -999.
         aItemProp%text = 'time step'
         aItemProp%waqtype = WAQTYPE_DEFAULT
         iret = ItemPropCollAdd(AllItems, aItemProp)
      end if

      aProcesProp%input_item(item_ind)%name = item_name
      aProcesProp%input_item(item_ind)%type = IOTYPE_SEGMENT_INPUT
      aProcesProp%input_item(item_ind)%item => AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(item_ind)%actdef = -999.
      aProcesProp%input_item(item_ind)%indx = item_ind
      aProcesProp%input_item(item_ind)%ip_val = 0
!
      item_desc = 'time step counter'
      item_ind = 6
      item_name = 'TCOUNT    '//aProcesProp%name(1:10)
      call update_process_properties(AllItems, aProcesProp, aItemProp, 0.0, item_desc, item_ind, item_name, IOTYPE_SEGMENT_WORK)
!
      IKEY = index_in_array('SUFFIX', KEYNAM)
      if (IKEY <= 0) then
         SUFFIX = ' '
      else
         SUFFIX = KEYVAL(IKEY)
         ISUSED(IKEY) = 1
      end if
      call get_trimmed_length(SUFFIX, ISLEN)
!
      if (SUFFIX(1:ISLEN) /= ' ') then
         aItemProp%name = SUFFIX(1:ISLEN)//'_'//aProcesProp%input_item(1)%name
      else
         aItemProp%name = 'TAVG_'//aProcesProp%input_item(1)%name
      end if
      aItemProp%default = -999.
      aItemProp%text = 'periodic average '//aProcesProp%input_item(1)%name
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd(AllItems, aItemProp)
      aProcesProp%output_item(1)%name = aItemProp%name
      aProcesProp%output_item(1)%type = IOTYPE_SEGMENT_OUTPUT
      aProcesProp%output_item(1)%item => AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(1)%indx = 1
      aProcesProp%output_item(1)%ip_val = 0
      write (LUNREP, 2000) 'Statistical output named [', aItemProp%name, &
         '] created with periodic average from [', aProcesProp%input_item(1)%name, ']'
!
      !     work array in input and in output
      if (SUFFIX(1:ISLEN) /= ' ') then
         aItemProp%name = SUFFIX(1:ISLEN)//'_'//aProcesProp%input_item(1)%name
      else
         aItemProp%name = 'TMIN_'//aProcesProp%input_item(1)%name
      end if
      aItemProp%default = -999.
      aItemProp%text = 'periodic minimum '//aProcesProp%input_item(1)%name
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd(AllItems, aItemProp)
      aProcesProp%output_item(2)%name = aItemProp%name
      aProcesProp%output_item(2)%type = IOTYPE_SEGMENT_OUTPUT
      aProcesProp%output_item(2)%item => AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(2)%indx = 2
      aProcesProp%output_item(2)%ip_val = 0
      write (LUNREP, 2000) 'Statistical output named [', aItemProp%name, &
         '] created with periodic minimum from [', aProcesProp%input_item(1)%name, ']'
!
      if (SUFFIX(1:ISLEN) /= ' ') then
         aItemProp%name = SUFFIX(1:ISLEN)//'_'//aProcesProp%input_item(1)%name
      else
         aItemProp%name = 'TMAX_'//aProcesProp%input_item(1)%name
      end if
      aItemProp%default = -999.
      aItemProp%text = 'periodic maximum '//aProcesProp%input_item(1)%name
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd(AllItems, aItemProp)
      aProcesProp%output_item(3)%name = aItemProp%name
      aProcesProp%output_item(3)%type = IOTYPE_SEGMENT_OUTPUT
      aProcesProp%output_item(3)%item => AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(3)%indx = 3
      aProcesProp%output_item(3)%ip_val = 0
      write (LUNREP, 2000) 'Statistical output named [', aItemProp%name, &
         '] created with periodic maximum from [', aProcesProp%input_item(1)%name, ']'
!
!     work array in input and in output
!
      if (SUFFIX(1:ISLEN) /= ' ') then
         aItemProp%name = 'T_'//SUFFIX(1:ISLEN)//'_'//aProcesProp%input_item(1)%name
      else
         aItemProp%name = 'T_'//aProcesProp%input_item(1)%name
      end if
      aItemProp%default = -999.
      aItemProp%text = 'work array '//aProcesProp%input_item(1)%name
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd(AllItems, aItemProp)
      aProcesProp%output_item(4)%name = aItemProp%name
      aProcesProp%output_item(4)%type = IOTYPE_SEGMENT_WORK
      aProcesProp%output_item(4)%item => AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(4)%indx = 4
      aProcesProp%output_item(4)%ip_val = 0
      aProcesProp%input_item(7)%name = aItemProp%name
      aProcesProp%input_item(7)%type = IOTYPE_SEGMENT_WORK
      aProcesProp%input_item(7)%item => AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(7)%actdef = -999.
      aProcesProp%input_item(7)%indx = 7
      aProcesProp%input_item(7)%ip_val = 0
!
!     work array in input and in output
!
      if (SUFFIX(1:ISLEN) /= ' ') then
         aItemProp%name = 'MINDYN_'//SUFFIX(1:ISLEN)//'_'//aProcesProp%input_item(1)%name
      else
         aItemProp%name = 'MINDYN_'//aProcesProp%input_item(1)%name
      end if
      aItemProp%default = -999.
      aItemProp%text = 'work array '//aProcesProp%input_item(1)%name
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd(AllItems, aItemProp)
      aProcesProp%output_item(5)%name = aItemProp%name
      aProcesProp%output_item(5)%type = IOTYPE_SEGMENT_WORK
      aProcesProp%output_item(5)%item => AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(5)%indx = 5
      aProcesProp%output_item(5)%ip_val = 0
      aProcesProp%input_item(8)%name = aItemProp%name
      aProcesProp%input_item(8)%type = IOTYPE_SEGMENT_WORK
      aProcesProp%input_item(8)%item => AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(8)%actdef = -999.
      aProcesProp%input_item(8)%indx = 8
      aProcesProp%input_item(8)%ip_val = 0

!
!     work array in input and in output
!
      if (SUFFIX(1:ISLEN) /= ' ') then
         aItemProp%name = 'MAXDYN_'//SUFFIX(1:ISLEN)//'_'//aProcesProp%input_item(1)%name
      else
         aItemProp%name = 'MAXDYN_'//aProcesProp%input_item(1)%name
      end if
      aItemProp%default = -999.
      aItemProp%text = 'work array '//aProcesProp%input_item(1)%name
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd(AllItems, aItemProp)
      aProcesProp%output_item(6)%name = aItemProp%name
      aProcesProp%output_item(6)%type = IOTYPE_SEGMENT_WORK
      aProcesProp%output_item(6)%item => AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(6)%indx = 6
      aProcesProp%output_item(6)%ip_val = 0
      aProcesProp%input_item(9)%name = aItemProp%name
      aProcesProp%input_item(9)%type = IOTYPE_SEGMENT_WORK
      aProcesProp%input_item(9)%item => AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%input_item(9)%actdef = -999.
      aProcesProp%input_item(9)%indx = 9
      aProcesProp%input_item(9)%ip_val = 0

      ! Add the companion for the TCOUNT input item
      aItemProp%name = 'TCOUNT    '//aProcesProp%name(1:10)
      aItemProp%default = -999.
      aItemProp%text = 'time step counter (work array)'
      aItemProp%waqtype = WAQTYPE_NONE
      iret = ItemPropCollAdd(AllItems, aItemProp)
      aProcesProp%output_item(7)%name = aItemProp%name
      aProcesProp%output_item(7)%type = IOTYPE_SEGMENT_OUTPUT
      aProcesProp%output_item(7)%item => AllItems%ItemPropPnts(iret)%pnt
      aProcesProp%output_item(7)%indx = 7
      aProcesProp%output_item(7)%ip_val = 0

!
!     check the use of the key words
!
      do IKEY = 1, NOKEY
         if (ISUSED(IKEY) == 0) then
            call status%increase_warning_count()
            write (LUNREP, *) 'WARNING: keyword not used'
            write (LUNREP, *) 'key   :', KEYNAM(IKEY)
            write (LUNREP, *) 'value :', KEYVAL(IKEY)
         end if
      end do
!
      deallocate (ISUSED)
!
      if (timon) call timstop(ithndl)
      return
2000  format(5a)
   end

   subroutine update_process_properties(all_items, process_prop, item_prop, default_value, item_desc, item_ind, item_name, &
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

      type(ItemPropColl), intent(IN) :: all_items ! all items of the proces system
      type(ItemProp), intent(OUT) :: item_prop ! one item
      type(ProcesProp), intent(OUT) :: process_prop ! output statistical proces definition
      character(LEN=20), intent(IN) :: item_name
      character(LEN=50), intent(IN) :: item_desc
      integer(kind=int_wp), intent(IN) :: item_ind, item_type
      integer(kind=int_wp) :: iret
      real(kind=real_wp), intent(IN) :: default_value

      item_prop%name = item_name
      item_prop%default = default_value
      item_prop%text = item_desc
      item_prop%waqtype = WAQTYPE_NONE

      iret = ItemPropCollAdd(all_items, item_prop)
      process_prop%input_item(item_ind)%name = item_name
      process_prop%input_item(item_ind)%type = item_type
      process_prop%input_item(item_ind)%item => all_items%ItemPropPnts(iret)%pnt
      process_prop%input_item(item_ind)%actdef = default_value
      process_prop%input_item(item_ind)%indx = item_ind
      process_prop%input_item(item_ind)%ip_val = 0

   end subroutine update_process_properties

end module m_setday
