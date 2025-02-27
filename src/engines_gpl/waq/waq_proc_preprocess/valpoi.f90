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
module m_valpoi
   use m_waq_precision
   use m_string_utils

   implicit none

   integer(kind=int_wp), parameter, public :: nopred = 6
   integer, parameter, private :: nzoek = 20
   character(len=nzoek), private :: predef(nopred) = [character(len=nzoek) :: &
                                                      'VOLUME', &
                                                      'ITIME', &
                                                      'IDT', &
                                                      'DELT', &
                                                      'ITSTRT', &
                                                      'ITSTOP']

contains

   subroutine valpoi(num_substances_total, num_spatial_parameters, num_spatial_time_fuctions, syname, num_constants, &
                     num_time_functions, constants, paname, funame, sfname, &
                     valnam, ivalip, line)
      !     Function            : sets pointers for process parametrs

      use m_waq_data_structure
      use timers !   performance timers

      integer(kind=int_wp), intent(in) :: num_substances_total !< Total number of substances
      integer(kind=int_wp), intent(in) :: num_spatial_parameters !< Number of parameters
      integer(kind=int_wp), intent(in) :: num_spatial_time_fuctions !< Number of segment functions
      integer(kind=int_wp), intent(in) :: num_constants !< Number of constants used
      integer(kind=int_wp), intent(in) :: num_time_functions !< Number of functions ( user )
      integer(kind=int_wp), intent(out) :: ivalip !< Pointer in SSA.

      character(len=*), intent(in) :: valnam !< Name of variable in question
      character(len=*), intent(out) :: line !< Report line

      character(len=*), intent(in) :: syname(num_substances_total) !< Constant names
      character(len=*), intent(in) :: paname(num_spatial_parameters) !< Parameter names
      character(len=*), intent(in) :: funame(num_time_functions) !< Function names
      character(len=*), intent(in) :: sfname(num_spatial_time_fuctions) !< Segment function names

      type(t_waq_item), intent(in) :: constants !< delwaq constants list
      !
      !     Local
      !
      integer(kind=int_wp) :: isys, isfun, ipa, ifun, ico
      integer(kind=int_wp) :: ithndl = 0

      if (timon) call timstrt("VALPOI", ithndl)
      !
      !
      !
      !     determine how VAL is modelled
      !
      !     Predefined ?
      !
      ivalip = index_in_array(valnam(:nzoek), predef)
      if (ivalip == 1) then
         write (line, '(A)') '       Using DELWAQ volume'
         goto 800
      end if
      if (ivalip == 2) then
         write (line, '(A)') '       Using DELWAQ time'
         goto 800
      end if
      if (ivalip == 3) then
         write (line, '(A)') '       Using DELWAQ timestep'
         goto 800
      end if
      if (ivalip == 4) then
         write (line, '(A)') '       Using DELWAQ timestep in days'
         goto 800
      end if
      if (ivalip == 5) then
         write (line, '(A)') '       Using DELWAQ start time'
         goto 800
      end if
      if (ivalip == 6) then
         write (line, '(A)') '       Using DELWAQ stop time'
         goto 800
      end if
      !
      !     as model variable ?
      !
      isys = index_in_array(valnam(:nzoek), syname)
      if (isys > 0) then
         write (line, '(A,I3)') '       Using substance nr ', isys
         ivalip = nopred + num_constants + num_spatial_parameters + num_time_functions + num_spatial_time_fuctions + isys
         goto 800
      end if
      !
      !     as segment function ?
      !
      isfun = index_in_array(valnam(:nzoek), sfname)
      if (isfun > 0) then
         write (line, '(A,I3)') '       Using segment function nr', isfun
         ivalip = nopred + num_constants + num_spatial_parameters + num_time_functions + isfun
         goto 800
      end if
      !
      !     as function ?
      !
      ifun = index_in_array(valnam(:nzoek), funame)
      if (ifun > 0) then
         write (line, '(A,I3)') '       Using function nr', ifun
         ivalip = nopred + num_constants + num_spatial_parameters + ifun
         goto 800
      end if
      !
      !     as parameter ?
      !
      ipa = index_in_array(valnam(:nzoek), paname)
      if (ipa > 0) then
         write (line, '(A,I3)') '       Using parameter nr', ipa
         ivalip = nopred + num_constants + ipa
         goto 800
      end if
      !
      !     as constant ?
      !
      ico = constants%find(valnam)
      if (ico > 0) then
         write (line, '(a,i3,a,g13.6)') '       Using constant nr', ico, ' with value:', constants%constant(ico)
         ivalip = nopred + ico
         goto 800
      end if
      !
      !     not found
      !
      ivalip = -1
      !
800   continue
      !
      if (timon) call timstop(ithndl)
      return
   end

end module m_valpoi
