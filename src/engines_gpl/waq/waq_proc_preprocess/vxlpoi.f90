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
module m_vxlpoi
   use m_waq_precision
   use m_string_utils

   implicit none

contains

   subroutine VXLPOI(num_constants, num_time_functions, num_dispersion_arrays, num_velocity_arrays, constants, &
                     FUNAME, DINAME, VENAME, VALNAM, IVALIP, &
                     LINE)
      !
      !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
      !
      !     CREATED:    december  1994 by Jan van Beek
      !
      !     FUNCTION            : sets pointers for input on exchange level
      !
      !     LOGICAL UNITNUMBERS :
      !
      !     SUBROUTINES CALLED  : ZOEK  , searches a string in an array

      use m_waq_data_structure

      !     PARAMETERS          : 13
      !
      !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
      !     ----    -----    ------     ------- -----------
      !     num_constants  INTEGER       1     INPUT   Number of constants used
      !     num_time_functions   INTEGER       1     INPUT   Number of functions ( user )
      !     num_dispersion_arrays  INTEGER       1     INPUT   Number of dispersion array's
      !     num_velocity_arrays  INTEGER       1     INPUT   Number of velocity array's
      !     CONAME  CHAR*20   num_constants    INPUT   Constant names
      !     FUNAME  CHAR*20   num_time_functions     INPUT   Function names
      !     DINAME  CHAR*20   num_dispersion_arrays    INPUT   Dispersion names
      !     VENAME  CHAR*20   num_velocity_arrays    INPUT   Velocity names
      !     VALNAM  CHAR*20       1     INPUT   Name of variable in question
      !     IVALIP  INTEGER       1     OUTPUT  Pointer in delwaq array
      !     LINE    CHAR*(*)      1     OUTPUT  Report line
      !
      use timers !   performance timers

      integer(kind=int_wp) :: num_constants, num_time_functions, num_dispersion_arrays, num_velocity_arrays, IVALIP
      character(len=*) VALNAM, LINE
      character(len=*) FUNAME(*), &
         DINAME(*), VENAME(*)
      type(t_waq_item), intent(inout) :: constants !< delwaq constants list
      !
      !     Local
      !
      integer(kind=int_wp), parameter :: NOPREF = 4
      character(len=10) PREDEF(NOPREF)

      integer(kind=int_wp) :: ICO, IDSP, IVEL, IFUN

      integer(kind=int_wp) :: ithndl = 0
      if (timon) call timstrt("vxlpoi", ithndl)
      !
      PREDEF(1) = 'FLOW'
      PREDEF(2) = 'XAREA'
      PREDEF(3) = 'XLENFROM'
      PREDEF(4) = 'XLENTO'
      !
      !
      !     determine how VAL is modelled
      !
      !     Predefined ?
      !
      IVALIP = index_in_array(VALNAM(:10), PREDEF)
      if (IVALIP == 1) then
         write (LINE, '(A)') '       Using DELWAQ flow'
         goto 800
      end if
      if (IVALIP == 2) then
         write (LINE, '(A)') '       Using DELWAQ exchange area'
         goto 800
      end if
      if (IVALIP == 3) then
         write (LINE, '(A)') '       Using DELWAQ from- length'
         goto 800
      end if
      if (IVALIP == 4) then
         write (LINE, '(A)') '       Using DELWAQ to- length'
         goto 800
      end if
      !
      !     as dispersion ?
      !
      IDSP = index_in_array(VALNAM(:10), DINAME(:num_dispersion_arrays))
      if (IDSP > 0) then
         write (LINE, '(A,I3)') '       Using dispersion nr ', IDSP
         IVALIP = NOPREF + IDSP
         goto 800
      end if
      !
      !     as a velocity ?
      !
      IVEL = index_in_array(VALNAM(:10), VENAME(:num_velocity_arrays))
      if (IVEL > 0) then
         write (LINE, '(A,I3)') '       Using velocity nr', IVEL
         IVALIP = NOPREF + num_dispersion_arrays + IVEL
         goto 800
      end if
      !
      !     as function ?
      !
      IFUN = index_in_array(VALNAM(:10), FUNAME(:num_time_functions))
      if (IFUN > 0) then
         write (LINE, '(A,I3)') '       Using function nr', IFUN
         IVALIP = NOPREF + num_dispersion_arrays + num_velocity_arrays + IFUN
         goto 800
      end if
      !
      !     as constant ?
      !
      ico = constants%find(valnam)
      if (ico > 0) then
         write (line, '(a,i3,a,g13.6)') '       Using constant nr', ico, ' with value:', constants%constant(ico)
         ivalip = nopref + num_dispersion_arrays + num_velocity_arrays + num_time_functions + ico
         goto 800
      end if
      !
      !     not found
      !
      IVALIP = -1
      !
800   continue
      !
      if (timon) call timstop(ithndl)
      return
   end

end module m_vxlpoi
