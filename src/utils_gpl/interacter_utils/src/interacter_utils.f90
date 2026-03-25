!----- LGPL --------------------------------------------------------------------
!  Copyright (C)  Stichting Deltares, 2011-2026.
!  LGPL-2.1 – see <http://www.gnu.org/licenses/>.
!-------------------------------------------------------------------------------
module interacter_utils
   use iso_c_binding, only: c_int
   implicit none
   private

   public :: patch_interacter_sendmessage

   interface
      subroutine patch_interacter_sendmessage() bind(c, name='patch_interacter_sendmessage')
      end subroutine
   end interface

end module interacter_utils