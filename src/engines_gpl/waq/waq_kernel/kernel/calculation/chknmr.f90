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
      module m_chknmr
      use m_waq_precision


      implicit none

      contains


      subroutine chknmr ( lunrep , num_cells  , iknmrk )
!
!     Deltares
!
!     Created             : Oct. 2007 by Alja Vrieling
!
!     Function            : Changes kenmerk array
!
!     Subroutines called  : None
!
!
!     Declaration of arguments
!
      use timers
      implicit none
      integer(kind=int_wp), intent(in)     ::lunrep         ! unit number of output file
      integer(kind=int_wp), intent(in)     ::num_cells          ! number of segments

      integer(kind=int_wp), intent(inout)  ::iknmrk(num_cells)  ! property array
!
!     Local variables :
!
      integer(kind=int_wp) ::iseg                ! segment index
      integer(kind=int_wp) ::var1,var2,var3,var4 ! 1st, 2nd, 3rd and 4th number of feature
      integer(kind=int_wp) ::icount              ! counts the number of changes
      integer(kind=int_wp) ::ithandl = 0
      if ( timon ) call timstrt ( "chknmr", ithandl )
!
!     on input, the kenmerk-array contains two digits:
!        1*var1 + 10*var2
!     where var1 describes whether a segment is active or not, and var2
!     describes its location with respect to the vertical (top segment,
!     middle, bottom, whole column).
!
!     on output, the kenmerk-array contains four digits:
!        1*var1 + 10*var2 + 100*var3 + 1000*var4
!     Now var1 indicates whether a segment is active AND belongs to the
!     current subdomain. Var2 is left untouched. Var3 contains the initial
!     value of var1. And var4 shows whether the segment belongs to the current
!     subdomain or not.
!
      icount = 0
      do iseg = 1, num_cells
!           Segment belongs to current subdomain
!            - maintain 1st digit
!            - maintain 2nd digit
!            - copy original 1st digit to 3rd digit
!            - set 4th digit to 1
!
            var1 = mod( iknmrk(iseg)    ,10)
            var2 = mod((iknmrk(iseg)/10),10)
            var3 = mod( iknmrk(iseg)    ,10)
            var4 = 1
      iknmrk(iseg) = var1 + 10*var2 + 100*var3 + 1000*var4
      end do
      if ( icount > 0 ) then
         write(lunrep ,*) 'number of changes in feature array', icount
      endif
!
      if ( timon ) call timstop ( ithandl )
      return
      end

      end module m_chknmr
