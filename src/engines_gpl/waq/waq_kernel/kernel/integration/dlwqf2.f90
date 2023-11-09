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
module m_dlwqf2

implicit none

contains


      subroutine dlwqf2 ( noseg  , nobnd  , idt    , volnew , trace  )

!     Deltares - Delft Software Department

!     Created   : Sept.1996 by Leo Postma

!     Function  : set diagonal, fast solvers version

!     Modified  : Nov. 1996, Kian Tan    : RHS moved to DLWQF4
!                 July 2008, Leo Postma  : WAQ perfomance timers
!                 July 2009, Leo Postma  : double precission version

      use timers                         ! WAQ performance timers

      implicit none

!     Arguments           :

!     Kind        Function         Name                  Description

      integer(4), intent(IN   ) :: noseg               ! Number of computational volumes
      integer(4), intent(IN   ) :: nobnd               ! Number of open boundaries
      integer(4), intent(IN   ) :: idt                 ! Time step size in scu's
      real   (4), intent(IN   ) :: volnew(noseg      ) ! Volumes end of time step
      real   (8), intent(  OUT) :: trace (noseg+nobnd) ! Diagonal vector

!     Local declarations

      real   (8) dt                                    ! Time step in double
      integer(4) iseg                                  ! Loop variable

!     The WAQ-timer

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqf2", ithandl )

!         set the diagonal

      dt = idt

      do iseg = 1 , noseg
         trace(iseg) = volnew(iseg)/dt
      enddo

      do iseg = noseg+1 , noseg+nobnd
         trace(iseg) = 1.0
      enddo

      if ( timon ) call timstop ( ithandl )

      return
      end

end module m_dlwqf2
