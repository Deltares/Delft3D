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
module m_show_error

implicit none

contains


      subroutine error ( mess )

!       Deltares Software Centre

!>\file
!>                          prints an error message and stops the run with condition 1

      use m_stop_exit
      use fileinfo       ! file information for all input/output files

      implicit none

!     Arguments

!     kind            function         name           description

      character(  *), intent(in   ) :: mess          !< message to be printed before quit

      write( *, 1000 ) mess
      write( *,  *   ) ' PART program aborted '

      write( lunit(2), 1000 ) mess

      call stop_exit(1)

 1000 format(' Error detected: ',/,a,//)
      return

      end

end module m_show_error
