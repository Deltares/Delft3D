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

subroutine write_error_message(string,ierr)
!**********************************************************************
!     +------------------------+
!     |    D E L T A R E S     |
!     +------------------------+
!
!***********************************************************************
!
!     Project : Open Processes Library
!     Author  : Jan van Beek, Arjen Markus
!     Date    : 091019             Version : 1.00
!
!     History :
!
!     Date    Author          Description
!     ------  --------------  -----------------------------------
!     091019  Arjen Markus    Adapted original for use in OPL
!***********************************************************************
!
!     Description of the module :
!
!        Print an error message and stop the program
!        This is a simplified version of the original to make sure
!        that the message is always visible.
!
!        Reason:
!        On Windows files opened to a unit number in the main program
!        seem to be independent of the files opened to the same unit
!        number in a DLL. As the original writes to the monitoring
!        file, the message would be hidden in a file like "fort.88"
!
    character*(*) string

    write(*,*) string
    write(*,*) 'The program stopped because of this'
    read (*,*)
    stop (1)

end subroutine write_error_message
