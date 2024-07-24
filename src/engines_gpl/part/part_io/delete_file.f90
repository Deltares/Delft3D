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

module delete_file_mod
    !
    !  module declarations
    !
    !
    !  data definition module(s)
    !
    use m_waq_precision    ! single and double precision
    use timers
    !
    implicit none
    !
contains
    subroutine delete_file (filnam, ierror)
        !
        !     Deltares
        !
        !     created: aug   1993 by jan van beek
        !
        !     function            : deletes a file by name
        !
        !     parameters          :
        !
        !     name    kind     length     funct.  description
        !     ----    -----    ------     ------- -----------
        !     filnam  char*(*)      1     input   file to be deleted
        !     ierror  integer       1     output  error indication
        !
        character(len = *) :: filnam
        integer  (int_wp) :: ierror
        !
        !     local
        !
        integer(int_wp) :: iolun, ilun
        logical(int_wp) :: lopen, lexist
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("delete_file", ithndl)
        !
        !     init
        !
        ierror = 0
        iolun = 0
        !
        !     if file exist
        !
        inquire (file = filnam, exist = lexist)
        if (.not. lexist) goto 9999
        !
        !     open and close file
        !
        if (iolun  /=  0) then
            open  (newunit = iolun, file = filnam, err = 900)
            close (iolun, status = 'delete', err = 900)
        else
            ierror = 1
        endif
        !
        9999 if (timon) call timstop (ithndl)
        return
        !
        !     errors close anyway
        !
        900 continue
        close (iolun)
        ierror = 1
        if (timon) call timstop (ithndl)
        return
    end subroutine
end module
