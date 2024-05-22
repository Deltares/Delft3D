!!  Copyright (C)  Stichting Deltares, 2021-2024.
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

subroutine ddc_version(lunrep)

    use delwaq_version_module
    use m_date_time_utils_external, only : write_date_time

    ! print version to report file

    integer, intent(in)      :: lunrep        ! unit number report file

    ! local variables

    character(len=120)       :: idstr         ! identification string
    character(len=20)        :: rundat        ! date and time string
    integer                  :: lennam        ! length of a string (dummy here)

    ! set version

    call getfullversionstring_delwaq(idstr)
    k = len_trim(idstr)

    ! write credentials to report file

    call write_date_time(rundat)
    write(lunrep, *)
    write(lunrep, '(a)') idstr(5:k)
    write(lunrep, *)
    write(lunrep,'(2a)') ' execution start: ',rundat
    write(lunrep,*)

    return
end subroutine
