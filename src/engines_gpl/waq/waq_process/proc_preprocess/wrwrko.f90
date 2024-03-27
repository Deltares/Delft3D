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
module m_wrwrko
    use m_waq_precision

    implicit none

contains


    subroutine wrwrko (lunwro, noutp, nbufmx, ioutps, outputs, &
            notot, substdname, subunit, subdescr)

        !     Deltares Software Centre

        !>/File
        !>      write output work file

        !     Created   : Nov   1994 by Jan van Beek
        !     Modified  : Aug   2012 by Jan van Beek, use results structure, modern look and feel

        use timers         !< performance timers
        use results, only : OutputPointers, ncopt

        implicit none

        integer(kind = int_wp), intent(in) :: lunwro                 !< output work file
        integer(kind = int_wp), intent(in) :: noutp                  !< total number of output files
        integer(kind = int_wp), intent(in) :: nbufmx                 !< maximum buffer length
        integer(kind = int_wp), intent(in) :: ioutps(7, *)            !< (old) output structure
        type(OutputPointers), intent(in) :: outputs                !< output structure
        integer(kind = int_wp), intent(in) :: notot                  !< total number of substances
        character*100, intent(in) :: substdname(notot)      !< substance standard name
        character*40, intent(in) :: subunit(notot)         !< substance unit
        character*60, intent(in) :: subdescr(notot)        !< substance description

        ! local

        real(kind = real_wp) :: versio                 !  version number output system
        integer(kind = int_wp) :: k                      !  loop counter
        integer(kind = int_wp) :: nrvart                 !  total number of variables in output
        integer(kind = int_wp) :: ithndl = 0             ! handle for performance timer
        if (timon) call timstrt("wrwrko", ithndl)

        versio = 0.2
        nrvart = outputs%cursize

        ! write work file

        write (lunwro) versio
        write (lunwro) noutp, nrvart, nbufmx, ncopt
        write (lunwro) (ioutps(1, k), k = 1, noutp)
        write (lunwro) (ioutps(2, k), k = 1, noutp)
        write (lunwro) (ioutps(3, k), k = 1, noutp)
        write (lunwro) (ioutps(4, k), k = 1, noutp)
        write (lunwro) (ioutps(5, k), k = 1, noutp)
        write (lunwro) (ioutps(6, k), k = 1, noutp)
        if (nrvart>0) then
            write (lunwro) (outputs%pointers(k), k = 1, nrvart)
            write (lunwro) (outputs%names   (k), k = 1, nrvart)
            write (lunwro) (outputs%std_var_name(k), k = 1, nrvart)
            write (lunwro) (outputs%units   (k), k = 1, nrvart)
            write (lunwro) (outputs%description  (k), k = 1, nrvart)
        end if
        if (notot>0) then
            write (lunwro) (substdname(k), k = 1, notot)
            write (lunwro) (subunit   (k), k = 1, notot)
            write (lunwro) (subdescr  (k), k = 1, notot)
        end if

        if (timon) call timstop(ithndl)
        return
        return
    end

end module m_wrwrko
