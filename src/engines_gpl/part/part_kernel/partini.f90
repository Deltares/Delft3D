!!  Copyright (C)  Stichting Deltares, 2012-2025.
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

module partini_mod
    !
    !  module declarations
    !
    !  data definition module(s)
    !
    use m_waq_precision ! single/double precision
    use timers
    !
    !  module procedure(s)
    !
    use get_key_mod
    use grid_search_mod
    use pinpok_mod
    !use wait_mod
    !
    implicit none
    !
contains
    subroutine partini(nopart, nosubs, ini_file, wpart, xpart, &
            ypart, zpart, npart, mpart, kpart, &
            iptime, lunpr)
        !
        !     programmer : jan van beek
        !     function   : set up of initial condition for a specific number of particles
        !     date       : sep 2011
        !
        !
        !     method     : for each partical the position and weigth of the first subtance is read and assigned
        !
        integer(int_wp) :: nopart, nosubs
        integer(int_wp) :: lunpr
        character(len = *) :: ini_file

        integer(int_wp), pointer, dimension(:) :: iptime
        integer(int_wp), pointer, dimension(:) :: npart, mpart, kpart
        real   (sp), pointer, dimension(:) :: xpart, ypart, zpart
        real   (sp), pointer, dimension(:, :) :: wpart

        integer(int_wp) :: lun_ini
        integer(int_wp) :: ios
        integer(int_wp) :: i
        integer(int_wp) :: np
        integer(int_wp) :: nopart_ini
        integer(int_wp) :: nosubs_ini
        integer(int_wp) :: nosubs_ext
        real :: rdummy
        !
        !     local scalars
        !
        integer(int_wp) :: len_file
        !
        !     required, otherwise under linux the built-in
        !     random generator will be used, rather than the
        !     part generator.
        !
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("partini", ithndl)

        len_file = len_trim(ini_file)

        open(newunit = lun_ini, file = ini_file, access = 'stream', form = 'unformatted', status = 'old', iostat = ios)
        if (ios /= 0) go to 900
        read(lun_ini, end = 920, err = 930) nopart_ini, nosubs_ini
        if (nosubs_ini > nosubs) then
            write (lunpr, *) ' warning, extra nosubs in initial condition not used'
            nosubs_ext = nosubs_ini - nosubs
            nosubs_ini = nosubs
        else
            nosubs_ext = 0
        endif
        do np = 1, nopart_ini
            wpart(1:nosubs, np) = 0.0
            read (lun_ini, end = 920, err = 930) mpart(np), npart(np), kpart(np), xpart(np), ypart(np), zpart(np), &
                    (wpart(i, np), i = 1, nosubs_ini), (rdummy, i = 1, nosubs_ext)
            iptime(np) = 0
        enddo
        nopart = nopart_ini

        if (timon) call timstop (ithndl)
        return
        !     error handling

        900 write(*, '(//a,a)')       ' Error: problem with ini-file ', ini_file(:len_file)
        write(*, '(a)')           ' Could not open/find ini-file ??'
        !call wait
        write(lunpr, '(//a,a)')   ' Error: problem with ini-file ', ini_file(:len_file)
        write(lunpr, '(a,a)')     ' Could not open/find ini-file ??'
        stop  ' Part aborted'

        920 write(*, '(//a,a)')       ' Error: problem with ini-file ', ini_file(:len_file)
        write(*, '(//a,a)')       ' End-of-file found on ini-file '
        !call wait
        write(lunpr, '(//a,a)')   ' Error: problem with ini-file ', ini_file(:len_file)
        write(lunpr, '(//a,a)')   ' End-of-file found on ini-file '
        stop  ' Part aborted'

        930 write(*, '(//a,a)')       ' Error: problem with ini-file ', ini_file(:len_file)
        write(*, '(//a,a)')       ' Error while reading ini-file'
        !call wait
        write(lunpr, '(//a,a)')   ' Error: problem with ini-file ', ini_file(:len_file)
        write(lunpr, '(//a,a)')   ' Error while reading ini-file'
        stop  ' Part aborted'

    end subroutine partini

end module
