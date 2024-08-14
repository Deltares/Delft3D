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
module m_inipart

    implicit none

contains


    subroutine inipart(lgrid, lgrid2, num_rows, num_columns, xcor, &
            ycor, nopart, nosubs, subst, ini_file, &
            xpol, ypol, npol, wpart, xpart, &
            ypart, zpart, npart, mpart, kpart, &
            iptime, npmax, nrowsmax, lunpr)

        !
        !     programmer : antoon koster
        !     function   : set up of initial condition for oil patches
        !     date       : may 2004
        !
        !
        !     method     : for each of the oil patches defined in the part ini-file,
        !                  their total mass will be spread out uniformly over the
        !                  circumscribing polygone.
        !
        !                  for each of these polygone the
        !                       - oil fraction
        !                       - total mass
        !                       - number of particles to be spread
        !                  must be provided in the header of the ini-file (tekal)
        !
        !                  spreading occurs by random sampling of the polygone
        !                  area by random pairs (x,y) for the given number of
        !                  particles.
        !
        !                  finally all spread particles will be assigned the
        !                  average mass (total mass/no. of particles)
        !
        !                  the oil patch is supposed to be floating oil, so located
        !                  at z=0.0 of the top layer (k=1)
        !
        use m_skip_comments
        use m_waq_precision ! single/double precision
        use timers
        use get_key_mod
        use grid_search_mod
        use pinpok_mod
        use random_generator
        use m_stop_exit
        use m_get_index

        implicit none

        !     Arguments

        !     kind           function         name                      description

        integer  (int_wp), intent(in) :: num_rows                    !< first dimension matrix
        integer  (int_wp), intent(in) :: num_columns                    !< second dimension matrix
        integer  (int_wp), intent(in) :: npmax                   !< maximum number of particles
        integer  (int_wp), intent(inout) :: nopart                  !< number of active particles
        integer  (int_wp), intent(in) :: lgrid (num_rows, num_columns)       !< active grid matrix
        integer  (int_wp), intent(in) :: lgrid2(num_rows, num_columns)       !< total grid matrix
        real     (real_wp), intent(in) :: xcor  (num_rows * num_columns)
        real     (real_wp), intent(in) :: ycor  (num_rows * num_columns)
        integer  (int_wp), intent(inout) :: nosubs                  !< number of substances
        character(*), intent(in) :: subst (*)               !< substance names
        character(*), intent(in) :: ini_file                !< polygon file
        integer  (int_wp), intent(in) :: npol                    !< number of substances
        integer  (int_wp), intent(in) :: nrowsmax                !< dimension of poligons
        real     (real_wp), intent(out) :: xpol  (nrowsmax)        !< xvalues polygons
        real     (real_wp), intent(out) :: ypol  (nrowsmax)        !< yvalues polygons
        real     (real_wp), intent(out) :: wpart (nosubs, npmax)    !< weight of the particles
        real     (real_wp), intent(out) :: xpart (npmax)           !< x of theparticles
        real     (real_wp), intent(out) :: ypart (npmax)           !< y of the particles
        real     (real_wp), intent(out) :: zpart (npmax)           !< z of the particles
        integer  (int_wp), intent(out) :: npart (npmax)           !< n of the particles
        integer  (int_wp), intent(out) :: mpart (npmax)           !< m of the particles
        integer  (int_wp), intent(out) :: kpart (npmax)           !< k of the particles
        integer  (int_wp), intent(out) :: iptime(npmax)           !< time in the system
        integer  (int_wp), intent(in) :: lunpr                   !< unit nr of the diagnostics file

        integer(int_wp), parameter :: max_len_line = 200
        integer(int_wp), parameter :: max_len_blockname = 4
        integer(int_wp), parameter :: max_len_key = 20

        integer(int_wp) :: lun_ini
        integer(int_wp) :: ios, ier
        integer(int_wp) :: npart_pol, isub, i, np, nerr
        integer(int_wp) :: nrows, ncols
        integer(int_wp) :: nnpart, mmpart
        integer(int_wp) :: inside

        real   (sp) :: big
        real   (sp) :: xmin, xmax, ymin, ymax, xx, yy
        real   (sp) :: totmass, avgmass
        real   (sp) :: xxcel, yycel
        logical :: okay
        logical :: polygone_complete
        logical :: end_of_file, read_error
        logical :: key_found
        logical :: substance_found

        character(len = max_len_blockname) :: blok
        character(len = max_len_key) :: key
        character(len = max_len_line) :: fract

        real (dp) :: rseed = 0.5d0
        !
        !     local scalars
        !
        integer(int_wp) :: ipol, len_file, len_fract
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("inipart", ithndl)

        big = huge(xpol) ! largest number

        okay = .true.
        polygone_complete = .false.
        len_file = len_trim(ini_file)

        open(newunit = lun_ini, file = ini_file, status = 'old', iostat = ios)
        if (ios /= 0) go to 900

        do ipol = 1, npol
            write(lunpr, *) 'Polygon ', ipol
            xmin = big
            ymin = big
            xmax = -big
            ymax = -big
            !
            !        read header information
            !
            !        example:
            !        *  fraction:    'ekofisk'
            !        *  total mass:   550000  kg
            !        *  particles:    20000
            !
            !
            !        get substance name for oil fraction
            !        (must match one of the substance on part input file)
            !
            key = 'fraction'
            call get_string_key(lun_ini, &
                    'fraction', fract, len_fract, key_found)
            if (.not. key_found) go to 910

            !        get total mass
            key = 'mass'
            call get_real_key  (lun_ini, 'mass', totmass, key_found)
            if (.not. key_found) go to 910

            !        get no. of particles
            key = 'particles'
            call get_int_key   (lun_ini, 'particles', npart_pol, key_found)
            if (.not. key_found) go to 910

            avgmass = totmass / npart_pol  ! average mass per particle

            isub = get_index (nosubs, subst, fract) ! find proper index
            substance_found = isub >= 0
            if (.not. substance_found) go to 940
            !
            !        read polygone (tekal format)
            !
            call skip_comment_lines(lun_ini, ios)
            end_of_file = ios < 0
            read_error = ios > 0
            if (end_of_file) go to 920
            if (read_error) go to 930

            read(lun_ini, '(a)', iostat = ios) blok
            end_of_file = ios < 0
            read_error = ios > 0
            if (end_of_file) go to 920
            if (read_error) go to 930

            call skip_comment_lines(lun_ini, ios)
            end_of_file = ios < 0
            read_error = ios > 0
            if (end_of_file) go to 920
            if (read_error) go to 930

            read(lun_ini, *, iostat = ios) nrows, ncols
            end_of_file = ios < 0
            read_error = ios > 0
            if (end_of_file) go to 920
            if (read_error) go to 930

            write(lunpr, *) trim(fract), totmass, npart_pol, avgmass
            do i = 1, nrows
                polygone_complete = .false.
                read(lun_ini, *, iostat = ios) xpol(i), ypol(i)
                end_of_file = ios < 0
                read_error = ios > 0
                if (end_of_file) go to 920
                if (read_error) go to 930
                !
                !           find circumscribing rectangle for limiting area
                !           for polygone sampling
                !
                xmin = min(xmin, xpol(i))
                xmax = max(xmax, xpol(i))
                ymin = min(ymin, ypol(i))
                ymax = max(ymax, ypol(i))
            enddo
            polygone_complete = .true.
            !
            !        double sample polygone area by random (x,y) pairs,
            !        resulting in uniform spreading of particles over polygone area
            !
            np = 0
            nerr = 0
            do while (np < npart_pol)
                xx = rnd(rseed) * (xmax - xmin) + xmin
                yy = rnd(rseed) * (ymax - ymin) + ymin
                call pinpok(xx, yy, nrows, xpol, ypol, inside)
                if (inside == 1) then
                    !
                    !               transform world coordinates (xx,yy) into cell-coordinates
                    !               (xxcel,yycel)
                    !
                    call part07 (lgrid, lgrid2, num_rows, num_columns, xcor, &
                            ycor, xx, yy, nnpart, mmpart, &
                            xxcel, yycel, ier)

                    if (ier == 0) then
                        nerr = 0
                        np = np + 1 !count spread particles for polygone
                        nopart = nopart + 1 !count total number spread particles

                        xpart(nopart) = xxcel    !  0 <= xxcel <= 1
                        ypart(nopart) = yycel    !  0 <= yycel <= 1

                        npart(nopart) = nnpart
                        mpart(nopart) = mmpart
                        !
                        !                  locate particles at water surface
                        !                  (top of surface layer (z=0.0 ; k=1))
                        !
                        zpart(nopart) = 0.0   ! top of layer
                        kpart(nopart) = 1     ! top layer (at water surface)

                        wpart(isub, nopart) = avgmass
                        iptime(nopart) = 0     ! age of particle
                    else
                        nerr = nerr + 1
                        if (nerr > 10000) go to 950
                    end if
                else
                    np = np ! debug statement
                endif
            enddo
            !
            okay = ios==0
        enddo
        close (lun_ini)
        if (timon) call timstop (ithndl)
        return
        !     error handling

        900 write(*, '(//a,a)')       ' Error: problem with ini-file ', ini_file(:len_file)
        write(*, '(a)')           ' Could not open/find ini-file ??'
        write(lunpr, '(//a,a)')   ' Error: problem with ini-file ', ini_file(:len_file)
        write(lunpr, '(a,a)')     ' Could not open/find ini-file ??'
        call stop_exit(1)

        910 write(*, '(//a,a)')       ' Error: problem with ini-file ', ini_file(:len_file)
        write(*, '(a,a)')         ' Could not find key ', key
        write(lunpr, '(//a,a)')   ' Error: problem with ini-file ', ini_file(:len_file)
        write(lunpr, '(a,a)')     ' Could not find key ', key
        call stop_exit(1)

        920 write(*, '(//a,a)')       ' Error: problem with ini-file ', ini_file(:len_file)
        write(*, '(//a,a)')       ' End-of-file found on ini-file '
        write(lunpr, '(//a,a)')   ' Error: problem with ini-file ', ini_file(:len_file)
        write(lunpr, '(//a,a)')   ' End-of-file found on ini-file '
        call stop_exit(1)

        930 write(*, '(//a,a)')       ' Error: problem with ini-file ', ini_file(:len_file)
        write(*, '(//a,a)')       ' Error while reading ini-file'
        write(lunpr, '(//a,a)')   ' Error: problem with ini-file ', ini_file(:len_file)
        write(lunpr, '(//a,a)')   ' Error while reading ini-file'
        call stop_exit(1)

        940 write(*, '(//a,a)')       ' Error: problem with ini-file ', ini_file(:len_file)
        write(*, '(//a,a)')       ' Could not find substance ', fract
        write(lunpr, '(//a,a)')   ' Error: problem with ini-file ', ini_file(:len_file)
        write(lunpr, '(//a,a)')   ' Could not find substance ', fract
        call stop_exit(1)

        950 write(*, '(//a,a)')       ' Error: problem with ini-file ', ini_file(:len_file)
        write(*, '(//a)')         ' Couldn''t find cells for the initial oil particles.'
        write(*, '(//a)')         ' Is (part of) the polygon within the grid range?'
        write(lunpr, '(//a,a)')   ' Error: problem with ini-file ', ini_file(:len_file)
        write(lunpr, '(//a)')     ' Couldn''t find cells for the initial oil particles.'
        write(lunpr, '(//a)')     ' Is (part of) the polygon within the grid range?'
        call stop_exit(1)

    end subroutine inipart

end module m_inipart
