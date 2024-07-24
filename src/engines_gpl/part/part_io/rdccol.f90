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
module m_rdccol
    use m_stop_exit

    implicit none

contains


    subroutine rdccol (num_rows, num_columns, lun, fnam, &
            lgrid, xbott, ybott, lun2)

        !     READING CURVILINEAR CCO FILE
        !          (initially)

        !     system administration : m. zeeuw


        !     created               : juli 1989, by m.e. sileon



        !     modified              : june 1993, by m. zeeuw
        !                             - implemented error numbers
        !                             nov 1997: uses openfl
        !                             dec 1997: read also layt form first record!!
        !                                       and 9 times xdummy

        !     note                  : standard version july 1991

        !     logical unit numbers  : lun

        !     subroutines called    : stop_exit

        !     functions   called    : none.

        use m_waq_precision      ! single and double precision
        use timers
        use openfl_mod

        !     parameters

        !     kind           function         name                Descriptipon

        integer  (int_wp), intent(in) :: num_rows              !< first dimension of the grid
        integer  (int_wp), intent(in) :: num_columns              !< second dimension of the grid
        integer  (int_wp), intent(in) :: lun               !< unit number cco file
        character(*), intent(in) :: fnam              !< name of cco file
        integer  (int_wp), intent(in) :: lgrid(num_rows, num_columns)  !< grid table
        real     (sp), intent(out) :: xbott(*)          !< x-values in the grid
        real     (sp), intent(out) :: ybott(*)          !< y-values in the grid
        integer  (int_wp), intent(in) :: lun2              !< unit number log-file

        !     local scalars

        integer(int_wp)   iocond    ! error indicator for file opening
        integer(int_wp)   nmaxc     ! num_rows in file
        integer(int_wp)   mmaxc     ! num_columns in file
        real   (real_wp)   x0, y0    ! coordinates of the zero
        real   (real_wp)   alpha     ! unknown
        integer(int_wp)   npart     ! unknown
        integer(int_wp)   layt      ! number of layers
        real   (sp)   xdummy    ! help variable
        integer(int_wp)   i, j     ! loop variables

        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("rdccol", ithndl)

        !     open cco-file

        write (lun2, *) ' Opening the grid coordinates file:', fnam(1:len_trim(fnam))
        call openfl (lun, fnam, 0)

        !     read requested data

        read (lun)
        read (lun) mmaxc, nmaxc, x0, y0, alpha, npart, layt
        if (mmaxc /= num_columns .or. nmaxc /= num_rows) then
            write (lun2, *)
            write (lun2, *) ' Error 4201. Dimensioning does not match!'
            write (lun2, *) '             num_rows,num_columns,lgrid-table:   ', num_rows, num_columns
            write (lun2, *) '             num_rows,num_columns,cco-file   :   ', nmaxc, mmaxc
            call stop_exit(1)
        endif

        !     skip header

        do i = 1, 2 * npart + 9
            read (lun) xdummy
        enddo
        read (lun, iostat = iocond) ((xbott(lgrid(i, j)), i = 1, nmaxc), j = 1, mmaxc)
        read (lun, iostat = iocond) ((ybott(lgrid(i, j)), i = 1, nmaxc), j = 1, mmaxc)
        !
        close (lun)

        if (iocond > 0) write (lun2, *) ' Error 4202. Reading cco-file:', fnam
        if (iocond < 0) write (lun2, *) ' Error 4203. Unexpected end cco-file:', fnam
        if (iocond /= 0) call stop_exit(1)
        write (lun2, '(a,a)') '  Succesful reading of the TELMAC-CCO file : ', fnam(1:len_trim(fnam))

        !     end of routine

        if (timon) call timstop (ithndl)
        return

    end subroutine

end module m_rdccol
