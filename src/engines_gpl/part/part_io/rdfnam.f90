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
module m_rdfnam
    use m_delpar_startup_screen
    use m_stop_exit

    implicit none

contains

    subroutine rdfnam(lun, ifnam, fnam, nfil, iout,            &
   &                    ipri, alone)
!
        use m_waq_precision             ! single and double precision
        use timers
        use rd_token        ! tokenized reading like in DELWAQ
!
!  module procedure(s)
!
        use genfil_mod
        use delete_file_mod
        implicit none             ! force explicit typing
!
!                   Deltares
!
!                        d e l p a r    v2.00
!
!     system administration : m. zeeuw
!
!     created               : february 1990, by l. postma
!
!     function              : reads the file names for the discrete
!                             particle model.
!
!
!     modification          : april 22nd 1993, m. zeeuw
!                              - checks if filename.dat exist; if not
!                                echo to screen.
!
!     note                  : standard version july 1991
!
!                             within delpar, this routine is invocated
!                             only once, so no value should be saved.
!                             suppress compile variables and/or options
!                             for enabling static memory in stead of
!                             dynamically memory. also dynamic memory
!                             allocation is assumed default for this
!                             routine.
!
!     logical unit numbers  : lun    - array with lu-numbers
!
!     subroutines called    : none.
!
!     functions   called    : none.

!     Arguments

!     kind            function         name           description

        integer(int_wp), intent(in) :: nfil          !< number of files
        integer(int_wp), intent(out) :: lun(nfil)     !< array of unit numbers
        character(*), intent(in) :: ifnam         !< name of the input file
        character(*), intent(out) :: fnam(nfil)    !< array of filenames
        integer(int_wp), intent(in) :: iout          !< index of report file in unit nr array
        integer(int_wp), intent(in) :: ipri          !< unit number report file
        logical, intent(in) :: alone         !< .false. if coupled with Delwaq

!     local scalars

        logical lexist          ! help variable
        character(1) cchar_save      ! save value from Delwaq
        integer(int_wp) lunut_save      ! save value from Delwaq
        integer(int_wp) npos_save       ! save value from Delwaq
        integer(int_wp) :: i, ierr, indx, ipp, isep, max, ifil
        integer(4) ithndl              ! handle to time this subroutine
        integer(4) luninfil
        character(len=256) finame
        character(:), allocatable :: banner_text

        data ithndl/0/
        if (timon) call timstrt("rdfnam", ithndl)

!     check if ifnam exists first

        inquire (file=ifnam, exist=lexist)
        if (.not. lexist) then

!       input file doesn't exist

            write (*, 99002) ifnam
            call stop_exit(1)
        end if

!       note:
!          15 files in file filename.dat
!          2 extra files will be generated (see below)

        open (newunit=luninfil, file=ifnam)
        do i = 1, 15 ! initial it was 15
            read (luninfil, *, end=100) lun(i), fnam(i)
            lun(i) = lun(i) + 900
        end do
100     continue

!     16. base file name for tracking nefis file (trk-... *.dat)
!        (base file name derived from input file - 1rst file)

        fnam(16) = fnam(1)
        call genfil(fnam(16), 'trk-', indx)
        lun(16) = lun(15) + 1
        call delete_file(fnam(16) (:indx)//'.dat', ierr)
        call delete_file(fnam(16) (:indx)//'.def', ierr)

!     17. depth file (*.dps) for bathymetry
!        (base file name derived from lga file - 3rd file)

        lun(17) = lun(16) + 1
        do i = len(fnam(3)), 1, -1
!                          backward slash
            if (fnam(3) (i:i) == char(92) .or. &
                fnam(3) (i:i) == '/') go to 110
        end do
110     isep = max(i, 1)
        ipp = index(fnam(3) (isep:), '.') + isep - 1
        write (fnam(17), '(a,a)') fnam(3) (:ipp), 'dps'

101     continue

!     18. hyd file

        open (newunit=lun(iout), file=fnam(iout))
        banner_text = delpar_startup_screen()
        write (lun(2), "(A)") banner_text
        write (*, "(A)") banner_text

!     find the .hyd file

        if (alone) then                 ! clean initialization
            ilun(1) = lun(1)
            lch(1) = fnam(1)
            ifil = 1
        else                              ! add delpar to the delwaq read-stack
            do ifil = 1, lstack              ! at the first free entry
                if (ilun(ifil) /= 0) cycle
                ilun(ifil) = 950 + ifil
                lch(ifil) = fnam(1)
                exit
            end do
            write (file_unit, '(/'' including file: '',a )') fnam(1)
            cchar_save = cchar
            lunut_save = file_unit
            npos_save = npos
        end if
        cchar = ';'
        file_unit = lun(2)
        push = .false.
        npos = 200
        iposr = 0
        open (newunit=ilun(ifil), file=lch(ifil))
        lun(1) = ilun(ifil)

!       read irrelevant information

        if (gettoken(finame, ierr) /= 0) goto 1000     ! version
        if (gettoken(finame, ierr) /= 0) goto 1000     ! 4 titles
        if (gettoken(finame, ierr) /= 0) goto 1000
        if (gettoken(finame, ierr) /= 0) goto 1000
        if (gettoken(finame, ierr) /= 0) goto 1000
        if (gettoken(fnam(18), ierr) /= 0) goto 1000   ! name of the hyd file (actually used)
        lun(18) = lun(17) + 1

!     25. base file name for tracking binary his file (trk-... *.his)
!        (base file name derived from input file - 1rst file)

        fnam(25) = fnam(16) (:indx)//'.his'
        call delete_file(fnam(25), ierr)

        if (ipri == 0) then
            open (newunit=lun(iout), file=fnam(iout), form='formatted')
        else
            write (lun(iout), *) ' Filenames :'
            do i = 1, 18
                write (lun(iout), 99001) i, lun(i), fnam(i)
            end do
        end if

        if (.not. alone) then        ! reset Delwaq settings
            if (ifil /= 1) then
                write (file_unit, '(/'' Closing file: '',A )') fnam(1)
                close (ilun(ifil))
                ilun(ifil) = 0
                write (file_unit, '(/'' Continuing on file: '',A )') lch(ifil - 1)
                cchar = cchar_save
                file_unit = lunut_save
                npos = npos_save
            end if
        end if
        close (luninfil)
        if (timon) call timstop(ithndl)
        return

1000    write (*, *) ' Error : reading the input file: ', fnam(1)
        call stop_exit(1)
        return
!
!     formats
!
99001   format(5x, 2i5, 5x, a80)
99002   format(' Input file :', a13, &
               ' does not exist in current directory...')
!
    end subroutine

end module m_rdfnam
