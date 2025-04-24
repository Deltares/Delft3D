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

    subroutine rdfnam(lun, ifnam, fnam, nfil, iout, &
            &                    ipri, alone, hyd)
        !
        use m_waq_precision             ! single and double precision
        use timers
        use rd_token        ! tokenized reading like in DELWAQ
        !
        !  module procedure(s)
        !
        use m_hydmod
        use genfil_mod
        use delete_file_mod
        implicit none
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
        type(t_hydrodynamics) :: hyd

        !     local scalars

        logical lexist          ! help variable
        character(1) cchar_save         ! save comment character from Delwaq
        integer(int_wp) lunut_save      ! save LU-unit output from Delwaq
        integer(int_wp) luinp_save      ! save LU-unit input from Delwaq
        integer(int_wp) npos_save       ! save position on line from Delwaq
        integer(int_wp) :: i, ierr, indx, ipp, isep, max, ifil
        integer(4) ithndl              ! handle to time this subroutine
        integer(4) luninfil
        character(len = 256) finame, input_save
        character(:), allocatable :: banner_text

        data ithndl/0/
        if (timon) call timstrt("rdfnam", ithndl)

        !     check if ifnam exists first

        inquire (file = ifnam, exist = lexist)
        if (.not. lexist) then

            !       input file doesn't exist

            write (*, 99002) ifnam
            call stop_exit(1)
        end if

        !       note:
        !       The output file names are derived from the input file (*.inp).
        !       The name of the hyd file is contained in the input file itself (*.hyd).

        open (newunit = luninfil, file = ifnam)
        fnam(1) = ifnam
        lun(1) = luninfil
100     continue



        isep = max(1, scan( fnam(1), '/\', back = .true. ))

        ipp = index(fnam(1) (isep:), '.') + isep - 1
        write (fnam(2), '(a,a)') fnam(1) (:ipp), 'out'
        write (fnam(8), '(a,a)') fnam(1) (:ipp), 'map'
        write (fnam(9), '(a,a)') fnam(1) (:ipp), 'plo'
        write (fnam(13), '(a,a)') fnam(1) (:ipp), 'his'

        open (newunit = lun(2), file = fnam(2))

        !     16. base file name for tracking nefis file (trk-... *.dat)
        !        (base file name derived from input file - 1rst file)

        fnam(16) = fnam(1)

        call genfil(fnam(16), 'trk-', indx)
        lun(16) = lun(15) + 1
        call delete_file(fnam(16) (:indx) // '.dat', ierr)
        call delete_file(fnam(16) (:indx) // '.def', ierr)

        !!     17. depth file (*.dps) for bathymetry.  removed here and moved to below.
        !!     18. hyd file

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
            if ( ifil > 1 ) then
                input_save = lch(ifil-1)
                luinp_save = ilun(ifil-1)
                cchar_save = cchar
                lunut_save = file_unit
                npos_save = npos
            endif
        end if
        cchar = ';'
        file_unit = lun(2)
        push = .false.
        npos = 200
        iposr = 0
        open (newunit = ilun(ifil), file = lch(ifil))
        lun(1) = ilun(ifil)

        !       read irrelevant information

        if (gettoken(finame, ierr) /= 0) goto 1000     ! version
        if (gettoken(finame, ierr) /= 0) goto 1000     ! 4 titles
        if (gettoken(finame, ierr) /= 0) goto 1000
        if (gettoken(finame, ierr) /= 0) goto 1000
        if (gettoken(finame, ierr) /= 0) goto 1000
        if (gettoken(fnam(18), ierr) /= 0) goto 1000   ! name of the hyd file (actually used)


        hyd%file_hyd%name = fnam(18)
        call read_hyd(hyd)
        call read_hyd_init(hyd)

        lun(18) = lun(17) + 1

        !     17. depth file (*.dps) for bathymetry
        !        (base file name derived from lga file - 3rd file)  later move it to hyd file.  fill in fnam string, from hyd file.

        isep = max(1, scan( fnam(18), '/\', back = .true. ))
        ipp = index(fnam(18) (isep:), '.') + isep - 1
        write (fnam(17), '(a,a)') fnam(18) (:ipp), 'dps'

        !!     3-7,17. lga,lgt,cco,vol,flo,his.  The file name should be read from inp file/hyd file.
        !write (fnam(17), '(a,a)') hyd%FILE_DPS%NAME


        write (fnam(3) , '(a,a)') hyd%FILE_LGA%NAME
        write (fnam(4) , '(a,a)') hyd%FILE_LGT%NAME
        write (fnam(5) , '(a,a)') hyd%FILE_CCO%NAME
        write (fnam(6) , '(a,a)') hyd%FILE_VOL%NAME
        write (fnam(7) , '(a,a)') hyd%FILE_FLO%NAME

        !     25. base file name for tracking binary his file (trk-... *.his)
        !        (base file name derived from input file - 1rst file)

        fnam(25) = fnam(16) (:indx) // '.his'
        call delete_file(fnam(25), ierr)

        if (ipri == 0) then
            open (newunit = lun(iout), file = fnam(iout), form = 'formatted')
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
                ilun(ifil-1) = file_unit
                lch(ifil-1)  = input_save
                ilun(ifil-1) = luinp_save
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
