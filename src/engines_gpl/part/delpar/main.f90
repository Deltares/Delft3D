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

program delpar_main
    use m_stop_exit
    use m_delpar
    use m_cli_utils, only: get_arguments

    implicit none

    character(len=256), allocatable :: argv(:)
    character(len=256) :: filepar
    character(len=256) :: fileerr
    character(len=256) :: filename
    character(len=256) :: runid
    logical :: exi
    integer(4) :: ioerr
    integer(4) :: i
    integer(4) :: imdp
    integer(4) :: iinp
    integer(4) :: lunfil

!
!     Retrieve input filename, either from a file called 'runid.par' or as a command line argument
!
    fileerr = "delpar_error.log"
    argv = get_arguments()
    if (size(argv) == 0) then
        filepar = "runid.par"
        inquire (file=filepar, exist=exi)
        if (exi) then
            open (newunit=lunfil, file=filepar)
            read (lunfil, '(a)', iostat=ioerr) runid
            close (lunfil)
            if (ioerr /= 0 .or. runid == ' ') then
                open (newunit=lunfil, file=fileerr, status='replace')
                write (*, '(a)', iostat=ioerr) 'ERROR: No commandline argument, and "runid.par" does not contain a runid'
                write (lunfil, '(a)', iostat=ioerr) 'ERROR: No commandline argument, and "runid.par" does not contain a runid'
                close (lunfil)
                call stop_exit(1)
            end if
            imdp = index(runid, ".mdp")
            iinp = index(runid, ".inp")
            if (imdp == 0 .and. iinp == 0) then
                filename = trim(runid)//".mdp"
            else if (iinp /= 0) then
                filename = runid(1:iinp - 1)//".mdp"
            else
                filename = trim(runid)
            end if
            inquire (file=filename, exist=exi)
            if (.not. exi) then
                open (newunit=lunfil, file=fileerr, status='replace')
                write (*, '(3a)', iostat=ioerr) 'ERROR: Input file from "runid.par" named "', trim(filename), '" was not found'
                write (lunfil, '(3a)', iostat=ioerr) 'ERROR: Input file from "runid.par" named "', trim(filename), '" was not found'
                close (lunfil)
                call stop_exit(1)
            end if
        else
            open (newunit=lunfil, file=fileerr, status='replace')
            write (*, '(a)', iostat=ioerr) 'ERROR: No commandline argument, and "runid.par" not found'
            write (lunfil, '(a)', iostat=ioerr) 'ERROR: No commandline argument, and "runid.par" not found'
            close (lunfil)
            call stop_exit(1)
        end if
    else
        filename = argv(1)
        imdp = index(filename, ".mdp")
        iinp = index(filename, ".inp")
        if (imdp == 0 .and. iinp == 0) then
            filename = trim(filename)//".mdp"
        else if (iinp /= 0) then
            filename = filename(1:iinp - 1)//".mdp"
        end if
        inquire (file=filename, exist=exi)
        if (.not. exi) then
            open (newunit=lunfil, file=fileerr, status='replace')
            write (*, '(3a)', iostat=ioerr) 'ERROR: Input file from commandline argument named "', trim(filename), '" was not found'
            write (lunfil, '(3a)', iostat=ioerr) 'ERROR: Input file from commandline argument named "', trim(filename), &
                '" was not found'
            close (lunfil)
            call stop_exit(1)
        end if
    end if

    call delpar(filename)

end program
