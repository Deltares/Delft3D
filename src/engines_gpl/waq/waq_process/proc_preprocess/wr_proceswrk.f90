module m_wr_proceswrk
    use m_waq_precision

    implicit none

contains

    !----- GPL ---------------------------------------------------------------------
    !
    !  Copyright (C)  Stichting Deltares, 2011-2024.
    !
    !  This program is free software: you can redistribute it and/or modify
    !  it under the terms of the GNU General Public License as published by
    !  the Free Software Foundation version 3.
    !
    !  This program is distributed in the hope that it will be useful,
    !  but WITHOUT ANY WARRANTY; without even the implied warranty of
    !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    !  GNU General Public License for more details.
    !
    !  You should have received a copy of the GNU General Public License
    !  along with this program.  If not, see <http://www.gnu.org/licenses/>.
    !
    !  contact: delft3d.support@deltares.nl
    !  Stichting Deltares
    !  P.O. Box 177
    !  2600 MH Delft, The Netherlands
    !
    !  All indications and logos of, and references to, "Delft3D" and "Deltares"
    !  are registered trademarks of Stichting Deltares, and remain the property of
    !  Stichting Deltares. All rights reserved.
    !
    !-------------------------------------------------------------------------------
    !
    !

    subroutine wr_proceswrk(lurep, procesdef, nodef, defaul, idpnw, &
            ivpnw, dsto, vsto, locnam, nopred, &
            nocons, nopa, nofun, nosfun, notot, &
            noloc, nodisp, novelo, ndspx, nvelx, &
            nlocx, nosys, nogrid, dename, coname, paname, &
            funame, sfname, syname, intopt, lun, &
            lchar, noutp, ioutps, outputs, ndmpar, &
            nbufmx, versio, ndspn, nveln, nrref, &
            proref, nproc, nflux, novar, nipmsa)

        !     Deltares Software Centre

        !>/File
        !>      write proces work file

        !     Created   : Aug   2012 by Jan van Beek

        use m_wrstoc
        use m_wripro
        use m_setvat
        use m_proc_totals
        use m_intoou
        use m_open_waq_files
        use timers         !< performance timers
        use processet      !< use processet definitions
        use results, only : OutputPointers         !< use results definitions
        implicit none

        ! arguments

        integer(kind = int_wp), intent(in) :: lurep                  !< unit number report file
        type(procespropcoll), intent(in) :: procesdef              !< the proces definition
        integer(kind = int_wp), intent(in) :: nodef                  !< number of default values
        real(kind = real_wp), intent(in) :: defaul(*)              !< array with default
        integer(kind = int_wp), intent(in) :: idpnw(*)               !< new dispersion pointers
        integer(kind = int_wp), intent(in) :: ivpnw(*)               !< new velocity pointers
        real(kind = real_wp), intent(in) :: dsto(*)                !< dispersion stochi factors
        real(kind = real_wp), intent(in) :: vsto(*)                !< velocity stochi factors
        character(len = 20), intent(in) :: locnam(*)              !< name of the local variables
        integer(kind = int_wp), intent(in) :: nopred                 !< number of predfined values
        integer(kind = int_wp), intent(in) :: nocons                 !< number of constants
        integer(kind = int_wp), intent(in) :: nopa                   !< number of parameters
        integer(kind = int_wp), intent(in) :: nofun                  !< number of functions
        integer(kind = int_wp), intent(in) :: nosfun                 !< number of segment functions
        integer(kind = int_wp), intent(in) :: notot                  !< number of substances
        integer(kind = int_wp), intent(in) :: noloc                  !< number of local values
        integer(kind = int_wp), intent(in) :: nodisp                 !< number of dispersions
        integer(kind = int_wp), intent(in) :: novelo                 !< number of velocities
        integer(kind = int_wp), intent(in) :: ndspx                  !< number of dispersions
        integer(kind = int_wp), intent(in) :: nvelx                  !< number of velocities
        integer(kind = int_wp), intent(in) :: nlocx                  !< number of local values on exchanges
        integer(kind = int_wp), intent(in) :: nosys                  !< number of active substances
        integer(kind = int_wp), intent(in) :: nogrid                 !< number of grids
        character(len = 20), intent(in) :: dename(*)              !< default names
        character(len = 20), intent(in) :: coname(*)              !< constant names
        character(len = 20), intent(in) :: paname(*)              !< parameter names
        character(len = 20), intent(in) :: funame(*)              !< function names
        character(len = 20), intent(in) :: sfname(*)              !< segm.func. names
        character(len = 20), intent(in) :: syname(*)              !< substance names
        integer(kind = int_wp), intent(in) :: intopt                 !< integration sub options
        integer(kind = int_wp), intent(inout) :: lun(*)                 !< unit numbers
        character(len = *), intent(in) :: lchar(*)               !< filenames
        integer(kind = int_wp), intent(in) :: noutp                  !< total number of output files
        integer(kind = int_wp), intent(in) :: ioutps(7, *)            !< (old) output structure
        type(OutputPointers), intent(in) :: outputs                !< output structure
        integer(kind = int_wp), intent(in) :: ndmpar                 !< number of dump areas
        integer(kind = int_wp), intent(in) :: nbufmx                 !< maximum buffer length
        real(kind = real_wp), intent(in) :: versio                 !< version number proces definition file
        integer(kind = int_wp), intent(in) :: ndspn                  !< number of new dispersions
        integer(kind = int_wp), intent(in) :: nveln                  !< number of new velocities
        integer(kind = int_wp), intent(in) :: nrref                  !< maximum nr of references to be resolved
        integer(kind = int_wp), intent(in) :: proref(nrref, *)        !< input items to be resolved for each process
        integer(kind = int_wp), intent(out) :: nproc                  !< number of processes
        integer(kind = int_wp), intent(out) :: nflux                  !< number of fluxes
        integer(kind = int_wp), intent(out) :: novar                  !< number of variables
        integer(kind = int_wp), intent(out) :: nipmsa                 !< actual length of pmsa array

        ! local

        integer(kind = int_wp) :: mxpmsa                 !  maximum length of pmsa array
        integer(kind = int_wp) :: nbpr                   !  number of active processes
        integer(kind = int_wp) :: ioffx                  !  offset to the exchange items
        integer(kind = int_wp) :: no_ins          ! number of output items
        integer(kind = int_wp) :: no_ine          ! number of output items
        integer(kind = int_wp) :: no_ous          ! number of output items
        integer(kind = int_wp) :: no_oue          ! number of output items
        integer(kind = int_wp) :: no_flu          ! number of output items
        integer(kind = int_wp) :: no_sto          ! number of output items
        integer(kind = int_wp) :: no_dis          ! number of output items
        integer(kind = int_wp) :: no_vel          ! number of output items
        integer(kind = int_wp), pointer :: nsvar(:)
        integer(kind = int_wp), pointer :: iflux(:)
        integer(kind = int_wp), pointer :: ipmsa(:)
        integer(kind = int_wp), pointer :: ipssa(:)
        integer(kind = int_wp), pointer :: prvvar(:)
        integer(kind = int_wp), pointer :: prvtyp(:)
        integer(kind = int_wp), pointer :: progrd(:)
        integer(kind = int_wp), pointer :: prondt(:)
        real(kind = real_wp), pointer :: stochi(:)
        character*10, allocatable :: pronam(:)
        character*20, allocatable :: varnam(:)       ! variable names
        integer(kind = int_wp), allocatable :: vararr(:)       ! variable array
        integer(kind = int_wp), allocatable :: varidx(:)       ! variable index
        integer(kind = int_wp), allocatable :: vartda(:)       ! variable type of dis-aggregation
        integer(kind = int_wp), allocatable :: vardag(:)       ! variable dis-aggregation variable
        integer(kind = int_wp), allocatable :: vartag(:)       ! variable type of aggregation
        integer(kind = int_wp), allocatable :: varagg(:)       ! variable aggregation variable
        integer(kind = int_wp) :: iproc                  !  loop counter processes
        integer(kind = int_wp) :: ierr2                  !  local error indication
        integer(kind = int_wp) :: ithndl = 0        ! handle for performance timer
        if (timon) call timstrt("wr_proceswrk", ithndl)

        ! count active processes (merge nbpr and nproc?)

        nproc = 0
        nflux = 0

        nbpr = 0
        do iproc = 1, procesdef%cursize
            if (procesdef%procesprops(iproc)%active) then
                nbpr = nbpr + 1
            endif
        enddo

        ! calculate new totals

        call proc_totals(lurep, procesdef, no_ins, no_ine, no_ous, &
                no_oue, no_flu, no_sto, no_dis, no_vel)

        ! calculate and fill output structure

        nipmsa = 0
        ioffx = nopred + nocons + nopa + nofun + nosfun + notot + noloc + nodef
        mxpmsa = no_ine + no_ins + no_ous + no_oue + no_flu
        allocate (nsvar(nbpr))
        allocate (iflux(nbpr))
        allocate (ipmsa(mxpmsa))
        allocate (ipssa(mxpmsa))
        allocate (prvvar(mxpmsa))
        allocate (prvtyp(mxpmsa))
        allocate (progrd(nbpr))
        allocate (prondt(nbpr))
        allocate (pronam(nbpr))
        call intoou (procesdef, nproc, nflux, nsvar, pronam, &
                iflux, ipmsa, ipssa, nipmsa, ioffx, &
                nocons, nopa, nofun, nosfun, notot, &
                nodisp, novelo, nodef, noloc, ndspx, &
                nvelx, nlocx, nopred, prvvar, prvtyp, &
                novar, progrd, prondt)

        deallocate(ipmsa, ipssa)

        ! set variables attribute's for aggregation dis-aggregation

        allocate(varnam(novar))
        allocate(vararr(novar))
        allocate(varidx(novar))
        allocate(vartda(novar))
        allocate(vardag(novar))
        allocate(vartag(novar))
        allocate(varagg(novar))
        call setvat (lurep, nocons, nopa, nofun, nosfun, &
                nosys, notot, nodisp, novelo, nodef, &
                noloc, ndspx, nvelx, nlocx, nflux, &
                nopred, novar, vararr, varidx, vartda, &
                vardag, vartag, varagg, nogrid, coname, &
                paname, funame, sfname, dename, syname, &
                locnam, varnam)
        deallocate(varnam)

        ! write stochi file, set stochi array, balance output settings

        if (btest(intopt, 3) .and. .not. btest(intopt, 4)) then
            call open_waq_files (lun(36), lchar(36), 36, 1, ierr2)
        endif
        allocate(stochi(notot * no_flu))
        call wrstoc (procesdef, lun(36), notot, syname, stochi, &
                noutp, ioutps, outputs, ndmpar, nbufmx, &
                intopt)
        if (btest(intopt, 3) .and. .not. btest(intopt, 4)) then
            close (lun(36))
        endif

        ! write process intermediate file

        call open_waq_files (lun(24), lchar(24), 24, 1, ierr2)
        call wripro (nproc, nsvar, iflux, nipmsa, prvvar, &
                prvtyp, noloc, nodef, defaul, pronam, &
                nflux, lun(24), versio, stochi, notot, &
                nosys, ndspx, nvelx, nlocx, dsto, &
                vsto, ndspn, idpnw, nveln, ivpnw, &
                progrd, prondt, novar, vararr, varidx, &
                vartda, vardag, vartag, varagg, nrref, &
                proref)
        close (lun(24))
        deallocate(stochi, nsvar, iflux)
        deallocate(prvvar, prvtyp, progrd, prondt)
        deallocate(pronam)
        deallocate(vararr, varidx, vartda, vardag, vartag, varagg)

        if (timon) call timstop(ithndl)
        return
    end

end module m_wr_proceswrk
