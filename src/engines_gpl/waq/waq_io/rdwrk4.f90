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
module m_rdwrk4
    use m_waq_precision
    use m_srstop

    implicit none

contains


    subroutine rdwrk4 (iin, lurep, modid, sysid, notot, &
            nodump, nosys, nobnd, nowst, nocons, &
            nopa, noseg, nseg2, coname, paname, &
            funame, nofun, sfname, nosfun, nodisp, &
            novelo, diname, vename, idpnt, ivpnt, &
            ndmpar, ntdmpq, ntdmps, noqtt, noraai, &
            ntraaq, nobtyp, nowtyp, nogrid, grdref, &
            sysgrd, sysndt)
        !>\file
        !>                          Reads part of the DelwaQ system file
        !>
        !>                          This routine re-reads the DelwaQ system file.\n
        !>                          It is called by dlwqp1, to get the names of the
        !>                          substances, constants, parameters, functions and
        !>                          segment functions.\n
        !>                          This information is used by dlwqp1 to set up the
        !>                          administratration around processes and to sort the
        !>                          processes in appropriate order.

        !     Deltares Software Centre

        !     LOGICAL UNITNUMBERS : IIN     - system intermediate file
        !                           LUREP   - monitoring output file

        !     SUBROUTINES CALLED  : SRSTOP, stops execution

        use dlwqgrid_mod
        use timers       !   performance timers

        implicit none

        !     Parameters         :

        !     kind           function         name                Descriptipon

        integer(kind = int_wp), intent(in) :: iin                !< system intermediate file
        integer(kind = int_wp), intent(in) :: lurep              !< unit number report file
        integer(kind = int_wp), intent(in) :: notot              !< Number of systems
        integer(kind = int_wp), intent(in) :: nogrid             !< Number of grids
        integer(kind = int_wp), intent(in) :: nodump             !< Number of dump segments
        integer(kind = int_wp), intent(in) :: nosys              !< Number of active systems
        integer(kind = int_wp), intent(in) :: nobnd              !< Number of open boundaries
        integer(kind = int_wp), intent(in) :: nowst              !< Number of load locations
        integer(kind = int_wp), intent(in) :: nocons             !< Number of constants used
        integer(kind = int_wp), intent(in) :: nopa               !< Number of parameters
        integer(kind = int_wp), intent(in) :: noseg              !< Number of segments
        integer(kind = int_wp), intent(in) :: nseg2              !< Number of layered bed segments
        integer(kind = int_wp), intent(in) :: nofun              !< Number of functions ( user )
        integer(kind = int_wp), intent(in) :: nosfun             !< Number of segment functions
        integer(kind = int_wp), intent(in) :: nodisp             !< Number of dispersion array's
        integer(kind = int_wp), intent(in) :: novelo             !< Number of velocity array's
        integer(kind = int_wp), intent(in) :: ndmpar             !< number of dump areas
        integer(kind = int_wp), intent(in) :: ntdmpq             !< total number exchanges in dump area
        integer(kind = int_wp), intent(in) :: ntdmps             !< total number segments in dump area
        integer(kind = int_wp), intent(in) :: noqtt              !< total number of exchanges inclusive NOQ4
        integer(kind = int_wp), intent(in) :: noraai             !< number of raaien
        integer(kind = int_wp), intent(in) :: ntraaq             !< total number of exch. in raaien
        integer(kind = int_wp), intent(in) :: nobtyp             !< Number of boundarie types
        integer(kind = int_wp), intent(in) :: nowtyp             !< Number of waste load types
        integer(kind = int_wp), intent(out) :: idpnt (nosys)     !< Pointers to dispersion array
        integer(kind = int_wp), intent(out) :: ivpnt (nosys)     !< Pointers to velocity array
        character(40), intent(out) :: modid (4)         !< Model and run-ID
        character(20), intent(out) :: sysid (notot)    !< Systems ID
        character(20), intent(out) :: coname(nocons)    !< Constant names
        character(20), intent(out) :: paname(nopa)      !< Parameter names
        character(20), intent(out) :: funame(nofun)    !< Function names
        character(20), intent(out) :: sfname(nosfun)    !< Segment function names
        character(20), intent(out) :: diname(nodisp)    !< Dispersion array names
        character(20), intent(out) :: vename(novelo)    !< Velocity array names
        integer(kind = int_wp), intent(out) :: grdref(nogrid)     !< Reference grid number
        integer(kind = int_wp), intent(out) :: sysgrd(notot)     !< Grid number substance
        integer(kind = int_wp), intent(out) :: sysndt(notot)     !< Step size substance

        !     Local

        integer(kind = int_wp) :: idummy         !  dummy integer
        real(kind = real_wp) :: rdummy         !  dummy real
        character(20) c20dum        !  dummy 20 byte character
        character(40) c40dum        !  dummy 40 byte character
        integer(kind = int_wp) :: nosss          !  total number of computational volumes
        integer(kind = int_wp) :: i, k           !  loop variables
        integer(kind = int_wp) :: igrid          !  loop variable
        integer(kind = int_wp) :: iseg           !  loop variable
        integer(kind = int_wp) :: isys           !  loop variable
        integer(kind = int_wp) :: ierror         !  error return variable
        type(GridPointer) :: aGrid  !  a single grid
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("rdwrk4", ithndl)

        !         read from the system file

        nosss = noseg + nseg2

        !       => group 1

        read (iin, end = 20, err = 20)   modid(1), modid(2), modid(3), modid(4)
        read (iin, end = 20, err = 20) (sysid(k), k = 1, notot)
        if (nodump > 0) read (iin, end = 20, err = 20) (idummy, c20dum, k = 1, nodump)

        !       => group 2

        if (ndmpar > 0) read (iin, end = 20, err = 20) (c20dum, k = 1, ndmpar)
        if (ndmpar > 0) read (iin, end = 20, err = 20) (idummy, k = 1, ndmpar)
        if (noraai > 0) read (iin, end = 20, err = 20) (c20dum, k = 1, noraai)

        !       => group 3

        !     sub-grid
        do igrid = 1, nogrid
            read (iin, end = 20, err = 20)  idummy, grdref(igrid), (idummy, iseg = 1, nosss)
        enddo

        !     dummy, the grid structures immediately deallocate the pointers
        do igrid = 1, nogrid
            ierror = GridRead(iin, aGrid, nosss)
            if (ierror /= 0) goto 20
            deallocate(aGrid%finalpointer)
            if (aGrid%space_var_nolay) deallocate(aGrid%nolay_var)
        enddo
        read (iin, end = 20, err = 20) (sysgrd(isys), isys = 1, notot)
        read (iin, end = 20, err = 20) (sysndt(isys), isys = 1, notot)

        !       => group attributes

        read (iin, end = 20, err = 20) (idummy, k = 1, nosss)
        if (nodisp > 0) read (iin, end = 20, err = 20) (diname(k), k = 1, nodisp)
        if (novelo > 0) read (iin, end = 20, err = 20) (vename(k), k = 1, novelo)
        read (iin, end = 20, err = 20) (idpnt(k), k = 1, nosys)
        read (iin, end = 20, err = 20) (ivpnt(k), k = 1, nosys)
        if (nobnd  > 0) read (iin, end = 20, err = 20) (idummy, k = 1, nobnd)
        if (nobnd  > 0) read (iin, end = 20, err = 20) (idummy, k = 1, nobnd)
        if (ndmpar > 0) read (iin, end = 20, err = 20) (idummy, i = 1, ndmpar), (idummy, i = 1, ntdmpq)
        if (ndmpar > 0) read (iin, end = 20, err = 20) (idummy, i = 1, ndmpar), (idummy, i = 1, ntdmps)
        if (noraai > 0) then
            read (iin, end = 20, err = 20)  (idummy, i = 1, noraai)
            read (iin, end = 20, err = 20)  (idummy, i = 1, noraai)
            read (iin, end = 20, err = 20)  (idummy, i = 1, ntraaq)
        endif
        if (noraai > 0 .or. ndmpar > 0) then
            read (iin, end = 20, err = 20)  (idummy, i = 1, noqtt)
        endif
        if (ndmpar > 0) then
            read (iin, end = 20, err = 20)  (idummy, i = 1, nosss)
        endif
        read (iin, end = 20, err = 20) idummy, (rdummy, k = 1, 3)
        read (iin, end = 20, err = 20) idummy, (rdummy, k = 1, 3)

        if (nobnd  > 0) then
            !          id's and names  (=new! (ver 4.900))
            do i = 1, nobnd
                read (iin, end = 20, err = 20) c20dum, c40dum
            enddo
            !          type-strings  (=new! (ver 4.900))
            read (iin, end = 20, err = 20) (c20dum, k = 1, nobtyp)
            !          type-integers per boundary (=new! (ver 4.900))
            read (iin, end = 20, err = 20) (idummy, k = 1, nobnd)
            !          read time lags
            read (iin, end = 20, err = 20) (idummy, k = 1, nobnd)
        endif

        if (nowst  > 0) then
            !          segnums, id's and names  (=new! (ver 4.900))
            do i = 1, nowst
                read (iin, end = 20, err = 20) idummy, idummy, c20dum, c40dum
            enddo
            !          type-strings  (=new! (ver 4.900))
            read (iin, end = 20, err = 20) (c20dum, k = 1, nowtyp)
            !          type-integers per wasteload (=new! (ver 4.900))
            read (iin, end = 20, err = 20) (idummy, k = 1, nowst)
        endif

        if (nocons > 0) read (iin, end = 20, err = 20) (coname(k), k = 1, nocons)
        if (nopa   > 0) read (iin, end = 20, err = 20) (paname(k), k = 1, nopa)
        if (nofun  > 0) read (iin, end = 20, err = 20) (funame(k), k = 1, nofun)
        if (nosfun > 0) read (iin, end = 20, err = 20) (sfname(k), k = 1, nosfun)

        !         completion successful

        if (timon) call timstop(ithndl)
        return

        !         unsuccessful read

        20 write (lurep, 2010)
        call srstop(1)

        !         output formats

        2010 format ('1  ERROR reading binary system file !!'/ &
                '   initialisation NOT successful    !!'/ &
                '   simulation impossible            !!')

    end
end module m_rdwrk4
