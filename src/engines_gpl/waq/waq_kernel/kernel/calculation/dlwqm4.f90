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
module m_dlwqm4
    use m_waq_precision

    implicit none

contains


    subroutine dlwqm4(isys, nosys, notot, noseg, conc, &
            concvt, nobnd, bound, noq, ipoint, &
            theta, flowtot, disptot, amass2, ndmpq, &
            iqdmp, dmpq, idt)

        !     Deltares - Delft Software Department

        !     Created   :      2007 by Pauline van Slingerland

        !     Function  : updates the mass balance

        !     Modified  : July 2009 by Leo Postma : double precission version

        use timers                         ! WAQ performance timers

        implicit none

        !     Arguments           :

        !     Kind        Function         Name                    Description

        integer(kind = int_wp), intent(in) :: isys                  ! current active substance
        integer(kind = int_wp), intent(in) :: nosys                 ! number of active substances
        integer(kind = int_wp), intent(in) :: notot                 ! total number of substances

        integer(kind = int_wp), intent(in) :: noseg                 ! number of segments
        real(kind = real_wp), intent(in) :: conc   (notot, noseg) ! old concentrations
        real(kind = dp), intent(in) :: concvt (noseg) ! first solution estimation by means of local theta method
        integer(kind = int_wp), intent(in) :: nobnd                 ! number of boundary segments
        real(kind = real_wp), intent(in) :: bound  (nosys, nobnd) ! boundary concentrations
        integer(kind = int_wp), intent(in) :: noq                   ! number of exchanges
        integer(kind = int_wp), intent(in) :: ipoint (4, noq) ! exchange pointers
        real(kind = real_wp), intent(in) :: theta  (noq)        ! local theta coefficients
        real(kind = real_wp), intent(in) :: flowtot(noq)        ! flows plus additional velos.
        real(kind = real_wp), intent(in) :: disptot(noq)        ! dispersion plus additional dipers.

        real(kind = real_wp), intent(inout) :: amass2 (notot, 5) ! amass2(*,1) masses
        ! amass2(*,2) processes
        ! amass2(*,3) discharges
        ! amass2(*,4) incoming boundary transport
        ! amass2(*,5) outgoing boundary transport
        integer(kind = int_wp), intent(in) :: ndmpq                 ! number of dumped exchanges
        integer(kind = int_wp), intent(in) :: iqdmp  (noq)        ! pointers dumped exchages
        real(kind = real_wp), intent(inout) :: dmpq  (nosys, ndmpq, 2) ! dmpq(*,*,1) incoming transport
        ! dmpq(*,*,2) outgoing transport
        integer(kind = int_wp), intent(in) :: idt                   ! time step
        real(kind = real_wp) :: cio, cjo              ! old from- and to concentrations
        real(kind = real_wp) :: cin, cjn              ! new from- and to concentrations
        real(kind = real_wp) :: fluxij                ! flux from i to j
        integer(kind = int_wp) :: ifrom, ito           ! from- and to volume indices
        integer(kind = int_wp) :: iq                    ! current edge

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqm4", ithandl)

        !         flow and diffusion

        do iq = 1, noq
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)

            !             only compute where needed

            if (ifrom > 0 .and. ito > 0 .and. iqdmp(iq) == 0) cycle
            if (ifrom == 0 .or.  ito == 0) cycle

            if (ifrom > 0) then
                cio = conc  (isys, ifrom)
                cin = concvt(ifrom)
            else
                cio = bound (isys, -ifrom)
                cin = bound (isys, -ifrom)
            endif

            if (ito   > 0) then
                cjo = conc  (isys, ito)
                cjn = concvt(ito)
            else
                cjo = bound (isys, -ito)
                cjn = bound (isys, -ito)
            endif

            if (flowtot(iq) > 0) then        ! flow from i to j
                fluxij = theta(iq) * (flowtot(iq) * cin - disptot(iq) * (cjn - cin)) &
                        + (1 - theta(iq)) * (flowtot(iq) * cio - disptot(iq) * (cjo - cio))
            else                                  ! flow from j to i
                fluxij = theta(iq) * (flowtot(iq) * cjn - disptot(iq) * (cjn - cin)) &
                        + (1 - theta(iq)) * (flowtot(iq) * cjo - disptot(iq) * (cjo - cio))
            endif
            if (ifrom < 0) then
                if (fluxij > 0) then
                    amass2(isys, 4) = amass2(isys, 4) + real(idt) * fluxij
                else
                    amass2(isys, 5) = amass2(isys, 5) - real(idt) * fluxij
                endif
            endif
            if (ito   < 0) then
                if (fluxij > 0) then
                    amass2(isys, 5) = amass2(isys, 5) + real(idt) * fluxij
                else
                    amass2(isys, 4) = amass2(isys, 4) - real(idt) * fluxij
                endif
            endif
            if (iqdmp(iq) > 0) then
                if (fluxij > 0) then
                    dmpq(isys, iqdmp(iq), 1) = dmpq(isys, iqdmp(iq), 1) + real(idt) * fluxij
                else
                    dmpq(isys, iqdmp(iq), 2) = dmpq(isys, iqdmp(iq), 2) - real(idt) * fluxij
                endif
            endif
        enddo

        if (timon) call timstop (ithandl)
    end subroutine dlwqm4

end module m_dlwqm4
