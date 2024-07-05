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
module m_dlwq64
    use m_waq_precision

    implicit none

contains


    !> Makes a mass balance final to steady state solutions.
    subroutine dlwq64(disp, disper, area, flow, aleng, &
            velo, conc, bound, ipoint, nosys, &
            notot, noq1, noq2, noq, nodisp, &
            novelo, idpnt, ivpnt, integration_id, amass2, &
            ilflag, dmpq, ndmpq, iqdmp)

        use timers

        real(kind = real_wp),   intent (in   ):: disp(3)        !< Main dispersion in the 3 directions
        real(kind = real_wp),   intent (in   ):: disper(*)      !< Additional dispersion (NODISP*NOQ)
        real(kind = real_wp),   intent (in   ):: area(*)        !< Exchange surface area
        real(kind = real_wp),   intent (in   ):: flow(*)        !< Flows accross exchange surfs
        real(kind = real_wp),   intent (in   ):: aleng(*)       !< From- and to lengths
        real(kind = real_wp),   intent (in   ):: velo(*)        !< Additional velocity
        real(kind = real_wp),   intent (in   ):: conc(*)        !< Concentrations
        real(kind = real_wp),   intent (in   ):: bound(*)       !< Boundary concentrations
        integer(kind = int_wp), intent(in   ) :: ipoint(4, *)   !< Exchange indices
        integer(kind = int_wp), intent(in   ) :: nosys          !< Number  of active substances
        integer(kind = int_wp), intent(in   ) :: notot          !< Number  of total substances
        integer(kind = int_wp), intent(in   ) :: noq1           !< Number of exchanges in first direction
        integer(kind = int_wp), intent(in   ) :: noq2           !< Number of exchanges in second direction
        integer(kind = int_wp), intent(in   ) :: noq            !< Total number of exchanges
        integer(kind = int_wp), intent(in   ) :: nodisp         !< Number of additional dispersions
        integer(kind = int_wp), intent(in   ) :: novelo         !< Number of additional velocities
        integer(kind = int_wp), intent(in   ) :: idpnt(*)       !< Pointer systems to dispersions
        integer(kind = int_wp), intent(in   ) :: ivpnt(*)       !< Pointer systems to velocities
        integer(kind = int_wp), intent(in   ) :: integration_id !< = 0, 2 DISP at zero flow
                                                                !< = 1, 3 no DISP at zero flow
                                                                !< = 0, 1 DISP over boundary
                                                                !< = 2, 3 no DISP over boundary
        real(kind = real_wp),   intent(inout) :: amass2(*)      !< Mass balance
        integer(kind = int_wp), intent(in   ) :: ilflag         !< If 0 then 3 length values
        real(kind = real_wp),   intent(inout) :: dmpq(*)        !< Mass balance dumped exchange if intopt>7
        integer(kind = int_wp), intent(in   ) :: ndmpq          !< Number of dumped exchanges
        integer(kind = int_wp), intent(in   ) :: iqdmp(*)       !< Pointer dumped exchanges

        ! Local variables
        logical :: ibflag
        integer(kind = int_wp) :: i, j, iq, is, i3, i4, i5, i6
        integer(kind = int_wp) :: ioptm, ipb, ipq, k1, k2
        integer(kind = int_wp) :: ithandl = 0

        real(kind = real_wp) :: q, a, al, e, dl, d, v, dq

        if (timon) call timstrt ("dlwq64", ithandl)

        ! Loop accross the number of exchanges
        i4 = 3 * notot
        i5 = 4 * notot
        i6 = nosys * ndmpq
        ibflag = mod(integration_id, 16) >= 8
        !
        do iq = 1, noq

            ! Initialistations, check for transport anyhow
            i = ipoint(1, iq)
            j = ipoint(2, iq)
            if (i == 0 .or. j == 0) goto 60

            ! Check if exchange is dump exchange, set ipb
            if (ibflag) then
                if (iqdmp(iq) > 0) then
                    ipb = iqdmp(iq)
                    ipq = (iqdmp(iq) - 1) * nosys
                else
                    ipb = 0
                end if
            else
                ipb = 0
            end if
            if (i > 0 .and. j > 0 .and. ipb == 0) goto 60
            a = area(iq)
            q = flow(iq)
            if (mod(integration_id, 2) == 1) then
                if (abs(q) < 10.0e-25)  goto 60
            end if
            e = disp(1)
            al = aleng(1)
            if (iq > noq1) then
                e = disp (2)
                al = aleng(2)
            end if
            if (iq > noq1 + noq2) then
                e = disp (3)
                al = aleng(3)
            end if
            if (ilflag == 1) then
                dl = a / (aleng(2 * iq - 1) + aleng(2 * iq))
            else
                dl = a / al
            end if
            e = e * dl
            if (i < 0) goto 20
            if (j < 0) goto 40

            ! The regular case
            k1 = (i - 1) * notot
            k2 = (j - 1) * notot
            do i3 = 1, notot
                is = min (i3, nosys)

                ! Dispersion
                if (idpnt(is) > 0) then
                    d = e + disper((iq - 1) * nodisp + idpnt(is)) * dl
                else
                    d = e
                end if

                ! Flow
                if (ivpnt(is) > 0) then
                    v = q + velo  ((iq - 1) * novelo + ivpnt(is)) * a
                else
                    v = q
                end if

                ! Transport
                if (v > 0.0) then
                    dq = (v + d) * conc(k1 + i3) - d * conc(k2 + i3)
                else
                    dq = (v - d) * conc(k2 + i3) + d * conc(k1 + i3)
                end if

                ! Mass balance
                if (dq > 0.0) then
                    dmpq(ipq + i3) = dmpq(ipq + i3) + dq
                else
                    dmpq(ipq + i3 + i6) = dmpq(ipq + i3 + i6) - dq
                end if
                !
            end do
            goto 60

            ! The 'from' element was a boundary. Note the 2 options.
            20 if (j < 0) goto 60
            k1 = (-i - 1) * notot
            k2 = (j - 1) * notot
            do i3 = 1, notot
                is = min (i3, nosys)
                v = q
                d = 0.0
                if (ivpnt(is) > 0) v = v + velo  ((iq - 1) * novelo + ivpnt(is)) * a
                if (mod(integration_id, 4) <  2) then
                    d = e
                    if (idpnt(is)>0) d = d + disper((iq - 1) * nodisp + idpnt(is)) * dl
                end if
                if (v > 0.0) then
                    dq = (v + d) * bound(k1 + i3) - d * conc (k2 + i3)
                else
                    dq = (v - d) * conc (k2 + i3) + d * bound(k1 + i3)
                end if
                if (dq > 0.0) then
                    amass2(i3 + i4) = amass2(i3 + i4) + dq
                else
                    amass2(i3 + i5) = amass2(i3 + i5) - dq
                end if
                if (ipb > 0) then
                    if (dq > 0.0) then
                        dmpq(ipq + i3) = dmpq(ipq + i3) + dq
                    else
                        dmpq(ipq + i3 + i6) = dmpq(ipq + i3 + i6) - dq
                    end if
                end if
            end do
            goto 60

            ! The 'to' element was a boundary.
            40 k1 = (i - 1) * notot
            k2 = (-j - 1) * notot
            do i3 = 1, notot
                is = min (i3, nosys)
                v = q
                d = 0.0
                if (ivpnt(is) > 0) v = v + velo  ((iq - 1) * novelo + ivpnt(is)) * a
                if (mod(integration_id, 4)  <  2) then
                    d = e
                    if (idpnt(is)>0) d = d + disper((iq - 1) * nodisp + idpnt(is)) * dl
                end if
                if (v > 0.0) then
                    dq = (v + d) * conc (k1 + i3) - d * bound(k2 + i3)
                else
                    dq = (v - d) * bound(k2 + i3) + d * conc (k1 + i3)
                end if
                if (dq > 0.0) then
                    amass2(i3 + i5) = amass2(i3 + i5) + dq
                else
                    amass2(i3 + i4) = amass2(i3 + i4) - dq
                end if
                if (ipb > 0) then
                    if (dq > 0.0) then
                        dmpq(ipq + i3) = dmpq(ipq + i3) + dq
                    else
                        dmpq(ipq + i3 + i6) = dmpq(ipq + i3 + i6) - dq
                    end if
                end if
            end do

            ! End of the loop over exchanges
            60 continue
        end do
        if (timon) call timstop (ithandl)
    end subroutine dlwq64
end module m_dlwq64
