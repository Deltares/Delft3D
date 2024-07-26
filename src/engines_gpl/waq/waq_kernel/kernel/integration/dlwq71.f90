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
module m_dlwq71
    use m_waq_precision

    implicit none

contains


    !> Makes a mass balance with central differencing in space.
    subroutine dlwq71(disp, disper, area, flow, aleng, &
            velo, conc, bound, ipoint, num_substances_transported, &
            num_substances_total, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges, num_dispersion_arrays, &
            num_velocity_arrays, idpnt, ivpnt, integration_id, amass2, &
            ilflag, dmpq, ndmpq, iqdmp)

        use timers

        real(kind = real_wp),   intent(in   ) :: disp(3)        !< Dispersion in 3 directions
        real(kind = real_wp),   intent(in   ) :: disper(*)      !< Additional dispersion array (num_dispersion_arrays*num_exchanges)
        real(kind = real_wp),   intent(in   ) :: area(*)        !< Exchange surface area (num_exchanges)
        real(kind = real_wp),   intent(in   ) :: flow(*)        !< Flows accross exchange surfs (num_exchanges)
        real(kind = real_wp),   intent(in   ) :: aleng(*)       !< From- and to lengths (2*num_exchanges)
        real(kind = real_wp),   intent(in   ) :: velo(*)        !< Additional velocity array (num_velocity_arrays*num_exchanges)
        real(kind = real_wp),   intent(in   ) :: conc (*)       !< Concentrations (num_substances_total*num_cells)
        real(kind = real_wp),   intent(in   ) :: bound(*)       !< Boundary concentrations (num_substances_total*?)
        integer(kind = int_wp), intent(in   ) :: ipoint(4, *)   !< Exchange pointers
        integer(kind = int_wp), intent(in   ) :: num_substances_transported          !< Number of active substances
        integer(kind = int_wp), intent(in   ) :: num_substances_total          !< Total number of substances
        integer(kind = int_wp), intent(in   ) :: num_exchanges_u_dir           !< Nr of exchanges in first dir.
        integer(kind = int_wp), intent(in   ) :: num_exchanges_v_dir           !< Nr of exchanges in second dir.
        integer(kind = int_wp), intent(in   ) :: num_exchanges            !< Total number of exchanges
        integer(kind = int_wp), intent(in   ) :: num_dispersion_arrays         !< Number  of additional dispers.
        integer(kind = int_wp), intent(in   ) :: num_velocity_arrays         !< Number  of additional velos.
        integer(kind = int_wp), intent(in   ) :: idpnt(*)       !< Pointer systems to dispersions (num_substances_transported)
        integer(kind = int_wp), intent(in   ) :: ivpnt(*)       !< Pointer systems to velocities (num_substances_transported)
        integer(kind = int_wp), intent(in   ) :: integration_id !< = 0, 2 DISP at zero flow
                                                                !< = 1 or 3 no DISP at zero flow
                                                                !< = 0 or 1 DISP over boundary
                                                                !< = 2 or 3 no DISP over boundary
        real(kind = real_wp), intent(inout)   :: amass2(*)      !< Mass balance array (num_substances_total*5)
        integer(kind = int_wp), intent(in   ) :: ilflag         !< If 0 then 3 length values
        real(kind = real_wp),   intent(inout) :: dmpq(*)        !< Mass balance dumped exchange (num_substances_total*NDMPQ*?)
                                                                !< If INTOPT > 7
        integer(kind = int_wp), intent(in   ) :: ndmpq          !< Number of dumped exchanges
        integer(kind = int_wp), intent(in   ) :: iqdmp(*)       !< Pointer dumped exchanges

        ! Local variables
        integer(kind = int_wp) :: i, i3, i4, i5, i6, is, iq, ibflag, ipb, ipq
        integer(kind = int_wp) :: j, k1, k2
        integer(kind = int_wp) :: ithandl = 0

        real(kind = real_wp) :: a, q, e, d, v, al, dl, dv, dq, f1, f2

        if (timon) call timstrt ("dlwq71", ithandl)

        ! loop accross the number of exchanges
        i4 = 3 * num_substances_total
        i5 = 4 * num_substances_total
        i6 = num_substances_transported * ndmpq
        if (mod(integration_id, 16) >= 8) then
            ibflag = 1
        else
            ibflag = 0
        endif
        !
        do iq = 1, num_exchanges

            ! initialisations , check for transport anyhow
            i = ipoint(1, iq)
            j = ipoint(2, iq)
            if (i == 0 .or. j == 0) goto 60

            ! Check if exchange is dump exchange, set IPB
            if (ibflag == 1) then
                if (iqdmp(iq) > 0) then
                    ipb = iqdmp(iq)
                    ipq = (iqdmp(iq) - 1) * num_substances_transported
                else
                    ipb = 0
                endif
            else
                ipb = 0
            endif
            if (i > 0 .and. j > 0 .and. ipb == 0) goto 60
            a = area(iq)
            q = flow(iq)
            if (mod(integration_id, 2) == 1) then
                if (abs(q) < 10.0e-25)  goto 60
            endif
            e = disp(1)
            al = aleng(1)
            if (iq > num_exchanges_u_dir) then
                e = disp (2)
                al = aleng(2)
            endif
            if (iq > num_exchanges_u_dir + num_exchanges_v_dir) then
                e = disp (3)
                al = aleng(3)
            endif
            if (ilflag == 1) then
                dl = a / (aleng(2 * iq - 1) + aleng(2 * iq))
                f1 = aleng(2 * iq) * dl / a
                f2 = aleng(2 * iq - 1) * dl / a
            else
                dl = a / al
                f1 = 0.5
                f2 = 0.5
            endif
            e = e * dl
            if (i < 0) goto 20
            if (j < 0) goto 40

            ! the regular case
            k1 = (i - 1) * num_substances_total
            k2 = (j - 1) * num_substances_total
            do i3 = 1, num_substances_total
                is = min (i3, num_substances_transported)
                d = e
                v = q
                if (idpnt(is) > 0) d = d + disper((iq - 1) * num_dispersion_arrays + idpnt(is)) * dl
                if (ivpnt(is) > 0) then
                    dv = velo((iq - 1) * num_velocity_arrays + ivpnt(is)) * a
                    v = v + dv
                endif
                dq = (v * f1 + d) * conc(k1 + i3) + (v * f2 - d) * conc(k2 + i3)

                ! mass balance
                if (dq > 0.0) then
                    dmpq(ipq + i3) = dmpq(ipq + i3) + dq
                else
                    dmpq(ipq + i3 + i6) = dmpq(ipq + i3 + i6) - dq
                endif
                !
            end do
            goto 60

            ! The 'from' element was a boundary. Note the 2 options.
            20 if (j < 0) goto 60
            k1 = (-i - 1) * num_substances_total
            k2 = (j - 1) * num_substances_total
            do i3 = 1, num_substances_total
                is = min (i3, num_substances_transported)
                v = q
                if (ivpnt(is) > 0) v = v + velo  ((iq - 1) * num_velocity_arrays + ivpnt(is)) * a
                d = 0.0
                if (mod(integration_id, 4) <  2) then
                    d = e
                    if (idpnt(is)>0) d = d + disper((iq - 1) * num_dispersion_arrays + idpnt(is)) * dl
                endif
                if (mod(integration_id, 8) >=  4) then
                    if (v > 0.0) then
                        f1 = 1.0
                        f2 = 0.0
                    else
                        f1 = 0.0
                        f2 = 1.0
                    endif
                endif
                dq = (v * f1 + d) * bound(k1 + i3) + (v * f2 - d) * conc (k2 + i3)
                if (dq > 0.0) then
                    amass2(i3 + i4) = amass2(i3 + i4) + dq
                else
                    amass2(i3 + i5) = amass2(i3 + i5) - dq
                endif
                if (ipb > 0) then
                    if (dq > 0.0) then
                        dmpq(ipq + i3) = dmpq(ipq + i3) + dq
                    else
                        dmpq(ipq + i3 + i6) = dmpq(ipq + i3 + i6) - dq
                    endif
                endif
            end do
            goto 60

            ! The 'to' element was a boundary.
            40 if (i == 0) goto 60
            k1 = (i - 1) * num_substances_total
            k2 = (-j - 1) * num_substances_total
            do i3 = 1, num_substances_total
                is = min (i3, num_substances_transported)
                v = q
                if (ivpnt(is) > 0) v = v + velo  ((iq - 1) * num_velocity_arrays + ivpnt(is)) * a
                d = 0.0
                if (mod(integration_id, 4)  <  2) then
                    d = e
                    if (idpnt(is)>0) d = d + disper((iq - 1) * num_dispersion_arrays + idpnt(is)) * dl
                endif
                if (mod(integration_id, 8) >=  4) then
                    if (v > 0.0) then
                        f1 = 1.0
                        f2 = 0.0
                    else
                        f1 = 0.0
                        f2 = 1.0
                    endif
                endif
                dq = (v * f1 + d) * conc (k1 + i3) + (v * f2 - d) * bound(k2 + i3)
                if (dq > 0.0) then
                    amass2(i3 + i5) = amass2(i3 + i5) + dq
                else
                    amass2(i3 + i4) = amass2(i3 + i4) - dq
                endif
                if (ipb > 0) then
                    if (dq > 0.0) then
                        dmpq(ipq + i3) = dmpq(ipq + i3) + dq
                    else
                        dmpq(ipq + i3 + i6) = dmpq(ipq + i3 + i6) - dq
                    endif
                endif
            end do

            ! end of the loop over exchanges
            60 continue
        end do
        !
        if (timon) call timstop (ithandl)
        return
    end subroutine dlwq71
end module m_dlwq71
