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
module m_dlwqb3
    use m_waq_precision

    implicit none
    contains

    !> Makes new volumes for computed volumes
    subroutine dlwqb3(area, flow, velo, ipoint, notot, &
            noq, novelo, ivpnt, volume, integration_id, &
            amass2, idt, iaflag, nosys, dmpq, &
            ndmpq, iqdmp)

        use timers

        integer(kind = int_wp), intent(in   ) :: ndmpq          !< Number of dumped exchanges
        integer(kind = int_wp), intent(in   ) :: notot          !< Number  of total substances
        integer(kind = int_wp), intent(in   ) :: noq            !< Total number of exchanges
        integer(kind = int_wp), intent(in   ) :: novelo         !< Number  of additional velos.
        integer(kind = int_wp), intent(in   ) :: nosys          !< Number  of active substances
        integer(kind = int_wp), intent(in   ) :: integration_id !< = 0 or 2 DISP at zero flow
                                                                !! = 1 or 3 no DISP at zero flow
        integer(kind = int_wp), intent(in   ) :: idt            !< Integration time step size
        integer(kind = int_wp), intent(in   ) :: iaflag         !< If 1 then accumulate mass
        integer(kind = int_wp), intent(in   ) :: iqdmp(*)       !< Pointer dumped exchanges
        integer(kind = int_wp), intent(in   ) :: ivpnt(*)       !< Pointer systems to velocities
        integer(kind = int_wp), intent(in   ) :: ipoint(4,*)    !< Exchange pointers

        real(kind = real_wp),   intent(in   ) :: area(*)    !< Exchange surface area
        real(kind = real_wp),   intent(in   ) :: flow(*)    !< Flow accross exchange surface area
        real(kind = real_wp),   intent(in   ) :: velo(*)    !< Additional velocity array
        real(kind = real_wp),   intent(inout) :: volume(*)  !< Volumes to update
        real(kind = real_wp),   intent(inout) :: amass2(*)  !< Mass balance array
        real(kind = real_wp),   intent(inout) :: dmpq(*)    !< Mass balance dumped exchange

        ! Local variables
        logical    masbal
        integer(kind = int_wp) :: i, j, i4, i5, i6, iq, ipq
        real(kind = real_wp) :: b, q

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqb3", ithandl)

        ! loop accross the number of exchanges
        i4 = 3 * notot + 1
        i5 = 4 * notot + 1
        i6 = nosys * ndmpq
        b = 0.0
        if (iaflag == 1) b = 1.0 / idt
        masbal = .false.
        if (mod(integration_id, 16) >= 8) masbal = .true.
        do iq = 1, noq
            ! initialisations, check for transport anyhow
            i = ipoint(1, iq)
            j = ipoint(2, iq)
            if (i == 0 .or. j == 0) goto 60
            q = flow(iq) * idt
            if (ivpnt(1) > 0) &
                    q = q + velo((iq - 1) * novelo + ivpnt(1)) * area(iq) * idt
            ! accumulate balance for dumped exchanges
            if (masbal) then
                if (iqdmp(iq) > 0) then
                    ipq = (iqdmp(iq) - 1) * nosys + 1
                    if (q > 0.0) then
                        dmpq(ipq) = dmpq(ipq) + q
                    else
                        dmpq(ipq + i6) = dmpq(ipq + i6) - q
                    endif
                endif
            endif
            !
            if (i < 0) goto 20
            if (j < 0) goto 40

            ! the regular case
            volume(i) = volume(i) - q
            volume(j) = volume(j) + q
            goto 60

            ! The 'from' element was a boundary. Note the 2 options.
            20 if (j < 0) goto 60
            volume(j) = volume(j) + q
            if (q > 0.0) then
                amass2(i4) = amass2(i4) + q * b
            else
                amass2(i5) = amass2(i5) - q * b
            endif
            goto 60

            ! The 'to' element was a boundary.
            40 volume(i) = volume(i) - q
            if (q > 0.0) then
                amass2(i5) = amass2(i5) + q * b
            else
                amass2(i4) = amass2(i4) - q * b
            endif

            ! end of the loop over exchanges
            60 continue
        end do
        if (timon) call timstop (ithandl)
    end subroutine dlwqb3
end module m_dlwqb3
