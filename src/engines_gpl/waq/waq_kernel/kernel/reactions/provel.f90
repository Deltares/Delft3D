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
module m_provel
    use m_waq_precision

    implicit none

contains


    subroutine provel (velonw, num_velocity_arrays_new, ivpnew, velo, num_velocity_arrays, &
            ivpnt, velx, num_velocity_arrays_extra, vsto, num_substances_transported, &
            num_exchanges, velndt, istep)
        !
        !     function            : makes velonw array from velo and velx array
        !
        !     created:            : december 1994 by jan van beek
        !
        !     modified            : october  2010, jvb,  only update new velocities if needed

        use timers
        implicit none

        ! declaration of arguments

        integer(kind = int_wp), intent(in) :: num_velocity_arrays_new
        integer(kind = int_wp), intent(in) :: num_velocity_arrays               ! number of velocities from input
        integer(kind = int_wp), intent(in) :: num_velocity_arrays_extra         ! number of velocities from processes
        integer(kind = int_wp), intent(in) :: num_substances_transported
        integer(kind = int_wp), intent(in) :: num_exchanges
        real(kind = real_wp), intent(inout) :: velonw(num_velocity_arrays_new, num_exchanges)               ! new velocity array
        integer(kind = int_wp), intent(in) :: ivpnew(num_substances_transported)                   ! pointer to new velo array (actually only input)
        real(kind = real_wp), intent(in) :: velo(num_velocity_arrays, num_exchanges)                ! velocities from input
        integer(kind = int_wp), intent(in) :: ivpnt(num_substances_transported)                    ! pointer to original velo
        real(kind = real_wp), intent(in) :: velx(num_velocity_arrays_extra, num_exchanges)                 ! velocities from processes
        real(kind = real_wp), intent(in) :: vsto(num_substances_transported, num_velocity_arrays_extra)               ! factor for velocities
        integer(kind = int_wp), intent(in) :: velndt(num_velocity_arrays_extra)                   ! time step size of the velocities
        integer(kind = int_wp), intent(in) :: istep                           ! time step nr.

        ! local declarations

        integer(kind = int_wp) :: isys                            ! index substances
        integer(kind = int_wp) :: isys2                           ! index substances
        integer(kind = int_wp) :: ivnw                            ! index new velocities
        integer(kind = int_wp) :: ivx                             ! index velocities from process
        integer(kind = int_wp) :: ivp                             ! index velocities from input
        integer(kind = int_wp) :: iq                              ! index exchange
        integer(kind = int_wp) :: ivpnew_loc(num_substances_transported)               ! local copy of ivpnew
        logical :: lfirst                          ! first velocity in combination of velocities
        logical :: update                          ! update of the combined velocity needed
        real(kind = real_wp) :: factor                          ! factor for susbtance velocity combination
        integer(kind = int_wp), save :: ithandl = 0                     ! handle in timer routines

        ! activate time routines

        if (timon) call timstrt ("provel", ithandl)

        ! local copy of ivpnew

        ivpnew_loc = ivpnew

        ! we construeren nu de velonw

        do isys = 1, num_substances_transported

            do ivnw = 1, num_velocity_arrays_new

                if (ivpnew_loc(isys) == ivnw) then

                    ! check if update is needed, always from input, fractional step from processes

                    update = .false.
                    if (ivpnt(isys) /= 0) update = .true.
                    do ivx = 1, num_velocity_arrays_extra
                        factor = vsto(isys, ivx)
                        if (abs(factor) > 1.e-20) then
                            if (mod(istep - 1, velndt(ivx)) == 0) update = .true.
                        endif
                    enddo

                    if (update) then

                        ! look in original velo

                        lfirst = .true.
                        if (ivpnt(isys) /= 0) then
                            lfirst = .false.
                            ivp = ivpnt(isys)
                            do iq = 1, num_exchanges
                                velonw(ivnw, iq) = velo(ivp, iq)
                            enddo
                        endif

                        ! add the contribution of the calculated velocities.

                        do ivx = 1, num_velocity_arrays_extra
                            factor = vsto(isys, ivx)
                            if (abs(factor) > 1.e-20) then
                                if (lfirst) then
                                    lfirst = .false.
                                    if (abs(factor - 1.0) < 1.e-10) then
                                        do iq = 1, num_exchanges
                                            velonw(ivnw, iq) = velx(ivx, iq)
                                        enddo
                                    else
                                        do iq = 1, num_exchanges
                                            velonw(ivnw, iq) = factor * velx(ivx, iq)
                                        enddo
                                    endif
                                else
                                    if (abs(factor - 1.0) < 1.e-10) then
                                        do iq = 1, num_exchanges
                                            velonw(ivnw, iq) = velonw(ivnw, iq) + &
                                                    velx(ivx, iq)
                                        enddo
                                    else
                                        do iq = 1, num_exchanges
                                            velonw(ivnw, iq) = velonw(ivnw, iq) + &
                                                    factor * velx(ivx, iq)
                                        enddo
                                    endif
                                endif
                            endif
                        enddo

                    endif

                    ! trick the other substances also pointing to this array by setting pointer negative

                    do isys2 = isys + 1, num_substances_transported
                        if (ivpnew_loc(isys2) == ivnw) then
                            ivpnew_loc(isys2) = -ivpnew_loc(isys2)
                        endif
                    enddo

                    ! there can be no other new velocity for this substance so exit num_velocity_arrays_new loop

                    exit

                endif

            enddo

        enddo

        if (timon) call timstop (ithandl)

        return
    end

end module m_provel
