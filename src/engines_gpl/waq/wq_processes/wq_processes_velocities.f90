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
module m_wq_processes_velocities
    use m_waq_precision

    implicit none

contains


    subroutine wq_processes_velocities (velonw, nveln, ivpnew, velx, nvelx, vsto, nosys, noq)
        !
        !     function            : makes velonw array from velo and velx array
        !

        use timers

        implicit none

        ! declaration of arguments

        integer(kind = int_wp), intent(in) :: nveln                            ! number of new velocities
        integer(kind = int_wp), intent(in) :: nvelx                            ! number of velocities from processes
        integer(kind = int_wp), intent(in) :: nosys                            ! number of active substances
        integer(kind = int_wp), intent(in) :: noq                              ! number of exchanges
        real(kind = real_wp), intent(inout) :: velonw(nveln, noq)                ! new velocity array
        integer(kind = int_wp), intent(in) :: ivpnew(nosys)                    ! pointer to new velo array (actually only input)
        real(kind = real_wp), intent(in) :: velx(nvelx, noq)                  ! velocities from processes
        real(kind = real_wp), intent(in) :: vsto(nosys, nvelx)                ! factor for velocities

        ! local declarations

        integer(kind = int_wp) :: isys                             ! index substances
        integer(kind = int_wp) :: isys2                            ! index substances
        integer(kind = int_wp) :: ivnw                             ! index new velocities
        integer(kind = int_wp) :: ivx                              ! index velocities from process
        integer(kind = int_wp) :: ivp                              ! index velocities from input
        integer(kind = int_wp) :: iq                               ! index exchange
        integer(kind = int_wp) :: ivpnew_loc(nosys)                ! local copy of ivpnew
        logical :: lfirst                          ! first velocity in combination of velocities
        logical :: update                          ! update of the combined velocity needed
        real(kind = real_wp) :: factor                           ! factor for susbtance velocity combination

        integer(kind = int_wp), save :: ithndl = 0
        if (timon) call timstrt("wq_processes_velocities", ithndl)

        ! local copy of ivpnew

        ivpnew_loc = ivpnew

        ! construct velonw

        do isys = 1, nosys
            do ivnw = 1, nveln
                if (ivpnew_loc(isys) == ivnw) then
                    lfirst = .true.
                    ! add the contribution of the calculated velocities.
                    do ivx = 1, nvelx
                        factor = vsto(isys, ivx)
                        if (abs(factor) > 1.e-20) then
                            if (lfirst) then
                                lfirst = .false.
                                if (abs(factor - 1.0) < 1.e-10) then
                                    do iq = 1, noq
                                        velonw(ivnw, iq) = velx(ivx, iq)
                                    enddo
                                else
                                    do iq = 1, noq
                                        velonw(ivnw, iq) = factor * velx(ivx, iq)
                                    enddo
                                endif
                            else
                                if (abs(factor - 1.0) < 1.e-10) then
                                    do iq = 1, noq
                                        velonw(ivnw, iq) = velonw(ivnw, iq) + velx(ivx, iq)
                                    enddo
                                else
                                    do iq = 1, noq
                                        velonw(ivnw, iq) = velonw(ivnw, iq) + factor * velx(ivx, iq)
                                    enddo
                                endif
                            endif
                        endif
                    enddo
                    ! trick the other substances also pointing to this array by setting pointer negative
                    do isys2 = isys + 1, nosys
                        if (ivpnew_loc(isys2) == ivnw) then
                            ivpnew_loc(isys2) = -ivpnew_loc(isys2)
                        endif
                    enddo
                    ! there can be no other new velocity for this substance so exit nveln loop
                    exit
                endif
            enddo
        enddo
        if (timon) call timstop(ithndl)
        return
    end

end module m_wq_processes_velocities
