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
module m_dlwqm0
    use m_waq_precision

    implicit none

contains


    subroutine dlwqm0 (isys, nosys, noq, noq1, noq2, &
            area, flow, flowtot, novelo, ivpnt, &
            velo, disp, disptot, nodisp, idpnt, &
            disper, mixlen)

        !     Deltares Software Centre

        !>/File
        !>               makes 2 arrays with substance dependent flow and diffusion

        !     Created   :          2007 by Pauline van Slingerland

        !     Modified  : November 2009 by Leo Postma : dlwqm7 was split off for all substances

        !     Note      : This routine takes 3 times as much computation time than the more complicated
        !                 dlwqm1 routine. The only reason for that is that the 2 dimensional indices of
        !                 the velo and disper arrays should have noq as first dimension rather than second.
        !                 For the coarse Hong Kong situation with 37500 volumes and 20 substances, the full
        !                 span of 20*37500*4 = 3 Mb of the disper array schould be read and approximately
        !                 half of it for the velo array. This is done 20 times per time step, so 90 Mb per
        !                 time step. With a 1066 MHz front side bus and the DDR3 memory of the Intel T9400
        !                 processor of my portable this takes 60/2 = 30 ms per time step or 7 seconds for
        !                 240 time steps for the test computation. If the 2 indexes are interchanged, only
        !                 4.5 Mb should be transported, which costs only 360 ms for the test.

        use timers
        implicit none

        integer(kind = int_wp), intent(in) :: isys                !< current active substance
        integer(kind = int_wp), intent(in) :: nosys               !< number of active substances
        integer(kind = int_wp), intent(in) :: noq                 !< number of exchanges
        integer(kind = int_wp), intent(in) :: noq1                !< number of exchanges in first direction
        integer(kind = int_wp), intent(in) :: noq2                !< number of exchanges in second direction
        real(kind = real_wp), intent(in) :: area   (noq)        !< exchange surface areas (dim: noq)
        real(kind = real_wp), intent(in) :: flow   (noq)        !< flows accross exchange surfs (dim: noq)
        real(kind = real_wp), intent(out) :: flowtot(noq)        !< flows plus additional velos. (dim: noq)
        integer(kind = int_wp), intent(in) :: novelo              !< number  of additional velos.
        integer(kind = int_wp), intent(in) :: ivpnt  (nosys)      !< pointer systems to velocities (dim: nosys)
        real(kind = real_wp), intent(in) :: velo   (novelo, noq) !< additional velocity array (dim: novelo*noq)
        real(kind = real_wp), intent(in) :: disp   (3)        !< dispersion in 3 directions
        real(kind = real_wp), intent(out) :: disptot(noq)        !< dispersion plus additional dipers. (dim: noq)
        integer(kind = int_wp), intent(in) :: nodisp              !< number  of additional dispers.
        integer(kind = int_wp), intent(in) :: idpnt  (nosys)      !< pointer systems to dispersions (dim: nosys)
        real(kind = real_wp), intent(in) :: disper (nodisp, noq) !< additional dispersion array (dim: nodisp*noq)
        real(kind = real_wp), intent(in) :: mixlen (noq)        !< area / length

        !     Local declarations

        integer(kind = int_wp) :: iq                  ! current edge
        integer(kind = int_wp) :: iv, id             ! additional volume and dispersion pointer

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqm0", ithandl)

        iv = ivpnt(isys)
        id = idpnt(isys)

        if (iv == 0) then
            flowtot = flow
        else
            do iq = 1, noq
                flowtot(iq) = flow(iq) + velo(iv, iq) * area(iq)
            enddo
        endif

        if (id == 0) then
            do iq = 1, noq1
                disptot(iq) = disp(1) * mixlen(iq)
            enddo
            do iq = noq1 + 1, noq1 + noq2
                disptot(iq) = disp(2) * mixlen(iq)
            enddo
            do iq = noq1 + noq2 + 1, noq
                disptot(iq) = disp(3) * mixlen(iq)
            enddo
        else
            do iq = 1, noq1
                disptot(iq) = (disp(1) + disper(id, iq)) * mixlen(iq)
            enddo
            do iq = noq1 + 1, noq1 + noq2
                disptot(iq) = (disp(2) + disper(id, iq)) * mixlen(iq)
            enddo
            do iq = noq1 + noq2 + 1, noq
                disptot(iq) = (disp(3) + disper(id, iq)) * mixlen(iq)
            enddo
        endif

        if (timon) call timstop (ithandl)
        return
    end

end module m_dlwqm0
