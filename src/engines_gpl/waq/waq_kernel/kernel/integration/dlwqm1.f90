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
module m_dlwqm1
    use m_waq_precision

    implicit none

contains


    subroutine dlwqm1(idt, noseg, nobnd, volold, noq, &
            noq1, noq2, ipoint, flowtot, disptot, &
            theta, thetaseg, antidiffusion, iexseg)

        !     Deltares - Delft Software Department

        !     Created   :          2007 by Pauline van Slingerland

        !     Function  : computes the theta's

        !     Modified  : November 2009 by Leo Postma : limits the theta to horizontals only.
        !                                               vertical demands almost always teta = 1.0

        use timers
        implicit none

        integer(kind = int_wp), intent(in) :: idt             ! time step in scu's
        integer(kind = int_wp), intent(in) :: noseg           ! number of segments
        integer(kind = int_wp), intent(in) :: nobnd           ! number of boundaries
        real(kind = real_wp), intent(in) :: volold(noseg)   ! volumes at beginning of step (dim: noseg)
        integer(kind = int_wp), intent(in) :: noq             ! total number of exchanges
        integer(kind = int_wp), intent(in) :: noq1            ! number of exchanges in the first direction
        integer(kind = int_wp), intent(in) :: noq2            ! number of exchanges in the second direction
        integer(kind = int_wp), intent(in) :: ipoint  (4, noq) ! exchange pointers (dim: 4 x noq)
        real(kind = real_wp), intent(in) :: flowtot (noq) ! total flows accross exchange surfs (dim: noq)
        real(kind = real_wp), intent(in) :: disptot (noq) ! total flows accross exchange surfs (dim: noq)
        real(kind = real_wp), intent(out) :: theta   (noq) ! variable theta coefficients (dim: noq)
        real(kind = real_wp), intent(out) :: thetaseg(noseg) ! variable theta coefficients per segment
        logical(4), intent(in) :: antidiffusion   ! if true: replace diffusion error by antidiffusion error
        integer(kind = int_wp), intent(out) :: iexseg  (noseg + nobnd) ! 0 if volume is explicit

        !     Local declarations

        integer(kind = int_wp) :: i, j            ! from- and to volumes
        integer(kind = int_wp) :: iq              ! current edge
        integer(kind = int_wp) :: iseg            ! current volumes
        integer(kind = int_wp) :: iexp            ! explicit fraction of the problem

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqm1", ithandl)

        !         initialisation

        do iseg = 1, noseg
            thetaseg(iseg) = 0.0
        enddo
        do iq = 1, noq1 + noq2
            theta   (iq) = 0.0
        enddo
        do iq = noq1 + noq2 + 1, noq
            theta   (iq) = 1.0
        enddo

        !         store the sum of outflows per volume in thetaseg
        !             horizontal only, vertical will be delt with implicitly

        do iq = 1, noq1 + noq2
            i = ipoint(1, iq)
            j = ipoint(2, iq)
            if (i > 0) thetaseg(i) = thetaseg(i) + max(0.0, flowtot(iq)) + disptot(iq)
            if (j > 0) thetaseg(j) = thetaseg(j) + max(0.0, -flowtot(iq)) + disptot(iq)
        enddo

        !         store local theta coefficients per volume in thetaseg

        do iseg = 1, noseg
            if (thetaseg(iseg) > 0) thetaseg(iseg) = max(0.0, 1.0 - volold(iseg) / (real(idt) * thetaseg(iseg)))
        enddo

        !         store local theta coefficients per edge in theta

        do iq = 1, noq1 + noq2
            i = ipoint(1, iq)
            j = ipoint(2, iq)
            if (i > 0 .and. j > 0) then
                theta(iq) = max(thetaseg(i), thetaseg(j))
            endif
            if (i > 0 .and. j < 0) theta(iq) = thetaseg(i) ! j is a boundary segment
            if (i < 0 .and. j > 0) theta(iq) = thetaseg(j) ! i is a boundary segment
        enddo

        ! replace antidiffusion error by diffusion error

        if (.not. antidiffusion) then ! implicit coefficients minimal 0.5 (default setting)
            do iq = 1, noq1 + noq2
                if (theta(iq) > 0) theta(iq) = max(0.5, theta(iq))
            enddo
        endif

        !         search for explicit cells

        do iq = 1, noq1 + noq2
            i = ipoint(1, iq)
            j = ipoint(2, iq)
            if (i > 0) thetaseg(i) = max(thetaseg(i), theta(iq))
            if (j > 0) thetaseg(j) = max(thetaseg(j), theta(iq))
        enddo
        iexp = 0
        iexseg = 0
        do iseg = 1, noseg
            if (thetaseg(iseg) < 1.0e-25) then
                iexp = iexp + 1
            else
                iexseg(iseg) = 1
            endif
        enddo

        if (timon) call timstop (ithandl)
        return
    end subroutine dlwqm1

end module m_dlwqm1
