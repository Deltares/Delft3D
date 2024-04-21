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
module m_dlwqm2
    use m_waq_precision

    implicit none

contains


    subroutine dlwqm2(idt, noseg, volnew, nobnd, noq, &
            ipoint, flowtot, disptot, theta, diag, &
            iscale, diagcc, nomat, mat, rowpnt, &
            fmat, tmat, iexseg)

        !     Deltares - Delft Software Department

        !     Created   :          2007 by Pauline van Slingerland

        !     Function  : fills the matrix

        !     Modified  : July     2009 by Leo Postma : double precission version
        !     Modified  : November 2009 by Leo Postma : imat and jmat introduced

        use timers
        implicit none

        !     Arguments           :

        !     Kind        Function         Name                  Description

        integer(kind = int_wp), intent(in) :: idt                   ! time step in scu's
        integer(kind = int_wp), intent(in) :: noseg                 ! number of segments
        real(kind = real_wp), intent(in) :: volnew(noseg)       ! segment volumes
        integer(kind = int_wp), intent(in) :: nobnd                 ! number of boundary segments
        integer(kind = int_wp), intent(in) :: noq                   ! number of exchanges
        integer(kind = int_wp), intent(in) :: ipoint(4, noq)       ! exchange pointers (dim: 4 x noq)
        real(kind = real_wp), intent(in) :: flowtot(noq)         ! flows plus additional velos. (dim: noq)
        real(kind = real_wp), intent(in) :: disptot(noq)         ! dispersion plus additional dipers. (dim: noq)
        real(kind = real_wp), intent(in) :: theta (noq)         ! variable theta coefficients
        real(kind = dp), intent(out) :: diag  (noseg + nobnd) ! (scaled) diagonal matrix elements
        integer(kind = int_wp), intent(in) :: iscale                ! 0: no diagonal scaling
        ! 1: diagonal scaling
        real(kind = dp), intent(out) :: diagcc(noseg + nobnd) ! copy of the unscaled diagonal, needed to scale the rhs later
        integer(kind = int_wp), intent(in) :: nomat                 ! number of nonzero offdiagonal matrix elements
        real(kind = dp), intent(out) :: mat   (nomat)       ! (scaled) nonzero offdiagonal matrix elements (elsewhere: amat)
        integer(kind = int_wp), intent(in) :: rowpnt(0:noseg + nobnd) ! row pointer, contains row lengths of mat (elsewhere: itrac)
        integer(kind = int_wp), intent(in) :: fmat  (noq)       ! pointer from(iq) in matrix
        integer(kind = int_wp), intent(in) :: tmat  (noq)       ! pointer to  (iq) in matrix
        integer(kind = int_wp), intent(in) :: iexseg(noseg + nobnd) ! zero if explicit

        !     Local declarations

        integer(kind = int_wp) :: iseg                  ! current volume
        integer(kind = int_wp) :: iq                    ! current edge
        integer(kind = int_wp) :: ito, ifrom            ! from and to volume indices
        real(kind = real_wp) :: q1, q2                ! flows

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqm2", ithandl)

        ! set the diagonal

        do iseg = 1, noseg
            diag(iseg) = volnew(iseg) / real(idt)
        enddo
        do iseg = noseg + 1, noseg + nobnd
            diag(iseg) = 1.0
        enddo

        ! reset the offdiagonal entries

        do iq = 1, nomat
            mat(iq) = 0.0
        enddo

        do iq = 1, noq
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or. ito == 0) cycle

            if (flowtot(iq) > 0.0) then
                q1 = (flowtot(iq) + disptot(iq)) * theta(iq)
                q2 = (0.0 - disptot(iq)) * theta(iq)
            else
                q1 = (0.0 + disptot(iq)) * theta(iq)
                q2 = (flowtot(iq) - disptot(iq)) * theta(iq)
            endif

            if (ifrom > 0) then
                diag(ifrom) = diag(ifrom) + q1
                mat (fmat(iq)) = mat (fmat(iq)) + q2
            endif
            if (ito   > 0) then
                diag(ito) = diag(ito) - q2
                mat (tmat(iq)) = mat (tmat(iq)) - q1
            endif
        enddo

        ! finally scale the matrix to avoid possible round-off errors in gmres
        ! this scaling may need some adaption for future domain decomposition b.c.

        if (iscale == 1) then
            do iseg = 1, noseg + nobnd
                ifrom = rowpnt(iseg - 1) + 1
                ito = rowpnt(iseg)

                ! check on zero's required for methods 17 and 18

                if (abs(diag(iseg)) < 1.0e-35) diag(iseg) = 1.0

                do iq = ifrom, ito
                    mat(iq) = mat(iq) / diag(iseg)
                enddo

                ! copy of diag for later scaling purposes in dlwqf4

                diagcc(iseg) = diag(iseg)
                diag  (iseg) = 1.0
            enddo
        else
            do iseg = 1, noseg + nobnd
                diagcc(iseg) = 1.0
            enddo
        endif

        if (timon) call timstop (ithandl)
        return
    end subroutine dlwqm2

end module m_dlwqm2
