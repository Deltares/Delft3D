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
module m_dlwqf3
    use m_waq_precision

    implicit none

contains


    subroutine dlwqf3 (idt, noseg, volnew, nobnd, noq, &
            &                    ipoint, flowtot, disptot, diag, iscale, &
            &                    diagcc, nomat, amat, idiag, fmat, &
            &                    tmat)

        !     Deltares - Delft Software Department

        !>/file
        !>                fills matrix to be used by the GMRES fast solver, once per substance
        !>
        !>                Matrix is filled:
        !>                - horizontally according to upwind differences in space
        !>                - vertically   according to upwind differences in space
        !>                It is assumed that any logic on drying and flooding is in the
        !>                precomputed flowtot and disptot arrays./n
        !>                The routine is very efficient because of the precomputed fmat
        !>                and tmat arrays for the from and to locations in the matrix.

        !     Created   : Sept.1996 by Leo Postma

        !     Modified  : Feb.     1997, Robert Vos  : Check on zero's in the scaling
        !                 July     2008, Leo Postma  : WAQ perfomance timers
        !                 July     2009, Leo Postma  : double precission version
        !                 November 2009, Leo Postma  : streamlined for parallel computing

        use timers                         ! WAQ performance timers
        implicit none

        !     Arguments           :

        !     Kind        Function         Name                  Description

        integer(kind = int_wp), intent(in) :: idt                  !< time step in scu's
        integer(kind = int_wp), intent(in) :: noseg                !< Number of computational volumes
        real(kind = real_wp), intent(in) :: volnew(noseg)      !< segment volumes
        integer(kind = int_wp), intent(in) :: nobnd                !< Number of open boundaries
        integer(kind = int_wp), intent(in) :: noq                  !< Total number fluxes in the water phase
        integer(kind = int_wp), intent(in) :: ipoint(4, noq)        !< from, to, from-1, to+1 volume numbers per flux
        real(kind = real_wp), intent(in) :: flowtot(noq)        !< flows plus additional velos. (dim: noq)
        real(kind = real_wp), intent(in) :: disptot(noq)        !< dispersion plus additional dipers. (dim: noq)
        real(kind = dp), intent(inout) :: diag  (noseg + nobnd)  !< diagonal of the matrix
        integer(kind = int_wp), intent(in) :: iscale               !< = 1 row scaling with the diagonal
        real(kind = dp), intent(inout) :: diagcc(noseg + nobnd)  !< copy of (unscaled) diagonal of the matrix
        integer(kind = int_wp), intent(in) :: nomat                !< dimension of off-diagonal matrix amat
        real(kind = dp), intent(out) :: amat  (nomat)        !< matrix with off-diagonal entries
        integer(kind = int_wp), intent(in) :: idiag(0:noseg + nobnd) !< position of the diagonals in amat
        integer(kind = int_wp), intent(in) :: fmat  (noq)        !< location from(iq) in matrix
        integer(kind = int_wp), intent(in) :: tmat  (noq)        !< location to  (iq) in matrix

        !     Local declarations

        integer(kind = int_wp) :: iseg           ! current volume
        integer(kind = int_wp) :: iq, jq        ! current edge
        integer(kind = int_wp) :: ito, ifrom     ! to and from volume number
        real(kind = real_wp) :: q1, q2        ! flows
        real(kind = dp) :: dt             ! time step in double

        !     WAQ timers

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqf3", ithandl)

        ! set the diagonal

        dt = idt
        do iseg = 1, noseg
            diag(iseg) = volnew(iseg) / dt
        enddo
        do iseg = noseg + 1, noseg + nobnd
            diag(iseg) = 1.0
        enddo

        !        reset the entire matrix

        amat = 0.0D0

        do iq = 1, noq
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or. ito == 0) cycle

            if (flowtot(iq) > 0.0) then
                q1 = flowtot(iq)
                q2 = 0.0
            else
                q1 = 0.0
                q2 = flowtot(iq)
            endif

            if (ifrom > 0) then
                diag (ifrom) = diag (ifrom) + q1 + disptot(iq)
                amat (fmat(iq)) = amat (fmat(iq)) + q2 - disptot(iq)
            endif
            if (ito   > 0) then
                diag (ito) = diag (ito) - q2 + disptot(iq)
                amat (tmat(iq)) = amat (tmat(iq)) - q1 - disptot(iq)
            endif
        enddo

        !     finally scale the matrix to avoid possible round-off errors in GMRES
        !     this scaling may need some adaption for future domain decomposition b.c.

        if (iscale == 1) then
            do iq = 1, noseg + nobnd
                ifrom = idiag(iq - 1) + 1
                ito = idiag(iq)

                !      check on zero's required for methods 17 and 18

                if (abs(diag(iq)) < 1.0d-100) diag(iq) = 1.0

                do jq = ifrom, ito
                    amat(jq) = amat(jq) / diag(iq)
                enddo

                !           copy of diag for later scaling purposes in DLWQF4

                diagcc(iq) = diag (iq)
                diag  (iq) = 1.0d00
            enddo
        else
            do iq = 1, noseg + nobnd
                diagcc(iq) = 1.0d00
            enddo
        endif

        if (timon) call timstop (ithandl)

        return
    end

end module m_dlwqf3
