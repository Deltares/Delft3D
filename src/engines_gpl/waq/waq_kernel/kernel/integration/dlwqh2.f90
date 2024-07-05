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
module m_dlwqh2
    use m_waq_precision

    implicit none

contains

    !> Fills off-diagonal values of the matrix for GMRES fast solver
    !! horizontally and vertically according to backward differences in space
    subroutine dlwqh2(noseg, nobnd, noq1, noq2, noq, &
                      ipoint, nodisp, novelo, idpnt, ivpnt, &
                      area, flow, disp, disper, velo, &
                      isys, nomat, amat, imat, idiag, &
                      diag, diagcc, iscale, fmat, tmat, &
                      mixlen, iknmrk)

        use timers
        implicit none

        integer(kind=int_wp), intent(in   ) :: noseg                  !< Number of computational volumes
        integer(kind=int_wp), intent(in   ) :: nobnd                  !< Number of open boundaries
        integer(kind=int_wp), intent(in   ) :: noq1                   !< Number of fluxes first direction
        integer(kind=int_wp), intent(in   ) :: noq2                   !< Number of fluxes second direction
        integer(kind=int_wp), intent(in   ) :: noq                    !< Total number fluxes in the water phase
        integer(kind=int_wp), intent(in   ) :: ipoint(4, noq)         !< From, to, from-1, to+1 volume numbers per flux
        integer(kind=int_wp), intent(in   ) :: nodisp                 !< Number of additional dispersion arrays
        integer(kind=int_wp), intent(in   ) :: novelo                 !< Number of additional velocity   arrays
        integer(kind=int_wp), intent(in   ) :: idpnt(*)               !< Dispersion array to be applied per substance
        integer(kind=int_wp), intent(in   ) :: ivpnt(*)               !< Velocity   array to be applied per substance
        real(kind=real_wp),   intent(in   ) :: area(noq)              !< Crosssectional surface areas of the fluxes
        real(kind=real_wp),   intent(in   ) :: flow(noq)              !< Fluxes
        real(kind=real_wp),   intent(in   ) :: disp(3)                !< Default dispersions in the 3 directions
        real(kind=real_wp),   intent(in   ) :: disper(nodisp, noq)    !< Additional dispersion arrays
        real(kind=real_wp),   intent(in   ) :: velo(novelo, noq)      !< Additional velocity arrays
        integer(kind=int_wp), intent(in   ) :: isys                   !< Substances number to be used for this matrix
        integer(kind=int_wp), intent(in   ) :: nomat                  !< Dimension of off-diagonal matrix amat
        real(kind=dp),        intent(  out) :: amat(nomat)            !< Matrix with off-diagonal entries
        integer(kind=int_wp), intent(in   ) :: imat(nomat)            !< Pointers of the off-diagonals in amat
        integer(kind=int_wp), intent(in   ) :: idiag(0:noseg + nobnd) !< Position of the diagonals in amat
        real(kind=dp),        intent(inout) :: diag(noseg + nobnd)    !< Diagonal of the matrix
        real(kind=dp),        intent(inout) :: diagcc(noseg + nobnd)  !< Copy of (unscaled) diagonal of the matrix
        integer(kind=int_wp), intent(in   ) :: iscale                 !< = 0 no row scaling of diagonal
                                                                      !< = 1    row scaling of diagonal
        integer(kind=int_wp), intent(in   ) :: fmat(noq)              !< Location from(iq) in matrix
        integer(kind=int_wp), intent(in   ) :: tmat(noq)              !< Location to  (iq) in matrix
        real(kind=real_wp),   intent(in   ) :: mixlen(noq)            !< Area/length for diffusion
        integer(kind=int_wp), intent(in   ) :: iknmrk(noseg)          !< Feature array, bit zero indicates wet or not

        ! Local variables
        logical lscale                !<  APPLY row scaling of the diagonal?
        integer(kind=int_wp) :: ifrom !<  From cell index
        integer(kind=int_wp) :: ito   !<  To cell index
        real(kind=real_wp)   :: a     !<  Auxiliary variable for exchange surface area in m2
        real(kind=real_wp)   :: q     !<  Auxiliary variable for the flux in m3/s
        real(kind=real_wp)   :: e     !<  Auxiliary variable for diffusive flux in m3/s
        integer(kind=int_wp) :: idp   !<  Auxiliary variable for idpnt(isys)
        integer(kind=int_wp) :: ivp   !<  Auxiliary variable for ivpnt(isys)
        real(kind=real_wp)   :: q1    !<  Auxiliary variable
        real(kind=real_wp)   :: q2    !<  Auxiliary variable
        integer(kind=int_wp) :: iq    !<  Loop counter
        integer(kind=int_wp) :: jq    !<  Loop counter

        integer(kind=int_wp) :: ithandl = 0
        if (timon) call timstrt("dlwqh2", ithandl)

        ! set the logicals for dispersion and scaling and other fixed items
        lscale = iscale == 1
        idp = idpnt(isys)
        ivp = ivpnt(isys)

        ! reset the entire matrix
        amat = 0.0d0

        do iq = 1, noq
            ! pointer administration check for transport anyhow
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)

            if (ifrom == 0 .or. ito == 0) cycle
            if (ifrom > 0) then
                if (.not. btest(iknmrk(ifrom), 0)) cycle   ! identified dry at start and end of timestep
            end if
            if (ito > 0) then
                if (.not. btest(iknmrk(ito), 0)) cycle
            end if

            ! initialisations
            a = area(iq)
            q = flow(iq)
            if (a < 1.0e-25) a = 1.0
            if (iq <= noq1) then
                e = disp(1)
            else if (iq <= noq1 + noq2) then
                e = disp(2)
            else
                e = disp(3)
            end if
            e = e*mixlen(iq)

            ! add additional dispersions and fluxes
            if (idp > 0) e = e + disper(idp, iq)*mixlen(iq)
            if (ivp > 0) q = q + velo(ivp, iq)*a

            ! the backward differencing in space
            if (q > 0.0) then
                q1 = q
                q2 = 0.0
            else
                q1 = 0.0
                q2 = q
            end if

            ! fill the matrix
            if (ifrom > 0) then
                diag(ifrom) = diag(ifrom) + q1 + e
                amat(fmat(iq)) = amat(fmat(iq)) + q2 - e
            end if
            if (ito > 0) then
                diag(ito) = diag(ito) - q2 + e
                amat(tmat(iq)) = amat(tmat(iq)) - q1 - e
            end if

            ! end of the loop over exchanges

        end do

        ! finally scale the matrix to avoid possible round-off errors in GMRES
        ! this scaling may need some adaption for future domain decomposition b.c.
        if (lscale) then
            do iq = 1, noseg + nobnd
                ifrom = idiag(iq - 1) + 1
                ito = idiag(iq)

                ! check on zero's required for methods 17 and 18
                if (abs(diag(iq)) < 1.0d-100) diag(iq) = 1.0

                do jq = ifrom, ito
                    amat(jq) = amat(jq)/diag(iq)
                end do

                ! copy of diag for later scaling purposes in DLWQF4
                diagcc(iq) = diag(iq)
                diag(iq) = 1.0d00
            end do
        else
            do iq = 1, noseg + nobnd
                diagcc(iq) = 1.0d00
            end do
        end if

        if (timon) call timstop(ithandl)
    end subroutine dlwqh2
end module m_dlwqh2
