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
module m_dlwqf7
    use m_waq_precision

    implicit none

contains

    !> Calculates mass balance of transport and copies solution in the concentration array
    !! Iterative solver solves 1 substance at a time (per thread in parallel mode).
    !! This routine saves the double precision solution in the single precision cons array.
    !! The tricky part is that for dry cells 0.0 is taken. In previous versions the old
    !! value remained untouched or if influenced by the iteration it got a strange value.
    !! Furthermore with the concentration values the mass balance aarays are updated.
    !! The update makes use of the pre-computed flowtot and disptot arrays for this substance.
    !! They contain the flow and disp plus additional velocity and dispersion terms.
    !! This avoids to have that logics again in this routine.
    subroutine dlwqf7(isys, nosys, notot, noseg, conc, &
                      concvt, nobnd, bound, noq, ipoint, &
                      flowtot, disptot, amass2, ndmpq, iqdmp, &
                      dmpq, iknmrk, idt)


        use timers

        implicit none

        integer(kind=int_wp), intent(in   ) :: isys                  !< current transported substance
        integer(kind=int_wp), intent(in   ) :: nosys                 !< number of active substances
        integer(kind=int_wp), intent(in   ) :: notot                 !< total number of substances
        integer(kind=int_wp), intent(in   ) :: noseg                 !< number of cells or computational volumes
        real(kind=real_wp),   intent(inout) :: conc(notot, noseg)    !< concentration vector to store results
        real(kind=dp),        intent(in   ) :: concvt(noseg)         !< newly obtained concentration from the solver
        integer(kind=int_wp), intent(in   ) :: nobnd                 !< number of volumes with open boundaries
        real(kind=real_wp),   intent(in   ) :: bound(nosys, nobnd)   !< boundary concentrations
        integer(kind=int_wp), intent(in   ) :: noq                   !< number of exchanges between volumes
        integer(kind=int_wp), intent(in   ) :: ipoint(4, noq)        !< exchange pointers
        real(kind=real_wp),   intent(in   ) :: flowtot(noq)          !< flows plus additional velos.
        real(kind=real_wp),   intent(in   ) :: disptot(noq)          !< dispersion plus additional dipers.
        real(kind=real_wp),   intent(inout) :: amass2(notot, 5)      !< mass balance array for the whole model area
        integer(kind=int_wp), intent(in   ) :: ndmpq                 !< number of dumped exchanges
        integer(kind=int_wp), intent(in   ) :: iqdmp(noq)            !< pointers dumped exchages
        real(kind=real_wp),   intent(inout) :: dmpq(nosys, ndmpq, 2) !< flux accumulation array for monitoring areas
        integer(kind=int_wp), intent(in   ) :: iknmrk(noseg)         !< feature array, bit zero indicates wet or not
        integer(kind=int_wp), intent(in   ) :: idt                   !< time step

        ! Local variables
        real(kind=real_wp) :: cin, cjn     !< from- and to concentrations
        real(kind=real_wp) :: fluxij       !< flux from i to j
        integer(kind=int_wp) :: ifrom, ito !< from- and to volume indices
        integer(kind=int_wp) :: iseg       !< current computational volume
        integer(kind=int_wp) :: iq         !< current edge

        integer(kind=int_wp) :: ithandl = 0

        if (timon) call timstrt("dlwqf7", ithandl)
        ! put result in the concentration array
        do iseg = 1, noseg
            if (btest(iknmrk(iseg), 0)) then
                conc(isys, iseg) = concvt(iseg)
            else
                conc(isys, iseg) = 0.0
            end if
        end do

        ! flow and diffusion
        do iq = 1, noq
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            ! only compute where needed
            if (ifrom > 0 .and. ito > 0 .and. iqdmp(iq) == 0) cycle
            if (ifrom == 0 .or. ito == 0) cycle

            if (ifrom > 0) then
                cin = concvt(ifrom)
            else
                cin = bound(isys, -ifrom)
            end if

            if (ito > 0) then
                cjn = concvt(ito)
            else
                cjn = bound(isys, -ito)
            end if

            if (flowtot(iq) > 0) then ! flow from i to j
                fluxij = real(idt)*((flowtot(iq) + disptot(iq))*cin - disptot(iq)*cjn)
            else ! flow from j to i
                fluxij = real(idt)*((flowtot(iq) - disptot(iq))*cjn + disptot(iq)*cin)
            end if

            ! mass balance of the whole area
            if (ifrom < 0) then
                if (fluxij > 0) then
                    amass2(isys, 4) = amass2(isys, 4) + fluxij
                else                                             ! amass2(*,1) masses of the substances in the model
                    amass2(isys, 5) = amass2(isys, 5) - fluxij      ! amass2(*,2) change by processes
                end if                                            ! amass2(*,3) change by discharges
            end if                                               ! amass2(*,4) incoming boundary transport
            if (ito < 0) then                            ! amass2(*,5) outgoing boundary transport
                if (fluxij > 0) then
                    amass2(isys, 5) = amass2(isys, 5) + fluxij
                else
                    amass2(isys, 4) = amass2(isys, 4) - fluxij
                end if
            end if

            ! mass balance of selected monitoring areas
            if (iqdmp(iq) > 0) then ! dmpq(*,*,1) incoming transport in a monitoring area
                if (fluxij > 0) then ! dmpq(*,*,2) outgoing transport from a monitoring area
                    dmpq(isys, iqdmp(iq), 1) = dmpq(isys, iqdmp(iq), 1) + fluxij
                else
                    dmpq(isys, iqdmp(iq), 2) = dmpq(isys, iqdmp(iq), 2) - fluxij
                end if
            end if
        end do
        if (timon) call timstop(ithandl)
    end subroutine dlwqf7
end module m_dlwqf7
