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
module m_dlwqf8
    use m_waq_precision

    implicit none

contains

    !> Computes new volumes from the old volumes and the flows
    !! This routine computes the new volumes rather than reading them
    !! from file. Together with the closure error correction routine this
    !! gives the posibility to deal with non-mass conserving hydrodynamics.
    !! This is something you should not want. It violates the mass balance
    !! of the substances in the same way as the water balance is violated.
    !! The end result is that you are never certain of anything in your
    !! water quality model. Decent modellers should not use this option.
    !! Bert Jagers was dealing with the Selfe model that did not conserve.
    !! Guus Stelling is lobbying for non-conservative schemes.
    subroutine dlwqf8(noseg, noq, ipnt, idt, iknmkv, &
                        volume, flow, voll, vol2)
        use timers
        implicit none

        integer(kind = int_wp), intent(in   ) :: noseg          !< Number of computational volumes
        integer(kind = int_wp), intent(in   ) :: noq            !< Number of exchanges
        integer(kind = int_wp), intent(in   ) :: ipnt  (4, noq) !< From-to pointer table
        integer(kind = int_wp), intent(in   ) :: idt            !< Time step size
        integer(kind = int_wp), intent(in   ) :: iknmkv(noseg)  !< Dry indicator 1 is wet
        real(kind = real_wp),   intent(in   ) :: volume(noseg)  !< Volume at start of time step
        real(kind = real_wp),   intent(in   ) :: flow  (noq)    !< Flows accross the noq links
        real(kind = real_wp),   intent(in   ) :: voll  (noseg)  !< New volume from file
        real(kind = real_wp),   intent(  out) :: vol2  (noseg)  !< Volume at end of time step

        ! Local variables
        integer(kind = int_wp) :: iq       !< Loop counter for the flows
        integer(kind = int_wp) :: ifrom    !< Cell number where the flux leaves
        integer(kind = int_wp) :: ito      !< Cell number where the flux arrives
        real(kind = real_wp)   :: flux     !< Flux to be applied

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqf8", ithandl)

        vol2 = volume               ! Initialize the new volume
        do iq = 1, noq              ! Loop over the flows
            ifrom = ipnt(1, iq)
            ito = ipnt(2, iq)
            flux = flow(iq) * idt
            if (ifrom > 0) vol2(ifrom) = vol2(ifrom) - flux
            if (ito   > 0) vol2(ito) = vol2(ito) + flux
        enddo
        do iq = 1, noseg
            if (iknmkv(iq) == 0) vol2(iq) = voll(iq)
            if (abs(vol2(iq)) < 1.0E-25) vol2(iq) = 1.0
        enddo

        if (timon) call timstop (ithandl)
    end subroutine dlwqf8
end module m_dlwqf8
