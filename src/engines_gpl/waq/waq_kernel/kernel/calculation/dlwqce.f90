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
module m_dlwqce
    use m_waq_precision

    implicit none

contains


    subroutine dlwqce (amass, volumn, voluml, nosys, notot, &
            noseg, lun)

        !     Deltares Software Centre

        !>/file
        !>              Makes closure error correction on masses
        !>
        !>         The volume after rewind of hydrodynamics (volumn) generally
        !>         does generally not correspond with the volume that is obtained
        !>         with mass conserving transport in the last time step of the
        !>         hydrodynamic file (voluml). This will give a jump in the the
        !>         time series of concentrations after rewind since DELWAQ preserves
        !>         mass. To avoid this jump, the mass can be adjusted according to
        !>         the volume error made with the rewind of the dataset.

        !     Logical unitnumbers : LUN     = number of monitoring file

        !     Subroutines called  : none

        use timers                         ! WAQ performance timers
        implicit none

        !     Arguments           :

        !     Kind        Function         Name                  Description

        integer(kind = int_wp), intent(in) :: lun                 !< Unit number of the monitroing file
        integer(kind = int_wp), intent(in) :: nosys               !< Number of transport substances
        integer(kind = int_wp), intent(in) :: notot               !< Total number of substances
        integer(kind = int_wp), intent(in) :: noseg               !< Number of computational volumes
        real(kind = real_wp), intent(inout) :: amass (notot, noseg) !< Delwaq mass array to be updated
        real(kind = real_wp), intent(in) :: volumn(noseg)       !< Volume after rewind
        real(kind = real_wp), intent(in) :: voluml(noseg)       !< Last volume before rewind

        !     Locals

        real(kind = dp) :: tovoll   ! total of the last volume array
        real(kind = dp) :: tovoln   ! total of the last new volume array
        real(kind = dp) :: clofac   ! closure correction factor
        integer(kind = int_wp) :: iseg     ! finite volume counter

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqce", ithandl)

        !     Say what you are doing

        write (lun, 1000)

        !     Loop accross the number of computational elements

        tovoll = 0.0d00
        tovoln = 0.0d00
        do iseg = 1, noseg

            !        Calculate closure error

            if (abs(voluml(iseg)) > 1.0e-28) then
                clofac = volumn(iseg) / voluml(iseg)
            else
                clofac = 1.0
            endif
            tovoll = tovoll + voluml(iseg)
            tovoln = tovoln + volumn(iseg)

            !        Correct mass of transported substances

            amass(1:nosys, iseg) = amass(1:nosys, iseg) * clofac

        enddo

        !     Write statistics

        write (lun, 1010) tovoll
        write (lun, 1020) tovoln
        write (lun, 1030) tovoln / tovoll

        if (timon) call timstop (ithandl)
        return

        !     Output formats

        1000 format ('Performing closure error correction')
        1010 format ('Total volume before rewind:', e24.13)
        1020 format ('Total volume after rewind :', e24.13)
        1030 format ('Total correction factor   :', e24.13)

    end

end module m_dlwqce
