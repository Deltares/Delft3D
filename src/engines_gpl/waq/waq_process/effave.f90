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
module m_effave
    use m_waq_precision
    use m_get_effi

    implicit none

contains


    subroutine effave (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)

        !>\file
        !>       Average efficiency for a Bloom time step (typically a day)

        !
        !     Description of the module :
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        implicit none
        real(kind = real_wp) :: pmsa  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(92), increm(92), noseg, noflux, &
                iexpnt(4, *), iknmrk(*), noq1, noq2, noq3, noq4

        integer(kind = int_wp) :: ip(92)
        real(kind = real_wp) :: delt, efftalg, limralg, rstep
        integer(kind = int_wp) :: navera
        integer(kind = int_wp) :: iseg, iflux, igro
        integer(kind = int_wp) :: nspe       ! number of bloom algae species

        integer(kind = int_wp), save :: istep = 0

        !     this is in a module/include, so we might put a flag if it was read of not.
        !     this should be a 'proto-proces', and thus needs to be added to the BLOOM.SPE

        !     Retrieve switch for averaging and nr. of steps to be averaged
        call get_nspe(nspe)

        delt = pmsa(ipoint(1))
        navera = nint(pmsa(ipoint(2)))
        rstep = real (istep, 4)

        !     Loop over segments

        ip = ipoint
        iflux = 0

        do iseg = 1, noseg

            do igro = 1, nspe
                efftalg = pmsa(ip(2 + igro))
                limralg = pmsa(ip(32 + igro))
                if (istep == 0) then
                    ! Store result over past period
                    pmsa(ip(62 + igro)) = efftalg
                    ! Reset integration variable to zero and add contribution of present time step to tracer
                    fl(iflux + igro) = (limralg - efftalg) / delt
                else
                    ! Add contribution of present time step to tracer
                    fl(iflux + igro) = ((1.0 / (rstep + 1.0) * limralg) + (rstep / (rstep + 1.0) - 1.0) * efftalg) / delt
                endif
            end do
            ip = ip + increm
            iflux = iflux + noflux
        enddo
        !     Add 1 to counter and check for period
        istep = istep + 1
        if (istep == navera) istep = istep - navera
        !
        return
        !
    end

end module m_effave
