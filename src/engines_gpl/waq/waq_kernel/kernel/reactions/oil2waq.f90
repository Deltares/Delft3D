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
module m_oil2waq
    use m_waq_precision
    use m_string_utils

    implicit none

contains


    subroutine oil2waq (nopart, nosys, notot, nosubs, noseg, &
            nolay, volume, surface, nmax, mmax, &
            lgrida, syname, itime, iddtim, npwndw, &
            iptime, npart, mpart, kpart, wpart, &
            amass, conc, iaflag, intopt, ndmps, &
            isdmp, dmps, amass2)

        !     Deltares Software Centre

        !>\File
        !>      Migrates particles from delpar to delwaq if their resedence time exceeds the take over time
        !>
        !>      At first call it is determined which part substance belongs to which waq substance.
        !>      That is done by looking for a waq substance with the same name as the name of the
        !>      part substance minus its last letter. It is advised to give the part substance the
        !>      name of the corresponding waq substance plus the letter 'p'.\n
        !>      If the corresponding waq substance is not transported, the mass contribution of the particle
        !>      is divided by the horizontal surface area for the concentration per m^2, otherwise it is
        !>      divided by the volume for the concentration per m^3.\n
        !>      After the take over by Delwaq the location of the particle is set in the upper left corner
        !>      of the grid, its weight is set to zero and the particle window counter is increased to the
        !>      level of particles that are still to young to be migrated.

        !     Created             : April     2013 by Leo Postma
        !     Adapted             : May 2013 Frank Kleissen - adapted from par2waq.f: specifc for the oil module
        !                           to transfer dispersed oil, no take over time
        !     Files               : none

        !     Routines            : zoek  - to search the delwaq names

        use timers

        implicit none

        !     kind           function         name                      description

        integer(kind = int_wp), intent(in) :: nopart                  !< total number of particles
        integer(kind = int_wp), intent(in) :: nosys                   !< transported substances in delwaq
        integer(kind = int_wp), intent(in) :: notot                   !< total substances in delwaq
        integer(kind = int_wp), intent(in) :: nosubs                  !< total substances in delpar
        integer(kind = int_wp), intent(in) :: noseg                   !< total number of gridcells in delwaq
        integer(kind = int_wp), intent(in) :: nolay                   !< number of layers in delwaq
        real(kind = real_wp), intent(in) :: volume (noseg)          !< delwaq volumes
        real(kind = real_wp), intent(in) :: surface(noseg)          !< delwaq horizontal surfaces
        integer(kind = int_wp), intent(in) :: nmax                    !< first grid dimension
        integer(kind = int_wp), intent(in) :: mmax                    !< second grid dimension
        integer(kind = int_wp), intent(in) :: lgrida (nmax, mmax)      !< active computational grid
        character(20), intent(in) :: syname (notot)          !< names of the substances
        integer(kind = int_wp), intent(in) :: itime                   !< current time
        integer(kind = int_wp), intent(in) :: iddtim                  !< delwaq take-over delay time
        integer(kind = int_wp), intent(inout) :: npwndw                  !< first active particle in array
        integer(kind = int_wp), intent(inout) :: iptime (nopart)         !< age of the particles
        integer(kind = int_wp), intent(inout) :: npart  (nopart)         !< first grid index particles
        integer(kind = int_wp), intent(inout) :: mpart  (nopart)         !< second grid index particles
        integer(kind = int_wp), intent(inout) :: kpart  (nopart)         !< third grid index particles
        real(kind = real_wp), intent(inout) :: wpart  (nosubs, nopart) !< weight of the particles
        real(kind = real_wp), intent(inout) :: amass  (notot, noseg) !< delwaq masses per cell
        real(kind = real_wp), intent(inout) :: conc   (notot, noseg) !< delwaq concentrations per cell
        integer(kind = int_wp), intent(in) :: iaflag                  !< if 1 then accumulation of balances
        integer(kind = int_wp), intent(in) :: intopt                  !< integration suboptions
        integer(kind = int_wp), intent(in) :: ndmps                   !< number of dumped volumes for balances
        integer(kind = int_wp), intent(in) :: isdmp  (noseg)         !< volume to dump-location pointer
        real(kind = real_wp), intent(inout) :: dmps   (notot, ndmps, *) !< dumped segment fluxes if INTOPT > 7
        real(kind = real_wp), intent(inout) :: amass2 (notot, 5)     !< mass balance array

        !     Local declarations

        integer(kind = int_wp), allocatable, save :: iwaqsub(:)        ! pointer from part substance to waq substance
        character(20)                 partsub           ! this particle substance
        integer(kind = int_wp) :: isub, ipart       ! loop variables
        integer(kind = int_wp) :: ic, iseg, ilay ! help variable for segment location
        integer(kind = int_wp) :: ioff              ! help variable start of delpar substances in delwaq
        integer(kind = int_wp) :: nosegl            ! number of cells per layer
        logical                       fluxes            ! set .true. if intopt > 7
        logical                       massbal           ! set .true. if iaflag eq 1
        integer(kind = int_wp) :: ipb, isys         ! help variables

        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("oil2waq", ithandl)

        massbal = iaflag == 1
        fluxes = btest(intopt, 3)

        if (.not. allocated(iwaqsub)) then
            allocate (iwaqsub(nosubs))
            ioff = notot - nosubs
            do isub = 1, nosubs
                partsub = syname(ioff + isub) (1:len_trim(syname(ioff + isub)) - 1) ! cut the 'p' off
                iwaqsub(isub) = index_in_array(partsub, syname(:ioff))
                if (iwaqsub(isub) < 0) iwaqsub(isub) = 0 ! not found!
                if (iwaqsub(isub) > nosys) iwaqsub(isub) = -iwaqsub(isub)      ! not dissolved
            enddo
        endif
        nosegl = noseg / nolay

        do ipart = npwndw, nopart
            ic = lgrida(npart(ipart), mpart(ipart))
            if (ic >  0) then
                ilay = kpart(ipart)
                iseg = (ilay - 1) * nosegl + ic
                ipb = isdmp(iseg)
                do isub = 1, nosubs
                    isys = iwaqsub(isub)
                    if (isys == 0) cycle
                    if (isub==2.and.wpart(isub, ipart)>0) then
                        if (isys < 0) then
                            isys = abs(isys)
                            amass(isys, iseg) = amass(isys, iseg) + wpart(isub, ipart)
                            conc (isys, iseg) = amass(isys, iseg) / surface(iseg)
                        else
                            amass(isys, iseg) = amass(isys, iseg) + wpart(isub, ipart)
                            conc (isys, iseg) = amass(isys, iseg) / volume (iseg)
                        endif
                        if (massbal) then
                            amass2(isys, 3) = amass2(isys, 3) + wpart(isub, ipart)
                        end if
                        if (ipb > 0 .and. fluxes) then
                            dmps(isys, ipb, 2) = dmps(isys, ipb, 2) + wpart(isub, ipart)
                        end if
                        npart (ipart) = 1
                        mpart (ipart) = 1
                        kpart (ipart) = 1
                        iptime(ipart) = 0
                        wpart(isub, ipart) = 0.0
                    endif
                enddo
            endif
        enddo

        if (timon) call timstop (ithandl)

        return
    end

end module m_oil2waq
