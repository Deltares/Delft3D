!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_write_part_restart_file

    implicit none

contains


    subroutine write_part_restart_file()

        use partmem      !   for PARTicle tracking
        use m_part_regular
        use spec_feat_par
        use fileinfo
        use openfl_mod
        use m_part_modeltypes       ! part model definitions

        implicit none

        ! Locals
        integer(int_wp) :: ilp, isp, ids, ide, iext, nores, noras
        integer(int_wp) :: lunut             !  output unit number
        integer :: lures



        ! first to calculate the number of particles in the restart files
        nores = 0
        noras = 0
        do ilp = 1, nopart
            if (npart(ilp)>1.and.mpart(ilp)>1) then          !only for the active particles
                if (lgrid(npart(ilp), mpart(ilp))>=1) then
                    nores = nores + 1          ! only for the active particles
                    if (max_restart_age > 0 .and. iptime(ilp) < max_restart_age) then
                        noras = noras + 1       ! if max_restart_age is a positve and the particles' age is less then max_restart_age
                    end if
                end if
            end if
        enddo

        res_file = fnamep(1)
        iext = len_trim(res_file) - 3
        if (max_restart_age < 0) then
            !             Write the restart file with all active paritcles
            if (modtyp == model_prob_dens_settling) then
                res_file(iext + 1:iext + 4) = 'ses'    !limited number of particles (for 'plastics' modeltype 6 restart, as 'ras' but including settling values)
                write (lunut, *) ' Including particle dependent settling velocity'
            else
                res_file(iext + 1:iext + 4) = 'res'     !all results, except those that are inactive (outside model)
            end if
            write (lunut, *) ' Opening restart particles file:', idp_file(1:len_trim(res_file))
            call openfl (lures, res_file, 1)
            write (lures) 0, nores, nosubs

            do ilp = 1, nopart
                if (npart(ilp)>1.and.mpart(ilp)>1) then
                    if (lgrid(npart(ilp), mpart(ilp))>=1) then  !only for the active particles
                        if (modtyp /= model_prob_dens_settling) then
                            write (lures) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), &
                                    wpart(1:nosubs, ilp), iptime(ilp)
                        else
                            write (lures) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), &
                                    wpart(1:nosubs, ilp), spart(1:nosubs, ilp), iptime(ilp)
                        end if
                    end if
                end if
            enddo
            write (lunut, *) ' Number of active particles in the restart file: ', nores
            close (lures)
        else
            !          Write the restart file with all active paritcles below a certain age
            if (modtyp == model_prob_dens_settling) then
                res_file(iext + 1:iext + 4) = 'sas'    !limited number of particles (for 'plastics' modeltype 6 restart, as 'ras' but including settling values)
                write (lunut, *) ' Including particle dependent settling velocity'
            else
                res_file(iext + 1:iext + 4) = 'ras'    !limited number of particles (remove particles older than a certain age or inactive)
            end if
            write (lunut, *) ' Opening restart particles file:', idp_file(1:len_trim(res_file))
            write (lunut, *) ' Particles older than ', max_restart_age, ' seconds are removed'
            call openfl (lures, res_file, 1)
            write (lures) 0, noras, nosubs

            do ilp = 1, nopart
                if (npart(ilp)>1.and.mpart(ilp)>1) then
                    if (lgrid(npart(ilp), mpart(ilp))>=1 .and. (iptime(ilp)<max_restart_age)) then   !only when the particles' age less than max_restart_age, time in seconds
                        if (modtyp /= model_prob_dens_settling) then
                            write (lures) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), &
                                    wpart(1:nosubs, ilp), iptime(ilp)
                        else
                            write (lures) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), &
                                    wpart(1:nosubs, ilp), spart(1:nosubs, ilp), iptime(ilp)
                        end if
                    end if
                end if
            enddo
            write (lunut, *) ' Number of active particles in the restart file below maximum age: ', noras
            close (lures)
        end if
    end subroutine write_part_restart_file


end module m_write_part_restart_file
