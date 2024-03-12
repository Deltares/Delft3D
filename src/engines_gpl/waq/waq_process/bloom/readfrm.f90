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
module m_readfrm
    use m_waq_precision

    implicit none

contains


    !
    !  Readfrm reads the frm-file that contains the species/groups names and the integrated efficiency curves.
    !
    subroutine readfrm

        use bloom_data_dim
        use bloom_data_size
        use bloom_data_arran
        use bloom_data_io
        use bloom_data_phyt
        use bloom_data_sumou

        implicit none

        character*60 aline

        integer(kind = int_wp) :: i, ioff, j


        !  Read data for the integrated photosynthetic efficiency curves.
        !  Input section for FORMATTED read of efficiency curves!
        !
        verfrm = 1.0
        read (infrm, '(a)') aline
        ioff = index(aline, 'BLOOMFRM_VERSION')
        if(ioff==0) then
            rewind(infrm)
        else
            read (aline(ioff + 17:ioff + 20), *) verfrm
            read (infrm, 199) (grname(j), j = 1, nuecog)
            read (infrm, 199) (spname(i), i = 1, nuspec)
            if(verfrm>2.00) then
                read (infrm, *) npoint
                do i = 1, npoint
                    read (infrm, *) power(i), (effic(i, j), j = 1, nuecog)
                end do
            endif
        endif
        if(verfrm>2.00) then
            read (infrm, 200) nz, tefcur
            do i = 1, nz
                read (infrm, 210) zvec(i), (fun(i, j), j = 1, nuecog)
            enddo
            read (infrm, 200) nz, tefcur
            do i = 1, nz
                read (infrm, 210) zvec(i), (der(i, j), j = 1, nuecog)
            enddo
        else
            read (infrm, 200) nz, tefcur
            read (infrm, 210) (zvec(i), i = 1, nz)
            read (infrm, 200) nz
            do i = 1, nz
                read (infrm, 210) (fun(i, j), j = 1, nuecog)
                read (infrm, 210) (der(i, j), j = 1, nuecog)
            end do
        endif
        199 format (30(a10, x))
        200 format (i5, 5x, f10.2)
        210 format (10(d15.8, 3x))
        daymul = 0.0d0
        dl = 0.0d0
        do i = 1, 24
            read (infrm, 240) dl(i), (daymul(i, j), j = 1, nuecog)
        end do
        240 format (11f5.0)

        !  Close the efficiency file.
        close (infrm)

        return
    end

end module m_readfrm
