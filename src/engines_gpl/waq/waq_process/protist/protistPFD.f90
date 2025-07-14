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
module m_protistpfd
    use m_waq_precision

    implicit none

contains



    ! 6 char name for process mathc with second line of PDF
    subroutine PROPFD     (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !
        !*******************************************************************************
        !
        IMPLICIT NONE
        !
        !     Type    Name         I/O Description
        !
        integer(kind = int_wp), parameter :: plen = 3 ! total length of the process_space_real input and output array
        real(kind = real_wp) :: process_space_real(*)      ! I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)        ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(plen) ! I  Array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(plen) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells        ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux       ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *)  ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)    ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir         ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir         ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir         ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir         ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        !
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        !     support variables
        integer(kind = int_wp) :: ipnt(plen)    ! Local work array for the pointering
        integer(kind = int_wp) :: iseg          ! Local loop counter for computational element loop
        integer(kind = int_wp) :: ioq
        integer(kind = int_wp) :: ikmrk1        ! first segment attribute

        ! input state variables
        real(kind = real_wp) :: Rad     ! irradiation at the segment upper-boundary              (W/m2)

        !auxiliaries
        real(kind = real_wp) :: PARRAD  ! from RAd to PARRAD
        real(kind = real_wp) :: PFD     ! from rad to photon flux density                        (umol photon m-2)

        !
        !*******************************************************************************
        !
        ipnt = ipoint

        !iflux = 0

        ! segment loop
        do iseg = 1, num_cells
            call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
            if (ikmrk1==1) then

                Rad = process_space_real(ipnt(1))  !    irradiation at the segment upper-boundary              (W/m2)

                ! calculate light availability in segment
                ! from RAD [W/m2] to PARRAD [W/m2]
                PARRAD = RAD * 0.45
                ! 1 W/m2 = 4.57 umol photons m-2 s-1
                PFD = PARRAD * 4.57 ! from PARRAD to PFD

                ! segment dependent, but species independent output
                process_space_real(ipnt(2)) = PFD
                process_space_real(ipnt(3)) = PARRAD

            endif ! end if check for dry cell

            !allocate pointers
            !iflux = iflux + noflux
            ipnt = ipnt + increm

        enddo ! end loop over segments
        return
    end
    ! end subroutine

end module m_protistpfd
