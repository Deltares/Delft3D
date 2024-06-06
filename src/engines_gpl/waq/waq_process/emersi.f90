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
module m_emersi
    use m_waq_precision

    implicit none

contains


    subroutine emersi (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       emersion of segments in z-layers, set segment features accordingly

        !***********************************************************************
        !
        !     function : emersion of segments, set segment features accordingly
        !
        !***********************************************************************

        use m_advtra
        use m_dhnoseg
        use m_dhnolay
        use m_evaluate_waq_attribute
        use bottomset     !  module with definition of the waterbottom segments

        implicit none

        ! arguments

        real(kind = real_wp) :: pmsa(*)            ! in/out input-output array space to be adressed with ipoint/increm
        real(kind = real_wp) :: fl(*)              ! in/out flux array
        integer(kind = int_wp) :: ipoint(*)          ! in     start index input-output parameters in the pmsa array (segment or exchange number 1)
        integer(kind = int_wp) :: increm(*)          ! in     increment for each segment-exchange for the input-output parameters in the pmsa array
        integer(kind = int_wp) :: noseg              ! in     number of segments
        integer(kind = int_wp) :: noflux             ! in     total number of fluxes (increment in fl array)
        integer(kind = int_wp) :: iexpnt(4, *)        ! in     exchange pointer table
        integer(kind = int_wp) :: iknmrk(*)          ! in     segment features array
        integer(kind = int_wp) :: noq1               ! in     number of exchanges in first direction
        integer(kind = int_wp) :: noq2               ! in     number of exchanges in second direction
        integer(kind = int_wp) :: noq3               ! in     number of exchanges in third direction
        integer(kind = int_wp) :: noq4               ! in     number of exchanges in fourth direction

        ! from pmsa array

        real(kind = real_wp) :: depth              ! 1  in  total depth of the water column
        real(kind = real_wp) :: zthreshold         ! 2  in  depth threshold for emersion (drying)
        integer(kind = int_wp) :: swemersion         ! 3  out switch indicating submersion(0) or emersion (1)

        ! local decalrations

        integer(kind = int_wp) :: ip1            ! index pointer in pmsa array
        integer(kind = int_wp) :: ip2            ! index pointer in pmsa array
        integer(kind = int_wp) :: ip3            ! index pointer in pmsa array
        integer(kind = int_wp) :: in3            ! increment in pmsa array
        integer(kind = int_wp) :: nosegw         ! number of water segments
        integer(kind = int_wp) :: nosegl         ! number of segments per layer
        integer(kind = int_wp) :: nolay          ! number of layers
        integer(kind = int_wp) :: iseg           ! loop counter segment loop
        integer(kind = int_wp) :: iseg_down      ! underlying segment
        integer(kind = int_wp) :: ikmrk1         ! first feature inactive/active/bottom
        integer(kind = int_wp) :: ikmrk2         ! second feature surf+bottom(0)-surf(1)-mid(2)-bottom(3) segment
        integer(kind = int_wp) :: ik             ! bottom column index
        integer(kind = int_wp) :: iq             ! exchange loop counter
        integer(kind = int_wp) :: iwa1           ! first water-sediment exchange in sediment column
        integer(kind = int_wp) :: iwa2           ! last water-sediment exchange in sediment column
        integer(kind = int_wp) :: itop           ! first sediment-sediment exchange in sediment column
        integer(kind = int_wp) :: ibot           ! last sediment-sediment exchange in sediment column
        integer(kind = int_wp) :: iwater         ! water segment number
        integer(kind = int_wp) :: sw_water       ! emersion switch for the water segment
        integer(kind = int_wp) :: ibodem         ! bottom segment number
        integer(kind = int_wp), save :: opemersion = 2 ! option emersion 1 = all, 2 = one, 3 = average

        ! initialise bottom if necessary

        call makko2 (iexpnt, iknmrk, noq1, noq2, noq3, &
                noq4)

        ip1 = ipoint(1)
        ip2 = ipoint(2)
        ip3 = ipoint(3)

        ! handle the active water segments

        call dhnoseg(nosegw)
        call dhnolay(nolay)
        nosegl = nosegw / nolay

        do iseg = 1, nosegw

            call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
            if (ikmrk1 == 1) then
                depth = pmsa(ip1)
                zthreshold = pmsa(ip2)

                ! look if segment has water surface

                call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
                if ((ikmrk2==1 .or. ikmrk2==0) .and. depth <= zthreshold) then

                    ! set this segment to inactive

                    iknmrk(iseg) = iknmrk(iseg) - 1
                    swemersion = 1

                    iseg_down = iseg + nosegl
                    if (iseg_down <= nosegw) then

                        ! give the underlying segment the attribure with water surface

                        call extract_waq_attribute(2, iknmrk(iseg_down), ikmrk2)
                        if (ikmrk2==2) then
                            iknmrk(iseg_down) = iknmrk(iseg_down) - 10 ! sets second attribute to 1
                        elseif (ikmrk2==3) then
                            iknmrk(iseg_down) = iknmrk(iseg_down) - 30 ! sets second attribute to 0
                        endif

                    endif
                else
                    swemersion = 0
                endif

                pmsa (ip3) = swemersion

            endif

            ip1 = ip1 + increm(1)
            ip2 = ip2 + increm(2)
            ip3 = ip3 + increm(3)

        enddo

        ! handle the bottom segments, if one of the overlying segments is wet then the bottom is submersed

        ip3 = ipoint(3)
        in3 = increm(3)

        do ik = 1, coll%current_size

            iwa1 = coll%set(ik)%fstwatsed
            iwa2 = coll%set(ik)%lstwatsed
            itop = coll%set(ik)%topsedsed
            ibot = coll%set(ik)%botsedsed

            if (opemersion == 1) then
                swemersion = 1
            else
                swemersion = 0
            endif
            do iq = iwa1, iwa2
                iwater = iexpnt(1, iq)
                sw_water = nint(pmsa(ip3 + (iwater - 1) * in3))
                if (opemersion == 1) then
                    if (sw_water==0) then
                        swemersion = 0
                    endif
                elseif (opemersion == 2) then
                    if (sw_water==1) then
                        swemersion = 1
                    endif
                endif
            enddo

            do iq = itop, ibot
                ibodem = iexpnt(1, iq)
                pmsa(ip3 + (ibodem - 1) * in3) = swemersion
            enddo

        enddo

        return
    end

end module m_emersi
