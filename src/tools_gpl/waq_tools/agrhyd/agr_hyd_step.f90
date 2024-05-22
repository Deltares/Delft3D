!!  Copyright (C)  Stichting Deltares, 2021-2023.
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

subroutine agr_hyd_step(input_hyd, ipnt, ipnt_q, ipnt_vdf, ipnt_tau, output_hyd)

    ! function : aggregeate one hydrodynamic step

    ! global declarations

    use m_logger_helper, only : stop_with_error
    use m_hydmod
    use aggregation, only : aggregate_extended, AGGREGATION_TYPE_ACCUMULATE, AGGREGATION_TYPE_ACCUMULATE_SIGNED, &
            AGGREGATION_TYPE_WEIGHTED_AVERAGE, AGGREGATION_TYPE_MINIMUM

    implicit none

    ! declaration of the arguments

    type(t_hydrodynamics) :: input_hyd     ! the input hydrodynamics
    integer :: ipnt(*)       ! aggregation pointer segments
    integer :: ipnt_q(*)     ! aggregation pointer exchanges
    integer :: ipnt_vdf(*)   ! aggregation pointer vertical diffusion
    integer :: ipnt_tau(*)   ! aggregation pointer tau
    type(t_hydrodynamics) :: output_hyd    ! the output hydrodynamics

    ! local declarations

    real, allocatable :: rwork(:)      ! general work array aggregation
    real, allocatable :: vdfwork(:)    ! work array vertical diffusion, minimum over the aggregated layers
    integer :: noseg2        ! length of vdfwork
    integer :: ilay          ! loop counter layers
    integer :: iseg          ! segment index
    integer :: iq            ! exchange index
    integer :: i1, i2, i3, i4! indexes in array to point to a certain layer
    integer :: ierr_alloc    !
    real, parameter :: rmiss = -999.


    ! some init

    allocate(rwork(output_hyd%noseg), stat = ierr_alloc)
    if (ierr_alloc /= 0) then ; write(*, *) ' error allocating memory' ; call stop_with_error() ;
    endif

    ! volumes

    call aggregate_extended(input_hyd%noseg, output_hyd%noseg, &
            1, 1, &
            1, 1, &
            1, 0, &
            0, 1, &
            ipnt, AGGREGATION_TYPE_ACCUMULATE, &
            input_hyd%volume, rwork, &
            rwork, output_hyd%volume)

    ! volumes always positive

    do iseg = 1, output_hyd%noseg
        output_hyd%volume(iseg) = max(0.0, output_hyd%volume(iseg))
    enddo

    ! areas

    call aggregate_extended(input_hyd%noq, output_hyd%noq, &
            1, 1, &
            1, 1, &
            1, 0, &
            0, 1, &
            ipnt_q, AGGREGATION_TYPE_ACCUMULATE, &
            input_hyd%area, rwork, &
            rwork, output_hyd%area)

    ! areas always positive

    do iq = 1, output_hyd%noq
        output_hyd%area(iq) = max(0.0, output_hyd%area(iq))
    enddo

    ! flows

    call aggregate_extended(input_hyd%noq, output_hyd%noq, &
            1, 1, &
            1, 1, &
            1, 0, &
            0, 1, &
            ipnt_q, AGGREGATION_TYPE_ACCUMULATE_SIGNED, &
            input_hyd%flow, rwork, &
            rwork, output_hyd%flow)

    ! salinity, averaged with volume

    if (input_hyd%sal_present) then
        call aggregate_extended(input_hyd%noseg, output_hyd%noseg, &
                1, 1, &
                1, 1, &
                1, 1, &
                1, 1, &
                ipnt, AGGREGATION_TYPE_WEIGHTED_AVERAGE, &
                input_hyd%sal, input_hyd%volume, &
                rwork, output_hyd%sal)
    endif

    ! temperature, averaged with volume

    if (input_hyd%tem_present) then
        call aggregate_extended(input_hyd%noseg, output_hyd%noseg, &
                1, 1, &
                1, 1, &
                1, 1, &
                1, 1, &
                ipnt, AGGREGATION_TYPE_WEIGHTED_AVERAGE, &
                input_hyd%tem, input_hyd%volume, &
                rwork, output_hyd%tem)
    endif

    ! tau, only do last layer, other layers 0.0

    !     if ( input_hyd%tau_present ) then
    !        output_hyd%tau = 0.0
    !        i1 = (input_hyd%nolay-1)*input_hyd%nosegl + 1
    !        i2 = (output_hyd%nolay-1)*output_hyd%nosegl + 1
    !        call aggregate_extended( input_hyd%nosegl , output_hyd%nosegl ,
    !    +                1                , 1                 ,
    !    +                1                , 1                 ,
    !    +                1                , 1                 ,
    !    +                1                , 1                 ,
    !    +                ipnt             , AGGREGATION_TYPE_WEIGHTED_AVERAGE       ,
    !    +                input_hyd%tau(i1), input_hyd%surf(i1),
    !    +                rwork            , output_hyd%tau(i2))
    !     endif
    if (input_hyd%tau_present) then
        output_hyd%tau = 0.0
        call aggregate_extended(input_hyd%noseg, output_hyd%noseg, &
                1, 1, &
                1, 1, &
                1, 1, &
                1, 1, &
                ipnt_tau, AGGREGATION_TYPE_WEIGHTED_AVERAGE, &
                input_hyd%tau, input_hyd%surf, &
                rwork, output_hyd%tau)
    endif

    ! vdf

    if (input_hyd%vdf_present) then

        if (input_hyd%nolay /= output_hyd%nolay) then

            ! take minimum of the surrounding segments (actauly exchanges) over the layers only

            noseg2 = input_hyd%nosegl * output_hyd%nolay
            allocate(vdfwork(noseg2), stat = ierr_alloc)
            if (ierr_alloc /= 0) then ; write(*, *) ' error allocating memory' ; call stop_with_error() ;
            endif
            do iseg = 1, noseg2
                vdfwork(iseg) = rmiss
            enddo
            call aggregate_extended(input_hyd%noseg, noseg2, &
                    1, 1, &
                    1, 1, &
                    1, 1, &
                    1, 1, &
                    ipnt_vdf, AGGREGATION_TYPE_MINIMUM, &
                    input_hyd%vdf, input_hyd%volume, &
                    rwork, vdfwork)

            ! then average over the horizonatal aggregation, last layer is not used

            do ilay = 1, output_hyd%nolay - 1
                i1 = (ilay - 1) * input_hyd%nosegl + 1
                i4 = ilay * input_hyd%nosegl
                i2 = (ilay - 1) * output_hyd%nosegl + 1
                i3 = ilay * output_hyd%nosegl
                call aggregate_extended(input_hyd%nosegl, output_hyd%nosegl, &
                        1, 1, &
                        1, 1, &
                        1, 1, &
                        1, 1, &
                        ipnt, AGGREGATION_TYPE_WEIGHTED_AVERAGE, &
                        vdfwork(i1), input_hyd%surf, &
                        rwork, output_hyd%vdf(i2:i3))
            enddo

            ! last layer set to zero

            i2 = (output_hyd%nolay - 1) * output_hyd%nosegl + 1
            output_hyd%vdf(i2:output_hyd%noseg) = 0.0

            deallocate(vdfwork, stat = ierr_alloc)
            if (ierr_alloc /= 0) then ; write(*, *) ' error deallocating memory' ; call stop_with_error() ;
            endif

        else

            output_hyd%vdf = 0.0
            call aggregate_extended(input_hyd%noseg, output_hyd%noseg, &
                    1, 1, &
                    1, 1, &
                    1, 1, &
                    1, 1, &
                    ipnt, AGGREGATION_TYPE_WEIGHTED_AVERAGE, &
                    input_hyd%vdf, input_hyd%surf, &
                    rwork, output_hyd%vdf)

        endif

    endif

    deallocate(rwork, stat = ierr_alloc)
    if (ierr_alloc /= 0) then ; write(*, *) ' error deallocating memory' ; call stop_with_error() ;
    endif
    return
end
