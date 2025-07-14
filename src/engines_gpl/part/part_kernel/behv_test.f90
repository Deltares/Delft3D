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

module behv_test_mod
    !
    !  data definition module(s)
    !
    use m_stop_exit
    use m_waq_precision              ! single/double precision
    use timers
    !
    !  module procedure(s)
    !
    use orien_salinity_mod          ! explicit interface
    use orien_temperature_mod       ! explicit interface
    use re_orien_sal_temp_mod       ! explicit interface
    use vert_swimm_tidal_mod        ! explicit interface
    !
    implicit none

contains
    subroutine behv_test (btype, hbtype, v_swim, d_swim, n, &
            m, num_rows, num_columns, mnmaxk, lgrid, &
            lgrid2, lgrid3, nosegl, wpart, ipart, &
            wsettl, k, kpart, zpart, xpart, &
            ypart, num_layers, idelt, day, phase_diurn, &
            ebb_flow, flow, depth, salin1, temper1, &
            vol1, vol2, vel1, vel2, fstage, &
            ztop, zlevel, zdepth, zbot, buoy, &
            vzact, vz, buoy1, buoy2, ztop1, &
            ztop2, zbot1, zbot2, vzact1, vzact2, &
            vswim1, vswim2, iseg, lunrep, angle)

        ! function  : Test behaviour
        !

        ! arguments :

        integer(int_wp), intent(in) :: lunrep              ! report file
        integer(int_wp), intent(in) :: nosegl              ! number segments per layer
        integer(int_wp), intent(in) :: num_layers               ! number of layers in calculation
        integer(int_wp), intent(in) :: num_rows                ! first grid dimension
        integer(int_wp), intent(in) :: num_columns                ! second grid dimension
        integer(int_wp), intent(in) :: mnmaxk              ! total number of active grid cells
        integer(int_wp), pointer :: lgrid (:, :)     ! grid with active grid numbers, negatives for open boundaries
        integer(int_wp), pointer :: lgrid2(:, :)     ! total grid
        integer(int_wp), pointer :: lgrid3(:, :)     ! original grid (conc array)

        integer(int_wp), pointer :: kpart (:)         ! third grid index of the particles
        real   (sp), pointer :: xpart (:)         ! x-value (0.0-1.0) first  direction within grid cell
        real   (sp), pointer :: ypart (:)         ! y-value (0.0-1.0) second direction within grid cell
        real   (sp), pointer :: zpart (:)         ! z-value (0.0-1.0) third  direction within grid cell
        real   (sp), pointer, intent(in) :: wpart (:, :)      ! weight factors of the subs per particle

        real   (sp), pointer :: wsettl(:)         ! settling per particle

        real   (sp), pointer :: angle (:)         ! angle with horizontal
        real   (sp), pointer :: depth(:)          ! total depth of the cells from top to bottom
        real   (sp), pointer :: vol1  (:)         ! volume begin hydr step
        real   (sp), pointer :: vol2  (:)         ! volume end hydr step
        real   (sp), pointer :: flow  (:)         ! all flows
        real   (sp), pointer :: salin1(:)         ! salinity segment numbering
        real   (sp), pointer :: temper1(:)        ! temperature segment numbering
        real   (sp), pointer :: v_swim(:)         ! horizontal swimming velocity m/
        real   (sp), pointer :: d_swim(:)         ! horizontal swimming direction (degree)
        real   (sp), pointer :: vel1  (:)         ! velocity begin hydr step
        real   (sp), pointer :: vel2  (:)         ! velocity end hydr step

        ! from input (const)

        integer :: iseg                ! iseg

        integer, pointer :: btype(:)            ! behaviour type
        real(sp), pointer :: buoy1(:)            ! buoyancy begin stage
        real(sp), pointer :: buoy2(:)            ! buoyancy end stage
        real(sp), pointer :: vzact1(:)           ! active vertical velocity begin stage
        real(sp), pointer :: vzact2(:)           ! active vertical velocity end stage
        real(sp), pointer :: ztop1(:)            ! depth top layer begin stage
        real(sp), pointer :: ztop2(:)            ! depth top layer end stage
        real(sp), pointer :: zbot1(:)            ! depth bot layer begin stage
        real(sp), pointer :: zbot2(:)            ! depth bot layer end stage
        integer, pointer :: hbtype(:)           ! horizontal behaviour type
        real(sp), pointer :: vswim1(:)           ! swim velocity at begin of stage
        real(sp), pointer :: vswim2(:)           ! swim velocity at end of stage

        ! local :

        real(sp), pointer :: phase_diurn(:)      ! phase in diurnal behaviour
        integer(int_wp) :: ipart               ! particle index
        real   (sp) :: fstage              ! fraction of current stage
        integer(int_wp) :: istage              ! integer stage development
        integer(int_wp) :: idelt               ! timesteps in seconds
        real   (sp) :: day                 ! time in days
        real   (sp) :: a                   ! a coefficient in development (-)
        real   (sp) :: b                   ! b coefficient in development (-)
        real   (sp) :: vz                  ! vz
        real   (sp) :: vzact               ! vzact
        real   (sp) :: buoy                ! buoy
        integer :: k                   ! k
        real   (sp) :: ztop                ! ztop
        real   (sp) :: zbot                ! zbot
        integer :: m                   ! m
        integer :: n                   ! n
        real   (sp) :: zdepth              ! z relative to water surface
        real   (sp) :: zlevel              ! z relative to bottom
        logical, pointer :: ebb_flow(:)       ! true if flow is ebb

        integer :: behaviour_type      ! actual behaviour type

        integer, parameter :: behaviour_none = 0 ! behaviour type none
        integer, parameter :: behaviour_bottom = 1 ! behaviour type bottom
        integer, parameter :: behaviour_midlow = 2 ! behaviour type midlow
        integer, parameter :: behaviour_midtop = 3 ! behaviour type midtop
        integer, parameter :: behaviour_pelagic = 4 ! behaviour type pelagic
        integer, parameter :: behaviour_demersal = 5 ! behaviour type demersal
        integer, parameter :: behaviour_diurnal = 6 ! behaviour type diurnal
        integer, parameter :: behaviour_herring = 7 ! behaviour type herring
        integer, parameter :: behaviour_stst_dem = 8 ! behaviour type stst_dem
        integer, parameter :: behaviour_stst_pel = 9 ! behaviour type stst_pel

        integer :: h_behaviour_type       ! horizontal behaviour

        integer, parameter :: h_behaviour_none = 0    ! no horizontal behaviour
        integer, parameter :: h_behaviour_salinity = 1    ! horizontal behaviour towards lowest salinity
        integer, parameter :: h_behaviour_temperature = 2    ! horizontal behaviour towards lowest temperature
        integer, parameter :: h_behaviour_re_orien_sal_temp = 3    ! horizontal behaviour towards lowest salinity
        ! avoinding temperature boundaries

        real :: vswim                  ! swimming velocity
        real :: local_angle            ! angle towards lowest salinity in grid

        real :: sal_n0
        real :: sal_n1
        real :: sal_n12
        real :: sal_n2
        real :: sal_n23
        real :: sal_n3
        real :: sal_n34
        real :: sal_n4
        real :: sal_n41

        real :: lb_sal             ! lower boundary of salinity
        real :: ub_sal             ! upper boundary of salinity

        real :: temp_n0
        real :: temp_n1
        real :: temp_n12
        real :: temp_n2
        real :: temp_n23
        real :: temp_n3
        real :: temp_n34
        real :: temp_n4
        real :: temp_n41

        real :: lb_temp             ! lower boundary of temperature
        real :: ub_temp             ! upper boundary of temperature

        logical :: stick_to_bottom        ! stick to bottom when reached


        !TEST NEW VERTICAL AND HORIZONTAL BEHAVIOURS

        istage = wpart(2, ipart)                                                         ! Get current stage of particle

        !Set layer and/or settling velocity according to stage and type of behaviour

        behaviour_type = btype(istage)                                                   ! Assign the vertical behaviour type by stage
        select case (behaviour_type)                                                   ! Select behaviour by numbering

        case (behaviour_none)                                                       !Behaviour 0
            wsettl(ipart) = 0.0                                                        ! Setteling velocity is set to 0

        case (behaviour_bottom)                                                     !Behaviour 1
            wsettl(ipart) = 0.0                                                        ! Setteling velocity is set to 0
            kpart(ipart) = num_layers + 1                                                   ! Particle is placed in the storage layer (total layers +1)
            zpart(ipart) = 0.5                                                         ! Particle is positioned in the middle of the cell in the third dimension

        case (behaviour_midlow)                                                     !Behaviour 2
            wsettl(ipart) = 0.0                                                        ! Setteling velocity is set to 0
            kpart(ipart) = num_layers                                                       ! Particle is placed in the layer above the bottom
            zpart(ipart) = 0.5                                                         ! Particle is positioned in the middle of the cell in the third dimension

        case (behaviour_midtop)                                                     !Behaviour 3
            wsettl(ipart) = 0.0                                                        ! Setteling velocity is set to 0
            kpart(ipart) = 1                                                           ! Particle is placed in the surface layer
            zpart(ipart) = 0.5                                                         ! Particle is positioned in the middle of the cell in the third dimension

        case (behaviour_pelagic)                                                    !Behaviour 4
            wsettl(ipart) = buoy1(istage) + fstage * (buoy2(istage) - buoy1(istage))       ! The setteling velocity is determined by the bouyancy dependend on stage

        case (behaviour_demersal)                                                   !Behaviour 5
            ztop = ztop1(istage) + fstage * (ztop2(istage) - ztop1(istage))              ! Set the maximum position in the water column for the particle
            zbot = zbot1(istage) + fstage * (zbot2(istage) - zbot1(istage))              ! Set the minimum position in the water column for the particle
            buoy = buoy1(istage) + fstage * (buoy2(istage) - buoy1(istage))              ! Set the bouyancy for the particle
            vzact = vzact1(istage) + fstage * (vzact2(istage) - vzact1(istage))            ! Set the vertical swimming velocity for the particle
            if (zlevel > ztop) then                                               !If the position of the particle (measured from the bottom) is higher than the maximum position in the water column
                vz = buoy                                                               ! The vertical velocity becomes equal to the bouyancy
            elseif (zlevel < zbot) then                                           !If the position of the particle (measured from the bottom) is lower than the minimum position in the water column
                vz = vzact                                                              ! The vertical velocity is equal to the vertical swimming velocity
            else                                                                       !If the position of the particle is in between the maximum and the minimum position in the water column
                vz = buoy + phase_diurn(istage) * (vzact - buoy)                            ! Based on the time of the day the particle has a downward or upward vertical velocity
            endif
            wsettl(ipart) = vz                                                         ! The settling of the particle becomes equal to the vertical velocity

        case (behaviour_diurnal)                                                    !Behaviour 6
            ztop = ztop1(istage) + fstage * (ztop2(istage) - ztop1(istage))              ! Set the maximum position in the water column for the particle
            zbot = zbot1(istage) + fstage * (zbot2(istage) - zbot1(istage))              ! Set the minimum position in the water column for the particle
            buoy = buoy1(istage) + fstage * (buoy2(istage) - buoy1(istage))              ! Set the bouyancy for the particle
            vzact = vzact1(istage) + fstage * (vzact2(istage) - vzact1(istage))            ! Set the vertical swimming velocity for the particle
            if (zdepth < ztop) then                                               !If the position of the particle (measured from the surface) is higher than the maximum position in the water column
                vz = buoy                                                               ! The vertical velocity becomes equal to the bouyancy
            elseif (zdepth > zbot) then                                           !If the position of the particle (measured from the surface) is lower than the minimum position in the water column
                vz = vzact                                                              ! The vertical velocity is equal to the vertical swimming velocity
            else                                                                       !If the position of the particle is in between the maximum and the minimum position in the water column
                vz = buoy + phase_diurn(istage) * (vzact - buoy)                            ! Based on the time of the day the particle has a downward or upward vertical velocity
            endif
            wsettl(ipart) = vz                                                         ! The settling of the particle becomes equal to the vertical velocity

        case (behaviour_herring)                                                    !Behaviour 7
            wsettl(ipart) = 0.0                                                        ! The settling is set to 0.0

        case (behaviour_stst_dem)                                                   !Behaviour 8

            stick_to_bottom = .true.

            vzact = vzact1(istage) + fstage * (vzact2(istage) - vzact1(istage))            ! Set the vertical swimming velocity for the particle

            ! Vertical positioning based on tide
            !  return: wsettl, kpart, zpart, v_swim and d_swim

            call vert_swimm_tidal (lunrep, ebb_flow, iseg, k, num_layers, &
                    stick_to_bottom, ipart, wsettl, kpart, zpart, &
                    buoy, vzact, v_swim, d_swim)

        case (behaviour_stst_pel)                                                   !Behaviour 9

            stick_to_bottom = .false.

            vzact = vzact1(istage) + fstage * (vzact2(istage) - vzact1(istage))            ! Set the vertical swimming velocity for the particle

            ! Vertical positioning based on tide
            !  return: wsettl, kpart, zpart, v_swim and d_swim

            call vert_swimm_tidal (lunrep, ebb_flow, iseg, k, num_layers, &
                    stick_to_bottom, ipart, wsettl, kpart, zpart, &
                    buoy, vzact, v_swim, d_swim)

        case default                                                                  !Behaviour Default
            write(lunrep, *) ' error, larval behaviour type not defined'                ! Give an error notice that vertical behaviour is not defined
            call stop_exit(1)                                                          ! Stop the calculation
        end select


        !Horizontal swimming

        h_behaviour_type = hbtype(istage)                                                ! Assign the horizontal behaviour type by stage
        select case (h_behaviour_type)                                                 ! Select behaviour by numbering

        case (h_behaviour_none)                                                     !Behaviour 0
            v_swim(ipart) = 0.0                                                        ! Set the horizontal swimming velocity to 0
            d_swim(ipart) = 0.0                                                        ! Set the horizontal swimming direction to 0

        case (h_behaviour_salinity)                                                 !Behaviour 1
            vswim = vswim1(istage) + fstage * (vswim2(istage) - vswim1(istage))            ! Set the horizontal swimming velocity indicator
            v_swim(ipart) = vswim                                                      ! Set the horizontal swimming velocity to the indicator

            ! Assemble salinity values of surrounding gridcells
            !  return: v_swim and d_swim
            call orien_salinity (n, m, num_rows, num_columns, mnmaxk, &
                    lgrid, lgrid2, lgrid3, salin1, v_swim, &
                    d_swim, angle, ipart, xpart, ypart, &
                    a, b, flow, local_angle, lb_sal, &
                    ub_sal, sal_n0, sal_n1, sal_n12, sal_n2, &
                    sal_n23, sal_n3, sal_n34, sal_n4, sal_n41)

        case (h_behaviour_temperature)                                              !Behaviour 2
            vswim = vswim1(istage) + fstage * (vswim2(istage) - vswim1(istage))            ! Set the horizontal swimming velocity indicator
            v_swim(ipart) = vswim                                                      ! Set the horizontal swimming velocity to the indicator

            ! Set direction by lowest temperature value
            !  return: v_swim and d_swim
            call orien_temperature (n, m, num_rows, num_columns, mnmaxk, &
                    lgrid, lgrid2, lgrid3, temper1, v_swim, &
                    d_swim, angle, ipart, xpart, ypart, &
                    a, b, flow, local_angle, lb_temp, &
                    ub_temp, temp_n0, temp_n1, temp_n12, temp_n2, &
                    temp_n23, temp_n3, temp_n34, temp_n4, temp_n41)

        case (h_behaviour_re_orien_sal_temp)                                        !Behaviour 3

            lb_temp = 12                                                               ! Hardcoded lower boundary for temperature
            ub_temp = 25                                                               ! Hardcoded upper boundary for temperature

            vswim = vswim1(istage) + fstage * (vswim2(istage) - vswim1(istage))            ! Set the horizontal swimming velocity indicator
            v_swim(ipart) = vswim                                                      ! Set the horizontal swimming velocity to the indicator

            ! Assemble salinity values of surrounding gridcells
            !  return: sal_n0, sal_n1 , sal_n12 , sal_n2 , sal_n23 , sal_n3 , sal_n34, sal_n4 and sal_n41
            call orien_salinity (n, m, num_rows, num_columns, mnmaxk, &
                    lgrid, lgrid2, lgrid3, salin1, v_swim, &
                    d_swim, angle, ipart, xpart, ypart, &
                    a, b, flow, local_angle, lb_sal, &
                    ub_sal, sal_n0, sal_n1, sal_n12, sal_n2, &
                    sal_n23, sal_n3, sal_n34, sal_n4, sal_n41)

            ! Assemble temperature values of surrounding gridcells
            !  return: temp_n0, temp_n1 , temp_n12 , temp_n2 , temp_n23 , temp_n3 , temp_n34, temp_n4 and temp_n41
            call orien_temperature (n, m, num_rows, num_columns, mnmaxk, &
                    lgrid, lgrid2, lgrid3, temper1, v_swim, &
                    d_swim, angle, ipart, xpart, ypart, &
                    a, b, flow, local_angle, lb_temp, &
                    ub_temp, temp_n0, temp_n1, temp_n12, temp_n2, &
                    temp_n23, temp_n3, temp_n34, temp_n4, temp_n41)

            ! Move closest to lowest salinity based on temperature avoidance
            !  return: v_swim and d_swim
            call re_orien_sal_temp (n, m, num_rows, num_columns, mnmaxk, &
                    lgrid, lgrid2, lgrid3, v_swim, d_swim, &
                    angle, ipart, xpart, ypart, a, &
                    b, flow, local_angle, sal_n0, sal_n1, &
                    sal_n12, sal_n2, sal_n23, sal_n3, sal_n34, &
                    sal_n4, sal_n41, temp_n0, temp_n1, temp_n12, &
                    temp_n2, temp_n23, temp_n3, temp_n34, temp_n4, &
                    temp_n41)

        case default                                                                  !Behaviour Default
            write(lunrep, *) ' error, larval behaviour type not defined'                ! Write error in horizontal behaviour
            call stop_exit(1)                                                          ! Stop ABM calculation
        end select

        return                                                                                 !Return from the subroutine
    end subroutine
end module
