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

module behv_mauve_stinger_mod
    !
    !  data definition module(s)
    !
    use m_stop_exit
    use m_waq_precision          ! single/double precision
    use timers
    !
    !  module procedure(s)
    !
    use vert_swimm_diurnal_mod         ! explicit interface
    use intpltd_diurnal_mod            ! explicit interface
    use intpltd_divelimit_mod          ! explicit interface
    use intpltd_motility_mod           ! explicit interface
    !
    implicit none

contains
    subroutine behv_mauve_stinger (btype, hbtype, v_swim, d_swim, n, &
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

        ! function  : Mauve stinger (Pelagica noctiluca) specific behaviour collected
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
        integer(int_wp) :: idelt               ! timestep in seconds
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
        logical :: daytime             ! true if it is daytime, false in night

        integer :: behaviour_type      ! actual behaviour type

        integer, parameter :: behaviour_none = 0 ! behaviour type none
        integer, parameter :: behaviour_passive = 1 ! behaviour type passive
        integer, parameter :: behaviour_pulsing = 2 ! behaviour type pulsing
        integer, parameter :: behaviour_dead = 3 ! behaviour type dead

        real :: vswim                  ! swimming velocity
        real :: local_angle            ! angle towards lowest salinity in grid

        real :: lb_sal                 ! lower boundary of temperature
        real :: ub_sal                 ! upper boundary of temperature

        integer :: n0_lgrid
        integer :: n0

        real :: sal_n0
        real :: sal_n1
        real :: sal_n12
        real :: sal_n2
        real :: sal_n23
        real :: sal_n3
        real :: sal_n34
        real :: sal_n4
        real :: sal_n41

        real :: lb_temp                ! lower boundary of salinity
        real :: ub_temp                ! upper boundary of salinity

        real :: temp_n0
        real :: temp_n1
        real :: temp_n12
        real :: temp_n2
        real :: temp_n23
        real :: temp_n3
        real :: temp_n34
        real :: temp_n4
        real :: temp_n41

        logical :: stick_to_bottom        ! stick to bottom when reached
        logical :: dive_at_night          ! dive during the night, if untrue dive during the day


        ! MAUVE STINGER BEHAVIOUR TYPES

        istage = wpart(2, ipart)                                                         ! Get current stage of particle

        !Set layer and/or settling velocity according to stage and type of vertical behaviour

        behaviour_type = btype(istage)                                                   ! Assign the vertical behaviour type by stage
        select case (behaviour_type)                                                   ! Select behaviour by numbering

        case (behaviour_none)                                                       !Behaviour 0

            !The particle shows no active behaviour in the vertical and in the horizontal

            !Horizontal behaviour
            v_swim(ipart) = 0.0                                                        ! Set the horizontal swimming velocity to 0
            d_swim(ipart) = 0.0                                                        ! Set the horizontal swimming direction to 0

            !Vertical behaviour
            wsettl(ipart) = 0.0                                                       ! Setteling velocity is set to 0

        case (behaviour_passive)                                                    !Behaviour 1

            !Horizontal behaviour
            v_swim(ipart) = 0.0                                                        ! Set the horizontal swimming velocity to 0
            d_swim(ipart) = 0.0                                                        ! Set the horizontal swimming direction to 0

            !The particle shows no active behaviour in the vertical and in the horizontal,
            ! but is subject to bouyancy and a maximum diving depth.

            buoy = buoy1(istage) + fstage * (buoy2(istage) - buoy1(istage))               ! Set the bouyancy for the particle
            vz = buoy                                                                 ! Set the vertical direction for the particle

            !Check for depth not to exceed maximum depth
            call intpltd_divelimit (lunrep, idelt, ipart, zlevel, zdepth, wsettl)

            wsettl(ipart) = vz

        case (behaviour_pulsing)                                                    !Behaviour 2

            !set dive at night
            stick_to_bottom = .false.
            dive_at_night = .false.

            !Horizontal behaviour
            v_swim(ipart) = 0.0                                                        ! Set the horizontal swimming velocity to 0
            d_swim(ipart) = 0.0                                                        ! Set the horizontal swimming direction to 0

            !Get stage specific input
            buoy = buoy1(istage) + fstage * (buoy2(istage) - buoy1(istage))
            vzact = vzact1(istage) + fstage * (vzact2(istage) - vzact1(istage))
            ztop = ztop1(istage) + fstage * (ztop2(istage) - ztop1(istage))

            ! get temperature
            n0_lgrid = lgrid (n, m)                                                      ! Get the gridnumbering from the active grid in the middle of particle position
            if(n0_lgrid <= 0) return                                                   ! Stop execution if particle has left the model
            n0 = lgrid3(n, m)                                                        ! Get current gridcell
            temp_n0 = temper1(n0)                                                        ! Get current temperature
            if(temp_n0 < -40.0) then                                                  ! check if temperature is available
                write(lunrep, *) 'ERROR no temperature provided, no mauve stinger model activated'
                write(*, *) 'ERROR no temperature provided, no mauve stinger model activated'
                stop
            endif

            !Get the temperature based mobility
            call intpltd_motility (lunrep, n, m, k, nosegl, lgrid, vzact, temper1)

            !Get daytime
            call intpltd_diurnal (lunrep, day, daytime)

            !Get diurnal behaviour
            call vert_swimm_diurnal (lunrep, daytime, k, num_layers, stick_to_bottom, &
                    dive_at_night, ipart, wsettl, kpart, zpart, &
                    buoy, vzact, v_swim, d_swim)


            !Check for depth not to exceed maximum depth
            call intpltd_divelimit (lunrep, idelt, ipart, zlevel, zdepth, wsettl)

            wsettl(ipart) = vz

        case (behaviour_dead)                                                       !Behaviour 3
            !The particle shows no active behaviour in the vertical and in the horizontal

            !Horizontal behaviour
            v_swim(ipart) = 0.0                                                        ! Set the horizontal swimming velocity to 0
            d_swim(ipart) = 0.0                                                        ! Set the horizontal swimming direction to 0

            !Vertical behaviour
            wsettl(ipart) = 0.0                                                       ! Setteling velocity is set to 0

        case default                                                                 !Behaviour Default

            write(lunrep, *) ' error, larval behaviour type not defined'               ! Give an error notice that vertical behaviour is not defined
            call stop_exit(1)                                                         ! Stop the calculation

        end select

        return                                                                                                         !Return from the subroutine
    end subroutine
end module
