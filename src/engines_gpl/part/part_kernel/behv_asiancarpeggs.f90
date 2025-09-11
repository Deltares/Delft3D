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

module behv_asiancarpeggs_mod
    !
    !  data definition module(s)
    !
    use m_stop_exit
    use m_waq_precision          ! single/double precision
    use timers
    !
    !  module procedure(s)
    !
    use orien_bathymetry_mod           ! explicit interface
    use vert_swimm_tidal_mod           ! explicit interface
    use vert_swimm_diurnal_mod         ! explicit interface
    use intpltd_diurnal_mod            ! explicit interface
    use random_generator
    !
    implicit none

contains
    subroutine behv_asiancarpeggs (btype, hbtype, v_swim, d_swim, n, &
            m, num_rows, num_columns, mnmaxk, lgrid, &
            lgrid2, lgrid3, nosegl, wpart, ipart, &
            wsettl, k, kpart, zpart, xpart, &
            ypart, num_layers, &
            ktopp, kbotp, idelt, day, phase_diurn, &
            ebb_flow, flow, depth, vdiff1, salin1, &
            temper1, vol1, vol2, vel1, vel2, &
            fstage, ztop, zlevel, zdepth, zbot, &
            buoy, vzact, vz, buoy1, buoy2, &
            ztop1, ztop2, zbot1, zbot2, vzact1, &
            vzact2, vswim1, vswim2, iseg, lunrep, &
            angle, factor_alpha, factor_beta, factor_delta, &
            factor_eta, factor_gamma, factor_phi)


        ! function  : Asian carp egg (Bighead Carp :: Hypophthalmichthys nobilis, Silver carp :: H. molitrix, 
        ! Grass Carp :: Ctenopharyngodon idella) specific behaviour collected.
        ! This behaviorial model is based on FluEgg (USGS) with publications used:
        ! George, A.E. Garcia, T. & Chapman, D.C., 2017. Comparison of Size, Terminal Fall Velocity, and Density of
        !    Bighead Carp, Silver Carp, and Grass Carp Eggs for Use in Drift Modelling. Trans. of the Americ. Fish. Soc. 
        !    146:834-843
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
        real   (sp), intent(in) :: wpart (:, :)      ! weight factors of the subs per particle

        real   (sp), pointer :: wsettl(:)         ! settling per particle  (m/day)

        real   (sp), pointer :: angle (:)         ! angle with horizontal
        real   (sp), pointer :: depth(:)          ! total depth of the cells from top to bottom
        real   (sp), pointer :: vol1  (:)         ! volume begin hydr step
        real   (sp), pointer :: vol2  (:)         ! volume end hydr step
        real   (sp), pointer :: flow  (:)         ! all flows
        real   (sp), pointer :: vdiff1(:)         ! vert. diffusion segment numbering
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
        real   (sp) :: vz                  ! vz (m/s)
        real   (sp) :: vzact               ! vzact (m/s)
        real   (sp) :: vzdiffuse           ! vzdiffuse (m2/s)
        real   (sp) :: buoy                ! buoy
        integer :: k                   ! k
        integer :: ktopp               ! ktopp
        integer :: kbotp               ! kbotp
        real   (sp) :: ztop                ! ztop
        real   (sp) :: zbot                ! zbot
        integer :: m                   ! m
        integer :: n                   ! n

        real   (sp) :: zdepth              ! z relative to water surface
        real   (sp) :: zlevel              ! z relative to bottom
        logical, pointer :: ebb_flow(:)       ! true if flow is ebb
        logical :: daytime             ! true if it is daytime, false in night

        integer :: behaviour_type      ! actual behaviour type

        integer, parameter :: behaviour_none = 0  ! behaviour type none
        integer, parameter :: behaviour_egg = 1  ! behaviour type egg
        integer, parameter :: behaviour_larval = 2  ! behaviour type larval before Gas bladder inflation
        integer, parameter :: behaviour_larval_GBI = 3  ! behaviour type larval with Gas bladder inflation

        real :: vswim                  ! swimming velocity
        real :: local_angle            ! angle towards lowest salinity in grid

        integer :: n0_lgrid
        integer :: n0

        real :: lb_sal                 ! lower boundary of salinity
        real :: ub_sal                 ! upper boundary of salinity

        real :: sal_n0
        real :: sal_n1
        real :: sal_n12
        real :: sal_n2
        real :: sal_n23
        real :: sal_n3
        real :: sal_n34
        real :: sal_n4
        real :: sal_n41

        real :: lb_temp                ! lower boundary of temperature
        real :: ub_temp                ! upper boundary of temperature

        real :: temp_n0
        real :: temp_n1
        real :: temp_n12
        real :: temp_n2
        real :: temp_n23
        real :: temp_n3
        real :: temp_n34
        real :: temp_n4
        real :: temp_n41

        real :: lb_bath                ! lower boundary of bathymetry
        real :: ub_bath                ! upper boundary of bathymetry

        real :: bath_n0
        real :: bath_n1
        real :: bath_n12
        real :: bath_n2
        real :: bath_n23
        real :: bath_n3
        real :: bath_n34
        real :: bath_n4
        real :: bath_n41

        real :: verdiff_n0

        logical :: stick_to_bottom        ! stick to bottom when reached
        logical :: dive_at_night          ! dive during the night, if untrue dive during the day

        real (sp) :: stime                  ! Time spent in stage (d)
        integer(int_wp) :: stime_sec              ! Time spent in stage (s)
        real (sp) :: stemp                  ! Average temperature experienced in stage (degrees Celsius)
        real (sp) :: egg_dens_std           ! Egg denisity under 22 degrees Celsius
        real (sp) :: egg_dens               ! Egg denisity under current temperature (kg/m3)
        real (sp) :: egg_diam_mm            ! Egg diameter (mm)
        real (sp) :: egg_diam_cm            ! Egg diameter (cm)
        real (sp) :: water_dens             ! Density of riverine water (kg/m3)
        real (sp) :: grav                   ! Acceleration of gravity(cm2/s2)
        real (sp) :: sg                     ! Specific gravity of the egg (cm2/s2)
        real (sp) :: dirfactor              ! direction specifying factor for sg > or < 1
        real (sp) :: kinvisc1               ! part of Kinematic viscoisity equation
        real (sp) :: kinvisc2               ! part of Kinematic viscoisity equation
        real (sp) :: kinvisc                ! Kinematic viscoisity of water (cm2/s)
        real (sp) :: reynparnr              ! Reynolds particle number (unitless)
        real (sp) :: rf                     ! Dimensionless form of terminal fall velocity (cm/s)
        real (sp), save :: factor_b1              ! factor for dimensionless term. fall velocity (unitless)
        real (sp), save :: factor_b2              ! factor for dimensionless term. fall velocity (unitless)
        real (sp), save :: factor_b3              ! factor for dimensionless term. fall velocity (unitless)
        real (sp), save :: factor_b4              ! factor for dimensionless term. fall velocity (unitless)
        real (sp), save :: factor_b5              ! factor for dimensionless term. fall velocity (unitless)
        real (sp), intent(in) :: factor_alpha           ! derived factor to predict species specific egg diameter at 22 degrees Celsius (unitless)
        real (sp), intent(in) :: factor_beta            ! derived factor to predict species specific egg diameter at 22 degrees Celsius (unitless)
        real (sp), intent(in) :: factor_delta           ! derived factor to predict species specific egg density at 22 degrees Celsius (unitless)
        real (sp), intent(in) :: factor_eta             ! derived factor to predict species specific egg density corrected for specific temperature (unitless)
        real (sp), intent(in) :: factor_gamma           ! derived factor to predict species specific egg density at 22 degrees Celsius (unitless)
        real (sp), intent(in) :: factor_phi             ! derived factor to predict species specific egg density at 22 degrees Celsius (unitless)

        real (dp) :: rseed1 = 0.5d0
        real (dp) :: rseed2 = 0.5d0
        real (sp) :: rdmnr1
        real (sp) :: rdmnr2


        ! ASIAN CARP EGG BEHAVIOUR TYPES

        ! set initials

        !water_dens = 998.0                                                              ! Set density of river water (kg/m3)
        grav = 980.665                                                             ! Set gravity (cm2/s2)
        factor_b1 = 2.891394                                                           ! set terminal fall velocity parameter b1
        factor_b2 = 0.95296                                                             ! set terminal fall velocity parameter b2
        factor_b3 = 0.056835                                                            ! set terminal fall velocity parameter b3
        factor_b4 = 0.002892                                                            ! set terminal fall velocity parameter b4
        factor_b5 = 0.000245                                                            ! set terminal fall velocity parameter b5

        ! read stage
        istage = wpart(2, ipart)                                                         ! Get current stage of particle

        !Set layer and/or settling velocity according to stage and type of vertical behaviour
        if(istage > 0) then
            behaviour_type = btype(istage)                                                   ! Assign the vertical behaviour type by stage
        else
            behaviour_type = 0                                                               ! give end of simulation behavior
        endif

        select case (behaviour_type)                                                   ! Select behaviour by numbering

        case (behaviour_none)                                                       !Behaviour 0

            !The particle shows no active behaviour in the vertical and in the horizontal and is placed in stor

            !Horizontal behaviour
            v_swim(ipart) = 0.0                                                        ! Set the horizontal swimming velocity to 0
            d_swim(ipart) = 0.0                                                        ! Set the horizontal swimming direction to 0

            ! settle on bed if arrived in lowest layer
            kpart(ipart) = kbotp                                                   ! Particle is placed in the storage layer
            zpart(ipart) = 0.5                                                         ! Particle is positioned in the middle of the cell in the third dimension

            !Vertical behaviour
            vzact = 0.0
            vz = 0.0
            wsettl(ipart) = 0.0                                                       ! Setteling velocity is set to 0

        case (behaviour_egg)                                                    !Behaviour 1

            stime = wpart(3, ipart)                                                     ! Get time spent in stage of particle
            stemp = wpart(4, ipart)                                                     ! Get avr. temp experienced in stage of particle

            stime_sec = stime * 86400

            ! get temperature
            n0_lgrid = lgrid (n, m)                                                      ! Get the gridnumbering from the active grid in the middle of particle position
            if(n0_lgrid <= 0) return                                                   ! Stop execution if particle has left the model
            n0 = lgrid3(n, m)                                                    ! Get current gridcell
            temp_n0 = temper1(n0 + (k - 1) * nosegl)                                     ! Get current temperature
            verdiff_n0 = vdiff1(n0 + (k - 1) * nosegl)                                      ! Get current verticle diffusivity

            if(temp_n0 < -40.0) then                                             ! check if temperature is available
                write(lunrep, *) 'ERROR no temperature provided, no asian carp egg model activated'
                write(*, *) 'ERROR no temperature provided, no asian carp egg model activated'
                stop
            endif

            if(stime /= 0) then

                ! Egg development
                egg_dens_std = factor_delta * exp(-1 * (stime_sec / factor_gamma)) + factor_phi ! calculates the egg density based on carp specific factors for 22 degrees Celsius
                egg_diam_mm = factor_alpha * (1 - exp (-1 * (stime_sec / factor_beta)))      ! calculates the egg diameter based on carp specific factors
                egg_dens = egg_dens_std + factor_eta * (22. - stemp)                         ! calculates the egg density based on carp specific factors for experienced temperature
                egg_diam_cm = egg_diam_mm / 10                                                    ! translate egg diameter from mm to cm

                !Horizontal behaviour
                v_swim(ipart) = 0.0                                                        ! Set the horizontal swimming velocity to 0
                d_swim(ipart) = 0.0                                                        ! Set the horizontal swimming direction to 0

                !The particle shows no active behaviour in the vertical and in the horizontal,
                ! but is subject to bouyancy and settling. These are calculate from the state of the egg.
                water_dens = 1000. / (((temp_n0 - 20) * 0.0002) + 1)
                sg = egg_dens / water_dens                                           ! calculate the specific gravity of the egg

                dirfactor = 1.                                                                 ! the original formula can not handle eggs lighter than water
                if(sg < 1.) dirfactor = -1.                                            ! by making ABS(sg - 1) and applying a factor on the direction this
                ! is enabled.

                kinvisc1 = (1.79E-6)                                                    ! calculate the kinematic viscoisty of water
                kinvisc2 = (1 + 0.03368 * temp_n0 + 0.00021 * temp_n0 ** 2)
                kinvisc = kinvisc1 / kinvisc2
                reynparnr = (sqrt((grav * (ABS(sg - 1.)) * egg_diam_cm)) * egg_diam_cm) / kinvisc  ! calculate the Reynolds particle number
                rf = exp(-1 * factor_b1 + factor_b2 * log(reynparnr) &               ! calculate the terminal fall velocity
                        - factor_b3 * log(reynparnr) ** 2               &
                        + -1 * factor_b4 * log(reynparnr) ** 3          &
                        + factor_b5 * (log(reynparnr)) ** 4)

                vzact = rf * sqrt(grav * (ABS(sg - 1.)) * egg_diam_cm)                ! calculate the fall velocity of the egg (cm/s)
                vzact = (vzact * dirfactor) / 100.                                       ! correct and calculate the fall velocity of the egg (m/s)

                rdmnr1 = (rnd(rseed1) * 2.) - 1.                                         ! calculate a random number between -1 and 1
                vzdiffuse = rdmnr1 * sqrt(2 * verdiff_n0 * idelt)                            ! calculate the vertical diffusion effect (m/idelt)
                vzdiffuse = vzdiffuse / idelt                                                ! calculate the vertical diffusion effect (m/s)

                vz = vzact + vzdiffuse

            else

                vzact = 0.0
                vz = 0.0

            endif

            wsettl(ipart) = vz

        case (behaviour_larval)                                                     !Behaviour 1

            ! the larval without gas bladder inflation behaviour is passive in the horizontal.
            ! in the vertical it can determine its position and prevent reaching the bottom.
            ! At this point larvae will remain neutally buoyant in the waterbody
            ! The particle shows no active behaviour in the horizontal

            zbot = zbot1(istage) + fstage * (zbot2(istage) - zbot1(istage))              ! Set the minimum position in the water column for the particle
            buoy = buoy1(istage) + fstage * (buoy2(istage) - buoy1(istage))              ! Set the bouyancy for the particle
            vzact = vzact1(istage) + fstage * (vzact2(istage) - vzact1(istage))            ! Set the vertical swimming velocity for the particle

            n0_lgrid = lgrid (n, m)                                                    ! Get the gridnumbering from the active grid in the middle of particle position
            if(n0_lgrid <= 0) return                                                 ! Stop execution if particle has left the model
            n0 = lgrid3(n, m)                                                  ! Get current gridcell
            verdiff_n0 = vdiff1(n0 + (k - 1) * nosegl)                                    ! Get current verticle diffusivity

            if (zlevel < zbot) then                                           !If the position of the particle (measured from the bottom) is lower than the minimum position in the water column
                vz = vzact                                                             ! The vertical velocity is equal to the vertical swimming velocity
            else                                                                   !If the position of the particle is in between the maximum and the minimum position in the water column
                rdmnr1 = (rnd(rseed1) * 2.) - 1.                                ! calculate a random number between -1 and 1
                vzdiffuse = rdmnr1 * sqrt(2 * verdiff_n0 * idelt)                   ! calculate the vertical diffusion effect (m/idelt)
                vzdiffuse = vzdiffuse / idelt                                       ! calculate the vertical diffusion effect (m/s)

                rdmnr2 = rnd(rseed2)                                             ! calculate a random number between 0 and 1
                vz = buoy + vzact * rdmnr2 + vzdiffuse

            endif
            wsettl(ipart) = vz                                                         ! The settling of the particle becomes equal to the vertical velocity (m/s)

        case (behaviour_larval_GBI)                                                       !Behaviour 3

            ! the larval with gas bladder inflation behaviour is added for the end of simulation
            ! At this point larvae will search for suitable nursary habitat along the waterbody
            ! The particle shows no active behaviour in the vertical and in the horizontal
            ! The particle is placed in the storage layer to maintain position

            !Horizontal behaviour
            v_swim(ipart) = 0.0                                                        ! Set the horizontal swimming velocity to 0
            d_swim(ipart) = 0.0                                                        ! Set the horizontal swimming direction to 0

            ! settle on bed if arrived in lowest layer
            kpart(ipart) = kbotp                                                   ! Particle is placed in the storage layer
            zpart(ipart) = 0.5                                                         ! Particle is positioned in the middle of the cell in the third dimension

            !Vertical behaviour
            vzact = 0.0
            vz = 0.0
            wsettl(ipart) = 0.0                                                       ! Setteling velocity is set to 0

        case default                                                                 !Behaviour Default

            write(lunrep, *) ' error, larval behaviour type not defined'               ! Give an error notice that vertical behaviour is not defined
            call stop_exit(1)                                                         ! Stop the calculation

        end select

        return                                                                                                         !Return from the subroutine
    end subroutine
end module



