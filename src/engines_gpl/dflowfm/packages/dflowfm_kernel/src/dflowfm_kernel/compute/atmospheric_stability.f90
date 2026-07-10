module m_atmospheric_stability
!! This module computes bulk air-sea exchange fluxes (wind stress, latent and sensible heat) using Monin-Obukhov 
!! Similarity Theory.
!! 
!! The algorithm iteratively solves for the turbulence scaling parameters (u*, t*, q*)
!! by updating roughness lengths and stability corrections (psi functions) until convergence. 
!! From these, surface fluxes are derived via:
!!
!!   tau   = rho_air * u_star^2 = rho_air * c_d * |U| * U
!!   Qh    = rho_air * Cp * u_star * t_star = rho_air * c_h * C_P * (T_air - T_surface) * U
!!   Qe    = rho_air * Lv * u_star * q_star = rho_air * L_V * c_e * (q_air - q_surface) * U
!!
!! Inputs required:
!!   - Wind velocity components (u, v)
!!   - Air temperature
!!   - Dew-point temperature 
!!   - Air pressure
!!   - Sea surface temperature
!!   - Charnock parameter 
   use precision, only: dp
   use m_sferic, only: pi
   use m_physcoef, only: vonkarw
   use m_flowparameters, only: EPS8
   
   implicit none(type, external)
   
   private

   public :: t_options
   public :: t_scales
   public :: t_fluxes
   public :: compute_scales_and_fluxes
   public :: get_wind_stress
   public :: get_latent_heat_flux
   public :: get_sensible_heat_flux
   public :: get_u_star
   public :: get_t_star
   public :: get_q_star
   public :: get_z0_momentum
   public :: get_z0_heat
   public :: get_z0_humidity
   public :: get_obukhov_length
   public :: get_richardson_number
   public :: get_bulk_exchange_diagnostics
   public :: get_w_star
   public :: get_transfer_coeff_momentum
   public :: get_transfer_coeff_sensible_heat
   public :: get_transfer_coeff_latent_heat
   
   real(kind=dp), parameter :: CONST_GRAVITY = 9.80665_dp ! gravitational acceleration [m/s2]
   real(kind=dp), parameter :: CONST_R = 8.31451_dp ! universal gas constant [J/(mol K)]
   real(kind=dp), parameter :: CONST_Md = 28.9644e-3_dp ! dry air molar mass [kg/mol]
   real(kind=dp), parameter :: CONST_Mv = 18.0153e-3_dp ! water vapor molar mass [kg/mol]
   real(kind=dp), parameter :: CONST_Lv = 2.5008e6_dp ! specific heat of vaporization [J/kg]
   real(kind=dp), parameter :: CONST_NU_AIR = 1.5e-5_dp ! kinematic viscosity of air [m2/s]
   real(kind=dp), parameter :: CONST_Rd = CONST_R / CONST_Md ! gas constant for dry air [J/(kg K)] ~ 287.05_dp
   real(kind=dp), parameter :: CONST_Rv = CONST_R / CONST_Mv ! gas constant for water vapor [J/(kg K)] ~ 461.5249_dp
   real(kind=dp), parameter :: CONST_EST = (CONST_Rv/CONST_Rd) - 1.0_dp ! ratio of gas constants for water vapor and dry air minus one [-]
   real(kind=dp), parameter :: CONST_Cpd = 7/2 * CONST_Rd ! specific heat capacity at constant pressure for dry air [J/(kg K)]
   real(kind=dp), parameter :: CONST_E0 = 611.21_dp ! water vapour saturation pressure over water (Pa) at triple point temperature (Tt) [Pa]
   real(kind=dp), parameter :: CONST_TT = 273.16_dp ! triple point temperature [K]
   real(kind=dp), parameter :: OBUKHOV_LENGTH_LIMIT = 1.0e4_dp ! limit of Obukhov length to prevent numerical issues [m]
   
   !> Optional switches controlling physical parameterizations.
   !! Enable terms only when they are needed for the target conditions.
   type :: t_options
      logical :: include_free_convection = .false.     !< Use in weak-wind unstable conditions (buoyancy-driven turbulence matters); usually unnecessary in moderate/strong wind or neutral/stable cases.
      logical :: include_stability = .true.            !< Use for stability-aware similarity corrections; disable for neutral conditions.
      real(kind=dp) :: fqsat = 1.0_dp                  !< Salinity reducing factor of saturation humidity.
      real(kind=dp) :: height_wind_velocity = 10.0_dp  !< Height of prescribed wind velocity [m]
      real(kind=dp) :: height_humidity = 2.0_dp        !< Height of prescribed humidity [m]
      real(kind=dp) :: height_air_temperature = 2.0_dp !< Height of prescribed air temperature [m]
   end type t_options

   !> Scaling parameters data type.
   type :: t_scales
      real(kind=dp) :: u_star = 0.0_dp                 !< Friction velocity [m/s].
      real(kind=dp) :: t_star = 0.0_dp                 !< Temperature scale [K].
      real(kind=dp) :: q_star = 0.0_dp                 !< Humidity scale [kg/kg].
      real(kind=dp) :: w_star = 0.0_dp                 !< Free convective velocity scale [m/s].
      real(kind=dp) :: obukhov_length = 0.0_dp         !< Obukhov length [m].
      real(kind=dp) :: richardson_number = 0.0_dp      !< Bulk Richardson number [-].
      real(kind=dp) :: z0_momentum = 0.0_dp            !< Momentum roughness [m].
      real(kind=dp) :: z0_heat = 0.0_dp                !< Heat roughness [m].
      real(kind=dp) :: z0_humidity = 0.0_dp            !< Humidity roughness [m].
      real(kind=dp) :: c_d = 0.0_dp                    !< Bulk transfer coefficient of momentum flux [-]
      real(kind=dp) :: c_h = 0.0_dp                    !< Bulk transfer coefficient of sensible heat flux [-]
      real(kind=dp) :: c_e = 0.0_dp                    !< Bulk transfer coefficient of latent heat flux [-]
   end type t_scales

   !> Fluxes data type.
   type :: t_fluxes
      real(kind=dp) :: wind_stress_x = 0.0_dp
      real(kind=dp) :: wind_stress_y = 0.0_dp
      real(kind=dp) :: latent_heat_flux = 0.0_dp
      real(kind=dp) :: sensible_heat_flux = 0.0_dp
   end type t_fluxes

   type(t_scales), dimension(:), allocatable :: scaling_parameters !< Array of scaling parameters
   type(t_fluxes), dimension(:), allocatable :: fluxes !< Array of fluxes

contains

   !> Compute turbulence scaling parameters for a point value.
   pure function compute_scaling_parameters(wind_velocity_x, wind_velocity_y, air_temperature, dew_point_temperature, &
                                  air_pressure, charnock, surface_temperature, options) result(result)
      real(kind=dp), intent(in) :: wind_velocity_x         !< x-direction wind component [m/s].
      real(kind=dp), intent(in) :: wind_velocity_y         !< y-direction wind component [m/s].
      real(kind=dp), intent(in) :: air_temperature         !< Air temperature [K].
      real(kind=dp), intent(in) :: dew_point_temperature   !< Dew-point temperature [K].
      real(kind=dp), intent(in) :: air_pressure            !< Air pressure [Pa].
      real(kind=dp), intent(in) :: charnock                !< Charnock parameter [-].
      real(kind=dp), intent(in) :: surface_temperature     !< Surface temperature [K].
      type(t_options), intent(in) :: options     !< Optional model switches.
      type(t_scales) :: result                !< Computed scaling outputs.

      real(kind=dp) :: salt_saturation_humidity_reduction_factor
      real(kind=dp) :: wind_velocity_magnitude
      real(kind=dp) :: vapor_pressure, saturated_vapor_pressure
      real(kind=dp) :: delta_wind_speed
      real(kind=dp) :: delta_temperature
      real(kind=dp) :: delta_specific_humidity
      real(kind=dp) :: surface_humidity
      real(kind=dp) :: air_humidity
      real(kind=dp) :: humidity_at_wind_height
      real(kind=dp) :: temperature_at_wind_height
      real(kind=dp) :: temperature_pfactor, humidity_pfactor
      real(kind=dp) :: psi_momentum, psi_heat, psi_humidity
      real(kind=dp) :: obukhov_length, richardson_number
      real(kind=dp) :: z0_momentum, z0_heat, z0_humidity
      real(kind=dp) :: u_star, t_star, q_star
      real(kind=dp) :: convergence_error
      real(kind=dp) :: log_denominator_momentum, log_denominator_heat, log_denominator_humidity
      real(kind=dp) :: heat_profile_correction, humidity_profile_correction
      real(kind=dp) :: convective_velocity_scale
      real(kind=dp) :: inverse_obukhov_length
      real(kind=dp) :: denom_d, denom_h, denom_e
      integer :: iteration
      integer, parameter :: MINIMUM_ITERATION = 5
      integer, parameter :: MAXIMUM_ITERATION = 50
      real(kind=dp), parameter :: EPSILON = 1.0e-4_dp

      real(kind=dp) :: height_wind_velocity
      real(kind=dp) :: height_humidity
      real(kind=dp) :: height_air_temperature
      
      height_wind_velocity = options%height_wind_velocity
      height_humidity = options%height_humidity
      height_air_temperature = options%height_air_temperature

      wind_velocity_magnitude = sqrt(wind_velocity_x**2 + wind_velocity_y**2)
      vapor_pressure = compute_saturation_pressure(dew_point_temperature)
      air_humidity = compute_specific_humidity(vapor_pressure, air_pressure)
      salt_saturation_humidity_reduction_factor = options%fqsat
      saturated_vapor_pressure = compute_saturation_pressure(surface_temperature)
      surface_humidity = compute_specific_humidity(saturated_vapor_pressure, air_pressure)
      surface_humidity = salt_saturation_humidity_reduction_factor * surface_humidity
      
      ! Convert temperature and specific humidity to equivalent values at wind sensor height using logarithmic profiles.
      temperature_pfactor = 0.04_dp * log(height_wind_velocity / height_air_temperature) / vonkarw
      temperature_at_wind_height = (air_temperature - temperature_pfactor*surface_temperature) / (1.0_dp - temperature_pfactor)
      humidity_pfactor = 0.04_dp * log(height_wind_velocity / height_humidity) / vonkarw
      humidity_at_wind_height = (air_humidity - humidity_pfactor*surface_humidity) / (1.0_dp - humidity_pfactor)

      if (options%include_free_convection) then
         convective_velocity_scale = 0.5_dp
         delta_wind_speed = sqrt(wind_velocity_magnitude**2 + convective_velocity_scale**2)
      else
         convective_velocity_scale = 0.0_dp
         delta_wind_speed = wind_velocity_magnitude
      end if

      delta_temperature = temperature_at_wind_height - surface_temperature
      delta_specific_humidity = humidity_at_wind_height - surface_humidity

      ! Initialize scaling parameters.
      u_star = 0.04_dp * delta_wind_speed
      t_star = 0.04_dp * delta_temperature
      q_star = 0.04_dp * delta_specific_humidity
      psi_momentum = 0.0_dp
      psi_heat = 0.0_dp
      psi_humidity = 0.0_dp
      obukhov_length = 0.0_dp
      richardson_number = 0.0_dp
      
      do iteration = 1, MAXIMUM_ITERATION
         call compute_roughness_lengths(u_star, charnock, z0_momentum, z0_heat, z0_humidity)

         if (options%include_free_convection) then
            convective_velocity_scale = compute_convective_velocity_scale(u_star, t_star, q_star, &
                                                        temperature_at_wind_height, humidity_at_wind_height)
            delta_wind_speed = sqrt(wind_velocity_magnitude**2 + convective_velocity_scale**2)
         end if

         if (options%include_stability) then
             richardson_number = compute_richardson_number(delta_wind_speed, surface_temperature, &
                              temperature_at_wind_height, air_pressure, humidity_at_wind_height, &
                              salt_saturation_humidity_reduction_factor, height_wind_velocity)

             log_denominator_momentum = log(height_wind_velocity) - log(z0_momentum) - psi_momentum
             log_denominator_momentum = sign(max(abs(log_denominator_momentum), SMALL_NUMBER), log_denominator_momentum)
             log_denominator_heat = log(height_wind_velocity) - log(z0_heat) - psi_heat

             obukhov_length = (height_wind_velocity / richardson_number) * log_denominator_heat / log_denominator_momentum**2
             obukhov_length = sign(min(abs(obukhov_length), OBUKHOV_LENGTH_LIMIT), obukhov_length)
             inverse_obukhov_length = 1.0_dp / obukhov_length

             psi_momentum = stability_profile_momentum((height_wind_velocity + z0_momentum) * inverse_obukhov_length) - &
                            stability_profile_momentum(z0_momentum * inverse_obukhov_length)
             psi_heat = stability_profile_heat_humidity((height_wind_velocity + z0_momentum) * inverse_obukhov_length) - &
                        stability_profile_heat_humidity(z0_heat * inverse_obukhov_length)
             psi_humidity = stability_profile_heat_humidity((height_wind_velocity + z0_momentum) * inverse_obukhov_length) - &
                            stability_profile_heat_humidity(z0_humidity * inverse_obukhov_length)

             if (abs(height_wind_velocity - height_air_temperature) > 0.01_dp) then
                heat_profile_correction = log(height_wind_velocity) - log(height_air_temperature) &
                     - stability_profile_heat_humidity((height_wind_velocity + z0_momentum) * inverse_obukhov_length) &
                     + stability_profile_heat_humidity((height_air_temperature + z0_momentum) * inverse_obukhov_length)
                temperature_at_wind_height = air_temperature + (t_star / vonkarw) * heat_profile_correction
                delta_temperature = temperature_at_wind_height - surface_temperature
             end if

             if (abs(height_wind_velocity - height_humidity) > 0.01_dp) then
                humidity_profile_correction = log(height_wind_velocity) - log(height_humidity) &
                     - stability_profile_heat_humidity((height_wind_velocity + z0_momentum) * inverse_obukhov_length) &
                     + stability_profile_heat_humidity((height_humidity + z0_momentum) * inverse_obukhov_length)
                humidity_at_wind_height = air_humidity + (q_star / vonkarw) * humidity_profile_correction
                delta_specific_humidity = humidity_at_wind_height - surface_humidity
             end if
             
            log_denominator_momentum = log(height_wind_velocity) - log(z0_momentum) - psi_momentum
            log_denominator_heat = log(height_wind_velocity) - log(z0_heat) - psi_heat
            log_denominator_humidity = log(height_wind_velocity) - log(z0_humidity) - psi_humidity
         else
            log_denominator_momentum = log(height_wind_velocity) - log(z0_momentum)
            log_denominator_heat = log(height_air_temperature) - log(z0_heat)
            log_denominator_humidity = log(height_humidity) - log(z0_humidity)
         end if
         
         log_denominator_momentum = sign(max(abs(log_denominator_momentum), EPS8), log_denominator_momentum)
         log_denominator_heat = sign(max(abs(log_denominator_heat), EPS8), log_denominator_heat)
         log_denominator_humidity = sign(max(abs(log_denominator_humidity), EPS8), log_denominator_humidity)

         result%u_star = vonkarw * delta_wind_speed / log_denominator_momentum
         result%t_star = vonkarw * delta_temperature / log_denominator_heat
         result%q_star = vonkarw * delta_specific_humidity / log_denominator_humidity

         convergence_error = sqrt(((result%u_star - u_star)/max(abs(u_star), EPS8))**2 + &
                             ((result%t_star - t_star)/max(abs(t_star), EPS8))**2 + &
                             ((result%q_star - q_star)/max(abs(q_star), EPS8))**2)

         u_star = result%u_star
         t_star = result%t_star
         q_star = result%q_star

         if (iteration >= MINIMUM_ITERATION .and. convergence_error <= EPSILON) then
            exit
         end if
      end do

      result%u_star = u_star
      result%t_star = t_star
      result%q_star = q_star
      result%w_star = convective_velocity_scale
      result%obukhov_length = obukhov_length
      result%richardson_number = richardson_number
      result%z0_momentum = z0_momentum
      result%z0_heat = z0_heat
      result%z0_humidity = z0_humidity
      
      denom_d = max(wind_velocity_magnitude, EPS8)
      denom_h = max(abs(delta_temperature), EPS8)
      denom_e = max(abs(delta_specific_humidity), EPS8)
      result%c_d = (u_star / denom_d)**2
      result%c_h = abs((u_star / denom_d)*(t_star / denom_h))
      result%c_e = abs((u_star / denom_d)*(q_star / denom_e))
   end function compute_scaling_parameters

   !> Compute arrays of scaling parameters and bulk surface fluxes.
   !! This routine gives no return values. It fills the module arrays with proper values.
   subroutine compute_scales_and_fluxes(wind_velocity_x, wind_velocity_y, air_temperature, dew_point_temperature, &
                             air_pressure, charnock, surface_temperature, options)
      real(kind=dp), intent(in) :: wind_velocity_x(:)        !< x-direction wind component [m/s].
      real(kind=dp), intent(in) :: wind_velocity_y(:)        !< y-direction wind component [m/s].
      real(kind=dp), intent(in) :: air_temperature(:)        !< Air temperature [K].
      real(kind=dp), intent(in) :: dew_point_temperature(:)  !< Dew-point temperature [K].
      real(kind=dp), intent(in) :: air_pressure(:)           !< Air pressure [Pa].
      real(kind=dp), intent(in) :: charnock(:)               !< Charnock parameter [-].
      real(kind=dp), intent(in) :: surface_temperature(:)    !< Surface temperature [K].
      type(t_options), intent(in) :: options                 !< Process options

      type(t_scales) :: scaling_parameter
      type(t_fluxes) :: flux
      real(kind=dp) :: vapor_pressure
      real(kind=dp) :: air_density
      real(kind=dp) :: wind_stress_magnitude
      real(kind=dp) :: wind_velocity_magnitude
      integer :: index
      integer :: number_of_elements

      number_of_elements = size(wind_velocity_x)

      if (size(wind_velocity_y) /= number_of_elements .or. size(air_temperature) /= number_of_elements .or. &
          size(dew_point_temperature) /= number_of_elements .or. size(air_pressure) /= number_of_elements .or. &
          size(charnock) /= number_of_elements .or. size(surface_temperature) /= number_of_elements) then
         error stop 'compute_scales_and_fluxes: all input arrays must have the same size.'
      end if

      if (allocated(scaling_parameters)) deallocate(scaling_parameters)
      if (allocated(fluxes)) deallocate(fluxes)
      allocate(scaling_parameters(number_of_elements))
      allocate(fluxes(number_of_elements))

      do index = 1, number_of_elements
         scaling_parameter = compute_scaling_parameters(wind_velocity_x(index), wind_velocity_y(index), air_temperature(index), &
                                         dew_point_temperature(index), air_pressure(index), charnock(index), &
                                         surface_temperature(index), options)

         vapor_pressure = compute_saturation_pressure(dew_point_temperature(index))
         air_density = compute_air_density(air_temperature(index), air_pressure(index), vapor_pressure)
         wind_stress_magnitude = air_density * scaling_parameter%u_star**2
         wind_velocity_magnitude = sqrt(wind_velocity_x(index)**2 + wind_velocity_y(index)**2)

         if (wind_velocity_magnitude > 1.0e-12_dp) then
            flux%wind_stress_x = wind_stress_magnitude * wind_velocity_x(index) / wind_velocity_magnitude
            flux%wind_stress_y = wind_stress_magnitude * wind_velocity_y(index) / wind_velocity_magnitude
         else
            flux%wind_stress_x = 0.0_dp
            flux%wind_stress_y = 0.0_dp
         end if
         flux%latent_heat_flux = air_density * CONST_Lv * scaling_parameter%u_star * scaling_parameter%q_star
         flux%sensible_heat_flux = air_density * CONST_Cpd * scaling_parameter%u_star * scaling_parameter%t_star

         scaling_parameters(index) = scaling_parameter
         fluxes(index) = flux
      end do
   end subroutine compute_scales_and_fluxes

   !> Return wind-stress component arrays from module-stored fluxes.
   subroutine get_wind_stress(wind_stress_x, wind_stress_y)
      real(kind=dp), allocatable, dimension(:), intent(out) :: wind_stress_x !< Wind-stress x-component [N/m^2].
      real(kind=dp), allocatable, dimension(:), intent(out) :: wind_stress_y !< Wind-stress y-component [N/m^2].
      integer :: index
      integer :: number_of_elements

      if (.not. allocated(fluxes)) then
         error stop 'get_wind_stress: module fluxes are not available. Call compute_scales_and_fluxes first.'
      end if

      number_of_elements = size(fluxes)
      allocate(wind_stress_x(number_of_elements))
      allocate(wind_stress_y(number_of_elements))

      do index = 1, number_of_elements
         wind_stress_x(index) = fluxes(index)%wind_stress_x
         wind_stress_y(index) = fluxes(index)%wind_stress_y
      end do
   end subroutine get_wind_stress

   !> Return latent heat flux array [W/m^2] from module-stored fluxes.
   subroutine get_latent_heat_flux(heat_flux)
      real(kind=dp), allocatable, dimension(:), intent(out) :: heat_flux !< Latent heat flux [W/m^2].
      integer :: index
      integer :: number_of_elements

      if (.not. allocated(fluxes)) then
         error stop 'get_latent_heat_flux: module fluxes are not available. Call compute_scales_and_fluxes first.'
      end if

      number_of_elements = size(fluxes)
      allocate(heat_flux(number_of_elements))

      do index = 1, number_of_elements
         heat_flux(index) = fluxes(index)%latent_heat_flux
      end do
   end subroutine get_latent_heat_flux

   !> Return sensible heat flux array [W/m^2] from module-stored fluxes.
   subroutine get_sensible_heat_flux(heat_flux)
      real(kind=dp), allocatable, dimension(:), intent(out) :: heat_flux !< Sensible heat flux [W/m^2].
      integer :: index
      integer :: number_of_elements

      if (.not. allocated(fluxes)) then
         error stop 'get_sensible_heat_flux: module fluxes are not available. Call compute_scales_and_fluxes first.'
      end if

      number_of_elements = size(fluxes)
      allocate(heat_flux(number_of_elements))

      do index = 1, number_of_elements
         heat_flux(index) = fluxes(index)%sensible_heat_flux
      end do
   end subroutine get_sensible_heat_flux

   !> Return u* array [m/s] from module-stored scaling parameters.
   subroutine get_u_star(u_star)
      real(kind=dp), allocatable, dimension(:), intent(out) :: u_star !< Friction velocity u* [m/s].
      integer :: index
      integer :: number_of_elements

      if (.not. allocated(scaling_parameters)) then
         error stop 'get_u_star: module scaling parameters are not available. Call compute_scales_and_fluxes first.'
      end if

      number_of_elements = size(scaling_parameters)
      allocate(u_star(number_of_elements))

      do index = 1, number_of_elements
         u_star(index) = scaling_parameters(index)%u_star
      end do
   end subroutine get_u_star

   !> Return t* array [K] from module-stored scaling parameters.
   subroutine get_t_star(t_star)
      real(kind=dp), allocatable, dimension(:), intent(out) :: t_star !< Temperature scale t* [K].
      integer :: index
      integer :: number_of_elements

      if (.not. allocated(scaling_parameters)) then
         error stop 'get_t_star: module scaling parameters are not available. Call compute_scales_and_fluxes first.'
      end if

      number_of_elements = size(scaling_parameters)
      allocate(t_star(number_of_elements))

      do index = 1, number_of_elements
         t_star(index) = scaling_parameters(index)%t_star
      end do
   end subroutine get_t_star

   !> Return q* array [kg/kg] from module-stored scaling parameters.
   subroutine get_q_star(q_star)
      real(kind=dp), allocatable, dimension(:), intent(out) :: q_star !< Humidity scale q* [kg/kg].
      integer :: index
      integer :: number_of_elements

      if (.not. allocated(scaling_parameters)) then
         error stop 'get_q_star: module scaling parameters are not available. Call compute_scales_and_fluxes first.'
      end if

      number_of_elements = size(scaling_parameters)
      allocate(q_star(number_of_elements))

      do index = 1, number_of_elements
         q_star(index) = scaling_parameters(index)%q_star
      end do
   end subroutine get_q_star

   !> Return w* array [m/s] from module-stored scaling parameters.
   subroutine get_w_star(w_star)
      real(kind=dp), allocatable, dimension(:), intent(out) :: w_star !< Convective velocity scale w* [m/s].
      integer :: index
      integer :: number_of_elements

      if (.not. allocated(scaling_parameters)) then
         error stop 'get_w_star: module scaling parameters are not available. Call compute_scales_and_fluxes first.'
      end if

      number_of_elements = size(scaling_parameters)
      allocate(w_star(number_of_elements))

      do index = 1, number_of_elements
         w_star(index) = scaling_parameters(index)%w_star
      end do
   end subroutine get_w_star

   !> Return bulk transfer coefficient of momentum flux [-] from module-stored scaling parameters.
   subroutine get_transfer_coeff_momentum(c_d)
      real(kind=dp), allocatable, dimension(:), intent(out) :: c_d !< Bulk momentum transfer coefficient [-].
      integer :: index
      integer :: number_of_elements

      if (.not. allocated(scaling_parameters)) then
         error stop 'get_transfer_coeff_momentum: module scaling parameters are not available. Call compute_scales_and_fluxes first.'
      end if

      number_of_elements = size(scaling_parameters)
      allocate(c_d(number_of_elements))

      do index = 1, number_of_elements
         c_d(index) = scaling_parameters(index)%c_d
      end do
   end subroutine get_transfer_coeff_momentum

   !> Return bulk transfer coefficient of sensible heat flux [-] from module-stored scaling parameters.
   subroutine get_transfer_coeff_sensible_heat(c_h)
      real(kind=dp), allocatable, dimension(:), intent(out) :: c_h !< Bulk sensible-heat transfer coefficient [-].
      integer :: index
      integer :: number_of_elements

      if (.not. allocated(scaling_parameters)) then
         error stop 'get_transfer_coeff_sensible_heat: module scaling parameters are not available. Call compute_scales_and_fluxes first.'
      end if

      number_of_elements = size(scaling_parameters)
      allocate(c_h(number_of_elements))

      do index = 1, number_of_elements
         c_h(index) = scaling_parameters(index)%c_h
      end do
   end subroutine get_transfer_coeff_sensible_heat

   !> Return bulk transfer coefficient of latent heat flux [-] from module-stored scaling parameters.
   subroutine get_transfer_coeff_latent_heat(c_e)
      real(kind=dp), allocatable, dimension(:), intent(out) :: c_e !< Bulk latent-heat transfer coefficient [-].
      integer :: index
      integer :: number_of_elements

      if (.not. allocated(scaling_parameters)) then
         error stop 'get_transfer_coeff_latent_heat: module scaling parameters are not available. Call compute_scales_and_fluxes first.'
      end if

      number_of_elements = size(scaling_parameters)
      allocate(c_e(number_of_elements))

      do index = 1, number_of_elements
         c_e(index) = scaling_parameters(index)%c_e
      end do
   end subroutine get_transfer_coeff_latent_heat

   !> Return momentum roughness length array [m] from module-stored scaling parameters.
   subroutine get_z0_momentum(z0_momentum)
      real(kind=dp), allocatable, dimension(:), intent(out) :: z0_momentum !< Momentum roughness length [m].
      integer :: index
      integer :: number_of_elements

      if (.not. allocated(scaling_parameters)) then
         error stop 'get_z0_momentum: module scaling parameters are not available. Call compute_scales_and_fluxes first.'
      end if

      number_of_elements = size(scaling_parameters)
      allocate(z0_momentum(number_of_elements))

      do index = 1, number_of_elements
         z0_momentum(index) = scaling_parameters(index)%z0_momentum
      end do
   end subroutine get_z0_momentum

   !> Return heat roughness length array [m] from module-stored scaling parameters.
   subroutine get_z0_heat(z0_heat)
      real(kind=dp), allocatable, dimension(:), intent(out) :: z0_heat !< Heat roughness length [m].
      integer :: index
      integer :: number_of_elements

      if (.not. allocated(scaling_parameters)) then
         error stop 'get_z0_heat: module scaling parameters are not available. Call compute_scales_and_fluxes first.'
      end if

      number_of_elements = size(scaling_parameters)
      allocate(z0_heat(number_of_elements))

      do index = 1, number_of_elements
         z0_heat(index) = scaling_parameters(index)%z0_heat
      end do
   end subroutine get_z0_heat

   !> Return humidity roughness length array [m] from module-stored scaling parameters.
   subroutine get_z0_humidity(z0_humidity)
      real(kind=dp), allocatable, dimension(:), intent(out) :: z0_humidity !< Humidity roughness length [m].
      integer :: index
      integer :: number_of_elements

      if (.not. allocated(scaling_parameters)) then
         error stop 'get_z0_humidity: module scaling parameters are not available. Call compute_scales_and_fluxes first.'
      end if

      number_of_elements = size(scaling_parameters)
      allocate(z0_humidity(number_of_elements))

      do index = 1, number_of_elements
         z0_humidity(index) = scaling_parameters(index)%z0_humidity
      end do
   end subroutine get_z0_humidity

   !> Return Obukhov length array [m] from module-stored scaling parameters.
   subroutine get_obukhov_length(obukhov_length)
      real(kind=dp), allocatable, dimension(:), intent(out) :: obukhov_length !< Obukhov length [m].
      integer :: index
      integer :: number_of_elements

      if (.not. allocated(scaling_parameters)) then
         error stop 'get_obukhov_length: module scaling parameters are not available. Call compute_scales_and_fluxes first.'
      end if

      number_of_elements = size(scaling_parameters)
      allocate(obukhov_length(number_of_elements))

      do index = 1, number_of_elements
         obukhov_length(index) = scaling_parameters(index)%obukhov_length
      end do
   end subroutine get_obukhov_length

   !> Return bulk Richardson number array [-] from module-stored scaling parameters.
   subroutine get_richardson_number(richardson_number)
      real(kind=dp), allocatable, dimension(:), intent(out) :: richardson_number !< Bulk Richardson number [-].
      integer :: index
      integer :: number_of_elements

      if (.not. allocated(scaling_parameters)) then
         error stop 'get_richardson_number: module scaling parameters are not available. Call compute_scales_and_fluxes first.'
      end if

      number_of_elements = size(scaling_parameters)
      allocate(richardson_number(number_of_elements))

      do index = 1, number_of_elements
         richardson_number(index) = scaling_parameters(index)%richardson_number
      end do
   end subroutine get_richardson_number

   !> Return a batch of atmospheric-stability diagnostics from module-stored scaling parameters.
   subroutine get_bulk_exchange_diagnostics(w_star, obukhov_length, c_d, c_h, c_e)
      real(kind=dp), allocatable, dimension(:), intent(out) :: w_star !< Convective velocity scale w* [m/s].
      real(kind=dp), allocatable, dimension(:), intent(out) :: obukhov_length !< Obukhov length [m].
      real(kind=dp), allocatable, dimension(:), intent(out) :: c_d !< Bulk momentum transfer coefficient [-].
      real(kind=dp), allocatable, dimension(:), intent(out) :: c_h !< Bulk sensible-heat transfer coefficient [-].
      real(kind=dp), allocatable, dimension(:), intent(out) :: c_e !< Bulk latent-heat transfer coefficient [-].
      integer :: index
      integer :: number_of_elements

      if (.not. allocated(scaling_parameters)) then
         error stop 'get_bulk_exchange_diagnostics: module scaling parameters are not available. Call compute_scales_and_fluxes first.'
      end if

      number_of_elements = size(scaling_parameters)
      allocate(w_star(number_of_elements))
      allocate(obukhov_length(number_of_elements))
      allocate(c_d(number_of_elements))
      allocate(c_h(number_of_elements))
      allocate(c_e(number_of_elements))

      do index = 1, number_of_elements
         w_star(index) = scaling_parameters(index)%w_star
         obukhov_length(index) = scaling_parameters(index)%obukhov_length
         c_d(index) = scaling_parameters(index)%c_d
         c_h(index) = scaling_parameters(index)%c_h
         c_e(index) = scaling_parameters(index)%c_e
      end do
   end subroutine get_bulk_exchange_diagnostics

   !> Compute roughness lengths following an ECMWF-style parameterization.
   pure subroutine compute_roughness_lengths(u_star, charnock, z0_momentum, z0_heat, z0_humidity)
      real(kind=dp), intent(in) :: u_star, charnock          !< u* [m/s], Charnock [-].
      real(kind=dp), intent(out) :: z0_momentum, z0_heat, z0_humidity !< Roughness [m].
      real(kind=dp), parameter :: ALPHA_M = 0.11_dp ! roughness length coefficient for momentum
      real(kind=dp), parameter :: ALPHA_H = 0.40_dp ! roughness length coefficient for heat
      real(kind=dp), parameter :: ALPHA_Q = 0.62_dp ! roughness length coefficient for humidity
      real(kind=dp) :: inverse_u_star

      inverse_u_star = 1.0_dp / sign(max(abs(u_star), EPS8), u_star)
      z0_momentum = ALPHA_M*CONST_NU_AIR*inverse_u_star + charnock*u_star*u_star/CONST_GRAVITY
      z0_heat = ALPHA_H*CONST_NU_AIR*inverse_u_star
      z0_humidity = ALPHA_Q*CONST_NU_AIR*inverse_u_star
   end subroutine compute_roughness_lengths

   !> Compute saturation-vapor-pressure from temperature (ECMWF_T2esat).
   !! It uses Tetens formula from IFS documentation, Part IV, Section 7.5.2 Relative Humidity.
   !! If input is dew-point temperature, it gives actual vapor-pressure. 
   pure function compute_saturation_pressure(temperature) result(vapor_pressure)
      real(kind=dp), intent(in) :: temperature !< Temperature [K].
      real(kind=dp) :: vapor_pressure          !< Saturation pressure [Pa].
      real(kind=dp), parameter :: coeff_a = 17.502_dp
      real(kind=dp), parameter :: coeff_b = 32.19_dp

      vapor_pressure = CONST_E0 * exp(coeff_a * ((temperature - CONST_TT) / (temperature - coeff_b)))
   end function compute_saturation_pressure

   !> Compute specific humidity based on vapor pressure and total air pressure (ECMWF_e2qv).
   pure function compute_specific_humidity(vapor_pressure, air_pressure) result(specific_humidity)
      real(kind=dp), intent(in) :: vapor_pressure   !< Vapor pressure [Pa].
      real(kind=dp), intent(in) :: air_pressure     !< Air pressure [Pa].
      real(kind=dp) :: specific_humidity            !< Specific humidity [kg/kg].

      specific_humidity = vapor_pressure / (air_pressure + CONST_EST*(air_pressure - vapor_pressure))
   end function compute_specific_humidity

   !> Compute moist air density from temperature, pressure and vapor pressure.
   pure function compute_air_density(air_temperature, air_pressure, vapor_pressure) result(air_density)
      real(kind=dp), intent(in) :: air_temperature !< Air temperature [K].
      real(kind=dp), intent(in) :: air_pressure    !< Air pressure [Pa].
      real(kind=dp), intent(in) :: vapor_pressure  !< Vapor pressure [Pa].
      real(kind=dp) :: air_density                 !< Air density [kg/m^3].
      real(kind=dp) :: specific_humidity
      real(kind=dp) :: virtual_temperature

      specific_humidity = compute_specific_humidity(vapor_pressure, air_pressure)
      virtual_temperature = air_temperature * (1.0_dp + CONST_EST * specific_humidity)
      air_density = air_pressure / (CONST_Rd * virtual_temperature)
   end function compute_air_density

   !> Bulk Richardson number based on virtual temperature difference (Richardson).
   pure function compute_richardson_number(wind_speed, surface_temperature, air_temperature, air_pressure, &
                  air_specific_humidity, saturated_humidity_reduction_factor, height_wind_velocity) result(richardson_number)
      real(kind=dp), intent(in) :: wind_speed             !< Wind speed magnitude [m/s].
      real(kind=dp), intent(in) :: surface_temperature    !< Surface temperature [K].
      real(kind=dp), intent(in) :: air_temperature        !< Air temperature [K].
      real(kind=dp), intent(in) :: air_pressure           !< Air pressure [Pa].
      real(kind=dp), intent(in) :: air_specific_humidity  !< Air specific humidity [kg/kg].
      real(kind=dp), intent(in) :: saturated_humidity_reduction_factor !< Saturation reduction factor [-].
      real(kind=dp), intent(in) :: height_wind_velocity   !< Height of prescribed wind velocity [m]
      real(kind=dp) :: richardson_number             !< Richardson number [-].
      real(kind=dp) :: surface_specific_humidity
      real(kind=dp) :: surface_virtual_temperature
      real(kind=dp) :: air_virtual_temperature
      real(kind=dp) :: virtual_temperature_difference

      surface_specific_humidity = saturated_humidity_reduction_factor * compute_specific_humidity( &
                      compute_saturation_pressure(surface_temperature), air_pressure)
      surface_virtual_temperature = surface_temperature * (1.0_dp + CONST_EST*surface_specific_humidity)
      air_virtual_temperature = air_temperature * (1.0_dp + CONST_EST*air_specific_humidity)
      virtual_temperature_difference = air_virtual_temperature - surface_virtual_temperature

      richardson_number = CONST_GRAVITY * virtual_temperature_difference * height_wind_velocity / &
                               max(air_virtual_temperature*wind_speed*wind_speed, 1.0e-10_dp)
   end function compute_richardson_number

   !> Stability profile for heat and humidity (ECMWF_Psi).
   pure function stability_profile_heat_humidity(stability_parameter) result(stability_correction)
      real(kind=dp), intent(in) :: stability_parameter !< Stability parameter z/L [-].
      real(kind=dp) :: stability_correction            !< Stability correction [-].
      real(kind=dp), parameter :: coef_a = 1.0_dp
      real(kind=dp), parameter :: coef_b = 2.0_dp/3.0_dp
      real(kind=dp), parameter :: coef_c = 5.0_dp
      real(kind=dp), parameter :: coef_d = 0.35_dp
      real(kind=dp), parameter :: coef_unstable = 16.0_dp
      real(kind=dp) :: clipped_zeta, unstable_factor

      clipped_zeta = min(stability_parameter, 5.0_dp)
      if (clipped_zeta < 0.0_dp) then
      ! unstable conditions
         unstable_factor = (1.0_dp - coef_unstable*clipped_zeta)**0.25_dp
         stability_correction = 2.0_dp * log((1.0_dp + unstable_factor*unstable_factor) / 2.0_dp)
      else if (clipped_zeta > 0.0_dp) then
      ! stable conditions
         stability_correction = (-coef_b*(clipped_zeta - (coef_c/coef_d))*exp(-coef_d*clipped_zeta)) - &
                                (1.0_dp + (2.0_dp/3.0_dp)*coef_a*clipped_zeta)**1.5_dp - (coef_b*coef_c/coef_d) + 1.0_dp
      else
      ! neutral conditions
         stability_correction = 0.0_dp
      end if
   end function stability_profile_heat_humidity

   !> Stability profile for momentum (ECMWF_Psi).
   pure function stability_profile_momentum(stability_parameter) result(stability_correction)
      real(kind=dp), intent(in) :: stability_parameter !< Stability parameter z/L [-].
      real(kind=dp) :: stability_correction            !< Stability correction [-].
      real(kind=dp), parameter :: coef_a = 1.0_dp
      real(kind=dp), parameter :: coef_b = 2.0_dp/3.0_dp
      real(kind=dp), parameter :: coef_c = 5.0_dp
      real(kind=dp), parameter :: coef_d = 0.35_dp
      real(kind=dp), parameter :: coef_unstable = 16.0_dp
      real(kind=dp) :: clipped_zeta, unstable_factor, psi_stable

      clipped_zeta = min(stability_parameter, 5.0_dp)

      if (clipped_zeta < 0.0_dp) then
      ! unstable conditions
         unstable_factor = (1.0_dp - coef_unstable*clipped_zeta)**0.25_dp
         stability_correction = (PI/2.0_dp) - (2.0_dp*atan(unstable_factor)) + &
                        log(((1.0_dp + unstable_factor)**2 * (1.0_dp + unstable_factor*unstable_factor)) / 8.0_dp)
      else if (clipped_zeta > 0.0_dp) then
      ! stable conditions
         psi_stable = (-coef_b*(clipped_zeta - (coef_c/coef_d))*exp(-coef_d*clipped_zeta)) - &
                      coef_a*clipped_zeta - (coef_b*coef_c/coef_d)
         stability_correction = min(psi_stable, 1.0e30_dp)
      else
      ! neutral conditions
         stability_correction = 0.0_dp
      end if
   end function stability_profile_momentum

   !> Compute free convection velocity scale (ECMWF_wster3)
   pure function compute_convective_velocity_scale(friction_velocity, temperature_scale, humidity_scale, &
                        air_temperature, air_specific_humidity) result(convective_velocity_scale)
      real(kind=dp), intent(in) :: air_temperature       !< Air temperature [K].
      real(kind=dp), intent(in) :: friction_velocity     !< Friction velocity [m/s].
      real(kind=dp), intent(in) :: temperature_scale     !< Temperature scale [K].
      real(kind=dp), intent(in) :: humidity_scale        !< Humidity scale [kg/kg].
      real(kind=dp), intent(in) :: air_specific_humidity !< Air specific humidity [kg/kg].
      real(kind=dp) :: convective_velocity_scale         !< Velocity [m/s].
      real(kind=dp), parameter :: boundary_layer_height = 1000.0_dp
      real(kind=dp) :: virtual_temperature_scale, buoyancy_flux

      virtual_temperature_scale = temperature_scale*(1.0_dp + air_specific_humidity*CONST_EST) + &
                                  (CONST_EST*air_temperature*humidity_scale)
      buoyancy_flux = max(0.0_dp, -CONST_GRAVITY*friction_velocity*virtual_temperature_scale / max(air_temperature, 1.0e-8_dp))
      convective_velocity_scale = (buoyancy_flux*boundary_layer_height)**(1.0_dp/3.0_dp)
   end function compute_convective_velocity_scale

end module m_atmospheric_stability
