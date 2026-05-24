module test_atmospheric_stability
   use assertions_gtest
   use precision, only: dp
   use precision_basics, only: comparereal
   use m_atmospheric_stability
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_atmospheric_stability, test_compute_scales_and_fluxes_moninobukhov_true, test_compute_scales_and_fluxes_moninobukhov_true,
   subroutine test_compute_scales_and_fluxes_moninobukhov_true() bind(C)
      real(kind=dp), allocatable :: wind_velocity_u(:), wind_velocity_v(:), air_temperature(:), dew_point_temperature(:), &
                                    air_pressure(:), charnock(:), surface_temperature(:)
      type(t_scales), allocatable :: scaling_parameter_array(:)
      type(t_fluxes), allocatable :: flux_array(:)
      real(kind=dp), allocatable :: wind_stress_u(:), wind_stress_v(:)
      real(kind=dp), allocatable :: latent_heat_flux(:), sensible_heat_flux(:)
      real(kind=dp), allocatable :: u_star(:), t_star(:), q_star(:), z0_momentum(:), z0_heat(:), z0_humidity(:), &
                                    monin_obukhov_length(:), richardson_number(:)
      type(t_options) :: options
      
      options%include_monin_obukhov = .true.
      wind_velocity_u = [7.869952506297818_dp, 0.117833360326781_dp, 18.539150194088315_dp]
      wind_velocity_v = [8.218675428381800_dp, -0.455614551078914_dp, -11.095442028172844_dp]
      air_temperature = [286.212334904992247_dp, 285.769323685018151_dp, 280.824297920699621_dp]
      dew_point_temperature = [283.140027966959963_dp, 283.279402521152690_dp, 275.760981569780142_dp]
      air_pressure = [101190.122711076852283_dp, 99932.877309904928552_dp, 100434.759443715374800_dp]
      charnock = [0.031984724136352_dp, 0.007887448339510_dp, 0.063390066055710_dp]
      surface_temperature = [286.743896484375000_dp, 286.676513671875000_dp, 282.813232421875000_dp]
      
      call compute_scales_and_fluxes(wind_velocity_u, wind_velocity_v, air_temperature, dew_point_temperature, &
                                  air_pressure, charnock, surface_temperature, options)

      call get_u_star(u_star)
      call get_t_star(t_star)
      call get_q_star(q_star)
      call get_z0_momentum(z0_momentum)
      call get_z0_heat(z0_heat)
      call get_z0_humidity(z0_humidity)
      call get_monin_obukhov_length(monin_obukhov_length)
      call get_richardson_number(richardson_number)
      call get_wind_stress(wind_stress_u, wind_stress_v)
      call get_latent_heat_flux(latent_heat_flux)
      call get_sensible_heat_flux(sensible_heat_flux)

      call f90_expect_near(u_star(1), 0.483570419681275_dp, 1e-5_dp, "u_star(1) does not match expected value.")
      call f90_expect_near(t_star(1), -0.017773509539055_dp, 1e-7_dp, "t_star(1) does not match expected value.")
      call f90_expect_near(q_star(1), -0.000070992040126_dp, 1e-9_dp, "q_star(1) does not match expected value.")
      call f90_expect_near(z0_momentum(1), 0.000766075070655_dp, 1e-7_dp, "z0_momentum(1) does not match expected value.")
      call f90_expect_near(z0_heat(1), 0.000012407831933_dp, 1e-9_dp, "z0_heat(1) does not match expected value.")
      call f90_expect_near(z0_humidity(1), 0.000019232139496_dp, 1e-9_dp, "z0_humidity(1) does not match expected value.")
      call f90_expect_near(monin_obukhov_length(1), -574.507255494245101_dp, 1e-1_dp, "monin_obukhov_length(1) does not match expected value.")
      call f90_expect_near(richardson_number(1), -0.002646973390076_dp, 1e-7_dp, "richardson_number(1) does not match expected value.")
      call f90_expect_true(comparereal(wind_stress_u(1), 0.19827531947147783_dp) == 0, "wind_stress_u(1) does not match expected value")
      call f90_expect_true(comparereal(wind_stress_v(1), 0.20706103307367532_dp) == 0, "wind_stress_v(1) does not match expected value")
      call f90_expect_true(comparereal(latent_heat_flux(1), -105.25243274353171_dp) == 0, "latent_heat_flux(1) does not match expected value")
      call f90_expect_true(comparereal(sensible_heat_flux(1), -9.0742363226028804_dp) == 0, "sensible_heat_flux(1) does not match expected value")
            
      call f90_expect_near(u_star(2), 0.021983450898111_dp, 1e-5_dp, "u_star(2) does not match expected value.")
      call f90_expect_near(t_star(2), -0.063781380223977_dp, 1e-5_dp, "t_star(2) does not match expected value.")
      call f90_expect_near(q_star(2), -0.000148940393852_dp, 1e-8_dp, "q_star(2) does not match expected value.")
      call f90_expect_near(z0_momentum(2), 0.000075442471892_dp, 1e-8_dp, "z0_momentum(2) does not match expected value.")
      call f90_expect_near(z0_heat(2), 0.000272922727444_dp, 1e-7_dp, "z0_heat(2) does not match expected value.")
      call f90_expect_near(z0_humidity(2), 0.000423030227538_dp, 1e-6_dp, "z0_humidity(2) does not match expected value.")
      call f90_expect_near(monin_obukhov_length(2), -0.401534400984983_dp, 1e-4_dp, "monin_obukhov_length(2) does not match expected value.")
      call f90_expect_near(richardson_number(2), -1.974372173626931_dp, 1e-4_dp, "richardson_number(2) does not match expected value.")
      call f90_expect_true(comparereal(wind_stress_u(2), 0.00014672001126972184_dp) == 0, "wind_stress_u(2) does not match expected value")
      call f90_expect_true(comparereal(wind_stress_v(2), -0.00056730769523640964_dp) == 0, "wind_stress_v(2) does not match expected value")
      call f90_expect_true(comparereal(latent_heat_flux(2), -9.9282858337395972_dp) == 0, "latent_heat_flux(2) does not match expected value")
      call f90_expect_true(comparereal(sensible_heat_flux(2), -1.4640973372837305_dp) == 0, "sensible_heat_flux(2) does not match expected value")

      call f90_expect_near(u_star(3), 1.260861637594213_dp, 1e-5_dp, "u_star(3) does not match expected value.")
      call f90_expect_near(t_star(3), -0.061491952723764_dp, 1e-7_dp, "t_star(3) does not match expected value.")
      call f90_expect_near(q_star(3), -0.000092343226883_dp, 1e-9_dp, "q_star(3) does not match expected value.")
      call f90_expect_near(z0_momentum(3), 0.010276823644789_dp, 1e-6_dp, "z0_momentum(3) does not match expected value.")
      call f90_expect_near(z0_heat(3), 0.000004758824805_dp, 1e-9_dp, "z0_heat(3) does not match expected value.")
      call f90_expect_near(z0_humidity(3), 0.000007376178448_dp, 1e-9_dp, "z0_humidity(3) does not match expected value.")
      call f90_expect_near(monin_obukhov_length(3), -1479.651083435764122_dp, 2e-1_dp, "monin_obukhov_length(3) does not match expected value.")
      call f90_expect_near(richardson_number(3), -0.002086552554703_dp, 1e-7_dp, "richardson_number(3) does not match expected value.")
      call f90_expect_true(comparereal(wind_stress_u(3), 1.6948223957543_dp) == 0, "wind_stress_u(3) does not match expected value")
      call f90_expect_true(comparereal(wind_stress_v(3), -1.0143293216394162_dp) == 0, "wind_stress_v(3) does not match expected value")
      call f90_expect_true(comparereal(latent_heat_flux(3), -361.76108401388166_dp) == 0, "latent_heat_flux(3) does not match expected value")
      call f90_expect_true(comparereal(sensible_heat_flux(3), -82.956322198020018_dp) == 0, "sensible_heat_flux(3) does not match expected value")

   end subroutine test_compute_scales_and_fluxes_moninobukhov_true
   !$f90tw )

   !$f90tw TESTCODE(TEST, test_atmospheric_stability, test_compute_scales_and_fluxes_moninobukhov_false, test_compute_scales_and_fluxes_moninobukhov_false,
   subroutine test_compute_scales_and_fluxes_moninobukhov_false() bind(C)
      real(kind=dp), allocatable :: wind_velocity_u(:), wind_velocity_v(:), air_temperature(:), dew_point_temperature(:), &
                                    air_pressure(:), charnock(:), surface_temperature(:)
      type(t_scales), allocatable :: scaling_parameter_array(:)
      type(t_fluxes), allocatable :: flux_array(:)
      real(kind=dp), allocatable :: wind_stress_u(:), wind_stress_v(:)
      real(kind=dp), allocatable :: latent_heat_flux(:), sensible_heat_flux(:)
      real(kind=dp), allocatable :: u_star(:), t_star(:), q_star(:), z0_momentum(:), z0_heat(:), z0_humidity(:), &
                                    monin_obukhov_length(:), richardson_number(:)
      real(kind=dp) :: tolerance = 1e-5_dp
      type(t_options) :: options
      
      options%include_monin_obukhov = .false.
      wind_velocity_u = [7.869952506297818_dp, 0.117833360326781_dp, 18.539150194088315_dp]
      wind_velocity_v = [8.218675428381800_dp, -0.455614551078914_dp, -11.095442028172844_dp]
      air_temperature = [286.212334904992247_dp, 285.769323685018151_dp, 280.824297920699621_dp]
      dew_point_temperature = [283.140027966959963_dp, 283.279402521152690_dp, 275.760981569780142_dp]
      air_pressure = [101190.122711076852283_dp, 99932.877309904928552_dp, 100434.759443715374800_dp]
      charnock = [0.031984724136352_dp, 0.007887448339510_dp, 0.063390066055710_dp]
      surface_temperature = [286.743896484375000_dp, 286.676513671875000_dp, 282.813232421875000_dp]
      
      call compute_scales_and_fluxes(wind_velocity_u, wind_velocity_v, air_temperature, dew_point_temperature, &
                                  air_pressure, charnock, surface_temperature, options)
   
      call get_u_star(u_star)
      call get_t_star(t_star)
      call get_q_star(q_star)
      call get_z0_momentum(z0_momentum)
      call get_z0_heat(z0_heat)
      call get_z0_humidity(z0_humidity)
      call get_monin_obukhov_length(monin_obukhov_length)
      call get_richardson_number(richardson_number)
      call get_wind_stress(wind_stress_u, wind_stress_v)
      call get_latent_heat_flux(latent_heat_flux)
      call get_sensible_heat_flux(sensible_heat_flux)

      call f90_expect_near(u_star(1), 0.479416158782993_dp, 1e-5_dp, "u_star(1) does not match expected value.")
      call f90_expect_near(t_star(1), -0.021149810129999_dp, 1e-7_dp, "t_star(1) does not match expected value.")
      call f90_expect_near(q_star(1), -0.000084472862640_dp, 1e-9_dp, "q_star(1) does not match expected value.")
      call f90_expect_near(z0_momentum(1), 0.000752955407655_dp, 1e-7_dp, "z0_momentum(1) does not match expected value.")
      call f90_expect_near(z0_heat(1), 0.000012516200200_dp, 1e-9_dp, "z0_heat(1) does not match expected value.")
      call f90_expect_near(z0_humidity(1), 0.000019400110310_dp, 1e-9_dp, "z0_humidity(1) does not match expected value.")
      call f90_expect_near(monin_obukhov_length(1), 0.0_dp, 0.0_dp, "monin_obukhov_length(1) does not match expected value.")
      call f90_expect_near(richardson_number(1), 0.0_dp, 0.0_dp, "richardson_number(1) does not match expected value.")
      call f90_expect_true(comparereal(wind_stress_u(1), 0.19488370311667186_dp) == 0, "wind_stress_u(1) does not match expected value")
      call f90_expect_true(comparereal(wind_stress_v(1), 0.20351913190267898_dp) == 0, "wind_stress_v(1) does not match expected value")
      call f90_expect_true(comparereal(latent_heat_flux(1), -124.16320694319903_dp) == 0, "latent_heat_flux(1) does not match expected value")
      call f90_expect_true(comparereal(sensible_heat_flux(1), -10.705244722352852_dp) == 0, "sensible_heat_flux(1) does not match expected value")

      call f90_expect_near(u_star(2), 0.016365185405138_dp, 1e-5_dp, "u_star(2) does not match expected value.")
      call f90_expect_near(t_star(2), -0.050263329612856_dp, 1e-5_dp, "t_star(2) does not match expected value.")
      call f90_expect_near(q_star(2), -0.000114210401362_dp, 1e-8_dp, "q_star(2) does not match expected value.")
      call f90_expect_near(z0_momentum(2), 0.000101038350813_dp, 1e-8_dp, "z0_momentum(2) does not match expected value.")
      call f90_expect_near(z0_heat(2), 0.000366628877207_dp, 1e-7_dp, "z0_heat(2) does not match expected value.")
      call f90_expect_near(z0_humidity(2), 0.000568274759671_dp, 1e-6_dp, "z0_humidity(2) does not match expected value.")
      call f90_expect_near(monin_obukhov_length(2), 0.0_dp, 0.0_dp, "monin_obukhov_length(2) does not match expected value.")
      call f90_expect_near(richardson_number(2), 0.0_dp, 0.0_dp, "richardson_number(2) does not match expected value.")
      call f90_expect_true(comparereal(wind_stress_u(2), 8.130856993426958e-5_dp) == 0, "wind_stress_u(2) does not match expected value")
      call f90_expect_true(comparereal(wind_stress_v(2), -0.00031438777173743303_dp) == 0, "wind_stress_v(2) does not match expected value")
      call f90_expect_true(comparereal(latent_heat_flux(2), -5.667460923352202_dp) == 0, "latent_heat_flux(2) does not match expected value")
      call f90_expect_true(comparereal(sensible_heat_flux(2), -0.85891175900128047_dp) == 0, "sensible_heat_flux(2) does not match expected value")

      call f90_expect_near(u_star(3), 1.254106938341015_dp, 1e-5_dp, "u_star(3) does not match expected value.")
      call f90_expect_near(t_star(3), -0.073256318751260_dp, 1e-7_dp, "t_star(3) does not match expected value.")
      call f90_expect_near(q_star(3), -0.000110008293278_dp, 1e-9_dp, "q_star(3) does not match expected value.")
      call f90_expect_near(z0_momentum(3), 0.010166908002376_dp, 1e-6_dp, "z0_momentum(3) does not match expected value.")
      call f90_expect_near(z0_heat(3), 0.000004784484715_dp, 1e-9_dp, "z0_heat(3) does not match expected value.")
      call f90_expect_near(z0_humidity(3), 0.000007415951308_dp, 1e-9_dp, "z0_humidity(3) does not match expected value.")
      call f90_expect_near(monin_obukhov_length(3), 0.0_dp, 0.0_dp, "monin_obukhov_length(3) does not match expected value.")
      call f90_expect_near(richardson_number(3), 0.0_dp, 0.0_dp, "richardson_number(3) does not match expected value.")
      call f90_expect_true(comparereal(wind_stress_u(3), 1.6767200204232939_dp) == 0, "wind_stress_u(3) does not match expected value")
      call f90_expect_true(comparereal(wind_stress_v(3), -1.0034952837275031_dp) == 0, "wind_stress_v(3) does not match expected value")
      call f90_expect_true(comparereal(latent_heat_flux(3), -428.65719607732001_dp) == 0, "latent_heat_flux(3) does not match expected value")
      call f90_expect_true(comparereal(sensible_heat_flux(3), -98.297891071891215_dp) == 0, "sensible_heat_flux(3) does not match expected value")

   end subroutine test_compute_scales_and_fluxes_moninobukhov_false
   !$f90tw )
end module test_atmospheric_stability
