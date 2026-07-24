module test_atmospheric_stability
   use assertions_gtest
   use precision, only: dp
   use precision_basics, only: comparereal
   use m_atmospheric_stability
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_atmospheric_stability, test_compute_scales_and_fluxes_moninobukhov_true, test_compute_scales_and_fluxes_moninobukhov_true,
   subroutine test_compute_scales_and_fluxes_moninobukhov_true() bind(C)
      real(kind=dp), allocatable, dimension(:) :: wind_velocity_u, wind_velocity_v, air_temperature, dew_point_temperature, &
                                    air_pressure, charnock, surface_temperature
      type(t_scales), allocatable, dimension(:) :: scaling_parameter_array
      type(t_fluxes), allocatable, dimension(:) :: flux_array
      real(kind=dp), allocatable, dimension(:) :: wind_stress_u, wind_stress_v
      real(kind=dp), allocatable, dimension(:) :: latent_heat_flux, sensible_heat_flux
      real(kind=dp), allocatable, dimension(:) :: u_star, t_star, q_star, w_star, z0_momentum, z0_heat, z0_humidity, &
                                    obukhov_length, richardson_number, c_d, c_h, c_e
      type(t_options) :: options
      
      options%include_stability = .true.
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
      call get_obukhov_length(obukhov_length)
      call get_richardson_number(richardson_number)
      call get_wind_stress(wind_stress_u, wind_stress_v)
      call get_latent_heat_flux(latent_heat_flux)
      call get_sensible_heat_flux(sensible_heat_flux)
      call get_w_star(w_star)
      call get_transfer_coeff_momentum(c_d)
      call get_transfer_coeff_sensible_heat(c_h)
      call get_transfer_coeff_latent_heat(c_e)

      call f90_expect_near(u_star(1), 0.483570419681275_dp, 1e-5_dp, "u_star(1) does not match expected value.")
      call f90_expect_near(t_star(1), -0.017773509539055_dp, 1e-7_dp, "t_star(1) does not match expected value.")
      call f90_expect_near(q_star(1), -0.000070992040126_dp, 1e-9_dp, "q_star(1) does not match expected value.")
      call f90_expect_near(z0_momentum(1), 0.000766075070655_dp, 1e-7_dp, "z0_momentum(1) does not match expected value.")
      call f90_expect_near(z0_heat(1), 0.000012407831933_dp, 1e-9_dp, "z0_heat(1) does not match expected value.")
      call f90_expect_near(z0_humidity(1), 0.000019232139496_dp, 1e-9_dp, "z0_humidity(1) does not match expected value.")
      call f90_expect_near(obukhov_length(1), -574.507255494245101_dp, 1e-1_dp, "obukhov_length(1) does not match expected value.")
      call f90_expect_near(richardson_number(1), -0.002646973390076_dp, 1e-7_dp, "richardson_number(1) does not match expected value.")
      call f90_expect_near(wind_stress_u(1), 0.19827124396041873_dp, 1e-9_dp, "wind_stress_u(1) does not match expected value.")
      call f90_expect_near(wind_stress_v(1), 0.20705677697396282_dp, 1e-9_dp, "wind_stress_v(1) does not match expected value.")
      call f90_expect_near(latent_heat_flux(1), -105.25074622690883_dp, 1e-9_dp, "latent_heat_flux(1) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(1), -9.0740928315913827_dp, 1e-9_dp, "sensible_heat_flux(1) does not match expected value.")
      call f90_expect_near(w_star(1), 0.0_dp, 1e-9_dp, "w_star(1) does not match expected value.")
      call f90_expect_near(c_d(1), 0.0018059159533254409_dp, 1e-9_dp, "c_d(1) does not match expected value.")
      call f90_expect_near(c_h(1), 0.0014208689934920476_dp, 1e-9_dp, "c_h(1) does not match expected value.")
      call f90_expect_near(c_e(1), 0.0014748989040242813_dp, 1e-9_dp, "c_e(1) does not match expected value.")
            
      call f90_expect_near(u_star(2), 0.021983450898111_dp, 1e-5_dp, "u_star(2) does not match expected value.")
      call f90_expect_near(t_star(2), -0.063781380223977_dp, 1e-5_dp, "t_star(2) does not match expected value.")
      call f90_expect_near(q_star(2), -0.000148940393852_dp, 1e-8_dp, "q_star(2) does not match expected value.")
      call f90_expect_near(z0_momentum(2), 0.000075442471892_dp, 1e-8_dp, "z0_momentum(2) does not match expected value.")
      call f90_expect_near(z0_heat(2), 0.000272922727444_dp, 1e-7_dp, "z0_heat(2) does not match expected value.")
      call f90_expect_near(z0_humidity(2), 0.000423030227538_dp, 1e-6_dp, "z0_humidity(2) does not match expected value.")
      call f90_expect_near(obukhov_length(2), -0.401534400984983_dp, 1e-4_dp, "obukhov_length(2) does not match expected value.")
      call f90_expect_near(richardson_number(2), -1.974372173626931_dp, 1e-4_dp, "richardson_number(2) does not match expected value.")
      call f90_expect_near(wind_stress_u(2), 0.00014672001126972184_dp, 1e-9_dp, "wind_stress_u(2) does not match expected value.")
      call f90_expect_near(wind_stress_v(2), -0.00056730769523640964_dp, 1e-9_dp, "wind_stress_v(2) does not match expected value.")
      call f90_expect_near(latent_heat_flux(2), -9.9282650485071215_dp, 1e-9_dp, "latent_heat_flux(2) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(2), -1.4640944180384317_dp, 1e-9_dp, "sensible_heat_flux(2) does not match expected value.")
      call f90_expect_near(w_star(2), 0.0_dp, 1e-9_dp, "w_star(2) does not match expected value.")
      call f90_expect_near(c_d(2), 0.0021821300262445326_dp, 1e-9_dp, "c_d(2) does not match expected value.")
      call f90_expect_near(c_h(2), 0.0032842424452251642_dp, 1e-9_dp, "c_h(2) does not match expected value.")
      call f90_expect_near(c_e(2), 0.003556338038678844_dp, 1e-9_dp, "c_e(2) does not match expected value.")

      call f90_expect_near(u_star(3), 1.2605930619410015_dp, 1e-5_dp, "u_star(3) does not match expected value.")
      call f90_expect_near(t_star(3), -0.061488117346359027_dp, 1e-7_dp, "t_star(3) does not match expected value.")
      call f90_expect_near(q_star(3), -9.2337265830901515e-05_dp, 1e-9_dp, "q_star(3) does not match expected value.")
      call f90_expect_near(z0_momentum(3), 0.010272354722833723_dp, 1e-6_dp, "z0_momentum(3) does not match expected value.")
      call f90_expect_near(z0_heat(3), 4.7598600384773734e-06_dp, 1e-9_dp, "z0_heat(3) does not match expected value.")
      call f90_expect_near(z0_humidity(3), 7.3777830596399287e-06_dp, 1e-9_dp, "z0_humidity(3) does not match expected value.")
      call f90_expect_near(obukhov_length(3), -1479.651083435764122_dp, 2e-1_dp, "obukhov_length(3) does not match expected value.")
      call f90_expect_near(richardson_number(3), -0.002086552554703_dp, 1e-7_dp, "richardson_number(3) does not match expected value.")
      call f90_expect_near(wind_stress_u(3), 1.6941085481015359_dp, 1e-9_dp, "wind_stress_u(3) does not match expected value.")
      call f90_expect_near(wind_stress_v(3), -1.0139020930358786_dp, 1e-9_dp, "wind_stress_v(3) does not match expected value.")
      call f90_expect_near(latent_heat_flux(3), -361.66130457981751_dp, 1e-9_dp, "latent_heat_flux(3) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(3), -82.9336247571147_dp, 1e-9_dp, "sensible_heat_flux(3) does not match expected value.")
      call f90_expect_near(w_star(3), 0.0_dp, 1e-9_dp, "w_star(3) does not match expected value.")
      call f90_expect_near(c_d(3), 0.0034041655741110614_dp, 1e-9_dp, "c_d(3) does not match expected value.")
      call f90_expect_near(c_h(3), 0.0018031744251089255_dp, 1e-9_dp, "c_h(3) does not match expected value.")
      call f90_expect_near(c_e(3), 0.0018663715487940969_dp, 1e-9_dp, "c_e(3) does not match expected value.")

   end subroutine test_compute_scales_and_fluxes_moninobukhov_true
   !$f90tw )

   !$f90tw TESTCODE(TEST, test_atmospheric_stability, test_compute_scales_and_fluxes_moninobukhov_false, test_compute_scales_and_fluxes_moninobukhov_false,
   subroutine test_compute_scales_and_fluxes_moninobukhov_false() bind(C)
      real(kind=dp), allocatable, dimension(:) :: wind_velocity_u, wind_velocity_v, air_temperature, dew_point_temperature, &
                                    air_pressure, charnock, surface_temperature
      type(t_scales), allocatable, dimension(:) :: scaling_parameter_array
      type(t_fluxes), allocatable, dimension(:) :: flux_array
      real(kind=dp), allocatable, dimension(:) :: wind_stress_u, wind_stress_v
      real(kind=dp), allocatable, dimension(:) :: latent_heat_flux, sensible_heat_flux
      real(kind=dp), allocatable, dimension(:) :: u_star, t_star, q_star, w_star, z0_momentum, z0_heat, z0_humidity, &
                                    obukhov_length, richardson_number, c_d, c_h, c_e
      real(kind=dp) :: tolerance = 1e-5_dp
      type(t_options) :: options
      
      options%include_stability = .false.
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
      call get_obukhov_length(obukhov_length)
      call get_richardson_number(richardson_number)
      call get_wind_stress(wind_stress_u, wind_stress_v)
      call get_latent_heat_flux(latent_heat_flux)
      call get_sensible_heat_flux(sensible_heat_flux)
      call get_w_star(w_star)
      call get_transfer_coeff_momentum(c_d)
      call get_transfer_coeff_sensible_heat(c_h)
      call get_transfer_coeff_latent_heat(c_e)

      call f90_expect_near(u_star(1), 0.479416158782993_dp, 1e-5_dp, "u_star(1) does not match expected value.")
      call f90_expect_near(t_star(1), -0.021149163348618615_dp, 1e-7_dp, "t_star(1) does not match expected value.")
      call f90_expect_near(q_star(1), -8.4470181306019103e-05_dp, 1e-9_dp, "q_star(1) does not match expected value.")
      call f90_expect_near(z0_momentum(1), 0.000752955407655_dp, 1e-7_dp, "z0_momentum(1) does not match expected value.")
      call f90_expect_near(z0_heat(1), 0.000012516200200_dp, 1e-9_dp, "z0_heat(1) does not match expected value.")
      call f90_expect_near(z0_humidity(1), 0.000019400110310_dp, 1e-9_dp, "z0_humidity(1) does not match expected value.")
      call f90_expect_near(obukhov_length(1), 0.0_dp, 0.0_dp, "obukhov_length(1) does not match expected value.")
      call f90_expect_near(richardson_number(1), 0.0_dp, 0.0_dp, "richardson_number(1) does not match expected value.")
      call f90_expect_near(wind_stress_u(1), 0.19487979851175327_dp, 1e-9_dp, "wind_stress_u(1) does not match expected value.")
      call f90_expect_near(wind_stress_v(1), 0.20351505428207375_dp, 1e-9_dp, "wind_stress_v(1) does not match expected value.")
      call f90_expect_near(latent_heat_flux(1), -124.15802195403018_dp, 1e-9_dp, "latent_heat_flux(1) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(1), -10.70481010568599_dp, 1e-9_dp, "sensible_heat_flux(1) does not match expected value.")
      call f90_expect_near(w_star(1),0.0_dp, 1e-9_dp, "w_star(1) does not match expected value.")
      call f90_expect_near(c_d(1), 0.0017750256168437642_dp, 1e-9_dp, "c_d(1) does not match expected value.")
      call f90_expect_near(c_h(1), 0.0014064781068651559_dp, 1e-9_dp, "c_h(1) does not match expected value.")
      call f90_expect_near(c_e(1), 0.0014598746383916721_dp, 1e-9_dp, "c_e(1) does not match expected value.")

      call f90_expect_near(u_star(2), 0.016365185405138_dp, 1e-5_dp, "u_star(2) does not match expected value.")
      call f90_expect_near(t_star(2), -0.050263329612856_dp, 1e-5_dp, "t_star(2) does not match expected value.")
      call f90_expect_near(q_star(2), -0.000114210401362_dp, 1e-8_dp, "q_star(2) does not match expected value.")
      call f90_expect_near(z0_momentum(2), 0.000101038350813_dp, 1e-8_dp, "z0_momentum(2) does not match expected value.")
      call f90_expect_near(z0_heat(2), 0.000366628877207_dp, 1e-7_dp, "z0_heat(2) does not match expected value.")
      call f90_expect_near(z0_humidity(2), 0.000568274759671_dp, 1e-6_dp, "z0_humidity(2) does not match expected value.")
      call f90_expect_near(obukhov_length(2), 0.0_dp, 0.0_dp, "obukhov_length(2) does not match expected value.")
      call f90_expect_near(richardson_number(2), 0.0_dp, 0.0_dp, "richardson_number(2) does not match expected value.")
      call f90_expect_near(wind_stress_u(2), 8.130856993426958e-05_dp, 1e-9_dp, "wind_stress_u(2) does not match expected value.")
      call f90_expect_near(wind_stress_v(2), -0.00031438777173743298_dp, 1e-9_dp, "wind_stress_v(2) does not match expected value.")
      call f90_expect_near(latent_heat_flux(2), -5.6674218418625282_dp, 1e-9_dp, "latent_heat_flux(2) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(2), -0.85890610244988019_dp, 1e-9_dp, "sensible_heat_flux(2) does not match expected value.")
      call f90_expect_near(w_star(2), 0.0_dp, 1e-9_dp, "w_star(2) does not match expected value.")
      call f90_expect_near(c_d(2), 0.0012092819913657618_dp, 1e-9_dp, "c_d(2) does not match expected value.")
      call f90_expect_near(c_h(2), 0.0016166103144087832_dp, 1e-9_dp, "c_h(2) does not match expected value.")
      call f90_expect_near(c_e(2), 0.001703369873101712_dp, 1e-9_dp, "c_e(2) does not match expected value.")

      call f90_expect_near(u_star(3), 1.2538467592669331_dp, 1e-5_dp, "u_star(3) does not match expected value.")
      call f90_expect_near(t_star(3), -0.073228814885929319_dp, 1e-7_dp, "t_star(3) does not match expected value.")
      call f90_expect_near(q_star(3), -0.00010996554406425283_dp, 1e-9_dp, "q_star(3) does not match expected value.")
      call f90_expect_near(z0_momentum(3), 0.010162697710431663_dp, 1e-6_dp, "z0_momentum(3) does not match expected value.")
      call f90_expect_near(z0_heat(3), 0.000004784484715_dp, 1e-9_dp, "z0_heat(3) does not match expected value.")
      call f90_expect_near(z0_humidity(3), 7.4174876197341148e-06_dp, 1e-9_dp, "z0_humidity(3) does not match expected value.")
      call f90_expect_near(obukhov_length(3), 0.0_dp, 0.0_dp, "obukhov_length(3) does not match expected value.")
      call f90_expect_near(richardson_number(3), 0.0_dp, 0.0_dp, "richardson_number(3) does not match expected value.")
      call f90_expect_near(wind_stress_u(3), 1.6760243824409651_dp, 1e-9_dp, "wind_stress_u(3) does not match expected value.")
      call f90_expect_near(wind_stress_v(3), -1.003078953376612_dp, 1e-9_dp, "wind_stress_v(3) does not match expected value.")
      call f90_expect_near(latent_heat_flux(3), -428.40172456086094_dp, 1e-9_dp, "latent_heat_flux(3) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(3), -98.240600038729312_dp, 1e-9_dp, "sensible_heat_flux(3) does not match expected value.")
      call f90_expect_near(w_star(3), 0.0_dp, 1e-9_dp, "w_star(3) does not match expected value.")
      call f90_expect_near(c_d(3), 0.0033678269969595418_dp, 1e-9_dp, "c_d(3) does not match expected value.")
      call f90_expect_near(c_h(3), 0.0017927819323621376_dp, 1e-9_dp, "c_h(3) does not match expected value.")
      call f90_expect_near(c_e(3), 0.0018555879030520107_dp, 1e-9_dp, "c_e(3) does not match expected value.")

   end subroutine test_compute_scales_and_fluxes_moninobukhov_false
   !$f90tw )

   !$f90tw TESTCODE(TEST, test_atmospheric_stability, test_compute_scales_and_fluxes_moninobukhov_free_convection, test_compute_scales_and_fluxes_moninobukhov_free_convection,
   subroutine test_compute_scales_and_fluxes_moninobukhov_free_convection() bind(C)
      real(kind=dp), allocatable, dimension(:) :: wind_velocity_u, wind_velocity_v, air_temperature, dew_point_temperature, &
                                    air_pressure, charnock, surface_temperature
      type(t_scales), allocatable, dimension(:) :: scaling_parameter_array
      type(t_fluxes), allocatable, dimension(:) :: flux_array
      real(kind=dp), allocatable, dimension(:) :: wind_stress_u, wind_stress_v
      real(kind=dp), allocatable, dimension(:) :: latent_heat_flux, sensible_heat_flux
      real(kind=dp), allocatable, dimension(:) :: u_star, t_star, q_star, w_star, z0_momentum, z0_heat, z0_humidity, &
                                    obukhov_length, richardson_number, c_d, c_h, c_e
      type(t_options) :: options
      
      options%include_stability = .true.
      options%include_free_convection = .true.
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
      call get_obukhov_length(obukhov_length)
      call get_richardson_number(richardson_number)
      call get_wind_stress(wind_stress_u, wind_stress_v)
      call get_latent_heat_flux(latent_heat_flux)
      call get_sensible_heat_flux(sensible_heat_flux)
      call get_w_star(w_star)
      call get_transfer_coeff_momentum(c_d)
      call get_transfer_coeff_sensible_heat(c_h)
      call get_transfer_coeff_latent_heat(c_e)

      call f90_expect_near(u_star(1), 0.48503892913306618_dp, 1e-5_dp, "u_star(1) does not match expected value.")
      call f90_expect_near(t_star(1), -0.017768759087295315_dp, 1e-7_dp, "t_star(1) does not match expected value.")
      call f90_expect_near(q_star(1), -7.0972344359100857e-05_dp, 1e-9_dp, "q_star(1) does not match expected value.")
      call f90_expect_near(z0_momentum(1), 0.00077069986987583989_dp, 1e-7_dp, "z0_momentum(1) does not match expected value.")
      call f90_expect_near(z0_heat(1), 1.2370298329560207e-05_dp, 1e-9_dp, "z0_heat(1) does not match expected value.")
      call f90_expect_near(z0_humidity(1), 1.9173962410818321e-05_dp, 1e-9_dp, "z0_humidity(1) does not match expected value.")
      call f90_expect_near(obukhov_length(1), -578.17152640039592_dp, 1e-1_dp, "obukhov_length(1) does not match expected value.")
      call f90_expect_near(richardson_number(1), -0.0026341587450735087_dp, 1e-7_dp, "richardson_number(1) does not match expected value.")
      call f90_expect_near(wind_stress_u(1), 0.19947770559280212_dp, 1e-9_dp, "wind_stress_u(1) does not match expected value.")
      call f90_expect_near(wind_stress_v(1), 0.20831669773783273_dp, 1e-9_dp, "wind_stress_v(1) does not match expected value.")
      call f90_expect_near(latent_heat_flux(1), -105.54113795809793_dp, 1e-9_dp, "latent_heat_flux(1) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(1), -9.0992213509878361_dp, 1e-9_dp, "sensible_heat_flux(1) does not match expected value.")
      call f90_expect_near(w_star(1), 0.79467884446586934_dp, 1e-9_dp, "w_star(1) does not match expected value.")
      call f90_expect_near(c_d(1), 0.001808086366247745_dp, 1e-9_dp, "c_d(1) does not match expected value.")
      call f90_expect_near(c_h(1), 0.0014213417457323611_dp, 1e-9_dp, "c_h(1) does not match expected value.")
      call f90_expect_near(c_e(1), 0.0014753746058560921_dp, 1e-9_dp, "c_e(1) does not match expected value.")
            
      call f90_expect_near(u_star(2), 0.027461288178040878_dp, 1e-5_dp, "u_star(2) does not match expected value.")
      call f90_expect_near(t_star(2), -0.056706055294144637_dp, 1e-5_dp, "t_star(2) does not match expected value.")
      call f90_expect_near(q_star(2), -0.00013124762840313573_dp, 1e-8_dp, "q_star(2) does not match expected value.")
      call f90_expect_near(z0_momentum(2), 6.0692098657220496e-05_dp, 1e-8_dp, "z0_momentum(2) does not match expected value.")
      call f90_expect_near(z0_heat(2), 0.00021849302305211344_dp, 1e-7_dp, "z0_heat(2) does not match expected value.")
      call f90_expect_near(z0_humidity(2), 0.00033866418573077589_dp, 1e-6_dp, "z0_humidity(2) does not match expected value.")
      call f90_expect_near(obukhov_length(2), -0.7047203096046154_dp, 1e-4_dp, "obukhov_length(2) does not match expected value.")
      call f90_expect_near(richardson_number(2), -1.0983972949202758_dp, 1e-4_dp, "richardson_number(2) does not match expected value.")
      call f90_expect_near(wind_stress_u(2), 0.00022894769431259983_dp, 1e-9_dp, "wind_stress_u(2) does not match expected value.")
      call f90_expect_near(wind_stress_v(2), -0.00088524793110132129_dp, 1e-9_dp, "wind_stress_v(2) does not match expected value.")
      call f90_expect_near(latent_heat_flux(2), -10.92882865654418_dp, 1e-9_dp, "latent_heat_flux(2) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(2), -1.6260205569500936_dp, 1e-9_dp, "sensible_heat_flux(2) does not match expected value.")
      call f90_expect_near(w_star(2), 0.42203215225096907_dp, 1e-9_dp, "w_star(2) does not match expected value.")
      call f90_expect_near(c_d(2), 0.0018872824216257314_dp, 1e-9_dp, "c_d(2) does not match expected value.")
      call f90_expect_near(c_h(2), 0.0027154904332284098_dp, 1e-9_dp, "c_h(2) does not match expected value.")
      call f90_expect_near(c_e(2), 0.0029144700927910602_dp, 1e-9_dp, "c_e(2) does not match expected value.")
      
      call f90_expect_near(u_star(3), 1.2648693276621181_dp, 1e-5_dp, "u_star(3) does not match expected value.")
      call f90_expect_near(t_star(3), -0.061471648254639494_dp, 1e-7_dp, "t_star(3) does not match expected value.")
      call f90_expect_near(q_star(3), -9.231166738590414e-05_dp, 1e-9_dp, "q_star(3) does not match expected value.")
      call f90_expect_near(z0_momentum(3), 0.010342127851722125_dp, 1e-6_dp, "z0_momentum(3) does not match expected value.")
      call f90_expect_near(z0_heat(3), 4.7437736225460225e-06_dp, 1e-9_dp, "z0_heat(3) does not match expected value.")
      call f90_expect_near(z0_humidity(3), 7.3528491149463359e-06_dp, 1e-9_dp, "z0_humidity(3) does not match expected value.")
      call f90_expect_near(obukhov_length(3), -1490.2281217368877_dp, 2e-1_dp, "obukhov_length(3) does not match expected value.")
      call f90_expect_near(richardson_number(3), -0.0020764345448452777_dp, 1e-7_dp, "richardson_number(3) does not match expected value.")
      call f90_expect_near(wind_stress_u(3), 1.7056217732530459_dp, 1e-9_dp, "wind_stress_u(3) does not match expected value.")
      call f90_expect_near(wind_stress_v(3), -1.0207926096393103_dp, 1e-9_dp, "wind_stress_v(3) does not match expected value.")
      call f90_expect_near(latent_heat_flux(3), -362.78755292699174_dp, 1e-9_dp, "latent_heat_flux(3) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(3), -83.192669140537305_dp, 1e-9_dp, "sensible_heat_flux(3) does not match expected value.")
      call f90_expect_near(w_star(3), 1.5066892480385707_dp, 1e-9_dp, "w_star(3) does not match expected value.")
      call f90_expect_near(c_d(3), 0.0034107139576907354_dp, 1e-9_dp, "c_d(3) does not match expected value.")
      call f90_expect_near(c_h(3), 0.0018044207012181282_dp, 1e-9_dp, "c_h(3) does not match expected value.")
      call f90_expect_near(c_e(3), 0.0018676438346224369_dp, 1e-9_dp, "c_e(3) does not match expected value.")

   end subroutine test_compute_scales_and_fluxes_moninobukhov_free_convection
   !$f90tw )

end module test_atmospheric_stability