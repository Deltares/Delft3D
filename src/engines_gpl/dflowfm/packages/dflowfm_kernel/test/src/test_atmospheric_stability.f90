module test_atmospheric_stability
   use assertions_gtest
   use precision, only: dp
   use precision_basics, only: comparereal
   use m_atmospheric_stability, only: t_atmospheric_stability
   use m_array_or_scalar, only: t_array_or_scalar
   use m_alloc, only: realloc
   implicit none

contains

   !$gtest
   subroutine test_compute_scales_and_fluxes_moninobukhov_true() bind(C)
      real(kind=dp), allocatable, dimension(:) :: wind_velocity_u, wind_velocity_v, air_temperature, dew_point_temperature, &
                                    air_pressure, charnock, surface_temperature
      real(kind=dp), allocatable, dimension(:) :: wind_stress_u, wind_stress_v
      real(kind=dp), allocatable, dimension(:) :: latent_heat_flux, sensible_heat_flux
      real(kind=dp), allocatable, dimension(:) :: u_star, t_star, q_star, w_star, z0_momentum, z0_heat, z0_humidity, &
                                    obukhov_length, richardson_number, c_d, c_h, c_e
      type(t_array_or_scalar), target :: salinity_reduction_factor_saturation_humidity
      type(t_atmospheric_stability) :: atmospheric_stability
      
      call atmospheric_stability%initialize_atmospheric_stability( &
               include_stability=.true., &
               include_free_convection=.false., &
               sensor_height_wind_velocity=10.0_dp, &
               sensor_height_air_temperature=2.0_dp, &
               sensor_height_humidity=2.0_dp, &
               alpha_m=0.11_dp, &
               alpha_h=0.40_dp, &
               alpha_q=0.62_dp)
      wind_velocity_u = [7.869952506297818_dp, 0.117833360326781_dp, 18.539150194088315_dp]
      wind_velocity_v = [8.218675428381800_dp, -0.455614551078914_dp, -11.095442028172844_dp]
      air_temperature = [286.212334904992247_dp, 285.769323685018151_dp, 280.824297920699621_dp]
      dew_point_temperature = [283.140027966959963_dp, 283.279402521152690_dp, 275.760981569780142_dp]
      air_pressure = [101190.122711076852283_dp, 99932.877309904928552_dp, 100434.759443715374800_dp]
      charnock = [0.031984724136352_dp, 0.007887448339510_dp, 0.063390066055710_dp]
      surface_temperature = [286.743896484375000_dp, 286.676513671875000_dp, 282.813232421875000_dp]
      salinity_reduction_factor_saturation_humidity%scalar = 1.0_dp
      call realloc(salinity_reduction_factor_saturation_humidity%values, size(surface_temperature), keepexisting=.true., fill=salinity_reduction_factor_saturation_humidity%scalar)
      salinity_reduction_factor_saturation_humidity%values = [1.0_dp, 0.99_dp, 0.98_dp]
      
      call atmospheric_stability%compute_scales_and_fluxes(wind_velocity_u, wind_velocity_v, air_temperature, dew_point_temperature, &
                                  air_pressure, charnock, surface_temperature, salinity_reduction_factor_saturation_humidity)

      call atmospheric_stability%get_u_star(u_star)
      call atmospheric_stability%get_t_star(t_star)
      call atmospheric_stability%get_q_star(q_star)
      call atmospheric_stability%get_z0_momentum(z0_momentum)
      call atmospheric_stability%get_z0_heat(z0_heat)
      call atmospheric_stability%get_z0_humidity(z0_humidity)
      call atmospheric_stability%get_obukhov_length(obukhov_length)
      call atmospheric_stability%get_richardson_number(richardson_number)
      call atmospheric_stability%get_wind_stress(wind_stress_u, wind_stress_v)
      call atmospheric_stability%get_latent_heat_flux(latent_heat_flux)
      call atmospheric_stability%get_sensible_heat_flux(sensible_heat_flux)
      call atmospheric_stability%get_w_star(w_star)
      call atmospheric_stability%get_transfer_coeff_momentum(c_d)
      call atmospheric_stability%get_transfer_coeff_sensible_heat(c_h)
      call atmospheric_stability%get_transfer_coeff_latent_heat(c_e)

      call f90_expect_near(u_star(1), 0.483570419681275_dp, 1e-5_dp, "u_star(1) does not match expected value.")
      call f90_expect_near(t_star(1), -0.01777296375372503_dp, 1e-7_dp, "t_star(1) does not match expected value.")
      call f90_expect_near(q_star(1), -7.0989777521434424e-05_dp, 1e-9_dp, "q_star(1) does not match expected value.")
      call f90_expect_near(z0_momentum(1), 0.000766075070655_dp, 1e-7_dp, "z0_momentum(1) does not match expected value.")
      call f90_expect_near(z0_heat(1), 0.000012407831933_dp, 1e-9_dp, "z0_heat(1) does not match expected value.")
      call f90_expect_near(z0_humidity(1), 0.000019232139496_dp, 1e-9_dp, "z0_humidity(1) does not match expected value.")
      call f90_expect_near(obukhov_length(1), -574.507255494245101_dp, 1e-1_dp, "obukhov_length(1) does not match expected value.")
      call f90_expect_near(richardson_number(1), -0.002646973390076_dp, 1e-7_dp, "richardson_number(1) does not match expected value.")
      call f90_expect_near(wind_stress_u(1), 0.19827119540272661_dp, 1e-9_dp, "wind_stress_u(1) does not match expected value.")
      call f90_expect_near(wind_stress_v(1), 0.2070567262646465_dp, 1e-9_dp, "wind_stress_v(1) does not match expected value.")
      call f90_expect_near(latent_heat_flux(1), -105.24793297696182_dp, 1e-9_dp, "latent_heat_flux(1) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(1), -10.5861689901751_dp, 1e-9_dp, "sensible_heat_flux(1) does not match expected value.")
      call f90_expect_near(w_star(1), 0.0_dp, 1e-9_dp, "w_star(1) does not match expected value.")
      call f90_expect_near(c_d(1), 0.0018059159533254409_dp, 1e-9_dp, "c_d(1) does not match expected value.")
      call f90_expect_near(c_h(1), 0.0014208689934920476_dp, 1e-9_dp, "c_h(1) does not match expected value.")
      call f90_expect_near(c_e(1), 0.0014748989040242813_dp, 1e-9_dp, "c_e(1) does not match expected value.")
            
      call f90_expect_near(u_star(2), 0.021959777501879552_dp, 1e-5_dp, "u_star(2) does not match expected value.")
      call f90_expect_near(t_star(2), -0.063662148797144533_dp, 1e-5_dp, "t_star(2) does not match expected value.")
      call f90_expect_near(q_star(2), -0.00014127432406157292_dp, 1e-8_dp, "q_star(2) does not match expected value.")
      call f90_expect_near(z0_momentum(2), 0.000075524194341495605_dp, 1e-8_dp, "z0_momentum(2) does not match expected value.")
      call f90_expect_near(z0_heat(2), 0.00027322300528684709_dp, 1e-7_dp, "z0_heat(2) does not match expected value.")
      call f90_expect_near(z0_humidity(2), 0.000423030227538_dp, 1e-6_dp, "z0_humidity(2) does not match expected value.")
      call f90_expect_near(obukhov_length(2), -0.40690796328902146_dp, 1e-4_dp, "obukhov_length(2) does not match expected value.")
      call f90_expect_near(richardson_number(2), -1.9479139245561901_dp, 1e-4_dp, "richardson_number(2) does not match expected value.")
      call f90_expect_near(wind_stress_u(2), 0.00014640312061274798_dp, 1e-9_dp, "wind_stress_u(2) does not match expected value.")
      call f90_expect_near(wind_stress_v(2), -0.00056608240560690373_dp, 1e-9_dp, "wind_stress_v(2) does not match expected value.")
      call f90_expect_near(latent_heat_flux(2), -9.4069781743218481_dp, 1e-9_dp, "latent_heat_flux(2) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(2), -1.7030586821710916_dp, 1e-9_dp, "sensible_heat_flux(2) does not match expected value.")
      call f90_expect_near(w_star(2), 0.0_dp, 1e-9_dp, "w_star(2) does not match expected value.")
      call f90_expect_near(c_d(2), 0.0021774187402240175_dp, 1e-9_dp, "c_d(2) does not match expected value.")
      call f90_expect_near(c_h(2), 0.0032745471807638675_dp, 1e-9_dp, "c_h(2) does not match expected value.")
      call f90_expect_near(c_e(2), 0.0035453130945900043_dp, 1e-9_dp, "c_e(2) does not match expected value.")

      call f90_expect_near(u_star(3), 1.2605257576398083_dp, 1e-5_dp, "u_star(3) does not match expected value.")
      call f90_expect_near(t_star(3), -0.06146839114230266_dp, 1e-7_dp, "t_star(3) does not match expected value.")
      call f90_expect_near(q_star(3), -8.7533093397411743e-05_dp, 1e-9_dp, "q_star(3) does not match expected value.")
      call f90_expect_near(z0_momentum(3), 0.010271257861220856_dp, 1e-6_dp, "z0_momentum(3) does not match expected value.")
      call f90_expect_near(z0_heat(3), 4.7598600384773734e-06_dp, 1e-9_dp, "z0_heat(3) does not match expected value.")
      call f90_expect_near(z0_humidity(3), 7.3777830596399287e-06_dp, 1e-9_dp, "z0_humidity(3) does not match expected value.")
      call f90_expect_near(obukhov_length(3), -1494.8705407521311_dp, 2e-1_dp, "obukhov_length(3) does not match expected value.")
      call f90_expect_near(richardson_number(3), -0.0020643714220382331_dp, 1e-7_dp, "richardson_number(3) does not match expected value.")
      call f90_expect_near(wind_stress_u(3), 1.693926057209548_dp, 1e-9_dp, "wind_stress_u(3) does not match expected value.")
      call f90_expect_near(wind_stress_v(3), -1.0137928745932032_dp, 1e-9_dp, "wind_stress_v(3) does not match expected value.")
      call f90_expect_near(latent_heat_flux(3), -342.82613139021657_dp, 1e-9_dp, "latent_heat_flux(3) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(3), -96.719645190260536_dp, 1e-9_dp, "sensible_heat_flux(3) does not match expected value.")
      call f90_expect_near(w_star(3), 0.0_dp, 1e-9_dp, "w_star(3) does not match expected value.")
      call f90_expect_near(c_d(3), 0.0034037988743427436_dp, 1e-9_dp, "c_d(3) does not match expected value.")
      call f90_expect_near(c_h(3), 0.001803070418981471_dp, 1e-9_dp, "c_h(3) does not match expected value.")
      call f90_expect_near(c_e(3), 0.0018662636169953025_dp, 1e-9_dp, "c_e(3) does not match expected value.")

   end subroutine test_compute_scales_and_fluxes_moninobukhov_true

   !$gtest
   subroutine test_compute_scales_and_fluxes_moninobukhov_false() bind(C)
      real(kind=dp), allocatable, dimension(:) :: wind_velocity_u, wind_velocity_v, air_temperature, dew_point_temperature, &
                                    air_pressure, charnock, surface_temperature
      real(kind=dp), allocatable, dimension(:) :: wind_stress_u, wind_stress_v
      real(kind=dp), allocatable, dimension(:) :: latent_heat_flux, sensible_heat_flux
      real(kind=dp), allocatable, dimension(:) :: u_star, t_star, q_star, w_star, z0_momentum, z0_heat, z0_humidity, &
                                    obukhov_length, richardson_number, c_d, c_h, c_e
      type(t_array_or_scalar), target :: salinity_reduction_factor_saturation_humidity
      type(t_atmospheric_stability) :: atmospheric_stability
      
      call atmospheric_stability%initialize_atmospheric_stability( &
               include_stability=.false., &
               include_free_convection=.false., &
               sensor_height_wind_velocity=10.0_dp, &
               sensor_height_air_temperature=2.0_dp, &
               sensor_height_humidity=2.0_dp, &
               alpha_m=0.11_dp, &
               alpha_h=0.40_dp, &
               alpha_q=0.62_dp)
      wind_velocity_u = [7.869952506297818_dp, 0.117833360326781_dp, 18.539150194088315_dp]
      wind_velocity_v = [8.218675428381800_dp, -0.455614551078914_dp, -11.095442028172844_dp]
      air_temperature = [286.212334904992247_dp, 285.769323685018151_dp, 280.824297920699621_dp]
      dew_point_temperature = [283.140027966959963_dp, 283.279402521152690_dp, 275.760981569780142_dp]
      air_pressure = [101190.122711076852283_dp, 99932.877309904928552_dp, 100434.759443715374800_dp]
      charnock = [0.031984724136352_dp, 0.007887448339510_dp, 0.063390066055710_dp]
      surface_temperature = [286.743896484375000_dp, 286.676513671875000_dp, 282.813232421875000_dp]
      salinity_reduction_factor_saturation_humidity%scalar = 1.0_dp
      
      call atmospheric_stability%compute_scales_and_fluxes(wind_velocity_u, wind_velocity_v, air_temperature, dew_point_temperature, &
                                  air_pressure, charnock, surface_temperature, salinity_reduction_factor_saturation_humidity)
   
      call atmospheric_stability%get_u_star(u_star)
      call atmospheric_stability%get_t_star(t_star)
      call atmospheric_stability%get_q_star(q_star)
      call atmospheric_stability%get_z0_momentum(z0_momentum)
      call atmospheric_stability%get_z0_heat(z0_heat)
      call atmospheric_stability%get_z0_humidity(z0_humidity)
      call atmospheric_stability%get_obukhov_length(obukhov_length)
      call atmospheric_stability%get_richardson_number(richardson_number)
      call atmospheric_stability%get_wind_stress(wind_stress_u, wind_stress_v)
      call atmospheric_stability%get_latent_heat_flux(latent_heat_flux)
      call atmospheric_stability%get_sensible_heat_flux(sensible_heat_flux)
      call atmospheric_stability%get_w_star(w_star)
      call atmospheric_stability%get_transfer_coeff_momentum(c_d)
      call atmospheric_stability%get_transfer_coeff_sensible_heat(c_h)
      call atmospheric_stability%get_transfer_coeff_latent_heat(c_e)

      call f90_expect_near(u_star(1), 0.479416158782993_dp, 1e-5_dp, "u_star(1) does not match expected value.")
      call f90_expect_near(t_star(1), -0.017745271690219569_dp, 1e-7_dp, "t_star(1) does not match expected value.")
      call f90_expect_near(q_star(1), -7.0874970945790985e-05_dp, 1e-9_dp, "q_star(1) does not match expected value.")
      call f90_expect_near(z0_momentum(1), 0.00075305723822398493_dp, 1e-7_dp, "z0_momentum(1) does not match expected value.")
      call f90_expect_near(z0_heat(1), 0.000012516200200_dp, 1e-9_dp, "z0_heat(1) does not match expected value.")
      call f90_expect_near(z0_humidity(1), 1.9398789540972799e-05_dp, 1e-9_dp, "z0_humidity(1) does not match expected value.")
      call f90_expect_near(obukhov_length(1), 0.0_dp, 0.0_dp, "obukhov_length(1) does not match expected value.")
      call f90_expect_near(richardson_number(1), 0.0_dp, 0.0_dp, "richardson_number(1) does not match expected value.")
      call f90_expect_near(wind_stress_u(1), 0.19488616345208651_dp, 1e-9_dp, "wind_stress_u(1) does not match expected value.")
      call f90_expect_near(wind_stress_v(1), 0.20352170125722105_dp, 1e-9_dp, "wind_stress_v(1) does not match expected value.")
      call f90_expect_near(latent_heat_flux(1), -104.17687951224615_dp, 1e-9_dp, "latent_heat_flux(1) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(1), -10.479059610964882_dp, 1e-9_dp, "sensible_heat_flux(1) does not match expected value.")
      call f90_expect_near(w_star(1),0.0_dp, 1e-9_dp, "w_star(1) does not match expected value.")
      call f90_expect_near(c_d(1), 0.0017750835906934265_dp, 1e-9_dp, "c_d(1) does not match expected value.")
      call f90_expect_near(c_h(1), 0.0014064919051007996_dp, 1e-9_dp, "c_h(1) does not match expected value.")
      call f90_expect_near(c_e(1), 0.0014598885991239116_dp, 1e-9_dp, "c_e(1) does not match expected value.")

      call f90_expect_near(u_star(2), 0.016365185405138_dp, 1e-5_dp, "u_star(2) does not match expected value.")
      call f90_expect_near(t_star(2), -0.04217341834728125_dp, 1e-5_dp, "t_star(2) does not match expected value.")
      call f90_expect_near(q_star(2), -9.5827471462144284e-05_dp, 1e-8_dp, "q_star(2) does not match expected value.")
      call f90_expect_near(z0_momentum(2), 0.000101038350813_dp, 1e-8_dp, "z0_momentum(2) does not match expected value.")
      call f90_expect_near(z0_heat(2), 0.000366628877207_dp, 1e-7_dp, "z0_heat(2) does not match expected value.")
      call f90_expect_near(z0_humidity(2), 0.000568274759671_dp, 1e-6_dp, "z0_humidity(2) does not match expected value.")
      call f90_expect_near(obukhov_length(2), 0.0_dp, 0.0_dp, "obukhov_length(2) does not match expected value.")
      call f90_expect_near(richardson_number(2), 0.0_dp, 0.0_dp, "richardson_number(2) does not match expected value.")
      call f90_expect_near(wind_stress_u(2), 8.130856993426958e-05_dp, 1e-9_dp, "wind_stress_u(2) does not match expected value.")
      call f90_expect_near(wind_stress_v(2), -0.00031438777173743298_dp, 1e-9_dp, "wind_stress_v(2) does not match expected value.")
      call f90_expect_near(latent_heat_flux(2), -4.7552412416935423_dp, 1e-9_dp, "latent_heat_flux(2) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(2), -0.84078031801719288_dp, 1e-9_dp, "sensible_heat_flux(2) does not match expected value.")
      call f90_expect_near(w_star(2), 0.0_dp, 1e-9_dp, "w_star(2) does not match expected value.")
      call f90_expect_near(c_d(2), 0.0012092819913657618_dp, 1e-9_dp, "c_d(2) does not match expected value.")
      call f90_expect_near(c_h(2), 0.0016166103144087832_dp, 1e-9_dp, "c_h(2) does not match expected value.")
      call f90_expect_near(c_e(2), 0.001703369873101712_dp, 1e-9_dp, "c_e(2) does not match expected value.")

      call f90_expect_near(u_star(3), 1.2538467592669331_dp, 1e-5_dp, "u_star(3) does not match expected value.")
      call f90_expect_near(t_star(3), -0.061443216883290658_dp, 1e-7_dp, "t_star(3) does not match expected value.")
      call f90_expect_near(q_star(3), -9.2267476891948934e-05_dp, 1e-9_dp, "q_star(3) does not match expected value.")
      call f90_expect_near(z0_momentum(3), 0.010162697710431663_dp, 1e-6_dp, "z0_momentum(3) does not match expected value.")
      call f90_expect_near(z0_heat(3), 0.000004784484715_dp, 1e-9_dp, "z0_heat(3) does not match expected value.")
      call f90_expect_near(z0_humidity(3), 7.4174876197341148e-06_dp, 1e-9_dp, "z0_humidity(3) does not match expected value.")
      call f90_expect_near(obukhov_length(3), 0.0_dp, 0.0_dp, "obukhov_length(3) does not match expected value.")
      call f90_expect_near(richardson_number(3), 0.0_dp, 0.0_dp, "richardson_number(3) does not match expected value.")
      call f90_expect_near(wind_stress_u(3), 1.6760243824409651_dp, 1e-9_dp, "wind_stress_u(3) does not match expected value.")
      call f90_expect_near(wind_stress_v(3), -1.003078953376612_dp, 1e-9_dp, "wind_stress_v(3) does not match expected value.")
      call f90_expect_near(latent_heat_flux(3), -359.4539231151748_dp, 1e-9_dp, "latent_heat_flux(3) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(3), -96.167812106680884_dp, 1e-9_dp, "sensible_heat_flux(3) does not match expected value.")
      call f90_expect_near(w_star(3), 0.0_dp, 1e-9_dp, "w_star(3) does not match expected value.")
      call f90_expect_near(c_d(3), 0.0033678269969595418_dp, 1e-9_dp, "c_d(3) does not match expected value.")
      call f90_expect_near(c_h(3), 0.0017927819323621376_dp, 1e-9_dp, "c_h(3) does not match expected value.")
      call f90_expect_near(c_e(3), 0.0018555879030520107_dp, 1e-9_dp, "c_e(3) does not match expected value.")

   end subroutine test_compute_scales_and_fluxes_moninobukhov_false

   !$gtest
   subroutine test_compute_scales_and_fluxes_moninobukhov_free_convection() bind(C)
      real(kind=dp), allocatable, dimension(:) :: wind_velocity_u, wind_velocity_v, air_temperature, dew_point_temperature, &
                                    air_pressure, charnock, surface_temperature
      real(kind=dp), allocatable, dimension(:) :: wind_stress_u, wind_stress_v
      real(kind=dp), allocatable, dimension(:) :: latent_heat_flux, sensible_heat_flux
      real(kind=dp), allocatable, dimension(:) :: u_star, t_star, q_star, w_star, z0_momentum, z0_heat, z0_humidity, &
                                    obukhov_length, richardson_number, c_d, c_h, c_e
      type(t_array_or_scalar), target :: salinity_reduction_factor_saturation_humidity
      type(t_atmospheric_stability) :: atmospheric_stability
      
      call atmospheric_stability%initialize_atmospheric_stability( &
               include_stability=.true., &
               include_free_convection=.true., &
               sensor_height_wind_velocity=10.0_dp, &
               sensor_height_air_temperature=2.0_dp, &
               sensor_height_humidity=2.0_dp, &
               alpha_m=0.11_dp, &
               alpha_h=0.40_dp, &
               alpha_q=0.62_dp)
      wind_velocity_u = [7.869952506297818_dp, 0.117833360326781_dp, 18.539150194088315_dp]
      wind_velocity_v = [8.218675428381800_dp, -0.455614551078914_dp, -11.095442028172844_dp]
      air_temperature = [286.212334904992247_dp, 285.769323685018151_dp, 280.824297920699621_dp]
      dew_point_temperature = [283.140027966959963_dp, 283.279402521152690_dp, 275.760981569780142_dp]
      air_pressure = [101190.122711076852283_dp, 99932.877309904928552_dp, 100434.759443715374800_dp]
      charnock = [0.031984724136352_dp, 0.007887448339510_dp, 0.063390066055710_dp]
      surface_temperature = [286.743896484375000_dp, 286.676513671875000_dp, 282.813232421875000_dp]
      salinity_reduction_factor_saturation_humidity%scalar = 1.0_dp
      
      call atmospheric_stability%compute_scales_and_fluxes(wind_velocity_u, wind_velocity_v, air_temperature, dew_point_temperature, &
                                  air_pressure, charnock, surface_temperature, salinity_reduction_factor_saturation_humidity)

      call atmospheric_stability%get_u_star(u_star)
      call atmospheric_stability%get_t_star(t_star)
      call atmospheric_stability%get_q_star(q_star)
      call atmospheric_stability%get_z0_momentum(z0_momentum)
      call atmospheric_stability%get_z0_heat(z0_heat)
      call atmospheric_stability%get_z0_humidity(z0_humidity)
      call atmospheric_stability%get_obukhov_length(obukhov_length)
      call atmospheric_stability%get_richardson_number(richardson_number)
      call atmospheric_stability%get_wind_stress(wind_stress_u, wind_stress_v)
      call atmospheric_stability%get_latent_heat_flux(latent_heat_flux)
      call atmospheric_stability%get_sensible_heat_flux(sensible_heat_flux)
      call atmospheric_stability%get_w_star(w_star)
      call atmospheric_stability%get_transfer_coeff_momentum(c_d)
      call atmospheric_stability%get_transfer_coeff_sensible_heat(c_h)
      call atmospheric_stability%get_transfer_coeff_latent_heat(c_e)

      call f90_expect_near(u_star(1), 0.48503892913306618_dp, 1e-5_dp, "u_star(1) does not match expected value.")
      call f90_expect_near(t_star(1), -0.017768202231609759_dp, 1e-7_dp, "t_star(1) does not match expected value.")
      call f90_expect_near(q_star(1), -7.0970035596894525e-05_dp, 1e-9_dp, "q_star(1) does not match expected value.")
      call f90_expect_near(z0_momentum(1), 0.00077069986987583989_dp, 1e-7_dp, "z0_momentum(1) does not match expected value.")
      call f90_expect_near(z0_heat(1), 1.2370298329560207e-05_dp, 1e-9_dp, "z0_heat(1) does not match expected value.")
      call f90_expect_near(z0_humidity(1), 1.9173962410818321e-05_dp, 1e-9_dp, "z0_humidity(1) does not match expected value.")
      call f90_expect_near(obukhov_length(1), -578.17152640039592_dp, 1e-1_dp, "obukhov_length(1) does not match expected value.")
      call f90_expect_near(richardson_number(1), -0.0026341587450735087_dp, 1e-7_dp, "richardson_number(1) does not match expected value.")
      call f90_expect_near(wind_stress_u(1), 0.19947763550406267_dp, 1e-9_dp, "wind_stress_u(1) does not match expected value.")
      call f90_expect_near(wind_stress_v(1), 0.20831662454341379_dp, 1e-9_dp, "wind_stress_v(1) does not match expected value.")
      call f90_expect_near(latent_heat_flux(1), -105.53829567292469_dp, 1e-9_dp, "latent_heat_flux(1) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(1), -10.615482756260775_dp, 1e-9_dp, "sensible_heat_flux(1) does not match expected value.")
      call f90_expect_near(w_star(1), 0.79467186098143938_dp, 1e-9_dp, "w_star(1) does not match expected value.")
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
      call f90_expect_near(wind_stress_v(2), -0.00088524652091501188_dp, 1e-9_dp, "wind_stress_v(2) does not match expected value.")
      call f90_expect_near(latent_heat_flux(2), -10.92877375112421_dp, 1e-9_dp, "latent_heat_flux(2) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(2), -1.8970149995995667_dp, 1e-9_dp, "sensible_heat_flux(2) does not match expected value.")
      call f90_expect_near(w_star(2), 0.42203147368164945_dp, 1e-9_dp, "w_star(2) does not match expected value.")
      call f90_expect_near(c_d(2), 0.0018872824216257314_dp, 1e-9_dp, "c_d(2) does not match expected value.")
      call f90_expect_near(c_h(2), 0.0027154904332284098_dp, 1e-9_dp, "c_h(2) does not match expected value.")
      call f90_expect_near(c_e(2), 0.0029144700927910602_dp, 1e-9_dp, "c_e(2) does not match expected value.")
      
      call f90_expect_near(u_star(3), 1.2648693276621181_dp, 1e-5_dp, "u_star(3) does not match expected value.")
      call f90_expect_near(t_star(3), -0.061452067922694101_dp, 1e-7_dp, "t_star(3) does not match expected value.")
      call f90_expect_near(q_star(3), -9.2281233469172454e-05_dp, 1e-9_dp, "q_star(3) does not match expected value.")
      call f90_expect_near(z0_momentum(3), 0.010342127851722125_dp, 1e-6_dp, "z0_momentum(3) does not match expected value.")
      call f90_expect_near(z0_heat(3), 4.7437736225460225e-06_dp, 1e-9_dp, "z0_heat(3) does not match expected value.")
      call f90_expect_near(z0_humidity(3), 7.3528491149463359e-06_dp, 1e-9_dp, "z0_humidity(3) does not match expected value.")
      call f90_expect_near(obukhov_length(3), -1490.2281217368877_dp, 2e-1_dp, "obukhov_length(3) does not match expected value.")
      call f90_expect_near(richardson_number(3), -0.0020757523818464301_dp, 1e-7_dp, "richardson_number(3) does not match expected value.")
      call f90_expect_near(wind_stress_u(3), 1.7056176728243093_dp, 1e-9_dp, "wind_stress_u(3) does not match expected value.")
      call f90_expect_near(wind_stress_v(3), -1.0207901555856531_dp, 1e-9_dp, "wind_stress_v(3) does not match expected value.")
      call f90_expect_near(latent_heat_flux(3), -362.667510804796_dp, 1e-9_dp, "latent_heat_flux(3) does not match expected value.")
      call f90_expect_near(sensible_heat_flux(3), -97.027081812817144_dp, 1e-9_dp, "sensible_heat_flux(3) does not match expected value.")
      call f90_expect_near(w_star(3), 1.5065268173434616_dp, 1e-9_dp, "w_star(3) does not match expected value.")
      call f90_expect_near(c_d(3), 0.0034107093168529892_dp, 1e-9_dp, "c_d(3) does not match expected value.")
      call f90_expect_near(c_h(3), 0.0018044195080504995_dp, 1e-9_dp, "c_h(3) does not match expected value.")
      call f90_expect_near(c_e(3), 0.0018676426008978233_dp, 1e-9_dp, "c_e(3) does not match expected value.")

   end subroutine test_compute_scales_and_fluxes_moninobukhov_free_convection

end module test_atmospheric_stability
