module test_ec_module_data_value
   use precision, only: dp
   use iso_c_utils, only: cstr
   use assertions_gtest
   use m_file_helpers, only: create_scratch_file
   implicit none(type, external)
   private
contains

   !$f90tw TESTCODE(TEST, test_ec_module_data_value, 
   !$f90tw test_data_value__windx_windy, test_data_value__windx_windy,
   subroutine test_data_value__windx_windy() bind(C)
      use m_sferic, only: jsferic
      use m_meteo, only: initialize_ec_module, ecInstancePtr, ec_addtimespacerelation, item_windx, item_windy, item_windxy_x, item_windxy_y, ec_gettimespacevalue
      use m_flowtimes, only: refdate_mjd, tzone, tunit
      use time_module, only: ymd2modified_jul
      use timespace_parameters, only: BCASCII, DATAVALUE, OPERAND_OVERRIDE, OPERAND_MULTIPLY
      use timespace_data, only: SPACEANDTIME, JUSTUPDATE
      use m_file_helpers, only: create_file
      use precision, only: dp
      use timespace_read, only: MAXNAMELEN
      use unstruc_messages, only: callback_msg
      use MessageHandling, only: LEVEL_DEBUG
      use m_ec_instance, only: ecInstancePrintState
      use m_wind, only: wx, wy
      use m_missing, only: dmiss

      character(len=*), parameter :: BC_FILE = "test_data_value__windx_windy.bc"
      integer, parameter :: IREFDATE = 20000101
      real(dp), parameter :: WINDSPEEDFACTOR = 0.8_dp
      character(len=MAXNAMELEN) :: quantity_name
      real(dp) :: x(1), y(1)
      integer  :: mask(1)
      logical  :: ok, mjd_ok

      ! Arrange
      allocate(wx(1), wy(1))
      wx = dmiss
      wy = dmiss

      call create_file(BC_FILE, [ &
         "[General]", &
         "fileVersion = 1.01", &
         "fileType = boundConds", &
         "", &
         "[forcing]", &
         "name = global", &  ! Windx value is applied uniformly over all of space
         "function = timeseries", &
         "timeInterpolation = linear", &
         "quantity = time", &
         "unit = seconds since 2000-01-01", &
         "quantity = windx", &
         "unit = m s-1", &
         "0 -1.0", &
         "100 3.0", &
         "", &
         "[forcing]", &
         "name = global", &  ! Windy value is applied globally over all of space
         "function = timeseries", &
         "timeInterpolation = linear", &
         "quantity = time", &
         "unit = seconds since 2000-01-01", &
         "quantity = windy", &
         "unit = m s-1", &
         "0 -4.0", &
         "100 2.0" &
      ])

      mjd_ok = ymd2modified_jul(IREFDATE, refdate_mjd)
      tzone  = 0.0_dp
      jsferic = 0
      call initialize_ec_module()

      x = 0.0_dp
      y = 0.0_dp
      mask = 1

      ! Act
      quantity_name = "windx"
      ok = ec_addtimespacerelation(quantity_name, x, y, mask, 1, "global", &
         BCASCII, SPACEANDTIME, OPERAND_OVERRIDE, forcingfile=BC_FILE)
      call F90_EXPECT_TRUE(ok, cstr("ec_addtimespacerelation failed for windx"))

      quantity_name = "windy"
      ok = ec_addtimespacerelation(quantity_name, x, y, mask, 1, "global", &
         BCASCII, SPACEANDTIME, OPERAND_OVERRIDE, forcingfile=BC_FILE)
      call F90_EXPECT_TRUE(ok, cstr("ec_addtimespacerelation failed for windy"))

      quantity_name = "windx"
      ok = ec_addtimespacerelation(quantity_name, x, y, mask, 1, "", &
         DATAVALUE, JUSTUPDATE, OPERAND_MULTIPLY, data_value=WINDSPEEDFACTOR)

      quantity_name = "windy"
      ok = ec_addtimespacerelation(quantity_name, x, y, mask, 1, "", &
         DATAVALUE, JUSTUPDATE, OPERAND_MULTIPLY, data_value=WINDSPEEDFACTOR)

      ! Assert
      ok = ec_gettimespacevalue(ecInstancePtr, item_windx, IREFDATE, tzone, tunit, 50.0_dp)
      call f90_expect_near(wx(1), WINDSPEEDFACTOR, 1.0e-6_dp, cstr("windxy_x@50")) ! 1.0 is halfway between -1.0 and 3.0

      ok = ec_gettimespacevalue(ecInstancePtr, item_windy, IREFDATE, tzone, tunit, 50.0_dp)
      call f90_expect_near(wy(1), -WINDSPEEDFACTOR, 1.0e-6_dp, cstr("windxy_y@50")) ! -1.0 is halfway between -4.0 and 2.0

      deallocate(wx, wy)
   end subroutine test_data_value__windx_windy
   !$f90tw )

   !$f90tw TESTCODE(TEST, test_ec_module_data_value,
   !$f90tw test_data_value__multiply_ignores_undefined_masked_target, test_data_value__multiply_ignores_undefined_masked_target,
   subroutine test_data_value__multiply_ignores_undefined_masked_target() bind(C)
      use m_ec_parameters, only: ec_undef_hp
      use m_flowtimes, only: tunit, tzone
      use m_meteo, only: ecInstancePtr, ec_addtimespacerelation, ec_gettimespacevalue, initialize_ec_module, item_windx
      use m_sferic, only: jsferic
      use m_wind, only: wx
      use timespace_data, only: JUSTUPDATE
      use timespace_parameters, only: DATAVALUE, OPERAND_MULTIPLY
      use timespace_read, only: MAXNAMELEN

      integer, parameter :: IREFDATE = 20000101
      real(dp), parameter :: FACTOR = 0.5_dp
      character(len=MAXNAMELEN) :: quantity_name
      real(dp) :: x(2), y(2)
      integer :: mask(2)
      logical :: success

      allocate (wx(2))
      wx = [100.0_dp, ec_undef_hp]
      x = 0.0_dp
      y = 0.0_dp
      mask = [1, 0]
      tzone = 0.0_dp
      jsferic = 0
      call initialize_ec_module()

      quantity_name = 'windx'
      success = ec_addtimespacerelation(quantity_name, x, y, mask, 1, '', &
                                        DATAVALUE, JUSTUPDATE, OPERAND_MULTIPLY, data_value=FACTOR)
      call f90_expect_true(success, cstr('ec_addtimespacerelation failed for masked dataValue'))

      success = ec_gettimespacevalue(ecInstancePtr, item_windx, IREFDATE, tzone, tunit, 0.0_dp)
      call f90_expect_true(success, cstr('multiply should ignore undefined masked targets'))
      call f90_expect_near(wx(1), 50.0_dp, 1.0e-6_dp, cstr('active target should be multiplied'))
      call f90_expect_eq(wx(2), ec_undef_hp, cstr('inactive target should remain undefined'))

      deallocate (wx)
   end subroutine test_data_value__multiply_ignores_undefined_masked_target
   !$f90tw )

   !$f90tw TESTCODE(TEST, test_ec_module_data_value,
   !$f90tw test_data_value__windxy, test_data_value__windxy,
   subroutine test_data_value__windxy() bind(C)
      use m_sferic, only: jsferic
      use m_meteo, only: initialize_ec_module, ecInstancePtr, ec_addtimespacerelation, item_windxy_x, item_windxy_y, ec_gettimespacevalue
      use m_flowtimes, only: refdate_mjd, tzone, tunit
      use time_module, only: ymd2modified_jul
      use timespace_parameters, only: BCASCII, DATAVALUE, OPERAND_OVERRIDE, OPERAND_MULTIPLY
      use timespace_data, only: SPACEANDTIME, JUSTUPDATE
      use m_file_helpers, only: create_file
      use precision, only: dp
      use timespace_read, only: MAXNAMELEN
      use m_wind, only: wx, wy
      use m_missing, only: dmiss

      character(len=*), parameter :: BC_FILE = "test_data_value__windxy.bc"
      integer, parameter :: IREFDATE = 20000101
      real(dp), parameter :: WINDSPEEDFACTOR = 0.8_dp
      character(len=MAXNAMELEN) :: quantity_name

      real(dp) :: x(1), y(1)
      integer  :: mask(1)
      logical  :: ok, mjd_ok

      ! Arrange
      allocate(wx(1), wy(1))
      wx = dmiss
      wy = dmiss

      call create_file(BC_FILE, [ &
         "[General]", &
         "fileVersion = 1.01", &
         "fileType = boundConds", &
         "", &
         "[forcing]", &
         "name = global", &  ! Windxy value is applied uniformly over all of space
         "function = timeseries", &
         "timeInterpolation = linear", &
         "vector = windxy:wx,wy", &
         "quantity = time", &
         "unit = seconds since 2000-01-01", &
         "quantity = wx", &
         "unit = m s-1", &
         "quantity = wy", &
         "unit = m s-1", &
         "0 -1.0 -4.0", &
         "100 3.0 2.0" &
      ])

      mjd_ok = ymd2modified_jul(IREFDATE, refdate_mjd)
      tzone  = 0.0_dp
      jsferic = 0
      call initialize_ec_module()

      x = 0.0_dp
      y = 0.0_dp
      mask = 1

      ! Act
      quantity_name = "windxy"
      ok = ec_addtimespacerelation(quantity_name, x, y, mask, 1, "global", &
         BCASCII, SPACEANDTIME, OPERAND_OVERRIDE, forcingfile=BC_FILE)
      call F90_EXPECT_TRUE(ok, cstr("ec_addtimespacerelation failed for windxy"))

      ok = ec_addtimespacerelation(quantity_name, x, y, mask, 1, "", &
         DATAVALUE, JUSTUPDATE, OPERAND_MULTIPLY, data_value=WINDSPEEDFACTOR)
      call F90_EXPECT_TRUE(ok, cstr("ec_addtimespacerelation failed for windxy dataValue"))

      ! Assert
      ok = ec_gettimespacevalue(ecInstancePtr, item_windxy_x, IREFDATE, tzone, tunit, 50.0_dp)
      call f90_expect_near(wx(1), WINDSPEEDFACTOR, 1.0e-6_dp, cstr("windxy_x@50")) ! 0.8 * halfway between -1.0 and 3.0
      call f90_expect_near(wy(1), -WINDSPEEDFACTOR, 1.0e-6_dp, cstr("windxy_y@50")) ! 0.8 * halfway between -4.0 and 2.0

      deallocate(wx, wy)
   end subroutine test_data_value__windxy
   !$f90tw )

   !$f90tw TESTCODE(TEST, test_ec_module_data_value,
   !$f90tw test_data_value__windx_windy_in_bc__windxy_datavalue, test_data_value__windx_windy_in_bc__windxy_datavalue,
   subroutine test_data_value__windx_windy_in_bc__windxy_datavalue() bind(C)
      use m_sferic, only: jsferic
      use m_meteo, only: initialize_ec_module, ecInstancePtr, ec_addtimespacerelation, item_windx, item_windy, item_windxy_x, ec_gettimespacevalue
      use m_flowtimes, only: refdate_mjd, tzone, tunit
      use time_module, only: ymd2modified_jul
      use timespace_parameters, only: BCASCII, DATAVALUE, OPERAND_OVERRIDE, OPERAND_MULTIPLY
      use timespace_data, only: SPACEANDTIME, JUSTUPDATE
      use m_file_helpers, only: create_file
      use precision, only: dp
      use timespace_read, only: MAXNAMELEN
      use m_wind, only: wx, wy
      use m_missing, only: dmiss

      character(len=*), parameter :: BC_FILE = "test_data_value__windx_windy_in_bc__windxy_datavalue.bc"
      integer, parameter :: IREFDATE = 20000101
      real(dp), parameter :: WINDSPEEDFACTOR = 0.8_dp
      character(len=MAXNAMELEN) :: quantity_name

      real(dp) :: x(1), y(1)
      integer  :: mask(1)
      logical  :: ok, mjd_ok

      ! Arrange
      allocate(wx(1), wy(1))
      wx = dmiss
      wy = dmiss

      call create_file(BC_FILE, [ &
         "[General]", &
         "fileVersion = 1.01", &
         "fileType = boundConds", &
         "", &
         "[forcing]", &
         "name = global", &  ! Windx value is applied uniformly over all of space
         "function = timeseries", &
         "timeInterpolation = linear", &
         "quantity = time", &
         "unit = seconds since 2000-01-01", &
         "quantity = windx", &
         "unit = m s-1", &
         "0 -1.0", &
         "100 3.0", &
         "", &
         "[forcing]", &
         "name = global", &  ! Windy value is applied globally over all of space
         "function = timeseries", &
         "timeInterpolation = linear", &
         "quantity = time", &
         "unit = seconds since 2000-01-01", &
         "quantity = windy", &
         "unit = m s-1", &
         "0 -4.0", &
         "100 2.0" &
      ])

      mjd_ok = ymd2modified_jul(IREFDATE, refdate_mjd)
      tzone  = 0.0_dp
      jsferic = 0
      call initialize_ec_module()

      x = 0.0_dp
      y = 0.0_dp
      mask = 1

      ! Act
      quantity_name = "windx"
      ok = ec_addtimespacerelation(quantity_name, x, y, mask, 1, "global", &
         BCASCII, SPACEANDTIME, OPERAND_OVERRIDE, forcingfile=BC_FILE)
      call F90_EXPECT_TRUE(ok, cstr("ec_addtimespacerelation failed for windx"))

      quantity_name = "windy"
      ok = ec_addtimespacerelation(quantity_name, x, y, mask, 1, "global", &
         BCASCII, SPACEANDTIME, OPERAND_OVERRIDE, forcingfile=BC_FILE)
      call F90_EXPECT_TRUE(ok, cstr("ec_addtimespacerelation failed for windy"))

      quantity_name = "windxy"
      ok = ec_addtimespacerelation(quantity_name, x, y, mask, 1, "", &
         DATAVALUE, JUSTUPDATE, OPERAND_MULTIPLY, data_value=WINDSPEEDFACTOR)
      call F90_EXPECT_TRUE(ok, cstr("ec_addtimespacerelation failed for windxy dataValue"))

      ! Assert
      ! Get `item_windx` and `item_windy` first. Here the `WINDSPEEDFACTOR` data value is not applied yet.
      ok = ec_gettimespacevalue(ecInstancePtr, item_windx, IREFDATE, tzone, tunit, 50.0_dp)
      call f90_expect_near(wx(1), 1.0_dp, 1.0e-6_dp, cstr("windxy_x@50")) ! 0.8 * halfway between -1.0 and 3.0

      ok = ec_gettimespacevalue(ecInstancePtr, item_windy, IREFDATE, tzone, tunit, 50.0_dp)
      call f90_expect_near(wy(1), -1.0_dp, 1.0e-6_dp, cstr("windxy_y@50")) ! 0.8 * halfway between -4.0 and 2.0

      ! Get `item_windxy_x`, this will apply the `WINDSPEEDFACTOR` data value to both `wx` and `wy`.
      ok = ec_gettimespacevalue(ecInstancePtr, item_windxy_x, IREFDATE, tzone, tunit, 50.0_dp)
      call f90_expect_near(wx(1), WINDSPEEDFACTOR, 1.0e-6_dp, cstr("windxy_x@50")) ! 0.8 * halfway between -1.0 and 3.0
      call f90_expect_near(wy(1), -WINDSPEEDFACTOR, 1.0e-6_dp, cstr("windxy_y@50")) ! 0.8 * halfway between -4.0 and 2.0

      deallocate(wx, wy)
   end subroutine test_data_value__windx_windy_in_bc__windxy_datavalue
   !$f90tw )

   !$f90tw TESTCODE(TEST, test_ec_module_data_value,
   !$f90tw test_data_value__solarradiation, test_data_value__solarradiation,
   subroutine test_data_value__solarradiation() bind(C)
      use m_sferic, only: jsferic
      use m_meteo, only: initialize_ec_module, ecInstancePtr, ec_addtimespacerelation, item_solar_radiation, ec_gettimespacevalue
      use m_flowtimes, only: refdate_mjd, tzone, tunit
      use time_module, only: ymd2modified_jul
      use timespace_parameters, only: BCASCII, DATAVALUE, OPERAND_OVERRIDE, OPERAND_MULTIPLY
      use timespace_data, only: SPACEANDTIME, JUSTUPDATE
      use m_file_helpers, only: create_file
      use precision, only: dp
      use timespace_read, only: MAXNAMELEN
      use m_wind, only: solar_radiation
      use m_missing, only: dmiss

      character(len=*), parameter :: BC_FILE = "test_data_value__solarradiation.bc"
      integer, parameter :: IREFDATE = 20000101
      real(dp), parameter :: SOLARRADIATIONFACTOR = 0.8_dp
      character(len=MAXNAMELEN) :: quantity_name

      real(dp) :: x(1), y(1)
      integer  :: mask(1)
      logical  :: ok, mjd_ok

      ! Arrange
      allocate(solar_radiation(1))
      solar_radiation = dmiss

      call create_file(BC_FILE, [ &
         "[General]", &
         "fileVersion = 1.01", &
         "fileType = boundConds", &
         "", &
         "[forcing]", &
         "name = global", &  ! Solar radiation value is applied uniformly over all of space
         "function = timeseries", &
         "timeInterpolation = linear", &
         "quantity = time", &
         "unit = seconds since 2000-01-01", &
         "quantity = solarradiation", &
         "unit = W m-2", &
         "0 100.0", &
         "100 300.0" &
      ])

      mjd_ok = ymd2modified_jul(IREFDATE, refdate_mjd)
      tzone  = 0.0_dp
      jsferic = 0
      call initialize_ec_module()

      x = 0.0_dp
      y = 0.0_dp
      mask = 1

      ! Act
      quantity_name = "solarradiation"
      ok = ec_addtimespacerelation(quantity_name, x, y, mask, 1, "global", &
         BCASCII, SPACEANDTIME, OPERAND_OVERRIDE, forcingfile=BC_FILE)
      call F90_EXPECT_TRUE(ok, cstr("ec_addtimespacerelation failed for solarradiation"))

      ok = ec_addtimespacerelation(quantity_name, x, y, mask, 1, "", &
         DATAVALUE, JUSTUPDATE, OPERAND_MULTIPLY, data_value=SOLARRADIATIONFACTOR)
      call F90_EXPECT_TRUE(ok, cstr("ec_addtimespacerelation failed for solarradiation dataValue"))

      ! Assert
      ok = ec_gettimespacevalue(ecInstancePtr, item_solar_radiation, IREFDATE, tzone, tunit, 50.0_dp)
      call f90_expect_near(solar_radiation(1), SOLARRADIATIONFACTOR * 200.0_dp, 1.0e-6_dp, cstr("solarradiation@50"))

      deallocate(solar_radiation)
   end subroutine test_data_value__solarradiation
   !$f90tw )

end module test_ec_module_data_value