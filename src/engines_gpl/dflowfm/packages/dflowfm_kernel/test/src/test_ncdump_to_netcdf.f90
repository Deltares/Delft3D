module test_netcdf_conversion
   use assertions_gtest, only: f90_assert_eq, f90_assert_true, f90_expect_eq
   use m_file_helpers, only: create_netcdf_from_cdl
   use netcdf, only: nf90_open, nf90_nowrite, nf90_inq_varid, nf90_get_var, nf90_close, nf90_noerr, nf90_strerror
   use precision, only: dp

   implicit none(type, external)
   private

   public :: test_ncdump_to_netcdf

contains

   !$gtest
   !> Convert a SWAN-style CDL fixture and check the values in the resulting NetCDF file.
   subroutine test_ncdump_to_netcdf() bind(C)
      character(len=*), parameter :: NC_FILE = "test_ncdump_to_netcdf.nc"
      logical :: success
      integer :: ncid, varid, status, close_status
      real(kind=dp), dimension(1, 1, 2) :: wave_height

      call create_netcdf_from_cdl( &
         NC_FILE, [character(len=80) :: &
            "netcdf swan_fixture {", &
            "dimensions:", &
            "    time = 2;", &
            "    yc = 1;", &
            "    xc = 1;", &
            "variables:", &
            "    float hs(time, yc, xc);", &
            '    hs:units = "m";', &
            '    hs:standard_name = "sea_surface_wave_significant_height";', &
            "data:", &
            "    hs = 0.5, 1.25;", &
            "}" &
         ], success=success)
      if (.not. f90_assert_true(success, "Could not create NetCDF fixture: "//NC_FILE)) then
         return
      end if

      status = nf90_open(NC_FILE, nf90_nowrite, ncid)
      if (.not. f90_assert_eq(status, nf90_noerr, "Opening generated fixture: "//trim(nf90_strerror(status)))) then
         return
      end if

      status = nf90_inq_varid(ncid, "hs", varid)
      if (f90_assert_eq(status, nf90_noerr, "Finding hs: "//trim(nf90_strerror(status)))) then
         status = nf90_get_var(ncid, varid, wave_height)
         call f90_expect_eq(status, nf90_noerr, "Reading hs: "//trim(nf90_strerror(status)))
      end if
      close_status = nf90_close(ncid)
      call f90_expect_eq(close_status, nf90_noerr, "Closing generated fixture: "//trim(nf90_strerror(close_status)))
      if (status /= nf90_noerr) then
         return
      end if

      call f90_expect_eq(wave_height(1, 1, 1), 0.5_dp, "Wave height at the first time step")
      call f90_expect_eq(wave_height(1, 1, 2), 1.25_dp, "Wave height at the second time step")
   end subroutine test_ncdump_to_netcdf

end module test_netcdf_conversion
