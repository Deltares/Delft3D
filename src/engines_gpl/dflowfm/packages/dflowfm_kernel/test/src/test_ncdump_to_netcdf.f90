module test_netcdf_conversion
   use assertions_gtest, only: f90_expect_eq, f90_expect_true
   use precision, only: dp

   implicit none(type, external)
   private

   public :: test_ncdump_to_netcdf

contains

   !$gtest
   !> Convert a small SWAN-style CDL fixture and check the values in the resulting NetCDF file.
   subroutine test_ncdump_to_netcdf() bind(C)
      use m_file_helpers, only: create_file
      use netcdf, only: nf90_open, nf90_nowrite, nf90_inq_varid, nf90_get_var, nf90_close, nf90_noerr, nf90_strerror

      character(len=*), parameter :: cdl_file = "bubblescreen_swan_fixture.cdl"
      character(len=*), parameter :: nc_file = "bubblescreen_swan_fixture.nc"
      character(len=512) :: cmdmsg
      integer :: cmdstat, exitstat, ncid, varid, status, close_status
      real(kind=dp), dimension(1, 1, 2) :: wave_height

      call create_file(cdl_file, [character(len=72) :: &
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
                                 "}"])

      call execute_command_line("ncgen -o "//nc_file//" "//cdl_file, &
                                wait=.true., cmdstat=cmdstat, cmdmsg=cmdmsg, exitstat=exitstat)
      if (cmdstat /= 0) then
         call f90_expect_true(.false., "Could not execute ncgen: "//trim(cmdmsg))
         return
      end if
      call f90_expect_eq(exitstat, 0, "ncgen conversion failed; check PATH and process output")
      if (exitstat /= 0) then
         return
      end if

      status = nf90_open(nc_file, nf90_nowrite, ncid)
      call f90_expect_eq(status, nf90_noerr, "Opening generated NetCDF fixture: "//trim(nf90_strerror(status)))
      if (status /= nf90_noerr) then
         return
      end if

      status = nf90_inq_varid(ncid, "hs", varid)
      call f90_expect_eq(status, nf90_noerr, "Finding hs: "//trim(nf90_strerror(status)))
      if (status == nf90_noerr) then
         ! NetCDF Fortran reads the CDL dimensions (time, yc, xc) in reverse order.
         status = nf90_get_var(ncid, varid, wave_height)
         call f90_expect_eq(status, nf90_noerr, "Reading hs: "//trim(nf90_strerror(status)))
      end if
      close_status = nf90_close(ncid)
      call f90_expect_eq(close_status, nf90_noerr, "Closing generated NetCDF fixture: "//trim(nf90_strerror(close_status)))
      if (status /= nf90_noerr) then
         return
      end if

      call f90_expect_eq(wave_height(1, 1, 1), 0.5_dp, "Wave height at the first time step")
      call f90_expect_eq(wave_height(1, 1, 2), 1.25_dp, "Wave height at the second time step")
   end subroutine test_ncdump_to_netcdf

end module test_netcdf_conversion
