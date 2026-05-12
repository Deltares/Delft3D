module nc_check
   !----- GPL ---------------------------------------------------------------------
   !
   !  Copyright (C)  Stichting Deltares, 2011-2026.
   !
   !  This program is free software: you can redistribute it and/or modify
   !  it under the terms of the GNU General Public License as published by
   !  the Free Software Foundation version 3.
   !
   !  This program is distributed in the hope that it will be useful,
   !  but WITHOUT ANY WARRANTY; without even the implied warranty of
   !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   !  GNU General Public License for more details.
   !
   !  You should have received a copy of the GNU General Public License
   !  along with this program.  If not, see <http://www.gnu.org/licenses/>.
   !
   !  contact: delft3d.support@deltares.nl
   !  Stichting Deltares
   !  P.O. Box 177
   !  2600 MH Delft, The Netherlands
   !
   !  All indications and logos of, and references to, "Delft3D" and "Deltares"
   !  are registered trademarks of Stichting Deltares, and remain the property of
   !  Stichting Deltares. All rights reserved.
   !
   !-------------------------------------------------------------------------------

   !!> \brief NetCDF error helper routines.
   !!>
   !!> Provides small helper routines to create NetCDF files and report NetCDF
   !!> errors in a consistent, human-readable form.
contains

   !!> \brief Create a NetCDF file and check for errors.
   !!>
   !!> Creates a NetCDF file using `nf90_create` and checks the returned error code.
   !!> If an error occurs the routine `nc_check_err` is called to print a descriptive
   !!> error message including the provided `error_message` and `file_name`.
   !!>
   !!> @param[in]  file_name      Name of the NetCDF file to create.
   !!> @param[in]  ncmode         Mode flags passed to `nf90_create` (NetCDF create mode).
   !!> @param[in]  error_message  Description text used in error output if creation fails.
   !!> @param[out] file_id        NetCDF file identifier returned by `nf90_create`.
   !!> @return     ierror         NetCDF error code returned by `nf90_create`.
   !!> @note This function uses the NetCDF Fortran module (`netcdf`) and relies on
   !!>       `nc_check_err` to report errors in a human-readable way.
   function nc_create_and_check(file_name, ncmode, error_message, file_id) result(ierror)
      use netcdf, only: nf90_create
      implicit none
      character(*), intent(in) :: file_name
      integer, intent(in) :: ncmode
      character(*), intent(in) :: error_message
      integer, intent(out) :: file_id
      integer :: ierror
      !
      ierror = nf90_create(file_name, ncmode, file_id)
      call nc_check_err(ierror, error_message, file_name)
   end function nc_create_and_check

   !!> \brief Open an existing NetCDF file and check for errors.
   !!>
   !!> Wrapper around `nf90_open` that checks the returned status and reports
   !!> a human-readable error using `nc_check_err`.
   !!>
   !!> @param[in]  file_name      Path to the NetCDF file to open.
   !!> @param[in]  ncmode         Access mode flags for `nf90_open`.
   !!> @param[in]  error_message  Description used in error reporting.
   !!> @param[out] file_id        Returned NetCDF file identifier on success.
   !!> @return     ierror         NetCDF error code returned by `nf90_open`.
   function nc_open_and_check(file_name, ncmode, error_message, file_id) result(ierror)
      use netcdf, only: nf90_open
      implicit none
      character(*), intent(in) :: file_name
      integer, intent(in) :: ncmode
      character(*), intent(in) :: error_message
      integer, intent(out) :: file_id
      integer :: ierror
      !
      ierror = nf90_open(file_name, ncmode, file_id)
      call nc_check_err(ierror, error_message, file_name)
   end function nc_open_and_check

   !!> \brief Put a NetCDF attribute and check for errors.
   !!>
   !!> Calls `nf90_put_att` to write an attribute and uses `nc_check_err` to
   !!> print an explanatory message when the NetCDF call fails.
   !!>
   !!> @param[in]  file_id        NetCDF file identifier.
   !!> @param[in]  var_id         Variable id (or `nf90_global` for global attributes).
   !!> @param[in]  key            Attribute name.
   !!> @param[in]  value          Attribute value (character string).
   !!> @param[in]  file_name      File name used in error messages.
   !!> @param[in]  error_message  Description used in error reporting.
   !!> @return     ierror         NetCDF error code returned by `nf90_put_att`.
   function nc_put_att_and_check(file_id, var_id, key, value, file_name, error_message) result(ierror)
      use netcdf, only: nf90_put_att
      implicit none
      integer, intent(in) :: file_id
      integer, intent(in) :: var_id
      character(*), intent(in) :: key
      character(*), intent(in) :: value
      character(*), intent(in) :: file_name
      character(*), intent(in) :: error_message
      integer :: ierror
      !
      ierror = nf90_put_att(file_id, var_id, key, value)
      call nc_check_err(ierror, error_message, file_name)
   end function nc_put_att_and_check

   !!> \brief Define a NetCDF dimension and check for errors.
   !!>
   !!> Wrapper around `nf90_def_dim` that reports errors via `nc_check_err`.
   !!>
   !!> @param[in]  file_id        NetCDF file identifier.
   !!> @param[in]  dim_name       Name of the dimension to define.
   !!> @param[in]  dim_size       Length of the dimension (or `nf90_unlimited`).
   !!> @param[out] dim_id         Returned dimension id on success.
   !!> @param[in]  file_name      File name used in error messages.
   !!> @param[in]  error_message  Description used in error reporting.
   !!> @return     ierror         NetCDF error code returned by `nf90_def_dim`.
   function nc_def_dim_and_check(file_id, dim_name, dim_size, dim_id, file_name, error_message) result(ierror)
      use netcdf, only: nf90_def_dim
      implicit none
      integer, intent(in) :: file_id
      character(*), intent(in) :: dim_name
      integer, intent(in) :: dim_size
      integer, intent(out) :: dim_id
      character(*), intent(in) :: file_name
      character(*), intent(in) :: error_message
      integer :: ierror
      !
      ierror = nf90_def_dim(file_id, dim_name, dim_size, dim_id)
      call nc_check_err(ierror, error_message, file_name)
   end function nc_def_dim_and_check

   !!> \brief End define mode and check for errors.
   !!>
   !!> Calls `nf90_enddef` for the provided file id and reports any error
   !!> through `nc_check_err`.
   !!>
   !!> @param[in]  file_id        NetCDF file identifier.
   !!> @param[in]  file_name      File name used in error messages.
   !!> @param[in]  error_message  Description used in error reporting.
   !!> @return     ierror         NetCDF error code returned by `nf90_enddef`.
   function nc_enddef_and_check(file_id, file_name, error_message) result(ierror)
      use netcdf, only: nf90_enddef
      implicit none
      integer, intent(in) :: file_id
      character(*), intent(in) :: file_name
      character(*), intent(in) :: error_message
      integer :: ierror
      !
      ierror = nf90_enddef(file_id)
      call nc_check_err(ierror, error_message, file_name)
   end function nc_enddef_and_check

   !!> \brief Inquire variable id and check for errors.
   !!>
   !!> Calls `nf90_inq_varid` to obtain a variable id by name and reports any
   !!> errors via `nc_check_err`.
   !!>
   !!> @param[in]  file_id        NetCDF file identifier.
   !!> @param[in]  var_name       Name of the variable to inquire.
   !!> @param[out] var_id         Returned variable id on success.
   !!> @param[in]  file_name      File name used in error messages.
   !!> @param[in]  error_message  Description used in error reporting.
   !!> @return     ierror         NetCDF error code returned by `nf90_inq_varid`.
   function nc_inq_varid_and_check(file_id, var_name, var_id, file_name, error_message) result(ierror)
      use netcdf, only: nf90_inq_varid
      implicit none
      integer, intent(in) :: file_id
      character(*), intent(in) :: var_name
      integer, intent(out) :: var_id
      character(*), intent(in) :: file_name
      character(*), intent(in) :: error_message
      integer :: ierror
      !
      ierror = nf90_inq_varid(file_id, var_name, var_id)
      call nc_check_err(ierror, error_message, file_name)
   end function nc_inq_varid_and_check

   !!> \brief Close NetCDF file and check for errors.
   !!>
   !!> Wrapper around `nf90_close` that checks the return status and uses
   !!> `nc_check_err` to provide a readable error message.
   !!>
   !!> @param[in]  file_id        NetCDF file identifier to close.
   !!> @param[in]  file_name      File name used in error messages.
   !!> @param[in]  error_message  Description used in error reporting.
   !!> @return     ierror         NetCDF error code returned by `nf90_close`.
   function nc_close_and_check(file_id, file_name, error_message) result(ierror)
      use netcdf, only: nf90_close
      implicit none
      integer, intent(in) :: file_id
      character(*), intent(in) :: file_name
      character(*), intent(in) :: error_message
      integer :: ierror
      !
      ierror = nf90_close(file_id)
      call nc_check_err(ierror, error_message, file_name)
   end function nc_close_and_check

   !!> \brief Check NetCDF error code and print human-readable message.
   !!>
   !!> Tests the provided NetCDF error code and, if it indicates an error,
   !!> prints a descriptive message including the supplied `description` and
   !!> the `file_name`. Uses `nf90_strerror` to convert the NetCDF error code
   !!> into an explanatory string.
   !!>
   !!> @param[in] ierror       NetCDF error code to test.
   !!> @param[in] description  Text describing the operation that failed (used in output).
   !!> @param[in] file_name     Name of the NetCDF file related to the operation.
   !!> @note Uses `nf90_noerr` and `nf90_strerror` from the NetCDF Fortran module.
   subroutine nc_check_err(ierror, description, file_name)
      use netcdf
      implicit none
      integer, intent(in) :: ierror
      character(*), intent(in) :: description
      character(*), intent(in) :: file_name
      !
      if (ierror /= nf90_noerr) then
         write (*, '(6a)') 'ERROR ', trim(description), '. NetCDF file : "', trim(file_name), '". Error message:', nf90_strerror(ierror)
      end if
   end subroutine nc_check_err
end module nc_check
