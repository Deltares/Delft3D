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
   !!> error message including the provided `error_message` and `filename`.
   !!>
   !!> @param[in]  filename       Name of the NetCDF file to create.
   !!> @param[in]  ncmode         Mode flags passed to `nf90_create` (NetCDF create mode).
   !!> @param[in]  error_message  Description text used in error output if creation fails.
   !!> @param[out] idfile         NetCDF file identifier returned by `nf90_create`.
   !!> @return     ierror         NetCDF error code returned by `nf90_create`.
   !!> @note This function uses the NetCDF Fortran module (`netcdf`) and relies on
   !!>       `nc_check_err` to report errors in a human-readable way.
   function nc_create_and_check(filename, ncmode, error_message, idfile) result(ierror)
      use netcdf, only: nf90_create
      implicit none
      character(*), intent(in) :: filename
      integer, intent(in) :: ncmode
      character(*), intent(in) :: error_message
      integer, intent(out) :: idfile
      integer :: ierror
      !
      ierror = nf90_create(filename, ncmode, idfile)
      call nc_check_err(ierror, error_message, filename)
   end function nc_create_and_check

   !!> \brief Check NetCDF error code and print human-readable message.
   !!>
   !!> Tests the provided NetCDF error code and, if it indicates an error,
   !!> prints a descriptive message including the supplied `description` and
   !!> the `filename`. Uses `nf90_strerror` to convert the NetCDF error code
   !!> into an explanatory string.
   !!>
   !!> @param[in] ierror       NetCDF error code to test.
   !!> @param[in] description  Text describing the operation that failed (used in output).
   !!> @param[in] filename     Name of the NetCDF file related to the operation.
   !!> @note Uses `nf90_noerr` and `nf90_strerror` from the NetCDF Fortran module.
   subroutine nc_check_err(ierror, description, filename)
      use netcdf
      implicit none
      integer, intent(in) :: ierror
      character(*), intent(in) :: description
      character(*), intent(in) :: filename
      !
      if (ierror /= nf90_noerr) then
         write (*, '(6a)') 'ERROR ', trim(description), '. NetCDF file : "', trim(filename), '". Error message:', nf90_strerror(ierror)
      end if
   end subroutine nc_check_err
end module nc_check
