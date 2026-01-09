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
module m_waqpb_export_settings
   use m_waqpb_base_settings

   implicit none

   private
   public :: waqpb_export_settings

   type, extends(waqpb_base_settings) :: waqpb_export_settings
      real :: version = 0.0
      integer :: serial = 0
      logical :: wrong_version = .false. !> Flag to indicate if the specified version is wrong (e.g. letters)
      logical :: wrong_serial = .false. !> Flag to indicate if the specified serial is wrong (e.g. letters)
      logical :: generate_latex_tables = .false. !> Flag to indicate whether to generate LaTeX tables
   contains
      procedure :: init
      procedure :: show_help
      procedure :: get_accepted_flag_args
      procedure :: get_accepted_keyval_args
   end type waqpb_export_settings

contains

   subroutine init(this)
      use m_cli_utils
      use m_waqpb_export_helper
      class(waqpb_export_settings), intent(inout) :: this
      real :: version
      integer :: serial, i
      character(len=256), dimension(:), allocatable :: accepted_flag_args, accepted_keyval_args, invalid_args
      logical :: success, parsing_error

      this%processes_overview_file_name = 'processes_overview_exported.asc'

      call this%init_base("waqpb_export")

      parsing_error = .false.
      success = get_command_argument_by_name('--version', version, parsing_error)
      if (success) then
         this%version = version
         this%wrong_version = .false.
      else
         call report_failure(parsing_error, '--version', 'real')
         this%version = generate_version()
         this%wrong_version = .true.
      end if

      parsing_error = .false.
      success = get_command_argument_by_name('--serial', serial, parsing_error)
      if (success) then
         this%serial = serial
         this%wrong_serial = .false.
      else
         call report_failure(parsing_error, '--serial', 'integer')
         this%serial = generate_serial()
         this%wrong_serial = .true.
      end if

      this%generate_latex_tables = is_command_arg_specified('--latex')
   end subroutine init

   subroutine report_failure(parsing_error, argument_name, expected_type)
      use m_cli_utils
      logical, intent(in) :: parsing_error
      character(len=*), intent(in) :: argument_name
      character(len=*), intent(in) :: expected_type

      character(:), allocatable :: string_found
      logical :: success

      if (parsing_error) then
         success = get_command_argument_by_name(argument_name, string_found)
         write (*, '(A)', ADVANCE = 'NO') 'An invalid value was given for the optional argument '//argument_name//': '
         write (*, '(A)', ADVANCE = 'NO') ' "'//trim(string_found)//'" was found. A number of type '//expected_type//' is expected. '
      else
         write (*, '(A)', ADVANCE = 'NO') 'No optional argument '//argument_name//' or value for it was found. '
      end if
      write (*, '(A)') 'Default value (automatically generated) will be used.'
   end subroutine report_failure

   !> Returns the accepted flag arguments for this tool
   function get_accepted_flag_args(this) result(accepted_flag_args)
      class(waqpb_export_settings), intent(in) :: this
      character(len=256), dimension(:), allocatable :: accepted_flag_args, base_flag_args, flag_args
      integer :: n_combined

      base_flag_args = this%get_accepted_flag_args_base()
      flag_args = ['--latex']

      n_combined = size(base_flag_args) + size(flag_args)

      allocate(accepted_flag_args(n_combined))
      accepted_flag_args = [ flag_args, base_flag_args ]
   end function get_accepted_flag_args

   !> Returns the accepted key-value arguments for this tool
   function get_accepted_keyval_args(this) result(accepted_keyval_args)
      class(waqpb_export_settings), intent(in) :: this
      character(len=256), dimension(:), allocatable :: accepted_keyval_args, base_keyval_args, keyval_args
      integer :: n_combined

      base_keyval_args = this%get_accepted_keyval_args_base()
      keyval_args = ['--version', '--serial']

      n_combined = size(base_keyval_args) + size(keyval_args)

      allocate(accepted_keyval_args(n_combined))
      accepted_keyval_args = [ keyval_args, base_keyval_args ]
   end function get_accepted_keyval_args

   subroutine show_help(this)
      class(waqpb_export_settings), intent(in) :: this

      call this%show_help_base('[--version <version_number>] [--serial <serial_number>] [--latex]')

      write (*, param_format) '--version <version_number>', separator, 'Overrides the default version number (equal to the one of DELWAQ) of this tool. If specified, a positive decimal number is expected.'
      write (*, param_format) '--serial <serial_number>', separator, 'Overrides the default serial number (generated from date and time) of this tool. If specified, an integer is expected.'
      write (*, param_format) '--latex', separator, 'Generates the LaTeX tables.'
      write (*, *)

      call show_help_manual()
   end subroutine show_help

end module m_waqpb_export_settings
