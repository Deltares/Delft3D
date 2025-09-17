!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
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
!> Module containing the settings for the waqpb_import tool
module m_waqpb_import_settings
   use m_waqpb_base_settings

   implicit none

   private
   public :: waqpb_import_settings

   type, extends(waqpb_base_settings) :: waqpb_import_settings
      logical :: create_new_tables = .false. !> Flag to indicate whether to create new tables, disregarding any existing *.csv files
   contains
      procedure :: init
      procedure :: show_help
      procedure :: get_accepted_flag_args
      procedure :: get_accepted_keyval_args
   end type waqpb_import_settings

contains

   !> Constructor for waqpb_import_settings
   !! Initializes the settings for the waqpb_import tool
   subroutine init(this)
      use m_cli_utils
      class(waqpb_import_settings), intent(inout) :: this
      integer :: i
      character(len=256), dimension(:), allocatable :: accepted_flag_args, accepted_keyval_args, invalid_args

      this%processes_overview_file_name = 'processes_overview.asc'

      call this%init_base("waqpb_import")

      this%create_new_tables = is_command_arg_specified('--new-tables')

   end subroutine init

   !> Returns the accepted flag arguments for this tool
   function get_accepted_flag_args(this) result(accepted_flag_args)
      class(waqpb_import_settings), intent(in) :: this
      character(len=256), dimension(:), allocatable :: accepted_flag_args, base_flag_args, flag_args
      integer :: n_combined

      base_flag_args = this%get_accepted_flag_args_base()
      flag_args = ['--new-tables']

      n_combined = size(base_flag_args) + size(flag_args)

      allocate(accepted_flag_args(n_combined))
      accepted_flag_args = [ flag_args, base_flag_args ]
   end function get_accepted_flag_args

   !> Returns the accepted key-value arguments for this tool
   function get_accepted_keyval_args(this) result(accepted_keyval_args)
      class(waqpb_import_settings), intent(in) :: this
      character(len=256), dimension(:), allocatable :: accepted_keyval_args

      accepted_keyval_args = this%get_accepted_keyval_args_base()
   end function get_accepted_keyval_args

   !> Show help for the waqpb_import tool
   subroutine show_help(this)
      class(waqpb_import_settings), intent(in) :: this

      call this%show_help_base('[--new-tables]')
      write (*, param_format) '--new-tables', separator, 'This optional argument will disregard the content of any existing *.csv files.'
      write (*, '(A,t32,A)') ' ', 'If any are found, they will be overwritten.'
      write (*, *)

      call show_help_manual()
   end subroutine show_help

end module m_waqpb_import_settings
