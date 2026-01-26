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

!> Module containing the base settings for the waqpb tools
module m_waqpb_base_settings
   implicit none

   private
   public :: waqpb_base_settings, param_format, separator, show_help_manual

   character(*), parameter :: param_format = '(A,t30,A,A)' !> Format for displaying parameters in help output
   character(2), parameter :: separator = ': ' !> Separator used in help output

   type, abstract :: waqpb_base_settings
      character(:), allocatable :: tool_name !> Name of the waqpb tool using these settings
      character(:), allocatable :: processes_overview_file_name !> Name of the processes overview file
      character(:), allocatable :: process_definition_folder_path !> Path to the folder containing the process definition file
      character(:), allocatable :: processes_overview_file_path !> Full path to the processes overview file
      character(:), allocatable :: csv_folder_path !> Path to the folder containing the database CSV files
   contains
      procedure, public :: init_base
      procedure(show_help_interface), deferred :: show_help
      procedure(get_accepted_flag_args_interface), deferred :: get_accepted_flag_args
      procedure(get_accepted_keyval_args_interface), deferred :: get_accepted_keyval_args
      procedure :: show_help_base, get_accepted_flag_args_base, get_accepted_keyval_args_base
   end type waqpb_base_settings

   abstract interface
      subroutine show_help_interface(this)
         import waqpb_base_settings
         class(waqpb_base_settings), intent(in) :: this
      end subroutine show_help_interface

      function get_accepted_flag_args_interface(this) result(accepted_flag_args)
         import waqpb_base_settings
         class(waqpb_base_settings), intent(in) :: this
         character(len=256), dimension(:), allocatable :: accepted_flag_args
      end function get_accepted_flag_args_interface

      function get_accepted_keyval_args_interface(this) result(accepted_keyval_args)
         import waqpb_base_settings
         class(waqpb_base_settings), intent(in) :: this
         character(len=256), dimension(:), allocatable :: accepted_keyval_args
      end function get_accepted_keyval_args_interface
   end interface

contains

   !> Constructor for base_settings
   subroutine init_base(this, tool_name)
      use m_cli_utils
      class(waqpb_base_settings), intent(inout) :: this !> Instance of the base settings
      character(*), intent(in) :: tool_name !> Name of the tool using these settings

      character(:), allocatable :: argument
      character(:), allocatable :: processes_overview_file_name
      integer :: i
      character(len=256), dimension(:), allocatable :: accepted_flag_args, accepted_keyval_args, invalid_args

      call store_command_arguments()

      this%tool_name = tool_name

      if (is_command_arg_specified('--help') .or. &
          is_command_arg_specified('-h') .or. &
          is_command_arg_specified('--usage')) then
         call this%show_help()
         stop
      end if

      accepted_flag_args = this%get_accepted_flag_args()
      accepted_keyval_args = this%get_accepted_keyval_args()

      invalid_args = get_invalid_args(accepted_flag_args, accepted_keyval_args)
      if (size(invalid_args) > 0) then
         write(*,*) 'Error! Invalid arguments found:'
         do i = 1, size(invalid_args)
            write(*,*) '  ', trim(invalid_args(i))
         end do
         write(*,*) 'Please refer to the help for valid arguments.'
         call this%show_help()
         stop 1
      end if

      this%process_definition_folder_path = './'
      if (get_command_argument_by_name('--pdef-path', argument)) then
         this%process_definition_folder_path = trim(argument)//"/"
      end if

      this%csv_folder_path = this%process_definition_folder_path//'csvFiles/'

      if (get_command_argument_by_name('--prov-name', processes_overview_file_name)) then
         this%processes_overview_file_name = processes_overview_file_name
      end if

      this%processes_overview_file_path = trim(this%process_definition_folder_path)//trim(this%processes_overview_file_name)
   end subroutine init_base

   !> Get the accepted flag arguments for the base settings
   !! This function can be overridden in derived classes to provide specific flag arguments
   function get_accepted_flag_args_base(this) result(accepted_flag_args)
      !> This function returns the accepted flag arguments for the base settings.
      class(waqpb_base_settings), intent(in) :: this
      character(len=256), dimension(:), allocatable :: accepted_flag_args

      ! This subroutine can be overridden in derived classes to provide specific flag arguments
      accepted_flag_args = ['-h', '--help', '--usage']
   end function get_accepted_flag_args_base

   !> Get the accepted key-value arguments for the base settings
   !! This function can be overridden in derived classes to provide specific key-value arguments
   function get_accepted_keyval_args_base(this) result(accepted_keyval_args)
      !> This function returns the accepted key-value arguments for the base settings.
      class(waqpb_base_settings), intent(in) :: this
      character(len=256), dimension(:), allocatable :: accepted_keyval_args

      ! This subroutine can be overridden in derived classes to provide specific key-value arguments
      accepted_keyval_args = ['--prov-name', '--pdef-path']
   end function get_accepted_keyval_args_base

   !> Show help information for the base settings
   !! This subroutine is called by derived classes to show the help information.
   subroutine show_help_base(this, argument_list)
      class(waqpb_base_settings), intent(in) :: this
      character(len=*), intent(in) :: argument_list

      write (*, *)
      write (*, '(A)') repeat("-", 30)
      write (*, '(A, t6, A, t30, A)') '|', this%tool_name//' tool', '|'
      write (*, '(A)') repeat("-", 30)
      write (*, '(A)') 'Usage:'
      write (*, '(A, A, A)') this%tool_name, ' [-h | --help | --usage] [--prov-name <name>] [--pdef-path <path>] ', argument_list
      write (*, *)
      write (*, '(A)') 'Description:'
      write (*, param_format) '-h | --help | --usage', separator, 'Shows this information about the waqpb_import tool and exits.'
      write (*, param_format) '--prov-name <name>', separator, 'Name of the processes overview file to use.'
      write (*, '(A,t32,A)') ' ', 'The default is "'// this%processes_overview_file_name//'".'
      write (*, param_format) '--pdef-path <path>', separator, 'Path to the folder containing the process definition files.'
      write (*, '(A,t32,A)') ' ', 'The default is the current directory.'
   end subroutine show_help_base

   subroutine show_help_manual()
      write (*, *)
      write (*, '(A)') 'For further information, please refer to the Open Process Library manual.'
      write (*, '(A)') 'Appendix C: "Using waqpb export and import tools" contains detailed guidance.'
      write (*, *)
   end subroutine show_help_manual

end module m_waqpb_base_settings
