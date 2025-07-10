!!  Copyright (C)  Stichting Deltares, 2012-2025.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

module m_command_line_help
    use m_waq_precision
    use delwaq_exe_version_module

    implicit none

    private
    character(2), parameter :: separator = ': '
    character(len=*), parameter :: param_format = '(A,t25,A,A)'
    character(len=*), parameter :: param_format_extra_line = '(t27,A)'

    public :: show_command_line_help, show_command_line_version

contains

    !> Shows the version information of delwaq on the console
    subroutine show_command_line_version()
        character(:), allocatable :: version_string

        call get_versionstring_delwaq(version_string)

        write (*, '(A)') repeat("-", 120)
        write (*, '(A, t120, A)') '|' // version_string, '|'
        write (*, '(A)') repeat("-", 120)
    end subroutine

    !> Shows help documentation on the command line
    subroutine show_command_line_help(identification_text)
        character(:), allocatable, intent(in) :: identification_text !< id string

        write (*, '(A)') repeat("-", 120)
        write (*, '(A, t120, A)') '|' // identification_text, '|'
        write (*, '(A)') repeat("-", 120)
        write (*, '(A/)') 'Usage: Delwaq(.exe) <input_file_path> [-p process_definition_file_path] [optional_args]'
        write (*, param_format) '-h, --help, /?', separator, 'Print help about Delwaq'
        write (*, param_format) '-v, --version', separator, 'Shows version of this Delwaq executable'
        write (*, param_format) '-a', separator, 'Only activated processes are switched on'
        write (*, param_format) '-np', separator, 'No processes from the process definition file are switched on'
        write (*, param_format) '-validation_mode', separator, 'Run Delwaq in validation mode. This only runs the pre-processing and'
        write (*, param_format_extra_line) 'produces an *.lst file with the results.'
        write (*, param_format) '-p <path>', separator, 'Path to the process definition file'
        write (*, param_format) '-openpb <path>', separator, 'Path to open process library'
        write (*, param_format) '-output <path>', separator, 'Path to the output folder'
        write (*, param_format) '-threads <number>', separator, 'Will turn on parallelism, and override any setting of the number'
        write (*, param_format_extra_line) 'of threads in the input file.'
        write (*, param_format_extra_line) 'No value or zero for [N] will use the maximum number of available threads'
        write (*,*)

    end subroutine show_command_line_help

end module m_command_line_help
