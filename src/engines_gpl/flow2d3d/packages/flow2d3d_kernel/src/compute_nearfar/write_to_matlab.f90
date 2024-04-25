module write_to_matlab
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
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
!  $Id: write_to_matlab.f90 5888 2016-02-24 10:14:54Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/write_to_matlab.f90 $
!!--description-----------------------------------------------------------------
!
! write_to_matlab.f90 --
!     Module to facilitate communication with MATLAB
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none

    interface to_matlab
        module procedure to_matlab_int
        module procedure to_matlab_real
        module procedure to_matlab_double
        module procedure to_matlab_string
    end interface to_matlab

    character(len=1), parameter, private :: tab = achar(9)

contains
subroutine to_matlab_int( lun, key, value )
    integer, intent(in) :: lun
    character(len=*), intent(in) :: key
    integer, intent(in) :: value

    write( lun, '(2a,i0)' ) trim(key), tab, value
end subroutine to_matlab_int

subroutine to_matlab_real( lun, key, value )
    integer, intent(in) :: lun
    character(len=*), intent(in) :: key
    real, intent(in) :: value

    character(len=20) :: string

    write( string, '(e15.7)' ) value
    write( lun, '(3a)' ) trim(key), tab, adjustl(string)
end subroutine to_matlab_real

subroutine to_matlab_double( lun, key, value )
    integer, intent(in) :: lun
    character(len=*), intent(in) :: key
    real(kind=kind(1.0d0)), intent(in) :: value

    character(len=30) :: string

    write( string, '(e23.17)' ) value
    write( lun, '(3a)' ) trim(key), tab, adjustl(string)
end subroutine to_matlab_double

subroutine to_matlab_string( lun, key, value )
    integer, intent(in) :: lun
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: value

    write( lun, '(3a)' ) trim(key), tab, adjustl(value)
end subroutine to_matlab_string

end module write_to_matlab
