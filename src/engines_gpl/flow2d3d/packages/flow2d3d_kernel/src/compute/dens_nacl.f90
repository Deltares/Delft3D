subroutine dens_nacl(temp, salt, rhonacl, rhods, rhodt)
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
!
!
!!--description-----------------------------------------------------------------
!
!    Function: Computes water density from temperature and
!              salinity using equation of state (rhowat).
!
! Method used: Equation of state following Millero/Delft Hydraulics
!              Valid for NaCl solutions
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
    use precision
    implicit none
!
! Global variables
!
    real(fp), intent(in)     :: salt
    real(fp), intent(in)     :: temp
    real(fp), intent(out)    :: rhonacl
    real(fp), intent(out)    :: rhods
    real(fp), intent(out)    :: rhodt
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    rhonacl  = 999.904_fp                  + 4.8292E-2_fp * temp           - &
     &         7.2312E-3_fp * temp**2      + 2.9963E-5_fp * temp**3        + &
     &         7.6427E-1_fp * salt         - 3.1490E-3_fp * salt * temp    + &
     &         3.1273E-5_fp * salt * temp**2

    rhods    = 7.6427E-1_fp                - 3.1490E-3_fp * temp           + &
     &         2*3.1273E-5_fp * salt * temp

    rhodt    = 4.8292E-2_fp                - 2_fp*7.2312E-3_fp * temp      + &
     &         3_fp*2.9963E-5_fp * temp**2 - 3.1490E-3_fp * salt           + &
     &         2_fp*3.1273E-5_fp * salt * temp
end subroutine dens_nacl
