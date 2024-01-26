subroutine coupled(kbot    , ktop    , lcon, &
                 & n_intake_i, k_intake_i, add )
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
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
!
!    Function: Coupled intakes/outlets for cortime to flow coupling
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use m_nfl_data
    !
    implicit none
    !
    ! Global variables
    !
    integer   , intent(in)  :: kbot
    integer   , intent(in)  :: ktop
    integer   , intent(in)  :: lcon
    integer   , intent(in)  :: n_intake_i
    integer   , intent(in)  :: k_intake_i
    real(hp)  , intent(out) :: add
    !
    ! Local variables
    !
    integer  :: k
    real(hp) :: frac
    !
    !! executable statements -------------------------------------------------------
    !
    add = 0.0_fp
    if (n_intake_i > 0) then
        if (k_intake_i == 0) then
            do k = kbot, ktop
                frac    = (fm_z_level(k) - fm_z_level(k-1)) / max(fm_water_depth(n_intake_i), 0.01_hp)
                add = add + fm_constituents(lcon,k)*frac
            enddo
        else
            add = fm_constituents(lcon,kbot+k_intake_i-1)
        endif
    endif
end subroutine coupled
