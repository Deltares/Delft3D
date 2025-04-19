!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
    
module m_dambreak_data
   use precision, only: dp

   implicit none

   public

   integer, allocatable :: dambreaks(:) !< store the dambreaks indexes among all structures
   integer, target :: n_db_links !< nr of dambreak links
   integer, target :: n_db_signals !< nr of dambreak signals
   integer, allocatable :: db_first_link(:) !< first dambreak link for each signal
   integer, allocatable :: db_last_link(:) !< last dambreak link for each signal
   integer, allocatable :: db_active_links(:) !< db_active_links, open dambreak links
   integer, allocatable :: breach_start_link(:) !< the starting link, the closest to the breach point
   integer, allocatable :: db_link_ids(:, :) !< dambreak links index array
   real(kind=dp), allocatable, target :: db_levels_widths_table(:) !< dambreak widths and heights
   character(len=128), allocatable, target :: db_ids(:) !< the dambreak ids
   real(kind=dp), dimension(:), allocatable, public :: db_link_effective_width !< dambreak effective flow widths
   real(kind=dp), dimension(:), allocatable, public :: db_link_actual_width !< dambreak actual flow widths
   
   integer, protected, pointer :: p_n_db_links => n_db_links
   integer, protected, pointer :: p_n_db_signals => n_db_signals
   

contains

   subroutine default_dambreak_data()

      n_db_links = 0 ! nr of dambreak links
      n_db_signals = 0 ! nr of dambreak signals

   end subroutine default_dambreak_data
   
   pure function exist_dambreak_links() result(res)
   logical :: res 

   res = n_db_links > 0

   end function exist_dambreak_links

end module m_dambreak_data
