!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

module modeldim
!
! module including all parameters required
! for dimensioning global arrays
!
use precision_part   ! single and double precision
implicit none
save
!
!  Derived type for defining array dimensions
!
   type :: model_dimensions_

!      general
       integer  :: no_timesteps
       integer  :: no_particles

       integer  :: no_substances
       integer  :: no_userdef_substances
       integer  :: no_oil_fractions
       integer  :: no_monitoring_stations
       integer  :: no_particle_tracks
       integer  :: no_constants
       integer  :: no_random_parameters

!      flow grid
       integer  :: mmax
       integer  :: nmax
       integer  :: kmax
       integer  :: exchanges
!
!      (aggregated) water quality grid
       integer  :: no_hor_segments                            ! number of horizontal segments
       integer  :: no_total_segments                          ! number of total segments
       integer  :: no_layers                                  ! number of layers
       integer  :: nosegl                                     ! possibly horizontal active only cell count
       integer  :: noseg                                      ! possibly total active only cell count
       integer  :: noq1                                       ! possibly active only count of noq1
       integer  :: noq2                                       ! possibly active only count of noq2
       integer  :: noq3                                       ! possibly active only count of noq3
       integer  :: noq                                        ! possibly active only count of noq

!      zoom grid
       integer  :: mmap
       integer  :: nmap

!      releases
       integer  :: no_ud_releases
       integer  :: no_dye_releases
       integer  :: no_cont_releases
       integer  :: no_total_releases

!      max. number of breakpoints in tables
       integer  :: max_brkpts_decay
       integer  :: max_brkpts_continuous_releases
       integer  :: max_brkpts_settling_velocities
       integer  :: max_brkpts_plottimes_zoomgrid
       integer  :: max_brkpts_wind
       integer  :: max_brkpts_ini_polygone

!      Nefis administration
       integer  :: no_nefis_elements

!      Buffer
       integer  :: buffer_size

   end type model_dimensions_

   type(model_dimensions_), target :: model_dimensions     ! allocate

end module modeldim
