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

!
!

 !> Resets the current flow- and time-state, but keeps al active parameter settings.
 !! To be called upon flow_modelinit().
 !! Upon program startup and loading of new model/MDU, call resetFullFlowModel() instead.
 subroutine resetFlow()
    use m_wind
    use m_flow
    use fm_external_forcings_data
    use m_flowparameters
    use m_statistics
    use m_flowgeom
    use m_modelbounds
    use m_flowtimes
    use waq
    use m_waves
    use m_hydrology_data
    use m_sobekdfm
    use m_save_ugrid_state, only: reset_save_ugrid_state
    use m_longculverts, only: reset_longculverts
    use m_sedtrails_data
    use m_nearfield, only: reset_nearfieldData
    use m_laterals, only: reset_lateral
    implicit none

    ! Only reset counters and other scalars, allocatables should be
    ! automatically reset elsewhere (e.g., allocateandset*, flow_geominit)

    call reset_wind()

    call reset_lateral

    call reset_waves()

    call reset_sobekdfm()

    ! Reset some flow (rest is done in flow_geominit())
    call reset_flowgeom()

    ! Sedtrails
    call sedtrails_resetdata()

    call reset_modelbounds()

    call reset_flowexternalforcings()

    call reset_longculverts()

    call reset_flowtimes()

    ! call reset_flowparameters()

    call reset_flow()

    call reset_waq()

    call reset_movobs()

    call reset_statistics()

    if (jawave == 4) then
       call xbeach_reset()
    end if

    call reset_save_ugrid_state()

    call reset_sedtra()

    call reset_hydrology_data()

    call reset_nearfieldData()

 end subroutine resetFlow
