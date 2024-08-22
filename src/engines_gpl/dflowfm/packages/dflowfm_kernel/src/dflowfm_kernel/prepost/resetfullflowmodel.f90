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

 !> Resets the current flow- and time-state, as well as all related (phys) parameters.
 !! To be called prior to loading a new MDU and upon program startup.
 subroutine resetFullFlowModel()
    use timers
    use m_wind
    use m_physcoef
    use m_turbulence
    use m_flow
    use fm_external_forcings_data
    use m_flowparameters
    use m_flowgeom
    use m_modelbounds
    use m_flowtimes
    use m_samples
    use unstruc_netcdf
    use unstruc_model
    use unstruc_display
    use m_observations
    use m_monitoring_crosssections
    use m_thindams
    use m_fixedweirs
    use m_sediment
    use m_trachy
    use m_hydrology_data
    use m_kml_parameters
    use m_structures
    use m_longculverts, only: default_longculverts
    use m_heatfluxes
    use m_ec_interpolationsettings
    use unstruc_channel_flow
    use m_sobekdfm
    use m_waves, only: default_waves
    use m_save_ugrid_state
    use m_xbeach_avgoutput, only: default_xbeach_avgoutput
    use m_nearfield, only: default_nearfieldData
    use m_ship
    use unstruc_caching
    use m_subsidence
    use m_sferic, only: default_sferic
    use m_1d2d_fixedweirs, only: default_1d2d_fixedweirs
    use m_laterals, only: default_lateral
    use m_f1dimp
    use fm_statistical_output
    use fm_deprecated_keywords, only: default_fm_deprecated_keywords
    use m_sediment, only: deallocgrains, default_sediment
    implicit none

    ! Only reset counters and other scalars, allocatables should be
    ! automatically reset elsewhere (e.g., allocateandset*, flow_geominit)
    call default_fm_deprecated_keywords()

    call init_unstruc_netcdf()

    call default_caching()

    ! TODO: UNST-487: Add default_fourier + reset
    call resetModel()

    call default_kml_parameters()

    call default_physcoef()

    call default_sferic()

    call default_grw()

    call default_wind()

    call default_lateral()

    call default_waves()

    call default_sobekdfm()

    call default_1d2d_fixedweirs()

    call dealloc(network) ! flow1d

    call default_heatfluxes()

    call default_sediment() ! stm_included not defined yet

    call default_subsupl()

    call default_trachy()

    call default_hydrology_data()

    call default_fm_wq_processes()

    call default_turbulence()

    call default_flowgeom()

    call default_modelbounds()

    call default_fm_external_forcing_data()

    call default_channel_flow()

    call default_structures()

    call default_longculverts()

    call default_flowtimes()

    call default_flowparameters()

    call default_flow()

    call default_interpolationsettings()

    call default_xbeach_avgoutput()

    call default_save_ugrid_state()

    call default_fm1dimp()

    call default_fm_statistical_output()

    call default_nearfieldData()

    call reset_sedtra()
    call deallocgrains()
    call default_sediment()

    !Reset samples:
    ns = 0

    ! Reset observations and cross sections
    call deleteObservations()
    call delCrossSections()
    call delThinDams()
    call delFixedWeirs()

 end subroutine resetFullFlowModel
