!!  Copyright (C)  Stichting Deltares, 2021-2025.
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

  module m_hyd_keys

      ! module containing keywords in hyd file

      implicit none

      character(len=*), parameter :: task = 'task'
      character(len=*), parameter :: geometry = 'geometry'
      character(len=*), parameter :: horizontal_aggregation = 'horizontal-aggregation'
      character(len=*), parameter :: minimum_vert_diffusion_used = 'minimum-vert-diffusion-used'
      character(len=*), parameter :: vertical_diffusion = 'vertical-diffusion'
      character(len=*), parameter :: description = 'description'
      character(len=*), parameter :: end_description = 'end-description'
      character(len=*), parameter :: reference_time = 'reference-time'
      character(len=*), parameter :: hydrodynamic_start_time = 'hydrodynamic-start-time'
      character(len=*), parameter :: hydrodynamic_stop_time = 'hydrodynamic-stop-time'
      character(len=*), parameter :: hydrodynamic_timestep = 'hydrodynamic-timestep'
      character(len=*), parameter :: conversion_ref_time = 'conversion-ref-time'
      character(len=*), parameter :: conversion_start_time = 'conversion-start-time'
      character(len=*), parameter :: conversion_stop_time = 'conversion-stop-time'
      character(len=*), parameter :: conversion_timestep = 'conversion-timestep'
      character(len=*), parameter :: grid_cells_first_direction = 'grid-cells-first-direction'
      character(len=*), parameter :: grid_cells_second_direction = 'grid-cells-second-direction'
      character(len=*), parameter :: number_hydrodynamic_layers = 'number-hydrodynamic-layers'
      character(len=*), parameter :: number_water_quality_layers = 'number-water-quality-layers'
      character(len=*), parameter :: hydrodynamic_file = 'hydrodynamic-file'
      character(len=*), parameter :: aggregation_file = 'aggregation-file'
      character(len=*), parameter :: grid_indices_file = 'grid-indices-file'
      character(len=*), parameter :: grid_coordinates_file = 'grid-coordinates-file'
      character(len=*), parameter :: volumes_file = 'volumes-file'
      character(len=*), parameter :: areas_file = 'areas-file'
      character(len=*), parameter :: flows_file = 'flows-file'
      character(len=*), parameter :: pointers_file = 'pointers-file'
      character(len=*), parameter :: lengths_file = 'lengths-file'
      character(len=*), parameter :: salinity_file = 'salinity-file'
      character(len=*), parameter :: temperature_file = 'temperature-file'
      character(len=*), parameter :: vert_diffusion_file = 'vert-diffusion-file'
      character(len=*), parameter :: surfaces_file = 'surfaces-file'
      character(len=*), parameter :: total_grid_file = 'total-grid-file'
      character(len=*), parameter :: discharges_file = 'discharges-file'
      character(len=*), parameter :: chezy_coefficients_file = 'chezy-coefficients-file'
      character(len=*), parameter :: shear_stresses_file = 'shear-stresses-file'
      character(len=*), parameter :: walking_discharges_file = 'walking-discharges-file'
      character(len=*), parameter :: minimum_vert_diffusion = 'minimum-vert-diffusion'
      character(len=*), parameter :: upper_layer = 'upper-layer'
      character(len=*), parameter :: lower_layer = 'lower-layer'
      character(len=*), parameter :: interface_depth = 'interface-depth'
      character(len=*), parameter :: end_minimum_vert_diffusion = 'end-minimum-vert-diffusion'
      character(len=*), parameter :: constant_dispersion = 'constant-dispersion'
      character(len=*), parameter :: first_direction = 'first-direction'
      character(len=*), parameter :: second_direction = 'second-direction'
      character(len=*), parameter :: third_direction = 'third-direction'
      character(len=*), parameter :: end_constant_dispersion = 'end-constant-dispersion'
      character(len=*), parameter :: hydrodynamic_layers = 'hydrodynamic-layers'
      character(len=*), parameter :: end_hydrodynamic_layers = 'end-hydrodynamic-layers'
      character(len=*), parameter :: water_quality_layers = 'water-quality-layers'
      character(len=*), parameter :: end_water_quality_layers = 'end-water-quality-layers'
      character(len=*), parameter :: discharges = 'discharges'
      character(len=*), parameter :: end_discharges = 'end-discharges'
      character(len=*), parameter :: domains = 'domains'
      character(len=*), parameter :: end_domains = 'end-domains'
      character(len=*), parameter :: dd_boundaries = 'dd-boundaries'
      character(len=*), parameter :: end_dd_boundaries = 'end-dd-boundaries'
      character(len=*), parameter :: normal = 'normal'
      character(len=*), parameter :: inlet = 'inlet'
      character(len=*), parameter :: outlet = 'outlet'
      character(len=*), parameter :: full_coupling = 'full-coupling'
      character(len=*), parameter :: coupling_per_domain = 'coupling-per-domain'
      character(len=*), parameter :: attributes_file = 'attributes-file'
      character(len=*), parameter :: depths_file = 'depths-file'
      character(len=*), parameter :: curvilinear_grid = 'curvilinear-grid'
      character(len=*), parameter :: calculated = 'calculated'
      character(len=*), parameter :: unstructured = 'unstructured'
      character(len=*), parameter :: number_horizontal_exchanges = 'number-horizontal-exchanges'
      character(len=*), parameter :: number_vertical_exchanges = 'number-vertical-exchanges'
      character(len=*), parameter :: number_water_quality_segments_per_layer = 'number-water-quality-segments-per-layer'
      character(len=*), parameter :: horizontal_surfaces_file = 'horizontal-surfaces-file'
      character(len=*), parameter :: boundaries_file = 'boundaries-file'
      character(len=*), parameter :: waqgeom_file = 'waqgeom-file'
      character(len=*), parameter :: walking = 'walking'
      character(len=*), parameter :: file_created_by = 'file-created-by'
      character(len=*), parameter :: file_creation_date = 'file-creation-date'
      character(len=*), parameter :: sink_sources = 'sink-sources'
      character(len=*), parameter :: end_sink_sources = 'end-sink-sources'
      character(len=*), parameter :: z_layers = 'z-layers'
      character(len=*), parameter :: z_layers_ztop = 'z-layers-ztop'
      character(len=*), parameter :: z_layers_zbot = 'z-layers-zbot'

  end module m_hyd_keys
