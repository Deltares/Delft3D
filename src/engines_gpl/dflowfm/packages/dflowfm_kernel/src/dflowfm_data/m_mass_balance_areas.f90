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

module m_mass_balance_areas
   use precision, only: dp
   integer, parameter :: NAMMBALEN = 128 !< maximum length of mass balance area names

   integer, parameter :: DIR_FROM = 1 !< flux direction from this area
   integer, parameter :: DIR_TO = 2 !< flux direction to this area

   integer :: jamba = 0 !< switch for mass balance areas being active
   integer :: nombs = 0 !< number of mass balances
   character(len=NAMMBALEN), allocatable :: mbsname(:) !< mass balance substance names
   integer :: nomba = 0 !< number of mass balance areas
   integer :: nombabnd !< number of mass balance areas and boundaries
   character(len=NAMMBALEN), allocatable :: mbaname(:) !< mass balance area names
   character(len=NAMMBALEN), allocatable :: mbabndname(:) !< mass balance area horizontal transport names
   integer, allocatable :: mbadef(:) !< mass balance area (mba) definition
   integer, allocatable :: mbadefdomain(:) !< mass balance area (mba) definition without ghost cells
   integer :: id_mba(3) !< mbd id's in map-file
   integer, allocatable :: mbalnfromto(:, :) !< from mba (1:lnxi) or bnd (lnxi+1:lnx) to mba for each link (2D)
   integer, allocatable :: mbalnused(:, :) !< number of links between mda and mbabnd that are actually active
   integer, allocatable :: mbasorsin(:, :) !< mba for each side of a source sink
   integer, allocatable :: mbasorsinout(:, :) !< (reduced) mba for each side of a source sink for output
   integer :: nombaln !< number of links needed for mass balance (2D)
   integer, allocatable :: mbalnlist(:) !< list of links needed for the mass balance (2D)
   logical :: mbaremaining !< mass balance area for ramaining cells added?

   integer, allocatable, dimension(:) :: imbs2sed !< D-Flow FM mass balance number to sediment fraction (0=not a sediment fraction)
   integer, allocatable, dimension(:) :: ised2mbs !< D-Flow FM sediment fraction to mass balance number

   character(len=255) :: nc_bal_name !< netCDF balance file name
   integer :: ncid_bal_file !< id of the netCDF balances file
   integer :: ncid_bal_strlen !< netCDF id of the string length for names in the balance file

   integer :: nc_bal_itime !< netCDF balance file time index
   integer :: ncid_bal_time_dim !< netCDF id of the time dimension on the balance file
   integer :: ncid_bal_time !< netCDF id of the time variable on the balance file

   integer :: ncid_nbalarea_dim !< netCDF id of the balance areas dimension
   integer :: ncid_bal_area_names !< netCDF id of the balance area names variable
   integer :: ncid_bal_area !< netCDF id of the surface area of the balance areas

   integer :: ncid_bal_water_balance_error !< netCDF id of the water balance error
   integer :: ncid_bal_water_balance_cumerror !< netCDF id of the water balance cumulative error
   integer :: ncid_bal_water_volume !< netCDF id of the water volume
   integer :: ncid_bal_water_depth !< netCDF id of the average water depth

   integer :: ncid_bal_flux_dir_dim !< netCDF id of the flux direction (from/to) dimension
   integer :: ncid_bal_flux_dir !< netCDF id of the flux direction (from/to) names

   integer, dimension(:), allocatable :: ncid_bal_water_flow_dim !< netCDF id of the water flow dimension
   integer, dimension(:), allocatable :: ncid_bal_water_flow_names !< netCDF id of the water flow names
   integer, dimension(:), allocatable :: ncid_bal_water_flow_values !< netCDF id of the water flow values

   integer, dimension(:), allocatable :: ncid_bal_const_balance_error !< netCDF id of the constituent balance error
   integer, dimension(:), allocatable :: ncid_bal_const_balance_cumerror !< netCDF id of the constituent balance cumulative error
   integer, dimension(:), allocatable :: ncid_bal_const_mass !< netCDF id of the constituent mass in water column
   integer, dimension(:), allocatable :: ncid_bal_const_fluff_mass !< netCDF id of the constituent mass in fluff layer
   integer, dimension(:), allocatable :: ncid_bal_const_bed_mass !< netCDF id of the constituent mass in bed stratigraphy
   integer, dimension(:), allocatable :: ncid_bal_const_bedshort_mass !< netCDF id of the constituent mass in bed shortage
   integer, dimension(:), allocatable :: ncid_bal_const_conc !< netCDF id of the average constituent concentration in water column

   integer, dimension(:, :), allocatable :: ncid_bal_const_flux_dim !< netCDF id of the constituent fluxes dimension
   integer, dimension(:, :), allocatable :: ncid_bal_const_flux_names !< netCDF id of the constituent flux names
   integer, dimension(:, :), allocatable :: ncid_bal_const_flux_values !< netCDF id of the constituent flux values

   integer :: lunmbahis !< logical unit of mba his-file
   integer :: lunmbatothis !< logical unit of mba total his-file
   integer :: lunmbabal !< logical unit of mba bal-file
   integer :: lunmbacsvm !< logical unit of mba mass csv-file
   integer :: lunmbacsvmb !< logical unit of mba mass balance csv-file
   integer :: lunmbatotbal !< logical unit of mba total bal-file
   integer :: itimembastart !< start time of balance period
   integer :: itimembastarttot !< start time of balance period
   integer :: itimembaend !< end time of balance period
   real(kind=dp) :: timembastart !< start time of balance period
   real(kind=dp) :: timembastarttot !< start time of balance period
   real(kind=dp) :: timembaend !< end time of balance period

   real(kind=dp), allocatable :: mbaarea(:) !< surface area of mass balance area

   real(kind=dp), allocatable, target :: mbavolumebegin(:) !< begin volume in mass balance area
   real(kind=dp), allocatable, target :: mbavolumebegintot(:) !< total begin volume in mass balance area
   real(kind=dp), allocatable :: mbavolumeend(:) !< end volume in mass balance area

   real(kind=dp), allocatable, target :: mbaflowhor(:, :, :) !< periodical flow between balance areas and between boundaries and balance areas
   real(kind=dp), allocatable, target :: mbaflowhortot(:, :, :) !< total flow between balance areas and between boundaries and balance areas
   real(kind=dp), allocatable, target :: mbaflowsorsin(:, :) !< periodical flow from source sinks
   real(kind=dp), allocatable, target :: mbaflowsorsintot(:, :) !< total flow from source sinks
   real(kind=dp), allocatable, target :: mbaflowraineva(:, :) !< periodical flow from rain and prescribed evaporation
   real(kind=dp), allocatable, target :: mbaflowrainevatot(:, :) !< total flow from rain and prescribed evaporation
   real(kind=dp), allocatable, target :: mbafloweva(:) !< periodical flow from calculated evaporation
   real(kind=dp), allocatable, target :: mbaflowevatot(:) !< total flow from calculated evaporation

   real(kind=dp), allocatable, target :: mbamassbegin(:, :) !< begin volume in mass balance area
   real(kind=dp), allocatable, target :: mbamassbegintot(:, :) !< total begin volume in mass balance area
   real(kind=dp), allocatable :: mbamassend(:, :) !< end volume in mass balance area

   real(kind=dp), target :: mbamorfacbegin !< begin morphological factor
   real(kind=dp), target :: mbamorfacbegintot !< total begin morphological factor
   real(kind=dp) :: mbamorfacend !< end morphological factor

   real(kind=dp), allocatable, target :: mbabedmassbegin(:, :) !< begin volume in bed stratigraphy of mass balance area
   real(kind=dp), allocatable, target :: mbabedmassbegintot(:, :) !< total begin volume in bed stratigraphy of mass balance area
   real(kind=dp), allocatable, target :: mbabedshortmassbegin(:, :) !< begin volume in bed shortage of mass balance area
   real(kind=dp), allocatable, target :: mbabedshortmassbegintot(:, :) !< total begin volume in bed shortage of mass balance area
   real(kind=dp), allocatable, target :: mbafluffmassbegin(:, :) !< begin volume in fluff layer of mass balance area
   real(kind=dp), allocatable, target :: mbafluffmassbegintot(:, :) !< total begin volume in fluff layer of mass balance area
   real(kind=dp), allocatable :: mbabedmassend(:, :) !< end volume in bed stratigraphy of mass balance area
   real(kind=dp), allocatable :: mbabedshortmassend(:, :) !< end volume in bed shortage of mass balance area
   real(kind=dp), allocatable :: mbafluffmassend(:, :) !< end volume in fluff layer of mass balance area

   real(kind=dp), allocatable, target :: mbasedflux(:, :, :, :) !< periodical bedload sediment fluxes between balance areas and between boundaries and balance areas
   real(kind=dp), allocatable, target :: mbasedfluxtot(:, :, :, :) !< total periodical bedload sediment fluxes between balance areas and between boundaries and balance areas
   real(kind=dp), allocatable :: mbasedfluxreduce(:, :, :, :) !< periodical bedload sediment fluxes between balance areas and between boundaries and balance areas (for MPI reduce)

   real(kind=dp), allocatable, target :: mbafluxhor(:, :, :, :) !< periodical fluxes between balance areas and between boundaries and balance areas
   real(kind=dp), allocatable, target :: mbafluxhortot(:, :, :, :) !< total fluxes between balance areas and between boundaries and balance areas
   real(kind=dp), allocatable, target :: mbafluxsorsin(:, :, :, :) !< periodical fluxes from source sinks
   real(kind=dp), allocatable, target :: mbafluxsorsintot(:, :, :, :) !< total fluxes from source sinks
   real(kind=dp), allocatable, target :: mbafluxheat(:, :) !< temperature heat flux
   real(kind=dp), allocatable, target :: mbafluxheattot(:, :) !< total temperature heat flux

   real(kind=dp), allocatable :: mbavolumereduce(:) !< begin volume in mass balance area
   real(kind=dp), allocatable :: mbaflowhorreduce(:, :, :) !< periodical flow between balance areas and between boundaries and balance areas
   real(kind=dp), allocatable :: mbaflowsorsinreduce(:, :) !< periodical flow from sources sinks
   real(kind=dp), allocatable :: mbaflowrainevareduce(:, :) !< periodical flow from rainfal and prescribed evaporation
   real(kind=dp), allocatable :: mbaflowevareduce(:) !< periodical flow from calculated evaporation
   real(kind=dp), allocatable :: mbamassreduce(:, :) !< begin volume in mass balance area
   real(kind=dp), allocatable :: mbafluxhorreduce(:, :, :, :) !< periodical fluxes between balance areas and between boundaries and balance areas
   real(kind=dp), allocatable :: mbafluxsorsinreduce(:, :, :, :) !< periodical fluxes from source sinks
   real(kind=dp), allocatable :: mbafluxheatreduce(:, :) !< temperature heat flux

   type balance_type
      integer :: n_entries !< number of flow/flux entries
      character(len=NAMMBALEN), dimension(:), allocatable :: group !< group to which balance flow/flux belongs
      character(len=NAMMBALEN), dimension(:), allocatable :: name !< name of balance flow/flux
      real(kind=dp), dimension(:, :), allocatable :: values !< value of balance flow/flux (1,:) = from, (2,:) = to
   end type balance_type

   type bal_group_type
      type(balance_type), dimension(:), allocatable :: bal_area !< balance information: names and flows/fluxes per area
      real(kind=dp), dimension(:), allocatable :: bal_error !< balance error per area
      real(kind=dp), dimension(:), allocatable :: bal_cumerror !< balance cumulative error per area
   end type bal_group_type

   type(bal_group_type), target :: water_flow !< water balance
   type(bal_group_type), dimension(:), allocatable, target :: const_flux !< constituent balances

end module m_mass_balance_areas
