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

module m_his_file_netcdf_ids

   implicit none
   
   public
   
   !> IDs of netcdf variables and dimensions
   integer :: &
      id_twodim, &
      id_timedim, id_timebds, id_num_timesteps, id_comp_time, &
      id_laydim, id_laydimw, id_sedtotdim, id_sedsusdim, id_frac_name, &
      id_statdim, id_strlendim, id_crsdim, id_crslendim, id_crsptsdim,  &
      id_statx, id_staty, id_stat_id, id_statname, id_time, id_timestep, &
      id_statlon, id_statlat, id_crs_id, id_crsname, id_varb, id_qsrccur, id_nlyrdim, &
      id_zcs, id_zws, id_zwu, id_checkmon, id_varruh, &
      id_pumpdim,    id_pump_id, &
      id_gatedim,    id_gate_id, &
      id_cdamdim,    id_cdam_id, &
      id_weirgendim, id_weirgen_id, &
      id_gategendim, id_gategen_id, &
      id_genstrudim, id_genstru_id, &
      id_orifgendim, id_orifgen_id, &
      id_bridgedim,  id_bridge_id, &
      id_culvertdim, id_culvert_id, &
      id_srcdim, id_srclendim, id_srcname, id_srcx, id_srcy, id_srcptsdim, &
      id_dredlinkdim, id_dreddim, id_dumpdim, id_dred_name, id_dump_name, &
      id_dambreakdim, id_dambreak_id, &
      id_uniweirdim, id_uniweir_id, &
      id_cmpstrudim, id_cmpstru_id, &
      id_longculvertdim, id_longculvert_id, &
      id_latdim, id_lat_id, id_lat_predis_inst, id_lat_predis_ave, id_lat_realdis_inst, id_lat_realdis_ave, &
      id_rugdim, id_rugx, id_rugy, id_rugid, id_rugname, &
      id_statgeom_node_count,          id_statgeom_node_coordx,          id_statgeom_node_coordy,          id_statgeom_node_lon, id_statgeom_node_lat, &
      id_latgeom_node_count,           id_latgeom_node_coordx,           id_latgeom_node_coordy,     &
      id_weirgengeom_input_node_count, id_weirgengeom_input_node_coordx, id_weirgengeom_input_node_coordy, &
      id_weirgengeom_node_count,       id_weirgengeom_node_coordx,       id_weirgengeom_node_coordy,       id_weirgen_xmid,     id_weirgen_ymid, &
      id_crsgeom_node_count,           id_crsgeom_node_coordx,           id_crsgeom_node_coordy,           id_crs_xmid,         id_crs_ymid, &
      id_orifgengeom_node_count,       id_orifgengeom_node_coordx,       id_orifgengeom_node_coordy,       id_orifgen_xmid,     id_orifgen_ymid, &
      id_genstrugeom_node_count,       id_genstrugeom_node_coordx,       id_genstrugeom_node_coordy,       id_genstru_xmid,     id_genstru_ymid, &
      id_uniweirgeom_node_count,       id_uniweirgeom_node_coordx,       id_uniweirgeom_node_coordy,       id_uniweir_xmid,     id_uniweir_ymid, &
      id_culvertgeom_node_count,       id_culvertgeom_node_coordx,       id_culvertgeom_node_coordy,       id_culvert_xmid,     id_culvert_ymid, &
      id_gategengeom_node_count,       id_gategengeom_node_coordx,       id_gategengeom_node_coordy,       id_gategen_xmid,     id_gategen_ymid, &
      id_pumpgeom_node_count,          id_pumpgeom_node_coordx,          id_pumpgeom_node_coordy,          id_pump_xmid,        id_pump_ymid, &
      id_bridgegeom_node_count,        id_bridgegeom_node_coordx,        id_bridgegeom_node_coordy,        id_bridge_xmid,      id_bridge_ymid, &
      id_srcgeom_node_count,           id_srcgeom_node_coordx,           id_srcgeom_node_coordy,           id_src_xmid,         id_src_ymid, &
      id_longculvertgeom_node_count,   id_longculvertgeom_node_coordx,   id_longculvertgeom_node_coordy,   id_longculvert_xmid, id_longculvert_ymid

   end module m_his_file_netcdf_ids
   