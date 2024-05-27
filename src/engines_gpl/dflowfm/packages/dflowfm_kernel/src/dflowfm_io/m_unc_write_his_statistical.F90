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

module m_unc_write_his_statistical

   use MessageHandling, only: mess, LEVEL_DEBUG, LEVEL_ERROR, err
   use unstruc_netcdf, only: ihisfile, definencvar
   use m_flowtimes, only: handle_extra, it_his
   use netcdf_utils, only: check_netcdf_error
   use timers, only: timon, timstrt, timstop
   use m_missing, only: dmiss
   use m_unc_write_his_ids

   implicit none
   
   private
   
   public :: unc_write_his_def_statoutput, unc_write_his_write_statoutput
   
contains

!> Define all user-requested statistical output variables
subroutine unc_write_his_def_statoutput(statcoordstring)
   use m_flowparameters, only: jahissed
   use m_sediment, only: jased, stm_included, stmpar
   use fm_statistical_output, only: config_set_his, out_variable_set_his
   use m_output_config, only: IDX_HIS_SBCX, IDX_HIS_SSCY, t_output_quantity_config, id_nc_type2nc_type_his, &
      UNC_LOC_GLOBAL, UNC_LOC_SOSI, UNC_LOC_GENSTRU, UNC_LOC_DAM, UNC_LOC_PUMP, UNC_LOC_GATE, &
      UNC_LOC_GATEGEN, UNC_LOC_WEIRGEN, UNC_LOC_ORIFICE, UNC_LOC_BRIDGE, UNC_LOC_CULVERT, &
      UNC_LOC_DAMBREAK, UNC_LOC_UNIWEIR, UNC_LOC_CMPSTRU, UNC_LOC_LONGCULVERT, UNC_LOC_STATION, &
      UNC_LOC_OBSCRS, UNC_LOC_LATERAL, UNC_LOC_RUG, UNC_LOC_DREDGE, UNC_LOC_DUMP, UNC_LOC_DRED_LINK
   use m_statistical_output_types, only: SO_CURRENT, SO_AVERAGE, SO_MAX, SO_MIN
   use m_ug_nc_attribute, only: ug_nc_attribute
   use netcdf, only: nf90_put_att
   
   character(len=1024), intent(in) :: statcoordstring
   
   character(len=25)                       :: transpunit
   integer                                 :: ivar
   character(len=4)                        :: stat_name_postfix
   character(len=11)                       :: stat_name_filter_postfix
   character(len=16)                       :: stat_long_name_postfix
   character(len=16)                       :: stat_cell_methods
   character(len=43)                       :: stat_cell_methods_filter_postfix
   character(len=255)                      :: var_name, var_standard_name, var_long_name
   type(t_output_quantity_config), pointer :: config
   type(ug_nc_attribute), target           :: attributes(4)
   character(len=1024)                     :: local_statcoordstring

   ! set sediment transport unit after modelinit
   if (jahissed > 0 .and. jased > 0 .and. stm_included) then
      select case(stmpar%morpar%moroutput%transptype)
      case (0)
         transpunit = 'kg s-1 m-1'
      case (1)
         transpunit = 'm3 s-1 m-1'
      case (2)
         transpunit = 'm3 s-1 m-1'
      end select
      do ivar = IDX_HIS_SBCX, IDX_HIS_SSCY
         config_set_his%configs(ivar)%unit = transpunit
      end do
   end if

   do ivar = 1, out_variable_set_his%count
      associate(config => out_variable_set_his%statout(ivar)%output_config, &
                  id_var => out_variable_set_his%statout(ivar)%id_var)

      if (config%location_specifier         /= UNC_LOC_STATION &
            .and. config%location_specifier /= UNC_LOC_OBSCRS &
            .and. config%location_specifier /= UNC_LOC_GLOBAL &
            .and. config%location_specifier /= UNC_LOC_SOSI &
            .and. config%location_specifier /= UNC_LOC_RUG &
            .and. config%location_specifier /= UNC_LOC_GENSTRU &
            .and. config%location_specifier /= UNC_LOC_DAM &
            .and. config%location_specifier /= UNC_LOC_PUMP &
            .and. config%location_specifier /= UNC_LOC_GATE &
            .and. config%location_specifier /= UNC_LOC_GATEGEN &
            .and. config%location_specifier /= UNC_LOC_WEIRGEN &
            .and. config%location_specifier /= UNC_LOC_ORIFICE &
            .and. config%location_specifier /= UNC_LOC_BRIDGE &
            .and. config%location_specifier /= UNC_LOC_CULVERT &
            .and. config%location_specifier /= UNC_LOC_DAMBREAK &
            .and. config%location_specifier /= UNC_LOC_UNIWEIR &
            .and. config%location_specifier /= UNC_LOC_CMPSTRU &
            .and. config%location_specifier /= UNC_LOC_LONGCULVERT &
            .and. config%location_specifier /= UNC_LOC_LATERAL &
            .and. config%location_specifier /= UNC_LOC_DREDGE &
            .and. config%location_specifier /= UNC_LOC_DUMP &
            .and. config%location_specifier /= UNC_LOC_DRED_LINK &

      ) then
         call mess(LEVEL_DEBUG, 'unc_write_his: skipping item '//trim(config%name)//', because it''s not on a known output location.')
         cycle
      end if

      select case(out_variable_set_his%statout(ivar)%operation_type)
      case (SO_CURRENT)
         stat_name_postfix      = ''
         stat_long_name_postfix = ''
         stat_cell_methods      = 'time: point'
      case (SO_AVERAGE)
         stat_name_postfix      = '_avg'
         stat_long_name_postfix = ' (average)'
         stat_cell_methods      = 'time: mean'
      case (SO_MAX)
         stat_name_postfix      = '_max'
         stat_long_name_postfix = ' (maximum)'
         stat_cell_methods      = 'time: maximum'
      case (SO_MIN)
         stat_name_postfix      = '_min'
         stat_long_name_postfix = ' (minimum)'
         stat_cell_methods      = 'time: minimum'
      end select
      stat_name_filter_postfix = ''
      stat_cell_methods_filter_postfix = ''
      if (out_variable_set_his%statout(ivar)%moving_average_window > 1) then
         write(stat_name_filter_postfix, '(a,i0)') '_filter', out_variable_set_his%statout(ivar)%moving_average_window
         write(stat_cell_methods_filter_postfix, '(a,i0,a)') ' (moving average filter using ', out_variable_set_his%statout(ivar)%moving_average_window, ' samples)'
      end if

      var_name          = trim(config%name) // trim(stat_name_postfix) // trim(stat_name_filter_postfix)
      var_standard_name = config%standard_name ! Intentionally no pre/postfix for standard_name
      if (len_trim(config%long_name) > 0) then
         var_long_name = trim(config%long_name) // trim(stat_long_name_postfix)
      else
         var_long_name = ''
      end if

      select case(config%location_specifier)
      case (UNC_LOC_SOSI)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_srcdim,         id_timedim /), var_name, var_long_name, config%unit, 'source_sink_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_RUG)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_rugdim,         id_timedim /), var_name, var_long_name, config%unit, 'rug_id', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_GENSTRU)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_genstrudim,     id_timedim /), var_name, var_long_name, config%unit, 'general_structure_id', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_DAM)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_cdamdim,        id_timedim /), var_name, var_long_name, config%unit, 'cdam_id', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_PUMP)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_pumpdim,        id_timedim /), var_name, var_long_name, config%unit, 'pump_id', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_GATE)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_gatedim,        id_timedim /), var_name, var_long_name, config%unit, 'gate_id', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_GATEGEN)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_gategendim,     id_timedim /), var_name, var_long_name, config%unit, 'gategen_id', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_WEIRGEN)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_weirgendim,     id_timedim /), var_name, var_long_name, config%unit, 'weirgen_id', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_ORIFICE)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_orifgendim,     id_timedim /), var_name, var_long_name, config%unit, 'orif_id', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_BRIDGE)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_bridgedim,      id_timedim /), var_name, var_long_name, config%unit, 'bridge_id', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_CULVERT)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_culvertdim,     id_timedim /), var_name, var_long_name, config%unit, 'culvert_id', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_DAMBREAK)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_dambreakdim,    id_timedim /), var_name, var_long_name, config%unit, 'dambreak_id', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_UNIWEIR)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_uniweirdim,     id_timedim /), var_name, var_long_name, config%unit, 'uniweir_id', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_CMPSTRU)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_cmpstrudim,     id_timedim /), var_name, var_long_name, config%unit, 'cmpstru_id', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_LONGCULVERT)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_longculvertdim, id_timedim /), var_name, var_long_name, config%unit, 'longculvert_id', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_LATERAL)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_latdim,         id_timedim /), var_name, var_long_name, config%unit, 'lat_id', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_DREDGE)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_dreddim,        id_timedim /), var_name, var_long_name, config%unit, 'dredge_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
         case (UNC_LOC_DUMP)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_dumpdim,        id_timedim /), var_name, var_long_name, config%unit, 'dump_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
         case (UNC_LOC_DRED_LINK)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_dredlinkdim, id_sedtotdim, id_timedim /), var_name, var_long_name, config%unit, 'dredge_link_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_STATION)
         if (allocated(config%nc_dim_ids)) then
            if (config%nc_dim_ids%laydim) then
               local_statcoordstring = trim(statcoordstring) // ' zcoordinate_c'
            else if (config%nc_dim_ids%laydim_interface_center) then
               local_statcoordstring = trim(statcoordstring) // ' zcoordinate_w'
            else if (config%nc_dim_ids%laydim_interface_edge) then
               local_statcoordstring = trim(statcoordstring) // ' zcoordinate_wu'
            else
               local_statcoordstring = statcoordstring
            end if
            call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), build_nc_dimension_id_list(config%nc_dim_ids), var_name, var_long_name, &
                              config%unit, local_statcoordstring, fillVal=dmiss, add_gridmapping = .true., extra_attributes=config%additional_attributes%atts)
         else
            call err('Internal error, please report: UNC_LOC_STATION variable '//trim(config%name)//' does not have nc_dim_ids set.')
         end if
      case (UNC_LOC_OBSCRS)
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_crsdim, id_timedim /), var_name, var_long_name, config%unit, 'cross_section_name', fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
      case (UNC_LOC_GLOBAL)
         if (timon) call timstrt ( "unc_write_his DEF bal", handle_extra(59))
         call definencvar(ihisfile, id_var, id_nc_type2nc_type_his(config%id_nc_type), (/ id_timedim /), var_name, var_long_name, config%unit, "", fillVal=dmiss, extra_attributes=config%additional_attributes%atts)
         if (timon) call timstop (handle_extra(59))
      end select

      if (len_trim(var_standard_name) > 0) then
         call check_netcdf_error( nf90_put_att(ihisfile, id_var, 'standard_name', trim(var_standard_name)))
      end if
      if (len_trim(stat_cell_methods) > 0) then
         call check_netcdf_error( nf90_put_att(ihisfile, id_var, 'cell_methods', trim(stat_cell_methods) // trim(stat_cell_methods_filter_postfix)))
      end if
      end associate
   end do
         
end subroutine unc_write_his_def_statoutput

!> Write all user-requested statistical output variables
subroutine unc_write_his_write_statoutput
   use fm_statistical_output, only: out_variable_set_his
   use m_output_config, only: &
      UNC_LOC_GLOBAL, UNC_LOC_SOSI, UNC_LOC_GENSTRU, UNC_LOC_DAM, UNC_LOC_PUMP, UNC_LOC_GATE, &
      UNC_LOC_GATEGEN, UNC_LOC_WEIRGEN, UNC_LOC_ORIFICE, UNC_LOC_BRIDGE, UNC_LOC_CULVERT, &
      UNC_LOC_DAMBREAK, UNC_LOC_UNIWEIR, UNC_LOC_CMPSTRU, UNC_LOC_LONGCULVERT, UNC_LOC_STATION, &
      UNC_LOC_OBSCRS, UNC_LOC_LATERAL, UNC_LOC_RUG, UNC_LOC_DREDGE, UNC_LOC_DUMP, UNC_LOC_DRED_LINK
   use netcdf, only: nf90_put_var
   use m_sediment, only: stmpar
   use m_dad, only: dadpar

   integer :: ivar

   do ivar = 1, out_variable_set_his%count
      associate(config => out_variable_set_his%statout(ivar)%output_config, &
                id_var => out_variable_set_his%statout(ivar)%id_var)

      if (config%location_specifier /= UNC_LOC_STATION &
            .and. config%location_specifier /= UNC_LOC_OBSCRS &
            .and. config%location_specifier /= UNC_LOC_GLOBAL &
            .and. config%location_specifier /= UNC_LOC_SOSI &
            .and. config%location_specifier /= UNC_LOC_RUG &
            .and. config%location_specifier /= UNC_LOC_GENSTRU &
            .and. config%location_specifier /= UNC_LOC_DAM &
            .and. config%location_specifier /= UNC_LOC_PUMP &
            .and. config%location_specifier /= UNC_LOC_GATE &
            .and. config%location_specifier /= UNC_LOC_GATEGEN &
            .and. config%location_specifier /= UNC_LOC_WEIRGEN &
            .and. config%location_specifier /= UNC_LOC_ORIFICE &
            .and. config%location_specifier /= UNC_LOC_BRIDGE &
            .and. config%location_specifier /= UNC_LOC_CULVERT &
            .and. config%location_specifier /= UNC_LOC_DAMBREAK &
            .and. config%location_specifier /= UNC_LOC_UNIWEIR &
            .and. config%location_specifier /= UNC_LOC_CMPSTRU &
            .and. config%location_specifier /= UNC_LOC_LONGCULVERT &
            .and. config%location_specifier /= UNC_LOC_LATERAL &
            .and. config%location_specifier /= UNC_LOC_DREDGE &
            .and. config%location_specifier /= UNC_LOC_DUMP &
            .and. config%location_specifier /= UNC_LOC_DRED_LINK &
            ) then
         call mess(LEVEL_DEBUG, 'unc_write_his: skipping item '//trim(config%name)//', because it''s not on a known statistical output location.')
         cycle
      end if

      select case(config%location_specifier)
      case ( UNC_LOC_OBSCRS, &
         UNC_LOC_RUG, &
         UNC_LOC_SOSI, &
         UNC_LOC_GENSTRU, &
         UNC_LOC_DAM, &
         UNC_LOC_PUMP, &
         UNC_LOC_GATE, &
         UNC_LOC_GATEGEN, &
         UNC_LOC_WEIRGEN, &
         UNC_LOC_ORIFICE, &
         UNC_LOC_BRIDGE, &
         UNC_LOC_CULVERT, &
         UNC_LOC_DAMBREAK, &
         UNC_LOC_UNIWEIR, &
         UNC_LOC_CMPSTRU, &
         UNC_LOC_LONGCULVERT, &
         UNC_LOC_LATERAL, &
         UNC_LOC_DREDGE, &
         UNC_LOC_DUMP &
         )
         call check_netcdf_error( nf90_put_var(ihisfile, id_var, out_variable_set_his%statout(ivar)%stat_output, start = (/ 1, it_his /)))
      case (UNC_LOC_STATION)
         call write_station_netcdf_variable(out_variable_set_his%statout(ivar))
      case (UNC_LOC_DRED_LINK)
         call check_netcdf_error( nf90_put_var(ihisfile, id_var, out_variable_set_his%statout(ivar)%stat_output, start = (/ 1, 1, it_his /), count = (/ dadpar%nalink, stmpar%lsedtot, 1 /)))
      case (UNC_LOC_GLOBAL)
         if (timon) call timstrt('unc_write_his IDX data', handle_extra(67))
         call check_netcdf_error( nf90_put_var(ihisfile, id_var, out_variable_set_his%statout(ivar)%stat_output,  start=(/ it_his /)))
         if (timon) call timstop(handle_extra(67))
      end select
      end associate
   end do
   
end subroutine unc_write_his_write_statoutput

!> Convert t_station_nc_dimensions to integer array of NetCDF dimension ids
function build_nc_dimension_id_list(nc_dim_ids) result(res)
   use m_output_config, only: t_station_nc_dimensions
   type(t_station_nc_dimensions), intent(in) :: nc_dim_ids !< The active NetCDF dimensions for this variable
   integer, allocatable                      :: res(:)     !< Array of NetCDF dimension ids

   res = pack([id_laydim, id_laydimw, id_nlyrdim, id_statdim, id_sedsusdim, id_sedtotdim, id_timedim], &
              make_mask_from_dim_ids(nc_dim_ids))
   if (any(res==0)) then
      call mess(LEVEL_ERROR,'A dimension ID was used without being defined!')
   end if
end function build_nc_dimension_id_list

!> Return array of NetCDF dimension start indices corresponding to NetCDF dimensions
function build_nc_dimension_id_start_array(nc_dim_ids) result(starts)
   use m_output_config, only: t_station_nc_dimensions
   type(t_station_nc_dimensions), intent(in) :: nc_dim_ids !< The active NetCDF dimensions for this variable
   integer, allocatable                      :: starts(:)  !< Array of start indices for each NetCDF dimension

   starts = pack([1, 1, 1, 1, 1, 1, it_his], &
              make_mask_from_dim_ids(nc_dim_ids))
end function build_nc_dimension_id_start_array

!> Return array of NetCDF dimension counts corresponding to NetCDF dimensions
function build_nc_dimension_id_count_array(ncid, nc_dim_ids) result(counts)
   use m_output_config, only: t_station_nc_dimensions
   integer,                       intent(in) :: ncid       !< NetCDF id of already open dataset
   type(t_station_nc_dimensions), intent(in) :: nc_dim_ids !< The active NetCDF dimensions for this variable
   integer, allocatable                      :: counts(:)  !< NetCDF dimension counts

   integer, allocatable           :: dim_ids(:)

   dim_ids = build_nc_dimension_id_list(nc_dim_ids)
   counts = [(get_dimid_len(ncid, dim_ids(i)), integer :: i = 1, size(dim_ids))]
   if (nc_dim_ids%timedim) then
      counts(size(counts)) = 1 ! Only write one element for time dimension, which comes last
   end if
end function build_nc_dimension_id_count_array

!> Build mask of which dimensions to include in netcdf variable, based on nc_dim_ids
pure function make_mask_from_dim_ids(nc_dim_ids) result(mask)
   use m_output_config, only: t_station_nc_dimensions
   type(t_station_nc_dimensions), intent(in) :: nc_dim_ids  !< The active NetCDF dimensions for this variable
   logical                                   :: mask(7)     !< The same but as a 1-D array of logicals

   mask = [nc_dim_ids%laydim, &
           nc_dim_ids%laydim_interface_center .or. nc_dim_ids%laydim_interface_edge, &
           nc_dim_ids%nlyrdim, &
           nc_dim_ids%statdim, &
           nc_dim_ids%sedsusdim, &
           nc_dim_ids%sedtotdim, &
           nc_dim_ids%timedim]
end function make_mask_from_dim_ids

!> Gets dimension length from NetCDF dimension id
integer function get_dimid_len(ncid, id)
   use netcdf, only: nf90_inquire_dimension
   integer, intent(in) :: ncid !< NetCDF id of already open dataset
   integer, intent(in) :: id   !< NetCDF id obtained from nf90_def_dim

   call check_netcdf_error( nf90_inquire_dimension(ncid, id, len = get_dimid_len))
end function get_dimid_len

subroutine write_station_netcdf_variable(output_variable_item)
   use m_reshape, only: reshape_implicit
   use MessageHandling, only: err
   use m_statistical_output_types, only: t_output_variable_item
   use m_output_config, only: t_output_quantity_config
   use netcdf, only: nf90_put_var
   
   type(t_output_variable_item), intent(in) :: output_variable_item

   type(t_output_quantity_config), pointer :: local_config
   integer                                 :: local_id_var, station_id_index
   integer, allocatable                    :: counts(:), starts(:), positions(:)
   double precision, allocatable           :: transformed_data(:)

   local_config => output_variable_item%output_config
   local_id_var = output_variable_item%id_var

   if (.not. local_config%nc_dim_ids%statdim) then
      call err('Programming error, please report: Station NetCDF variable must have the station dimension')
   end if

   counts = build_nc_dimension_id_count_array(ihisfile, local_config%nc_dim_ids)
   starts = build_nc_dimension_id_start_array(local_config%nc_dim_ids)

   positions = [(i, integer :: i = 1, size(counts))]
   station_id_index = findloc(build_nc_dimension_id_list(local_config%nc_dim_ids), value = id_statdim, dim = 1)
   ! Bring the dimension corresponding to stations to the front, because it comes first in valobs
   positions(1) = station_id_index
   positions(station_id_index) = 1
   ! Unflatten the array to its proper dimensions (counts), reorder the dimensions to place stations to the front, and flatten it back
   transformed_data = reshape_implicit(output_variable_item%stat_output, counts, positions)

   call check_netcdf_error( nf90_put_var(ihisfile, local_id_var, transformed_data, count = counts, start = starts))
end subroutine write_station_netcdf_variable

end module m_unc_write_his_statistical