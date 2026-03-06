module icecover_input_module
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
!-------------------------------------------------------------------------------!

use icecover_module, only: icecover_type, ICECOVER_EXT, ICECOVER_NONE
use properties, only: tree_data, prop_get
use MessageHandling, only: mess, LEVEL_ERROR, LEVEL_ALL, LEVEL_FATAL
use string_module, only: str_lower
implicit none

private

!
! functions and subroutines
!
public late_activation_ext_force_icecover
public read_icecover
public echo_icecover

contains


!> activation of icecover module based on external forcing input
function late_activation_ext_force_icecover(icecover, md_ptr, chapter) result(istat)
   type (icecover_type), intent(inout) :: icecover !< data structure containing ice cover data
   type(tree_data), pointer, intent(in) :: md_ptr !< pointer to the input file
   character(len=*), intent(in) :: chapter !< chapter name of the ice section
   
   integer :: istat !< status flag for allocation

   istat = 0
   if (icecover%model_type == ICECOVER_EXT) then
      ! icecover already set to externally forced
   elseif (icecover%model_type == ICECOVER_NONE) then
      ! activate icecover and switch on the pressure effect
      icecover%model_type = ICECOVER_EXT
      icecover%apply_pressure = .true.
      call mess(LEVEL_ALL, 'Activating ice cover module based on external forcing.')
      ! we need to rescan the output flags since the model_type has changed which affects the default
      call read_icecover_output(icecover, md_ptr, chapter)
   else
      ! don't overrule previously selected icecover ...
      call mess(LEVEL_FATAL, 'Ice cover forcing data conflicts with selected ice cover model.')
   endif
end function late_activation_ext_force_icecover


!> Read ice cover settings (Note: the meteo module should already have been initialized)
subroutine read_icecover(icecover, md_ptr, chapter, error)
   type (icecover_type), intent(inout) :: icecover !< ice cover data structure containing the data read
   type(tree_data), pointer, intent(in) :: md_ptr !< pointer to the input file
   character(len=*), intent(in) :: chapter !< chapter name of the ice section
   logical, intent(out) :: error !< flag indicating an execution error

   call determine_icecover_model(icecover, md_ptr, chapter, error)
   if (error) return

   call read_icecover_parameters(icecover, md_ptr, chapter, error)
   if (error) return
   
   call read_icecover_output(icecover, md_ptr, chapter)
end subroutine read_icecover


!> Support routine to determine which icecover model has been selected
subroutine determine_icecover_model(icecover, md_ptr, chapter, error)
   use icecover_module, only: select_icecover_model, ICECOVER_NONE, ICECOVER_EXT, ICECOVER_SEMTNER

   type (icecover_type), intent(inout) :: icecover !< ice cover data structure containing the data read
   type(tree_data), pointer, intent(in) :: md_ptr !< pointer to the input file
   character(len=*), intent(in) :: chapter !< chapter name of the ice section
   logical, intent(out) :: error !< flag indicating an execution error

   integer :: istat !< reading status flag
   integer :: model_type !< local ice cover model flag
   character(256) :: tmp !< temporary string for input processing

   error = .false.
   model_type = ICECOVER_NONE
      
   tmp = ' '
   call prop_get(md_ptr,chapter,'iceCoverModel',tmp)
   call str_lower(tmp,len(tmp))
   select case (tmp)
   case ('none', ' ')
      ! default selected
   case ('external')
      model_type = ICECOVER_EXT
   case ('semtner','deltares')
      model_type = ICECOVER_SEMTNER
   case default
      call mess(LEVEL_ERROR, 'invalid ice cover model "'//trim(tmp)//'"')
      error = .true.
      ! still want to properly initialize the icecover module, so don't return immediately
   end select
   istat = select_icecover_model(icecover, model_type)
end subroutine determine_icecover_model


!> Support routine to read ice cover parameters
subroutine read_icecover_parameters(icecover, md_ptr, chapter, error)
   use icecover_module, only: ICECOVER_EXT
   use icecover_module, only: FRICT_AS_DRAG_COEFF
   use icecover_module, only: ICE_WINDDRAG_NONE, ICE_WINDDRAG_CUBIC, ICE_WINDDRAG_LB05, &
      & ICE_WINDDRAG_AN10, ICE_WINDDRAG_LINEAR, ICE_WINDDRAG_RAYS, ICE_WINDDRAG_JOYCE19
   !
   type (icecover_type), intent(inout) :: icecover !< ice cover data structure containing the data read
   type(tree_data), pointer, intent(in) :: md_ptr !< pointer to the input file
   character(len=*), intent(in) :: chapter !< chapter name of the ice section
   logical, intent(out) :: error !< flag indicating an execution error
   !
   integer :: model_type !< local ice cover model flag
   character(256) :: tmp !< temporary string for input processing

   call prop_get(md_ptr, chapter, 'applyPressure', icecover%apply_pressure)
   call prop_get(md_ptr, chapter, 'applyFriction', icecover%apply_friction)
   call prop_get(md_ptr, chapter, 'reduceSurfExch', icecover%reduce_surface_exchange)
   call prop_get(md_ptr, chapter, 'reduceWaves', icecover%reduce_waves)
   tmp = ' '
   call prop_get(md_ptr, chapter, 'modifyWindDrag', tmp)
   call str_lower(tmp, len(tmp))
   select case (tmp)
   case ('none', ' ')
      model_type = ICE_WINDDRAG_NONE
   case ('linear')
      model_type = ICE_WINDDRAG_LINEAR
   case ('cubic', 'icecube')
      model_type = ICE_WINDDRAG_CUBIC
   case ('lupkes_birnbaum')
      model_type = ICE_WINDDRAG_LB05
   case ('andreas')
      model_type = ICE_WINDDRAG_AN10
   case ('raysice')
      model_type = ICE_WINDDRAG_RAYS
   case ('joyce')
      model_type = ICE_WINDDRAG_JOYCE19
      call prop_get(md_ptr, chapter, 'iceSkinDrag', icecover%ice_skin_drag)
      call prop_get(md_ptr, chapter, 'maximumIceFormDrag', icecover%maximum_ice_form_drag)
   case default
      call mess(LEVEL_ERROR, 'invalid wind drag option "'//trim(tmp)//'"')
      error = .true.
      return
   end select
   icecover%modify_winddrag = model_type
   !
   call prop_get(md_ptr, chapter, 'iceDensity', icecover%ice_density)
   call prop_get(md_ptr, chapter, 'iceAlbedo', icecover%ice_albedo)
   call prop_get(md_ptr, chapter, 'snowAlbedo', icecover%snow_albedo)
   !
   tmp = ' '
   call prop_get(md_ptr, chapter, 'iceFricType', tmp)
   call str_lower(tmp, len(tmp))
   select case (tmp)
   case ('cdrag', ' ')
      ! default selected
      icecover%frict_type = FRICT_AS_DRAG_COEFF
   case default
      call mess(LEVEL_ERROR, 'invalid ice cover friction type "'//trim(tmp)//'", only "cdrag" supported.')
      error = .true.
      return
   end select
   call prop_get(md_ptr, chapter, 'iceFricValue', icecover%frict_val)
   !
   select case (icecover%model_type)
   case (ICECOVER_EXT)
      ! No extra parameters for external forcing of ice cover
   case default
   
   end select
end subroutine read_icecover_parameters


!> Support routine to read all ice cover output settings
subroutine read_icecover_output(icecover, md_ptr, chapter)
   use properties, only: tree_data, prop_get

   type (icecover_type), intent(inout) :: icecover !< ice cover data structure containing the data read
   type(tree_data), pointer, intent(in) :: md_ptr !< pointer to the input file
   character(len=*), intent(in) :: chapter !< chapter name of the ice section

   ! backward compatibility flags located in the ice and output chapters
   call prop_get(md_ptr, chapter, 'addIceToHis', icecover%hisout%default)
   call prop_get(md_ptr, chapter, 'addIceToMap', icecover%mapout%default)
   call prop_get(md_ptr, 'output', 'wriMap_ice', icecover%mapout%default)
   
   ! new output flags always located in the output chapter
   call read_output_flags_per_quantity(icecover%hisout, md_ptr, 'output', 'wriHis', icecover%model_type)
   call read_output_flags_per_quantity(icecover%mapout, md_ptr, 'output', 'wriMap', icecover%model_type)
end subroutine read_icecover_output


!> Support routine to read ice cover output settings per quantity for selected output type
subroutine read_output_flags_per_quantity(outflags, md_ptr, chapter, prefix, model_type)
   use icecover_module, only: apply_default_output_flag, icecover_output_flags
   use properties, only: tree_data, prop_get
   
   type(icecover_output_flags), intent(inout) :: outflags !< output flags structure
   type(tree_data), pointer, intent(in) :: md_ptr !< pointer to the input file
   character(len=*), intent(in) :: chapter !< chapter name of the ice section
   character(len=*), intent(in) :: prefix !< name of output file   
   integer, intent(in) :: model_type !< ice cover model flag
   
   call prop_get(md_ptr, chapter, prefix//'_ice_default', outflags%default)
   call apply_default_output_flag(outflags, model_type)
   
   call prop_get(md_ptr, chapter, prefix//'_ice_open_water_level', outflags%ice_s1)
   call prop_get(md_ptr, chapter, prefix//'_ice_lower_surface_height', outflags%ice_zmin)
   call prop_get(md_ptr, chapter, prefix//'_ice_surface_height', outflags%ice_zmax)
   call prop_get(md_ptr, chapter, prefix//'_ice_area_fraction', outflags%ice_area_fraction)
   call prop_get(md_ptr, chapter, prefix//'_ice_thickness', outflags%ice_thickness)
   call prop_get(md_ptr, chapter, prefix//'_ice_pressure', outflags%ice_pressure)
   call prop_get(md_ptr, chapter, prefix//'_ice_temperature', outflags%ice_temperature)
   call prop_get(md_ptr, chapter, prefix//'_snow_thickness', outflags%snow_thickness)
   call prop_get(md_ptr, chapter, prefix//'_snow_temperature', outflags%snow_temperature)
end subroutine read_output_flags_per_quantity


!> Write ice cover settings to diagnostic
function echo_icecover(icecover, lundia) result (error)
   use precision
   use icecover_module, only: icecover_type, &
       & ICECOVER_NONE, ICECOVER_EXT, ICECOVER_SEMTNER, FRICT_AS_DRAG_COEFF, &
       & ICE_WINDDRAG_NONE, ICE_WINDDRAG_CUBIC, ICE_WINDDRAG_LB05, ICE_WINDDRAG_AN10, &
       & ICE_WINDDRAG_LINEAR, ICE_WINDDRAG_RAYS, ICE_WINDDRAG_JOYCE19
   use MessageHandling, only: mess, LEVEL_ERROR
   !

   type (icecover_type)         , intent(inout) :: icecover !< ice cover data structure containing the data read
   integer                      , intent(in)    :: lundia   !< unit number of diagnostics file
   logical                                      :: error    !< flag indicating an execution error

   character(45) :: txtput1 !< string for parameter description
   character(120) :: txtput2 !< string for parameter value

   ! don't print any ice messages if there is no ice cover
   error = .false.
   if (icecover%model_type == ICECOVER_NONE) return

   write (lundia, '(a)' ) '*** Start  of ice cover input'
   
   txtput1 = '  Ice cover model'
   select case (icecover%model_type)
   case (ICECOVER_EXT)
       txtput2 = 'external'
   case (ICECOVER_SEMTNER)
       txtput2 = 'Semtner (1975)'
   case default
       txtput2 = 'unknown'
   end select
   write (lundia, '(3a)') txtput1, ': ', txtput2
   
   if (icecover%model_type == ICECOVER_EXT) then
      txtput1 = '  Area fraction forcing'
      call write_logical(lundia, txtput1, icecover%ice_area_fraction_forcing_available /= 0)

      txtput1 = '  Ice thickness forcing'
      call write_logical(lundia, txtput1, icecover%ice_thickness_forcing_available /= 0)
      
      if (icecover%ice_area_fraction_forcing_available == 0 .and. icecover%ice_thickness_forcing_available == 0) then
         call mess(LEVEL_ERROR, 'icecover set to external forcing but no forcing data found')
         error = .true.
      elseif (icecover%ice_area_fraction_forcing_available == 0) then
         call mess(LEVEL_ERROR, 'icecover set to external forcing but no area fraction forcing found')
         error = .true.
      elseif (icecover%ice_thickness_forcing_available == 0) then
         call mess(LEVEL_ERROR, 'icecover set to external forcing but no ice thickness forcing found')
         error = .true.
      endif
   endif

   txtput1 = '  Apply pressure'
   call write_logical(lundia, txtput1, icecover%apply_pressure)

   txtput1 = '  Apply friction'
   call write_logical(lundia, txtput1, icecover%apply_friction)

   txtput1 = '  Reduce surface exchange'
   call write_logical(lundia, txtput1, icecover%reduce_surface_exchange)

   txtput1 = '  Reduce waves'
   call write_logical(lundia, txtput1, icecover%reduce_waves)

   txtput1 = '  Modify wind drag'
   select case (icecover%modify_winddrag)
   case (ICE_WINDDRAG_NONE)
       write (lundia, '(2a)') txtput1, ': No'
   case (ICE_WINDDRAG_CUBIC)
       write (lundia, '(2a)') txtput1, ': Cubic (Chapman & Massey)'
   case (ICE_WINDDRAG_RAYS)
       write (lundia, '(2a)') txtput1, ': RaysIce (Chapman et al.)'
   case (ICE_WINDDRAG_LB05)
       write (lundia, '(2a)') txtput1, ': Lupkes & Birnbaum (2005)'
       txtput1 = 'Skin drag for ice floes'
       write (lundia, '(2a,e20.4)') txtput1, ': ', icecover%ice_skin_drag
   case (ICE_WINDDRAG_AN10)
       write (lundia, '(2a)') txtput1, ': Andreas et al. (2010)'
   case (ICE_WINDDRAG_JOYCE19)
       write (lundia, '(2a)') txtput1, ': Joyce et al. (2019)'
       txtput1 = 'Skin drag for ice floes'
       write (lundia, '(2a,e20.4)') txtput1, ': ', icecover%ice_skin_drag
       txtput1 = 'Max form drag for ice floes'
       write (lundia, '(2a,e20.4)') txtput1, ': ', icecover%maximum_ice_form_drag
   case (ICE_WINDDRAG_LINEAR)
       write (lundia, '(2a)') txtput1, ': No drag below ice'
   end select

   txtput1 = '  Albedo of ice cover'
   write (lundia, '(2a,e20.4)') txtput1, ': ', icecover%ice_albedo
   txtput1 = '  Albedo of snow cover'
   write (lundia, '(2a,e20.4)') txtput1, ': ', icecover%snow_albedo
   txtput1 = '  Ice Density'
   write (lundia, '(2a,e20.4)') txtput1, ': ', icecover%ice_density

   txtput1 = '  Ice cover friction type'
   select case (icecover%frict_type)
   case (FRICT_AS_DRAG_COEFF)
       txtput2 = 'drag coefficient'
   case default
       txtput2 = 'unknown'
   end select
   write (lundia, '(3a)') txtput1, ': ', txtput2
   txtput1 = '  Ice Cover Friction Value'
   write (lundia, '(2a,e20.4)') txtput1, ': ', icecover%frict_val

   txtput1 = '  His File Output Quantities'
   write (lundia, '(1a)') txtput1
   call echo_icecover_output(lundia, icecover%hisout, icecover%model_type)

   txtput1 = '  Map File Output Quantities'
   write (lundia, '(1a)') txtput1
   call echo_icecover_output(lundia, icecover%mapout, icecover%model_type)

   write (lundia, '(a)' ) '*** End    of ice cover input'
   write (lundia, *)
end function echo_icecover


!> Support routine to write ice cover output settings to diagnostic
subroutine echo_icecover_output(lundia, outflags, model_type)
   use icecover_module, only: icecover_output_flags, check_output_flags

   integer , intent(in) :: lundia !< unit number of diagnostics file
   type(icecover_output_flags), intent(inout) :: outflags !< output flags structure
   integer, intent(in) :: model_type !< ice cover model type

   logical :: any_quantity !< flag indicating if any quantity has been selected for output

   call check_output_flags(outflags, model_type)
   
   any_quantity = .false.
   call write_quantity_name(outflags%ice_s1, lundia, 'sea surface height of open water', any_quantity)
   call write_quantity_name(outflags%ice_zmin, lundia, 'lower surface height of ice cover', any_quantity)
   call write_quantity_name(outflags%ice_zmax, lundia, 'upper surface height of ice cover', any_quantity)
   call write_quantity_name(outflags%ice_area_fraction, lundia, 'area fraction covered by ice', any_quantity)
   call write_quantity_name(outflags%ice_thickness, lundia, 'ice thickness', any_quantity)
   call write_quantity_name(outflags%ice_pressure, lundia, 'pressure of ice cover', any_quantity)
   call write_quantity_name(outflags%ice_temperature, lundia, 'temperature of ice cover', any_quantity)
   call write_quantity_name(outflags%snow_thickness, lundia, 'snow thickness', any_quantity)
   call write_quantity_name(outflags%snow_temperature, lundia, 'temperature of snow cover', any_quantity)
   if (.not. any_quantity) then
      write(lundia, '(a)') '  * No ice cover output quantities selected'
   end if
end subroutine echo_icecover_output

!> Support routine for reporting output quantities to diagnostic file
subroutine write_quantity_name(output_flag, lundia, quantity_name, any_flag_true)
   logical, intent(in) :: output_flag !< flag specifying if the quantity should be written
   integer, intent(in) :: lundia !< unit number of diagnostics file
   character(len=*), intent(in) :: quantity_name !< name of the quantity
   logical, intent(inout) :: any_flag_true !< flag indicating if any quantity has been written

   if (output_flag) then
      write (lundia, '(2a)') '  * ', quantity_name
      any_flag_true = .true.
   end if
end subroutine write_quantity_name

!> Support routine to write logical flags to diagnostic file
subroutine write_logical(lundia, txtput1, option)
   integer                      , intent(in)    :: lundia   !< unit number of diagnostics file
   character(*)                 , intent(in)    :: txtput1  !< base string
   logical                      , intent(in)    :: option   !< logical option to report

   if (option) then
       write (lundia, '(3a)') txtput1, ': Yes'
   else
       write (lundia, '(3a)') txtput1, ': No'
   endif
end subroutine write_logical

end module icecover_input_module