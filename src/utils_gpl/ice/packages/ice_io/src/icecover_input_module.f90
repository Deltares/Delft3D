module icecover_input_module
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
!-------------------------------------------------------------------------------!

private

!
! functions and subroutines
!
public read_icecover
public echo_icecover

contains

!> Read ice cover parameters (Note: the meteo module should already have been initialized)
subroutine read_icecover(icecover, md_ptr, chapter, error, jamapice)
!!--declarations----------------------------------------------------------------
   use precision
   use icecover_module, only: icecover_type, alloc_icecover, select_icecover_model, &
      & set_default_output_flags
   use icecover_module, only: ICECOVER_NONE, ICECOVER_EXT, ICECOVER_SEMTNER
   use icecover_module, only: FRICT_AS_DRAG_COEFF
   use icecover_module, only: ICE_WINDDRAG_NONE, ICE_WINDDRAG_CUBIC, ICE_WINDDRAG_LB05, &
      & ICE_WINDDRAG_AN10, ICE_WINDDRAG_LINEAR, ICE_WINDDRAG_RAYS, ICE_WINDDRAG_JOYCE19
   use MessageHandling, only: mess, LEVEL_ERROR
   use properties, only: tree_data, prop_get
   use string_module, only: str_lower
   !
   implicit none
   !
!
! Arguments
! 
   type (icecover_type)         , intent(inout) :: icecover !< ice cover data structure containing the data read
   type(tree_data)              , pointer       :: md_ptr   !< pointer to the input file
   character(len=*)             , intent(in)    :: chapter  !< chapter name of the ice section
   logical                      , intent(out)   :: error    !< flag indicating an execution error
   integer, optional, intent(in) :: jamapice !< flag indicating whether ice data should be written to the map file
!
! Local variables
!
   logical                                      :: default_mapout  !< local logical
   integer                                      :: istat !< reading status flag
   integer                                      :: model !< local ice cover model
   character(256)                               :: tmp   !< temporary string for input processing
!
!! executable statements -------------------------------------------------------
!
   error = .false.
   model =  ICECOVER_NONE
   
   tmp = ' '
   call prop_get(md_ptr,chapter,'iceCoverModel',tmp)
   call str_lower(tmp,len(tmp))
   select case (tmp)
   case ('none', ' ')
      ! default selected
   case ('external')
      model =  ICECOVER_EXT
   case ('semtner','deltares')
      model =  ICECOVER_SEMTNER
   case default
      call mess(LEVEL_ERROR, 'invalid ice cover model "'//trim(tmp)//'"')
      error = .true.
      ! still want to properly initialize the icecover module, so don't return immediately
   end select
   istat = select_icecover_model(icecover, model)
   if (error) return
   !
   ! Process flags
   !
   call prop_get(md_ptr,chapter,'applyPressure',icecover%apply_pressure)
   call prop_get(md_ptr,chapter,'applyFriction',icecover%apply_friction)
   call prop_get(md_ptr,chapter,'reduceSurfExch',icecover%reduce_surface_exchange)
   call prop_get(md_ptr,chapter,'reduceWaves',icecover%reduce_waves)
   tmp = ' '
   call prop_get(md_ptr,chapter,'modifyWindDrag',tmp)
   call str_lower(tmp,len(tmp))
   select case (tmp)
   case ('none',' ')
      model = ICE_WINDDRAG_NONE
   case ('linear')
      model = ICE_WINDDRAG_LINEAR
   case ('cubic','icecube')
      model = ICE_WINDDRAG_CUBIC
   case ('lupkes_birnbaum')
      model = ICE_WINDDRAG_LB05
   case ('andreas')
      model = ICE_WINDDRAG_AN10
   case ('raysice')
      model = ICE_WINDDRAG_RAYS
   case ('joyce')
      model = ICE_WINDDRAG_JOYCE19
      call prop_get(md_ptr,chapter,'iceSkinDrag',icecover%ice_skin_drag)
      call prop_get(md_ptr,chapter,'maximumIceFormDrag',icecover%maximum_ice_form_drag)
   case default
      call mess(LEVEL_ERROR, 'invalid wind drag option "'//trim(tmp)//'"')
      error = .true.
      return
   end select
   icecover%modify_winddrag = model
   !
   ! Parameters
   !
   call prop_get(md_ptr,chapter,'iceDensity',icecover%ice_density)
   call prop_get(md_ptr,chapter,'iceAlbedo' ,icecover%ice_albedo)
   call prop_get(md_ptr,chapter,'snowAlbedo',icecover%snow_albedo)
   !
   tmp = ' '
   call prop_get(md_ptr,chapter,'iceFricType',tmp)
   call str_lower(tmp,len(tmp))
   select case (tmp)
   case ('cdrag', ' ')
      ! default selected
      icecover%frict_type = FRICT_AS_DRAG_COEFF
   case default
      call mess(LEVEL_ERROR, 'invalid ice cover friction type "'//trim(tmp)//'", only "cdrag" supported.')
      error = .true.
      return
   end select
   call prop_get(md_ptr,chapter,'iceFricValue',icecover%frict_val)
   !
   select case (icecover%modeltype)
   case (ICECOVER_EXT)
      ! No extra parameters for external forcing of ice cover
   case default
   
   end select
   !
   ! Output flags
   !
   if (present(jamapice)) then
      default_mapout = jamapice /= 0
   else
      default_mapout = .false.
   end if
   call prop_get(md_ptr, chapter, 'addIceToMap', default_mapout)
   call set_default_output_flags(icecover%mapout, model, default_mapout)
   call read_icecover_output(md_ptr, 'output', 'wrimap', icecover%mapout)
end subroutine read_icecover


!> Write ice cover parametersto diagnostic
function echo_icecover(icecover, lundia) result (error)
!!--declarations----------------------------------------------------------------
   use precision
   use icecover_module, only: icecover_type, alloc_icecover, &
       & ICECOVER_NONE, ICECOVER_EXT, ICECOVER_SEMTNER, FRICT_AS_DRAG_COEFF, &
       & ICE_WINDDRAG_NONE, ICE_WINDDRAG_CUBIC, ICE_WINDDRAG_LB05, ICE_WINDDRAG_AN10, &
       & ICE_WINDDRAG_LINEAR, ICE_WINDDRAG_RAYS, ICE_WINDDRAG_JOYCE19
   use MessageHandling, only: mess, LEVEL_ERROR
   !
   implicit none
!
! Arguments
!
   type (icecover_type)         , intent(inout) :: icecover !< ice cover data structure containing the data read
   integer                      , intent(in)    :: lundia   !< unit number of diagnostics file
   logical                                      :: error    !< flag indicating an execution error
!
! Local variables
!
   character(45)                                                     :: txtput1
   character(120)                                                    :: txtput2
!
!! executable statements -------------------------------------------------------
!
   !
   ! don't print any ice messages if there is no ice cover
   !
   error = .false.
   if (icecover%modeltype == ICECOVER_NONE) return
   !
   write (lundia, '(a)' ) '*** Start  of ice cover input'
   
   txtput1 = '  Ice cover model'
   select case (icecover%modeltype)
   case (ICECOVER_EXT)
       txtput2 = 'external'
   case (ICECOVER_SEMTNER)
       txtput2 = 'Semtner (1975)'
   case default
       txtput2 = 'unknown'
   end select
   write (lundia, '(3a)') txtput1, ': ', txtput2
   
   if (icecover%modeltype == ICECOVER_EXT) then
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
   !
   ! process flags
   !
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

   !
   ! parameters
   !
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

   !
   ! output flags
   !
   txtput1 = '  Map File Output'
   write (lundia, '(1a)') txtput1
   call echo_icecover_output(lundia, icecover%mapout)

   write (lundia, '(a)' ) '*** End    of ice cover input'
   write (lundia, *)
end function echo_icecover

subroutine read_icecover_output(md_ptr, chapter, prefix, outflags)
   use properties, only: tree_data, prop_get
   use icecover_module, only: icecover_output_flags
!
! Arguments
!
   type(tree_data)              , pointer       :: md_ptr   !< pointer to the input file
   character(len=*)             , intent(in)    :: chapter  !< chapter name of the ice section
   character(len=*)             , intent(in)    :: prefix   !< name of output file
   type(icecover_output_flags)  , intent(inout) :: outflags !< output flags structure
!
   character(:), allocatable :: prefix_ !< prefix including underscore
!
!! executable statements -------------------------------------------------------
!
   prefix_ = prefix//'_'
   call prop_get(md_ptr, chapter, prefix//'open_water_level', outflags%ice_s1)
   call prop_get(md_ptr, chapter, prefix//'ice_lower_surface_height', outflags%ice_zmin)
   call prop_get(md_ptr, chapter, prefix//'ice_surface_height', outflags%ice_zmax)
   call prop_get(md_ptr, chapter, prefix//'ice_area_fraction', outflags%ice_area_fraction)
   call prop_get(md_ptr, chapter, prefix//'ice_thickness', outflags%ice_thickness)
   call prop_get(md_ptr, chapter, prefix//'ice_pressure', outflags%ice_pressure)
   call prop_get(md_ptr, chapter, prefix//'ice_temperature', outflags%ice_temperature)
   call prop_get(md_ptr, chapter, prefix//'snow_thickness', outflags%snow_thickness)
   call prop_get(md_ptr, chapter, prefix//'snow_temperature', outflags%snow_temperature)
end subroutine read_icecover_output

subroutine echo_icecover_output(lundia, outflags)
   use icecover_module, only: icecover_output_flags
!
! Arguments
!
   integer                      , intent(in)    :: lundia   !< unit number of diagnostics file
   type(icecover_output_flags)  , intent(in)    :: outflags !< output flags structure
!
! Local variables
!
   character(45) :: txtput1
!
!! executable statements -------------------------------------------------------
!
   if (outflags%ice_s1) then
      write (lundia, '(2a)') '  * ', 'sea surface height of open water'
   end if
   if (outflags%ice_zmin) then
      write (lundia, '(2a)') '  * ', 'lower surface height of ice cover'
   end if
   if (outflags%ice_zmax) then
      write (lundia, '(2a)') '  * ', 'upper surface height of ice cover'
   end if
   if (outflags%ice_area_fraction) then
      write (lundia, '(2a)') '  * ', 'area fraction covered by ice'
   end if
   if (outflags%ice_thickness) then
      write (lundia, '(2a)') '  * ', 'ice thickness'
   end if
   if (outflags%ice_pressure) then
      write (lundia, '(2a)') '  * ', 'pressure of ice cover'
   end if
   if (outflags%ice_temperature) then
      write (lundia, '(2a)') '  * ', 'temperature of ice cover'
   end if
   if (outflags%snow_thickness) then
      write (lundia, '(2a)') '  * ', 'snow thickness'
   end if
   if (outflags%snow_temperature) then
      write (lundia, '(2a)') '  * ', 'temperature of snow cover'
   end if

end subroutine echo_icecover_output


!> Support routine to write logical flags to diagnostic file
subroutine write_logical(lundia, txtput1, option)
!
! Arguments
!
   integer                      , intent(in)    :: lundia   !< unit number of diagnostics file
   character(*)                 , intent(in)    :: txtput1  !< base string
   logical                      , intent(in)    :: option   !< logical option to report
!
! Local variables
!
!    NONE
!
!! executable statements -------------------------------------------------------
!
   if (option) then
       write (lundia, '(3a)') txtput1, ': Yes'
   else
       write (lundia, '(3a)') txtput1, ': No'
   endif
end subroutine write_logical

end module icecover_input_module