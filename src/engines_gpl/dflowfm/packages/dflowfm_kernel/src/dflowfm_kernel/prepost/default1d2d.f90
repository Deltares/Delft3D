module m_default1d2d
   implicit none

   private
   public add_default_cross_sections_for_1d2d_links

contains

   !> Some 1D2D links require 'default' cross section in the 1D channel administration.
   !! This subroutine installs the required cross section definitions, adds an instance
   !! of this cross section, and assigns the default cross sections to the 1D2D links.
   subroutine add_default_cross_sections_for_1d2d_links(network)
      use precision, only: dp
      use m_network, only: t_network
      use network_data, only: kn, numl1d, LINK_1D2D_STREETINLET
      use m_GlobalParameters, only: t_chainage2cross
      use m_readcrosssections, only: finalizeCrs
      use MessageHandling, only: SetMessage, LEVEL_ERROR

      use m_flowgeom, only: wu1Duni5, hh1Duni5
      use m_physcoef, only: frcunistreetinlet
      use m_Roughness, only: R_MANNING

      type(t_network), target, intent(inout) :: network !< The 1D channel administration.

      integer :: idef, icrs, L, stat

      call add_1d2d_cross_section_definition( &
         network%CSDefinitions, width=wu1Duni5, height=hh1Duni5, &
         friction_type=R_MANNING, friction_value=frcunistreetinlet, idef=idef &
      )
      call add_cross_section(network, idef, icrs)

      call realloc_line2cross(network%adm%line2cross, [numl1d, 3], stat=stat)
      if (stat /= 0) then
         call SetMessage(LEVEL_ERROR, "Failed to reallocate line2cross")
         return
      end if

      do L = 1, numl1d
         associate (link_cross_sections => network%adm%line2cross(L, :))
            ! Street inlets share their link code `LINK_1D2D_STREETINLET` with long culverts. We assume that
            ! at this point long culverts already have their cross sections installed in `line2cross`.
            if (kn(3, L) == LINK_1D2D_STREETINLET .and. all(link_cross_sections%c1 == -1)) then
               link_cross_sections = t_chainage2cross(c1=icrs, c2=icrs, f=1.0_dp, distance=0.0_dp)
            end if
         end associate
      end do
   end subroutine add_default_cross_sections_for_1d2d_links

   !> Add a rectangular closed cross section, with a friction section, to the definition set.
   subroutine add_1d2d_cross_section_definition(cs_defs, width, height, friction_type, friction_value, idef)
      use precision, only: dp
      use m_CrossSections, only: t_CSDefinitionSet, AddCrossSectionDefinition

      type(t_CSDefinitionSet), intent(inout) :: cs_defs !< Cross section definition set to add the new cross section to.
      real(kind=dp), intent(in) :: width !< Width of the new (rectangular) cross section definition.
      real(kind=dp), intent(in) :: height !< Height of the new (rectagular) cross section definition.
      integer, intent(in) :: friction_type !< Friction type of the new cross section definition. See `m_roughness` for examples.
      real(kind=dp), intent(in) :: friction_value !< Friction value of the new cross section definition.
      integer, intent(out) :: idef !< The returned identifier of the new cross section definition.

      real(kind=dp) :: heights(2)
      real(kind=dp) :: widths(2)
      real(kind=dp) :: plains(3)

      ! Create a default 1D2D rectangular cross-section definition
      heights = [0.0_dp, height]
      widths = width
      plains = 0.0_dp

      ! Add the cross-section definition to the network
      idef = AddCrossSectionDefinition( &
         cs_defs, id='default_1d2d_rect', numLevels=2, level=heights, &
         flowWidth=widths, totalWidth=widths, plains=plains, &
         crestLevel=0.0_dp, baseLevel=0.0_dp, flowArea=0.0_dp, totalArea=0.0_dp, &
         closed=.true., groundlayerUsed=.false., groundlayer=0.0_dp &
      )

      ! Set up friction section using default 1D2D friction values
      associate(cs_def => cs_defs%CS(idef))
         cs_def%frictionSectionsCount = 1
         cs_def%frictionSectionID = ['']
         cs_def%frictionSectionIndex = [0]
         cs_def%frictionType = [friction_type]
         cs_def%frictionValue = [friction_value]
      end associate
   end subroutine add_1d2d_cross_section_definition

   !> Add an instance of the cross section with index `idef` to the cross sections in the 1D channel `network`.
   subroutine add_cross_section(network, idef, icrs)
      use m_network, only: t_network
      use m_readcrosssections, only: finalizeCrs
      use m_CrossSections, only: realloc

      type(t_network), intent(inout) :: network !< The 1D channel network object.
      integer, intent(in) :: idef !< The cross section definition index.
      integer, intent(out) :: icrs !< The index of the new cross section instance.

      associate(cross_sections => network%crs)
         if (cross_sections%count + 1 > cross_sections%size) then
            call realloc(cross_sections)
         end if
         icrs = cross_sections%count + 1
         call finalizeCrs(network, cross_sections%cross(icrs), idef, icrs)
      end associate
   end subroutine add_cross_section

   !> Re-allocate and reshape the `line2cross` to have `new_size`. If reallocation is necessary
   !! then copy the old elements to the new array.
   subroutine realloc_line2cross(line2cross, new_size, stat)
      use m_GlobalParameters, only: t_chainage2cross

      type(t_chainage2cross), dimension(:,:), pointer, intent(inout) :: line2cross !< Link to cross sections administration.
      integer, dimension(2), intent(in) :: new_size !< New shape of the of the `line2cross` array.
      integer, intent(out) :: stat !< Status code returned to make error handling possible.

      type(t_chainage2cross), dimension(:,:), pointer :: temp
      integer, dimension(2) :: copy_size

      stat = 0  ! Default `stat` value: No error

      if (associated(line2cross)) then
         if (all(new_size == ubound(line2cross))) then
            return ! Array is already the right size. Nothing to do
         end if
      end if

      ! Reallocation required
      allocate(temp(1:new_size(1), 1:new_size(2)), stat=stat)
      if (stat /= 0) then
         return
      end if

      ! Copy as many elements in old `line2cross` as possible
      if (associated(line2cross)) then
         copy_size = min(ubound(line2cross), new_size)
         temp(1:copy_size(1), 1:copy_size(2)) = line2cross(1:copy_size(1), 1:copy_size(2))
         deallocate(line2cross, stat=stat)
      end if

      line2cross => temp
   end subroutine realloc_line2cross

end module m_default1d2d
