module m_default1d2d
  implicit none

contains

subroutine set_default_1d2d_crs(network)
   use precision, only: dp
   use m_network, only: t_network
   use m_flowgeom, only: wu1Duni5, hh1Duni5
   use m_physcoef, only: frcuni1d2d, ifrctypuni
   use m_CrossSections, only: AddCrossSectionDefinition
   use network_data, only: numl1d, kn
   use m_GlobalParameters, only: t_chainage2cross

   type(t_network), target,  intent(inout)   :: network
   
   ! Local variables
   integer :: inext, L
   integer :: numLevels
   real(kind=dp) :: level(2)
   real(kind=dp) :: width(2)
   real(kind=dp) :: plains(3)
   real(kind=dp) :: crestLevel, baseLevel, flowArea, totalArea
   logical :: closed
   logical :: groundlayerUsed
   real(kind=dp) :: groundlayer
   character(len=:), allocatable :: id
   integer, parameter :: MANNING = 1
   type(t_chainage2cross), dimension(:,:), allocatable :: temp_line2cross
   
   ! Create a default 1D2D rectangular cross-section definition
   id = 'default_1d2d_rect'
   closed = .true.
   groundlayerUsed = .false.
   groundlayer = 0.0_dp
   
   ! Set up rectangular profile with wu1Duni5 (width) and hh1Duni5 (height)
   numLevels = 2  ! closed rectangular section has 2 levels
   
   level(1) = 0.0_dp           ! Bottom level
   level(2) = hh1Duni5        ! Top level (height)
   width(1) = wu1Duni5        ! Width at bottom
   width(2) = wu1Duni5        ! Width at top (same for rectangle)
   
   plains     = wu1Duni5 
   crestLevel = 0.0_dp
   baseLevel  = 0.0_dp
   flowArea   = 0.0_dp
   totalArea  = 0.0_dp
   
   ! Add the cross-section definition to the network
   inext = AddCrossSectionDefinition(network%CSDefinitions, id, numLevels, level, width, &
                                     width, plains, crestLevel, baseLevel, flowArea, totalArea, &
                                     closed, groundlayerUsed, groundlayer)
   
   if (inext > 0) then
      ! Set up friction section using default 1D2D friction values
      network%CSDefinitions%CS(inext)%frictionSectionsCount = 1
      allocate(network%CSDefinitions%CS(inext)%frictionSectionID(1))
      allocate(network%CSDefinitions%CS(inext)%frictionSectionIndex(1))
      allocate(network%CSDefinitions%CS(inext)%frictionType(1))
      allocate(network%CSDefinitions%CS(inext)%frictionValue(1))
      
      network%CSDefinitions%CS(inext)%frictionSectionID(1) = 'Main'
      network%CSDefinitions%CS(inext)%frictionSectionIndex(1) = 0  
      
      ! Use default friction values for 1D2D links
      ! For lateral 1D2D links, use frcuni1d2d with ifrctypuni
      network%CSDefinitions%CS(inext)%frictionType(1) = ifrctypuni
      network%CSDefinitions%CS(inext)%frictionValue(1) = frcuni1d2d
      
      ! Note: For street inlets and roof gutters, you would use:
      ! frictionType = MANNING
      ! frictionValue = frcunistreetinlet or frcuniroofgutterpipe
   endif
temp_line2cross = network%adm%line2cross
deallocate(network%adm%line2cross)
allocate(network%adm%line2cross(numl1d,3))
network%adm%line2cross(1:size(temp_line2cross, 1), :) = temp_line2cross
do L = 1, numL1D
   if (kn(3,L) == 5) then
      network%adm%line2cross(L, :)%c1 = inext
      network%adm%line2cross(L, :)%c2 = inext
      network%adm%line2cross(L, :)%f  = 1.0_dp
      network%adm%line2cross(L, :)%distance  = 0.0_dp
   end if
end do
      
end subroutine set_default_1d2d_crs

end module m_default1d2d