!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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
module m_poshcheck

  implicit none
  
  logical :: is_hu_changed
  integer :: key_local

end module   m_poshcheck
        
 subroutine poshcheck(key)
    use m_poshcheck
    use m_flow
    use m_flowgeom
    use m_flowtimes
    use m_partitioninfo
    use m_timer
    use unstruc_display,  only : jaGUI
    
    implicit none

    integer,        intent(out) :: key
   
    double precision, parameter :: SET_VALUE = 0d0
    logical,          parameter :: IS_MIN_WATER_LEVEL_AT_BOTTOM = .true.
    logical,          parameter :: IS_AU_TO_BE_REDUCED = .true.

    integer                     :: reduced_data(2)


    if ( jaGUI == 1 ) then
       call setcol(221) ! white
    end if
   
    call set_water_level_and_hu_for_dry_cells(size(s1), s1, size(hu), hu, &
        SET_VALUE, IS_MIN_WATER_LEVEL_AT_BOTTOM, IS_AU_TO_BE_REDUCED, jamapFlowAnalysis)
 
    key = key_local 
      
    if ( jampi == 1 ) then
       reduced_data = (/ key, nodneg /)

       if ( jatimer == 1 ) call starttimer(IMPIREDUCE)
       call reduce_int_max(2,reduced_data)
       if ( jatimer == 1 ) call stoptimer(IMPIREDUCE)

       key    = reduced_data(1)
       nodneg = reduced_data(2)
    end if

    if (nodneg /= 0 .and. jposhchk /= -1) then
       if (jposhchk == 1 .or. jposhchk == 3 .or. jposhchk == 5 .or. jposhchk == 7) then
          dts = 0.7d0*dts
       end if
       dsetb  = dsetb + 1                               ! total nr of setbacks
       s1     = s0
       vol1   = vol0
       if (dts < dtmin) then
          s1 = max(s1,bl)                              ! above bottom
          call okay(0)
          key = 1                                      ! for easier mouse interrupt
       end if
    end if
 
    if ( is_hu_changed ) then
       call fill_onlyWetLinks()
    end if
 
 end subroutine poshcheck
 
 !> set_water_level_and_hu_for_dry_cells
 subroutine set_water_level_and_hu_for_dry_cells(size_water_level, water_level, size_upwind_waterheight, upwind_waterheight, &
     set_value, is_min_water_level_at_bottom, is_au_to_be_reduced, ja_map_flow_analysis)
    use m_poshcheck
    use m_flow,           only : au, u1, jposhchk, negativedepths, nodneg, numnodneg, testdryflood, epshu, eps6
    use m_flowgeom
    use unstruc_display,  only : jaGUI

    implicit none

    integer,          intent(in)    :: size_water_level                             !< size_water_level
    integer,          intent(in)    :: size_upwind_waterheight                      !< size_upwind_waterheight
    double precision, intent(inout) :: water_level(size_water_level)                !< water_level
    double precision, intent(inout) :: upwind_waterheight(size_upwind_waterheight)  !< upwind_waterheight
    double precision, intent(in)    :: set_value                                    !< set_value
    logical,          intent(in)    :: is_min_water_level_at_bottom                 !< is_min_water_level_at_bottom
    logical,          intent(in)    :: is_au_to_be_reduced                          !< is_au_to_be_reduced
    integer,          intent(in)    :: ja_map_flow_analysis                         !< ja_map_flow_analysis
    
    integer                         :: node, link, link_index
    double precision                :: threshold
    double precision, parameter     :: WATER_LEVEL_TOLERANCE = 1d-10
    double precision, parameter     :: DELFT3D_MIN = 1d-9
    double precision, parameter     :: DELFT3D_MAX = 1d-3
    double precision, parameter     :: REDUCTION_FACTOR = 0.2d0
    integer,          parameter     :: FLAG_REDO_TIMESTEP = 2

    Nodneg    = 0
    key_local = 0
    is_hu_changed = .false.
    
    if (jposhchk == 0) return

    if ( testdryflood == 1 ) then
    ! The algoritm of Delft3D-FLOW is applied to prevent very thin layers 
       threshold = max(DELFT3D_MIN, min(epshu, DELFT3D_MAX))
    else
       threshold = 0d-0
    end if
 
    do node = 1, ndxi
       if (abs(kfs(node)) /= 0) then ! Also check ghost nodes for posh/setbacks
          if ( water_level(node) < bl(node) + threshold ) then
              if ( water_level(node) < bl(node) + threshold - WATER_LEVEL_TOLERANCE ) then
                 nodneg    = node
                 numnodneg = numnodneg + 1
                 if ( jaGUI == 1 ) then
                    call rcirc( xz(node), yz(node) )
                 end if
                 select case(jposhchk)
                    case(-1)                  ! only detect dry cells and return (for Nested Newton restart)
                       key_local = FLAG_REDO_TIMESTEP
                    case(1)                   ! only timestep reduction
                       key_local = FLAG_REDO_TIMESTEP
                       exit
                    case(2, 3)                ! set dry all attached links
                       key_local = FLAG_REDO_TIMESTEP
                       do link_index = 1, nd(node)%lnx
                         link        = iabs(nd(node)%ln(link_index))
                         upwind_waterheight(link) = set_value
                         is_hu_changed = .true.
                       end do
                    case(4, 5)                ! reduce links au
                       do link_index = 1, nd(node)%lnx
                          link       = iabs(nd(node)%ln(link_index))
                          if (upwind_waterheight(link) > 0) then
                             if (REDUCTION_FACTOR*au(link) < eps6) then
                                upwind_waterheight(link) = set_value
                                key_local     = FLAG_REDO_TIMESTEP
                                is_hu_changed = .true.
                             end if
                             if ( is_au_to_be_reduced ) then
                                 au(link) = REDUCTION_FACTOR*au(link)
                             end if
                          end if
                       end do
                    case(6, 7)                 ! only set dry outflowing links
                       do link_index = 1, nd(node)%lnx
                          link       = iabs(nd(node)%ln(link_index))
                          if (nd(node)%ln(link_index) < 0 .and. u1(link) > 0 .or. &
                              nd(node)%ln(link_index) > 0 .and. u1(link) < 0 ) then
                             upwind_waterheight(link) = set_value
                             key_local     = FLAG_REDO_TIMESTEP
                             is_hu_changed = .true.
                          end if
                       end do
                 end select
              end if

              if (ja_map_flow_analysis > 0) then
                 negativeDepths(node) = negativeDepths(node) + 1
              end if

              if ( is_min_water_level_at_bottom ) then
                 water_level(node) = bl(node)
              else
                 water_level(node) = set_value
              end if
           end if
       end if

    end do

 end subroutine set_water_level_and_hu_for_dry_cells
 
 
