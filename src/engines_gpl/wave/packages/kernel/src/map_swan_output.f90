subroutine map_swan_output (sof,fof,gm, fg)
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
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use swan_flow_grid_maps
   !
   implicit none
!
! Global variables
!
   type (grid)               :: fg
   type (output_fields)      :: sof
   type (output_fields)      :: fof
   type (grid_map)           :: gm
!
! Local variables
!
   real :: pi
!
!! executable statements -------------------------------------------------------
!
   pi=4.*atan(1.)
   !
   ! Interpolate from swan to flow grid
   !
   call grmap (sof%hs           ,sof%npts ,fof%hs            , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%period       ,sof%npts ,fof%period        , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%fx           ,sof%npts ,fof%fx            , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%fy           ,sof%npts ,fof%fy            , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%dirc         ,sof%npts ,fof%dirc          , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%dirs         ,sof%npts ,fof%dirs          , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%mx           ,sof%npts ,fof%mx            , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%my           ,sof%npts ,fof%my            , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%dissip(:,:,1),sof%npts ,fof%dissip(:,:,1) , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%dissip(:,:,2),sof%npts ,fof%dissip(:,:,2) , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%dissip(:,:,3),sof%npts ,fof%dissip(:,:,3) , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%dissip(:,:,4),sof%npts ,fof%dissip(:,:,4) , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%depth        ,sof%npts ,fof%depth         , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%dhsign       ,sof%npts ,fof%dhsign        , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%drtm01       ,sof%npts ,fof%drtm01        , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%tps          ,sof%npts ,fof%tps           , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%tm02         ,sof%npts ,fof%tm02          , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%tmm10        ,sof%npts ,fof%tmm10         , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%setup        ,sof%npts ,fof%setup         , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%ubot         ,sof%npts ,fof%ubot          , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   call grmap (sof%wlen         ,sof%npts ,fof%wlen          , fof%npts, gm%ref_table, gm%weight_table, 4, 0)
   !
   call fxfydr(fof%dirc         ,fof%dirs, fof%dir           , fof%npts, pi               )
end subroutine map_swan_output
