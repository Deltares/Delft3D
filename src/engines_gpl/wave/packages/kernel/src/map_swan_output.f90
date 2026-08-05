subroutine map_swan_output (sof,fof,gm, fg)
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
   interface
      subroutine grmap_esmf(i1, f1, n1, f2, mmax, nmax, f2s, f2g)
         use swan_flow_grid_maps
         integer                   , intent(in)  :: i1
         integer                   , intent(in)  :: n1
         integer                   , intent(in)  :: mmax
         integer                   , intent(in)  :: nmax
         real   , dimension(n1)    , intent(in)  :: f1
         real   , dimension(mmax,nmax)           :: f2
         type(grid_map)            , intent(in)  :: f2s
         type(grid)                              :: f2g
      end subroutine grmap_esmf
   end interface
!
!! executable statements -------------------------------------------------------
!
   pi=4.*atan(1.)
   !
   ! Interpolate from swan to flow grid
   !
   if (gm%ext_mapper) then
      call grmap_esmf(gm%provider_index, sof%hs           , sof%npts, fof%hs           , fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%period       , sof%npts, fof%period       , fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%fx           , sof%npts, fof%fx           , fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%fy           , sof%npts, fof%fy           , fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%dirc         , sof%npts, fof%dirc         , fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%dirs         , sof%npts, fof%dirs         , fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%mx           , sof%npts, fof%mx           , fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%my           , sof%npts, fof%my           , fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%dissip(:,:,1), sof%npts, fof%dissip(:,:,1), fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%dissip(:,:,2), sof%npts, fof%dissip(:,:,2), fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%dissip(:,:,3), sof%npts, fof%dissip(:,:,3), fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%dissip(:,:,4), sof%npts, fof%dissip(:,:,4), fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%depth        , sof%npts, fof%depth        , fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%dhsign       , sof%npts, fof%dhsign       , fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%drtm01       , sof%npts, fof%drtm01       , fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%tps          , sof%npts, fof%tps          , fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%tm02         , sof%npts, fof%tm02         , fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%tmm10        , sof%npts, fof%tmm10        , fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%setup        , sof%npts, fof%setup        , fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%ubot         , sof%npts, fof%ubot         , fof%mmax, fof%nmax, gm, fg)
      call grmap_esmf(gm%provider_index, sof%wlen         , sof%npts, fof%wlen         , fof%mmax, fof%nmax, gm, fg)
   else
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
   endif
   !
   call fxfydr(fof%dirc         ,fof%dirs, fof%dir           , fof%npts, pi               )
end subroutine map_swan_output
