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

module m_xbeach_avgoutput

   double precision, allocatable      :: H_mean(:), H_var(:), H_min(:), H_max(:), H_varcross(:), H_varsquare(:)                                !< Sign wave height
   double precision, allocatable      :: urms_mean(:), urms_var(:), urms_max(:), urms_min(:), urms_varcross(:), urms_varsquare(:)                     !< orbital velocity
   double precision, allocatable      :: ust_mean(:), ust_var(:), ust_max(:), ust_min(:), ust_varcross(:), ust_varsquare(:)                         !< stokes drift
   double precision, allocatable      :: vst_mean(:), vst_var(:), vst_max(:), vst_min(:), vst_varcross(:), vst_varsquare(:)                         !<
   double precision, allocatable      :: Fx_mean(:),Fx_var(:), Fx_min(:), Fx_max(:), Fx_varcross(:), Fx_varsquare(:)                             !< x-comp wave forcing
   double precision, allocatable      :: Fy_mean(:),Fy_var(:), Fy_min(:), Fy_max(:), Fy_varcross(:), Fy_varsquare(:)                              !< y-comp wave forcing 
   double precision, allocatable      :: E_mean(:),E_var(:), E_min(:), E_max(:), E_varcross(:), E_varsquare(:)                                  !< Bulk wave energy
   double precision, allocatable      :: R_mean(:),R_var(:), R_min(:), R_max(:), R_varcross(:), R_varsquare(:)                                  !< Bulk roller energy
   double precision, allocatable      :: D_mean(:),D_var(:), D_min(:), D_max(:), D_varcross(:), D_varsquare(:)                                  !< Bulk wave dissipation
   double precision, allocatable      :: DR_mean(:),DR_var(:), DR_min(:), DR_max(:), DR_varcross(:), DR_varsquare(:)                              !< Bulk roller dissipation
   double precision, allocatable      :: s1_mean(:),s1_var(:), s1_min(:), s1_max (:), s1_varcross(:), s1_varsquare(:)                             !< Water level
   double precision, allocatable      :: ucx_mean(:),ucx_var(:),ucx_min(:), ucx_max(:), ucx_varcross(:), ucx_varsquare(:)                              !< velocity
   double precision, allocatable      :: ucy_mean(:),ucy_var(:),ucy_min(:), ucy_max(:), ucy_varcross(:), ucy_varsquare(:)                              !< velocity
   double precision, allocatable      :: thetamean_mean(:),thetamean_var(:), thetamean_min(:), thetamean_max(:), &                                 !< wave direction
                                         thetamean_varcross(:), thetamean_varsquare(:), &
                                         thetamean_sin(:), thetamean_cos(:)
   double precision, allocatable      :: cwav_mean(:),cwav_var(:), cwav_min(:), cwav_max(:), cwav_varcross(:), cwav_varsquare(:)                    !< phase velocity
   double precision, allocatable      :: cgwav_mean(:),cgwav_var(:), cgwav_min(:), cgwav_max(:), cgwav_varcross(:), cgwav_varsquare(:)              !< group velocity
   double precision, allocatable      :: sigmwav_mean(:),sigmwav_var(:), sigmwav_min(:), sigmwav_max(:), sigmwav_varcross(:), sigmwav_varsquare(:)  !< relative freq

   double precision, allocatable      :: xbdsdx(:), xbdsdy(:)                                               !< water level slope
   double precision, allocatable      :: ududx(:),udvdx(:),vdudy(:),vdvdy(:)                                !< advection terms velocity
   double precision, allocatable      :: visx(:),visy(:)                                                    !< viscosity

   double precision                   :: multcum

   integer                            :: jaavgwriteall                      !< flags to write variables to _wav.nc files
   integer                            :: jaavgwriteH
   integer                            :: jaavgwriteE
   integer                            :: jaavgwriteR
   integer                            :: jaavgwriteD
   integer                            :: jaavgwriteCel
   integer                            :: jaavgwriteDir
   integer                            :: jaavgwriteU
   integer                            :: jaavgwriteF
   integer                            :: jaavgwriteUrms
   integer                            :: jaavgwriteS
   integer                            :: jaavgwriteSigm

   contains

   subroutine default_xbeach_avgoutput()
      implicit none
      jaavgwriteall         = 0
      jaavgwriteH           = 0
      jaavgwriteE           = 0
      jaavgwriteR           = 0
      jaavgwriteD           = 0
      jaavgwriteCel         = 0
      jaavgwriteDir         = 0
      jaavgwriteU           = 0
      jaavgwriteF           = 0
      jaavgwriteUrms        = 0
      jaavgwriteS           = 0
      jaavgwriteSigm        = 0

   end subroutine

end module m_xbeach_avgoutput
