subroutine magdir_to_uv(alfa, grdang, &
                      & mag , dir   , u, v)
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
!  $Id: magdir_to_uv.f90 6242 2016-07-01 17:07:16Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/general/magdir_to_uv.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use mathconsts
    !
    implicit none
!
! Global parameters
!
    real(fp)   , intent(in)  :: alfa   ! Cell orientation, see declaration of alfas in esm_alloc_real.f90
    real(fp)   , intent(in)  :: grdang ! Grid orientation, relative to the north, positive clockwise
    real(fp)   , intent(in)  :: mag    ! Magnitude of vector
    real(fp)   , intent(in)  :: dir    ! Direction of vector, relative to the north, positive clockwise
    real(fp)   , intent(out) :: u      ! U component of vector in ksi direction on grid
    real(fp)   , intent(out) :: v      ! V component of vector in eta direction on grid
!
! Local parameters
!
    real(fp) :: angle
    real(fp) :: workx
    real(fp) :: worky
!
!! executable statements -------------------------------------------------------
!
       ! direction is given relative to the north, positive
       ! clockwise the velocity is coming from that direction.
       ! angle to east-axis
       !   alpha = 90-uvdir
       ! velocity streams to direction
       !   beta  = 90-uvdir+180 = 270-uvdir
       ! velocity adjusted to grdang
       !   gamma = 270-uvdir+grdang
       !
       angle =  (90.0_fp - dir + grdang)*degrad
       workx =  mag * cos(angle)
       worky =  mag * sin(angle)
       u     =  workx*cos(alfa*degrad) + worky*sin(alfa*degrad)
       v     = -workx*sin(alfa*degrad) + worky*cos(alfa*degrad)
end subroutine magdir_to_uv
