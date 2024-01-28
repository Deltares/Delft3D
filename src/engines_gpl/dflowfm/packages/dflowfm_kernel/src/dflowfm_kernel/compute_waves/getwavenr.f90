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

 subroutine getwavenr(h, T, k)
 use m_sferic
 use m_physcoef
 implicit none
 ! get wavenr from waterdepth and period, see d3d doc

 double precision, parameter    :: a1 = 5.060219360721177D-01, a2 = 2.663457535068147D-01,  &
                                   a3 = 1.108728659243231D-01, a4 = 4.197392043833136D-02,  &
                                   a5 = 8.670877524768146D-03, a6 = 4.890806291366061D-03,  &
                                   b1 = 1.727544632667079D-01, b2 = 1.191224998569728D-01,  &
                                   b3 = 4.165097693766726D-02, b4 = 8.674993032204639D-03

 double precision , intent(in)  :: h                    !  Waterheight
 double precision , intent(in)  :: t                    !  Period
 double precision , intent(out) :: k                    !  Approximation of wave lenght


 double precision               :: den                  ! Denominator
 double precision               :: kd                   ! Double value for K
 double precision               :: num                  ! Numerator
 double precision               :: ome2 , rk, fac, rlabda, rlab0, rn    ! Omega

 ome2 = ( (twopi/T)**2 )*h/ag
 num  = 1.0D0 + ome2*(a1 + ome2*(a2 + ome2*(a3 + ome2*(a4 + ome2*(a5 + ome2*a6)))))
 den  = 1.0D0 + ome2*(b1 + ome2*(b2 + ome2*(b3 + ome2*(b4 + ome2*a6))))
 k    = sqrt(ome2*num/den)/ h

 end subroutine getwavenr
