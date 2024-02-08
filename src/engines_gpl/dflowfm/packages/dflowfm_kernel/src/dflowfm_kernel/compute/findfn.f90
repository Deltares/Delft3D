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

subroutine findfn( cz, zn, fn )

   implicit none
   integer :: i, j
   double precision :: acof, bcof
   double precision, intent(in)  :: zn
   double precision, intent(in)  :: cz
   double precision, intent(out) :: fn
   integer, parameter :: itab=100, jtab=1
   double precision, dimension(0:itab) :: fnarray
   double precision :: znc, czc, dzn

   fnarray = (/ 0.0000000e+00 ,  0.0000000e+00 , -3.2435628e+00 , -4.8713274e+00 , -5.8552221e+00 , -6.4959081e+00 , &
               -6.9255574e+00 , -7.2137312e+00 , -7.4013821e+00 , -7.5145552e+00 , -7.5707797e+00 , &
               -7.5823759e+00 , -7.5583058e+00 , -7.5052724e+00 , -7.4284060e+00 , -7.3317101e+00 , &
               -7.2183609e+00 , -7.0909148e+00 , -6.9514544e+00 , -6.8016956e+00 , -6.6430654e+00 , &
               -6.4767611e+00 , -6.3037949e+00 , -6.1250285e+00 , -5.9412000e+00 , -5.7529454e+00 , &
               -5.5608153e+00 , -5.3652891e+00 , -5.1667855e+00 , -4.9656720e+00 , -4.7622720e+00 , &
               -4.5568716e+00 , -4.3497240e+00 , -4.1410546e+00 , -3.9310641e+00 , -3.7199319e+00 , &
               -3.5078185e+00 , -3.2948682e+00 , -3.0812105e+00 , -2.8669622e+00 , -2.6522285e+00 , &
               -2.4371048e+00 , -2.2216772e+00 , -2.0060236e+00 , -1.7902149e+00 , -1.5743155e+00 , &
               -1.3583837e+00 , -1.1424727e+00 , -9.2663110e-01 , -7.1090286e-01 , -4.9532825e-01 , &
               -2.7994396e-01 , -6.4783449e-02 ,  1.5012272e-01 ,  3.6474668e-01 ,  5.7906297e-01 , &
                7.9304836e-01 ,  1.0066816e+00 ,  1.2199435e+00 ,  1.4328162e+00 ,  1.6452838e+00 , &
                1.8573316e+00 ,  2.0689463e+00 ,  2.2801157e+00 ,  2.4908290e+00 ,  2.7010760e+00 , &
                2.9108478e+00 ,  3.1201362e+00 ,  3.3289339e+00 ,  3.5372344e+00 ,  3.7450317e+00 , &
                3.9523205e+00 ,  4.1590963e+00 ,  4.3653548e+00 ,  4.5710924e+00 ,  4.7763060e+00 , &
                4.9809928e+00 ,  5.1851506e+00 ,  5.3887774e+00 ,  5.5918716e+00 ,  5.7944319e+00 , &
                5.9964574e+00 ,  6.1979473e+00 ,  6.3989011e+00 ,  6.5993187e+00 ,  6.7992001e+00 , &
                6.9985455e+00 ,  7.1973553e+00 ,  7.3956301e+00 ,  7.5933705e+00 ,  7.7905776e+00 , &
                7.9872522e+00 ,  8.1833957e+00 ,  8.3790092e+00 ,  8.5740942e+00 ,  8.7686522e+00 , &
                8.9626847e+00 ,  9.1561934e+00 ,  9.3491800e+00 ,  9.5416465e+00 ,  9.7333452e+00 /)
   dzn = 1d0 / itab

   i    = zn / dzn
   znc  = i  * dzn
   !j    = cz * jtab
   !czc  = j  * jtab
   acof = ( fnarray(i+1) - fnarray(i) ) / dzn
   bcof = fnarray(i)
   fn = acof * ( zn - znc ) + bcof

end subroutine findfn
