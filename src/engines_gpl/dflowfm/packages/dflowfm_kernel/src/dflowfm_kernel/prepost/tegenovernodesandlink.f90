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

  subroutine tegenovernodesandlink(np,LL,k1a,k2a,La)
  use m_netw
  implicit none
  integer :: np,LL,k1a,k2a,La

  integer :: lk
  integer :: n
  integer :: na

  do n  = 1,netcell(np)%n
     Lk = netcell(np)%lin(n)
     if (Lk == LL) then
        exit
     endif
  enddo

  if (n == 1) then
     na = 3
  else if (n == 2) then
     na = 4
  else if (n == 3) then
     na = 1
  else if (n == 4) then
     na = 2
  endif

  La  = netcell(np)%lin(na)
  k1a = kn(1, La)
  k2a = kn(2, La)

  end subroutine tegenovernodesandlink
