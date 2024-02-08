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

! =================================================================================================
! =================================================================================================
   subroutine setstruclink()

      use m_flow
      implicit none
      integer          :: i, n, L, Lf, La
      !
      ! === Gates (old)
      !
      do n = 1,ngatesg
         do L = L1gatesg(n), L2gatesg(n)
            Lf = kgate(3,L)
            La = abs( Lf )
            struclink(La) = 1
         enddo
      enddo
      !
      ! === Gates (new)
      !
      do n = 1,ngategen
         i = gate2cgen(n)
         do L = L1cgensg(i), L2cgensg(i)
            Lf = kcgen(3,L)
            La = abs( Lf )
            struclink(La) = 1
         enddo
      enddo
      !
      ! === General structures (old)
      !
      do n = 1,ncgensg
         i = n
         do L = L1cgensg(i),L2cgensg(i)
            Lf = kcgen(3,L)
            La = abs( Lf )
            struclink(La) = 1
         enddo
      enddo
      !
      ! === General structures (new)
      !
      do n = 1,ngenstru
         i = genstru2cgen(n)
         do L = L1cgensg(i),L2cgensg(i)
            Lf = kcgen(3,L)
            La = abs( Lf )
            struclink(La) = 1
         enddo
      enddo
      !
      ! === Weirs
      !
      do n = 1,nweirgen
         i = weir2cgen(n)
         do L = L1cgensg(i),L2cgensg(i)
            Lf = kcgen(3,L)
            La = abs( Lf )
            struclink(La) = 1
         enddo
      enddo

   end subroutine setstruclink
