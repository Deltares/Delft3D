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

      DOUBLE PRECISION FUNCTION DLENGTH(K1,K2)
      use m_netw
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none
      integer :: K1, K2
      double precision :: XD,YD,ZD
      IF (NETFLOW == 1) THEN
         XD      = XK(K2) - XK(K1)
         YD      = YK(K2) - YK(K1)
         ZD      = ZK(K2) - ZK(K1)
         DLENGTH = SQRT(XD*XD + YD*YD + ZD*ZD)
      ELSE ! FLOW
         DLENGTH = DBDISTANCE(XK(K1), YK(K1), XK(K2), YK(K2), jsferic, jasfer3D, dmiss)
      ENDIF
      RETURN
      END FUNCTION DLENGTH
