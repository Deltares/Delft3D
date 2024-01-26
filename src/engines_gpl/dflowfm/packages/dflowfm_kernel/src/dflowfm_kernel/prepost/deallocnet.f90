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

   SUBROUTINE DEALLOCNET()
   use m_netw
   USE M_FLOWgeom
   implicit none
   integer :: p, numpx

   NUMK = 0; NUML = 0
   IF (SIZE(XK) > 0) THEN
      DEALLOCATE(NOD,XK ,YK , KC ,NMK)
      DEALLOCATE(KN,LC)
   ENDIF
   IF (SIZE(XK0) > 0) THEN
      DEALLOCATE(NOD0,XK0 ,YK0 ,ZK0 , KC0 ,NMK0)
      DEALLOCATE(KN0,LC0)
   ENDIF
   IF ( SIZE(LNN)  > 0) THEN
      DEALLOCATE (LNN, LNE, ln2lne)
   ENDIF
   IF (NUMP > 0) THEN
      numpx = size(netcell)
      do p=1,numpx
        if (allocated(netcell(p)%nod)) then
            deallocate(netcell(p)%nod, netcell(p)%lin)
        end if
      end do
      deallocate (netcell)
      nump = 0
   ENDIF
   END SUBROUTINE DEALLOCNET
