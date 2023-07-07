!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

!> deallocate particle mesh data
subroutine dealloc_partmesh()
   use m_partmesh
   implicit none

   if ( allocated(edge2node ) ) deallocate(edge2node )
   if ( allocated(edge2cell ) ) deallocate(edge2cell )
   if ( allocated(xnode     ) ) deallocate(xnode     )
   if ( allocated(ynode     ) ) deallocate(ynode     )
   if ( allocated(znode     ) ) deallocate(znode     )

   if ( allocated(xzwcell   ) ) deallocate(xzwcell   )
   if ( allocated(yzwcell   ) ) deallocate(yzwcell   )
   if ( allocated(zzwcell   ) ) deallocate(zzwcell   )
   if ( allocated(areacell  ) ) deallocate(areacell  )

   if ( allocated(dnn       ) ) deallocate(dnn       )
   if ( allocated(dnx       ) ) deallocate(dnx       )
   if ( allocated(dny       ) ) deallocate(dny       )
   if ( allocated(dnz       ) ) deallocate(dnz       )
   if ( allocated(w         ) ) deallocate(w         )

   if ( allocated(edge2link ) ) deallocate(edge2link )
!  if ( allocated(nod2cell  ) ) deallocate(nod2cell  )
   if ( allocated(cell2nod  ) ) deallocate(cell2nod  )

   if ( allocated(icell2edge) ) deallocate(icell2edge)
   if ( allocated(jcell2edge) ) deallocate(jcell2edge)


   return
end subroutine dealloc_partmesh
