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

 module m_julday
 
 contains
 integer function julday(mm,id,iyyy)
 implicit none
 integer :: igreg
 integer :: mm, id, iyyy
 integer :: jy, jm, ja
 parameter (igreg=15+31*(10+12*1582))
 !     if (iyyy == 0) pause 'there is no year zero.'
 if (iyyy < 0) iyyy=iyyy+1
 if (mm > 2) then
   jy=iyyy
   jm=mm+1
 else
   jy=iyyy-1
   jm=mm+13
 endif
 julday=int(365.25*jy)+int(30.6001*jm)+id+1720995
 if (id+31*(mm+12*iyyy) >= igreg) then
   ja=int(0.01*jy)
   julday=julday+2-ja+int(0.25*ja)
 endif
 return
 end function julday
 end module m_julday
