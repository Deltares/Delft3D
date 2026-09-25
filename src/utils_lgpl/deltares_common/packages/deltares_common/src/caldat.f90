!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2026.                                
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

module m_caldat
   implicit none(type, external)

contains

   subroutine caldat(julian, mm, id, iyyy)

   integer :: julian, mm, id, iyyy
   integer :: jalpha, ja, jb, jc, jd, je
   integer, parameter :: IGREG = 2299161

   if (julian >= IGREG) then
      jalpha = int(((julian - 1867216) - 0.25) / 36524.25)
      ja = julian + 1 + jalpha - int(0.25 * jalpha)
   else
      ja = julian
   end if

   jb = ja + 1524
   jc = int(6680. + ((jb - 2439870) - 122.1) / 365.25)
   jd = 365 * jc + int(0.25 * jc)
   je = int((jb - jd) / 30.6001)
   id = jb - jd - int(30.6001 * je)
   mm = je - 1

   if (mm > 12) then
      mm = mm - 12
   end if

   iyyy = jc - 4715

   if (mm > 2) then
      iyyy = iyyy - 1
   end if

   if (iyyy <= 0) then 
      iyyy = iyyy - 1
   end if

   end subroutine caldat
end module m_caldat
