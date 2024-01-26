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

  SUBROUTINE VIEWCYCLE(KEY)
  implicit none
  double precision :: deltx
  double precision :: delty
  double precision :: deltz
  double precision :: dscr
  integer :: jav
  integer :: jview
  double precision :: wpqr
  double precision :: xyz
  double precision :: zfac
  double precision :: zupw
  integer :: KEY
  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
  COMMON /PERSPX/ WPQR,DELTX,DELTY,DELTZ,ZFAC,DSCR,ZUPW
  JVIEW = JVIEW + 1
  IF (JVIEW .GT. JAV) JVIEW = 1
  KEY   = 3
  RETURN
  END SUBROUTINE VIEWCYCLE
