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

      subroutine checkdislin()
      use m_polygon
      use m_sferic
      use geometry_module, only: dlinedis
      use m_missing, only: dmiss

      implicit none
      integer :: ja
      integer :: jashow
      integer :: jmouse
      double precision :: xa
      double precision :: xlc
      double precision :: ya
      double precision :: ylc
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW
      double precision :: dis, xn, yn

      if (npl >= 2) then
         call DLINEDIS(xlc, ylc ,Xpl(1),ypl(1), xpl(2), ypl(2) ,JA,DIS,XN,YN,jsferic, jasfer3D, dmiss)
      endif

      call DLINEDIS(1d0,0d0,0d0,0d0,1d0,1d0, JA, DIS,XN,YN,jsferic, jasfer3D, dmiss)

      dis = 0.5d0*sqrt(2d0)

      call DLINEDIS(1d0,0.5d0,0d0,0d0,1d0,1d0, JA, DIS,XN,YN,jsferic, jasfer3D, dmiss)

      dis = 0.25d0*sqrt(2d0)

      jsferic = 1
      call DLINEDIS(4d0,60d0,3d0,60d0,4d0,61d0, JA, DIS,XN,YN,jsferic, jasfer3D, dmiss)

      call DLINEDIS(4d0,60.8d0,3d0,60d0,4d0,61d0, JA, DIS,XN,YN,jsferic, jasfer3D, dmiss)

      call rcirc( xn, yn )
      end subroutine checkdislin
