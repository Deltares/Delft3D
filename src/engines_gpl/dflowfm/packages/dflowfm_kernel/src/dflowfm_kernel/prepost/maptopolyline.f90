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

      !> Maps a list of distances to a list of points.
      !! The points are placed onto a polyline at the distances measured along
      !! the consecutive polyline segments.
      SUBROUTINE mapToPolyline(XHO, YHO, DPL, NO, XH, YH, DPLA, NPL) ! HAAL HUIDIGE PUNTEN OP
      implicit none
      DOUBLE PRECISION, intent(in)  :: XHO(NO), YHO(NO) !< Polyline points.
      double precision, intent(in)  :: DPL(NO)          !< Accumulated segment sizes along polyline.
      integer,          intent(in)  :: NO               !< Nr. of polyline points.
      double precision, intent(out) :: XH(NPL), YH(NPL) !< Output points interpolated on polyline.
      double precision, intent(in)  :: DPLA(NPL)        !< Desired distances for all points.
      integer,          intent(in)  :: npl              !< Nr. of points to be interpolated.

      integer :: ja
      integer :: n

      DO N = 1, NPL
         CALL interpolateOnPolyline(XHO,YHO,YHO,DPL,NO,XH(N),YH(N),YH(N),DPLA(N),JA)
      ENDDO

      END SUBROUTINE mapToPolyline
