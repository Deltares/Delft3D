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

     SUBROUTINE nPLOTPLUSMIN(IPM)
     USE M_FLOW
     use M_flowgeom
     implicit none
     integer :: IPM, NRLAY


      IF (IPM == 1) THEN
!         nPLOT = MIN(nPLOT+1,ndx)
         nplot = nplot+1
         if ( nplot.gt.Ndx ) nplot = nplot - Ndx
      ELSE if (ipm == -1) then
!         nPLOT = MAX(nPLOT-1,1)
         nplot = nplot-1
         if ( nplot.lt.1 ) nplot = nplot + Ndx
      else
         nplot = ipm
      ENDIF
      if (kmx > 0) then
         NRLAY = KTOP(NPLOT) - KBOT(NPLOT) + 1
         KPLOT = MAX(1, MIN(KPLOT, NRLAY) )
      endif
      CALL TEXTFLOW()
      END SUBROUTINE nPLOTPLUSMIN
