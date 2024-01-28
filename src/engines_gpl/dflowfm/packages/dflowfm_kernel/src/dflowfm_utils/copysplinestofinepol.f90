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

!> copy spline to resampled polygon
subroutine copySplinesToFinePol(numk)
   USE M_SPLINES
   use m_polygon
   use m_missing

   implicit none

    integer, intent(in) :: numk  !< resample factor

    integer :: i, k, m, numpi, Numnew, ierror
    double precision :: tn, xk, yk, xh2(500), yh2(500)

!    NUMK  = 11
    do m = 1,mcs
        CALL NUMP(m,NUMPI)

        IF (NUMPI .GT. 1) THEN
            Numnew = 1+(NUMPI-1)*numk
            if ( NPL.gt.0 .and. xpl(max(NPL,1)).ne.DMISS ) then
               call increasepol(Numnew+2, 1)
               NPL = NPL+1
               xpl(NPL) = DMISS
            else
               call increasepol(Numnew+1,1)
            end if

            do
               call sample_spline(NUMPI, xsp(m,1:NUMPI), ysp(m,1:NUMPI), numk-1, Numnew, xpl(NPL+1:NPL+Numnew), ypl(NPL+1:NPL+Numnew), ierror)
               if ( ierror.eq.2 ) then
                  call increasepol(Numnew+1,1)
               else
                  exit
               end if
            end do
            NPL = NPL + Numnew
        ENDIF

!       add DMISS
        NPL = NPL+1
        xpl(NPL) = DMISS
        ypl(NPL) = DMISS
        zpl(NPL) = DMISS
    enddo
end subroutine copySplinesToFinePol
