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

    SUBROUTINE PFILLER(X,Y,N_,NCOL,NCLR)
    use unstruc_opengl
    use m_sferic

    implicit none
    integer :: N_
    integer :: nclr
    integer :: ncol, i, n
    integer :: ncolnow
    integer :: ndraw
    double precision :: X(N_), Y(N_), xx, yy
    COMMON /DRAWTHIS/ ndraw(50)
    COMMON /COLNOW/ NCOLNOW

    integer, parameter :: NMAX = 128
    real xr(NMAX), yr(NMAX)

    CALL SETCOL(NCOL)

!   safety
    N = min(N_, NMAX)

    if (jsfertek == 1) then
       do i = 1,n
          call dproject(x(i), y(i), xx, yy, 1)
          xr(i) = xx ; yr(i) = yy
       enddo
    else
    xr(1:N) = x(1:N)
    yr(1:N) = y(1:N)
    endif

    CALL PFILLERCORE(xr,yr,N)

    IF (.NOT. InOpenGLRendering .AND. (NCLR .NE. NCOL .or. ndraw(10) .ne. 0)) then
        CALL realPolygon(Xr,Yr,N,NCLR)
    ENDIF

    RETURN
   END
