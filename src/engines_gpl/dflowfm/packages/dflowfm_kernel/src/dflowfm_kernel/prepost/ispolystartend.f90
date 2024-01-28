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

    !> Checks whether a polyline point is at the start or end of a polyline.
    !!
    !! Multiple polylines are stored in one large array, separated by dmiss.
    !! To know whether point at index L1 is a start/end point of one of these
    !! polylines, check on a neighbouring dmiss.
    logical function ispolystartend( X, Y, N, MAXPOL, ipoi) result(res)
    use m_missing
    implicit none
        integer,          intent(in) :: MAXPOL !< Length of polyline coordinate arrays.
        double precision, intent(in) :: X(MAXPOL), Y(MAXPOL) !< Entire polyline coordinate arrays
        integer,          intent(in) :: N      !< Index of last filled polyline point (npol<=maxpol)
        integer,          intent(in) :: ipoi   !< Index of polyline point to be checked.

        ! First check invalid input
        if (ipoi <= 0 .or. ipoi > n .or. n > maxpol) then
            res = .false.
            return
        end if

        ! Next, check on trivial end points
        if (ipoi == 1 .or. ipoi == n) then
            res = .true.
            return
        end if

        ! Generic case: somewhere in middle of poly, check on dmiss.
        res = (x(ipoi-1) == dmiss .or. x(ipoi+1) == dmiss)

    end function ispolystartend
