function numbersonline_m(rec       )
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer         :: numbersonline_m
    character(*)    :: rec
!
!
! Local variables
!
    integer                        :: i
    integer                        :: istarti
    integer                        :: leeg
    integer                        :: lend
!
!
!! executable statements -------------------------------------------------------
!
    !
    numbersonline_m = 0
    leeg = 1
    lend = len_trim(rec)
    do i = 1, lend
       if (index(rec(i:i), ' ')==0) then
          !           hier staat iets
          if (leeg==1) then
             leeg = 0
             istarti = i
             numbersonline_m = numbersonline_m + 1
          endif
       else
          leeg = 1
       endif
    enddo
end function numbersonline_m
