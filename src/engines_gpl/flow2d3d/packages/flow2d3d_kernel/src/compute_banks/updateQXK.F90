  SUBROUTINE updateQXK(hu        ,guu         ,aguu       ,thick        ,u1         ,qxk        ,nmlb       ,nmub       ,nmmax       ,kmax)
!
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  Ndryact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: My_intersec.f90 
!  $HeadURL:
!!--description-----------------------------------------------------------------
!
!   Function: Force exact friction at u point
!
!
!   Author: Alberto Canestrelli
!
!--declarations----------------------------------------------------------------
!
  use precision
  use mathconsts
!
  implicit none
!
! global variables
!
    real(fp), dimension(nmlb:nmub,kmax)                            , intent(out)   :: qxk
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: hu
    real(fp), dimension(nmlb:nmub,kmax)                            , intent(in)    :: u1
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: guu
    real(fp), dimension(nmlb:nmub)                                 , intent(in)    :: aguu
    real(fp), dimension(kmax)                                      , intent(in)    :: thick
    integer                                                        , intent(in)    :: nmlb
    integer                                                        , intent(in)    :: nmub
    integer                                                        , intent(in)    :: nmmax
    integer                                                        , intent(in)    :: kmax
!
!
! local variables
!
    integer                    :: nm,k
!
   !
    do nm = 1, nmmax
       do k = 1, kmax
          qxk(nm, k) = guu(nm)*hu(nm)*thick(k)*u1(nm, k)*aguu(nm)
          !qyk(nm, k) = gvv(nm)*hv(nm)*thick(k)*v1(nm, k)
       enddo
    enddo
!
    RETURN
end subroutine updateQXK
