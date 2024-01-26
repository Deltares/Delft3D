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

! determine the grid grow factor for a given total grid height, first grid layer height and number of grid layers
double precision function comp_dgrow(height, dheight0, nfac, ierror)
   use m_missing

   implicit none

   double precision, intent(in)  :: height    !< total grid height
   double precision, intent(in)  :: dheight0  !< first grid layer height
   integer,          intent(in)  :: nfac      !< number of grid layers
   integer,          intent(out) :: ierror    !< error (1) or not (0)

   integer                      :: iter

   double precision             :: fkp1, fk, fkm1, gkp1, gk, gkm1, deltag

   double precision, external   :: comp_h

   integer,          parameter  :: maxiter=1000

   double precision, parameter  :: dtol = 1d-8
   double precision, parameter  :: deps = 1d-2
   double precision, parameter  :: relax = 0.5d0

   ierror = 1

   gk   = 1d0
   fk   = comp_h(gk, dheight0, nfac) - height

   gkp1 = 1d0 + deps
   fkp1 = comp_h(gkp1, dheight0, nfac) - height

   if ( abs(fkp1).gt.dtol .and. abs(fkp1-fk).gt.dtol ) then
      do iter=1,maxiter
         gkm1 = gk
         fkm1 = fk

         gk   = gkp1
         fk   = fkp1

         gkp1 = gk - relax * fk / (fk-fkm1+1d-16) * ( gk - gkm1)
         fkp1 = comp_h(gkp1, dheight0, nfac) - height

!         if ( abs(fkp1).lt.dtol .or. abs(fkp1-fk).lt.dtol ) exit
         if ( abs(fkp1).lt.dtol ) exit
      end do
   end if

   if ( abs(fkp1).gt.dtol ) then
!     no convergence
!      call qnerror('comp_dgrow: no convergence', ' ', ' ')
      comp_dgrow = DMISS
      goto 1234
   else
      comp_dgrow = gkp1
   end if

   ierror = 0

!   error handling
1234 continue

   return
end function comp_dgrow
