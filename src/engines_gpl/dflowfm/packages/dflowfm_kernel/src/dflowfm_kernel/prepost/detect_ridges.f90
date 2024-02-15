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

!> detect ridges and reduce structured sample set
subroutine detect_ridges(jadeleteHessians)
   use m_samples
   use m_samples_refine
   use m_missing
   use geometry_module, only: dbdistance
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   integer, intent(in) :: jadeleteHessians   !< delete Hessians upon completion (1) or not (0)

   integer             :: i, j, ip, jacancelled
   integer             :: ierror, Nsamplesmooth_bak

   double precision    :: Dh, dum

   double precision, external :: comp_sampleDh

   ierror = 1

!  store settings
   Nsamplesmooth_bak = Nsamplesmooth

!  default value
   Nsamplesmooth = 4

!  check if the sample set is structured and non-empty
   if ( MXSAM*MYSAM.ne.NS .or. NS.eq.0 ) goto 1234

!  compute sample mesh width
   Dh = min(dbdistance(xs(1),ys(1),xs(2),ys(2),jsferic, jasfer3D, dmiss), dbdistance(xs(1),ys(1),xs(1+MXSAM),ys(1+MXSAM),jsferic, jasfer3D, dmiss))

!  store samples
   call savesam()

   call prepare_sampleHessian(ierror)
   if ( ierror.ne.0 ) goto 1234

!  plot ridges
   call plot_ridges(ierror)
!   if ( ierror.ne.0 ) goto 1234

!  remove samples from sample set that are not associated with a ridge
   do i=1,MXSAM
      do j=1,MYSAM
         ip = i+(j-1)*MXSAM

         Dh = comp_sampleDh(i,j)

         if ( abs(zss(5,i,j)).gt.0.5d0*Dh .or. zss(4,i,j).gt.-1d-8 .or. zss(5,i,j).eq.DMISS ) then
            xs(ip) = DMISS
            ys(ip) = DMISS
!            zs(ip) = DMISS
         end if
      end do
   end do

   ierror = 0
1234 continue

!  restore settings
   Nsamplesmooth = Nsamplesmooth_bak

   if ( jadeleteHessians.eq.1 ) then
      call deallocate_sampleHessian()
   end if

   return
end subroutine detect_ridges
