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

!> compute the height of the subintervals of grid layers on a cross spline, w.r.t. a center spline
subroutine comp_subheights(is, Lorient, num, xs, ys, ncs, ics, t, cosphi, nsubL, nsubR, hL, hR)
   use m_splines
   use m_spline2curvi

   implicit none

   integer,                              intent(in)    :: is           !< center spline number
   logical,                              intent(in)    :: Lorient      !< orientation of cross spline
   integer,                              intent(in)    :: num          !< number of control points in cross spline (should be 2)
   double precision, dimension(num),     intent(in)    :: xs, ys       !< coordinates of cross spline control points
   integer,                              intent(in)    :: ncs          !< number of splines crossing the cross spline
   integer,          dimension(ncs),     intent(in)    :: ics          !< spline numbers of splines that cross the cross spline
   double precision, dimension(ncs),     intent(in)    :: t            !< cross spline coordinates of the crossings
   double precision, dimension(ncs),     intent(in)    :: cosphi       !< cosine of crossing angle

   integer,                              intent(out)   :: nsubL, nsubR !< number of subintervals left and right of the center spline
   double precision, dimension(Nsubmax), intent(inout) :: hL, hR       !< subinterval heights left and right of center spline

   integer                                           :: k, kk, kL, kR, ks, kkL, kkR, Ndum

   double precision, dimension(Nsubmax)              :: hdum

   double precision, external                        :: splinelength_int

   hL = 0d0
   hR = 0d0

!  for this cross spline, find the left and right neighboring splines w.r.t. the center spline
   kL = 0
   kR = 0
   do k=1,ncs
      ks = ics(k)
      if ( ks.eq.is ) then
         if ( k.gt.1 )   kL = ics(k-1)
         if ( k.lt.ncs ) kR = ics(k+1)
         exit
      end if
   end do

!  compute the heights of the subintervals
   NsubR = 0
   kkR = k
   do kk=k,ncs-1
      if ( NsubR.ge.Nsubmax-1 ) exit
      if ( splineprops(ics(kk+1))%id.ne.-is ) cycle
      kkL = kkR
      kkR = kk+1
      NsubR = NsubR+1
      hR(NsubR) = splinelength_int(num, xs, ys, t(kkL), t(kkR))

!     begin test
!         hR(NsubR) = cosphi(kk)*hR(NsubR)
!     end test


   end do
   NsubR = NsubR+1
   hR(NsubR) = splinelength_int(num, xs, ys, t(kkR), dble(num-1))

!     begin test
!         hR(NsubR) = cosphi(ncs)*hR(NsubR)
!     end test

   if ( NsubR.lt.Nsubmax ) hR(NsubR+1:Nsubmax) = 0d0

   NsubL = 0
   kkL = k
   do kk=k,2,-1
      if ( NsubL.ge.Nsubmax-1 ) exit
      if ( splineprops(ics(kk-1))%id.ne.-is ) cycle
      kkR = kkL
      kkL = kk-1
      NsubL = NsubL+1
      hL(NsubL) = splinelength_int(num, xs, ys, t(kkL), t(kkR))

!     begin test
!         hL(NsubL) = cosphi(kk)*hL(NsubL)
!     end test

   end do
   NsubL = NsubL+1
   hL(NsubL) = splinelength_int(num, xs, ys, 0d0, t(kkL))

!     begin test
!         hL(NsubL) = cosphi(1)*hL(NsubL)
!     end test

   if ( NsubL.lt.Nsubmax ) hL(NsubL+1:Nsubmax) = 0d0

!  check orientation
   if ( .not.Lorient ) then
      Ndum = NsubL
      NsubL = NsubR
      NsubR = Ndum

      hdum  = hL
      hL    = hR
      hR    = hdum
   end if

   return
end subroutine comp_subheights
