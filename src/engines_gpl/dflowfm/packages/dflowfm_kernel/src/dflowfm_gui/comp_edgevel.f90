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

!> compute edge grow velocities, grow factors and number of grid layers in the subintervals
subroutine comp_edgevel(mc, edgevel, dgrow1, nfac1, ierror)
   use m_splines
   use m_gridsettings
   use m_spline2curvi
   use m_alloc
   use m_missing

   implicit none

   integer,                                                intent(in)  :: mc       !< number of grid points
   double precision,              dimension(mc-1),         intent(out) :: edgevel  !< edge-based grid grow velocity, first layer only
   double precision,              dimension(Nsubmax,mc-1), intent(out) :: dgrow1   !< edge-based grid growth factor, for each subinterval of grid layers
   integer,                       dimension(Nsubmax,mc-1), intent(out) :: nfac1    !< edge-based number of grid layers,  for each subinterval of grid layers
   integer,                                                intent(out) :: ierror   !< 0: no error, 1: error

   double precision, allocatable :: eheight(:,:)  ! edge-based grid height, for each subinterval of grid layers


   double precision                                              :: growfac, hmax, h_h0_maxL, h_h0_maxR

   integer                                                       :: i, is, igL, igR, j, js, mfacmax, nfacmax, ncs, numtruecross
   integer                                                       :: Ndum, NuniL, NuniR, NexpL, NexpR, NsubL, NsubR, ja
   integer                                                       :: iother, iter

   integer,          external                                    :: comp_nfac
   double precision, external                                    :: comp_dgrow

   ierror = 1

   mfacmax = mfac
   nfacmax = nfac

   edgevel = DMISS
   dgrow1  = 1d0
   nfac1(1,:) = 1
   nfac1(2:Nsubmax,:) = 0
   allocate(eheight(Nsubmax,mc-1))

   call comp_gridheights(mc, eheight, ierror)

!  compute edge velocities and number of gridlayers
   do is=1,mcs
      mfac = splineprops(is)%mfac
!      if ( mfac.lt.1 ) cycle
      if ( splineprops(is)%id .ne. 0 ) cycle ! center splines only
!
      igL  = splineprops(is)%iL
      igR  = splineprops(is)%iR
      ncs  = splineprops(is)%ncs

!     compute the number of cells perpendicalur to the center spline(s)
      dheight0 = daspect*dwidth


      dheight0 = min(maxval(eheight(1,:), MASK=eheight(1,:).ne.DMISS), dheight0)

      NsubL = Nsubmax
      NsubR = Nsubmax
      numtruecross = 0
      do j=1,ncs
         js = splineprops(is)%ics(j)

         if ( splineprops(js)%id.ne.1 ) cycle   ! true cross splines only
         numtruecross = numtruecross+1
         NsubL = min(NsubL, splineprops(is)%NsubL(j))
         NsubR = min(NsubR, splineprops(is)%NsubR(j))
      end do

      if ( numtruecross.eq.0 ) then             ! no true cross splines: exponentially growing grid only
         NsubL = 0
         NsubR = 0
      end if

      do iter = 1,2 ! repeat, so the gridlayer thicknesses on both sides will match

!     Left, uniform part
      if ( (NsubL.gt.1 .and. NsubL.eq.NsubR) .or. NsubL.gt.NsubR ) then
         hmax = maxval(eheight(1,igL:igL+mfac-1))
         NuniL = floor(hmax/dheight0 + 0.99999d0)
!        at maximum nfacUNImax grid layers in uniform part
         NuniL = min(NuniL, nfacUNImax)

         h_h0_maxL = 0d0  ! (h/h0)_max
         do i=igL,igL+mfac-1
            nfac1(1,i) = NuniL
            edgevel(i) = eheight(1,i)/NuniL
            h_h0_maxL = max( h_h0_maxL, eheight(2,i)/edgevel(i) )
         end do
      else
!        only one subinterval: no uniform part
         NuniL = 0
         h_h0_maxL = 0d0  ! (h/h0)_max
         do i=igL,igL+mfac-1
            nfac1(1,i) = NuniL
            edgevel(i) = dheight0

!           compare with other side of spline
            iother = igR+mfac-(i-igL+1)
            if ( edgevel(iother).ne.DMISS ) then
               if ( nfac1(1,iother).eq.0 ) then   ! no uniform part on other side: take max
                  edgevel(i) = max(edgevel(i), edgevel(iother))
               else                               ! uniform part on other side: take that value
                  edgevel(i) = edgevel(iother)
               end if
            end if

            eheight(2:Nsubmax,i) = eheight(1:Nsubmax-1,i)
            h_h0_maxL = max( h_h0_maxL, eheight(2,i)/edgevel(i) )
         end do
      end if


!     Right, uniform part
      if ( (NsubR.gt.1 .and. NsubL.eq.NsubR) .or. NsubR.gt.NsubL ) then
         hmax = maxval(eheight(1,igR:igR+mfac-1))
         NuniR = floor(hmax/dheight0 + 0.99999d0)
!        at maximum nfacmax grid layers in uniform part
         NuniR = min(NuniR, nfacUNImax)

         h_h0_maxR = 0d0  ! (h/h0)_max
         do i=igR,igR+mfac-1
            nfac1(1,i) = NuniR
            edgevel(i) = eheight(1,i)/NuniR
            h_h0_maxR = max( h_h0_maxR, eheight(2,i)/edgevel(i) )
         end do
      else
!        only one subinterval: no uniform part
         NuniR = 0
         h_h0_maxR = 0d0  ! (h/h0)_max
         do i=igR,igR+mfac-1
            nfac1(1,i) = NuniR
            edgevel(i) = dheight0

!           compare with other side of spline
            iother = igL+mfac-(i-igR+1)
            if ( edgevel(iother).ne.DMISS ) then
               if ( nfac1(1,iother).eq.0 ) then   ! no uniform part on other side: take max
                  edgevel(i) = max(edgevel(i), edgevel(iother))
               else                               ! uniform part on other side: take that value
                  edgevel(i) = edgevel(iother)
               end if
            end if

            eheight(2:Nsubmax,i) = eheight(1:Nsubmax-1,i)
            h_h0_maxR = max( h_h0_maxR, eheight(2,i)/edgevel(i) )
         end do
      end if

      end do   ! do iter=1,2

      ja = jaoutside
      if ( (NsubL.eq.0 .and. NsubR.le.1) .or. (NsubR.eq.0 .and. NsubL.le.1) .or. (NsubL.eq.1 .and. NsubR.eq.1) ) then
         ja = 1
      end if

!     Left, exponentially growing part
      if ( ja.eq.1 ) then
         NexpL = min(comp_nfac(h_h0_maxL, dgrow),nfac)
      else
         NexpL = 0
      end if
      nfac1(2,igL:igL+mfac-1) = NexpL

!     Right, exponentially growing part
      if ( ja.eq.1 ) then
         NexpR = min(comp_nfac(h_h0_maxR, dgrow),nfac)
      else
         NexpR = 0
      end if
      nfac1(2,igR:igR+mfac-1) = NexpR
   end do   ! do is = 1,mcs


!  compute local grow factors
   do is=1,mcs
      if ( splineprops(is)%mfac.lt.1 ) cycle

      do i=splineprops(is)%iL,splineprops(is)%iR + splineprops(is)%mfac-1
         if ( xg1(i).eq.DMISS .or. xg1(i+1).eq.DMISS .or. nfac1(2,i).lt.1 ) cycle

         dgrow1(2,i) = comp_dgrow(eheight(2,i), edgevel(i), nfac1(2,i), ierror)
         if ( ierror.eq.1 ) then
            dgrow1(2,i) = 1d0
!            goto 1234
         end if

!        no shrinking grid layers, decrease first exponentially growing grid layer height instead (to 1, i.e. equidistant grid layers)
!         dgrow1(i) = max(dgrow1(i),1d0)

!        compute the first grid layer height
!         if ( abs(dgrow1(i)-1d0).gt.1d-8 ) then
!            growfac = (dgrow1(i)-1d0)/(dgrow1(i)**(nfac1(1,i)-nfac1(2,i))-1d0)
!         else
!            growfac = 1d0/dble(nfac1(1,i)-nfac1(2,i))
!         end if
!         edgevel(i) = (eheight(1,i)-eheight(2,i)) * growfac
      end do
   end do

   ierror = 0

!  error handling
1234 continue

!  restore
   mfac = mfacmax
   nfac = nfacmax ! not necessary, as nfac was not altered, but just to be sure
   deallocate(eheight)

   return
end subroutine comp_edgevel
