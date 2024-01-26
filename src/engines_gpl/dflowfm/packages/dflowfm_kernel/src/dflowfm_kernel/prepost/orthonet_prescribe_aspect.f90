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

!>  prescribe link-based aspect ratios in curvi-grids for mesh refinement (obsolete)
subroutine orthonet_prescribe_aspect(smp_mu, idir, aspect, ic, jc)
   use m_netw
   use m_sferic
   use m_missing
   use geometry_module, only: dbdistance, dcosphi

   IMPLICIT NONE

   double precision, dimension(numk)  :: smp_mu    !< mesh attractor

   integer                            :: idir      !< mesh adaptation direction

   double precision, dimension(numL)  :: aspect    !< aspect ratio at the links

   integer, dimension(numk)           :: ic, jc    !< start indices on curvi-grid

   double precision, dimension(2)     :: orient    ! prescribed orientation


!   integer, parameter                 :: IMISS = -999999

   double precision                   :: x1,y1, x2,y2, x3,y3
   double precision                   :: R01, cosphi, cos2phi, sin2phi
   double precision                   :: A, A2, fA2, mu

   integer                            :: L, k1, k2, imin, jmin, mc, nc

!   orient = (/ 0d0, 1d0 /)
!   orient = orient / sqrt(sum(orient**2))
!
!   imin  = minval(ic, ic.ne.IMISS)
!   jmin  = minval(jc, jc.ne.IMISS)
!
!   ic    = ic   - imin + 1
!   jc    = jc   - jmin + 1
!
!   mc = maxval(ic)
!   nc = maxval(jc)

   do L=1,numL
!     compute the angle of link L with the prescribed orientation
      k1 = kn(1,L)
      k2 = kn(2,L)

      if ( kc(k1).ne.1 .or. kc(k2).ne.1 ) cycle
!     determine orientation based on node indices
      cosphi = DMISS
      if ( abs(ic(k2)-ic(k1)).eq.1 ) then
         cosphi = 1d0
      else if ( abs(jc(k2)-jc(k1)).eq.1 ) then
         cosphi = 0d0
      end if

      if ( idir.eq.1 ) cosphi = 1d0 - cosphi

      if ( cosphi.ne.DMISS .and. cosphi.ne.1d0-DMISS) then
         if ( kc(k2).ne.1 ) smp_mu(k2) = smp_mu(k1)
         if ( kc(k1).ne.1 ) smp_mu(k1) = smp_mu(k2)

         mu = 0.5d0*(smp_mu(k1) + smp_mu(k2))

         A   = mu
         A2  = A**2
         fA2 = 1d0/A2

         cos2phi = cosphi**2
         sin2phi = 1d0 - cos2phi
         aspect(L) = aspect(L) * sqrt( (cos2phi + fA2 * sin2phi) / (cos2phi + A2 * sin2phi) ) * A
      end if
   end do

end subroutine orthonet_prescribe_aspect
