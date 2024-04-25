      subroutine wrwaqflo ( nmax   , mmax   , kmax   , nlb    , nub    , &
     &                      mlb    , mub    , itim   , kfsmin , quwaq  , &
     &                      qvwaq  , qwwaq  , areau  , areav  , noseg  , &
     &                      horsurf, iqaggr , noq    , noq12  , qag    , &
     &                      zmodel , naccum , lunare )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
      use precision
      implicit none
!
!           Global variables
!
      integer( 4),intent(in   ) :: nmax                               !!  Dimension of first index in 2d arrays
      integer( 4),intent(in   ) :: mmax                               !!  Dimension of second index in 2d arrays
      integer( 4),intent(in   ) :: kmax                               !!  number of layers
      integer( 4),intent(in   ) :: nlb                                !!  Lower bound of all n dimensions
      integer( 4),intent(in   ) :: nub                                !!  Upper bound of all n dimensions
      integer( 4),intent(in   ) :: mlb                                !!  Lower bound of all m dimensions
      integer( 4),intent(in   ) :: mub                                !!  Upper bound of all m dimensions
      integer( 4),intent(in   ) :: itim                               !!  Time in file
      integer( 4),intent(in   ) :: kfsmin(nlb:nub , mlb:mub )         !!  Variable lowest active layer (z-model-only)
      real   (fp),intent(inout) :: quwaq( nlb:nub , mlb:mub , 1:kmax) !!  Array with Q in u dir values
      real   (fp),intent(inout) :: qvwaq( nlb:nub , mlb:mub , 1:kmax) !!  Array with Q in v dir values
      real   (fp),intent(inout) :: qwwaq( nlb:nub , mlb:mub , 0:kmax) !!  Array with Q in w dir values
      real   (fp),intent(in   ) :: areau( nlb:nub , mlb:mub ,   kmax) !!  Exchange areas in u points
      real   (fp),intent(in   ) :: areav( nlb:nub , mlb:mub ,   kmax) !!  Exchange areas in v points
      integer( 4),intent(in   ) :: noseg                              !!  number of Waq computational cells
      real   ( 4),intent(in   ) :: horsurf( noseg )                   !!  Vertical exchange areas
      integer( 4),intent(in   ) :: iqaggr(nmax,mmax,kmax,3)           !!  flow aggreagtion pointer
      integer( 4),intent(in   ) :: noq                                !!  number of resulting exchanges
      integer( 4),intent(in   ) :: noq12                              !!  number of horizontal exchanges
      real   ( 4),intent(  out) :: qag(0:noq)                         !!  single precission result array for flow
      logical   , intent(in   ) :: zmodel                             !!  true if z-model feature is used
      integer( 4),intent(in   ) :: naccum                             !!  number of fluxes accumulated
      integer( 4),intent(in   ) :: lunare                             !!  file unit number to an output file
!
!           Local variables
!
      integer(4) i, j, k, iq
!
!! executable statements -------------------------------------------------------
!
      qag   = 0.0
      do k = 1,kmax
         do j = 1,mmax
            do i = 1,nmax
               iq = iqaggr(i,j,k,1)
               qag(iq) = qag(iq) + areav(i,j,k)
               iq = iqaggr(i,j,k,2)
               qag(iq) = qag(iq) + areau(i,j,k)
            enddo
         enddo
      enddo
      if ( noq .ne. noq12) qag(noq12+1:noq) = horsurf(1:noq-noq12)
      write ( lunare ) itim, qag(1:noq)
!
      qag   = 0.0
      do k = 1,kmax
         do j = 1,mmax
            do i = 1,nmax
               if ( zmodel .and. k .lt. kfsmin(i,j) ) cycle  ! not all layers may be active
               iq = iqaggr(i,j,k,1)
               qag(iq) = qag(iq) + qvwaq(i,j,k)/naccum       ! 1st direction
               iq = iqaggr(i,j,k,2)
               qag(iq) = qag(iq) + quwaq(i,j,k)/naccum       ! 2nd direction
               if ( k .ne. kmax ) then
                  iq = iqaggr(i,j,k,3)
                  qag(iq) = qag(iq) - qwwaq(i,j,k)/naccum    ! 3rd direction
               endif
            enddo
         enddo
      enddo
!
      quwaq    = 0.0D+00
      qvwaq    = 0.0D+00
      qwwaq    = 0.0D+00

      return
      end subroutine wrwaqflo
