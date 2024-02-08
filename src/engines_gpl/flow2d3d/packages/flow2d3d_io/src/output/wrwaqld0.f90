      subroutine wrwaqld0 ( nsrc   , nmax   , mmax   , kmax   , mnksrc , &
     &                      discumwaq , loads  , nobrk  , nowalk , iwlk   , &
     &                      itim   , mode   , isaggr , lunwlk )
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
      integer(4) nsrc                    !!  Number of loads
      integer(4) nmax                    !!  Dimensions of the grid
      integer(4) mmax                    !!  Dimensions of the grid
      integer(4) kmax                    !!  Dimensions of the grid
      integer(4) mnksrc(7,nsrc)          !!  locations and type
      real  (fp) discumwaq(  nsrc)       !!  the cumulative flows
      real   (4) loads (  nsrc)          !!  the saved actual flows
      integer(4) nobrk                   !!  number of breakpoints in workfile
      integer(4) nowalk                  !!  number of walking discharges
      integer(4) iwlk  (  nsrc)          !!  their number in the list of dicharges
      integer(4) itim                    !!  time of writing
      integer(4) mode                    !!  zero at init, 2 at finalisation
      integer(4) isaggr(nmax,mmax,kmax)  !!  horizontal grid aggregation pointer
      integer(4) lunwlk                  !!  file unit number to an output file
!
!           Local variables
!
      integer(4) iseg, i, n, m, k
!
!! executable statements -------------------------------------------------------
!
!             trivial

      if ( nsrc .eq. 0 ) return

!             initialisation

      discumwaq = 0.0
      loads     = 0.0
      nobrk     = 0

!             position walking discharge

      if ( nowalk .gt. 0 ) then
         m = mnksrc(4,iwlk(1))
         n = mnksrc(5,iwlk(1))
         k = mnksrc(6,iwlk(1))
         iseg = isaggr(n,m,k)
         write ( lunwlk , '(i12/4i12)' ) 1, iseg, m, n, k
         do i = 2,nowalk
            m = mnksrc(4,iwlk(i))
            n = mnksrc(5,iwlk(i))
            k = mnksrc(6,iwlk(i))
            iseg = isaggr(n,m,k)
            write ( lunwlk , '(4I12)' )    iseg, m, n, k
         enddo
      endif
      end subroutine wrwaqld0
