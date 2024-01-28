      subroutine wrwaqcco ( nmax   , mmax   , kmax   , nlb    , nub    , &
     &                      mlb    , mub    , xcor   , ycor   , filnam )
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
      integer(4) nmax                    !!  Dimension of first index in 2d arrays
      integer(4) mmax                    !!  Dimension of second index in 2d arrays
      integer(4) kmax                    !!  number of layers
      integer(4) nlb                     !!  Lower bound of all n dimensions
      integer(4) nub                     !!  Upper bound of all n dimensions
      integer(4) mlb                     !!  Lower bound of all m dimensions
      integer(4) mub                     !!  Upper bound of all m dimensions
      real(fp) xcor( nlb:nub , mlb:mub ) !!  Array with x-values corners
      real(fp) ycor( nlb:nub , mlb:mub ) !!  Array with y-values corners
      character(*) filnam                !!  Filename without extension
!
!           Local variables
!
      real(4), pointer :: cor(:,:)       !!  To deal with double precission
      integer(4) i, j                    !!  loop counters
      real   (4) x, y                    !!  for the first (x,y) point
      integer(4) lunout
      integer(4) istat                   !!  allocate return status
!
!! executable statements -------------------------------------------------------
!
      allocate ( cor(nmax,mmax), stat=istat )
      if (istat/=0) then
         write(*,*) '*** ERROR: wrwaqcco: memory allocation error'
         return
      endif
!

#ifdef HAVE_FC_FORM_BINARY
      open  ( newunit = lunout , file=trim(filnam)//'cco' , form = 'binary' )
#else
      open  ( newunit = lunout , file=trim(filnam)//'cco' , form = 'unformatted', access='stream')
#endif
      x = xcor(1,1)
      y = ycor(1,1)
      write ( lunout ) mmax, nmax, x, y, 0, 0, kmax
      write ( lunout ) 0, 0, 0, 0, 0, 0, 0, 0, 0
      do j = 1,mmax
         do i = 1,nmax
            cor(i,j) = xcor(i,j)
         enddo
      enddo
      write ( lunout ) cor
      do j = 1,mmax
         do i = 1,nmax
            cor(i,j) = ycor(i,j)
         enddo
      enddo
      write ( lunout ) cor
      close ( lunout )
!
      deallocate ( cor )
      end subroutine wrwaqcco
