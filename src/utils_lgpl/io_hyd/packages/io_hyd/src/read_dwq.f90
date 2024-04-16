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

      subroutine read_dwq(file_dwq, mmax  , nmax, ipnt )

      ! function : read a dwq file and check dimensions

      ! global declarations

      use m_srstop
      use m_waq_file                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_file)                       :: file_dwq               ! aggregation-file
      integer                                :: mmax                   ! grid cells m direction
      integer                                :: nmax                   ! grid cells n direction
      integer                                :: ipnt(nmax,mmax)        ! aggregation pointer

      ! local declarations

      integer                                :: mmaxd                  ! grid cells m direction from dwq file
      integer                                :: nmaxd                  ! grid cells n direction from dwq file
      integer                                :: nmd                    ! total number of grid cells from dwq file
      integer                                :: ioptdd                 ! dido option ?
      integer                                :: idum                   ! dummy
      integer                                :: n,m                    ! loop counters
      integer                                :: ioerr                  ! error on file

      call file_dwq%open()
      read(file_dwq%unit,*,iostat=ioerr) nmaxd, mmaxd, nmd, ioptdd, idum
      if ( ioerr .ne. 0 ) then
         write(*,*) ' error reading dwq file'
         call srstop(1)
      endif

!     If nmaxd or mmaxd is one, only check if nmd.ne.nmax*mmax
      if (nmaxd.eq.1.or.mmaxd.eq.1) then
        if (nmd.ne.nmax*mmax) then
           write(*,*) ' dimensions grid on dido file differ from input hydrodynamics'
           call srstop(1)
        endif
      else   
        if (nmaxd.ne.nmax.or.mmaxd.ne.mmax) then
           write(*,*) ' dimensions grid on dido file differ from input hydrodynamics'
           call srstop(1)
        endif
      endif
      read(file_dwq%unit,*,iostat=ioerr) ((ipnt(n,m),n=1,nmax),m=1,mmax)
      if ( ioerr .ne. 0 ) then
         write(*,*) ' error reading dwq file'
         call srstop(1)
      endif

      return
      end
