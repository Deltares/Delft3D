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

      subroutine read_cco(file_cco, mmax  , nmax  , xdepth, ydepth)

      ! function : read a cco file and check dimensions

      ! global declarations

      use m_srstop
      use m_monsys
      use filmod                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_dlwqfile)                       :: file_cco               ! aggregation-file
      integer                                :: mmax                   ! grid cells m direction
      integer                                :: nmax                   ! grid cells n direction
      real                                   :: xdepth(nmax,mmax)      ! x coordinate depth points
      real                                   :: ydepth(nmax,mmax)      ! y coordinate depth points

      ! local declarations

      integer                                :: mmaxd                  ! grid cells m direction from cco file
      integer                                :: nmaxd                  ! grid cells n direction from cco file
      real                                   :: x0                     ! x coordinate origin
      real                                   :: y0                     ! y coordinate origin
      real                                   :: alpha                  ! alpha
      integer                                :: npart                  ! npart
      integer                                :: nolay                  ! nolay
      integer                                :: ioerr                  ! error on file
      integer                                :: i                      ! loop counter
      integer                                :: m                      ! loop counter
      integer                                :: n                      ! loop counter
      real                                   :: rdum                   ! dummy
      integer                                :: lunrep                 ! unit number report file

      call getmlu(lunrep)

      call dlwqfile_open(file_cco)
      read(file_cco%unit_nr,iostat=ioerr) mmaxd, nmaxd, x0, y0, alpha, npart, nolay
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading cco file header record'
         call srstop(1)
      endif

      if ( nmaxd.ne.nmax .or. mmaxd.ne.mmax ) then
         write(lunrep,*) ' dimensions cco file differ from input hydrodynamics'
         call srstop(1)
      endif

      do i=1 , 2*npart+9
         read(file_cco%unit_nr,iostat=ioerr) rdum
         if ( ioerr .ne. 0 ) then
            write(lunrep,*) ' error reading cco file dummy records'
            call srstop(1)
         endif
      enddo

      read(file_cco%unit_nr,iostat=ioerr) ((xdepth(n,m),n=1,nmax),m=1,mmax)
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading cco file xdepth'
         call srstop(1)
      endif
      read(file_cco%unit_nr,iostat=ioerr) ((ydepth(n,m),n=1,nmax),m=1,mmax)
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading cco file ydepth'
         call srstop(1)
      endif

      close(file_cco%unit_nr)
      file_cco%status = FILE_STAT_UNOPENED

      return
      end
