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

      subroutine write_grd(file_grd, mmax  , nmax  , xdepth, ydepth)

      ! function : write a grd file

      ! global declarations

      use filmod                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_dlwqfile)                       :: file_grd               ! aggregation-file
      integer                                :: mmax                   ! grid cells m direction
      integer                                :: nmax                   ! grid cells n direction
      real                                   :: xdepth(nmax,mmax)      ! x coordinate depth points
      real                                   :: ydepth(nmax,mmax)      ! y coordinate depth points

      ! local declarations

      integer                                :: n, m                   ! loop counter
      integer                                :: lun

      call dlwqfile_open(file_grd)
      lun    = file_grd%unit_nr

      write(file_grd%unit_nr,'(a)')   'Coordinate System = Cartesian'
      write(file_grd%unit_nr,'(2i8)') mmax-1,nmax-1
      write(file_grd%unit_nr,'(a)')   ' 0 0 0'

      do n = 1 , nmax - 1
         write(file_grd%unit_nr,'(a,i5,2x,5(e24.17,2x),12x)') ' ETA=',n,(xdepth(n,m),m=1,mmax-1)
      enddo

      do n = 1 , nmax - 1
         write(file_grd%unit_nr,'(a,i5,2x,5(e24.17,2x),12x)') ' ETA=',n,(ydepth(n,m),m=1,mmax-1)
      enddo

      close(file_grd%unit_nr)
      file_grd%status = FILE_STAT_UNOPENED

      return
      end
