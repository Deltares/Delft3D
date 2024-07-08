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

      subroutine write_grd(file_grd, num_columns  , num_rows  , xdepth, ydepth)

      ! function : write a grd file

      ! global declarations

      use m_waq_file                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_file)                       :: file_grd               ! aggregation-file
      integer                                :: num_columns                   ! grid cells m direction
      integer                                :: num_rows                   ! grid cells n direction
      real                                   :: xdepth(num_rows,num_columns)      ! x coordinate depth points
      real                                   :: ydepth(num_rows,num_columns)      ! y coordinate depth points

      ! local declarations

      integer                                :: n, m                   ! loop counter
      integer                                :: lun

      call file_grd%open()
      lun    = file_grd%unit

      write(file_grd%unit,'(a)')   'Coordinate System = Cartesian'
      write(file_grd%unit,'(2i8)') num_columns-1,num_rows-1
      write(file_grd%unit,'(a)')   ' 0 0 0'

      do n = 1 , num_rows - 1
         write(file_grd%unit,'(a,i5,2x,5(e24.17,2x),12x)') ' ETA=',n,(xdepth(n,m),m=1,num_columns-1)
      enddo

      do n = 1 , num_rows - 1
         write(file_grd%unit,'(a,i5,2x,5(e24.17,2x),12x)') ' ETA=',n,(ydepth(n,m),m=1,num_columns-1)
      enddo

      close(file_grd%unit)
      file_grd%status = FILE_STAT_UNOPENED

      return
      end
