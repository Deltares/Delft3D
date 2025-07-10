!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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

      subroutine write_srf ( file_srf, num_columns  , num_rows  , nosegl, surf  )
!
!     created             : jan van beek
!
!     function            : writes horizontal surface file.
!
!     subroutines called  : -
!                         : which_operating_system, return platform type
!
      ! global declarations

      use m_waq_file                   ! module contains everything for the files
      implicit none

      ! declaration of arguments

      type(t_file)                       :: file_srf               ! surfaces-file
      integer                                :: num_columns                   ! grid cells m direction
      integer                                :: num_rows                   ! grid cells n direction
      integer                                :: nosegl                 ! number of segments per layer
      real                                   :: surf(nosegl)           ! surf

      ! local declarations

      integer       lun
      integer       i
      integer       idummy
      integer       irlen
      integer       plform
      integer       filtyp
      integer       filsta

      plform = which_operating_system()
      idummy = 0

      ! initialise file

      call file_srf%open()
      lun    = file_srf%unit
      filtyp = file_srf%type

      ! write surfaces file

      if ( filtyp .eq. FT_UNF .or. filtyp .eq. FT_BIN) then
         write (lun) num_rows,num_columns,nosegl,nosegl,nosegl,idummy
         write (lun) (surf(i),i=1,nosegl)
      elseif ( filtyp .eq. FT_ASC ) then
         write (lun,'(4i8)') num_rows,num_columns,nosegl,nosegl,nosegl,idummy
         write (lun,'(e13.6)') (surf(i),i=1,nosegl)
      endif

      close(file_srf%unit)
      file_srf%status = FILE_STAT_UNOPENED

      return
      end

      subroutine write_hsrf ( file_hsrf, num_cells, surf  )
!
!     created             : michelle jeuken
!
!     function            : writes horizontal surface file (new unstructured style).
!
      ! global declarations

      use m_waq_file                   ! module contains everything for the files
      implicit none

      ! declaration of arguments

      type(t_file)                       :: file_hsrf              ! surfaces-file
      integer                                :: num_cells                  ! number of segments
      real                                   :: surf(num_cells)            ! horizontal surfaces

      ! local declarations

      integer       lun
      integer       i
      integer       idummy
      integer       irlen
      integer       plform
      integer       filtyp
      integer       filsta

      plform = which_operating_system()
      idummy = 0

      ! initialise file

      call file_hsrf%open()
      lun    = file_hsrf%unit
      filtyp = file_hsrf%type

      ! write horizontal surfaces file
      if ( filtyp .eq. FT_UNF .or. filtyp .eq. FT_BIN) then
         write (lun) idummy, (surf(i),i=1,num_cells)
      elseif ( filtyp .eq. FT_ASC ) then
         write (lun,'(i8)') idummy
         write (lun,'(e13.6)') (surf(i),i=1,num_cells)
      endif

      close(file_hsrf%unit)
      file_hsrf%status = FILE_STAT_UNOPENED

      return
      end
