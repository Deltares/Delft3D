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

      subroutine write_lgt ( file_lgt, num_columns  , num_rows  , num_layers )
!
!     delft hydraulics
!
!     created             : jan van beek
!
!     function            : writes total grid to file.
!
!     subroutines called  : -
!
!     parameters          :
!
!     name    kind     length     funct.  description
!     ----    -----    ------     ------- -----------
!     filnr   integer  1          input   index file in file administr.
!     num_columns    integer  1          input   x,u direction, second in lgrid
!     num_rows    integer  1          input   y,v direction, first in lgrid
!     num_layers   integer  1          input   number of layers
!
      ! global declarations

      use m_waq_file                   ! module contains everything for the files
      implicit none

!     declaration of arguments

      type(t_file)                       :: file_lgt               ! aggregation-file
      integer       num_columns  , num_rows  , num_layers
!
!     local declarations
!
      integer       lun
      integer       i
      integer       irlen
      integer       plform
      character(len=256) filnam
      integer       filtyp
      integer       filsta
      character(len=6) binary
      binary = 'BINARY'
      plform = PL_DOS
!
!     initialise file
!
      call file_lgt%open()
      lun    = file_lgt%unit
      filtyp = file_lgt%type
!
!     write table
!
      if ( filtyp .eq. FT_UNF .or. filtyp .eq. FT_BIN) then
         write (lun) num_rows,num_columns,num_rows*num_columns,num_layers
         write (lun) (i,i=1,num_rows*num_columns)
      elseif ( filtyp .eq. FT_ASC ) then
         write (lun,'(4i8)') num_rows,num_columns,num_rows*num_columns,num_layers
         write (lun,'(i7)') (i,i=1,num_rows*num_columns)
      endif
!
      return
      end
