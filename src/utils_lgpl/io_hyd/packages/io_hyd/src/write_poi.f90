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

      subroutine write_poi ( file_poi , num_exchanges   , num_exchanges_u_dir  , num_exchanges_v_dir  , num_exchanges_z_dir  , ipoint   )
!
!     created             : jan van beek
!
!     function            : writes pointers to delwaq auxiliary input file.
!
!     subroutines called  : -
!
!     parameters          :
!
!     name    kind     length     funct.  description
!     ----    -----    ------     ------- -----------
!     filnr   integer  1          input   index file in file administr.
!     num_exchanges_u_dir    integer  1          input   number of items 1 in file
!     num_exchanges_v_dir    integer  1          input   number of items 2 in file
!     num_exchanges_z_dir    integer  1          input   number of items 3 in file
!     ipoint  integer  *          input   pointer array
!
      ! global declarations

      use m_waq_file                   ! module contains everything for the files
      implicit none

!     declaration of arguments
!
      type(t_file)                       :: file_poi               ! pointer file
      integer       num_exchanges   , num_exchanges_u_dir  , num_exchanges_v_dir  , num_exchanges_z_dir
      integer       ipoint(*)
!
!     local declarations
!
      integer       noq12 , noq123, lun, k
      integer       irlen
      integer       plform
      character(len=256) filnam
      integer       filtyp
      integer       filsta

      plform = which_operating_system()
!
!     initialise file
!
      call file_poi%open()
      lun    = file_poi%unit
      filtyp = file_poi%type
      filnam = file_poi%name
!
!     write pointers
!
      noq12  = num_exchanges_u_dir  + num_exchanges_v_dir
      noq123 = noq12 + num_exchanges_z_dir
!
      if ( filtyp .eq. FT_UNF .or. filtyp .eq. FT_BIN) then
         if ( num_exchanges_u_dir .gt. 0 ) write (lun) (ipoint(k),k=1,4*num_exchanges_u_dir)
         if ( num_exchanges_v_dir .gt. 0 ) write (lun) (ipoint(k),k=4*num_exchanges_u_dir+1,4*noq12)
         if ( num_exchanges_z_dir .gt. 0 ) write (lun) (ipoint(k),k=4*noq12+1,4*noq123)
      elseif ( filtyp .eq. FT_ASC ) then
         if ( num_exchanges_u_dir .gt. 0 ) write (lun,'(4(i7,1x))') (ipoint(k),k=1,4*num_exchanges_u_dir)
         if ( num_exchanges_v_dir .gt. 0 ) write (lun,'(4(i7,1x))') (ipoint(k),k=4*num_exchanges_u_dir+1,4*noq12)
         if ( num_exchanges_z_dir .gt. 0 ) write (lun,'(4(i7,1x))') (ipoint(k),k=4*noq12+1,4*noq123)
      endif

      close(file_poi%unit)
      file_poi%status = FILE_STAT_UNOPENED

      return
      end
