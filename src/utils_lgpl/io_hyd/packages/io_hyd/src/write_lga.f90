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

      subroutine write_lga ( file_lga, mmax  , nmax  , nolay , nosegl, &
                             noq1    , noq2  , noq3  , lgrid )
!
!     created             : jan van beek
!
!     function            : writes active grid to file.
!
!     subroutines called  : -
!
!     parameters          :
!
!     name    kind     length     funct.  description
!     ----    -----    ------     ------- -----------
!     filnr   integer  1          input   index file in file administr.
!     mmax    integer  1          input   x,u direction, second in lgrid
!     nmax    integer  1          input   y,v direction, first in lgrid
!     nolay   integer  1          input   number of layers
!     nosegl  integer  1          input   number of delwaq cells per layer aggregated
!     noq1    integer  1          input   number of exchanges 1st direction aggregated
!     noq2    integer  1          input   number of exchanges 2st direction aggregated
!     noq3    integer  1          input   number of exchanges 3d direction aggregated
!     lgrid   integer  nmax,mmax  input   grid table
!
      ! global declarations

      use m_waq_file                   ! module contains everything for the files
      implicit none

!     declaration of arguments

      type(t_file)                       :: file_lga               ! aggregation-file
      integer       mmax  , nmax  , nolay , nosegl, noq1  , noq2  , noq3
      integer       lgrid(nmax,mmax)
!
!     local declarations
!
      integer       lun
      integer       n
      integer       m
      integer       irlen
      integer       plform
      character*256 filnam
      integer       filtyp
      integer       filsta
      character*6   binary
      binary = 'BINARY'
      plform = PL_DOS
!
!     initialise file
!
      call file_lga%open()
      lun    = file_lga%unit
      filtyp = file_lga%type
!
!     write table
!
      if ( filtyp .eq. FT_UNF .or. filtyp .eq. FT_BIN) then
         write (lun) nmax,mmax,nosegl,nolay,noq1,noq2,noq3
         write (lun) ((lgrid(n,m),n=1,nmax),m=1,mmax)
      elseif ( filtyp .eq. FT_ASC ) then
         write (lun,'(4i8)') nmax,mmax,nosegl,nolay,noq1,noq2,noq3
         write (lun,'(i7)') ((lgrid(n,m),n=1,nmax),m=1,mmax)
      endif
!
      return
      end
