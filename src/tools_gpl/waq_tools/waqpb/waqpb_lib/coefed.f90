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

      subroutine coefed(serial,itmswi)
!     
!          Create COEFEDIT.DAT file (Sobek only)
!     
!          Include data structures for tables
      use m_waqpb_data

      integer serial, lu_out, iitem, niteml
      logical itmswi(nitemm)

      niteml = 0
      do iitem = 1,nitem
          if ( itmswi(iitem) ) niteml = niteml + 1
      enddo

      open (newunit=lu_out,file='coefedit.dat')

      write (lu_out, '(i10)' ) serial
      write (lu_out, '(''NC'')' ) 
      write (lu_out, '(i6)' ) niteml
      write (lu_out, '(''COEFFICIENTS'')' ) 
      do iitem = 1,nitem
          if ( itmswi(iitem) ) then
            write (lu_out,'(''"'',a10,''",'',g15.7,'',1,1,1'')' ) itemid(iitem),itemde(iitem)
          end if
      enddo
      write ( lu_out,'(''NG''/''1''/''GROUPS''/ & 
             ''1,"Process parameters",0,0'')' )


      close (lu_out)

      return
      end
