      subroutine chkdry(iwet, kfs, notims, nostat)
!
      implicit none
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
!
!***********************************************************************
! Deltares                         marine and coastal management
!
! subroutine         : chkdry
! version            : v1.0
! date               : July 1997
! programmer         : Theo van der Kaaij
!
! function           : Checks whether a nesting station is permanent
!                      dry
! limitations        :
! subroutines called :
!***********************************************************************
!
      integer nostat, notims
      integer iwet (nostat)
      integer kfs  (nostat,notims)
!
      integer istat, itim
!----------------------------------------------------------------------
!     cycle over all stations
!----------------------------------------------------------------------
      do 10 istat = 1, nostat
         iwet (istat) = 0
         do 20 itim = 1, notims
!----------------------------------------------------------------------
!           detemine if a monitoring point is permanently dry 
!----------------------------------------------------------------------
            if (kfs(istat,itim)==1) then
               iwet (istat) = 1
               goto 30
            endif
   20    continue
   30    continue
   10 continue
      return
      end
