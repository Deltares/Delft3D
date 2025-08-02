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

      subroutine finuni ( itemd, ihulp )
      character(len=50) itemd
      integer      ihulp , j     , nhaak
      logical      unit

!              write (*,*) ' FINUNI: '
!              write (*,*) '[',itemd,']'
      unit = .false.
      nhaak = 0
      ihulp = 0
      do 100 j = 50,1,-1
          if ( itemd(j:j) .eq. '(' .or. & 
              itemd(j:j) .eq. '['      ) then
              nhaak = nhaak-1
          endif
          if ( itemd(j:j) .eq. ')' .or. & 
              itemd(j:j) .eq. ']'      ) then
              nhaak = nhaak+1
              unit = .true.
          endif
          if ( nhaak .lt. 0 ) then
              write (*,*) ' ITEM: ',itemd
              stop 'FINUNI 001'
          endif
!              write (*,*) ' j    : ',j
!              write (*,*) ' nhaak: ',nhaak
!              write (*,*) ' unit : ',unit
          if ( nhaak .eq. 0 .and. unit ) then
              ihulp = j
              goto 200
          endif
  100 continue
  200 continue
      return
      end
