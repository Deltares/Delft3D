      subroutine wrihyd(lundia,lun   ,bndval,typbnd,nobnd ,notims,
     *                  kmax  ,nolay ,tstart,dtmin )
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
!***********************************************************************
! Deltares                         marine and coastal management
!
! subroutine         : wrihyd
! version            : v1.0
! date               : July 1997
! programmer         : Theo van der Kaaij
!
! function           : Writes hydrodynamic bc. to TRISULA attribute file
!                      (old format)
! limitations        :
! subroutines called :
!***********************************************************************

      double precision tstart
      integer lundia, itdate, nolay, nobnd, notims, kmax
      
      double precision dtmin
      integer ifmt, ibnd, itim, lun, k, isize

      double precision bndval (nobnd ,notims,kmax  ,1     ,2)

      character*15 format
      character* 1 typbnd (nobnd )
!
      write (*     ,'('' >>> Writing hydrodynamic boundary '',
     *                ''conditions <<<'')')
      write (lundia,'('' >>> Writing hydrodynamic boundary '',
     *                ''conditions <<<'')')
!----------------------------------------------------------------------
!     Determine format for 3-dimensional bc (depending on nolay)
!----------------------------------------------------------------------

      format = '(   (1x,ES15.7))'
      write (format( 2: 4),'(i3)') nolay + 1

!----------------------------------------------------------------------
!     Write to time-series file
!----------------------------------------------------------------------

      do 10 itim = 1, notims
         do 10 ibnd = 1, nobnd
            do 10 isize = 1, 2
               if (typbnd(ibnd) .eq. 'z') then
                  write(lun,'(2(1x,f12.4))')tstart +  (itim -1)*dtmin,
     *                  bndval (ibnd,itim,1,1,isize)
               else
                  write(lun,format) tstart + (itim -1)*dtmin,
     *                 (bndval (ibnd,itim,k,1,isize),k = 1, nolay)
               endif
   10 continue

      return
      end
