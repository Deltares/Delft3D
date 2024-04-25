      subroutine corcon (bndval,thick ,namcon,itypc ,nobnd ,notims,
     *                   kmax  ,lstci ,nocon ,cgen  ,cadd  ,cmax  ,
     *                   cmin                                     )
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
! subroutine         : corcon
! version            : v1.0
! date               : July 1997
! programmer         : Theo van der Kaaij
!
! function           : Adjust transport bc. and determine deth averaged
!                      bc.
! limitations        :
! subroutines called :
!***********************************************************************

      integer      cgen   (  6   )
      integer nocon, nobnd, notims, kmax

      double precision thick  (kmax  ),
     *                 cadd   (  6   ), cmax  (  6   ), cmin  (  6   )

      double precision bndval (nobnd ,notims,kmax  ,lstci,2)

      character*20 namcon(lstci  + 2)
      integer icon, lstci, itim
      integer ibnd, isize, k, itypc
!
!----adjust boundary conditions
!
      nocon = 0
      do 10 icon = 1, lstci

         if (cgen (icon) .eq. 1) then
            nocon = nocon + 1
            namcon(nocon) = namcon(icon)

            do 20 ibnd = 1, nobnd
               do 20 isize = 1, 2
                  do 20 itim = 1, notims
                     do 20 k = 1, kmax
                        bndval(ibnd,itim,k,nocon,isize) =
     *                  bndval(ibnd,itim,k,icon ,isize) + cadd (icon)
                        bndval(ibnd,itim,k,nocon,isize) =
     *                  min(bndval(ibnd,itim,k,nocon,isize),cmax (icon))
                        bndval(ibnd,itim,k,nocon,isize) =
     *                  max(bndval(ibnd,itim,k,nocon,isize),cmin (icon))
   20       continue
        endif
   10 continue
!
!----determine depth-averaged boundary conditions (if requested)
!
      if (itypc .eq. 1) then
         do 30 ibnd = 1, nobnd
            do 30 isize = 1, 2
               do 30 itim = 1, notims
                  do 30 icon = 1, nocon
                     bndval (ibnd,itim,1,icon,isize) =
     *               bndval (ibnd,itim,1,icon,isize) * thick(1)
                     do 40 k = 2, kmax
                        bndval (ibnd,itim,1,icon,isize) =
     *                  bndval (ibnd,itim,1,icon,isize) +
     *                  bndval (ibnd,itim,k,icon,isize) * thick(k)
   40                continue

                     bndval (ibnd,itim,kmax,icon,isize) =
     *               bndval (ibnd,itim,1   ,icon,isize)
   30    continue
      endif

      return

      end
