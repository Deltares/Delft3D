!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
      program observ
c
      integer obsfil
      integer i, j
c

      open (newunit=obsfil, file='observ.inp')

      do 10 i = 1, 10
         write(obsfil,'(''location: '',i3)') i
         do 20 j = 1, 100
c           velocities(1-3) and waterdepth(4) at time j, location i
            write(obsfil,*)
     *           1000*i + 10 * j + 1,
     *           1000*i + 10 * j + 2,
     *           1000*i + 10 * j + 3,
     *           1000*i + 10 * j + 4
   20    continue
   10 continue

      close (obsfil)

      stop
      end
