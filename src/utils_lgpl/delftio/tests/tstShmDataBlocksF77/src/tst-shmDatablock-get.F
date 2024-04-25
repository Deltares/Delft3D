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
      program test_get_shmds_f77

      integer t, nTimes, nGetTimes, nRealTimes
      double precision curTime, otherTime
      character*(100) name

      character*(100) resFileName
      integer resLun

      resFileName = 'TESTShmF77-res.txt'
      open(newunit=resLun,file=resFileName)

      nRealTimes = 10
      nGetTimes = 15
      curTime = 100.D+00
      otherTime= 7.7D+00
      name = 'testje'

      do t = 1, nGetTimes

        call CUTIL_SLEEP(50)

        curTime = curTime + 10.D+00

 100    otherTime = 0.D+00
        call dio_shm_f77_getdb_double(name, otherTime)
        if ( otherTime .lt. (curTime - 1.D-05) ) then
         if ( t .le. nRealTimes) goto 100
        endif
        write(resLun,*) 'Processing step: ', t, ' Time:', otherTime
        write(*,*) 'Processing step: ', t, ' Time:', otherTime

      enddo

      close(resLun)

      end
