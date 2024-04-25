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
program test_put_shmds_f90

    use Dio_Shm

    integer :: t, nTimes = 10
    double precision :: curTime = 100.D+00
    character(len=100) :: name = 'testje'
    integer :: sleepMilliSec = 999

    do t = 1, nTimes
        call CUTIL_SLEEP(sleepMilliSec)
        curTime = curTime + 10.D+00
        write(*,*) 'Step: ', t, ' Sending: ', curTime
        call DioShmDataBlockPutDouble(name, curTime)
        write(*,*) 'Step: ', t, ' Sent   : ', curTime
    enddo


end
