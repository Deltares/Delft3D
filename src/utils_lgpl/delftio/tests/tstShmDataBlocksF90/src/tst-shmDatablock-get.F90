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
program test_get_shmds_f90

    use Dio_Shm

    integer :: t, nTimes = 10
    logical :: retVal
    double precision :: curTime = 100.D+00, otherTime= 7.7D+00
    character(len=100) :: name = 'testje'

    character(len=100) :: resFileName = 'TESTShmF90-res.txt' ! result file
    integer            :: resLun                             ! handle to result file

    open(newunit=resLun,file=resFileName)

    do t = 1, nTimes

#if (defined(WIN32))
        call sleep(100)
#else
        call DIOSYNCcSLEEP(100)
#endif

        curTime = curTime + 10.D+00

    100 retVal = DioShmDataBlockGetDouble(name, otherTime)
        if ( .not. retVal) then
            write(resLun,*) 'Step: ', t, ' DID NOT GET: ', otherTime
        endif
        if ( otherTime < (curTime - 1.D-05) ) goto 100

        write(resLun,*) 'Processing step: ', t, ' time: ', otherTime
        write(*,*) 'Processing step: ', t, ' time: ', otherTime

    enddo

    close(resLun)

end
