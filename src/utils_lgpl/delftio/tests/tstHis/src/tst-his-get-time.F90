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
program his_get_time

    use Dio_Plt_Rw

    ! arguments

    character(Len=100) :: resFileName = 'TESTHisTimestep-res.txt'
    character(Len=100) :: datasetName = 'TESTHisTimestep.his'

    character(Len=DioMaxTimLen), external :: DioJulian2DioTime
    character(Len=DioMaxTimLen)           :: dioTime

    real * 4, dimension(:,:), pointer  :: readValues
    double precision   :: jul, julInSet, prevJul, diffJul
    double precision, dimension(:), pointer :: allJulians
    integer, dimension(:), pointer :: allHisSteps
    integer :: nTimes

    type(DioPltType)   :: hisInSet

    integer            :: i, resLun

!   Open file for storing results

    open(newunit=resLun,file=resFileName)

!   Open data stream

!   Get HIS IN data set

    write (*, *)
    write (*, *) 'Getting IN Dataset'

    hisInSet = DioPltGetDataset(datasetName)
    write (*, *) 'GOT IN Dataset'
    allHisSteps => DioPltGetHisSteps(hisInSet)
    allJulians => DioPltGetTimes(hisInSet)
    nTimes = DioPltGetNTimes(hisInSet)
    write (*, *) 'nTimes: ', nTimes
    write (resLun, *) 'Times:'
    do i = 1, nTimes
        if ( associated(allJulians) .and. &
             associated(allHisSteps) ) then
            dioTime = DioDsJulian2HisTime(allJulians(i))
            write (resLun, '(I3,A,I6,A,F16.8,A,A)') i, ' his:', allHisSteps(i),  ' jul:', &
                 allJulians(i), '  ', trim(dioTime)
        endif
    end do

!   Get Values

    prevJul = hisInSet % ds % startTimeVal
    do i = 1, 200
        if (DioPltGet(hisInSet, jul, readValues)) then
            write (resLun, *) 'Got hisInSet reals for Step: ', i, ' ', &
                trim(DioJulian2DioTime(DioPltGetCurrentTimeStep(hisInSet)))
            write (resLun, *) 'HIS=', hisInSet % ds % hisStep(1), jul
            julInSet = hisInSet % ds % timeStep(1)
            diffJul = jul - prevJul
            write (resLun, *) 'Jul=', jul, ' DiffJul: ', diffJul
            prevJul = Jul
            if ( i .eq. 1 .or. mod(i,10) .eq. 0 ) write(resLun,*) readValues
        else
            write (resLun, *) 'Did NOT Get hisInSet reals for Step: ', i
        endif
    end do
    
!   destroy dataset

    call DioPltDestroy(hisInSet)

    write (*, *) 'Have destroyed HIS IN set'

!   Close file for with results
    close(resLun)

end program his_get_time

