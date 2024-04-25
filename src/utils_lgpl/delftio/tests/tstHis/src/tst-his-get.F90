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
#define DO_ALL 1

subroutine his_get(resFileName, datasetName)

    use Dio_Plt_Rw

    ! arguments

    character*(*) :: resFileName
    character*(*) :: datasetName

    character(Len=DioMaxTimLen), external :: DioJulian2DioTime

    real, dimension(:,:,:), pointer :: readValues
    integer, dimension(:), pointer  :: intIds

    type(DioPltType)  :: hisInSet

    integer           :: i,resLun

!   Open file for storing results

    open(newunit=resLun,file=resFileName)

!   Open data stream

!   Get HIS IN data set

    write (*, *)
    write (*, *) 'Getting IN Dataset'

    hisInSet = DioPltGetDataset(datasetName)
    write (*, *) 'GOT IN Datasets'
    intIds => DioPltGetIntIds(hisInSet)
    if (associated(intIds))then
        do i = 1, DioPltGetNLoc(hisInSet)
            write (resLun, *) 'IntID ', i, ' : ', intIds(i)
        enddo
    else
        write (resLun, *) 'Could not Get IntIDs'
    endif

!   Get Values

    readValues => DioPltGetAll (hisInSet)

    do i = 1, 50
        write (*, *) 'hisInSet reals for Step: ', i, ' ', &
            trim(DioJulian2DioTime(hisInSet % ds % timeStep(i)))
        write (resLun, *) 'hisInSet reals for Step: ', i, ' ', &
            trim(DioJulian2DioTime(hisInSet % ds % timeStep(i)))
        write(resLun,*) readValues(:,:,i)
    end do
    
!   destroy dataset

    call DioPltDestroy(hisInSet)

    write (*, *) 'Have destroyed HIS IN set'

!   Close file for with results
    close(resLun)

end subroutine his_get


program test_his_get

    use Dio_Plt_Rw

    ! initialise Dio, call get function

#if DO_ALL
    call DioInit('dioconfigShm.ini')
    call his_get('TESTShm-res.txt', 'TESTHisData.shm')       ! Shm Exchange
#endif

    call DioInit
    call his_get('TESTHis-res.txt', 'TESTHisData.his') ! dioconfig.ini (HIS)

#if DO_ALL
    call DioInit('dioconfigAscii.ini')
    call his_get('TESTAscii-res.txt', 'TESTHisData.asc') ! ASCII files
#endif

end

