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
program test_get_dio_f90

    use Dio_Plt_Rw

    real * 4, dimension(:,:), pointer  :: readValues

    type(DioStreamType) :: hisInStream
    type(DioPltType)    :: hisInSet

    type(DioStreamType) :: hisInStream_Copy

    integer :: i,resLun

    logical :: readRes
    double precision :: julTime

    call DioInit

!   Open file for storing results

    open(newunit=resLun,file='TESTHisCopy-res.txt')

!   Open data stream

    write (*, *) 'Opening HIS IN Stream'
    hisInStream = DioStreamCreateSynched(dio_His_stream, 'tstHisIn.his', 'r', .false.)
    hisInStream_Copy = DioStreamCreateSynched(dio_His_stream, 'TESTHis_Copy.his', 'w', .false.)
    write (*, *) 'HIS IN Streams Created'

!   Get HIS OUT data set

    hisInSet = DioPltGetDataset(hisInStream, 'HisData')
    write (resLun, *) 'pars: ', hisInSet % header % nPar
    write (resLun, *) hisInSet % header % pars
    write (resLun, *) 'locs: ', hisInSet % header % nLoc
    write (resLun, *) hisInSet % header % locs
    call DioPltPutOnStream(hisInSet, hisInStream_Copy)
    write (*, *) 'HIS IN Datasets Initialized'

!   Get Values

    readRes = .true.
    i = 0
    do while ( readRes )
        readRes = DioPltGet(hisInSet, julTime, readValues)
        if (readRes) then
            i = i + 1
            write (*, *) 'Got hisInSet reals for Step: ', i
            write (resLun, *) 'Got hisInSet reals for Step: ', i
            if ( mod(i,10) .eq. 0 ) write(resLun,*) readValues
            call DioPltPut (hisInSet, julTime, readValues)
            write (*, *) 'Copied hisInSet for Step: ', i
            write (resLun, *) 'Copied hisInSet for Step: ', i
        endif
    end do
    
!   Close data stream

    call DioStreamClose(hisInStream)
    call DioStreamClose(hisInStream_Copy)

!   Close file for with results
    close(resLun)

    write (*, *) 'Got Everything'
    ! write (*, *) 'Got Everything, GIVE ENTER'
    ! read(*,*)
end

