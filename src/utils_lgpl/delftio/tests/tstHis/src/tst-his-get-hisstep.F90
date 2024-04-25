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
program his_get_hisstep

    use Dio_Plt_Rw

    ! arguments

    character(Len=100) :: resFileName = 'TESTHisHisstep-res.txt'
    character(Len=100) :: datasetName = 'TESTHisHisstep.his'

    character(Len=DioMaxTimLen), external   :: DioJulian2DioTime

    real * 4, dimension(:,:), pointer       :: readValues
    double precision, dimension(:), pointer :: allJulians
    integer                                 :: nTimes
    integer                                 :: hisStep, hisUnit, hisMult

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
    allJulians => DioPltGetTimes(hisInSet)
    nTimes = DioPltGetNTimes(hisInSet)
    write (*, *) 'nTimes: ', nTimes
    write (resLun, *) 'Times:'
    do i = 1, nTimes
        write (resLun, *) allJulians(i), '  ', DioJulian2DioTime(allJulians(i))
    end do
    hisUnit = DioPltGetHisTimeUnit(hisInSet)
    hisMult = DioPltGetHisTimeMult(hisInSet)
    write (resLun, *) 'HIS TIME STEP UNIT      : ', hisUnit
    write (resLun, *) 'HIS TIME STEP MULTIPLIER: ', hisMult

!   Get Values

    do i = 1, 50
        if (DioPltGet(hisInSet, hisStep, readValues)) then
            write (resLun, *) 'Got hisInSet reals for Step: ', i,&
                         ' hisStep: ', hisStep
            write(resLun,*) readValues
        else
            write (resLun, *) 'Did NOT Get hisInSet reals for Step: ', i
        endif
    end do
    
!   destroy dataset

    call DioPltDestroy(hisInSet)

    write (*, *) 'Have destroyed HIS IN set'

!   Close file for with results
    close(resLun)

end program his_get_hisstep

