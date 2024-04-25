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
program tst_rewind

    use Dio_Plt_Rw
    
    implicit none

    ! arguments

    character(Len=100) :: datasetName     = 'TESTRewind1.his'
    character(Len=100) :: datasetNoHeader = 'TESTRewind2.his'

    character(10), dimension(5) :: vars
    character(10), dimension(3) :: locs
    real * 4, dimension(5,3)    :: values
    integer, dimension(3)       :: intIds

    character(Len=DioMaxTimLen) :: dioTime
    double precision            :: julTimeStart, deltaJulTime, julTime

    character(HisRunIdSize), dimension(HisRunIdDim) :: runId

    type(DioPltType)  :: hisOutSet

    data vars   / 'Aa', 'Bb', 'Cc', 'Dd', 'Ee' /
    data locs   / '11', '22', '33' /
    data intIds / 101, 102, 103 /

    data runId / 'myTestRun', 'check if set Append Works', &
                 'write_his_long', 'T0: 2002.01.01 00:00:00  (scu=    3600s)' /

    integer :: i, j

!   Set Julian start time and delta
!   Delta Julian: 14 seconds

    dioTime = '1999.12.31 23:59:59'
    julTimeStart = DioDsTimeString2Julian(dioTime)
    deltaJulTime = 1.0D+00 / 86400.0D+00

!   Create HIS OUT data set

    do i = 1, 3
        do j = 1, 5
            values(j,i) = j * 0.01 + i * 0.10
        enddo
    enddo

!   Create OUT data set
    write (*, *) 'Creating OUT dataset'
    hisOutSet = DioPltDefine(datasetName, RunID, Dio_Plt_Real, vars, intIds, locs, julTimeStart)
    write (*, *) 'OUT Dataset Created'


!   Put OUT data for 3 timesteps
    julTime = julTimeStart
    do i = 1, 3
        call DioPltPut (hisOutSet, julTime, values)
        values = values + 0.1
        julTime = julTime + deltaJulTime
    enddo

!   Destroy dataset
    call DioPltDestroy(hisOutSet)
    write (*, *) 'Have destroyed HIS OUT set'

!   Open OUT data set for append (check existence)
!   Put OUT data for 7 more timesteps
    hisOutSet = DioPltDefine(datasetNoHeader, RunID, Dio_Plt_Real, vars, intIds, locs, julTimeStart)
    !!! hisOutSet = DioPltDefine(datasetNoHeader, Dio_Plt_Real, vars, locs)
    if (DioPltOpenedOK(hisOutSet)) then
        call DioPltRewind(hisOutSet)
        do i = 1, 7
            call DioPltPut (hisOutSet, julTime, values)
            values = values + 0.1
            julTime = julTime + deltaJulTime
        enddo
    endif

!   Destroy dataset
    call DioPltDestroy(hisOutSet)
    write (*, *) 'Have destroyed HIS OUT set'

end program tst_rewind

