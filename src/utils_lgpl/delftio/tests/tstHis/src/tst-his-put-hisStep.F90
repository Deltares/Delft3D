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
program his_put_hisstep

    use Dio_Plt_Rw
    
    ! arguments

    character(Len=100) :: datasetName = 'TESTHisHisstep.his'

    character(10), dimension(5) :: vars
    character(10), dimension(3) :: locs
    real * 4, dimension(5,3)    :: values
    integer, dimension(3)       :: intIds
    integer                     :: i,j

    character(HisRunIdSize), dimension(HisRunIdDim) :: runId

    type(DioPltType)  :: hisOutSet

    data vars   / 'Aa', 'Bb', 'Cc', 'Dd', 'Ee' /
    data locs   / '11', '22', '33' /
    data intIds / 101, 102, 103 /

    data runId / 'myTestRun', 'check if set T0 works', &
                 'write_his_long', 'T0: 2002.01.01 00:00:00  (scu=       7s)' /

!   Create HIS OUT data set

    do i = 1, 3
        do j = 1, 5
            values(j,i) = j * 0.01 + i * 0.10
        enddo
    enddo

    write (*, *) 'Creating OUT dataset'

    hisOutSet = DioPltDefine(datasetName, RunID, Dio_Plt_Real, vars, intIds, locs)

    write (*, *) 'OUT Dataset Created'

!   Put HIS OUT data for each timestop

    do i = 1, 50
        call DioPltPut (hisOutSet, i*2, values)
        values = values + 0.1
        write (*, *) 'Reals have been put for Step: ', i
        ! read (*, *)
    enddo

!   destroy datasets

    call DioPltDestroy(hisOutSet)

    write (*, *) 'Have destroyed HIS OUT sets'

end program his_put_hisstep

