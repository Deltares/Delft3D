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
subroutine write_his_long_l

    use Dio_Plt_Rw
    
    ! locals

    character(Len=DioMaxStreamLen)  :: datasetName = 'TESTHisLong'

    integer, parameter              :: nVars = 7
    integer, parameter              :: nLocs = 4

    character(Len=200), dimension(nVars):: vars
    character(Len=200), dimension(nLocs):: locs
    real * 4, dimension(nVars,nLocs):: values
    integer :: i,j

    type(DioPltType)  :: hisOutSet

    character(HisRunIdSize), dimension(HisRunIdDim) :: runId

    data vars   / 'Aa', 'Bb is een hele lange, veel te lange naam', &
                  'Cc', 'Dd', 'En ook Ee is een hele lange, veel te &
                                                      &lange naam', &
                  'Ff', 'Gg'  /

    data locs   / '11 is best wel een locatie naam waarvan je zegt: &
                   &die is ontzettend lang zeg, nog langer dan wat &
                   &de LongHisNames module aan zou moeten kunnen', &
                  '22', '33', '44 is en vreselijk lange, ingewikkelde&
                               & en vervelende locatie naam' /

    data runId / 'myTestRun', 'check if long names work', &
                 'write_his_long_l', ' ' /

!   Create HIS OUT data set

    do i = 1, nLocs
        do j = 1, nVars
            values(j,i) = j * 0.01 + i * 0.10
        enddo
    enddo

    write (*, *) 'Creating OUT dataset'

    hisOutSet = DioPltDefine(datasetName, &
                 runId, Dio_Plt_Real, vars, locs)

    write (*, *) 'OUT Dataset Created'

!   Put HIS OUT data for each timestop

    do i = 1, 50
        call DioPltPut (hisOutSet,values)
        values = values + 0.1
        write (*, *) 'Reals have been put for Step: ', i
        ! read (*, *)
    enddo

!   destroy datasets

    call DioPltDestroy(hisOutSet)

    write (*, *) 'Have destroyed HIS OUT sets'

end subroutine write_his_long_l


subroutine read_his_long_l

    use Dio_Plt_Rw

    character(Len=DioMaxStreamLen)     :: datasetName = 'TESTHisLong'
    real * 4, dimension(:,:), pointer  :: readValues

    type(DioPltType)                   :: hisInSet

    integer :: resLun, ls
    integer :: nPar, NLoc
    character(Len=DioMaxParLen), dimension(:), pointer :: pars
    character(Len=DioMaxLocLen), dimension(:), pointer :: locs

    logical :: readRes
    integer :: i, ierr

!   Open file for storing results

    open(newunit=resLun,file='TESTHisLong-res.txt', iostat=ierr)

!   Get HIS IN data set

    hisInSet = DioPltGetDataset(datasetName)
    write (*, *) 'HIS IN Datasets Initialized'

    nPar =  DioPltGetNPar(hisInSet); nLoc =  DioPltGetNLoc(hisInSet)
    pars => DioPltGetPars(hisInSet); locs => DioPltGetLocs(hisInSet)
    do i = 1, nPar
        write(resLun,*) 'Par: ', i, ': ', trim(pars(i))
    enddo
    do i = 1, nLoc
        write(resLun,*) 'Loc: ', i, ': ', trim(locs(i))
    enddo

!   Get Values

    readRes = .true.
    i = 0
    do while ( readRes )
        readRes = DioPltGet(hisInSet, readValues)
        if (readRes) then
            i = i + 1
            write (*, *) 'Got hisInSet reals for Step: ', i
            write (resLun, *) 'Got hisInSet reals for Step: ', i
            if ( mod(i,10) .eq. 0 ) write(resLun,*) readValues
        endif
    end do
    

!   destroy datasets

    call DioPltDestroy(hisInSet)

!   Close file for with results
    close(resLun)

    write (*, *) 'Got Everything'
    ! write (*, *) 'Got Everything, GIVE ENTER'
    ! read(*,*)

end subroutine read_his_long_l


program test_his_long

    call write_his_long_l
    call read_his_long_l

end program test_his_long


