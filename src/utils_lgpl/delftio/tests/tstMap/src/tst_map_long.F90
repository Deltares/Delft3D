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
subroutine write_map_long

    use Dio_Plt_Rw
    
    ! locals

    character(Len=DioMaxStreamLen)  :: datasetName = 'TESTMapLong.map'

    integer, parameter              :: nVars = 7
    integer, parameter              :: nLocs = 4

    character(Len=200), dimension(nVars):: vars
    character(Len=200), dimension(nLocs):: locs
    real * 4, dimension(nVars,nLocs):: values
    integer :: i,j

    type(DioPltType)  :: outSet

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
                 'write_map_long', 'Vierde String' /

!   Create MAP OUT data set

    do i = 1, nLocs
        do j = 1, nVars
            values(j,i) = j * 0.01 + i * 0.10
        enddo
    enddo

    write (*, *) 'Creating OUT dataset'

    outSet = DioPltDefine(datasetName, &
                 runId, Dio_Plt_Real, vars, nLocs)

    write (*, *) 'OUT Dataset Created'

!   Put MAP OUT data for each timestop

    do i = 1, 50
        call DioPltPut (outSet,values)
        values = values + 0.1
        write (*, *) 'Reals have been put for Step: ', i
        ! read (*, *)
    enddo

!   destroy datasets

    call DioPltDestroy(outSet)

    write (*, *) 'Have destroyed MAP OUT sets'

end subroutine write_map_long


subroutine read_map_long

    use Dio_Plt_Rw

    character(Len=DioMaxStreamLen)     :: datasetName = 'TESTMapLong.map'
    real * 4, dimension(:,:), pointer  :: readValues

    type(DioPltType)                   :: inSet

    integer :: resLun, ls
    integer :: nPar, NLoc
    character(Len=DioMaxParLen), dimension(:), pointer :: pars
    ! character(Len=DioMaxLocLen), dimension(:), pointer :: locs

    logical :: readRes
    integer :: i, ierr

    nullify(pars)
    ! nullify(locs)
!   Open file for storing results

    open(newunit=resLun,file='TESTMapLong-res.txt', iostat=ierr)

!   Get MAP IN data set

    inSet = DioPltGetDataset(datasetName)
    write (*, *) 'MAP IN Datasets Initialized'

    if ( .not. inSet % ds % inStream % opened ) then
        write (*, *) 'DID NOT GET MAP Dataset ', datasetName
    else
        nPar =  DioPltGetNPar(inSet); nLoc =  DioPltGetNLoc(inSet)
        pars => DioPltGetPars(inSet); ! locs => DioPltGetLocs(inSet)
        write(resLun,*) 'NPar: ', nPar
        do i = 1, nPar
            write(resLun,*) 'Par: ', i, ': ', trim(pars(i))
        enddo
        write(resLun,*) 'NLoc: ', nLoc
        !if (associated(locs)) then
        !    do i = 1, nLoc
        !        write(resLun,*) 'Loc: ', i, ': ', trim(locs(i))
        !    enddo
        !else
        !    write(resLun,*) 'MAP: No Locations'
        !endif

    !   Get Values

        readRes = .true.
        i = 0
        do while ( readRes )
            readRes = DioPltGet(inSet, readValues)
            if (readRes) then
                i = i + 1
                write (*, *) 'Got inSet reals for Step: ', i
                write (resLun, *) 'Got inSet reals for Step: ', i
                if ( mod(i,10) .eq. 0 ) write(resLun,*) readValues
            endif
        end do

    endif
    
!   destroy datasets

    call DioPltDestroy(inSet)

!   Close file for with results
    close(resLun)

    write (*, *) 'Got Everything'
    ! write (*, *) 'Got Everything, GIVE ENTER'
    ! read(*,*)

end subroutine read_map_long


program test_map_long

    call write_map_long
    call read_map_long

end program test_map_long


