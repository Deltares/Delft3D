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
subroutine his_descr_put(datasetName)

    use Dio_Plt_Rw
    
    implicit none

    ! arguments

    character(Len=DioMaxStreamLen), dimension(2) :: datasetName

    ! locals

    integer, parameter :: numVars      = 5
    integer, parameter :: numLocs      = 3
    integer, parameter :: numTimeSteps = 7
    
    character(32), dimension(numVars) :: vars
    character(64), dimension(numVars) :: parDescr
    character(40), dimension(numLocs) :: locs
    character(80), dimension(numLocs) :: locDescr
    integer, dimension(numLocs)       :: intIds
    real, dimension(numVars,numLocs)  :: values

    type(DioPltType)  :: hisOutSet(2)

    type(TDioIniFile)              :: hiaFile          ! HIA IniFile Handle

    data vars      / 'Aa', 'Bb', 'Cc', 'Dd', 'Ee' /
    data parDescr  / 'Aa-aa-aa', '', 'Cc-cc-cc', 'Dd-dd-dd', 'Ee-ee-ee' /
    data locs      / '11', '22', '33' /
    data locDescr  / '11-11-11', '22-22-22', '' /
    data intIds    /  111,  222,  333 /
 
    integer :: i, j

!   Create HIS OUT data set

    do i = 1, numLocs
        do j = 1, numVars
            values(j,i) = j * 0.01 + i * 0.10
        enddo
    enddo

    write (*, *) 'Creating OUT dataset'

    hisOutSet(2) = DioPltDefine(datasetName(2), Dio_Plt_Real, vars, intIds, locs)
    call DioPltAddDescriptions(hisOutSet(2), dio_plt_locs, locDescr)
    call DioPltAddDescriptions(hisOutSet(2), dio_plt_pars, parDescr)

    hisOutSet(1) = DioPltDefine(datasetName(1), Dio_Plt_Real, vars, locs)
    call DioPltAddDescriptions(hisOutSet(1), dio_plt_locs, locDescr)    

    write (*, *) 'OUT Dataset Created'

!   Put HIS OUT data for each timestop

    do i = 1, numTimeSteps
        call DioPltPut (hisOutSet(1),values)
        call DioPltPut (hisOutSet(2),values + 2220)
        values = values + 0.1
        write (*, *) 'Reals have been put for Step: ', i
        ! read (*, *)
    enddo

!   destroy datasets

    call DioPltDestroy(hisOutSet(1))
    call DioPltDestroy(hisOutSet(2))

    write (*, *) 'Have destroyed HIS OUT sets'

end subroutine his_descr_put


subroutine his_descr_get(resFileName, datasetName)

    use Dio_Plt_Rw
    
    implicit none

    ! arguments

    character*(*)                                :: resFileName
    character(Len=DioMaxStreamLen), dimension(2) :: datasetName

    ! locals

    character(Len=DioMaxParLen), dimension(:), pointer :: parDescr
    character(Len=DioMaxLocLen), dimension(:), pointer :: locDescr
    integer                    , dimension(:), pointer :: intIds
    real,                    dimension(:,:,:), pointer :: readValues

    integer, parameter                     :: numInSets = 2
    type(DioPltType), dimension(numInSets) :: hisInSet

    integer                        :: resLun, plt, i

!   Open file for storing results

    open(newunit=resLun,file=resFileName)

!   Open data stream

!   Get HIS IN data set

    do plt = 1, numInSets

        write (resLun, *) 'Getting IN Dataset ', trim(datasetName(plt))
        write (*, *) 'Getting IN Dataset ', trim(datasetName(plt))

        hisInSet(plt) = DioPltGetDataset(datasetName(plt))

        write (*, *) 'Got IN Dataset ', trim(datasetName(plt)), ', IntIds'

        intIds => DioPltGetIntIds(hisInSet(plt))
        if (associated(intIds))then
            do i = 1, DioPltGetNLoc(hisInSet(plt))
                write (resLun, *) 'IntID ', i, ' : ', intIds(i)
            enddo
        else
            write (resLun, *) 'Could not Get IntIDs for ', trim(datasetName(plt))
        endif

        write (*, *) 'Got IN Dataset ', trim(datasetName(plt)), ', parDescr.s'

        parDescr => DioPltGetDescriptions(hisInSet(plt), dio_plt_pars)
        if (associated(parDescr))then
            do i = 1, DioPltGetNPar(hisInSet(plt))
                write (resLun, '(A,I2,2A)') '  ParDescr ', i, '): ', trim(parDescr(i))
            enddo
        else
            write (resLun, *) 'NO parDescr.s for ', trim(datasetName(plt))
        endif

        write (*, *) 'Got IN Dataset ', trim(datasetName(plt)), ', locDescr.s'

        locDescr => DioPltGetDescriptions(hisInSet(plt), dio_plt_locs)
        if (associated(locDescr))then
            do i = 1, DioPltGetNLoc(hisInSet(plt))
                write (resLun, '(A,I2,2A)') '  LocDescr ', i, '): ', trim(locDescr(i))
            enddo
        else
            write (resLun, *) 'NO locDescr.s for ', trim(datasetName(plt))
        endif

    end do
    
!   destroy dataset

    do plt = 1, numInSets
        call DioPltDestroy(hisInSet(plt))
    end do
    
    write (*, *) 'Have destroyed HIS IN sets'
    write (resLun, *) 'Have destroyed HIS IN sets'

!   Close file for with results
    close(resLun)

end subroutine his_descr_get


program test_hia_descr

    use Dio_Plt_Rw
    
    character(Len=DioMaxStreamLen), dimension(2) :: datasetName

    datasetName(1) = 'TESTLocDesc' 
    datasetName(2) = 'TESTParAndLocWithIntIds' 

    call his_descr_put(datasetName)
    call his_descr_get('TESTDescr-res.txt', datasetName)

end

