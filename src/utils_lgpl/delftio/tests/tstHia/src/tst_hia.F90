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
subroutine read_his_long(datasetName)

    use Dio_Plt_Rw

    character(Len=*)  :: datasetName
    character(Len=DioMaxStreamLen)     :: baseName
    character(Len=DioMaxStreamLen)     :: resFile

    type(DioPltType)                   :: hisInSet

    integer :: i, resLun, ls
    integer :: nPar, NLoc
    character(Len=DioMaxParLen), dimension(:), pointer :: pars
    character(Len=DioMaxLocLen), dimension(:), pointer :: locs

    logical :: readRes

!   Open file for storing results

    call DioSyncMakeBaseName(datasetName, baseName)
    resFile = 'TEST' // trim(baseName) // '-res.txt'
    open(newunit=resLun,file=resFile)

!   Get HIS IN data set

    hisInSet = DioPltGetDataset(datasetName)
    if ( .false. ) then
        write (*, *) 'DID NOT GET HIS Dataset ', datasetName
    else
        write (*, *) 'GOT HIS Dataset ', datasetName

        nPar =  DioPltGetNPar(hisInSet); nLoc =  DioPltGetNLoc(hisInSet)
        pars => DioPltGetPars(hisInSet); locs => DioPltGetLocs(hisInSet)
        do i = 1, nPar
            write(resLun,*) 'Par: ', i, ': ', trim(pars(i))
        enddo
        do i = 1, nLoc
            write(resLun,*) 'Loc: ', i, ': ', trim(locs(i))
        enddo

    !   destroy datasets

        call DioPltDestroy(hisInSet)

    !   Close file for with results
        close(resLun)

        write (*, *) 'Got Everything'
        ! write (*, *) 'Got Everything, GIVE ENTER'
        ! read(*,*)

    endif

end subroutine read_his_long


program test_his_long

#if (defined(HAVE_CONFIG_H))
    call read_his_long('../../tstHia/unix/calcpnt1')
    call read_his_long('calcpnt2')
    call read_his_long('../../tstHia/unix/calcpnt3')
#else
    call read_his_long('..\..\tstHia\w32\calcpnt1')
    call read_his_long('calcpnt2')
    call read_his_long('..\..\tstHia\w32\calcpnt3')
#endif

end program test_his_long


