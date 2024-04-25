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
program read_map

    use Dio_Plt_Rw

    character(Len=DioMaxTimLen), external :: DioJulian2DioTime
    character(Len=DioMaxTimLen)           :: dioTime

    character(Len=DioMaxStreamLen)  :: datasetName
    character(Len=DioMaxStreamLen)  :: resFile

    type(DioPltType)                :: inSet

    integer :: nPar, NLoc
    character(Len=DioMaxParLen), dimension(:), pointer :: pars
    character(Len=DioMaxLocLen), dimension(:), pointer :: locs

    logical :: readRes
    real, dimension(:,:), pointer :: reals
    double precision :: julTime
    integer :: resLun, i, j, t

!   Open file for storing results

    resFile = 'TESTMap-res.txt'
    open(newunit=resLun,file=resFile)

!   Get MAP IN data set

#if (defined(HAVE_CONFIG_H))
    datasetName = '../../tstMap/w32/tstMapIn.map'
#else
    datasetName = '..\..\tstMap\w32\tstMapIn.map'
#endif

    inSet = DioPltGetDataset(datasetName)
    if ( .not. inSet % ds % inStream % opened ) then
        write (*, *) 'DID NOT GET MAP Dataset ', datasetName
    else
        write (*, *) 'GOT MAP Dataset ', datasetName

        nPar =  DioPltGetNPar(inSet); nLoc =  DioPltGetNLoc(inSet)
        pars => DioPltGetPars(inSet); locs => DioPltGetLocs(inSet)
        write(resLun,*) 'NPar: ', nPar
        do i = 1, nPar
            write(resLun,*) 'Par: ', i, ': ', trim(pars(i))
        enddo
        write(resLun,*) 'NLoc: ', nLoc
        if ( associated(locs) ) then
            do i = 1, nLoc
                write(resLun,*) 'Loc: ', i, ': ', trim(locs(i))
            enddo
        else
            write(resLun,*) 'MAP: No Locations'
        endif

        t = 1
        readRes = DioPltGet(inSet, julTime, reals)
        if ( readRes ) then
            dioTime = DioJulian2DioTime(julTime)
            write (resLun, '(A, I3, A, F17.8, A, A)') &
                     'Got Step ', t, ' JUL:', julTime, &
                    ' Time: ', trim(dioTime)
            do j = 1, nLoc
                write(resLun,*) 'Loc: ', j
                do i = 1, nPar
                    write(resLun,*) 'Par: ', i, ' val: ', reals(i,j)
                enddo
            enddo
        else
            write(resLun,*) 'MAP: Could not get Reals'
        endif

    !   destroy datasets

        call DioPltDestroy(inSet)

    !   Close file for with results
        close(resLun)

        write (*, *) 'Got Everything'
        ! write (*, *) 'Got Everything, GIVE ENTER'
        ! read(*,*)

    endif

end program read_map

