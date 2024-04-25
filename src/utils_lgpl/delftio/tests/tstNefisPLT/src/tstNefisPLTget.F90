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

    real, dimension(:,:), pointer  :: realValues
    double precision, dimension(:,:), pointer  :: dbleValues

    type(DioStreamType) :: inStream
    type(DioPltType)  :: inSet

    ! type(DioStreamType) :: inStreamDble
    type(DioPltType)  :: inSetDble

    character(Len=DioMaxParLen), pointer, dimension(:) :: pars ! par. names
    character(Len=DioMaxLocLen), pointer, dimension(:) :: locs ! loc. names
    integer                                            :: nPar ! #pars
    integer                                            :: nLoc ! #locs

    integer :: i,resLun

!   Initialize dio library
    call DioInit

!   Open file for storing results

    open(newunit=resLun,file='TESTNefisPLT-res.txt')

!   Open data stream

    write (*, *) 'Opening IN Stream'
    inStream = DioStreamCreateSynched(dio_Nefis_stream, 'TESTNefis', 'r', .false.)
    ! inStreamDble = DioStreamCreateSynched(dio_Nefis_stream, 'TESTNefisDble', 'r', .false.)
    write (*, *) 'IN Streams Created'

!   Get IN data set

    inSet = DioPltGetDataset(inStream, 'R1')
    inSetDble = DioPltGetDataset(inStream, 'DP1')

    nPar =  DioPltGetNPar(inSet); nLoc =  DioPltGetNLoc(inSet)
    pars => DioPltGetPars(inSet); locs => DioPltGetLocs(inSet)
    write(resLun,*) 'NPar inSet: ', nPar
    do i = 1, nPar
        write(resLun,*) 'Par: ', i, ': ', trim(pars(i))
    enddo
    write(resLun,*) 'NLoc inSet: ', nLoc
    do i = 1, nLoc
        write(resLun,*) 'Loc: ', i, ': ', trim(locs(i))
    enddo

    nPar =  DioPltGetNPar(inSetDble); nLoc =  DioPltGetNLoc(inSetDble)
    pars => DioPltGetPars(inSetDble); locs => DioPltGetLocs(inSetDble)
    write(resLun,*) 'NPar inSetDble: ', nPar
    do i = 1, nPar
        write(resLun,*) 'Par: ', i, ': ', trim(pars(i))
    enddo
    write(resLun,*) 'NLoc inSetDble: ', nLoc
    do i = 1, nLoc
        write(resLun,*) 'Loc: ', i, ': ', trim(locs(i))
    enddo

    write (*, *) 'IN Datasets Initialized'

!   Get Values

    do i = 1, 200
        if ( DioPltGet (inSet, realValues) ) then
            if ( i .eq. 1 .or. mod(i,10) .eq. 0 ) then
                write (resLun, *) 'Got inSet reals for Step: ', i
                write (resLun, *) realValues
            endif
        else
            write (resLun, *) 'Did NOT get inSet reals for Step: ', i
        endif
        if ( DioPltGet (inSetDble, dbleValues) ) then
            if ( i .eq. 1 .or. mod(i,10) .eq. 0 ) then
                write (resLun, *) 'Got inSetDble doubles for Step: ', i
                write (resLun, *) dbleValues
            endif
        else
            write (resLun, *) 'Did NOT get inSetDble doubles for Step: ', i
        endif
    end do
    
!   Close data stream

    call DioStreamClose(inStream)
    ! call DioStreamClose(inStreamDble)

!   Close file for with results
    close(resLun)

    write (*, *) 'Got Everything'
    ! write (*, *) 'Got Everything, GIVE ENTER'
    ! read(*,*)
end

