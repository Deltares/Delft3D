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
program tst_his_get_selection

    use Dio_Plt_Rw

    implicit none

    character(Len=DioMaxTimLen), external :: DioJulian2DioTime

    ! arguments

    character(Len=100) :: resFileName = 'TESTHisSelection-res.txt'
    character(Len=100) :: datasetName = 'tstHisSelection.his'

    ! TDR real * 4, dimension(:,:), pointer       :: readValues
    ! TDR double precision                        :: jul, julInSet, prevJul, diffJul
    integer                                 :: nPar, nLoc, nTimes

    double precision, dimension(:), pointer :: tims
    character(Len=DioMaxParLen), dimension(:), pointer :: pars
    character(Len=DioMaxLocLen), dimension(:), pointer :: locs
    
    type(DioPltType)   :: hisInSet

    integer            :: resLun, i,j,t

    integer, parameter  :: nSel1_Pars=3, nSel1_Locs=3, nSel1_Tims=7
    integer, dimension(nSel1_Pars) :: sel1_Pars
    integer, dimension(nSel1_Locs) :: sel1_Locs
    integer, dimension(nSel1_Tims) :: sel1_Tims
    real, dimension(nSel1_Pars,nSel1_Locs,nSel1_Tims) :: sel1_Values

    integer, parameter  :: nSel2_Pars=1, nSel2_Locs=2, nSel2_Tims=4
    integer, dimension(nSel2_Pars) :: sel2_Pars
    integer, dimension(nSel2_Locs) :: sel2_Locs
    integer, dimension(nSel2_Tims) :: sel2_Tims
    real, dimension(nSel2_Pars,nSel2_Locs,nSel2_Tims) :: sel2_Values

    integer, parameter  :: nSel3_Pars=4, nSel3_Locs=1, nSel3_Tims=50
    integer, dimension(nSel3_Pars) :: sel3_Pars
    integer, dimension(nSel3_Locs) :: sel3_Locs
    integer, dimension(nSel3_Tims) :: sel3_Tims
    real, dimension(nSel3_Pars,nSel3_Locs,nSel3_Tims) :: sel3_Values

    real, dimension(:,:), allocatable :: selSlice_Values

    data sel1_Pars / 1, 3, 5 /
    data sel1_Locs / 1, 2, 3 /
    data sel1_Tims / 2, 7, 8, 9, 48, 49, 50 /

    data sel2_Pars / 5 /
    data sel2_Locs / 1, 3 /
    data sel2_Tims / 1, 21, 22, 23 /

    data sel3_Pars / 2, 3, 4, 5 /
    data sel3_Locs / 2 /
    do i = 1, nSel3_Tims
        sel3_Tims(i) = i
    enddo


!   Open file for storing results

    open(newunit=resLun,file=resFileName)

!   Open data stream

!   Get HIS IN data set

    write (*, *)
    write (*, *) 'Getting IN Dataset'


    hisInSet = DioPltGetDataset(datasetName)
    write (resLun, *) 'GOT IN Dataset'

    nPar = DioPltGetNPar(hisInSet)
    write (resLun, *) 'nPar: ', nPar

    pars => DioPltGetPars(hisInSet)
    write (resLun, *) 'Pars:'
    do i = 1, nPar
        write (resLun, '(I3,''  '',A)') i, trim(pars(i))
    end do

    nLoc = DioPltGetNLoc(hisInSet)
    write (resLun, *) 'nLoc: ', nLoc

    locs => DioPltGetLocs(hisInSet)
    write (resLun, *) 'Locs:'
    do i = 1, nLoc
        write (resLun, '(I3,''  '',A)') i, trim(locs(i))
    end do

    nTimes = DioPltGetNTimes(hisInSet)
    write (resLun, *) 'nTimes: ', nTimes

    tims => DioPltGetTimes(hisInSet)
    write (resLun, *) 'Times:'
    do i = 1, nTimes
        write (resLun, *) tims(i), '  ', tims(i), '  ', trim(DioJulian2DioTime(tims(i)))
    end do


!   Get Selections

    if (DioPltGetSelection(hisInSet, nSel1_Pars, sel1_Pars, &
                                          nSel1_Locs, sel1_Locs,&
                                          nSel1_Tims, sel1_Tims,&
                                          sel1_Values)              ) then
        write(resLun, *) 'Got selection 1:'
        do t = 1, nSel1_Tims
            do j = 1, nSel1_Locs
                do i = 1, nSel1_Pars
                    write(resLun, *) sel1_Tims(t), sel1_Locs(j), sel1_Pars(i), &
                                    ':', sel1_Values(i, j, t)
                enddo
            enddo
        enddo
    endif


    if (DioPltGetSelection(hisInSet, nSel2_Pars, sel2_Pars, &
                                          nSel2_Locs, sel2_Locs,&
                                          nSel2_Tims, sel2_Tims,&
                                          sel2_Values)              ) then
        write(resLun, *) 'Got selection 2:'
        do t = 1, nSel2_Tims
            do j = 1, nSel2_Locs
                do i = 1, nSel2_Pars
                    write(resLun, *) sel2_Tims(t), sel2_Locs(j), sel2_Pars(i), &
                                    ':', sel2_Values(i, j, t)
                enddo
            enddo
        enddo
    endif


    if (DioPltGetSelection(hisInSet, nSel3_Pars, sel3_Pars, &
                                          nSel3_Locs, sel3_Locs,&
                                          nSel3_Tims, sel3_Tims,&
                                          sel3_Values)              ) then
        write(resLun, *) 'Got selection 3:'
        do t = 1, nSel3_Tims
            do j = 1, nSel3_Locs
                do i = 1, nSel3_Pars
                    write(resLun, *) sel3_Tims(t), sel3_Locs(j), sel3_Pars(i), &
                                    ':', sel3_Values(i, j, t)
                enddo
            enddo
        enddo
    endif


!   Get Headers again

    nPar = DioPltGetNPar(hisInSet)
    write (resLun, *) 'nPar: ', nPar

    pars => DioPltGetPars(hisInSet)
    if ( associated(pars) ) then
        write (resLun, *) 'Pars:'
        do i = 1, nPar
            write (resLun, '(I3,''  '',A)') i, trim(pars(i))
        end do
    else
        write (resLun, *) 'Pars: NULL'
    endif

    nLoc = DioPltGetNLoc(hisInSet)
    write (resLun, *) 'nLoc: ', nLoc

    locs => DioPltGetLocs(hisInSet)
    if ( associated(locs) ) then
        write (resLun, *) 'Locs:'
        do i = 1, nLoc
            write (resLun, '(I3,''  '',A)') i, trim(locs(i))
        end do
    else
        write (resLun, *) 'Locs: NULL'
    endif

!   get slice for time step 7
    allocate(selSlice_Values(nPar, nLoc))
    if (DioPltGetSelection(hisInSet, 7, selSlice_Values) ) then
        write(resLun, *) 'Got Slice selection:'
        do j = 1, nLoc
            do i = 1, nPar
                write(resLun, *) j, i, ':', selSlice_Values(i, j)
            enddo
        enddo
    endif
    deallocate(selSlice_Values)

!   destroy dataset

    call DioPltDestroy(hisInSet)

    write (*, *) 'Have destroyed HIS IN set'

!   Close file for with results
    close(resLun)

end program tst_his_get_selection

