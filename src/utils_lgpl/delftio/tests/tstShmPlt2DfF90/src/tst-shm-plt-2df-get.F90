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
program getDatasets

    ! modules

    use Dio_Shm_Tst

    ! Result file

    logical                       :: toScreen = .False. ! write to screen?
    logical                       :: toFile   = .True.  ! write to file?

    integer, parameter            :: resLun
    character(len=100), parameter :: resFile = 'TESTShmPlt2df-res.txt'

    ! PLT datasets and their data

    type(DioPltType) , dimension(nPltSets) :: pltSet
    type(dsData)     , dimension(nPltSets), target :: pltData

    ! 2DField datasets and their data

    type(Dio2DFType) , dimension(n2DFSets) :: f2dSet
    type(dsData)     , dimension(n2DFSets), target :: f2dData

    ! time / dataset loops

    integer               :: t     ! time step counter

    integer               :: plt   ! PLT dataset counter
    integer               :: f2d   ! 2DField dataset counter

    type(dsData), pointer :: dsDat ! pointer to current dataset in loop

    ! vars for checking results of Get call

    character(Len=DioMaxParLen), &
        pointer, dimension(:)   :: pars    ! received parameters
    character(Len=DioMaxLocLen), &
         pointer, dimension(:)  :: locs    ! received locations
    integer :: par, loc                    ! parameter / location counters

    logical                     :: retVal  ! result of Get(Values) call
    real , &
        pointer, dimension(:,:) :: reals   ! received reals
    double precision, &
        pointer, dimension(:,:) :: doubles ! received doubles
    integer, &
        pointer, dimension(:,:) :: ints    ! received integers

    logical                     :: diffVal ! result of difference check


    open(newunit=resLun, file=resFile)

!
!   Create Data to be put
!
    call DioInit('dioconfig_shm.ini')

    ! #pars/locs in PLT sets, #M/N in 2DField datasets, var. type

    call InitPLTData(pltData)
    call Init2DFData(f2dData)
    
    ! Initialise values

    do plt = 1, nPltsets
        call initValues(pltData(plt))
    enddo
    do f2d = 1, n2DFSets
        call initValues(f2dData(f2d))
    enddo

!
!   Get IN PLT datasets, compare expected sizes and par/loc names
!
    do plt = 1, nPltSets
        dsDat => pltData(plt)
        write(resLun,*) 'Getting IN PLT dataset ', trim(dsDat % Name)
        if (toScreen) write(*,*) 'Getting IN PLT dataset ', trim(dsDat % Name)

        pltSet(plt) = DioPltGetDataset(dsDat % Name)
        
        if (dsDat % numM .ne. DioPLTGetNPar(pltSet(plt)) ) then
            write (resLun, *) 'Size does not match for PLT: ', trim(dsDat % Name)
        else
            pars => DioPLTGetPars(pltSet(plt))
            do par = 1 , dsDat % numM
                if (dsDat % pars(par) .ne. pars(par) ) then
                    write (resLun, *) 'par ', par, ' does not match for PLT: ', trim(dsDat % name)
                endif
            enddo
        endif
        
        if (dsDat % numN .ne. DioPLTGetNLoc(pltSet(plt)) ) then
            write (resLun, *) 'Size does not match for PLT: ', trim(dsDat % name)
        else
            locs => DioPLTGetLocs(pltSet(plt))
            do loc = 1 , dsDat % numN
                if (dsDat % locs(loc) .ne. locs(loc) ) then
                    write (resLun, *) 'loc ', loc, ' does not match for PLT: ', trim(dsDat % name)
                endif
            enddo
        endif
    enddo

!
!   Get IN 2DF datasets, compare expected sizes
!
    do f2d = 1, n2DFSets
        dsDat => f2dData(f2d)
        write(resLun,*) 'Getting IN 2DF dataset ', trim(dsDat % Name)
        if (toScreen) write(*,*) 'Getting IN 2DF dataset ', trim(dsDat % Name)

        f2dSet(f2d) = Dio2DFGetDataset(dsDat % Name)
        if (dsDat % numM .ne. Dio2DFGetNumM(f2dSet(f2d)) ) then
            write (resLun, *) 'Size does not match for 2DF: ', trim(dsDat % name)
        endif
        
        if (dsDat % numN .ne. Dio2DFGetNumN(f2dSet(f2d)) ) then
            write (resLun, *) 'Size does not match for 2DF: ', trim(dsDat % name)
        endif
    enddo

    write (resLun, *) 'IN Datasets Created'

!
!   Get data for each timestop
!
    do t = 1, NTIMES

        ! Get PLT dataset values,
        ! Dump on selecte time steps,
        ! Compare received and expected results.

        do plt = 1, nPltSets
            diffVal = .true.
            dsDat => pltData(plt)
            call incrementValues(dsDat)

            select case ( dsDat % varType )
                case (Dio_Plt_Real)
                    retVal = DioPltGet(pltSet(plt), reals)
                    if (retVal) diffVal = diffInValues(dsDat, reals, 1.D-7)
                case (Dio_Plt_Double)
                    retVal = DioPltGet(pltSet(plt), doubles)
                    if (retVal) diffVal = diffInValues(dsDat, doubles, 1.D-7)
                case (Dio_Plt_Integer)
                    retVal = DioPltGet(pltSet(plt), ints)
                    if (retVal) diffVal = diffInValues(dsDat, ints, 1.D-20)
            end select
            
            if (retVal) then
                if (toFile) write (resLun, *) 'Got data for Step: ', t, ' Plt: ', trim(dsDat % name)
                if (toScreen) write (*, *) 'Got data for Step: ', t, ' Plt: ', trim(dsDat % name)

                if ( t==1 .or. t==int((NTIMES + 1)/2) .or. t==NTIMES ) then
                    select case ( dsDat % varType )
                        case (Dio_Plt_Real) ; if (toFile) write (resLun, *) reals
                        case (Dio_Plt_Double) ; if (toFile) write (resLun, *) doubles
                        case (Dio_Plt_Integer) ; if (toFile) write (resLun, *) ints
                    end select
                endif
            else
                write (resLun, *) 'NO  data for Step: ', t, ' Plt: ', trim(dsDat % name)
            endif

            if ( diffVal ) then
                write(resLun,*) 'DIFF in PLT ', trim(dsDat % name), ' Step', t
            endif

        enddo

        ! Get 2DF dataset values,
        ! Dump on selecte time steps,
        ! Compare received and expected results.

        do f2d = 1, n2DFSets
            diffVal = .true.
            dsDat => f2dData(f2d)
            call incrementValues(dsDat)

            select case ( dsDat % varType )
                case (Dio_Plt_Real)
                    retVal = Dio2DFGet(f2dSet(f2d), reals)
                    if (retVal) diffVal = diffInValues(dsDat, reals, 1.D-6)
                case (Dio_Plt_Double)
                    retVal = Dio2DFGet(f2dSet(f2d), doubles)
                    if (retVal) diffVal = diffInValues(dsDat, doubles, 1.D-6)
                case (Dio_Plt_Integer)
                    retVal = Dio2DFGet(f2dSet(f2d), ints)
                    if (retVal) diffVal = diffInValues(dsDat, ints, 1.D-6)
            end select

            if (retVal) then
                if (toFile) write (resLun, *) 'Got data for Step: ', t, ' 2DF: ', trim(dsDat % name)
                if (toScreen) write (*, *) 'Got data for Step: ', t, ' 2DF: ', trim(dsDat % name)
                if ( t==1 .or. t==int((NTIMES + 1)/2) .or. t==NTIMES ) then
                    select case ( dsDat % varType )
                        case (Dio_Plt_Real) ; if (toFile) write (resLun, *) reals
                        case (Dio_Plt_Double) ; if (toFile) write (resLun, *) doubles
                        case (Dio_Plt_Integer) ; if (toFile) write (resLun, *) ints
                    end select
                endif
            else
                write (resLun, *) 'NO  data for Step: ', t, ' 2DF: ', trim(dsDat % name)
            endif
            
            if ( diffVal ) then
                write(resLun,*) 'DIFF in 2DF ', trim(dsDat % name), ' Step', t
            endif
        enddo

        if (toFile) write (resLun, *) 'Data has been got for Step: ', t

    enddo

!   cleanup PLT and 2DF datasets

    do plt = 1, nPltSets
        call DioPltDestroy(pltSet(plt))
        call DestroyData(pltData(plt))
        write (resLun, *) 'Have destroyed IN PLT dataset', plt
    enddo

    do f2d = 1, n2DFSets
        call Dio2DFDestroy(f2dSet(f2d))
        call DestroyData(f2dData(f2d))
        write (resLun, *) 'Have destroyed IN 2DF dataset', f2d
    enddo

    close(resLun)

end

