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
program DatasetsInMem

    ! modules

    use Dio_Shm_Tst

    ! Result file

    logical                       :: toScreen = .False. ! write to screen?
    logical                       :: toFile   = .True.  ! write to file?

    integer, parameter            :: resLun
    character(len=100), parameter :: resFile = 'TESTShmPlt2dfInMem-res.txt'

    ! PLT datasets and their data

    type(DioPltType) , dimension(nPltSets) :: pltInSet
    type(dsData)     , dimension(nPltSets), target :: pltInData

    type(DioPltType) , dimension(nPltSets) :: pltOutSet
    type(dsData)     , dimension(nPltSets), target :: pltOutData

    ! 2DField datasets and their data

    type(Dio2DFType) , dimension(n2DFSets) :: f2dInSet
    type(dsData)     , dimension(n2DFSets), target :: f2dInData

    type(Dio2DFType) , dimension(n2DFSets) :: f2dOutSet
    type(dsData)     , dimension(n2DFSets), target :: f2dOutData

    ! time / dataset loops

    integer               :: t     ! time step counter

    integer               :: plt   ! PLT dataset counter
    integer               :: f2d   ! 2DField dataset counter

    type(dsData), pointer :: dsDat ! pointer to current in/out dataset

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
    double precision            :: dTempVal
    integer                     :: iTempVal
    real                        :: rTempVal


    open(newunit=resLun, file=resFile)

!
!   Create Data to be put
!

    call DioInit('dioconfig_inmem.ini')

    ! #pars/locs in PLT sets, #M/N in 2DField datasets, var. type

    call InitPLTData(pltOutData)
    call InitPLTData(pltInData)
    call Init2DFData(f2dOutData)
    call Init2DFData(f2dInData)
    
    ! Initialise values

    do plt = 1, nPltsets
        call initValues(pltOutData(plt))
        call initValues(pltInData(plt))
    enddo
    do f2d = 1, n2DFSets
        call initValues(f2dOutData(f2d))
        call initValues(f2dInData(f2d))
    enddo

!
!   Create OUT PLT datasets, disturb par./loc. names
!
    do plt = 1, nPltSets
        dsDat => pltOutData(plt)
        if (plt .eq. 2 ) dsDat % pars(1) = 'WRONG PAR'
        if (plt .eq. 3 ) dsDat % locs(1) = 'WRONG LOC'
        write(*,*) 'Creating OUT PLT dataset ', dsDat % Name
        pltOutSet(plt) = DioPltDefine(dsDat % Name, dsDat % varType, &
                        dsDat % pars, dsDat % locs)
    enddo

!
!   Create 2DF datasets
!
    do f2d = 1, n2DFSets
        dsDat => f2dOutData(f2d)
        write(*,*) 'Creating OUT 2DF dataset ', dsDat % Name
        f2dOutSet(f2d) = Dio2DFDefine(dsDat % Name, dsDat % varType,  &
                                    dsDat % numM, dsDat % numN )
    enddo


!
!   Get IN PLT datasets, compare expected sizes and par/loc names
!
    do plt = 1, nPltSets
        dsDat => pltInData(plt)
        write(resLun,*) 'Getting IN PLT dataset ', trim(dsDat % Name)
        if (toScreen) write(*,*) 'Getting IN PLT dataset ', trim(dsDat % Name)

        pltInSet(plt) = DioPltGetDataset(dsDat % Name)
        
        if (dsDat % numM .ne. DioPLTGetNPar(pltInSet(plt)) ) then
            write (resLun, *) 'Size does not match for PLT: ', trim(dsDat % Name)
        else
            pars => DioPLTGetPars(pltInSet(plt))
            do par = 1 , dsDat % numM
                if (dsDat % pars(par) .ne. pars(par) ) then
                    write (resLun, *) 'par ', par, ' does not match for PLT: ', trim(dsDat % Name)
                endif
            enddo
        endif
        
        if (dsDat % numN .ne. DioPLTGetNLoc(pltInSet(plt)) ) then
            write (resLun, *) 'Size does not match for PLT: ', trim(dsDat % Name)
        else
            locs => DioPLTGetLocs(pltInSet(plt))
            do loc = 1 , dsDat % numN
                if (dsDat % locs(loc) .ne. locs(loc) ) then
                    write (resLun, *) 'loc ', loc, ' does not match for PLT: ', trim(dsDat % Name)
                endif
            enddo
        endif
    enddo

!
!   Get IN 2DFIn datasets, compare expected sizes
!
    do f2d = 1, n2DFSets
        dsDat => f2dInData(f2d)
        write(resLun,*) 'Getting IN 2DF dataset ', trim(dsDat % Name)
        if (toScreen) write(*,*) 'Getting IN 2DF dataset ', trim(dsDat % Name)

        f2dInSet(f2d) = Dio2DFGetDataset(dsDat % Name)
        if (dsDat % numM .ne. Dio2DFGetNumM(f2dInSet(f2d)) ) then
            write (resLun, *) 'Size does not match for 2DF: ', trim(dsDat % Name)
        endif
        
        if (dsDat % numN .ne. Dio2DFGetNumN(f2dInSet(f2d)) ) then
            write (resLun, *) 'Size does not match for 2DF: ', trim(dsDat % Name)
        endif
    enddo

    write (resLun, *) 'IN Datasets Created'

!
!   Get data for each timestop
!
    do t = 1, NTIMES

        ! Disturb some of the PLT values before putting them,
        ! Put PLT dataset values.

        do plt = 1, nPltSets
            dsDat => pltOutData(plt)
            call incrementValues(dsDat)

            select case ( dsDat % varType )

                case (Dio_Plt_Real)

                    if (t .eq. 7 ) then
                        rTempVal = dsDat % rValues(1,1)
                        dsDat % rValues(1,1) = 0.0
                    endif

                    call DioPltPut(pltOutSet(plt), dsDat % rValues)

                    if (t .eq. 7 ) dsDat % rValues(1,1) = rTempVal

                case (Dio_Plt_Double)

                    if (t .eq. NTIMES ) then
                        dTempVal = dsDat % dValues(1,1)
                        dsDat % dValues(1,1) = 0.0D+00
                    endif

                    call DioPltPut(pltOutSet(plt), dsDat % dValues)

                    if (t .eq. NTIMES ) dsDat % dValues(1,1) = dTempVal

                case (Dio_Plt_Integer)

                    if (t .eq. 12) then
                        iTempVal = dsDat % iValues(1,1)
                        dsDat % iValues(1,1) = 0
                    endif

                    call DioPltPut(pltOutSet(plt), dsDat % iValues)

                    if (t .eq. 12) dsDat % iValues(1,1) = iTempVal

            end select

        enddo

        ! Disturb some of the 2DF values before putting them,
        ! Put 2DF dataset values.

        do f2d = 1, n2DFSets
            dsDat => f2dOutData(f2d)
            call incrementValues(dsDat)

            select case ( dsDat % varType )
                case (Dio_Plt_Real)

                    if (t .eq. 11 ) then
                        rTempVal = dsDat % rValues(1,1)
                        dsDat % rValues(1,1) = 0.0
                    endif

                    call Dio2DFPut(f2dOutSet(f2d), dsDat % rValues)

                    if (t .eq. 11 ) dsDat % rValues(1,1) = rTempVal

                case (Dio_Plt_Double)

                    if (t .eq. NTIMES ) then
                        dTempVal = dsDat % dValues(1,1)
                        dsDat % dValues(1,1) = 0.0D+00
                    endif

                    call Dio2DFPut(f2dOutSet(f2d), dsDat % dValues)

                    if (t .eq. NTIMES ) dsDat % dValues(1,1) = dTempVal

                case (Dio_Plt_Integer)

                    if (t .eq. 9) then
                        iTempVal = dsDat % iValues(1,1)
                        dsDat % iValues(1,1) = 0
                    endif

                    call Dio2DFPut(f2dOutSet(f2d), dsDat % iValues)

                    if (t .eq. 9) dsDat % iValues(1,1) = iTempVal

            end select

        enddo

        if (mod(t,100).eq.0) write (*, *) 'Data has been put for Step: ', t

        ! Get PLT dataset values,
        ! Dump on selected time steps,
        ! Compare received and expected results.

        do plt = 1, nPltSets
            dsDat => pltInData(plt)
            call incrementValues(dsDat)

            select case ( dsDat % varType )
                case (Dio_Plt_Real)
                   retVal = DioPltGet(pltInSet(plt), reals)
                    diffVal = diffInValues(dsDat, reals, 1.D-7)
                case (Dio_Plt_Double)
                    retVal = DioPltGet(pltInSet(plt), doubles)
                    diffVal = diffInValues(dsDat, doubles, 1.D-7)
                case (Dio_Plt_Integer)
                    retVal = DioPltGet(pltInSet(plt), ints)
                    diffVal = diffInValues(dsDat, ints, 1.D-20)
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

        ! Get 2DFIn dataset values,
        ! Dump on selecte time steps,
        ! Compare received and expected results.

        do f2d = 1, n2DFSets
            dsDat => f2dInData(f2d)
            call incrementValues(dsDat)

            select case ( dsDat % varType )
                case (Dio_Plt_Real)
                    retVal = Dio2DFGet(f2dInSet(f2d), reals)
                    diffVal = diffInValues(dsDat, reals, 1.D-6)
                case (Dio_Plt_Double)
                    retVal = Dio2DFGet(f2dInSet(f2d), doubles)
                    diffVal = diffInValues(dsDat, doubles, 1.D-6)
                case (Dio_Plt_Integer)
                    retVal = Dio2DFGet(f2dInSet(f2d), ints)
                    diffVal = diffInValues(dsDat, ints, 1.D-6)
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
        call DioPltDestroy(pltInSet(plt))
        call DestroyData(pltInData(plt))
        call DioPltDestroy(pltOutSet(plt))
        call DestroyData(pltOutData(plt))
        write (resLun, *) 'Have destroyed IN PLT dataset', plt
    enddo

    do f2d = 1, n2DFSets
        call Dio2DFDestroy(f2dInSet(f2d))
        call DestroyData(f2dInData(f2d))
        call Dio2DFDestroy(f2dOutSet(f2d))
        call DestroyData(f2dOutData(f2d))
        write (resLun, *) 'Have destroyed IN 2DF dataset', f2d
    enddo

    call dio_shm_f77_dbcleanup
    close(resLun)

end

