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
subroutine getDatasets_pltget(synched, auto, resFileName)

    use Dio_Plt_Tst

    ! arguments
    
    logical       :: synched       ! streams synchronized or not
    logical       :: auto          ! stream types automatic or not
    character*(*) :: resFileName   ! name of result file

    ! locals

    integer                   :: resLun        ! handle to result file
    character(100)            :: dioVersion, dioIdent           ! DIO version/identification

    type(DioStreamType), dimension(NSETS) :: stream ! streams
    type(DioPltType)   , dimension(NSETS) :: set    ! datasets
    character(len=100) , dimension(NSETS) :: name   ! dataset names

    character(Len=DioMaxParLen), pointer, dimension(:) :: pars ! par. names
    character(Len=DioMaxLocLen), pointer, dimension(:) :: locs ! loc. names
    integer                                            :: nPar ! #pars
    integer                                            :: nLoc ! #locs

    real*4,          pointer, dimension(:,:) :: rValues ! received reals
    double precision,pointer, dimension(:,:) :: dValues ! received doubles
    integer,         pointer, dimension(:,:) :: iValues ! received integers

    real*4,          dimension(NPARS,NLOCS) :: checkRValues ! expected reals
    double precision,dimension(NPARS,NLOCS) :: checkDValues ! expected dbles
    integer,         dimension(NPARS,NLOCS) :: checkIValues ! expected ints

    double precision,dimension(NPARS,NLOCS) :: diffValues   ! diff in values

    integer                   :: ds      ! dataset counter
    integer                   :: numSets ! actual #sets (3 for Auto)
    integer                   :: i       ! par./loc. counter

!   Open file for results, write DioVersion

    open(newunit=resLun,file=resFileName)
    call DioGetVersion(dioVersion)
    call DioGetIdent(dioIdent)
    write(resLun,*) 'dioVersion: ', trim(dioVersion)
    write(resLun,*) 'dioIdent: ', trim(dioIdent)
    write(resLun,*) 'getDatasets_pltget: ', synched, auto

!   Initialise expected Data

    call initValues(checkRValues, checkDValues, checkIValues)

!   Initialize data set names

#if (defined(WIN32))
    name(ra) = 'testrealascii'
    name(da) = 'testdoubleascii'
    name(ia) = 'testintascii'

    name(rb) =  'testrealbinary'
    name(db) =  'testdoublebinary'
    name(ib) =  'testintbinary'
#else
    name(ra) = 'TESTRealASCII'
    name(da) = 'TESTDoubleASCII'
    name(ia) = 'TESTIntASCII'

    name(rb) =  'TESTRealBinary'
    name(db) =  'TESTDoubleBinary'
    name(ib) =  'TESTIntBinary'
#endif

    do ds = 1, NSETS
        if ( synched ) then
            name(ds) = trim(name(ds)) // '.sync'
        endif
        if ( auto ) then
            name(ds) = trim(name(ds)) // '.auto'
        endif
    enddo

    if ( auto ) then
        numSets = NSETS/2
    else

        numSets = NSETS

        !   Create IN data streams

        stream(ra) = DioStreamCreate(Dio_ASCII_stream, name(ra), 'r', synched)
        stream(da) = DioStreamCreate(Dio_ASCII_stream, name(da), 'r', synched)
        stream(ia) = DioStreamCreate(Dio_ASCII_stream, name(ia), 'r', synched)

        stream(rb) = DioStreamCreate(Dio_Binary_stream, name(rb), 'r', synched)
        stream(db) = DioStreamCreate(Dio_Binary_stream, name(db), 'r', synched)
        stream(ib) = DioStreamCreate(Dio_Binary_stream, name(ib), 'r', synched)

        write (*, *) 'IN streams Created'

    endif
    
!   Create IN data sets

    do ds = 1, numSets
        write (*, *) 'Getting Dataset ', name(ds)
        write (resLun, *) 'Getting Dataset ', name(ds)
        if ( auto ) then
            set(ds) = DioPltGetDataset(name(ds))
        else
            set(ds) = DioPltGetDataset(stream(ds),  name(ds))
        endif
        nPar =  DioPltGetNPar(set(ds)); nLoc =  DioPltGetNLoc(set(ds))
        pars => DioPltGetPars(set(ds)); locs => DioPltGetLocs(set(ds))
        do i = 1, nPar
            write(resLun,*) 'Par: ', i, ': ', trim(pars(i))
        enddo
        do i = 1, nLoc
            write(resLun,*) 'Loc: ', i, ': ', trim(locs(i))
        enddo
    enddo

!   Get data for each timestep

    do i = 1, NTIMES

        if (DioPltGet(set(ra),rValues)) then
            write (resLun, *) 'Got reals/ASCII for Step: ', i
            write(resLun,*) rValues
            diffValues = rValues - checkRvalues
            if ( diffInValues(diffValues, 1.D-6) ) then
                write(resLun,*) 'DIFFERENCES in reals/ASCII, Step', i, ':'
                write(resLun,*) diffValues
            endif
        else
            write(*,*) 'Did not get reals/ASCII for Step: ', i
        endif

        if (DioPltGet(set(da),dValues)) then
            write (resLun, *) 'Got doubles/ASCII for Step: ', i
            write(resLun,*) dValues
            diffValues = dValues - checkDvalues
            if ( diffInValues(diffValues, 1.D-15) ) then
                write(resLun,*) 'DIFFERENCES in doubles/ASCII, Step', i, ':'
                write(resLun,*) diffValues
            endif
        else
            write(*,*) 'Did not get doubles/ASCII for Step: ', i
        endif

        if (DioPltGet(set(ia),iValues)) then
            write (resLun, *) 'Got ints/ASCII for Step: ', i
            write(resLun,*) iValues
            diffValues = iValues - checkIvalues
            if ( diffInValues(diffValues, 1.D-20) ) then
                write(resLun,*) 'DIFFERENCES in ints/ASCII, Step', i, ':'
                write(resLun,*) diffValues
            endif
        else
            write(*,*) 'Did not get ints/ASCII for Step: ', i
        endif

        if (.not. auto) then
            if (DioPltGet(set(rb),rValues)) then
                write (resLun, *) 'Got reals/Binary for Step: ', i
                write(resLun,*) rValues
                diffValues = rValues - checkRvalues
                if ( diffInValues(diffValues, 1.D-6) ) then
                    write(resLun,*) 'DIFFERENCES in reals/Binary, Step', i, ':'
                    write(resLun,*) diffValues
                endif
            else
                write(*,*) 'Did not get reals/Binary for Step: ', i
            endif

            if (DioPltGet(set(db),dValues)) then
                write (resLun, *) 'Got doubles/Binary for Step: ', i
                write(resLun,*) dValues
                diffValues = dValues - checkDvalues
                if ( diffInValues(diffValues, 1.D-20) ) then
                    write(resLun,*) 'DIFFERENCES in doubles/Binary, Step', i, ':'
                    write(resLun,*) diffValues
                endif
            else
                write(*,*) 'Did not get doubles/Binary for Step: ', i
            endif

            if (DioPltGet(set(ib),iValues)) then
                write (resLun, *) 'Got ints/Binary for Step: ', i
                write(resLun,*) iValues
                diffValues = iValues - checkIvalues
                if ( diffInValues(diffValues, 1.D-20) ) then
                    write(resLun,*) 'DIFFERENCES in ints/Binary, Step', i, ':'
                    write(resLun,*) diffValues
                endif
            else
                write(*,*) 'Did not get ints/Binary for Step: ', i
            endif
        endif

        call incrementValues(checkRValues, checkDValues, checkIValues)

    enddo

!   cleanup

    do ds = 1, numSets
        call DioPltDestroy(set(ds))
        write (*, *) 'Have destroyed IN dataset', ds
    enddo

    if ( .not. auto ) then

        !   Close IN data streams

        do ds = 1, numSets
            call DioStreamClose(stream(ds))
            write (*, *) 'Have closed IN stream ', ds
        enddo
    endif

    close(resLun)

end subroutine getDatasets_pltget


program test_get_dio_f90

    use Dio_Plt_Tst

! initialise Dio

    call DioInit
    call getDatasets_pltget(.true., .false., 'TESTPltSynch-res.txt')     ! get synched streams
    call getDatasets_pltget(.false., .false., 'TESTPltSerial-res.txt')   ! get non synched streams

    call DioInit('dioconfigShm.ini')
    call getDatasets_pltget(.true., .true., 'TESTPltAutoShm-res.txt')    ! get synched streams

    call DioInit('dioconfigFiles.ini')
    call getDatasets_pltget(.false., .true., 'TESTPltAutoFiles-res.txt') ! get non synched streams

end

