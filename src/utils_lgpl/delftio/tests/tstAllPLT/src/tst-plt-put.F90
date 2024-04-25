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
subroutine putDatasets_pltput(synched, auto)

    use Dio_Plt_Tst

    ! arguments
    
    logical :: synched ! streams synchronized or not
    logical :: auto    ! stream types automatic or not

    ! locals

    type(DioStreamType), dimension(NSETS) :: stream  ! streams
    type(DioPltType)   , dimension(NSETS) :: set     ! datasets
    character(len=100) , dimension(NSETS) :: name    ! dataset names
    integer            , dimension(NSETS) :: varType ! dataset var type

    character(Len=DioMaxParLen), dimension(NPARS) :: pars    ! parameter names
    character(Len=DioMaxLocLen), dimension(NLOCS) :: locs    ! location names

    real*4,          dimension(NPARS,NLOCS)    :: rValues ! real values in dataset
    double precision,dimension(NPARS,NLOCS)    :: dValues ! double values in dataset
    integer,         dimension(NPARS,NLOCS)    :: iValues ! integer values in dataset

    integer :: ds ! dataset counter
    integer :: i
    integer :: numSets

    data pars   / 'Aa', 'Bb', 'Cc', 'Dd', 'Ee' /     ! parameter names
    data locs   / '11', '22', '33' /                 ! location names

!   Initialise Data to be put

    call initValues(rValues, dValues, iValues)

!   Initialize data set names

    name(ra) = 'TESTRealASCII'    
    name(da) = 'TESTDoubleASCII'  
    name(ia) = 'TESTIntASCII'     

    name(rb) =  'TESTRealBinary'  
    name(db) =  'TESTDoubleBinary'
    name(ib) =  'TESTIntBinary'   

    varType(ra) = Dio_PLT_Real
    varType(da) = Dio_PLT_Double
    varType(ia) = Dio_PLT_Integer

    varType(rb) = Dio_PLT_Real
    varType(db) = Dio_PLT_Double
    varType(ib) = Dio_PLT_Integer

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

        !   Create OUT data streams

        stream(ra) = DioStreamCreate(Dio_ASCII_stream, name(ra), 'w', synched)
        stream(da) = DioStreamCreate(Dio_ASCII_stream, name(da), 'w', synched)
        stream(ia) = DioStreamCreate(Dio_ASCII_stream, name(ia), 'w', synched)

        stream(rb) = DioStreamCreate(Dio_Binary_stream, name(rb), 'w', synched)
        stream(db) = DioStreamCreate(Dio_Binary_stream, name(db), 'w', synched)
        stream(ib) = DioStreamCreate(Dio_Binary_stream, name(ib), 'w', synched)

        write (*, *) 'OUT Streams Created'

    endif

!   Create OUT data sets

    do ds = 1, numSets
        write (*, *) 'Putting Dataset ', trim(name(ds))
        if ( auto ) then
            set(ds) = DioPltDefine(name(ds), varType(ds), pars, locs)
            write (*, *) 'OUT Dataset ds = ', ds, ' AUTO defined'
        else
            set(ds) = DioPltDefine(stream(ds), name(ds), varType(ds), pars, locs)
        endif
    enddo

    write (*, *) 'OUT Datasets Created'

!   Put data for each timestop

    do i = 1, NTIMES

        call DioPltPut(set(ra),rvalues)
        call DioPltPut(set(da),dvalues)
        call DioPltPut(set(ia),ivalues)

        if (.not. auto) then
            call DioPltPut(set(rb),rvalues)
            call DioPltPut(set(db),dvalues)
            call DioPltPut(set(ib),ivalues)
        endif

        write (*, *) 'Data has been put for Step: ', i

        call incrementValues(rValues, dValues, iValues)
    enddo

!   cleanup

    do ds = 1, numSets
        call DioPltDestroy(set(ds))
        write (*, *) 'Have destroyed OUT dataset', ds
    enddo

    if ( .not. auto ) then

    !   Close OUT data streams

        do ds = 1, numSets
            call DioStreamClose(stream(ds))
            write (*, *) 'Have closed OUT stream ', ds
        enddo

    endif

end subroutine putDatasets_pltput




program test_put_dio_f90

    use Dio_Plt_Tst

    character(100) :: dioVersion, dioIdent           ! DIO version/identification

! initialise Dio

    call DioInit
    call DioGetVersion(dioVersion)
    call DioGetIdent(dioIdent)

    write(*,*) dioVersion, dioIdent

    call putDatasets_pltput(.false., .false.) ! put non synched streams

    call putDatasets_pltput(.true., .false.)  ! put synched streams

    call DioInit('dioconfigFiles.ini')
    call putDatasets_pltput(.false., .true.) ! put non synched streams

    call DioInit('dioconfigShm.ini')
    call putDatasets_pltput(.true., .true.)  ! put synched streams

end
