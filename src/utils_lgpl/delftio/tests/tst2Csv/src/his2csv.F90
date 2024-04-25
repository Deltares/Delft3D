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

program his2csv

    use Dio_Plt_Rw

    implicit none

    integer, external :: nargs

    integer                        :: numArgs        ! #program arguments
    character(Len=DioMaxStreamLen), &
                      dimension(3) :: args           ! program arguments

    character(Len=DioMaxStreamLen) :: hisFileName    ! incoming his file (with or without ext.)
    character(Len=DioMaxStreamLen) :: outFileName    ! name(s) of resulting file(s)
    integer                        :: outFileHandle  ! out file handle

    type(DioPltType)               :: plt            ! par./loc./time dataset read from his file
    character(Len=DioMaxParLen), &
             pointer, dimension(:) :: pars           ! par. names
    character(Len=DioMaxParLen), &
             pointer, dimension(:) :: locs           ! loc. names
    double precision, &
             pointer, dimension(:) :: times          ! pointer to julTimes
    character(Len=DioMaxParLen), &
             pointer, dimension(:) :: parsCopy       ! par. names
    double precision, &
             pointer, dimension(:) :: timesCopy      ! pointer to julTimes
    character(Len=DioMaxParLen), &
             pointer, dimension(:) :: locsCopy       ! loc. names
    integer                        :: nPar           ! #pars
    integer                        :: nLoc           ! #locs
    integer                        :: nTim           ! #time steps
    real, &
         pointer, dimension(:,:,:) :: rValues        ! received reals

    integer                        :: par,loc,t      ! loop counters
    integer                        :: retval         ! return value
    integer                        :: streamType     ! delftIO stream type
    character(Len=20)              :: valueString    ! string containing value

    retval = 0

    ! check / read arguments
    numArgs = COMMAND_ARGUMENT_COUNT()
    args(:) = ''
    if (numArgs >= 1) call GetArg(1, args(1))
    if ( numArgs == 1 .or. &
         numArgs == 2 .and.&
         ( args(1) == '/?' .or. args(1) == '-?' .or. &
           StringsEqual( CaseInsens, args(1), '-help' ) .or. &
           StringsEqual( CaseInsens, args(1), '/help' ) )    ) then
        write(*,'(A)') 'Usage: his2csv <hisfile> [ specific parameter [ specific location ] ]'
        write(*,'(A)') ' ex.1: his2csv myResults     or    his2csv myResults.his'
        write(*,'(A)') ' ex.2: his2csv myResults.his "discharge [m3/s]"'
        write(*,'(A)') ' ex.3: his2csv myResults "water level" "hoek van holland"'
        stop
    endif

    if (numArgs >= 2) call GetArg(2, args(2))
    if (numArgs >= 3) call GetArg(2, args(2))
    
    ! open and check the dataset
    streamType = DioDetermineStreamTypeFromName(args(1))
    if ( streamType == Dio_Unknown_stream) then
         streamType = Dio_Unknown_stream
    endif
    if ( streamType /= Dio_His_stream) then
        write(*, '(A)') 'only HIS files support'
    endif
    hisFileName = args(1) 
    plt = DioPltGetDataset(hisFileName)

    if ( .not. DioPltOpenedOK(plt) ) then
        write(*, '(A)') DioGetLastErrorMsg()
        retval = -1
    endif

    nPar =  DioPltGetNPar(plt)  ; pars  => DioPltGetPars(plt)
    nLoc =  DioPltGetNLoc(plt)  ; locs  => DioPltGetLocs(plt)
    nTim =  DioPltGetNTimes(plt); times => DioPltGetTimes(plt)
    allocate( parsCopy(nPar));  parsCopy = pars
    allocate(timesCopy(nTim)); timesCopy = times
    allocate( locsCopy(nLoc));  locsCopy = locs
        
    if (nPar <= 0 ) then
        write(*, '(2A)') 'No parameters available in ', hisFileName
        retval = -2
    endif
    if (nLoc <= 0 ) then
        write(*, '(2A)') 'No locations available in ', hisFileName
        retval = -2
    endif
    if (nTim <= 0 ) then
        write(*, '(2A)') 'No time steps available in ', hisFileName
        retval = -2
    endif

    if ( retVal == 0 ) then

        if (numArgs == 2) then  ! all values

            rValues => DioPltGetAllReals(plt)
            outFileName = trim(hisFileName(1:len_trim(hisFileName)-4))//'.csv'
            open(newunit=outFileHandle,file=outFileName)

            ! full his file
            do par = 1, nPar
                write(outFileHandle, '(2A)') 'parameter: ', trim(parsCopy(par))
                write(outFileHandle,'(A)') 'Time Stamp'
                do loc = 1, nLoc
                    write(outFileHandle, '(2A)') ',', trim(locsCopy(loc))
                enddo
                write(outFileHandle,'(A)') ''
                do t = 1, nTim
                    write(outFileHandle, '(A)') DioDsJulian2DioTime(timesCopy(t))
                    do loc = 1, nLoc
                        write(valueString,'(G15.7)') rValues(par,loc,t)
                        write(outFileHandle, '(2A)') ',', trim(valueString)
                    enddo
                    write(outFileHandle, '(A)') ''
                enddo
            enddo
        endif

        close(outFileHandle)

    endif


end program his2csv




