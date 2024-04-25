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

    use his2csv_version_module
    use Dio_Plt_Rw

    implicit none

    integer, external :: nargs

    character(Len=256)             :: version_string ! first argument (fileName)
    integer                        :: numArgs        ! #program arguments
    integer                        :: numPreLocArgs  ! #arguments before the first location argument
    character(Len=DioMaxStreamLen) :: arg1           ! first argument (fileName)
    character(Len=DioMaxParLen)    :: arg2           ! optional second argument (-skip)
    character(Len=DioMaxParLen)    :: parName        ! second or third argument (parameter)
    character(Len=DioMaxLocLen)    :: argLocName     ! string for reading next arguments (locations)

    logical                        :: skipIterations ! skip iterations lines?
    logical, pointer, dimension(:) :: locSelected    ! flag to indicate if location is selected

    character(Len=DioMaxStreamLen) :: hisFileName    ! incoming his file (with or without ext.)
    character(Len=DioMaxStreamLen) :: outFileName    ! name(s) of resulting file(s)
    integer                        :: outFileHandle  ! out file handle

    type(DioPltType)               :: plt            ! par./loc./time dataset read from his file
    character(Len=DioMaxParLen), &
             pointer, dimension(:) :: pars           ! par. names
    character(Len=DioMaxLocLen), &
             pointer, dimension(:) :: locs           ! loc. names
    double precision, &
             pointer, dimension(:) :: times          ! pointer to julTimes
    character(Len=DioMaxParLen), &
             pointer, dimension(:) :: parsCopy       ! copy to par. names (returned pointer may get redirected)
    character(Len=DioMaxLocLen), &
             pointer, dimension(:) :: locsCopy       ! copy to loc. names (  "  )
    double precision, &
             pointer, dimension(:) :: timesCopy      ! copy tojulTimes    (  "  )

    integer                        :: nPar           ! #pars
    integer                        :: nLoc           ! #locs
    integer                        :: nTim           ! #time steps
    real, &
         pointer, dimension(:,:,:) :: allValues      ! received reals (all par/loc/time values at once)
    real, &
         pointer, dimension(:,:)   :: timeValues     ! received reals (par/loc values for one time step)

    integer                        :: i,j,par,loc,t  ! loop counters
    integer                        :: parIndex       ! index of selected parameter
    integer                        :: retval         ! return value
    integer                        :: streamType     ! delftIO stream type
    character(Len=20)              :: valueString    ! string containing value
    character(Len=DioMaxLocLen*100):: lineString     ! string containing full line
    logical                        :: endOfIter      ! last value of iteration values with same time step?
    double precision, parameter    :: timeEpsilon = 1.15740741D-08 ! epsilon for comparing julian times, millisecond

    retval = 0

    call getfullversionstring_his2csv(version_string)
    write(*,'(A)') trim(version_string)

    ! check / read arguments
    numArgs = nargs()
    if (numArgs >= 1) call GetArg(1, arg1)
    if ( numArgs == 1 .or. &
         numArgs == 2 .and.&
         ( arg1 == '/?' .or. arg1 == '-?' .or. &
           StringsEqual( CaseInsens, arg1, '-help' ) .or. &
           StringsEqual( CaseInsens, arg1, '/help' ) )    ) then
        call write_help()
        stop
    endif

    hisFileName = arg1

    numPreLocArgs = 99999
    parName = ' '

    if (numArgs > 2) call GetArg(2, arg2)
    if (arg2(1:1) == '-') then
        if (.not. StringsEqual( CaseInsens, arg2, '-skip' ) ) then
            write(*, *)
            write(*,'(2A)') 'Unknown option: ', arg2
            call write_help()
            stop
        else
            skipIterations = .true.
            if (numArgs > 3) then
                call GetArg(3, parName)
                numPreLocArgs = 3
            endif
        endif
    else
        if (numArgs > 2) then
            call GetArg(2, parName)
            numPreLocArgs = 2
        endif
    endif
    
    ! open and check the dataset
    streamType = DioDetermineStreamTypeFromName(hisFileName)
    if ( streamType == Dio_Unknown_stream) then
         streamType = Dio_His_stream
    endif
    if ( streamType /= Dio_His_stream) then
        write(*, '(A)') 'only HIS files support'
    endif
    plt = DioPltGetDataset(hisFileName)

    if ( .not. DioPltOpenedOK(plt) ) then
        write(*, '(A)') DioGetLastErrorMsg()
        retval = -1
    endif

    nPar =  DioPltGetNPar(plt)  ; pars  => DioPltGetPars(plt)
    nLoc =  DioPltGetNLoc(plt)  ; locs  => DioPltGetLocs(plt)
    nTim =  DioPltGetNTimes(plt); times => DioPltGetTimes(plt)
    allocate( parsCopy(nPar));  parsCopy = pars
    allocate( locsCopy(nLoc));  locsCopy = locs
    allocate(timesCopy(nTim));  timesCopy = times
        
    allocate(locSelected(nLoc))
    locSelected = .true.

    if (numArgs > numPreLocArgs) then
        locSelected = .false.
        do i = 1, numArgs - numPreLocArgs
            call GetArg(numPreLocArgs-1+i, argLocName) 
            do j = 1, nLoc
                if (argLocName == locsCopy(j)) then
                    locSelected(j) = .true.
                    exit
                endif
            enddo
        enddo
    endif
    
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
        
        if (numArgs>2) then
            outFileName = trim(parName)
            do i = 1, len_trim(outFileName)
                if ( outFileName(i:i) == '/' .or. outFileName(i:i) == '\') then
                    outFileName(i:i) = '_'
                endif
            enddo
            outFileName = trim(hisFileName(1:len_trim(hisFileName)-4))//'-'//trim(outFileName)//'.csv'
        else
            outFileName = trim(hisFileName(1:len_trim(hisFileName)-4))//'.csv'
        endif

        open(newunit=outFileHandle,file=outFileName)

        if (parName == ' ') then  ! all values

            allValues => DioPltGetAllReals(plt)
            do par = 1, nPar
                write(outFileHandle, '(2A)') 'parameter: ', trim(parsCopy(par))
                write(outFileHandle,'(A)') ''
                do loc = 1, nLoc
                    write(outFileHandle, '(2A)') 'Location,', trim(locsCopy(loc))
                    do t = 1, nTim
                        write(outFileHandle,'(A19,A,G15.7)') DioDsJulian2DioTime(timesCopy(t)), ',', allValues(par,loc,t)
                    enddo
                    write(outFileHandle, '(A)') ''
                enddo
            enddo

        else ! one parameter
            parIndex = -1
            do par = 1, nPar
                if (parsCopy(par)(1:len_trim(parName)) == parName) then
                    parIndex = par
                    exit
                endif
            enddo
            if (parIndex == -1) then
                write(*, '(3A)') 'Parameter ', parName, ' not found. Available parameters:'
                do par = 1, nPar
                    write(*, '    (A)') parsCopy(par)
                enddo
            else
                write(outFileHandle, '(2A)') 'parameter,', trim(parsCopy(par))
                write(lineString,'(A)') 'time'
                do loc = 1, nLoc
                    if (locSelected(loc)) then
                        lineString = trim(lineString) // ',' // trim(locsCopy(loc))
                    endif
                enddo
                write(outFileHandle, '(A)') trim(lineString)

                t = 1
                if (.not. DioPltGetReals(plt, timeValues)) then
                    write(*, '(A,I)') 'Error reading values for first time step'
                    stop
                endif
                do while(t <= nTim)
                    if (t == nTim) then
                        endOfIter = .true.
                    else
                        endOfIter = (timesCopy(t+1) > (timesCopy(t) + timeEpsilon))
                    endif
                    if (.not. skipIterations .or. skipIterations .and. endOfIter) then
                        write(lineString, '(A19)') trim(DioDsJulian2DioTime(timesCopy(t)))
                        do loc = 1, nLoc
                            if (locSelected(loc)) then
                                write(valueString,'(G15.7)') timeValues(par,loc)
                                lineString = trim(lineString) // ',' // trim(adjustl(valueString))
                            endif
                        enddo
                        write(outFileHandle, '(A)') trim(lineString)
                    endif
                    if (t < nTim) then
                        if (.not. DioPltGetReals(plt, timeValues)) then
                            write(*, '(A,I)') 'Error reading values for time step ', t
                            stop
                        endif
                    endif
                    t = t + 1
                enddo
            endif
        endif

        close(outFileHandle)

    endif

end program his2csv

subroutine write_help()
    write(*,'(A)') 'Usage: his2csv <hisfile>'
    write(*,'(A)') ' or:his2csv <hisfile> [ -skip ] [ parameter [ location [ location ... ] ] ]'
    write(*,'(A)') ' skipIter: in case of a his file with repeated time steps due to iterions'
    write(*,'(A)') '           skip the iterations and only provide the last time step'
    write(*,'(A)') ' ex.1: his2csv myResults.his'
    write(*,'(A)') ' ex.2: his2csv myResults.his "discharge [m3/s]"'
    write(*,'(A)') ' ex.3: his2csv myResults.his -skip "discharge [m3/s]"'
    write(*,'(A)') ' ex.4: his2csv myResults.his "Waterlevel" "Hoek van Holland"'
    write(*,'(A)') ' ex.5: his2csv myResults.his -skip "Waterlevel" "Hoek van Holland"'
    write(*,'(A)') ' ex.6: his2csv myResults.his "Discharge" "Pump-B" "De Blocq van Kuffeler"'
end subroutine write_help
