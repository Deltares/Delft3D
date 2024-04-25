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
program test_put_dio_f90

    use Dio_Plt_Rw

    integer, parameter :: nPar = 5
    integer, parameter :: nLoc = 3
    integer, parameter :: nParDble = 7
    integer, parameter :: nLocDble = 9

    character(10), dimension(nPar)                :: vars
    character(10), dimension(nLoc)                :: locs
    character(10), dimension(nParDble)            :: varsDble
    character(10), dimension(nLocDble)            :: locsDble
    real,     dimension(nPar,nLoc)                :: values
    double precision, dimension(nParDble,nLocDble):: dbleValues
    integer, dimension(nPar,nLoc)                 :: intValues
    integer                                       :: i

    type(DioStreamType) :: outStream
    type(DioPltType)  :: outSet
    type(DioPltType)  :: outSetDble

    data vars   / 'Aa', 'Bb', 'Cc', 'Dd', 'Ee' /
    data locs   / '11', '22', '33' /

    data varsDble   / 'AAA', 'BBB', 'CCC', 'DDD', 'EEE', 'FFF', 'GGG' /
    data locsDble   / '111', '222', '333', '444', '555', '666', '777', '888', '999' /

    data values / 0.11, 0.12, 0.13, 0.14, 0.15, &
                  0.21, 0.22, 0.23, 0.24, 0.25, &
                  0.31, 0.32, 0.33, 0.34, 0.35  &
                /

    data dbleValues / 11.D+0, 12.D+0, 13.D+0, 14.D+0, 15.D+0, 16.D+0, 17.D+0, &
                      21.D+0, 22.D+0, 23.D+0, 24.D+0, 25.D+0, 26.D+0, 27.D+0, &
                      31.D+0, 32.D+0, 33.D+0, 34.D+0, 35.D+0, 36.D+0, 37.D+0, &
                      41.D+0, 42.D+0, 43.D+0, 44.D+0, 45.D+0, 46.D+0, 47.D+0, &
                      51.D+0, 52.D+0, 53.D+0, 54.D+0, 55.D+0, 56.D+0, 57.D+0, &
                      61.D+0, 62.D+0, 63.D+0, 64.D+0, 65.D+0, 66.D+0, 67.D+0, &
                      71.D+0, 72.D+0, 73.D+0, 74.D+0, 75.D+0, 76.D+0, 77.D+0, &
                      81.D+0, 82.D+0, 83.D+0, 84.D+0, 85.D+0, 86.D+0, 87.D+0, &
                      91.D+0, 92.D+0, 93.D+0, 94.D+0, 95.D+0, 96.D+0, 97.D+0  &
                    /

    data intValues / 11, 12, 13, 14, 15, &
                     21, 22, 23, 24, 25, &
                     31, 32, 33, 34, 35  &
                   /

!   Initialize dio library
    call DioInit

!   Open data stream

    outStream = DioStreamCreateSynched(dio_Nefis_stream, 'TESTNefis', 'w', .false.)

    write (*, *) 'OUT Streams Created'
    ! write (*, *) 'OUT Streams Created, GIVE ENTER'
    ! read (*, *)

!   Create OUT data set

    outSet = DioPltDefine(outStream, 'R1', &
                 Dio_Plt_Real, vars, locs)
    outSetDble = DioPltDefine(outStream, 'DP1', &
                 Dio_Plt_Double, varsDble, locsDble)

    write (*, *) 'OUT Datasets Created'
    ! write (*, *) 'OUT Datasets Created, GIVE ENTER'
    ! read (*, *)

!   Put OUT data for each timestop

    do i = 1, 200
        call DioPltPut (outSet,values)
        call DioPltPut (outSetDble,dbleValues)
        values = values + 0.1
        dbleValues = dbleValues + 0.1D+0
        write (*, *) 'Reals have been put for Step: ', i
        ! read (*, *)
    enddo

!   close data stream

    call DioStreamClose(outStream)

    write (*, *) 'Have closed OUT stream'
    ! write (*, *) 'Have closed OUT stream, GIVE ENTER'
    ! read (*, *)
end

