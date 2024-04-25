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
      subroutine putDatasets_f77put(synched)

      implicit none

      include 'dio-f77-tst.inc'
      include 'dio-plt.inc'

C     ! arguments
      logical :: synched

C     ! timeframe

    integer, parameter :: ra = 1 ! dataset/stream reals/ASCII
    integer, parameter :: da = 2 ! dataset/stream doubles/ASCII
    integer, parameter :: ia = 3 ! dataset/stream ints/ASCII
    integer, parameter :: rb = 4 ! dataset/stream reals/Binary
    integer, parameter :: db = 5 ! dataset/stream doubles/Binary
    integer, parameter :: ib = 6 ! dataset/stream ints/Binary

#if (defined(WIN32))
      integer, parameter :: NTIMES = 100
#else
      integer, parameter :: NTIMES = 10
#endif
      integer, parameter :: NSETS  =  6
C     ! locals

      integer        :: stream(NSETS)
      integer        :: set(NSETS)
      character(100) :: name(NSETS)

      character(DioMaxParLen) :: pars(NPARS)
      character(DioMaxLocLen) :: locs(NLOCS)

      real*4 :: rValues(NPARS,NLOCS)

      integer :: ds, i

      data pars   / 'Aa', 'Bb', 'Cc', 'Dd', 'Ee' /
      data locs   / '11', '22', '33' /

C     !   Initialise expected Data

      call initValues(NPARS, NLOCS, rValues)

C     !   Initialize data set names

      name(ra) = 'TESTRealASCII'
      name(rb) = 'TESTRealBinary'
      name(rh) = 'TESTRealHis.his'


      do ds = 1, NSETS
          if ( synched ) then
              name(ds) = 'sync.' // trim(name(ds))
          endif
      enddo

C       !   Create IN data streams

      if ( synched ) then
          stream(ra) = DioCreateStreamSynched(
     +                    Dio_ASCII_stream, name(ra), 'w')
          stream(rb) = DioCreateStreamSynched(
     +                    Dio_Binary_stream, name(rb), 'w')
          stream(rh) = DioCreateStreamSynched(
     +                    Dio_HIS_stream, name(rh), 'w')
      else
          stream(ra) = DioCreateStream(
     +                    Dio_ASCII_stream, name(ra), 'w')
          stream(rb) = DioCreateStream(
     +                    Dio_Binary_stream, name(rb), 'w')
          stream(rh) = DioCreateStream(
     +                    Dio_HIS_stream, name(rh), 'w')
      endif
      write (*, *) 'IN streams Created'

C     !   Create IN data sets

      do ds = 1, NSETS
          write (*, *) 'Putting Dataset ', name(ds)
          set(ds) = DioDefinePltDataSet(stream(ds),  name(ds),
     +                    Dio_PLT_Real, NPARS, pars, NLOCS, locs)
      enddo

C     !   Put data for each timestep

      do i = 1, NTIMES

          write (*, *) 'Putting Time Step', i
          call DioPutPltDataSetReals(set(ra), "defaultTime",
     +                               NPARS, NLOCS, rValues)

          call DioPutPltDataSetReals(set(rb), "defaultTime",
     +                               NPARS, NLOCS, rValues)

          call DioPutPltDataSetReals(set(rh), "defaultTime",
     +                               NPARS, NLOCS, rValues)

          call incrementValues(NPARS, NLOCS, rValues)

      enddo

C     !   cleanup

      do ds = 1, NSETS
            call DioDestroyPltDataSet(set(ds))
            write (*, *) 'Have destroyed IN dataset', ds
      enddo

      do ds = 1, NSETS
            call DioCloseStream(stream(ds))
            write (*, *) 'Have closed IN stream ', ds
      enddo

      end


      program test_put_dio_f90

      include 'dio-f77-tst.inc'

C     ! initialise Dio

      call DiofInit
      call putDatasets_f77put(.false.)
      call putDatasets_f77put(.true.)

      end

