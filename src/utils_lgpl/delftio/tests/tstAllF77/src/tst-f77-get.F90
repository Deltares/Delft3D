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
subroutine getDatasets_f77get(synched, resFileName)

   implicit none

   include 'dio-f77-tst.inc'
   include 'dio-plt.inc'

   C     ! arguments

   logical       :: synched
   character*(*) :: resFileName

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
   integer, parameter :: NSETS            =  6
   integer, parameter :: DioMaxTimLen     = 25
   C     ! locals

   integer         :: resLun

   integer         :: stream(NSETS)
   integer         :: set(NSETS)
   character(100)  :: name(NSETS)

   character(DioMaxParLen) :: pars(NPARS)
   character(DioMaxLocLen) :: locs(NLOCS)
   character(DioMaxTimLen) :: tims(NTIMES)

   real*4    ::       rValues(NPARS,NLOCS)

   real*4    ::       checkRValues(NPARS,NLOCS)

   double precision :: diffValues(NPARS,NLOCS)

   integer :: nPar, nLoc, nTim

   integer :: ds, t, i, j

   C     !   Open file for results, write DioVersion

   open(newunit=resLun,file=resFileName)

   write(resLun,*) 'getDatasets_f77get: ', synched

   C     !   Initialise expected Data

   call initValues(NPARS, NLOCS, checkRValues)

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
      +                    Dio_ASCII_stream, name(ra), 'r')
      stream(rb) = DioCreateStreamSynched(
      +                    Dio_Binary_stream, name(rb), 'r')
      stream(rh) = DioCreateStreamSynched(
      +                    Dio_HIS_stream, name(rh), 'r')
   else
      stream(ra) = DioCreateStream(
      +                    Dio_ASCII_stream, name(ra), 'r')
      stream(rb) = DioCreateStream(
      +                    Dio_Binary_stream, name(rb), 'r')
      stream(rh) = DioCreateStream(
      +                    Dio_HIS_stream, name(rh), 'r')
   endif
   write (*, *) 'IN streams Created'

   C     !   Create IN data sets

   do ds = 1, NSETS
      write (*, *) 'Getting Dataset ', name(ds)
      write (resLun, *) 'Getting Dataset ', name(ds)
      set(ds) = DioGetPltDataSetInfo(stream(ds),  name(ds),
      +                    npar, pars, nloc, locs, nTim, tims)
      write(resLun,*) 'nPar: ', nPar, ', pars:'
      write(resLun,*) pars
      write(resLun,*) 'nLoc: ', nLoc, ', locs:'
      write(resLun,*) locs
   enddo

   C     !   Get data for each timestep

   do t = 1, NTIMES

      if (DioGetPltDataSetReals(set(ra), "defaultTime",
      +                               npar, nloc, rValues)) then
      write (resLun, *) 'Got reals/ASCII for Step: ', t
      write(resLun,*) rValues
      do i = 1,NPARS
         do j = 1,NLOCS
            diffValues(i,j)=rValues(i,j)-checkRvalues(i,j)
         enddo
      enddo
      if ( diffInValues(NPARS, NLOCS, diffValues, 1.D-6) ) then
         write(resLun,*)
         +                'DIFFERENCES in reals/ASCII, Step', t, ':'
         write(resLun,*) diffValues
      endif
   else
      write(*,*) 'Did not get reals/ASCII for Step: ', t
   endif

   if (DioGetPltDataSetReals(set(rb), "defaultTime",
   +                               npar, nloc, rValues)) then
   write (resLun, *) 'Got reals/Binary for Step: ', t
   write(resLun,*) rValues
   do i = 1,NPARS
      do j = 1,NLOCS
         diffValues(i,j)=rValues(i,j)-checkRvalues(i,j)
      enddo
   enddo
   if ( diffInValues(NPARS, NLOCS, diffValues, 1.D-6) ) then
      write(resLun,*)
      +                'DIFFERENCES in reals/Binary, Step', t, ':'
      write(resLun,*) diffValues
   endif
else
   write(*,*) 'Did not get reals/Binary for Step: ', t
endif

if (DioGetPltDataSetReals(set(rh), "defaultTime",
+                               npar, nloc, rValues)) then
write (resLun, *) 'Got reals/His for Step: ', t
write(resLun,*) rValues
do i = 1,NPARS
   do j = 1,NLOCS
      diffValues(i,j)=rValues(i,j)-checkRvalues(i,j)
   enddo
enddo
if ( diffInValues(NPARS, NLOCS, diffValues, 1.D-6) ) then
   write(resLun,*)
   +                'DIFFERENCES in reals/His, Step', t, ':'
   write(resLun,*) diffValues
endif
else
write(*,*) 'Did not get reals/His for Step: ', t
endif

call incrementValues(NPARS, NLOCS, checkRValues)

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

close(resLun)

end


program test_get_dio_f90

   include 'dio-f77-tst.inc'

   C     ! initialise Dio

   call DiofInit
   call getDatasets_f77get(.true., 'TESTF77Synch-res.txt')
   call getDatasets_f77get(.false., 'TESTF77Serial-res.txt')

end

