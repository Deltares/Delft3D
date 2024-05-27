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
subroutine write_his_time

   implicit none

   include 'dio-plt.inc'

   ! locals

   character*(DioMaxStreamLen) datasetName1, datasetName2

   integer nPar, nLoc
   parameter (nPar = 7, nLoc = 4)

   character*(DioMaxParLen) pars(nPar)
   character*(DioMaxLocLen) locs(nLoc)
   real values(nPar,nLoc)

   double precision startTime, endTime1, endTime2, julTime

   integer outSet1, outSet2
   integer i,j

   character*(40) runId(4)

   character*(DioMaxTimLen) dioTime

   character*(DiofMaxErrMsgLen) errMsg
   integer errNr

   data pars /'Aa', 'Bb', 'Cc', 'Dd', 'Ee', 'Ff', 'Gg'  /

   data locs /'11', '22', '33', '44'/

   data runId / 'TimeMult End Time', 'check if endTime works',&
   &'write_his_time', 'Vierde String' /

   do i = 1, nLoc
      do j = 1, nPar
         values(j,i) = j * 0.01 + i * 0.10
      enddo
   enddo

!     Create HIS OUT datasets

   dioTime = '1972/01/01;00:00:00.00'
   startTime = DiofTimeString2Julian(dioTime)

   dioTime = '2022/01/01;00:00:00.00'
   endTime1  = DiofTimeString2Julian(dioTime)

   dioTime = '2072/01/01;00:00:00.00'
   endTime2  = DiofTimeString2Julian(dioTime)

   datasetName1 = 'TESTEndTime1.his'
   datasetName2 = 'TESTEndTime2.his'

   write (*, *) 'Creating OUT dataset'

   outSet1 = DiofPltDefine_2a(datasetName1, Dio_Plt_Real,&
   &nPar, pars, nLoc, locs,&
   &startTime, endTime1)

   outSet2 = DiofPltDefine_2a(datasetName2, Dio_Plt_Real,&
   &nPar, pars, nLoc, locs,&
   &startTime, endTime2)

!     Put HIS OUT data for each timestop

   if ( outSet1 .eq. 0 ) then
      call DiofGetLastErrorMsg(errMsg)
#if (defined(WIN32))
      write(*,*) 'DIOTESTERROR: ',&
      &DiofGetLastError(), ' ' ,trim(errMsg)
#else
      write(*,*) 'DIOTESTERROR: ',&
      &DiofGetLastError(), ' ' ,errMsg
#endif
   else
      write (*, *) 'OUT Dataset 1 Created'
      julTime = startTime
      do i = 1, 69
         call DiofPltPutFloats(outSet1, julTime,&
         &nPar, nLoc, values)
         julTime = julTime + 365.0D+0
      enddo
   endif

   if ( outSet2 .eq. 0 ) then
      call DiofGetLastErrorMsg(errMsg)
#if (defined(WIN32))
      write(*,*) 'DIOTESTERROR: ',&
      &DiofGetLastError(), ' ' ,trim(errMsg)
#else
      write(*,*) 'DIOTESTERROR: ',&
      &DiofGetLastError(), ' ' ,errMsg
#endif
   else
      write (*, *) 'OUT Dataset 2 Created'
      julTime = startTime
      do i = 1, 100
         call DiofPltPutFloats(outSet2, julTime,&
         &nPar, nLoc, values)
         julTime = julTime + 365.0D+0
      enddo
   endif

!     destroy datasets

   call DiofPltClose(outSet1)
   call DiofPltClose(outSet2)

   write (*, *) 'Have destroyed HIS OUT sets'

   return
end


subroutine read_his_time

   implicit none

   include 'dio-plt.inc'

   character*(DioMaxStreamLen) datasetName1, datasetName2
   character*(HisRunIdSize) headerLine

   integer nTim

   integer nMaxTim
   parameter (nMaxTim = 500)

   double precision tims(nMaxTim)

   integer inSet1, inSet2
   integer resLun, i, t

   character*(DiofMaxErrMsgLen) errMsg
   integer errNr

!     Open file for storing results
   open(newunit=resLun,file='TESTEndTime-res.txt')

!     Get HIS IN data set

   datasetName1 = 'TESTEndTime1.his'
   datasetName2 = 'TESTEndTime2.his'

   inSet1 = DiofPltGetDataset(datasetName1)
   inSet2 = DiofPltGetDataset(datasetName2)
   write (*, *) 'HIS IN Datasets Initialized'

   if ( inSet1 .eq. 0 ) then
      write (*, *) 'DID NOT GET HIS Dataset ', datasetName1
   else
      write(resLun,*) 'HEADER LINES:'
      do i = 1, 4
         if ( DiofPltGetHeaderLine(inSet1, i, headerLine) ) then
            write(resLun,*) i, ':', headerLine
         endif
      enddo
      nTim = DiofPltGetNTimes(inSet1)
      write(resLun,*) 'nTim: ', nTim
      if ( DiofPltGetTimes(inSet1, nTim, tims) ) then
         write(resLun,*) 'tims:'
         do t = 1, nTim
#if (defined(WIN32))
            write(resLun,*) t, ':', tims(t), ' : ',&
            &trim(DiofJulian2DioTime(tims(t)))
#else
            write(resLun,*) t, ':', tims(t), ' : ',&
            &DiofJulian2DioTime(tims(t))
#endif
         enddo
      endif
   endif

   if ( inSet2 .eq. 0 ) then
      write (*, *) 'DID NOT GET HIS Dataset ', datasetName2
   else
      write(resLun,*) 'HEADER LINES:'
      do i = 1, 4
         if ( DiofPltGetHeaderLine(inSet2, i, headerLine) ) then
            write(resLun,*) i, ':', headerLine
         endif
      enddo
      nTim = DiofPltGetNTimes(inSet2)
      write(resLun,*) 'nTim: ', nTim
      if ( DiofPltGetTimes(inSet2, nTim, tims) ) then
         write(resLun,*) 'tims:'
         do t = 1, nTim
#if (defined(WIN32))
            write(resLun,*) t, ':', tims(t), ' : ',&
            &trim(DiofJulian2DioTime(tims(t)))
#else
            write(resLun,*) t, ':', tims(t), ' : ',&
            &DiofJulian2DioTime(tims(t))
#endif
         enddo
      endif
   endif

   call DiofPltClose(inSet1)
   call DiofPltClose(inSet2)

!     Close file for with results
   close(resLun)

   write (*, *) 'Got Everything'

   return
end


program test_his_time

   call write_his_time
   call read_his_time

end


