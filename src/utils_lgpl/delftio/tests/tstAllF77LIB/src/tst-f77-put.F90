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
subroutine putf77(name, mapFile, doRunId, doInts,&
&doParDescr, doLocDescr  )

   implicit none

   include 'dio-f77-tst.inc'
   include 'dio-plt.inc'

!     ! arguments

   character*(*) name
   logical       mapFile, doRunId, doInts, doParDescr, doLocDescr

!     ! locals

   integer            set

   character*(DioMaxTimLen) dioTime
   character*(DioMaxParLen) pars(NPARS)
   character*(DioMaxLocLen) locs(NLOCS)
   integer                  intIds(NLOCS)
   character(DioMaxDescLen) parDescr(NPARS)
   character(DioMaxDescLen) locDescr(NLOCS)

   character*(40) runId(4)

   data runId / 'testHisF77', 'check if F77 interface works',&
   &'putf77',&
   &'T0: 2002.01.01 00:00:00  (scu=      60s)' /

   real*4           rValues(NPARS,NLOCS)

   integer i
   double precision t, tStart

   data pars   / 'Aa', 'Bb', 'Cc', 'Dd', 'Ee' /
   data locs   / '11', '22', '33' /
   data intIds /  111,  222,  333 /
   data parDescr  / 'Aa-aa-aa', '', '', 'Dd-dd-dd', 'Ee-ee-ee' /
   data locDescr  / '11-11-11', '' , '33-33-33'/


!     !   Initialise expected Data

   call initValues(NPARS, NLOCS, rValues)

!     !   Create IN data sets

   dioTime = '1972/01/01;00:00:00.00'

   tStart = DiofTimeString2Julian(dioTime)

   set = 0
   if ( mapFile ) then
      if ( .not. doInts ) then
         if ( doRunId ) then
            write (*, *) 'Putting MAP RID: ', name
            set = DiofPltDefine_14(name, runId,&
            &Dio_PLT_Real, NPARS, pars, NLOCS, tStart)
         else
            write (*, *) 'Putting MAP:', name
            set = DiofPltDefine_12(name,&
            &Dio_PLT_Real, NPARS, pars, NLOCS, tStart)
         endif
      endif
   else
      if ( doRunId ) then
         if ( doInts ) then
            write (*, *) 'Putting HIS, RID / INTID: ', name
            set = DiofPltDefine_5(name, runId, Dio_PLT_Real,&
            &NPARS, pars, NLOCS, intIds, locs, tStart)
         else
            write (*, *) 'Putting HIS, RID: ', name
            set = DiofPltDefine_4(name, runId, Dio_PLT_Real,&
            &NPARS, pars, NLOCS, locs, tStart)
         endif
      else
         if ( .not. doInts ) then
            write (*, *) 'Putting HIS Dataset ', name
            set = DiofPltDefine_2(name, Dio_PLT_Real,&
            &NPARS, pars, NLOCS, locs, tStart)
         endif
      endif
   endif
   if ( set .eq. 0 ) then
      write (*, *) 'DID / Could not Define ', name
   else

!     !   Put descriptions if required
      if ( doParDescr ) then
         call DiofPltAddDescriptions(set, dio_plt_pars,&
         &NPARS, parDescr)
      endif

      if ( doLocDescr ) then
         call DiofPltAddDescriptions(set, dio_plt_locs,&
         &NLOCS, locDescr)
      endif

!     !   Put data for each timestep

      do i = 1, NTIMES
         if ( set .ne. 0 ) then
            t = tStart + (i-1) * 2.0D+00
            call DiofPltPutFloats(set, t,&
            &NPARS, NLOCS, rValues)
         endif
         call incrementValues(NPARS, NLOCS, rValues)
      enddo

   endif

!     !   cleanup

   if ( set .ne. 0 ) then
      call DiofPltClose(set)
      write (*, *) 'Have destroyed IN dataset'
   endif

end


subroutine getheaderf77(name, resLun)

   implicit none

   include 'dio-f77-tst.inc'
   include 'dio-plt.inc'

!     ! arguments (dataset name, result file handle)
   character*(*) name
   integer resLun

!     ! locals
   character*(40) headerLine
   integer i
#if (defined(HAVE_CONFIG_H))
   character*(DioMaxTimLen) dioTime
#endif

!     ! PLT Set and pars / locs
   integer                  set
   integer                  nPar, nLoc
   character*(DioMaxParLen) pars(NPARS)
   character*(DioMaxLocLen) locs(NLOCS)
   character*(DioMaxDescLen)parDescr(NPARS)
   character*(DioMaxDescLen)locDescr(NLOCS)
   integer                  IDs(NLOCS)
   integer                  nTim
   real*8                   tims(NTIMES)

!     !   Get IN data set

   write (resLun, '(A,A)') 'Getting Dataset ', name
   set = DiofPltGetDataSet(name)
   if ( set .eq. 0 ) then
      write(resLun,'(A,A)') 'Could not get Set:', name
   else
      write(resLun,'(A)') 'HEADER LINES:'
      do i = 1,4
         if ( DiofPltGetHeaderLine(set, i, headerLine) ) then
            write(resLun,'(I1,A,A)') i, ':', headerLine
         endif
      enddo

      nPar = DiofPltGetNPars(set)
      write(resLun,'(A,I5)') 'nPar: ', nPar
      nLoc = DiofPltGetNLocs(set)
      write(resLun,'(A,I5)') 'nLoc: ', nLoc
      if ( DiofPltGetPars(set, nPar, pars) ) then
         write(resLun,'(A)') 'pars:'
         do i = 1,nPar
#if (defined(WIN32))
            write(resLun,'(A)') trim(pars(i))
#else
            write(resLun,'(A)') pars(i)
#endif
         enddo
      endif
      if ( DiofPltGetLocs(set, nLoc, locs) ) then
         write(resLun,'(A)') 'locs:'
         do i = 1,nLoc
#if (defined(WIN32))
            write(resLun,'(A)') trim(locs(i))
#else
            write(resLun,'(A)') locs(i)
#endif
         enddo
      endif
      if ( DiofPltGetDescriptions(set, dio_plt_pars,&
      &nPar, parDescr) ) then
         write(resLun,'(A)') 'parDescr.s:'
         do i = 1,nPar
#if (defined(WIN32))
            write(resLun,'(A)') trim(parDescr(i))
#else
            write(resLun,'(A)') pars(i)
#endif
         enddo
      endif
      if ( DiofPltGetDescriptions(set, dio_plt_locs,&
      &nLoc, locDescr) ) then
         write(resLun,'(A)') 'locDescr.s:'
         do i = 1,nLoc
#if (defined(WIN32))
            write(resLun,'(A)') trim(locDescr(i))
#else
            write(resLun,'(A)') locs(i)
#endif
         enddo
      endif
      if ( DiofPltGetIntIds(set, nLoc, IDs) ) then
         write(resLun,'(A)') 'IDs:'
         do i = 1,nLoc
            write(resLun,'(I5)') IDs(i)
         enddo
      endif

#if (defined(WIN32))
      nTim = DiofPltGetNTimes(set)
      if ( DiofPltGetTimes(set, nTim, tims) ) then
#if (defined(WIN32))
         write (resLun, '(A, A, F17.8, A, A)')&
         &'Time Stamp 1', ' JUL:', tims(1),&
         &' Time: ', trim(DiofJulian2DioTime(tims(1)))
#else
         dioTime=DiofJulian2DioTime(tims(1))
         write (resLun, '(A, A, F17.8, A, A)')&
         &'Time Stamp 1', ' JUL:', tims(1),&
         &' Time: ', trim(dioTime)
#endif
      endif
#endif
   endif

!     !   cleanup

   if (set .ne. 0) then
      call DiofPltClose(set)
   endif

end


program test_f77_put

   implicit none

!     Locals
   character*(100) resFileName
   integer resLun

!     Put various HIS

   call DiofInit()

   call putf77('TestReal'             , .false., .false., .false.,&
   &.false., .false.)
   call putf77('TestRealWithRunId'    , .false., .true. , .false.,&
   &.false., .false.)
   call putf77('TestRealWithIds'      , .false., .false., .true. ,&
   &.false., .false.)
   call putf77('TestRealWithRIDandIds', .false., .true. , .true. ,&
   &.false., .false.)

!     Test Discription calls

   call putf77('TestReal'             , .true. , .false., .false.,&
   &.false., .false.)
   call putf77('TestRealWithRunId'    , .true. , .true. , .false.,&
   &.false., .false.)
   call putf77('TestRealWithIds'      , .true. , .false., .true. ,&
   &.false., .false.)
   call putf77('TestRealWithRIDandIds', .true. , .true. , .true. ,&
   &.false., .false.)

   call putf77('TestParLocDescr'     , .false., .false., .false. ,&
   &.true. , .true.)
   call putf77('TestRunIDParDescr'   , .false., .true. , .false.,&
   &.true. , .false.)
   call putf77('TestParLocDescr'     , .true. , .false., .false. ,&
   &.true. , .true.)
   call putf77('TestRunIDParDescr'   , .true. , .true. , .false.,&
   &.true. , .false.)

!     !   Open file for results

   resFileName = 'TESTF77PUTLIB-res.txt'
   open(newunit=resLun,file=resFileName)

   call getheaderf77('TestReal.his'             , resLun )
   call getheaderf77('TestRealWithRunId.his'    , resLun )
   call getheaderf77('TestRealWithIds.his'      , resLun )
   call getheaderf77('TestRealWithRIDandIds.his', resLun )

   call getheaderf77('TestReal.map'             , resLun )
   call getheaderf77('TestRealWithRunId.map'    , resLun )
   call getheaderf77('TestRealWithIds.map'      , resLun )
   call getheaderf77('TestRealWithRIDandIds.map', resLun )

   call getheaderf77('TestParLocDescr.his'   , resLun )
   call getheaderf77('TestRunIDParDescr.his' , resLun )
   call getheaderf77('TestParLocDescr.map'   , resLun )
   call getheaderf77('TestRunIDParDescr.map' , resLun )

   close(resLun)

end
