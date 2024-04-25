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
      program test_dll_f77_get

      implicit none

      include 'dio-f77-tst.inc'
      include 'dio-plt.inc'

C     ! arguments
      
      character*(100) resFileName

C     ! locals

      integer resLun
      integer t, i, j

      character*(DiofMaxErrMsgLen) errMsg
      character*(40) headerLine
      character*(DioMaxTimLen) dioTime
      integer errNr

C     ! PLT Set for timestep based retieval (3 identical sets)
      integer         set
      character*(100) name

      character*(DioMaxParLen) pars(NPARS)
      character*(DioMaxLocLen) locs(NLOCS)
      integer                  IDs(NLOCS)
      real*8                   tims(NTIMES)

      real*4           rValues(NPARS,NLOCS)
      real*4           checkRValues(NPARS,NLOCS)

      double precision diffValues(NPARS,NLOCS)
      double precision julTime

      integer nPar, nLoc, nTim

#if (defined(WIN32)||defined(HAVE_CONFIG_H))
C     ! Data for Selection testing

      integer    nSel1_Pars,   nSel1_Locs,   nSel1_Tims
      parameter (nSel1_Pars=3, nSel1_Locs=3, nSel1_Tims=7)
      integer sel1_Pars(nSel1_Pars)
      integer sel1_Locs(nSel1_Locs)
      integer sel1_Tims(nSel1_Tims)
      double precision sel1_Values(nSel1_Pars,nSel1_Locs,nSel1_Tims)

      integer    nSel2_Pars,   nSel2_Locs,   nSel2_Tims
      parameter (nSel2_Pars=1, nSel2_Locs=2, nSel2_Tims=4)
      integer sel2_Pars(nSel2_Pars)
      integer sel2_Locs(nSel2_Locs)
      integer sel2_Tims(nSel2_Tims)
      real    sel2_Values(nSel2_Pars,nSel2_Locs,nSel2_Tims)

      integer    nSel3_Pars,   nSel3_Locs,   nSel3_Tims
      parameter (nSel3_Pars=4, nSel3_Locs=1, nSel3_Tims=50)
      integer sel3_Pars(nSel3_Pars)
      integer sel3_Locs(nSel3_Locs)
      integer sel3_Tims(nSel3_Tims)
      real    sel3_Values(nSel3_Pars,nSel3_Locs,nSel3_Tims)

      data sel1_Pars / 1, 3, 5 /
      data sel1_Locs / 1, 2, 3 /
      data sel1_Tims / 2, 7, 8, 9, 48, 49, 50 /

      data sel2_Pars / 5 /
      data sel2_Locs / 1, 3 /
      data sel2_Tims / 1, 21, 22, 23 /

      data sel3_Pars / 2, 3, 4, 5 /
      data sel3_Locs / 2 /
      do i = 1, nSel3_Tims
          sel3_Tims(i) = i
      enddo
#endif


C     !   Open file for results, write DioVersion

      resFileName = 'TESTF77LIB-res.txt'
      open(newunit=resLun,file=resFileName)

C     !   Initialise expected Data

      call initValues(NPARS, NLOCS, checkRValues)

C     !   Get non existin data set

      call DiofInit()

      name = 'TESTNotThere'

      set = DiofPltGetDataSet(name)
      if ( set .eq. 0 ) then
          write(resLun,*) 'Could not get Set:', name
        call DiofGetLastErrorMsg(errMsg)
#if (defined(WIN32))
          write(resLun,*) 'DIOTESTERROR: ',
     +           DiofGetLastError(), ' ' ,trim(errMsg)
#else
          write(resLun,*) 'DIOTESTERROR: ',
     +           DiofGetLastError(), ' ' ,errMsg
#endif
      else
          call DiofPltClose(set)
      endif

C     !   Get IN data set

      name = 'TestReal'

      write (*, *) 'Getting Dataset ', name
      write (resLun, *) 'Getting Dataset ', name
      set = DiofPltGetDataSet(name)
      if ( set .eq. 0 ) then
          write(resLun,*) 'Could not get Set:', name
      else
          nPar = DiofPltGetNPars(set)
          write(resLun,*) 'nPar: ', nPar
          nLoc = DiofPltGetNLocs(set)
          write(resLun,*) 'nLoc: ', nLoc
#if (defined(WIN32)||defined(HAVE_CONFIG_H))
          nTim = DiofPltGetNTimes(set)
          write(resLun,*) 'nTim: ', nTim
#endif
          if ( DiofPltGetPars(set, nPar, pars) ) then
              write(resLun,*) 'pars:'
              write(resLun,*) pars
          endif
          if ( DiofPltGetLocs(set, nLoc, locs) ) then
              write(resLun,*) 'locs:'
              write(resLun,*) locs
          endif
          if ( DiofPltGetIntIds(set, nLoc, IDs) ) then
              write(resLun,*) 'IDs:'
              write(resLun,*) IDs
          endif
#if (defined(WIN32)||defined(HAVE_CONFIG_H))
          if ( DiofPltGetTimes(set, nTim, tims) ) then
              write(resLun,*) 'tims:'
              write(resLun,*) tims
          endif
#endif
          write(resLun,*) 'HEADER LINES:'
          do i = 1,4
              if ( DiofPltGetHeaderLine(set, i, headerLine) ) then
                  write(resLun,*) i, ':', headerLine
            endif
          enddo

      endif

C     !   Get data for each timestep

      do t = 1, NTIMES+1

          if (set .ne. 0) then
              if (.not. DiofPltGetNextFloats(set, npar, nloc,
     +                        julTime, rValues) ) then
                  write(*,*) 'Did not get reals for Step: ',t
                  call DiofGetLastErrorMsg(errMsg)
#if (defined(WIN32))
                  write(resLun,*) 'DIOTESTERROR: ', 
     +                        DiofGetLastError(), ' ' ,trim(errMsg)
#else
                  write(resLun,*) 'DIOTESTERROR: ', 
     +                        DiofGetLastError(), ' ' ,errMsg
#endif
              else
#if (defined(WIN32))
                  write (resLun, '(A, I3, A, F17.8, A, A)')
     +               'Got Step:',t, ' JUL:', julTime,
     +              ' Time: ', trim(DiofJulian2DioTime(julTime))
#else
                  dioTime=DiofJulian2DioTime(julTime)
                  write (resLun, '(A, I3, A, F17.8, A, A)')
     +               'Got Step:',t, ' JUL:', julTime,
     +              ' Time: ', dioTime
#endif
                  write(resLun,*) rValues
                  do i = 1,NPARS
                      do j = 1,NLOCS
                           diffValues(i,j)=
     +                          rValues(i,j)-checkRvalues(i,j)
                      enddo
                  enddo
                  if (diffInValues(NPARS,NLOCS,diffValues,1.D-6)) then
                      write(resLun,*) 'DIFFS in reals for Step ', t
                      write(resLun,*) diffValues
                  endif
              endif
          endif
          call incrementValues(NPARS, NLOCS, checkRValues)

      enddo

C     !   cleanup

      if (set .ne. 0) then
          call DiofPltClose(set)
          write (*, *) 'Have destroyed IN dataset'
      endif

#if (defined(WIN32)||defined(HAVE_CONFIG_H))
      name = 'tstHisSelection.his'
      write (*, *) 'Getting Dataset ', name
      write (resLun, *) 'Getting Dataset ', name
      set = DiofPltGetDataSet(name)
      if ( set .eq. 0 ) then
          write(resLun,*) 'Could not get Set:', name
      else
          nPar = DiofPltGetNPars(set)
          write(resLun,*) 'nPar: ', nPar
          nLoc = DiofPltGetNLocs(set)
          write(resLun,*) 'nLoc: ', nLoc
          nTim = DiofPltGetNTimes(set)
          write(resLun,*) 'nTim: ', nTim
          if ( DiofPltGetPars(set, nPar, pars) ) then
              write(resLun,*) 'pars:'
              write(resLun,*) pars
          endif
          if ( DiofPltGetLocs(set, nLoc, locs) ) then
              write(resLun,*) 'locs:'
              write(resLun,*) locs
          endif
          if ( DiofPltGetTimes(set, nTim, tims) ) then
              write(resLun,*) 'tims:'
              write(resLun,*) tims
          endif

          if (DiofPltGetSelectionDoubles(set,
     +            nSel1_Pars, sel1_Pars, nSel1_Locs, sel1_Locs,
     +            nSel1_Tims, sel1_Tims, sel1_Values) ) then
              write(resLun, *) 'Got selection 1:'
              do t = 1, nSel1_Tims
                  do j = 1, nSel1_Locs
                      do i = 1, nSel1_Pars
                          write(resLun, *) sel1_Tims(t), sel1_Locs(j),
     +                         sel1_Pars(i), ':', sel1_Values(i, j, t)
                      enddo
                  enddo
              enddo
          endif

          if ( DiofPltGetLocs(set, nLoc, locs) ) then
              write(resLun,*) 'locs:'
              write(resLun,*) locs
          else
            write(resLun,*) 'COULD NOT GET LOCS AGAIN'
          endif

          if (DiofPltGetSelectionFloats(set,
     +            nSel2_Pars, sel2_Pars, nSel2_Locs, sel2_Locs,
     +            nSel2_Tims, sel2_Tims, sel2_Values) ) then
              write(resLun, *) 'Got selection 2:'
              do t = 1, nSel2_Tims
                  do j = 1, nSel2_Locs
                      do i = 1, nSel2_Pars
                          write(resLun, *) sel2_Tims(t), sel2_Locs(j),
     +                         sel2_Pars(i), ':', sel2_Values(i, j, t)
                      enddo
                  enddo
              enddo
          endif

          if (DiofPltGetSelectionFloats(set,
     +            nSel3_Pars, sel3_Pars, nSel3_Locs, sel3_Locs,
     +            nSel3_Tims, sel3_Tims, sel3_Values) ) then
              write(resLun, *) 'Got selection 3:'
              write(resLun, *) '           T           L           P'
              do t = 1, nSel3_Tims
                  do j = 1, nSel3_Locs
                      do i = 1, nSel3_Pars
                          write(resLun, *) sel3_Tims(t), sel3_Locs(j),
     +                         sel3_Pars(i), ':', sel3_Values(i, j, t)
                      enddo
                  enddo
              enddo
          endif

      endif
#endif

      close(resLun)

      end

