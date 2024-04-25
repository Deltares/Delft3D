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
      subroutine write_map_long

      implicit none

      include 'dio-plt.inc'

      ! locals

      character*(DioMaxStreamLen) datasetName

      integer nPar, nLoc
      parameter (nPar = 7, nLoc = 4)

      character*(DioMaxParLen) pars(nPar)
      character*(DioMaxLocLen) locs(nLoc)
      real values(nPar,nLoc)

      integer outSet
      integer i,j

      character*(40) runId(4)

      character*(DiofMaxErrMsgLen) errMsg
      integer errNr

      data pars
     +/'Aa', 'Bb is een hele lange, veel te lange naam',
     + 'Cc', 'Dd', 'En ook Ee is een hele lange, veel te lange naam',
     + 'Ff', 'Gg'  /

      data locs
     +/'11 is best wel een locatie naam waarvan je zegt:
     + die is ontzettend lang zeg, nog langer dan wat
     + de HIA module aan zou moeten kunnen',
     + '22', '33', '44 is en vreselijk lange, ingewikkelde
     + en vervelende locatie naam' /

      data runId / 'myTestRun', 'check if long names work',
     +             'write_map_long', 'Vierde String' /

C     Create MAP OUT data set

      datasetName = 'TESTMapF77.map'

      do i = 1, nLoc
          do j = 1, nPar
              values(j,i) = j * 0.01 + i * 0.10
          enddo
      enddo

      write (*, *) 'Creating OUT dataset'

      outSet = DiofPltDefine_11(datasetName, Dio_Plt_Real,
     +                                  nPar, pars, nLoc)


C     Put MAP OUT data for each timestop

      if ( outSet .eq. 0 ) then
          call DiofGetLastErrorMsg(errMsg)
#if (defined(WIN32))
          write(*,*) 'DIOTESTERROR: ',
     +           DiofGetLastError(), ' ' ,trim(errMsg)
#else
          write(*,*) 'DIOTESTERROR: ',
     +           DiofGetLastError(), ' ' ,errMsg
#endif
      else
          write (*, *) 'OUT Dataset Created'
          do i = 1, 50
              call DiofPltPutNextFloats(outSet, nPar, nLoc, values)
          enddo
      endif

C     destroy datasets

      call DiofPltClose(outSet)

      write (*, *) 'Have destroyed MAP OUT sets'

      return
      end


      subroutine read_map_long

      implicit none

      include 'dio-plt.inc'

      character*(DioMaxStreamLen) datasetName

      integer nPar, nLoc, nTim

      integer nMaxPar, nMaxLoc, nMaxTim
      parameter (nMaxPar = 10, nMaxLoc = 10, nMaxTim = 50)


      character*(DioMaxParLen) pars(nMaxPar)
      character*(DioMaxLocLen) locs(nMaxLoc)
      double precision tims(nMaxTim)

      real rValues(nMaxPar,nMaxLoc)
      double precision julTime

      integer inSet
      integer resLun, i, j, t

      character*(DiofMaxErrMsgLen) errMsg
      integer errNr

      character*(DioMaxTimLen) dioTime

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
      data sel1_Tims / 1, 2, 3, 25, 48, 49, 50 /

      data sel2_Pars / 5 /
      data sel2_Locs / 3, 4 /
      data sel2_Tims / 1, 21, 22, 23 /

      data sel3_Pars / 2, 3, 4, 5 /
      data sel3_Locs / 2 /
      do i = 1, nSel3_Tims
          sel3_Tims(i) = i
      enddo
#endif


C     Open file for storing results
      call diofinit()
      open(newunit=resLun,file='TESTF77Map-res.txt')

C     Get MAP IN data set

      datasetName = 'TESTMapF77.map'
      inSet = DiofPltGetDataset(datasetName)
      write (*, *) 'MAP IN Datasets Initialized'

      if ( inSet .eq. 0 ) then
          write (*, *) 'DID NOT GET MAP Dataset ', datasetName
      else

          nPar = DiofPltGetNPars(inSet)
          write(resLun,*) 'nPar: ', nPar

          nLoc = DiofPltGetnLocs(inSet)
          write(resLun,*) 'nLoc: ', nLoc

#if (defined(WIN32)||defined(HAVE_CONFIG_H))
          nTim = DiofPltGetNTimes(inSet)
          write(resLun,*) 'nTim: ', nTim
#endif

          if ( DiofPltGetPars(inSet, nPar, pars) ) then
              write(resLun,*) 'pars:'
              do i = 1, nPar
#if (defined(WIN32))
                  write(resLun,*) trim(pars(i))
#else
                  write(resLun,*) pars(i)
#endif
              enddo
          endif

          if ( .not. DiofPltGetLocs(inSet, nLoc, locs) ) then
              write(resLun,*) 'MAP: No Locations'
              call DiofGetLastErrorMsg(errMsg)
#if (defined(WIN32))
              write(*,*) 'DIOTESTERROR: ',
     +               DiofGetLastError(), ' ' ,trim(errMsg)
#else
              write(*,*) 'DIOTESTERROR: ',
     +               DiofGetLastError(), ' ' ,errMsg
#endif
          else
              write(resLun,*) 'MAP: locs:'
              do i = 1, nLoc
#if (defined(WIN32))
                  write(resLun,*) trim(locs(i))
#else
                  write(resLun,*) locs(i)
#endif
              enddo
          endif

#if (defined(WIN32)||defined(HAVE_CONFIG_H))
          if ( DiofPltGetTimes(inSet, nTim, tims) ) then
              write(resLun,*) 'tims:'
              write(resLun,*) tims(1:nTim)
          endif
#endif

      !   Get Values

          if (.not. DiofPltGetNextFloats(inSet, npar, nloc,
     +                        julTime, rValues) ) then

              write(*,*) 'Did not get reals'

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
              write (resLun, '(A, F17.8, A, A)')
     +              'Got Reals, JUL:', julTime,
     +              ' Time: ', trim(DiofJulian2DioTime(julTime))
#else
              dioTime=DiofJulian2DioTime(julTime)
              write (resLun, '(A, F17.8, A, A)')
     +              'Got Reals, JUL:', julTime,
     +              ' Time: ', dioTime
#endif

            endif

#if (defined(WIN32)||defined(HAVE_CONFIG_H))
          if (DiofPltGetSelectionDoubles(inSet,
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

          if (DiofPltGetSelectionFloats(inSet,
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

          if (DiofPltGetSelectionFloats(inSet,
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
#endif

      endif
      
C     destroy datasets

      call DiofPltClose(inSet)

C     Close file for with results
      close(resLun)

      write (*, *) 'Got Everything'
      ! write (*, *) 'Got Everything, GIVE ENTER'
      ! read(*,*)

      return
      end


      program test_map_long

      call write_map_long
      call read_map_long

      end


