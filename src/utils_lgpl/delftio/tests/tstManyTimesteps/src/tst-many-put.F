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
      program test_many_put

      implicit none

      include 'dio-many-tst.inc'
      include 'dio-plt.inc'

C     ! locals

      integer            set
      character*(100)    name

      character*(DioMaxParLen) pars(NPARS)
      character*(DioMaxLocLen) locs(NLOCS)
      character*(HisRunIdSize) runId(HisRunIdDim)

      real*4           rValues(NPARS,NLOCS)

      integer i
      double precision t, tStart

      data pars   / 'Aa', 'Bb', 'Cc' /
      data locs   / '11', '22' /
      data runId  / 'tstManyPut', 'check if large HIS files work',
     +              '3 Pars, 2 Locs',
     +              'T0: 0000/01/01;00:00:00  (scu=      60s)' /


      character*(DioMaxTimLen) dioTime
      dioTime  = '0000/01/01;00:00:00.00'
      tStart   = DiofTimeString2Julian(dioTime)

C     !   Initialise expected Data

      call initValues(NPARS, NLOCS, rValues)

C     !   Initialize data set names

      name = 'TESTMany'

C     !   Create IN data sets


      write (*, *) 'Putting Dataset ', name
      set = DiofPltDefine_3(name, runId,
     +               Dio_PLT_Real, NPARS, pars, NLOCS, locs)
      if ( set .eq. 0 ) then
          write (*, *) 'Could not Define ', name
      endif

C     !   Put data for each timestep
      t = tStart
      do i = 1, NTIMES
          if ( set .ne. 0 ) then
              t = t + 4.0D+00
              if (i .eq. 1 .or. mod(i,5000).eq.0) then
#if (defined(WIN32))
                  write(*, *) 'Putting Time Step ',i,
     +            ' JUL: ', t,
     +            ' DATE: ', trim(DiofJulian2DioTime(t))
#else
                  write(*, *) 'Putting Time Step ',i,
     +            ' JUL: ', t,
     +            ' DATE: ', DiofJulian2DioTime(t)
#endif
              endif
              call DiofPltPutFloats(set, t,
     +                         NPARS, NLOCS, rValues)
          endif
          call incrementValues(NPARS, NLOCS, rValues)
      enddo

C     !   cleanup

      if ( set .ne. 0 ) then
          call DiofPltClose(set)
          write (*, *) 'Have destroyed IN dataset'
      endif

      end

