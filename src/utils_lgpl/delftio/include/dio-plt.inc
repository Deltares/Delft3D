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
!  $Id$
!  $HeadURL$
!
!     dio-plt.inc
!
!     F77 Interface to
!     DelftIO Parameter-Location-Time Datasets
!
!     Stef.Hummel@deltares.nl
!
!     (c) Deltares, Feb 01
!---------------------------------------------
!
      include 'dio-streams.inc'
!
!     Sizes for character strings
!
      integer DioMaxDsNameLen
      integer DioMaxParLen
      integer DioMaxLocLen
      integer DioMaxDescLen

      parameter ( DioMaxDsNameLen = 132 )
      parameter ( DioMaxParLen    = 80  )
      parameter ( DioMaxLocLen    = 132 )
      parameter ( DioMaxDescLen   = 132 )

      integer     HisRunIdSize      , HisRunIdDim
      parameter ( HisRunIdSize = 40 , HisRunIdDim = 4)
!
!     Dataset Types
!
      integer Dio_Plt_Unknown
      integer Dio_Plt_Integer
      integer Dio_Plt_Real
      integer Dio_Plt_Double
      integer Dio_Plt_Logical

      parameter ( Dio_Plt_Unknown  = 0 )
      parameter ( Dio_Plt_Integer  = 1 )
      parameter ( Dio_Plt_Real     = 2 )
      parameter ( Dio_Plt_Double   = 3 )
      parameter ( Dio_Plt_Logical  = 4 )
!
!     Manipulate Parameters or Locations?
!
      integer Dio_Plt_Pars
      integer Dio_Plt_Locs

      parameter ( Dio_Plt_Pars  = 1 )
      parameter ( Dio_Plt_Locs  = 2 )
!
!     Function declarations
!
!     ORIGINAL interface (used by SRW)
!
      integer DioDefinePltDataSet
      integer DioGetPltDataSetInfo
      logical DioGetPltDataSetReals

      external DioDefinePltDataSet
      external DioGetPltDataSetInfo, DioGetPltDataSetReals
!
!     ODS Server interface
!
      integer  DiofPltDefine_1 , DiofPltDefine_2, DiofPltDefine_2a, &
               DiofPltDefine_3 , DiofPltDefine_4, DiofPltDefine_4a, &
               DiofPltDefine_5 , DiofPltDefine_5a
      external DiofPltDefine_1 , DiofPltDefine_2,                   &
               DiofPltDefine_3 , DiofPltDefine_4, DiofPltDefine_4a, &
               DiofPltDefine_5 , DiofPltDefine_5a

      integer  DiofPltDefine_11, DiofPltDefine_12, DiofPltDefine_12a, &
               DiofPltDefine_13, DiofPltDefine_14, DiofPltDefine_14a
      external DiofPltDefine_11, DiofPltDefine_12, DiofPltDefine_12a, &
               DiofPltDefine_13, DiofPltDefine_14, DiofPltDefine_14a

      integer  DiofPltGetDataSet
      external DiofPltGetDataSet

      integer  DiofPltGetNPars, DiofPltGetNLocs, DiofPltGetNTimes
      external DiofPltGetNPars, DiofPltGetNLocs, DiofPltGetNTimes

      logical  DiofPltGetPars, DiofPltGetLocs, DiofPltGetTimes
      external DiofPltGetPars, DiofPltGetLocs, DiofPltGetTimes

      logical  DiofPltGetDescriptions
      external DiofPltGetDescriptions

      logical  DiofPltGetHeaderLine, DiofPltGetIntIds
      external DiofPltGetHeaderLine, DiofPltGetIntIds

      logical  DiofPltGetNextFloats
      external DiofPltGetNextFloats

      logical  DiofPltGetNextDoubles
      external DiofPltGetNextDoubles

      logical  DiofPltGetSelectionFloats, DiofPltGetSelectionDoubles
      external DiofPltGetSelectionFloats, DiofPltGetSelectionDoubles

