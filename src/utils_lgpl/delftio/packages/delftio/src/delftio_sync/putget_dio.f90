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
      subroutine putdio ( StreamName, DataSetName, first, dioOutSet )
!     
!     Created             : jan  2001 by J.v.Gils
!
!     Function            : Gives permission to DIO to proceed
!                           one step (synchronised mode)
!
!
!     use dio_streams
!     use dio_plt_rw
      include 'dio-plt.inc'
!
      integer                  dioOutSet      !! output number stream
      integer                  dioOutStream
      integer                  Nr_Variables
      integer                  Nr_Locations
!
      logical                  first          !! first invokation for this stream ?
!
      character*(*)            DataSetName    !! input dataset name
      character*(dioMaxLocLen) locs(1)
      character*(*)            StreamName     !! input streamname
      character*(dioMaxTimLen) tims(1)
      character*(dioMaxParLen) vars(1)
      real                     values(1,1)
!
      if ( first ) then
!               Open data stream
         dioOutStream = DioCreateStreamSynched ( dio_Binary_stream, &
                                                 StreamName, 'w'  )
!               Create data set
         Nr_Locations = 1
         Nr_Variables = 1
         locs(1) = 'dioLocNam'
         vars(1) = 'dioVarNam'
         tims(1) = 'dioDate'
         dioOutSet    = DioDefinePltDataSet    ( dioOutStream,        &
                                                 DataSetName ,        &
                                                 Dio_Plt_Real,        &
                                                 Nr_Variables, vars , &
                                                 Nr_Locations, locs )
      endif
!               Put dataset values
      values = 0.0
      call DioPutPltDataSetReals (dioOutSet, tims(1), 1, 1, values )
!
      return
      end subroutine putdio 
!
      subroutine getdio ( StreamName, DataSetName, first, dioInSet )
!
!     Created             : jan  2001 by J.v.Gils
!
!     Function            : Gets permission from DIO to proceed
!                           one step (synchronised mode)
!
!
!     use dio_streams
!     use dio_plt_rw
      include 'dio-plt.inc'
!
      character*(*) StreamName    !! input streamname
      character*(*) DataSetName   !! input dataset name
      logical       first         !! first invokation for this stream ?
      integer       dioInSet      !! input number stream
!
      character*(dioMaxParLen) vars(1)
      character*(dioMaxLocLen) locs(1)
      character*(dioMaxTimLen) tims(1)
      real                     values(1,1)
      integer                  dioInStream
      integer                  Nr_Variables
      integer                  Nr_Locations
      integer                  Nr_Times
      logical                  getResult
!
      if ( first ) then
!               Open data stream
         dioInStream = DioCreateStreamSynched( dio_Binary_stream, &
                                               StreamName, 'r'  )
!               Get data set info
         dioInSet    = DioGetPltDataSetInfo  ( dioInStream,        &
                                               DataSetName,        &
                                               Nr_Variables, vars, &
                                               Nr_Locations, locs, &
                                               Nr_Times    , tims )
      endif

!               Get dataset values
      getResult = DioGetPltDataSetReals ( dioInSet,tims(1), &
                                          Nr_Variables    , &
                                          Nr_Locations    , &
                                          values          )

      return
      end subroutine getdio 
