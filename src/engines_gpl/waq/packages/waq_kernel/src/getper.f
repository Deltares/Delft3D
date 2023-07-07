!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
      module m_getper

      implicit none

      contains


      SUBROUTINE GETPER (StreamName,DataSetName)
!
!     Deltares
!
!     CREATED             : jan  2001 by J.v.Gils
!
!     FUNCTION            : Gets permission from DIO to proceed
!                           one step (synchronised mode)
!
!
      use m_dio_plt

      character*(*) StreamName,DataSetName

      integer Nr_Variables, Nr_Locations, Nr_Times

      character*(dioMaxParLen) vars
      character*(dioMaxLocLen) locs
      character*(dioMaxTimLen) tims

      dimension vars(1)
      dimension locs(1)
      dimension tims(1)

      real      values
      dimension values(1,1)

      integer dioInStream
      integer dioInSet
cjvb  logical getResult
      integer getResult

      logical first
      save
!     save first, dioInSet
      data first /.true./

      if ( first ) then

      first = .false.

!     Open data stream
      dioInStream = DioCreateStreamSynched(dio_Binary_stream,
     +                    StreamName, 'r')

!     Get data set info
      dioInSet = DioGetPltDataSetInfo(dioInStream,
     +                                 DataSetName,
     +                                 Nr_Variables,vars,
     +                                 Nr_Locations,locs,
     +                                 Nr_Times, tims )

      endif

!     Get dataset values

      getResult = DioGetPltDataSetReals (dioInSet,tims(1),
     +                                   Nr_Variables,
     +                                   Nr_Locations,
     +                                   values)

      RETURN
      END
      SUBROUTINE GETPCF (StreamName,DataSetName)
!
!     Deltares
!
!     FUNCTION            : Gets permission from DIO to proceed
!                           one step (synchronised mode)
!
!
      use m_dio_plt


      character*(*) StreamName,DataSetName

      integer Nr_Variables, Nr_Locations, Nr_Times

      character*(dioMaxParLen) vars
      character*(dioMaxLocLen) locs
      character*(dioMaxTimLen) tims

      dimension vars(1)
      dimension locs(1)
      dimension tims(1)

      real      values
      dimension values(1,1)

      integer dioInStream
      integer dioInSet
cjvb  logical getResult
      integer getResult

      logical first
      save
!     save first, dioInSet
      data first /.true./

      if ( first ) then

      first = .false.

!     Open data stream
      dioInStream = DioCreateStreamSynched(dio_Binary_stream,
     +                    StreamName, 'r')

!     Get data set info
      dioInSet = DioGetPltDataSetInfo(dioInStream,
     +                                 DataSetName,
     +                                 Nr_Variables,vars,
     +                                 Nr_Locations,locs,
     +                                 Nr_Times, tims )

      endif

!     Get dataset values

      getResult = DioGetPltDataSetReals (dioInSet,tims(1),
     +                                   Nr_Variables,
     +                                   Nr_Locations,
     +                                   values)

      RETURN
      END

      end module m_getper
