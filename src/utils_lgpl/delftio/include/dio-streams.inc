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
!     dio-streams.inc
!
!     F77 Interface to
!     DelftIO Streams
!
!     Stef.Hummel@deltares.nl
!
!     (c) Deltares, Feb 01
!---------------------------------------------
!
      include 'dio-time.inc'
!
!     Sizes for character strings
!
      integer DioMaxStreamLen
      parameter ( DioMaxStreamLen = 256 )
!
!     Stream Types
!
      integer dio_ASCII_stream
      integer dio_Binary_stream
      integer dio_His_stream
      integer dio_Nefis_stream

      parameter ( dio_ASCII_stream  = 1 , &
                  dio_Binary_stream = 2 , &
                  dio_His_stream    = 3 , &
                  dio_Nefis_stream  = 4 )

!
!     ORIGINAL interface (used by SRW)
!
      integer DioCreateStream, DioCreateStreamSynched
      external DioCreateStream, DioCreateStreamSynched
      
!
!     ODS Server interface
!
      integer DiofMaxErrMsgLen
      parameter ( DiofMaxErrMsgLen = 256 )

      integer DiofGetLastError
      external DiofGetLastError

