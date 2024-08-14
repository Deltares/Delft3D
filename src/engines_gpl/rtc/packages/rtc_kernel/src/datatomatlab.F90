!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
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

      function DataToMatlab (IDEBUG, IDUM , IOUT1, IflRtnRtc) result(RetVal)

! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IOUT1  = file unit number of output file with messages
! ***  IDUM   = time step number, note: idum-timeshift>=1.
! *********************************************************************
      use OtherData, only: MATUSE, MATDBG
      use ParameterModule, only: CharIdLength
      use m_alloc, only: realloc
      use rtc_matlab_module, only: rtc_matlab_instance, send_inputpar_names, send_outputpar_names, send_inputpar_values

      IMPLICIT NONE

      integer :: IDEBUG
      integer :: IDUM
      integer :: IOUT1
      integer :: IflRtnRtc
      integer :: RetVal

      if (MATDBG>0) write(IDEBUG,'(A)') 'Entering DataToMatlab'

      RetVal = 0

#if (defined(USE_MATLAB))
      if (MATUSE) then
         if (IDUM==1) then
            call send_inputpar_names(rtc_matlab_instance,IDEBUG,MATDBG)
            call send_outputpar_names(rtc_matlab_instance,IDEBUG,MATDBG)
         endif
         call send_inputpar_values(rtc_matlab_instance,IDUM,IDEBUG,MATDBG)
      endif
#endif

      end function DataToMatlab
