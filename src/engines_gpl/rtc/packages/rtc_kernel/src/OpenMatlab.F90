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

      function OpenMatlab (IDEBUG) result(RetVal)

      use OtherData, only : MATUSE, MATDIR, MATFIL, MATDBG, MATEXE
      use system_utils, only: FILESEP
      use rtc_matlab_module, only : rtc_matlab_instance, start_matlab

      IMPLICIT NONE
      !
      integer :: IDEBUG
      integer :: Retval
      integer :: iud
      !
      integer        :: istat
      logical        :: error
      character(256) :: pathd
      character(8)   :: date
      character(10)  :: time
      character(256) :: diofolder
      character(256) :: rtcfolder
      character(256) :: userscript
      character(256) :: channel
          
      RetVal = 0 

      if (MATDBG>0) write(IDEBUG,'(A)') 'Entering OpenMatlab'

#if (defined(USE_MATLAB))
      if (MATUSE) then
         !
         ! provide attach opportunity for debugging ...
         !
!10       CALL GPSLEEP (1)
!         open(newunit=iud,file='d:\waitfile.txt',err=10,status='OLD')
!         close(iud,status='DELETE')
         !
         call getexedir (error, pathd)
         diofolder  = trim(pathd)//'matlab'//FILESEP//'delftio'//FILESEP//'progsrc'
         rtcfolder  = trim(pathd)//'matlab'//FILESEP//'rtc'//FILESEP//'progsrc'
         userscript = trim(MATDIR)//FILESEP//MATFIL
         !
         ! create unique channel ID for shared memory communication with MATLAB
         !
         call DATE_AND_TIME(date,time)
         channel    = date//time
         !
         RetVal = start_matlab(rtc_matlab_instance, rtcfolder, diofolder, userscript, MATEXE, channel, IDEBUG, MATDBG)
         !
      endif
#endif

      end function OpenMatlab
