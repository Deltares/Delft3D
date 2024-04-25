subroutine rdtdf(lundia    ,luntdp    ,error     ,filnam    ,fmttmp    , &
               & nrval     ,rval      ,dt        ,itstrt    ,itfinish  , &
               & gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
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
!  
!  
!!--description-----------------------------------------------------------------
!
!    Function: This general purpose routine reads the time depen-
!              dent data from file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    use system_utils, only: exifil
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                     , intent(in)  :: itfinish !  Description and declaration in inttim.igs
    integer                     , intent(in)  :: itstrt   !  Description and declaration in inttim.igs
    integer                     , intent(in)  :: lundia   !  Description and declaration in inout.igs
    integer                     , intent(in)  :: luntdp   !  Unit number of the unformatted file where the data, just read, are to be rewritten again (for FLOW sim.)
    integer                     , intent(in)  :: nrval    !  NR. of data in a record to be read
    logical                     , intent(out) :: error    !  Flag=TRUE if an error is encountered
    real(fp)                    , intent(in)  :: dt       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nrval)  , intent(out) :: rval     !  Array for the time dependent data
                                                          !  RVAL   (1,I) = Values at t=TIM0
                                                          !  RVAL   (2,I) = Values at t=TIM1
                                                          !  I=1,.,NDIM   = nr. of REC. to read
    character(*)                , intent(in)  :: filnam   !  Name of the relevant file
    character(11)               , intent(in)  :: fmttmp   !  Format of the relevant file
!
! Local variables
!
    integer                        :: iocond ! IO status for reading 
    integer                        :: it
    integer                        :: itold  ! Help var. to store last read time to test accending order 
    integer                        :: ittdep ! Help var. for the time read (now de- fined as multiples of DT, but in fu- ture it may take any value) 
    integer                        :: lfile  ! Help var. specifying the length of character variables for file names 
    integer                        :: lunrd  ! Unit number of the attribute file containing the time series 
    integer                        :: n      ! Help var. for the nr. of data to be read (see LENDAT) 
    integer         , external     :: newlun
    logical                        :: dtn
    logical                        :: rec1st ! Flag set to TRUE if the record read is the first record 
    real(fp)                       :: t
    real(fp)                       :: timrd  ! Time in minutes read 
    character(300)                 :: errmsg ! Character var. containing the error message to be written to file. The message depend on the error. 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !-----define length of file name
    !
    call remove_leading_spaces(filnam    ,lfile     )
    !
    !-----test file existence <YES>
    !
    if (exifil(filnam, lundia)) then
       !
       !--------Values time dependent data for the first time
       !
       rec1st = .true.
       ittdep = -1
       itold  = -1
       !
       open (newunit=lunrd, file = filnam(:lfile), form = fmttmp, status = 'old')
       if (fmttmp(:2)/='un') then
          !
          !-----------skip lines starting with a '*'
          !
          call skipstarlines(lunrd     )
       endif
       !
       ! -->
       !
  100  continue
       if (fmttmp(:2)=='un') then
          read (lunrd, iostat = iocond) timrd, (rval(n), n = 1, nrval)
       else
          read (lunrd, *, iostat = iocond) timrd, (rval(n), n = 1, nrval)
       endif
       !
       ! If an NaN is read -> error
       !
       do n = 1, nrval
           if (isnan(rval(n))) then
              write(errmsg,'(a,a)') 'NaN in ', trim(filnam)
              call prterr(lundia    ,'P004'    ,errmsg      )
              !
              error = .true.
              goto 200
           endif
       enddo
       !
       !-----------test last time read (IOCOND < 0),
       !           or reading error (IOCOND > 0)
       !
       if (iocond<0) then
          if (itold < itfinish) then
             errmsg = 'Last time in file ' // filnam(:lfile) // ' <'
             call prterr(lundia    ,'U042'    ,errmsg(:20 + lfile)  )
             !
             error = .true.
          endif
          goto 200
       elseif (iocond>0) then
          call prterr(lundia    ,'G007'    ,filnam(:lfile)       )
          !
          error = .true.
          goto 200
       else
       endif
       !
       !-----------NOTE : in the future one should be able to interpolate across dt
       !
       ittdep = nint(timrd/dt)
       if (dtn(ittdep, timrd, dt)) then
          errmsg = 'Times in file ' // filnam(:lfile)
          call prterr(lundia    ,'U044'    ,errmsg(:14 + lfile)  )
          !
          error = .true.
          goto 200
       endif
       !
       !-----------Set the times into standard integer time par., define mini
       !           mum time and write the data in the unformatted file
       !
       if (rec1st) then
          if (ittdep>itstrt) then
             errmsg = 'First time in file ' // filnam(:lfile) // ' >'
             call prterr(lundia    ,'U041'    ,errmsg(:21 + lfile)  )
             !
             error = .true.
             goto 200
          endif
          rec1st = .false.
       endif
       !
       if (ittdep<=itold) then
          call prterr(lundia    ,'U062'    ,filnam(:lfile)       )
          !
          error = .true.
          goto 200
       endif
       !
       !-----------write to help file
       !
       write (luntdp) timrd, (rval(n), n = 1, nrval)
       !
       itold = ittdep
       goto 100
       !
       ! <--
       !
       !-----------stop reading file
       !
       !
       !--------close file and define maximum time frame
       !
  200  continue
       close (lunrd)
    !
    !-----test file existence <NO>
    !
    else
       error = .true.
    endif
end subroutine rdtdf
