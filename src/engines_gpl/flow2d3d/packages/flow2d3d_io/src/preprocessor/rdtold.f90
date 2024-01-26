subroutine rdtold(lunrd     ,lundia    ,error     ,filnam    ,ntimrd    , &
                & nrskip    ,dt        ,itstrt    ,itfinish  ,itold     ,gdp       )
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
!    Function: Reads the times from old time dependent data file
!              to determine number ot times read and to check
!              the condition required for the specifdied times
!              and to check the completeness of the file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
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
    integer                   :: itold    !  Help var. to store last read time to test accending order
    integer     , intent(in)  :: itfinish !  Description and declaration in inttim.igs
    integer     , intent(in)  :: itstrt   !  Description and declaration in inttim.igs
    integer     , intent(in)  :: lundia   !  Description and declaration in inout.igs
    integer     , intent(in)  :: lunrd    !  Unit number of the attribute file containing the time series
    integer     , intent(in)  :: nrskip   !  Nr. of records to be read/skipped that must exists if the file is complete
    integer                   :: ntimrd   !  Nr. of times read
    logical     , intent(out) :: error    !  Flag=TRUE if an error is encountered
    real(fp)    , intent(in)  :: dt       !  Description and declaration in esm_alloc_real.f90
    character(*), intent(in)  :: filnam   !  Name of the relevant file
!
! Local variables
!
    integer                        :: iocond               ! IO status for reading 
    integer                        :: ittdep               ! Help var. for the time read (now de- fined as multiples of DT, but in fu- ture it may take any value) 
    integer                        :: n                    ! Help var. 
    real(fp)                       :: timrd                ! Time in minutes read 
    character(256)                 :: errmsg               ! Character var. containing the error message to be written to file. The message depend on the error. 
    character(72)                  :: rec72                ! Record 
!
!
!! executable statements -------------------------------------------------------
!
    !
    error = .false.
    !
    !-----Count number of records in the file
    !
  110 continue
    read (lunrd, *, iostat = iocond) timrd
    !
    !-------test last time read (IOCOND < 0)
    !       OK depending on ITOLD >= ITFINISH and ITO = 1
    !       reading error (IOCOND > 0), not allowed
    !
    if (iocond/=0) then
       if (iocond<0) then
          if (itold < itfinish) then
             errmsg = 'Last time in ' // filnam // ' <'
             call prterr(lundia    ,'U042'    ,errmsg    )
             !
             error = .true.
          endif
       else
          !
          !-------------error (IOCOND > 0), not allowed
          !
          call prterr(lundia    ,'G007'    ,filnam    )
          !
          write (lundia, '(a,f12.3)') 'RECORD: ', timrd
          error = .true.
       endif
       goto 9999
    else
       ntimrd = ntimrd + 1
    endif
    !
    !-------Perform some checks on time value read
    !
    call chckit(lundia    ,error     ,filnam    ,timrd     ,dt        , &
              & ittdep    ,itold     ,itstrt    ,ntimrd    ,gdp       )
    !
    if (error) goto 9999
    if (ittdep>itold) itold = ittdep
    !
    !-------Skip NRSKIP-1 records
    !
    do n = 1, nrskip - 1
       read (lunrd, '(a)', iostat = iocond) rec72
       !
       !----------last record read or error (IOCOND <> 0), not allowed
       !
       if (iocond/=0) then
          error = .true.
          call prterr(lundia    ,'G007'    ,filnam    )
          !
          write (lundia, '(a,a)') 'RECORD: ', rec72
          goto 9999
       endif
    enddo
    goto 110
    ! <--
    !
 9999 continue
end subroutine rdtold
