subroutine rwbcb(lundia    ,lunrd     ,filinp    ,error     ,itstrt    , &
               & itfinish  ,gdp       )
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
!    Function: - Reads from BCB-file into temporary file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    use time_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer  , pointer :: itdate
    real(fp) , pointer :: dt
!
! Global variables
!
    integer         , intent(in)  :: itfinish   !  Description and declaration in inttim.igs
    integer         , intent(in)  :: itstrt     !  Description and declaration in inttim.igs
    integer         , intent(in)  :: lundia     !  Description and declaration in inout.igs
    integer         , intent(in)  :: lunrd      !  Unit number for input file
    logical         , intent(out) :: error      !  Flag=TRUE if an error is encountered
    character(*)    , intent(in)  :: filinp     !  Name of input file
!
!
! Local variables
!
    integer                        :: endpos               ! Endposition of scan 
    integer                        :: i                    ! Loop counter 
    integer                        :: iacdat               ! Actual simulation day for RTC 
    integer                        :: iactim               ! Actual simulation time for RTC 
    integer                        :: icurec               ! Current record of input file 
    integer                        :: idef                 ! Default for READ routine 
    integer                        :: ierror               ! errorcode from read routines 
    integer                        :: ifound
    integer                        :: itold                ! Help var. to store last read time to test accending order 
    integer                        :: ittdep               ! Help var. for the time read (now de- fined as multiples of DT, but in fu- ture it may take any value) 
    integer                        :: j                    ! Loop counter 
    integer                        :: julday               ! Julian day number simulation start date 
    integer                        :: lcurec               ! Length of trimmed record number 
    integer                        :: lninp                ! Help var. specifying the length of input file name 
    integer                        :: lrec                 ! Length of record 
    integer                        :: lunout               ! Unit number for transformed file 
    integer                        :: newlun
    integer                        :: nrecs                ! Number of records in table 
    integer                        :: stapos               ! Start position of scan 
    integer                        :: timref               ! Reference time read 
    logical                        :: ex                   ! Flag to test if file exists 
    logical                        :: lnewtb               ! Logical array to determine if new table was started 
    logical, dimension(mxkwtd)     :: lkeyfd               ! Logical array to determine which keys have been found. 
    real(fp)                       :: height               ! Height of barrier read 
    real(fp)                       :: rdef                 ! Default for READ routine 
    real(fp)                       :: timrd                ! Time in minutes read 
    real(fp)                       :: timscl               ! Multiple factor to create minutes from read times 
    character(12)                  :: filout               ! Name of output file 
    character(132)                 :: crecrd               ! Record string 
    character(20)                  :: inte20               ! Interpolation method for barrier 
    character(20)                  :: loca20               ! Name of barrier location 
    character(20)                  :: timuni               ! Time unit read 
    character(300)                 :: errmsg               ! errormessage text 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    itdate  => gdp%gdexttim%itdate
    dt      => gdp%gdexttim%dt
    !
    icurec = 0
    call remove_leading_spaces(filinp    ,lninp     )
    timscl = 1.0
    !
    !-----Open output file
    !
    filout = 'TMP_Bar.bcb'
    inquire (file = filout, exist = ex)
    if (ex) then
       open (newunit=lunout, file = filout)
       close (lunout, status = 'delete')
    endif
    open (newunit=lunout, file = filout, form = 'unformatted', status = 'unknown')
    !
    !---- Read the data, check it and write the output file
    !
    do i = 1, 9999
       !------ Clear keyword flags
       do j = 1, mxkwtd
          lkeyfd = .false.
       enddo
       !------ Set new table flag
       lnewtb = .true.
       !
       !------ First the header part
       !       From the header we scan for the location, interpolation
       !       method, reference-time, time unit and records in table.
       !       Location and records in table are compulsory.
       !
       ! --->
  100  continue
       icurec = icurec + 1
       read (lunrd, '(a)', err = 7777, end = 8888) crecrd
       !------ Ignore when empty
       if (crecrd==' ') goto 100
       !------ After read no more new table anymore
       lnewtb = .false.
       stapos = 1
       endpos = len(crecrd)
       call srckey(crecrd    ,stapos    ,endpos    ,ifound    ,gdp       )
       !
       !
       !------ IFOUND =      -9999 error occurred
       !              =       9999 comment line
       !              =          0 not found
       !              = [1,MXKWTD] found
       !
       !------ Error: message and quit.
       if (ifound== - 9999) goto 7777
       !------ Comment: read next record
       if (ifound==9999) goto 100
       !------ No keyword found: Presume end of header, most probably
       !       not complete.
       if (ifound==0) goto 600
       if (ifound==3) then
          !
          !---------Location keyword found
          !
          call keyinp(crecrd(stapos:endpos),loca20    )
          if (loca20==' ') goto 7777
          lkeyfd(3) = .true.
       elseif (ifound==9) then
          !
          !---------Reference time keyword found
          !
          lrec = len(crecrd)
          idef = 0
          call read1i(crecrd    ,lrec      ,stapos    ,endpos    ,timref    , &
                    & idef      ,ierror   )
          if (ierror<=0) goto 7777
          if (timref/=itdate) then
             call prterr(lundia    ,'P004'    ,'Reference time must be equal to model reference time '          )
             !
             goto 7777
          endif
          lkeyfd(9) = .true.
       elseif (ifound==10) then
          !
          !---------Time unit keyword found
          !
          call keyinp(crecrd(stapos:endpos),timuni    )
          call small(timuni    ,len(timuni)          )
          if (timuni(1:7)=='seconds') then
             timscl = 1./60.
          elseif (timuni(1:7)=='minutes') then
             timscl = 1.
          elseif (timuni(1:4)=='hours') then
             timscl = 60.
          elseif (timuni(1:4)=='days') then
             timscl = 1440.
          else
             timscl = -1.0
          endif
          if (timscl<0.0) then
             call prterr(lundia    ,'P004'    ,'Time unit must be seconds, minutes, hours or days '  )
             !
             goto 7777
          endif
          lkeyfd(10) = .true.
       elseif (ifound==12) then
          !
          !---------Interpolation keyword found
          !
          call keyinp(crecrd(stapos:endpos),inte20    )
          if (inte20==' ') goto 7777
          if (inte20(1:6)/='linear' .and. inte20(1:5)/='block') then
             call prterr(lundia    ,'P004'    ,'Interpolation must be ''linear'' or ''block'' '      )
             !
             goto 7777
          endif
          lkeyfd(12) = .true.
       elseif (ifound==16) then
          !
          !---------Nr. of record keyword found
          !         Number of time record should be > 0
          !
          lrec = len(crecrd)
          idef = 0
          call read1i(crecrd    ,lrec      ,stapos    ,endpos    ,nrecs     , &
                    & idef      ,ierror   )
          if (nrecs<=0) then
             call prterr(lundia    ,'P004'    ,'Records in table must be > 0 ' )
             !
             goto 7777
          endif
          lkeyfd(16) = .true.
          !-------- End of header
          goto 600
       else
       endif
       !
       !------ Read next record
       !
       goto 100
       ! <---
       !
       !------ Header read
       !
       !
       !------ Check for compulsory items
       !
       !------ Location
  600  continue
       if (.not.lkeyfd(3)) then
          call prterr(lundia    ,'P004'    ,'No location in header '        )
          !
          goto 7777
       endif
       !------ Records in table
       if (.not.lkeyfd(16)) then
          call prterr(lundia    ,'P004'    ,'No number of records given '   )
          !
          goto 7777
       endif
       !
       !------ Check for facultative items and set defaults if required
       !
       !------ Reference time
       if (.not.lkeyfd(9)) then
          timref = itdate
       endif
       !------ Time unit
       if (.not.lkeyfd(10)) then
          timscl = 1.
       endif
       !------ Interpolation method
       if (.not.lkeyfd(12)) then
          inte20 = 'linear'
       endif
       !
       !------ Write header data to file
       !
       write (lunout) loca20
       write (lunout) inte20
       write (lunout) nrecs
       !
       !------ Initialise check values
       !
       ittdep = -1
       itold  = -1
       !
       !------ Read table data
       !
       do j = 1, nrecs
          icurec = icurec + 1
          read (lunrd, '(a)', err = 7777, end = 8888) crecrd
          stapos = 1
          endpos = len(crecrd)
          !
          !---------Read time.
          !
          lrec = len(crecrd)
          rdef = 0
          call read1r(crecrd    ,lrec      ,stapos    ,endpos    ,timrd     , &
                    & rdef      ,ierror   )
          !
          !---------Premature EOR (IER = 0) or empty value (IER = -1)
          !
          if (ierror<=0) goto 7777
          !
          !---------Re-defined time read (TIMRD ) in minutes
          !
          timrd = timrd*timscl
          !
          !-----------Perform some checks on time value read
          !
          call chckit(lundia    ,error     ,filinp    ,timrd     ,dt        , &
                    & ittdep    ,itold     ,itstrt    ,j         ,gdp       )
          !
          if (error) goto 7777
          !
          !---------Calculate Date and time for RTC
          !
          julday = ymd2jul(timref)
          call timdat(julday    ,timrd*60.0_fp,iacdat    ,iactim    )
          !
          !---------Re-define ITOLD
          !
          itold = ittdep
          !
          !---------Read height.
          !
          rdef = 0
          stapos = endpos + 1
          endpos = len(crecrd)
          call read1r(crecrd    ,lrec      ,stapos    ,endpos    ,height    , &
                    & rdef      ,ierror   )
          !
          !---------Premature EOR (IER = 0) or empty value (IER = -1)
          !
          if (ierror<=0) goto 7777
          !
          !------ Write record data to file
          !
          write (lunout) iacdat, iactim, real(height, sp)
       enddo
       !
       !------Define maximum time
       !
       if (itold/= - 1) then
          if (itold < itfinish) then
             write(errmsg,'(5a)') 'Last time in file ', trim(filinp), ' for location #',trim(loca20),'# <' 
             call prterr(lundia    ,'U042'    ,errmsg)
             error = .true.
             goto 9999
          endif
       endif
    enddo
    !
    !---- Goto Exit
    !
    goto 9999
    !
    !---- Reading error
    !
 7777 continue
    error = .true.
    errmsg = ' '
    write (crecrd, '(i12)') icurec
    call remove_leading_spaces(crecrd    ,lcurec    )
    errmsg = filinp(1:lninp) // ', Record: ' // crecrd(1:lcurec)
    call prterr(lundia    ,'G007'    ,errmsg    )
    !
    goto 9999
    !
    !---- (Sometimes not) Premature end of file
    !     If EOF found while trying to read first record of next table,
    !     EOF just means 'no more tables'
    !
 8888 continue
    if (.not.lnewtb) then
       error = .true.
       call prterr(lundia    ,'G006'    ,filinp(1:lninp)      )
    !
    endif
    !
    !---- Exit
    !
    !
    !---- If error destroy output file, otherwise close it.
    !
 9999 continue
    if (error) then
       close (lunout, status = 'delete')
    else
       close (lunout)
    endif
end subroutine rwbcb
