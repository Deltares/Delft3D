subroutine rdeva(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
               & runid     ,fileva    ,fmteva    ,dt        ,itstrt    , &
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
!    Function: - Reads the time dependent data for the rainfall/
!                evaporation module directly from the MD-file or
!                indirectly from the attribute file Fileva.
!              - Tests the file or data consistency.
!              - Checks whether the file exists or the required
!                data is not empty.
!              - An essential assumption is that the data has to
!                be specified sequentially in time. This imply
!                that NT times Rain/evaporation records
!                should exist in the file (NT unrestricted).
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: itis
!
! Global variables
!
    integer                                       :: itfinish   !  Description and declaration in inttim.igs
    integer                                       :: itstrt     !  Description and declaration in inttim.igs
    integer                         , intent(in)  :: lundia     !  Description and declaration in inout.igs
    integer                         , intent(in)  :: lunmd      !  Description and declaration in inout.igs
    integer                                       :: nrrec      !  Pointer to the record number in the MD-file
    logical                         , intent(out) :: error      !  Flag=TRUE if an error is encountered
    real(fp)                        , intent(in)  :: dt         !  Description and declaration in esm_alloc_real.f90
    character(*)                                  :: fileva     !  File name for the time varying rain/evaporation model file
    character(*)                                  :: mdfrec     !  Standard rec. length in MD-file (300)
    character(*)                                  :: runid      !  Run identification code for the current simulation (used to determine
                                                                !  the names of the in- /output files used by the system)
    character(2)                                  :: fmteva     !  File format for the time varying rain/evaporation model file
!
!
! Local variables
!
    integer                        :: iocond               ! IO status for reading 
    integer                        :: itold                ! Help var. to store last read time to test accending order 
    integer                        :: ittdep               ! Help var. for the time read (now de- fined as multiples of DT, but in fu- ture it may take any value) 
    integer                        :: l                    ! Help var. 
    integer                        :: lenc                 ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                        :: lf
    integer                        :: lkw                  ! Length (in characters) of keyword 
    integer                        :: lrid                 ! Length of character string runid 
    integer                        :: lunout               ! Unit number for unformatted FLOW help file between TDATOM and TRISIM 
    integer                        :: newlun
    integer                        :: nlook                ! Help var.: nr. of data to look for in the MD-file 
    integer                        :: nrval                ! Number of values to read from file 
    integer                        :: ntrec                ! Help. var to keep track of NRREC 
    logical                        :: dtn
    logical                        :: ex                   ! Flag to test if file exists 
    logical                        :: found                ! FOUND=TRUE if KEYW in the MD-file was found 
    logical                        :: lerror               ! Flag=TRUE if a local error is encountered 
    logical                        :: newkw                ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical                        :: nodef                ! Flag set to YES if default value may NOT be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical                        :: noread               ! Flag if FILBCC is equal to TMP file and should not be read. 
    logical                        :: rec1st               ! Flag set to TRUE if the record read is the first record 
    real(fp)                       :: rtime                ! Time associated with data
    real(fp)                       :: rdef                 ! Help var. containing default va- lue(s) for real variable 
    real(fp)                       :: rdummy
    real(fp), dimension(4)         :: rval                 ! Help array (real) where the data, recently read from the MD-file, are stored temporarily 
    character(11)                  :: fmtdef               ! Default file format (usually=blank) 
    character(11)                  :: fmttmp               ! Help variable for file format 
    character(12)                  :: fildef               ! Default file name (usually = blank) 
    character(256)                 :: filout               ! Help variable for file name 
    character(6)                   :: keyw                 ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
    character(300)                 :: message
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    itis  => gdp%gdrdpara%itis
    !
    lerror = .false.
    newkw  = .true.
    nodef  = .false.
    found  = .false.
    noread = .false.
    rec1st = .true.
    rdef   = 0.0
    nlook  = 1
    fildef = ' '
    filout = ' '
    fmtdef = 'FRformatted'
    fmttmp = ' '
    !
    lunout = 8
    !
    !-----initialize global parameters
    !
    fileva = fildef
    fmteva = 'FR'
    !
    !-----locate 'Fileva' record for time varying rainfall/evaporation data
    !     in attribute file
    !
    keyw = 'Fileva'
    ntrec = nrrec
    lkw = 6
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    !-----not found ?
    !
    if (found) then
       lenc = 12
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,fileva    ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       !
       !-------reading error?
       !
       if (lerror) then
          lerror = .false.
          fileva = fildef
       endif
    endif
    !
    !-----time varying evaporation/rain data in file? <YES>
    !     locate 'Fmteva' record for format definition of input file
    !
    if (fileva/=fildef) then
       fmttmp = fmtdef(3:)
       keyw = 'Fmteva'
       ntrec = nrrec
       lkw = 6
       call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                 & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                 & 'NO'      )
       lerror = .false.
       !
       !-------not found ?
       !
       if (found) then
          lenc = 2
          call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,fmttmp    ,fmtdef    ,lenc      ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          !
          !---------reading error?
          !
          if (lerror) then
             lerror = .false.
             fmttmp = fmtdef(3:)
          else
             call filfmt(lundia    ,keyw      ,fmttmp    ,lerror    ,gdp       )
             !
             if (lerror) then
                lerror = .false.
                fmttmp = fmtdef(3:)
             endif
          endif
       else
          fmttmp = fmtdef(3:)
       endif
       !
       fmteva = 'FR'
       if (fmttmp(:2)=='un') fmteva = 'UN'
       !
       !-------Check filename "fileva" <> TMP file or
       !       "fileva" = TMP file and access is unformatted
       !       Define length of RUNID
       !       open output file (ONLY VERSION 2.48 or lower) +
       !       Set name for the constituents, Regenerated locally
       !
       call remove_leading_spaces(runid     ,lrid      )
       filout = 'TMP_' // runid(:lrid) // '.eva'
       !
       !---------Check filename and format
       !         Value of FMTTMP will only be overwritten for FILOUT=FILEVA
       !         and FMTEVA='UN' which leads to NOREAD=.true. or error
       !
       if (filout==fileva) then
          !
          !-----------Check filename format
          !
          if (fmteva=='UN') then
             inquire (file = filout(:8 + lrid), exist = ex)
             if (.not.ex) then
                call prterr(lundia    ,'G004'    ,filout    )
                !
                error = .true.
                goto 9999
             endif
             !
             open (newunit=lunout, file = filout(:8 + lrid), form = 'unformatted')
             read (lunout, iostat = iocond) rdummy
             close (lunout)
             lunout = 8
             !
             !-------------FMTEVA='UN' but file formatted => error
             !
             if (iocond/=0) then
                call prterr(lundia    ,'U080'    ,filout    )
                !
                error = .true.
                goto 9999
             endif
             !
             !-------------FMTEVA='UN' => and file unformatted NOREAD=.true.
             !
             noread = .true.
          !
          !-----------FMTEVA='FR' => error
          !
          else
             call prterr(lundia    ,'U080'    ,filout    )
             !
             error = .true.
             goto 9999
          endif
       endif
       !
       !---------define length of file name
       !
       call remove_leading_spaces(fileva    ,lf        )
       !
       !---------Read data from file only in case .not.NOREAD
       !
       if (.not.noread) then
          !
          !-----------open unformatted eva-file
          !
          inquire (file = filout(:8 + lrid), exist = ex)
          if (ex) then
             open (newunit=lunout, file = filout(:8 + lrid))
             close (lunout, status = 'delete')
          endif
          open (newunit=lunout, file = filout(:8 + lrid), form = 'unformatted',      &
              & status = 'unknown')
          !
          write (message, '(2a)') 'Reading Evaporation & Rain file ', fileva(:lf)
          call prterr(lundia, 'G051', trim(message))
          nrval = 3
          call rdtdf(lundia    ,lunout    ,error     ,fileva    ,fmttmp    , &
                   & nrval     ,rval      ,dt        ,itstrt    ,itfinish  , &
                   & gdp       )
          if (error) goto 9999
       !
       else
          !
          !-------------Reading TDD file for evaporation and rain skipped in
          !             TDATOM Define "fake" timeframe
          !
          write (message, '(3a)') 'Evaporation & Rain file ', fileva(:lf), ' will be skipped in TDATOM'
          call prterr(lundia, 'G051', trim(message))
       endif
    !
    !-----time varying evaporation/rain data in file? <NO>
    !
    else
       !
       !---------Define length of RUNID
       !         open output file
       !
       call remove_leading_spaces(runid     ,lrid      )
       filout = 'TMP_' // runid(:lrid) // '.eva'
       !
       !---------Open file
       !
       inquire (file = filout(:8 + lrid), exist = ex)
       if (ex) then
          open (newunit=lunout, file = filout(:8 + lrid))
          close (lunout, status = 'delete')
       endif
       open (newunit=lunout, file = filout(:8 + lrid), form = 'unformatted',         &
            & status = 'unknown')
       !
       !-------time varying evaporation/rain data contains a group of records
       !       with keyword 'Tseva  '
       !
       rewind (lunmd)
       read (lunmd, '(a300)') mdfrec
       !
       !-------locate 'Tseva ' record
       !       first time NEWKW  = .true., next times NEWKW  = .false.
       !
       ittdep = -1
       itold  = -1
       !
       nrrec = 1
       ntrec = nrrec
       nlook = 4
       !
       ! -->
       !
  110  continue
       keyw = 'Tseva '
       lkw = 5
       call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
                 & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
                 & 'NO'      )
       if (lerror) then
          error = .true.
          call prterr(lundia    ,'U100'    ,keyw      )
          !
          goto 500
       endif
       !
       !---------not found ?
       !
       if (.not.found) then
          if (itold < itfinish) then
             write(message,'(a,a,a)') 'Last time of parameters for rain/evaporation model in file ', trim(fileva), ' <' 
             call prterr(lundia    ,'U042'    ,message  )
             !
             error = .true.
          endif
          goto 500
       endif
       !
       !---------read rval from the record that just has been read
       !
       newkw = .true.
       call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,rval      ,rdef      ,nodef     ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       !
       !---------reading error?
       !
       if (lerror) then
          error = .true.
          lerror = .false.
          goto 500
       endif
       !
       ! If an NaN is read -> error
       !
       do l = 1, nlook
           if (isnan(rval(l))) then
              write(message,'(a,a)') 'NaN in ', trim(fileva)
              call prterr(lundia    ,'P004'    ,message      )
              !
              error = .true.
              goto 9999
           endif
       enddo
       !
       rtime = rval(1)
       !
       ittdep = nint(rtime/dt)
       if (dtn(ittdep, rtime, dt)) then
          error = .true.
          call prterr(lundia    ,'U044'    ,'Tseva'   )
       !
       endif
       !
       !---------test times and define minimum time
       !
       if (rec1st) then
          if (ittdep>itstrt) then
             call prterr(lundia    ,'U041'    ,'First time Tseva >' )
             !
             error = .true.
          endif
          rec1st = .false.
       endif
       !
       if (ittdep<=itold) then
          call prterr(lundia    ,'U060'    ,'Tseva'   )
          !
          error = .true.
       endif
       !
       !---------writing to LUNOUT
       !
       write (lunout) (rval(l), l = 1, nlook)
       !
       !---------next time to read
       !
       itold = ittdep
       newkw = .false.
       !
       ! <--
       !
       goto 110
       !
       !-------stop reading
       !
       !
       !-------define actual number of times for time varying rainfall /
       !       evaporation data and define maximum time
       !
  500  continue
       if (itold/= - 1) then
          if (itold < itfinish) then
             write(message,'(a,a,a)') 'Last time in file ', trim(fileva), ' <' 
             call prterr(lundia    ,'U042'    ,message)
             error = .true.
             goto 9999
          endif
       endif
    endif
    !
    !-----close files
    !
 9999 continue
    if (lunout/=8) then
       if (error) then
          close (lunout, status = 'delete')
       else
          close (lunout)
       endif
    endif
end subroutine rdeva
