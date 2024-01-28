subroutine tdfil(lundia    ,luntd     ,error     ,filtd     ,fmttmp    , &
               & fltd      ,gdp       )
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
!   Reads the Thin dames from the attribute file
!   Flag fltd is set and returned
! OR
!   Reads the (general) cut cells from the attribute file
!   Flag fltd in this routine corresponds to flag flcut in the calling routine
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use dfparall
    use system_utils, only: exifil
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer, pointer      :: mfg
    integer, pointer      :: nfg
!
! Global variables
!
    integer                    :: lundia !  Description and declaration in inout.igs
    integer      , intent(in)  :: luntd  !  Description and declaration in luntmp.igs
    logical      , intent(out) :: fltd   !  Description and declaration in tmpfil.igs
    logical                    :: error  !!  Flag=TRUE if an error is encountered
    character(*)               :: filtd  !!  Name of the relevant file
    character(11), intent(in)  :: fmttmp !!  Help var. for the attribute file formats (eg. the thin dams file)
!
! Local variables
!
    integer               :: ibeg   ! Begin position in the RECORD from where the search for data/record is started 
    integer               :: idef   ! Help var. containing default va- lue(s) for integer variable 
    integer               :: iend   ! Last position in the RECORD when the searched data/record is finished 
    integer               :: ier    ! =  0 -> end of record encountered =  1 -> real value found = -1 -> length or number of data is larger then specified by the calling routine 
    integer               :: imnt   ! counter to count thin dam points inside subdomain
    integer               :: iocond ! Help var. for iostat condition 
    integer               :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer               :: lfile  ! Length of file name 
    integer               :: luntmp ! Unit number for attribute file 
    integer               :: m      ! Help var. 
    integer, external     :: newlun
    integer               :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer, dimension(4) :: ival   ! Help array (int.) where the data, recently read from the MD-file, are stored temporarily 
    logical               :: outsd  ! indicating whether a line of thin dams is outside subdomain (.TRUE.) or not (.FALSE.)
    logical               :: onParbndIsInside
    character(1)          :: dirtd  ! Velocity points on which the thin dams have been specified (U or V) 
    character(132)        :: rec132 ! Standard rec. length in an attribute file (132) 
!
!! executable statements -------------------------------------------------------
!
    mfg => gdp%gdparall%mfg
    nfg => gdp%gdparall%nfg
    ! initialisation
    !
    lenc  = 1
    nlook = 4
    idef  = 0
    onParbndIsInside = .true.
    !
    ! test file existence, if so read
    !
    lfile = len(filtd)
    !
    if (exifil(filtd, lundia)) then
       open (newunit=luntmp, file = filtd(1:lfile), form = fmttmp, status = 'old')
       !
       ! unformatted file
       !
       if (fmttmp(:2)=='un') then
       !
       ! read ival  and write to semi-scratch file
       ! end_of_file ok, errorcondition not ok
       !
       ! -->
          imnt = 0
  110     continue
          read (luntmp, iostat = iocond) (ival(m), m = 1, 4), dirtd
          if (iocond/=0) then
             if (iocond>0) then
                error = .true.
                call prterr(lundia    ,'G007'    ,filtd(1:lfile)       )
             endif
             goto 200
          endif
          ival(1) = ival(1) - mfg + 1
          ival(2) = ival(2) - nfg + 1
          ival(3) = ival(3) - mfg + 1
          ival(4) = ival(4) - nfg + 1
          !
          ! check if thin dams are fully (.TRUE.) or partly (.FALSE.) outside subdomain
          !
          ! Note: for single domain runs, outsd = .FALSE., i.e. thin dams are completely inside domain
          !
          call adjlin (ival, outsd, gdp%d%mmax, gdp%d%nmaxus, onParbndIsInside)
          if ( .not. outsd ) then
             imnt = imnt + 1
             write (luntd) (ival(m), m = 1, 4), dirtd
          endif
          goto 110
       ! <--
       !
       ! stop reading file
       !
       !
       ! freeformatted file
       !
       else
          !
          ! skip lines starting with a '*'
          !
          call skipstarlines(luntmp    )
          !
          ! read ival  and write to semi-scratch file
          ! end_of_file ok, errorcondition not ok
          !
          ! -->
          imnt = 0
  210     continue
          read (luntmp, '(a)', iostat = iocond) rec132
          if (iocond/=0) then
             if (iocond>0) then
                error = .true.
                call prterr(lundia    ,'G007'    ,filtd(1:lfile)       )
             endif
             goto 200
          endif
          !
          ! read ival  and write to semi-scratch file
          !
          ibeg = 1
          iend = 132
          call readni(rec132    ,132       ,ibeg      ,iend      ,nlook     , &
                    & ival      ,idef      ,ier       )
          if (ier<=0) then
             error = .true.
             call prterr(lundia    ,'G007'    ,filtd(1:lfile)       )
             !
             goto 200
          endif
          !
          ! read dirtd from record
          ! string to long and no value not allowed => ier > 0
          !
          ibeg = iend + 1
          call read1c(rec132    ,132       ,ibeg      ,iend      ,dirtd     , &
                    & lenc      ,ier       )
          if (ier<=0) then
             error = .true.
             call prterr(lundia    ,'G007'    ,filtd(1:lfile)       )
             goto 200
          endif
          !
          ! write mntd and dirtd to semi-scratch file
          !
          ival(1) = ival(1) -mfg +1
          ival(2) = ival(2) -nfg +1
          ival(3) = ival(3) -mfg +1
          ival(4) = ival(4) -nfg +1
          !
          ! check if thin dams are fully (.TRUE.) or partly (.FALSE.) outside subdomain
          !
          ! Note: for single domain runs, outsd = .FALSE., i.e. thin dams are completely inside domain
          !
          call adjlin (ival, outsd, gdp%d%mmax, gdp%d%nmaxus, onParbndIsInside)
          if ( .not. outsd ) then
             imnt = imnt + 1
             write (luntd) (ival(m), m = 1, 4), dirtd
          endif
          goto 210
       ! <--
       !
       ! stop reading file
       !
       endif
       !
       ! close file and set flag
       !
  200  continue
       close (luntmp)
       !
       fltd = .true.
       if (error .or. imnt==0) fltd = .false.
    else
       !
       ! test file existence <NO>
       !
       error = .true.
    endif
end subroutine tdfil
