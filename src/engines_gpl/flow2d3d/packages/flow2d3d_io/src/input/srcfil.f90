subroutine srcfil(lundia    ,filsrc    ,error     ,nsrc      ,mnksrc    , &
                & namsrc    ,disint    ,gdp       )
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
!    Function: Reads the discharge location definitions from the
!              attribute file
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
    real(fp), pointer :: maxTOutlet
!
! Global variables
!
    integer                                        :: lundia !  Description and declaration in inout.igs
    integer                          , intent(in)  :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(7, nsrc), intent(out) :: mnksrc !  Description and declaration in esm_alloc_int.f90
    logical                                        :: error  !  Flag=TRUE if an error is encountered
    character(*)                                   :: filsrc !  Name of the relevant file
    character(1) , dimension(nsrc)                 :: disint !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nsrc)                 :: namsrc !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                :: ibeg    ! Begin position in the RECORD from where the search for data/record is started 
    integer                :: idef    ! Help var. containing default va- lue(s) for integer variable 
    integer                :: iend    ! Last position in the RECORD when the searched data/record is finished 
    integer                :: ier     ! =  0 -> end of record encountered =  1 -> real value found = -1 -> length or number of data is larger then specified by the calling routine 
    integer                :: iocond  ! IO status for reading 
    integer                :: lenc
    integer                :: lfile   ! Number of non blank characters of file name 
    integer                :: lr132   ! Standard length of a record in the attribute file = 132 
    integer                :: luntmp  ! Temporary file unit 
    integer                :: n
    integer, external      :: newlun
    integer , dimension(3) :: ival    ! Help array (integer) where the data, recently read from the MD-file, are stored temporarily
    real(fp)               :: rdef
    real(fp), dimension(1) :: rval
    logical                :: qType   ! True when there is at least one Q-type powerstation present
    character(1)           :: cdefi   ! Default value for interpolotion (Y) 
    character(1)           :: chulpi  ! Help string for reading interpolation 
    character(20)          :: cdefn   ! Default value when CVAR not found 
    character(132)         :: rec132  ! Standard rec. length in an attribute file (132) 
    character(300)         :: message
!
!! executable statements -------------------------------------------------------
!
    maxTOutlet => gdp%gddischarge%maxTOutlet
    !
    ! initialize local parameters
    !
    cdefn  = ' '
    cdefi  = 'Y'
    chulpi = cdefi
    idef   = 0
    rdef   = -999.0_fp
    lr132  = 132
    lenc   = 1
    qType  = .false.
    !
    ! test file existence
    !
    call remove_leading_spaces(filsrc    ,lfile     )
    !
    error = .not.exifil(filsrc, lundia)
    if (error) goto 9999
    !
    ! open formatted file, if not formatted IOCOND <> 0
    !
    open (newunit=luntmp, file = filsrc(:lfile), form = 'formatted', status = 'old',    &
        & iostat = iocond)
    if (iocond /= 0) then
       error = .true.
       call prterr(lundia    ,'G007'    ,filsrc(:lfile)       )
       goto 9999
    endif
    !
    ! freeformatted file, skip lines starting with a '*'
    !
    call skipstarlines(luntmp    )
    !
    ! freeformatted file, read input and test iocond
    !
    do n = 1, nsrc
       read (luntmp, '(a)', iostat = iocond) rec132
       if (iocond /= 0) then
          if (iocond < 0) then
             call prterr(lundia    ,'G006'    ,filsrc(:lfile)       )
          else
             call prterr(lundia    ,'G007'    ,filsrc(:lfile)       )
          endif
          error = .true.
          exit
       endif
       !
       ! define discharge location name
       !
       namsrc(n) = rec132(:20)
       !
       ! there must be a name defined !!
       !
       if (namsrc(n) == cdefn) then
          error = .true.
          call prterr(lundia    ,'V012'    ,' '       )
          exit
       endif
       !
       ! read DISINT from record, default value not allowed
       !
       lenc = 1
       ibeg = 21
       call read1c(rec132    ,lr132     ,ibeg      ,iend      ,chulpi    , &
                 & lenc      ,ier       )
       if (ier <= 0) then
          call prterr(lundia    ,'G007'    ,filsrc(1:lfile)      )
          disint(n) = cdefi
          error     = .true.
          exit
       endif
       disint(n) = chulpi
       !
       ! test for interpolation option is 'Y'
       !
       if (disint(n) == 'n') then
          disint(n) = 'N'
       endif
       if (disint(n) /= 'N') then
          disint(n) = cdefi(:1)
       endif
       !
       ! read ival  (3) from record, default value allowed
       !
       ibeg = iend + 1
       call readni(rec132    ,lr132     ,ibeg      ,iend      ,3         , &
                 & ival      ,idef      ,ier       )
       if (ier <= 0) then
          call prterr(lundia    ,'G007'    ,filsrc(1:lfile)      )
          mnksrc(1, n) = idef
          mnksrc(2, n) = idef
          mnksrc(3, n) = idef
          mnksrc(4, n) = idef
          mnksrc(5, n) = idef
          mnksrc(6, n) = idef
          error = .true.
          exit
       endif
       !
       ! discharge location is defined in M,N,K coordinates
       !
       mnksrc(1, n) = ival(1)
       mnksrc(2, n) = ival(2)
       mnksrc(3, n) = ival(3)
       mnksrc(4, n) = ival(1)
       mnksrc(5, n) = ival(2)
       mnksrc(6, n) = ival(3)
       !
       ! discharge location is walking discharge or power station
       !
       lenc = 1
       ibeg = iend + 1
       call read1c(rec132    ,lr132     ,ibeg      ,iend      ,chulpi    , &
                 & lenc      ,ier       )
       !
       ! if error go on (probably Normal discharge point)
       !
       if (ier <= 0) then
          chulpi = 'N'
       endif
       mnksrc(7, n) = 0
       !
       ! test if the discharge is walking discharge, powerstation or culvert
       !
       if (chulpi=='w' .or. chulpi=='W') mnksrc(7, n) = 1
       if (chulpi=='p' .or. chulpi=='P' .or. &
         & chulpi=='q' .or. chulpi=='Q' .or. &
         & chulpi=='c' .or. chulpi=='C' .or. &
         & chulpi=='d' .or. chulpi=='D' .or. &
         & chulpi=='e' .or. chulpi=='E' .or. &
         & chulpi=='f' .or. chulpi=='F' .or. &
         & chulpi=='u' .or. chulpi=='U') then
          !
          ! read ival  (3) from record, default value allowed
          !
          ibeg = iend + 1
          call readni(rec132    ,lr132     ,ibeg      ,iend      ,3         , &
                    & ival      ,idef      ,ier       )
          if (ier <= 0) then
             write(message,'(2a)') 'Missing second set of coordinates in file ', trim(filsrc)
             call prterr(lundia, 'G007', trim(message))
             mnksrc(4, n) = idef
             mnksrc(5, n) = idef
             mnksrc(6, n) = idef
             error = .true.
             exit
          endif
          mnksrc(4, n) = ival(1)
          mnksrc(5, n) = ival(2)
          mnksrc(6, n) = ival(3)
          if (chulpi=='p' .or. chulpi=='P') mnksrc(7, n) = 2
          if (chulpi=='c' .or. chulpi=='C') mnksrc(7, n) = 3
          if (chulpi=='e' .or. chulpi=='E') mnksrc(7, n) = 4
          if (chulpi=='d' .or. chulpi=='D') mnksrc(7, n) = 5
          if (chulpi=='q' .or. chulpi=='Q') then
             qType = .true.
             mnksrc(7, n) = 6
             ibeg = iend + 1
             call readnr(rec132    ,lr132     ,ibeg      ,iend      ,1         , &
                       & rval      ,rdef      ,ier       )
             if (ier <= 0) then
                write(message,'(2a)') 'Missing regular temperature difference for Q-type power station in file ', trim(filsrc)
                call prterr(lundia, 'G007', trim(message))
                error = .true.
                exit
             else
                maxTOutlet = rval(1)
             endif
          endif
          if (chulpi=='u' .or. chulpi=='U') mnksrc(7, n) = 7
          if (chulpi=='f' .or. chulpi=='F') mnksrc(7, n) = 8
       endif
    enddo
    !
    ! close file
    !
    close (luntmp)
 9999 continue
    if (qType) then
       !
       ! Allocate array for q-type powerstations
       !
       if (associated(gdp%gddischarge%capacity)) deallocate(gdp%gddischarge%capacity, stat=ier)
       allocate (gdp%gddischarge%capacity(nsrc) , stat=ier)
       if (ier /= 0) then
          call prterr(lundia, 'U021', 'srcfil: memory alloc error for capacity array')
          call d3stop(1, gdp)
       endif
       gdp%gddischarge%capacity = 0.0_fp
    endif
end subroutine srcfil
