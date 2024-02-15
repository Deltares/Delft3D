subroutine drofil(lundia    ,fildro    ,fmttmp    ,error     ,ndro      , &
                & dt        ,namdro    ,mndro     ,itdro     ,dxydro    , &
                & drodep    ,gdp       )
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
!    Function: Reads the drogue definitions from attri-
!              bute file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
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
    integer                                        :: lundia !  Description and declaration in inout.igs
    integer                          , intent(in)  :: ndro   !  Description and declaration in dimens.igs
    integer      , dimension(2, ndro)              :: itdro  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(2, ndro)              :: mndro  !  Description and declaration in esm_alloc_int.f90
    logical                                        :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                                       :: dt     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(2, ndro), intent(out) :: dxydro !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(ndro)   , intent(out) :: drodep
    character(*)                                   :: fildro !!  Name of the relevant file
    character(11)                    , intent(in)  :: fmttmp !!  Help var. for the attribute file formats (eg. the grid file)
    character(20), dimension(ndro)                 :: namdro !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                :: i        ! Loop variable 
    integer                :: ibeg     ! Begin position in the RECORD from where the search for data/record is started 
    integer                :: iend     ! Last position in the RECORD when the searched data/record is finished 
    integer                :: ier      ! =  0 -> end of record encountered =  1 -> real value found = -1 -> length or number of data is larger then specified by the calling routine 
    integer                :: iocond   ! IO status for reading 
    integer                :: it
    integer                :: lfile    ! Number of non blank characters of file name 
    integer                :: lr132    ! Standard length of a record in the attribute file = 132 
    integer                :: luntmp   ! Temporary file unit 
    integer                :: n
    integer, external      :: newlun
    logical                :: dtn
    real(fp)               :: rdef     ! Help var. containing default va- lue(s) for real variable 
    real(fp)               :: t
    real(fp), dimension(4) :: rval     ! Help array (real) where the data, recently read from the MD-file, are stored temporarily 
    character(10)          :: errmsg   ! Text string for error message 
    character(132)         :: rec132   ! Standard rec. length in an attribute file (132) 
    character(20)          :: cdef     ! Default value when CVAR not found 
    character(20)          :: chulp    ! Help var. 
!
!! executable statements -------------------------------------------------------
!
    ! initialize local parameters
    !
    cdef  = ' '
    chulp = cdef
    rdef  = 0.0
    lr132 = 132
    !
    ! test file existence
    !
    lfile = len(fildro)
    !
    error = .not.exifil(fildro, lundia)
    if (error) goto 9999
    !
    open (newunit=luntmp, file = fildro(:lfile), form = fmttmp, status = 'old')
    !
    ! unformatted file
    ! read input and test iocond
    !
    if (fmttmp(1:2)=='un') then
       ! -->
       do n = 1, ndro
          read (luntmp, iostat = iocond) chulp, (rval(i), i = 1, 4)
          if (iocond/=0) then
             if (iocond<0) then
                call prterr(lundia    ,'G006'    ,fildro(:lfile)       )
             else
                call prterr(lundia    ,'G007'    ,fildro(:lfile)       )
             endif
             error = .true.
             exit
          endif
          !
          ! define drogue name
          !
          namdro(n) = chulp
          !
          ! there must be a name defined !!
          !
          if (namdro(n)==cdef) then
             error = .true.
             call prterr(lundia    ,'V034'    ,' '       )
             exit
          endif
          !
          ! define timestep numbers
          !
          itdro(1, n) = nint(rval(1)/dt)
          itdro(2, n) = nint(rval(2)/dt)
          if (dtn(itdro(1, n), rval(1), dt)) then
             error  = .true.
             errmsg = 'start time'
             call prterr(lundia    ,'U044'    ,errmsg    )
             write (lundia, '(a,a)') ' for drogue: ', namdro(n)
          endif
          if (dtn(itdro(2, n), rval(2), dt)) then
             error  = .true.
             errmsg = 'stop  time'
             call prterr(lundia    ,'U044'    ,errmsg    )
             write (lundia, '(a,a)') ' for drogue: ', namdro(n)
          endif
          if (error) then
             exit
          endif
          !
          ! xy-drogue is defined as real m,n coordinates
          ! hence MNDRO = int (RVAL) and DXYDRO = RVAL - MNDRO
          !
          mndro (1, n) = int(rval(3))
          dxydro(1, n) = rval(3) - mndro(1, n)
          mndro (2, n) = int(rval(4))
          dxydro(2, n) = rval(4) - mndro(2, n)
       enddo
       !
       ! close file
       !
       close (luntmp)
    else
       !
       ! freeformatted file, skip lines starting with a '*'
       !
       call skipstarlines(luntmp    )
       !
       ! read input and test iocond
       !
       do n = 1, ndro
          read (luntmp, '(a)', iostat = iocond) rec132
          if (iocond/=0) then
             if (iocond<0) then
                call prterr(lundia    ,'G006'    ,fildro(:lfile)       )
             else
                call prterr(lundia    ,'G007'    ,fildro(:lfile)       )
             endif
             error = .true.
             exit
          endif
          !
          ! define drogue name
          !
          namdro(n) = rec132(:20)
          !
          ! there must be a name defined !!
          !
          if (namdro(n)==cdef) then
             error = .true.
             call prterr(lundia    ,'V034'    ,' '       )
             exit
          endif
          !
          ! read rval  (4) from record, default value allowed
          !
          iend = 20
          do i = 1, 4
             ibeg = iend + 1
             call read1r(rec132    ,lr132     ,ibeg      ,iend      ,rval(i)   , &
                       & rdef      ,ier       )
             if (ier<=0) then
                call prterr(lundia    ,'G007'    ,fildro(:lfile)       )
                error = .true.
                goto 300
             endif
          enddo
          !
          ! define timestep numbers
          !
          itdro(1, n) = nint(rval(1)/dt)
          itdro(2, n) = nint(rval(2)/dt)
          if (dtn(itdro(1, n), rval(1), dt)) then
             error  = .true.
             errmsg = 'start time'
             call prterr(lundia    ,'U044'    ,errmsg    )
             write (lundia, '(a,a)') ' for drogue: ', namdro(n)
          endif
          if (dtn(itdro(2, n), rval(2), dt)) then
             error  = .true.
             errmsg = 'stop  time'
             call prterr(lundia    ,'U044'    ,errmsg    )
             write (lundia, '(a,a)') ' for drogue: ', namdro(n)
          endif
          if (error) then
             exit
          endif
          !
          ! xy-drogue is defined as real m,n coordinates
          ! hence MNDRO = int (RVAL) and DXYDRO = RVAL - MNDRO
          !
          mndro (1, n) = int(rval(3))
          dxydro(1, n) = rval(3) - mndro(1, n)
          mndro (2, n) = int(rval(4))
          dxydro(2, n) = rval(4) - mndro(2, n)
          !
          ! Read depth of the drogue, no value present ==> depro = 0.
          !
          ibeg = iend + 1
          call read1r(rec132    ,lr132     ,ibeg      ,iend      ,rval(1)   , &
                    & rdef      ,ier       )
          if (ier <= 0) then
             drodep(n) = 0.
          else
             drodep(n) = rval(1)
          endif
       enddo
       !
       ! close file
       !
  300  continue
       close (luntmp)
    endif
 9999 continue
end subroutine drofil
