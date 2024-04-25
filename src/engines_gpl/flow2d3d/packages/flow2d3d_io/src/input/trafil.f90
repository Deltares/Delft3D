subroutine trafil(lundia    ,filtra    ,fmttmp    ,error     ,ntruv     , &
                & namtra    ,mnit      ,gdp       )
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
!    Function: Reads the monitoring cross-section definitions
!              from the attribute file
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
    integer                                           :: lundia  !  Description and declaration in inout.igs
    integer                             , intent(in)  :: ntruv   !  Description and declaration in dimens.igs
    integer       , dimension(4, ntruv) , intent(out) :: mnit    !  Description and declaration in esm_alloc_int.f90
    logical                                           :: error   !!  Flag=TRUE if an error is encountered
    character(*)                                      :: filtra  !!  Name of the relevant file
    character(11)                       , intent(in)  :: fmttmp  !!  Help var. for the attribute file formats (eg. the grid file)
    character(20) , dimension(ntruv)                  :: namtra  !  Description and declaration in esm_alloc_char.f90
!
!
! Local variables
!
    integer                        :: i             ! Loop variable 
    integer                        :: ibeg          ! Begin position in the RECORD from where the search for data/record is started 
    integer                        :: idef          ! Help var. containing default va- lue(s) for integer variable 
    integer                        :: iend          ! Last position in the RECORD when the searched data/record is finished 
    integer                        :: ier           ! =  0 -> end of record encountered =  1 -> real value found = -1 -> length or number of data is larger then specified by the calling routine 
    integer                        :: iocond        ! IO status for reading 
    integer                        :: lfile         ! Number of non blank characters of file name 
    integer                        :: lr132         ! Standard length of a record in the attribute file = 132 
    integer                        :: luntmp        ! Temporary file unit 
    integer                        :: n
    integer, external              :: newlun
    integer        , dimension(4)  :: ival          ! Help array (integer) where the data, recently read from the MD-file, are stored temporarily 
    character(132)                 :: rec132        ! Standard rec. length in an attribute file (132) 
    character(20)                  :: cdef          ! Default value when CVAR not found 
    character(20)                  :: chulp         ! Help var. 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----initialize local parameters
    !
    cdef = ' '
    chulp = cdef
    idef = 0
    lr132 = 132
    !
    !-----test file existence
    !
    lfile = len(filtra)
    !
    error = .not.exifil(filtra, lundia)
    if (error) goto 9999
    !
    open (newunit=luntmp, file = filtra(:lfile), form = fmttmp, status = 'old')
    !
    !-----unformatted file
    !     read input and test iocond
    !
    if (fmttmp(1:2)=='un') then
       ! -->
       do n = 1, ntruv
          read (luntmp, iostat = iocond) chulp, (ival(i), i = 1, 4)
          if (iocond/=0) then
             if (iocond<0) then
                call prterr(lundia    ,'G006'    ,filtra(:lfile)       )
             !
             else
                call prterr(lundia    ,'G007'    ,filtra(:lfile)       )
             !
             endif
             error = .true.
             exit
          endif
          !
          !-----------define monitoring cross-section name
          !
          namtra(n) = chulp
          !
          !-----------there must be a name defined !!
          !
          if (namtra(n)==cdef) then
             error = .true.
             call prterr(lundia    ,'V014'    ,' '       )
             !
             exit
          endif
          !
          !-----------monitoring cross-section is defined in M,N coordinates
          !
          mnit(1, n) = ival(1)
          mnit(2, n) = ival(2)
          mnit(3, n) = ival(3)
          mnit(4, n) = ival(4)
       enddo
       !
       !--------close file
       !
       close (luntmp)
    else
       !
       !-----freeformatted file, skip lines starting with a '*'
       !
       call skipstarlines(luntmp    )
       !
       !-----freeformatted file, read input and test iocond
       !
       do n = 1, ntruv
          read (luntmp, '(a)', iostat = iocond) rec132
          if (iocond/=0) then
             if (iocond<0) then
                call prterr(lundia    ,'G006'    ,filtra(:lfile)       )
             !
             else
                call prterr(lundia    ,'G007'    ,filtra(:lfile)       )
             !
             endif
             error = .true.
             exit
          endif
          !
          !-----------define monitoring cross-section name
          !
          namtra(n) = rec132(:20)
          !
          !-----------there must be a name defined !!
          !
          if (namtra(n)==cdef) then
             error = .true.
             call prterr(lundia    ,'V014'    ,' '       )
             !
             exit
          endif
          !
          !-----------read ival  (4) from record, default values not allowed
          !
          iend = 20
          do i = 1, 4
             ibeg = iend + 1
             call read1i(rec132    ,lr132     ,ibeg      ,iend      ,ival(i)   , &
                       & idef      ,ier       )
             if (ier<=0) then
                call prterr(lundia    ,'G007'    ,filtra(:lfile)       )
                !
                error = .true.
                goto 300
             endif
          enddo
          !
          !-----------monitoring cross-section defined in M,N coordinates
          !
          mnit(1, n) = ival(1)
          mnit(2, n) = ival(2)
          mnit(3, n) = ival(3)
          mnit(4, n) = ival(4)
       enddo
       !
       !--------close file
       !
  300  continue
       close (luntmp)
    endif
    !
 9999 continue
end subroutine trafil
