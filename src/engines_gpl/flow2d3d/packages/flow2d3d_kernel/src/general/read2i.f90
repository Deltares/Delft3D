subroutine read2i(lunmd     ,error     ,keyw      ,newkw     ,nlook     , &
                & record    ,ival      ,idef      ,defaul    ,nrrec     , &
                & ntrec     ,lundia    ,gdp       )
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
!    Function: Reads NLOOK integer values from a record of MD-
!              file. NLOOK may equal 0, which implies that the
!              nr. of record to be read is undetermined on input.
!              The values should be separated by blanks.
! Method used: - Look for the appropriate KEYW in the MD-file
!                If not found error = TRUE, ERRMSG is set and
!                the prog. is returned
!              - Read one integer at a time (READ1I)
!              - If   EOR , if   NLOOK = 1 -> error =TRUE
!                           else read new record
!                              If   not found
!                                   if not continuation line
!                                      if NLOOK>0 -> error
!                                   else increase NRREAD
!                                        read new value if req.
!                              else error
!                else if IER < 0 & NO DEFAULT -> error
!                else read anew solong as N < NLOOK OR NLOOK = 0
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
    integer , pointer :: ifis
    integer , pointer :: itis
!
! Global variables
!
    integer                                :: idef   !!  Default value when IVAR not found
    integer                                :: lundia !  Description and declaration in inout.igs
    integer                                :: lunmd  !  Description and declaration in inout.igs
    integer                   , intent(in) :: nlook  !!  Help var.: nr. of data to look for in the MD-file
    integer                                :: nrrec  !!  Pointer to the record number in the MD-file
    integer                                :: ntrec  !!  Help. var to keep track of NRREC
    integer     , dimension(*)             :: ival   !!  Help array (int.) where the data,
                                                     !!  recently read from the MD-file, are stored temporarily
    logical                   , intent(in) :: defaul !!  Flag set to YES if default value may be applied in case var. read is
                                                     !!  empty (ier <= 0, or nrread < nlook)
    logical                                :: error  !!  Flag=TRUE if an error is encountered
    logical                                :: newkw  !!  Logical var. specifying whether a new recnam should be read from the
                                                     !!  MD-file or just new data in the continuation line
    character(*)                           :: record !!  Record read from either the MD-file or from the attribute file
    character(6)                           :: keyw   !!  Name of record to look for in the MD-file (usually KEYWRD or RECNAM)
!
!
! Local variables
!
    integer       :: ibeg    ! Begin position in the RECORD from where the search for data/record is started 
    integer       :: iend    ! Last position in the RECORD when the searched data/record is finished 
    integer       :: ier     ! =  0 -> end of record encountered =  1 -> real value found = -1 -> length or number of data is larger then specified by the calling routine 
    integer       :: iocond  ! General FORTRAN IO-error cond. 
    integer       :: lkw     ! Length of char. str (usually the KEYWRD or RECNAM) 
    integer       :: lrec    ! Help var. containing the length of RECORD 
    integer       :: n       ! Help var. 
    logical       :: found   ! If FOUND = TRUE then recnam in the MD-file was found 
    character(4)  :: errornr ! error number (stored as characters) in case error occurred 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    ifis  => gdp%gdrdpara%ifis
    itis  => gdp%gdrdpara%itis
    !
    errornr = 'U036'
    lrec = len(record)
    lkw = index(keyw, ' ') - 1
    if (lkw<=0) lkw = 6
    found = .false.
    error = .false.
    !
    !-----look for keyword in the file
    !
    ibeg = itis
    iend = ifis - 1
    call search(lunmd     ,error     ,newkw     ,nrrec     ,found     , &
              & ntrec     ,record    ,ibeg      ,keyw      ,lkw       , &
              & 'NO'      )
    if (.not.found) then
       error = .true.
       errornr = 'U100'
       goto 999
    endif
    !
    !-----read value if ier <> 0 then no value not allowed =>
    !
    n = 0
    !===>
   10 continue
    n = n + 1
   20 continue
    ibeg = iend + 1
    call read1i(record    ,lrec      ,ibeg      ,iend      ,ival(n)   , &
              & idef      ,ier       )
    !
    !--------end of record (ier = 0), read new record if nread < nlook and
    !        nlook > 1
    !
    if (ier==0) then
       if (nlook==1) then
          error = .true.
       else
          read (lunmd, '(a300)', iostat = iocond) record
          !
          !--------------iocond = 0, read continuation record
          !                          ensure the record is all blanks before = sign
          !                     < 0, end of file encountered (not ok)
          !                     > 0, error occurred (not ok)
          !
          if (iocond==0) then
             nrrec = nrrec + 1
             if (record(:itis)/=' ') then
                if (nlook/=0) error = .true.
             else
                iend = ifis - 1
                if (nlook==0) goto 20
                n = n - 1
                if (n<nlook) goto 10
             endif
          else
             error = .true.
          endif
       endif
    elseif (ier<0 .and. .not.defaul) then
       !
       !-----------ival (n) = [] default value not allowed (ier = -1)
       !
       error = .true.
    else
       if (nlook==0) goto 10
       if (n<nlook) goto 10
    endif
  999 continue
    if (error) call prterr(lundia, errornr, keyw)
end subroutine read2i
