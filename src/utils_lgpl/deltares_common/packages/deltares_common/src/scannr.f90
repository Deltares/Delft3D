subroutine scannr(string    ,stapos    ,endpos    ,nrflds    ,itype     , &
                & ifield    ,rfield    ,cfield    ,lenchr    ,maxfld    , &
                & lconvu    ,lconv1    ,lconv2    )
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!    Function: Split string up into sub-strings driven by
!              spaces, tabs and quotes (' or ") and convert
!              sub-fields to integers or reals if possible and
!              required.
!              Strings delimited by " can contain ' and
!              strings delimited by ' can contain ".
!              Note: With the logicals LCONVU, LCONV1 and LCONV2
!                    it can be specified if unquoted strings,
!                    strings delimited by ' or strings delimited
!                    by " must be converted into integer or real
!                    if possible.
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Global variables
!
    integer                        , intent(in)  :: endpos !!  Endposition of scan
    integer                        , intent(in)  :: maxfld !!  Size of buffers (max number of sub-fields).
    integer                                      :: nrflds !!  Number of found sub-fields
                                                           !!  Error values:
                                                           !!  -1: One ore more parameters wrong
                                                           !!      STAPOS < 1;
                                                           !!      ENDPOS < STAPOS;
                                                           !!      ENDPOS > LEN(STRING);
                                                           !!      MAXFLD < 1.
                                                           !!  -2: More sub-fields than MAXFLD.
                                                           !!      There are more sub-fields than
                                                           !!      that there is space in ITYPE,
                                                           !!      IFIELD, RFIELD and CFIELD.
                                                           !!  -3: There is a character sub-field
                                                           !!      which is longer than the size
                                                           !!      of CFIELD.
                                                           !!  -4: Unmatching quotes.
    integer                        , intent(in)  :: stapos !!  Start position of scan
    integer     , dimension(maxfld), intent(out) :: ifield !!  Buffer integer sub-fields
    integer     , dimension(maxfld), intent(out) :: itype  !!  Sub-field descriptions
                                                           !!   1: INTEGER. Value in IFIELD.
                                                           !!   2: REAL. Value in RFIELD.
                                                           !!   3: CHARACTER. Text in CFIELD. The
                                                           !!      length is given in LENCHR.
    integer     , dimension(maxfld), intent(out) :: lenchr !!  Length of character sub-fields
    logical                        , intent(in)  :: lconv1 !!  Logical to determine if strings
                                                           !!  delimited by ' must be converted
                                                           !!  to integer or real if possible.
    logical                        , intent(in)  :: lconv2 !!  Logical to determine if strings
                                                           !!  delimited by " must be converted
                                                           !!  to integer or real if possible.
    logical                        , intent(in)  :: lconvu !!  Logical to determine if unquoted
                                                           !!  string must be converted
                                                           !!  to integer or real if possible.
    real(fp)    , dimension(maxfld), intent(out) :: rfield !!  Buffer real sub-fields
    character(*)                   , intent(in)  :: string !!  String to be scanned
    character(*), dimension(maxfld)              :: cfield !!  Buffer character sub-fields
!
! Local variables
!
    integer      :: chrlng
    integer      :: chrpos
    integer      :: endchr
    integer      :: i
    integer      :: nbrchr
    integer      :: stachr
    logical      :: lconv
    logical      :: lebyq
    logical      :: lfound
    logical      :: lqstr
    logical      :: parerr
    character(1) :: blank
    character(1) :: quote
    character(1) :: quote1
    character(1) :: quote2
    character(1) :: tab
    character(8) :: fmtr
!
!! executable statements -------------------------------------------------------
!
    ! initialisation
    !
    nrflds = 0
    blank  = ' '
    tab    = char(09)
    quote  = ' '
    quote1 = char(39)
    quote2 = char(34)
    !
    ! Clear sub-field buffers.
    !
    do i = 1, maxfld
       cfield(i) = ' '
       rfield(i) = 0.0
       ifield(i) = 0
       itype(i)  = 0
       lenchr(i) = 0
    enddo
    !
    ! test parameters
    !
    parerr = stapos<1 .or. endpos<stapos .or. endpos>len(string) .or. maxfld<1
    !
    ! if something wrong: return(-1)
    !
    if (parerr) then
       nrflds = -1
       goto 999
    endif
    !
    ! Get maximum length of character-subfields
    !
    chrlng = len(cfield(1))
    !
    ! Initialise local variables
    !
    stachr = -1
    endchr = -1
    lqstr  = .false.
    lebyq  = .false.
    !
    ! scan and convert the string-field
    !
    do chrpos = stapos, endpos + 1
       !
       ! Check if unquoted sub-field was ended by quote
       !
       if (lebyq) then
          stachr = chrpos - 1
          lebyq  = .false.
          lqstr  = .true.
       endif
       !
       ! Test on end of scan-string
       !
       if (chrpos > endpos) then
          endchr = endpos - 1
          if (lqstr) then
             !
             ! End of scan found in quote-mode.
             ! That means no matching quote found.
             !
             nrflds = -4
             exit
          endif
       elseif (.not.lqstr) then
          if (     string(chrpos:chrpos)==quote1 &
            & .or. string(chrpos:chrpos)==quote2  )then
             !
             ! Set active quote to found quote
             !
             quote = string(chrpos:chrpos)
             if (chrpos < endpos) then
                if (stachr > 0) then
                   !
                   ! Unquoted string terminated by quote
                   !
                   endchr = chrpos - 1
                   lebyq  = .true.
                   quote  = string(chrpos:chrpos)
                else
                   !
                   ! Quote found, go into quoted string mode
                   !
                   stachr = chrpos
                   lqstr  = .true.
                endif
             else
                !
                ! Quote found on last position, can never match
                !
                nrflds = -4
                exit
             endif
          elseif (     string(chrpos:chrpos)==blank &
                & .or. string(chrpos:chrpos)==tab    )then
             if (stachr > 0) then
                !
                ! End of unquoted sub-string found
                !
                endchr = chrpos - 1
             endif
          elseif (      string(chrpos:chrpos)/=blank &
                & .and. string(chrpos:chrpos)/=tab    )then
             if (stachr == -1) then
                !
                ! Start of unquoted sub-string found
                !
                stachr = chrpos
             endif
          else
          endif
       elseif (string(chrpos:chrpos) == quote) then
          !
          ! Matching quote found
          !
          endchr = chrpos
       else
       endif
       !
       ! Check if substring found, if so process it
       !
       if (stachr>0 .and. endchr>0) then
          !
          ! In case of Quote-delimited string, exclude the quotes
          ! and determine length of found sub-field
          !
          if (lqstr) then
             stachr = stachr + 1
             endchr = endchr - 1
          endif
          nbrchr = endchr - stachr + 1
          !
          ! If length of sub-field is zero (only will happen
          ! in case of two successive quotes) no field has
          ! to be added, field is emty!!
          !
          if (nbrchr > 0) then
             !
             ! Test if maximum number of allowed fields is exceeded
             !
             if (nrflds == maxfld) then
                nrflds = -2
                exit
             endif
             nrflds = nrflds + 1
             !
             ! Determine if conversion is required
             !
             if (lqstr) then
                if (     (quote=='''' .and. lconv1) &
                  & .or. (quote=='"'  .and. lconv2)  )then
                   lconv = .true.
                else
                   lconv = .false.
                endif
             else
                !
                ! As given for unquoted strings
                !
                lconv = lconvu
             endif
             lfound = .false.
             if (lconv) then
                !
                ! Try if it is an integer
                !
                fmtr = '(ixxx)'
                write (fmtr(3:5), '(i3.3)') nbrchr
                read (string(stachr:endchr), fmtr, err = 111) ifield(nrflds)
                lfound        = .true.
                itype(nrflds) = 1
                !
                ! If no integer, try a real
                !
  111           continue
                if (.not.lfound) then
                   fmtr = '(gxxx.0)'
                   write (fmtr(3:5), '(i3.3)') nbrchr
                   read (string(stachr:endchr), fmtr, err = 222) rfield(nrflds)
                   lfound        = .true.
                   itype(nrflds) = 2
                else
                   !
                   ! Provide integer data also as real data
                   !
                   rfield(nrflds) = real(ifield(nrflds),fp)
                endif
             endif
             !
             ! If no real as well, take it as character string
             !
  222        continue
             if (.not.lfound) then
                !
                ! Character field, first check on length
                !
                if (nbrchr > chrlng) then
                   nrflds = -3
                   exit
                endif
                itype (nrflds) = 3
                lenchr(nrflds) = nbrchr
                !
                ! Copy character sub-field to output-field
                !
                cfield(nrflds) = string(stachr:endchr)
             else
                !
                ! Provide integer and real data also as string
                ! limit to maximum space, don't generate error!
                !
                if (nbrchr > chrlng) then
                   lenchr(nrflds) = chrlng
                   cfield(nrflds) = string(stachr:stachr+chrlng-1)
                else
                   lenchr(nrflds) = nbrchr
                   cfield(nrflds) = string(stachr:endchr)
                endif
             endif
          endif
          !
          ! Reset substring locations and quote-mode to
          ! be ready for searching for the next sub-field.
          !
          stachr = -1
          endchr = -1
          lqstr  = .false.
       endif
    enddo
    !
    ! exit-label
    !
  999 continue
end subroutine scannr
