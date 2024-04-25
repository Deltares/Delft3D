module string_module
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
!    Function: - Various string processing routines
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------

   implicit none

   private

   !
   ! functions and subroutines
   !
   public :: string_module_info
   public :: str_token
   public :: str_tolower
   public :: str_lower
   public :: str_toupper
   public :: str_upper
   public :: strcmpi
   public :: trimexact
   public :: remove_leading_spaces
   public :: remove_all_spaces
   public :: replace_multiple_spaces_by_single_spaces
   public :: find_first_word
   public :: find_first_letter
   public :: find_first_char
   public :: count_words
   public :: remove_substr
   public :: remove_chars
   public :: replace_char
   public :: replace_string
   public :: splitstr
   public :: strsplit
   public :: char_array_to_string_by_len
   public :: strip_quotes
   public :: real2string, real2stringLeft
   public :: GetLine
   public :: int2str

   interface strip_quotes
      module procedure strip_quotes1
      module procedure strip_quotes2
   end interface strip_quotes

   contains

      ! ------------------------------------------------------------------------------
      !   Subroutine: string_module_info
      !   Purpose:    Add info about this string module to the messages stack
      !   Summary:    Add id string and URL
      !   Arguments:
      !   messages    Stack of messages to add the info to
      ! ------------------------------------------------------------------------------
      subroutine string_module_info(messages)
          use message_module
          !
          ! Arguments
          !
          type(message_stack), pointer :: messages
          !
          !! executable statements ---------------------------------------------------
          !
          call addmessage(messages,'')
          call addmessage(messages,'$URL$')
      end subroutine string_module_info



      ! ------------------------------------------------------------------------------
      !   Subroutine: str_token
      !   Purpose:    Obtain first token from string
      !   Summary:    Scan string for non-delimiter (e.g. non-space) characters and
      !               return first set found.
      !   Arguments:
      !   string      on input : String to be scanned
      !               on output: Remainder of string
      !   token       on output: String containing token
      !   quote       on input : Optional quote character
      !   delims      on input : Optional string with delimiter characters.
      !                          Default: space, tab, LF, CR: 32, 9, 10, 13.
      ! ------------------------------------------------------------------------------
      subroutine str_token(string, token, quote, delims)
          !
          ! Arguments
          !
          character(*)          , intent(inout) :: string
          character(*)          , intent(out)   :: token
          character(1), optional, intent(in)    :: quote
          character(*), optional, intent(in)    :: delims !< String where each character will be used as a delimiter (replacing the default whitespace delimiters)
          !
          ! Local variables
          !
          integer :: i
          integer :: i1
          integer :: i2
          integer :: j
          integer :: strlen
          logical :: quoted
          integer :: ndelim
          integer, allocatable :: idelims(:) !< Integer character codes for one or more delimiters
          !
          !! executable statements ---------------------------------------------------
          !
          if (present(delims)) then
             ndelim = len(delims)
             allocate(idelims(ndelim))
             do i=1,ndelim
                idelims(i) = ichar(delims(i:i))
             end do
          else
             ndelim = 4
             allocate(idelims(ndelim))
             idelims = (/ 32, 9, 10, 13 /)
          end if

          i1     = -1
          i2     = -1
          quoted = .false.
          strlen = len_trim(string)
          ! find start of token
          do i = 1, strlen
             j = ichar(string(i:i))
             if (any(idelims == j)) then
                ! a delimiter (e.g. space)
                if (i1>0 .and. .not.quoted) then
                   ! token ends here
                   i2 = i-1
                   exit
                endif
             else
                ! not a delimiter
                if (i1<0) then
                   ! token starts here and may continue till the end of the string
                   if (present(quote)) then
                      if (string(i:i) == quote) then
                         quoted = .true.
                      endif
                   endif
                   i1 = i
                   i2 = strlen
                elseif (quoted) then
                   if (string(i:i) == quote) then
                      quoted = .false.
                   endif
                endif
             endif
          enddo
          !
          if (i1<0) then
             ! empty string: no token found
             token = ' '
          else
             ! token found
             token  = string(i1:i2)
             if (present(quote)) then
                ! remove quotes
                if (string(i1:i1)==quote .and. string(i2:i2)==quote) then
                   token = string(i1+1:i2-1)
                endif
             endif
             string = string(i2+1:strlen)
          endif
          
          if (allocated(idelims)) then
             deallocate(idelims)
          end if
      end subroutine str_token

      !> Return copy of input string with all lowercase characters changed
      !! into uppercase.
      !! This is the function version of subroutine str_upper()
      function str_toupper(string) result(stringout)
          character(len=*), intent(in) :: string !< String to be converted.
          character(len=len(string))   :: stringout

          stringout = string
          call str_upper(stringout)
      end function str_toupper

      !> Return copy of input string with all uppercase characters changed
      !! into lowercase.
      !! This is the function version of subroutine str_lower()
      function str_tolower(string) result(stringout)
          character(len=*), intent(in) :: string !< String to be converted.
          character(len=len(string))   :: stringout

          stringout = string
          call str_lower(stringout)
      end function str_tolower


      ! ------------------------------------------------------------------------------
      !   Subroutine: str_lower
      !   Purpose:    Convert upper case characters to lower case
      !   Summary:    Scan string for upper case characters and
      !               convert them.
      !   Arguments:
      !   string      String to be converted
      !   lenstr      Optional length of string to be converted
      ! ------------------------------------------------------------------------------
      subroutine str_lower(string, lenstr)
          !
          ! Arguments
          !
          integer     , optional, intent(in) :: lenstr
          character(*)                       :: string
          !
          ! Local variables
          !
          integer :: i
          integer :: j
          integer :: newlen
          !
          !! executable statements ---------------------------------------------------
          !
          if (present(lenstr)) then
             newlen = min(lenstr, len_trim(string))
          else
             newlen = len_trim(string)
          endif
          do i = 1, newlen
             j = ichar(string(i:i))
             if ((j>64) .and. (j<91)) then
                j = j + 32
                string(i:i) = char(j)
             endif
          enddo
      end subroutine str_lower



      ! ------------------------------------------------------------------------------
      !   Subroutine: str_upper
      !   Purpose:    Convert lower case characters to upper case
      !   Summary:    Scan string for lower case characters and
      !               convert them.
      !   Arguments:
      !   string      String to be converted
      !   lenstr      Optional length of string to be converted
      ! ------------------------------------------------------------------------------
      subroutine str_upper(string, lenstr)
          !
          ! Arguments
          !
          integer     , optional, intent(in) :: lenstr
          character(*)                       :: string
          !
          ! Local variables
          !
          integer :: i
          integer :: j
          integer :: newlen
          !
          !! executable statements ---------------------------------------------------
          !
          if (present(lenstr)) then
             newlen = min(lenstr, len_trim(string))
          else
             newlen = len_trim(string)
          endif
          do i = 1, newlen
             j = ichar(string(i:i))
             if ((j>96) .and. (j<123)) then
                j = j - 32
                string(i:i) = char(j)
             endif
          enddo
      end subroutine str_upper



      ! ------------------------------------------------------------------------------
      !   Subroutine: remove_all_spaces
      !   Purpose:    Remove all spaces from a string
      !   Summary:    Scan string for space characters and if one exists, move the
      !               following characters forward.
      !   Arguments:
      !   string      String to be converted
      !   lenstr      Optional trimmed length of string after removal of spaces
      ! ------------------------------------------------------------------------------
      subroutine remove_all_spaces(string, lenstr)
          !
          ! Arguments
          !
          character(*)                       :: string
          integer     , optional, intent(out):: lenstr
          !
          ! Local variables
          !
          integer :: i
          integer :: newlen
          !
          !! executable statements ---------------------------------------------------
          !
          newlen = len_trim(string)
          !
          ! loop over all characters in string
          !    if it is a space character, move remainder of string forward
          !
          i = 1
          do while (i<newlen)
             if (string(i:i)==' ') then
                string(i:newlen) = string(i+1:newlen) // ' '
                newlen = newlen-1
             else
                i = i+1
             endif
          enddo
          !
          if (present(lenstr)) then
             lenstr = newlen
          endif
      end subroutine remove_all_spaces



      ! ------------------------------------------------------------------------------
      !   Subroutine: replace_multiple_spaces_by_single_spaces
      !   Purpose:    Replace multiple spaces in a string, and replace them with
      !               a single space instead
      !   Summary:    Scan string for multiple space characters and if they exists, 
      !               replace them with a single space.
      !   Arguments:
      !   string      String to be converted
      ! ------------------------------------------------------------------------------
      subroutine replace_multiple_spaces_by_single_spaces(string)
          !
          ! Arguments
          !
          character(*)                       :: string
          !
          ! Local variables
          !
          integer          :: lenstr
          integer          :: lenstrnew
          integer          :: iold
          integer          :: inew
          !
          !! executable statements ---------------------------------------------------
          !
          lenstr = len(string)
          !
          ! loop over all characters in string minus last
          !    if it is a double space character, skip copying
          !    single spaces are copied
          !
          inew = 0
          do iold = 1, lenstr - 1
              if(string(iold:iold + 1) /= '  ') then
                  inew = inew + 1
                  string(inew:inew) = string(iold:iold)
              endif
          enddo
          !
          ! last character might be defined, so copy that one
          !
          if(string(lenstr:lenstr) /= ' ') then
              inew = inew + 1
              string(inew:inew) = string(lenstr:lenstr)
          endif
          !
          ! fill up the remainder of the string with spaces
          !
          if (inew < lenstr) then
              lenstrnew = inew
              do inew = lenstrnew + 1, lenstr
                  string(inew:inew) = ' '
              enddo
          endif
          return
      end subroutine replace_multiple_spaces_by_single_spaces

      ! ------------------------------------------------------------------------------
      !   Subroutine: remove_leading_spaces
      !   Purpose:    Remove leading spaces from a string
      !   Summary:    Scan string for space characters at beginning of string and
      !               if they exist, move the actual string forward.
      !   Arguments:
      !   string      String to be converted
      !   lenstr      Optional trimmed length of string after removal of spaces
      ! ------------------------------------------------------------------------------
      subroutine remove_leading_spaces(string, lenstr)
          !
          ! Arguments
          !
          character(*)                       :: string
          integer     , optional, intent(out):: lenstr
          !
          ! Local variables
          !
          integer :: i
          integer :: newlen
          !
          !! executable statements ---------------------------------------------------
          !
          newlen = len_trim(string)
          !
          ! find first non-space character
          !
          i = 1
          do while (i<newlen)
             if (string(i:i)==' ') then
                i = i+1
             else
                exit
             endif
          enddo
          !
          ! remove leading spaces
          !
          string = string(i:newlen)
          !
          if (present(lenstr)) then
             lenstr = len_trim(string)
          endif
      end subroutine remove_leading_spaces


      !> Trims input string to a given length, filling with spaces at the end when necessary.
      !!
      !! When input string is longer than length, result is identical to normal string.
      !! When input string is shorter than length, result is filled with spaces on the right.
      function trimexact(string, length) result(trimmed)
         character(len=*), intent(in) :: string  !< Input string.
         integer,          intent(in) :: length  !< Exact length for the returned string.
         character(len=length)        :: trimmed !< Resulting string.
         trimmed = string
      end function trimexact

      ! ------------------------------------------------------------------------------
      !   Function:   strcmpi
      !   Purpose:    Case-insensitive comparison of strings (upto certain length)
      !   Summary:    Change strings to lower case and compare (sub)strings.
      !   Arguments:
      !   string1     First string to be compared
      !   string2     Second string to be compared
      !   lencmp      Optional length over which to compare strings
      ! ------------------------------------------------------------------------------
      function strcmpi(string1, string2, lenreq) result(retval)
          !
          ! Arguments
          !
          character(*)                   , intent(in) :: string1
          character(*)                   , intent(in) :: string2
          integer              , optional, intent(in) :: lenreq
          logical                                     :: retVal  ! .true.  if strings are equal
                                                                 ! .false. if strings are not equal or len1 /= len2
          !
          ! Local variables
          !
          integer                                     :: len1    ! length of string1, without trailing blanks
          integer                                     :: len2    ! length of string2, without trailing blanks
          integer                                     :: lencmp  ! length of strings to be compared
          character(999) , dimension(:) , allocatable :: locstr  ! copy of strings, to convert to lowercase
          !
          !! executable statements ---------------------------------------------------
          !
          retval = .false.
          len1   = len_trim(string1)
          len2   = len_trim(string2)
          !
          ! determine comparison length
          !
          if (present(lenreq)) then
             lencmp = lenreq
          else
             lencmp = max(len1,len2)
          endif
          !
          ! do a quick check on string length
          !
          if (len1<lencmp .or. len2<lencmp) then
             !
             ! at least one string is shorter than the comparison length
             ! they can only be equal if their length is equal
             !
             if (len1 /= len2) then
                retval = .false.
                return
             else
                !
                ! strings have equal length, but are shorter than comparison length
                ! we only have to check the strings for their actual length
                !
                lencmp = len1
             endif
          endif
          !
          ! local copy of the strings needed to switch case without changing the
          ! original version.
          !
          allocate (locstr(2))
          !
          ! strings will be compared upto lencmp
          !
          locstr(1) = string1(1:lencmp)
          call str_lower(locstr(1), lencmp)
          !
          locstr(2) = string2(1:lencmp)
          call str_lower(locstr(2), lencmp)
          !
          if (locstr(1)(1:lencmp) == locstr(2)(1:lencmp)) then
             !
             ! strings are equal upto lencmp
             !
             retval = .true.
          else
             !
             ! strings are not equal
             !
             retval = .false.
          endif
          deallocate (locstr)
      end function strcmpi

      !> Determine the index of the first non-whitespace character in a string.
      !! Failure is indicated by: idx = 0
      function find_first_char(string) result(idx)
          integer                      :: idx    !< index of the first non-whitespace character in string.
          character(len=*), intent(in) :: string !< string to inspect

          integer                        :: i
          !
          do i = 1, len(string)
             if (.not. is_whitespace(string(i:i))) then
                idx = i
                return
             endif
          enddo
          idx = 0
      end function find_first_char

      !> Determine the indices of the first letter (not number) and last character of the first word in a string.
      !! Failure is indicated by: i1 = 0; i2 = 0
      subroutine find_first_word(string, i1, i2)
         character(len=*), intent(in)  :: string !< string to inspect
         integer,          intent(out) :: i1     !< string index of the first letter of the first word
         integer,          intent(out) :: i2     !< string index of the last character of the first word
         !
         integer :: i !< loop counter
         integer :: L !< length of string, excluding trailing whitespace
         !
         L = len_trim(string)
         i1 = find_first_letter(string(1:L))
         i2 = 0
         i = 0
         !
         if (i1 > 0) then
            i2 = L
            do i=i1+1, L
               if (is_whitespace(string(i:i))) then
                  i2 = i-1
                  exit
               end if
            end do
         end if
      end subroutine find_first_word
      
      !> Determine the index of the first letter in a string.
      !! Failure is indicated by: index = 0
      function find_first_letter(string) result(idx)
         integer                      :: idx  !< index of first letter
         character(len=*), intent(in) :: string !< string to inspect
         !
         integer :: i  !< loop index
         integer :: i1 !< helper variable
         integer :: i2 !< helper variable
         integer :: i3 !< helper variable
         !
         idx = 0
         do i=1, len_trim(string)
            i1 = index('qwertyuiopasdfghjklzxcvbnm', string(i:i))
            i2 = index('QWERTYUIOPASDFGHJKLZXCVBNM', string(i:i))
            i3 = max(i1, i2)
            if (i3 /= 0) then
               idx = i
               exit
            endif
         enddo
      end function find_first_letter
      
      !> Count the number of whitespace separated character groups.
      function count_words(string) result(number)
         integer                      :: number !< number of words
         character(len=*), intent(in) :: string !< string to inspect
         !
         integer :: i !< loop counter
         logical :: was_whitespace !< helper variable
         !
         number = 0
         was_whitespace = .true.
         !
         do i=1, len(string)
            if (is_whitespace(string(i:i))) then
               if (.not. was_whitespace) then
                  ! word has ended
                  was_whitespace = .true.
               end if
            else
               if (was_whitespace) then
                  ! word has started
                  number = number + 1
                  was_whitespace = .false.
               end if
            end if
         end do
      end function count_words
      
      !> Checks whether the character is whitespace.
      function is_whitespace(letter)
         logical                      :: is_whitespace !< 
         character(len=1), intent(in) :: letter        !< 
         !
         is_whitespace = .false.
         !
         if (letter == ' ') then ! space
            is_whitespace = .true.
         else if (letter == char(9)) then ! tab
            is_whitespace = .true.
         end if
      end function is_whitespace

      !> Replace character with code ichar1 by code ichar2
      subroutine replace_char(r,ichar1,ichar2) 
         character(len=*), intent(inout) :: r
         integer         , intent(in)    :: ichar1
         integer         , intent(in)    :: ichar2
         !
         integer :: ch
         integer :: i
         !
         do i=1,len_trim(r)
            ch = ichar(r(i:i))
            if (ch==ichar1) then
               r(i:i) = achar(ichar2)
            endif
         enddo
      end subroutine replace_char      


      !> Replace substring in a total string by a replacement string.
      !! If the search string occurs multiple times, all will be replaced.
      function replace_string(totalstring, searchstring, replstring) result(resultstring)
         use m_alloc

         character(len=*), intent(in   ) :: totalstring  !< Input string in which searching is done.
         character(len=*), intent(in   ) :: searchstring !< Search string, will be used completely, without trimming.
         character(len=*), intent(in   ) :: replstring   !< Replacement string, will be used completely, without trimming.
         character(len=:), allocatable   :: resultstring !< Resulting string containing all of totalstring, and all occurrences of searchstring replaced by replstring.
         !
         integer :: istart, ifound, iresult, ntotal, nsearch, nrepl, nresult
         !
         istart  = 1
         iresult = 1
         ntotal  = len(totalstring)
         nsearch = len(searchstring)
         nrepl   = len(replstring)

         allocate(character(len=ntotal) :: resultstring)
         nresult = len(resultstring)

         do while (istart <= ntotal)
            ifound = index(totalstring(istart:), searchstring)
            if (ifound > 0) then
               ! Copy substring preceding the newly found match
               resultstring(iresult:iresult+ifound-2) = totalstring(istart:istart+ifound-2)
               iresult = iresult+ifound-1
               ! Next, put the replacement string
               if (iresult+nrepl-1 > nresult) then
                  call realloc(resultstring, iresult+nrepl-1, keepExisting=.true.)
                  nresult = iresult+nrepl-1
               end if
               resultstring(iresult:iresult+nrepl-1) = replstring
               iresult = iresult+nrepl
               istart = istart+ifound-1+nsearch
            else
               exit
            end if
         end do
         ! End with appending the last remaining part of the input string.
         if (iresult+ntotal-istart > nresult) then
            call realloc(resultstring, iresult+ntotal-istart, keepExisting=.true.)
            nresult = iresult+ntotal-istart
         end if
         resultstring(iresult:iresult+ntotal-istart) = totalstring(istart:ntotal)
      end function replace_string

      
      !> For each character in the given set, remove any occurrence in the subject
      subroutine remove_chars(r,charset) 
         character(len=*), intent(inout) :: r               !< subject on which to perform removal
         character(len=*), intent(in)    :: charset         !< collection of characters to be removed 
         !
         integer :: i, j
         !
         j=1
         do i=1,len_trim(r)
            if (index(charset,r(i:i))<=0) then
               r(j:j) = r(i:i)
               j = j + 1
            endif
         enddo
         r(j:len_trim(r)) = ' '
      end subroutine remove_chars
        
      !> Remove substring substr from r
      subroutine remove_substr(r,substr)
         character(len=*), intent(inout) :: r
         character(len=*), intent(in)    :: substr
         !
         integer :: first
         !
         first = index(r,substr)
         do while ((first>0) .and. (first<=len(trim(r))))
            r = r(1:first-1)//r(first+1:len_trim(r))
            first = index(r,substr)
         enddo 
      end subroutine remove_substr
      
      !> Split String at Separator
      function splitstr(string, strlen, separator) result(split)

         character(len=*), intent(inout)   :: string
         character(len=1), intent(in)      :: separator
         integer, intent(in)               :: strlen
         character(len=strlen)             :: split
      
         integer islash
      
         islash = index(string, separator)
         
         if (islash > 1) then
            split  = string(1:islash-1)
            string = string(islash+1:)
         else
            split = string
         endif
         
      end function splitstr

      !> Constructs a character string from an array of single characters.
      pure function char_array_to_string_by_len(char_array, N) result(string)
        character(len=1), intent(in) :: char_array(:) !< Input array of single characters.
        integer,          intent(in) :: N             !< Length up to which the array needs to be converted.
        character(len=N)             :: string        !< The resulting string of exactly length N.

        integer :: i
        do i = 1, N
           string(i:i) = char_array(i)
        enddo
      end function char_array_to_string_by_len


      subroutine get_substr_ndx(tgt,ndx0,ndx,sep)
         character(len=*), intent(in)           ::  tgt
         integer, intent(inout)                 ::  ndx0
         integer, intent(inout)                 ::  ndx
         character(len=*), intent(in), optional ::  sep
         integer           :: ltrim
         logical           :: single_quoted
         logical           :: double_quoted
         character(len=:), allocatable  :: sep_

         if (present(sep)) then
             sep_ = sep
         else
             sep_ = " "
         endif
         single_quoted = .false.
         double_quoted = .false.
         ltrim = len_trim(tgt)
         do while((is_whitespace(tgt(ndx0:ndx0)) .or. index(tgt(ndx0:),sep_)==1) .and. (ndx0<=ltrim))
            ndx0 = ndx0 + 1
         enddo
         ndx = ndx0
         do while(ndx<=ltrim)
            if (.not.(single_quoted .or. double_quoted)) then
               if (is_whitespace(tgt(ndx:ndx)) .or. index(tgt(ndx:),sep_)==1) exit
            endif
            if (tgt(ndx:ndx)=='"') double_quoted = .not.double_quoted
            if (tgt(ndx:ndx)=="'") single_quoted = .not.single_quoted
           ndx = ndx + 1
         enddo
      end subroutine get_substr_ndx

      !> Fill allocatable string array with elements of a space-delimited string
      !> The incoming string array must be unallocated
      recursive subroutine strsplit(tgt, ndx0, pcs, npc, sep)
         integer,          intent(in)                                 ::  npc   !< element index
         character(len=*), intent(in)                                 ::  tgt   !< input string
         integer, intent(in)                                          ::  ndx0  !< start position in string tgt
         character(len=*), intent(inout), dimension(:), allocatable   ::  pcs   !< resulting array of strings
         character(len=*), intent(in), optional                       ::  sep   !< optional separator

         integer                          ::  ndx, ndx1    ! position in string

         ndx1 = ndx0
         call get_substr_ndx(tgt,ndx1,ndx, sep)
         if (ndx<=len_trim(tgt)) then
            call strsplit(tgt, ndx, pcs, npc+1, sep)
         else
            allocate(pcs(npc))
         endif

         ndx = ndx - 1
         call strip_quotes(tgt, ndx1, ndx)
         pcs(npc) = tgt(ndx1 : ndx)

      end subroutine strsplit

      !> check on single or double quotes at start or end
      !! return (new) first and last positions
      subroutine strip_quotes1(tgt, pos1, pos2)
         character(len=*), intent(in)    :: tgt  !< input string
         integer         , intent(inout) :: pos1 !< first position
         integer         , intent(inout) :: pos2 !< last position

         character  ::  ch     ! help character

         ch = tgt(pos1:pos1)
         if (ch == '"' .or. ch =="'") pos1 = pos1 + 1

         ch = tgt(pos2:pos2)
         if (ch =='"' .or. ch == "'") pos2 = pos2 - 1

      end subroutine strip_quotes1

      !> check on single or double quotes at start or end
      !! returns cropped string
      subroutine strip_quotes2(tgt)
         character(len=:), allocatable, intent(inout) :: tgt  !< input string

         integer         :: pos1     ! first position
         integer         :: pos2     ! last position
         integer         :: pos1orig ! original first position
         integer         :: pos2orig ! original last position

         pos1 = 1
         pos2 = len(tgt)
         pos1orig = pos1
         pos2orig = pos2

         call strip_quotes1(tgt, pos1, pos2)

         if (pos1 /= pos1orig .or. pos2 /= pos2orig) then
            tgt = tgt(pos1:pos2)
         endif

      end subroutine strip_quotes2

      !> convert a real to a string with user defined format.
      !! if it does not fit, fall back on a more general format
      subroutine real2string(cnumber, formatReal, valueReal)
         character(len=*), intent(in)  :: formatReal  !< format string to be used
         real(kind=8), intent(in)      :: valueReal   !< number to be convert
         character(len=*), intent(out) :: cnumber     !< output string

         integer :: ierr

         write(cnumber, formatReal, iostat=ierr) valueReal
         if (ierr /= 0 .or. index(cnumber, '*') > 0) then
             write(cnumber,'(ES14.5E3)') valueReal
         endif

      end subroutine real2string

      !> convert a real to a string with user defined format.
      !! if it does not fit, fall back on a more general format
      !! align the string to the left (to allow printing with only trim())
      subroutine real2stringLeft(cnumber, formatReal, valueReal)
         character(len=*), intent(in)  :: formatReal  !< format string to be used
         real(kind=8), intent(in)      :: valueReal   !< number to be convert
         character(len=*), intent(out) :: cnumber     !< output string

         call real2string(cnumber, formatReal, valueReal)
         cnumber = adjustl(cnumber)

      end subroutine real2stringLeft

      subroutine GetLine(unit, line, stat, iomsg)
      !!
      !> Reads a complete line (end-of-record terminated) from a file.
      !!
      !! @param[in]     unit              Logical unit connected for formatted input to the file.
      !!
      !! @param[out]    line              The line read.
      !!
      !! @param[out]    stat              Error code, positive on error, IOSTAT_END (which is negative) on end of file.
      !!
      !! @param[out]    iomsg             Error message - only defined if iostat is non-zero.
      !!
      !! found in: https://software.intel.com/en-us/comment/1730972
      !!
      use, intrinsic :: iso_fortran_env, only: iostat_eor
      !---------------------------------------------------------------------------
      ! arguments
      integer,      intent(in)               :: unit
      character(:), intent(out), allocatable :: line
      integer,      intent(out)              :: stat
      character(*), intent(out), optional    :: iomsg
      !---------------------------------------------------------------------------
      ! Local variables
      character(len=256) :: buffer         ! Buffer to read the line (or partial line).
      integer            :: size           ! Number of characters read from the file.
      integer            :: size_trim      ! Number of characters read from the file  (trimmed).
      logical            :: isFirstBuffer  ! flag to handle first read different from others
      !***************************************************************************
      isFirstBuffer = .true.
      do
        buffer = ''
        if (present(iomsg)) then
            read (unit, "(A)", ADVANCE='NO', IOSTAT=stat, IOMSG=iomsg, SIZE=size)  buffer
        else
            read (unit, "(A)", ADVANCE='NO', IOSTAT=stat, SIZE=size)  buffer
        endif
        !
        ! The following correction (including the IF) is necessary since in multi-treading applications,
        ! the read statement appears to not always be thread-safe. Sometimes a string is read in BUFFER correctly,
        ! but the returned SIZE = 0.
        !
        if (size == 0) then
           size      = len(buffer)
           size_trim = len(trim(buffer))
           if (size_trim < size .and. stat == 0) then
              if (abs(size - size_trim) <= 10) then
                 !
                 ! Since size will always be 256, (almost) the full buffer was read
                 ! We assume that no more than 10 spaces are used between entries in a file on one line
                 ! If the difference between the line and the trimmed line is less than 10 (and the full buffer (256) was read
                 ! probably the line is longer than what was read, so stat should remain zero and we store the untrimmed line
                 ! This is done below. It was the default way, when no errors occur. The difference is that we have now explicitly set
                 ! size = len(buffer)
              else
                 !
                 ! Less than 246 chars were filled in the buffer
                 ! We assume that we have read the whole line and explicitly set size to size_trim and stat = IOSTAT_EOR (end of record)
                 !
                 size = size_trim
                 stat = IOSTAT_EOR
              endif
           endif
        endif
        if (stat > 0) then
            line = ''
            exit      ! Some sort of error.
        endif
        if (isFirstBuffer) then
            size = max(1, size)
            line = buffer(:size)
            isFirstBuffer = .false.
        else            
            line = line // buffer(:size)
        endif
        if (stat < 0) then
            if (stat == IOSTAT_EOR) stat = 0
            exit
        endif
      enddo
      end subroutine GetLine

      !> convert an integer into a string
      function int2str(i) result(string)
         integer          , intent(in) :: i        !< integer to be represented by a string
         
         character(len=11)             :: string11 !< temporary fixed-length string: 11 is long enough for any 64bit integer
         character(len=:), allocatable :: string   !< flexible length string
         
         write(string11,'(i0)') i
         string = trim(string11)
      end function int2str

end module string_module
