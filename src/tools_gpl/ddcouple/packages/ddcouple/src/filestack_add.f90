!!  Copyright (C)  Stichting Deltares, 2021-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine filestack_add(inpfil,filename,ierr)

      ! funtion : add file to input file stack

      ! global definitions

      use m_monsys
      use m_dhnlun
      use dlwqdata_mod
      implicit none

      ! declaration of arguments :

      type(inputfilestack),intent(inout) :: inpfil       ! input file strucure
      character (len=*)   ,intent(in)    :: filename     ! input file name
      integer             ,intent(out)   :: ierr         ! error indication

      ! local declarations

      character(len=79)                  :: line
      integer                            :: ilun

      ierr = 0
      inpfil%inputf=inpfil%inputf+1
      if ( inpfil%inputf .gt. maxfil ) then
         line = 'ERROR: Number of nested include files exceeds max:'
         write(line(52:),'(I2)') maxfil
         call monsys(line,1)
         ierr = 1
         return
      endif
      call dhnlun(10,ilun)
      open(ilun,file=filename,iostat=ierr)
      inpfil%inplun(inpfil%inputf) = ilun
      inpfil%linenr(inpfil%inputf) = 0
      inpfil%nrword(inpfil%inputf) = 0
      inpfil%ilword(inpfil%inputf) = 0
      if ( inpfil%inputf .eq. 1 ) then
         inpfil%cchar = ';'
         inpfil%grpsep= ' '
         inpfil%npos  = len(inpfil%linbuf(inpfil%inputf))
      endif
      inpfil%finame(inpfil%inputf) = filename
      inpfil%linbuf(inpfil%inputf) = ' '
      inpfil%iposr                 = 0
      inpfil%ierr                  = 0
      inpfil%ctoken                = ' '
      inpfil%itoken                = 0
      inpfil%rtoken                = 0.0
      inpfil%token_used            = .true.
      inpfil%nrepeat               = 0

      return
      end
