!!  Copyright (C)  Stichting Deltares, 2012-2023.
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
      module m_rdvers
      use m_srstop


      implicit none

      contains


      subroutine rdvers ( lunin  , lfile  , lunut  , npos   , vrsion ,
     &                    ioutpt )

!     Deltares Software Centre

!>\file
!>               Searches and reads the input file for the version string
!>
!>               The version string looks like DELWAQ_VERSION_n.nnn\n
!>               It can be placed anywhere in the input file\n
!>               The n.nnn number is used to determine how to parse the
!>               input file

!     Created             : May   1997  by  Leo Postma

!     Modified            : April 2011  by  Leo Postma
!                                       Fortran 90 look and feel + documentation

!     Subroutine called   : srstop  stops execution with return code

!     Functions  called   : char    returns ASCII characters

!     Logical unitnumbers : LUNIN = input file
!                           LUNUT = output report file

      implicit none

!     Parameters

!     kind           function         name                 description

      integer  ( 4), intent(in   ) :: lunin             !< unit number input file
      character*(*), intent(in   ) :: lfile             !< file name
      integer  ( 4), intent(in   ) :: lunut             !< unit number report file
      integer  ( 4), intent(in   ) :: npos              !< number of significant positions in one line
      real     ( 4), intent(  out) :: vrsion            !< Version number
      integer  ( 4), intent(  out) :: ioutpt            !< Output option

!     Local

      character*(npos) car                              !  read buffer
      character*1      ctrlz , ch_cr                    !  special characters
      integer          i, i2                            !  loop counter
      integer          status                           !  iostatus

      ch_cr = char(13)
      ctrlz = char(26)

      vrsion = 0.0
      ioutpt = -1
      status = 0
      do while ( status .eq. 0 )
         read ( lunin , '(a)' , iostat=status ) car

!        search the tokens, read the numbers

         do i  = 1, npos-19
            if ( car(i:i+14) .eq. 'DELWAQ_VERSION_' ) then
               do i2 = i+15 , i+19
                  if ( car(i2:i2) .eq. ctrlz .or.
     &                 car(i2:i2) .eq. ch_cr      ) car(i2:i2) = ' '
               enddo
               read  ( car(i+15:i+19) , '(f5.0)' ) vrsion
               write ( lunut , '(a,a,f6.3)' ) '       ---------->',
     &                   ' Version number of the input file is: ', vrsion
            endif
            if ( car(i:i+19) .eq. 'PRINT_OUTPUT_OPTION_' ) then
               read ( car(i+20:i+20) , '(i1)' ) ioutpt
               write ( lunut , '(a,a,i1)'   ) '       ---------->',
     &                   ' Output level of the listfile is: ',ioutpt
            endif
         enddo
      enddo

      if ( status .lt. 0 ) then    !        end of file encountered
         rewind lunin
         read ( lunin , '(a)' ) car
         return
      else                         !        errors during read
         write ( lunut , 2000 ) lunin , lfile
         call srstop(1)
      endif

!        output formats

 2000 format (  ' ERROR, reading file on unit',I3,' !!',
     &         /' Filename is: ',A20,
     &         /' ********** EXECUTION TERMINATED ********' )

      end
      end module m_rdvers
