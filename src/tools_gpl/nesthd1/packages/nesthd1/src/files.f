      subroutine opnfl1(lun   ,filnam, CIDENT)
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
!***********************************************************************
! Deltares                         marine and coastal management
!
! subroutine         : opnfl1
! version            : v1.0
! date               : June 1997
! programmer         : Theo van der Kaaij
!
! function           : Opens all files for the program NESTHD1
! limitations        :
! subroutines called :
!***********************************************************************

      integer       lun    (7)
      character*80  filnam (7)
      character*(*) CIDENT
      logical      ex

      write (*,'(/, 2a)') ' ', 'Overall model'
      write (*,'(   2a)') ' ', '-------------'

10    write (*,'(2a,$)') ' ','File name grid file               : '
      read  (*,'(a80 )') filnam (1)

      inquire(file=filnam(1),exist=ex)
      if ( .not.ex ) then
        write(*,'(a,a,a)')' >> File ',trim(filnam(1)),' does not exist'
        write(*,'(a    )') ' >> please reenter '
        go to 10
      endif
      lun(1) = newlun ( )
      open (lun(1),file=filnam(1),status='old')

20    write (*,'(2a,$)') ' ','File name enclosure file          : '
      read  (*,'(a80    )') filnam (2)
      inquire(file=filnam(2),exist=ex)
      if ( .not.ex ) then
        write(*,'(a,a,a)')' >> File ',trim(filnam(2)),' does not exist'
        write(*,'(a    )') ' >> please reenter '
        go to 20
      endif
      lun(2) = newlun ( )
      open (lun(2),file=filnam(2),status='old')

      write (*,'(/, 2a)') ' ', 'Detailed model'
      write (*,'(   2a)') ' ', '---------------'

 30   write (*,'(2a,$)') ' ','File name grid file               : '
      read  (*,'(a80    )') filnam (3)
      inquire(file=filnam(3),exist=ex)
      if ( .not.ex ) then
        write(*,'(a,a,a)')' >> File ',trim(filnam(3)),' does not exist'
        write(*,'(a    )') ' >> please reenter '
        go to 30
      endif
      lun(3) = newlun ( )
      open (lun(3),file=filnam(3),status='old')

 40   write (*,'(2a,$)') ' ','File name enclosure file          : '
      read  (*,'(a80    )') filnam (4)
      inquire(file=filnam(4),exist=ex)
      if ( .not.ex ) then
        write(*,'(a,a,a)')' >> File ',trim(filnam(4)),' does not exist'
        write(*,'(a    )') ' >> please reenter '
        go to 40
      endif
      lun(4) = newlun ( )
      open (lun(4),file=filnam(4),status='old')

 50   write (*,'(2a,$)') ' ','File name boundary definition file: '
      read  (*,'(a80    )') filnam (5)
      inquire(file=filnam(5),exist=ex)
      if ( .not.ex ) then
        write(*,'(a,a,a)')' >> File ',trim(filnam(5)),' does not exist'
        write(*,'(a    )') ' >> please reenter '
        go to 50
      endif
      lun(5) = newlun ( )
      open (lun(5),file=filnam(5),status='old')

      write (*,'(/, 2a,$)') ' ', 'File name administration file     : '
      read  (*,'(a80    )') filnam (6)
      lun(6) = newlun( )
      open (lun(6),file=filnam(6),status='unknown')

      write (*,'(/, 2a,$)') ' ', 'File FLOW observation file        : '
      read  (*,'(a80    )') filnam (7)
      write (*,'( )')

      lun(7) = newlun( )

      inquire (file = filnam(7), exist = ex)
      if (ex) then
         open  (lun(7), file=filnam(7))
         close (lun(7), status='delete')
      endif

      open (lun(7),file=filnam(7),status='unknown')

      call wrigen (lun(6),filnam, CIDENT)

      return

      end

      subroutine opnfl2(lun   ,extnef)
!***********************************************************************
! Deltares                         marine and coastal management
!
! subroutine         : opnfl2
! version            : v1.0
! date               : July 1997
! programmer         : Theo van der Kaaij
!
! function           : Opens all files for the program NESTHD2
! limitations        :
! subroutines called :
!***********************************************************************

      integer      lun    (5)

      character*80 filnam
      character* 3 extnef


      write (*,'(/,2a,$)') ' ','File name boundary definition file : '
      read  (*,'(a80    )') filnam
      lun(1) = newlun ( )
      open (lun(1),file=filnam,status='old')

      write (*,'(2a,$)') ' ', 'File name administration file      : '
      read  (*,'(a80    )') filnam
      lun(2) = newlun( )
      open (lun(2),file=filnam,status='unknown')

      write (*,'(2a,$)') ' ', 'Extension NEFIS files overall model: '
      read  (*,'(a3    )') extnef

      write (*,'(2a,$)') ' ', 'File name hydrodynamic bc.         : '
      read  (*,'(a80    )') filnam
      lun(3) = newlun( )
      open (lun(3),file=filnam,status='unknown')

      write (*,'(2a,$)') ' ', 'File name transport bc.            : '
      read  (*,'(a80    )') filnam
      lun(4) = newlun( )
      open (lun(4),file=filnam,status='unknown')

      write (*,'(2a,$)') ' ', 'File name diagnostic file          : '
      read  (*,'(a80    )') filnam
      lun(5) = newlun( )
      open (lun(5),file=filnam,status='unknown')

      return

      end

      subroutine clsfil(lun   , nolun)
!***********************************************************************
! Deltares                         marine and coastal management
!
! subroutine         : clsfil
! version            : v1.0
! date               : June 1997
! specs last update  :
! programmer         : Theo van der Kaaij
!
! function           : Closes all files for nest
! limitations        :
! subroutines called :
!***********************************************************************

      integer lun(nolun)

      do 10 i = 1, nolun
         close (lun(i))
   10 continue

      return
      end

      subroutine wrigen (lun   , filnam, CIDENT)

!***********************************************************************
! Deltares                         marine and coastal management
!
! subroutine         : wrigen
! version            : v1.0
! date               : June 1997
! specs last update  :
! programmer         : Theo van der Kaaij
!
!***********************************************************************

      integer       leng
      character* 80 filnam (7)
      character*(*) CIDENT
      character* 20 rundat

      call dattim (rundat)            

      iend = len_trim( cident )
      write (lun,'(''*'')')
      write (lun,'(''* '',a)') cident(1:iend) 
      write (lun,'(''*'')')
      write (lun,'(''* Run date: '',a20)') rundat
      write (lun,'(''*'')')
      write (lun,'(''* Name grid file overall model              : '',
     *               a80)')  filnam (1)
      write (lun,'(''* Name enclosure file overall model         : '',
     *               a80)') filnam (2)
      write (lun,'(''*'')')
      write (lun,'(''* Name grid file detailed model             : '',
     *               a80)') filnam (3)
      write (lun,'(''* Name enclosure file detailed model        : '',
     *               a80)') filnam (4)
      write (lun,'(''* Name bnd. definition file detailed model  : '',
     *               a80)') filnam (5)
      write (lun,'(''*'')')
      write (lun,'(''* Name nest administration file             : '',
     *               a80)') filnam (6)
      write (lun,'(''* Name FLOW observation file                : '',
     *               a80)') filnam (7)
      write (lun,'(''*'')')

!-----------------------------------------------------------------------
!---- return to calling module
!-----------------------------------------------------------------------

      return

      end
