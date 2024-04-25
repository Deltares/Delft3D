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
!  
!  
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

      integer       lun    (*)
      character*(*) filnam (*)
      character*(*) CIDENT
      logical      ex

      write (*,'(/, 2a)') ' ', 'Overall model'
      write (*,'(   2a)') ' ', '-------------'

      write (*,'(2a,$)') ' ','File name grid file               : '
      read  (*,'(a)') filnam (1)
      lun(1) = newlun ( )
      open (lun(1),file=trim(filnam(1)),status='old')

      write (*,'(2a,$)') ' ','File name enclosure file          : '
      read  (*,'(a)') filnam (2)
      lun(2) = newlun ( )
      open (lun(2),file=trim(filnam(2)),status='old')

      write (*,'(/, 2a)') ' ', 'Detailed model'
      write (*,'(   2a)') ' ', '---------------'

      write (*,'(2a,$)') ' ','File name grid file               : '
      read  (*,'(a)') filnam (3)
      lun(3) = newlun ( )
      open (lun(3),file=trim(filnam(3)),status='old')

      write (*,'(2a,$)') ' ','File name enclosure file          : '
      read  (*,'(a)') filnam (4)
      lun(4) = newlun ( )
      open (lun(4),file=trim(filnam(4)),status='old')

      write (*,'(2a,$)') ' ','File name boundary definition file: '
      read  (*,'(a)') filnam (5)
      lun(5) = newlun ( )
      open (lun(5),file=trim(filnam(5)),status='old')

      write (*,'(/, 2a,$)') ' ', 'File name administration file     : '
      read  (*,'(a)') filnam (6)
      lun(6) = newlun( )
      open (lun(6),file=trim(filnam(6)),status='unknown')

      write (*,'(/, 2a,$)') ' ', 'File FLOW observation file        : '
      read  (*,'(a)') filnam (7)
      write (*,'( )')

      lun(7) = newlun( )

      inquire (file = trim(filnam(7)), exist = ex)
      if (ex) then
         open  (lun(7), file=trim(filnam(7)))
         close (lun(7), status='delete')
      endif

      open (lun(7),file=trim(filnam(7)),status='unknown')

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

      character*256 filnam
      character*(*) extnef

      write (*,'(/, 2a)') ' ', 'Detail model'
      write (*,'(   2a)') ' ', '-------------'
      write (*,'(2a,$)') ' ','File name boundary definition file : '
      read  (*,'(a80    )') filnam
      lun(1) = newlun ( )
      open (lun(1),file=trim(filnam),status='old')

      write (*,'(2a,$)') ' ', 'File name administration file      : '
      read  (*,'(a)') filnam
      lun(2) = newlun( )
      open (lun(2),file=trim(filnam),status='unknown')

      write (*,'(/, 2a)') ' ', 'Overall model'
      write (*,'(   2a)') ' ', '-------------'
      write (*,'(2a,$)') ' ', 'Run-id NEFIS files                 : '
      read  (*,'(a   )') extnef

      write (*,'(/, 2a)') ' ', 'Ouput for detail model'
      write (*,'(   2a)') ' ', '----------------------'
      write (*,'(2a,$)') ' ', 'File name hydrodynamic bc.         : '
      read  (*,'(a    )') filnam
      lun(3) = newlun( )
      open (lun(3),file=trim(filnam),status='unknown')

      write (*,'(2a,$)') ' ', 'File name transport bc.            : '
      read  (*,'(a)') filnam
      lun(4) = newlun( )
      open (lun(4),file=trim(filnam),status='unknown')

      write (*,'(2a,$)') ' ', 'File name diagnostic file          : '
      read  (*,'(a)') filnam
      lun(5) = newlun( )
      open (lun(5),file=trim(filnam),status='unknown')

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
      character*(*) filnam (7)
      character*(*) CIDENT
      character* 20 rundat

      call dattim (rundat)            

      iend = len_trim( cident )
      write (lun,'(''*'')')
      write (lun,'(''* '',A)') CIDENT(5:iend) 
      write (lun,'(''*'')')
      write (lun,'(''* Run date :'',a20)') rundat
      write (lun,'(''*'')')
      write (lun,'(''* Name grid file overall model              : '',
     *               a)') trim(filnam (1))
      write (lun,'(''* Name enclosure file overall model         : '',
     *               a)') trim(filnam (2))
      write (lun,'(''*'')')
      write (lun,'(''* Name grid file detailed model             : '',
     *               a)') trim(filnam (3))
      write (lun,'(''* Name enclosure file detailed model        : '',
     *               a)') trim(filnam (4))
      write (lun,'(''* Name bnd. definition file detailed model  : '',
     *               a)') trim(filnam (5))
      write (lun,'(''*'')')
      write (lun,'(''* Name nest administration file             : '',
     *               a)') trim(filnam (6))
      write (lun,'(''* Name FLOW observation file                : '',
     *               a)') trim(filnam (7))

!-----------------------------------------------------------------------
!---- return to calling module
!-----------------------------------------------------------------------

      return

      end
