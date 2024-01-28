subroutine check  (lundia,fout  ,mnstat,mnes ,nnes   ,weight , &
                   mcbsp ,ncbsp ,iwet  ,nostat               )
    implicit none
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
! subroutine         : check
! version            : v1.0
! date               : July 1997
! programmer         : Theo van der Kaaij
!
! function           : Checks whether nesting stations are available
!                      on history file
! limitations        :
! subroutines called :
!***********************************************************************
    integer :: nones, iwght, istat, lundia, mcbsp, ncbsp, nostat
    double precision :: wghtot
    integer :: iwet  (nostat), mnes   (  4  ), nnes  (  4  )
    integer :: mnstat(2,nostat)

    double precision :: weight (  4  )

    logical :: fout  ,found
!
!-----cycle over nesting station
!
    wghtot = 0.0
    nones  = 0

    do 10 iwght = 1, 4
        if (mnes(iwght) .ne. 0) then
            found  = .false.
            do 20 istat = 1, nostat
                if ((mnstat(1,istat) .eq. mnes  (iwght)) .and. &
                (mnstat(2,istat) .eq. nnes  (iwght)) .and. &
                (.not. found) ) then
                    found = .true.
                    nones = nones + 1
                    if (iwet (istat) .eq. 1) then
                        wghtot = wghtot + weight (iwght)
                    else
                        write (lundia,'( &
                                '' *** Warning: Nest station ('',i5,'','',i5, &
                                '') is permanent dry throughout simulation.'')') &
                                mnes(iwght),nnes(iwght)
                        mnes  (iwght) = 0
                        nnes  (iwght) = 0
                        weight(iwght) = 0.
                    endif
                endif
   20       continue
            if (.not. found) then
                write (lundia,'('' *** Warning: Nest station ('',i5, &
                        '','',i5, &
                        '') not on history file. Weights reset to zero'')') &
                        mnes(iwght),nnes(iwght)
                mnes  (iwght) = 0
                nnes  (iwght) = 0
                weight(iwght) = 0.
            endif
        endif
    10 continue
!
!---No nesting stations at all ==> Error
!
    if (nones .eq. 0) then
        write (lundia,'(''*** Error: No nesting stations for '', &
                ''support point ('',i5,'','',i5,'')'')') &
                mcbsp ,ncbsp
        fout = .true.
        goto 999
    else
!
!-------Recalculate weights
!
        do 30 iwght = 1, 4
            if (wghtot /= 0.0) then
                weight (iwght) = weight(iwght)/wghtot
            else
                weight (iwght) = 1.0
            endif
   30   continue
    endif

999 continue
    return
end subroutine
