      subroutine wrihyd2(lundia,lun   ,bndval,typbnd,nambnd,nobnd ,
     *                   notims,kmax  ,nolay ,tstart,dtmin ,itdate)
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
! subroutine         : wrihyd
! version            : v1.0
! date               : July 1997
! programmer         : Theo van der Kaaij
!
! function           : Writes hydrodynamic bc. to TRISULA attribute file
!                      (new format)
! limitations        :
! subroutines called :
!***********************************************************************
!
      character(len=37), dimension(13) :: fmtbct
      character(len=20), dimension(16) :: keywrd
      
      double precision bndval (nobnd ,notims,kmax  ,1     ,2)

      double precision tstart
      integer lundia, itdate, nolay, nobnd, notims, kmax
      
      double precision dtmin
      integer ifmt, ibnd, itim, lun, k, isize

      character*1  eol
      character*1  typbnd (nobnd )
      character*1  quote
      character*3  cdum
      character*17 layer
      character*20 nambnd (nobnd )
      character*40 cntain
      
      data         fmtbct/'(a1,i5,                       t83,a1)',
     *                    '(a20,a1,a42,a3,a1,            t83,a1)',
     *                    '(2(a20,a1),a40,               t83,a1)',
     *                    '(2(a20,a1),                   t83,a1)',
     *                    '(a20,                         t83,a1)',
     *                    '(a20,                         t83,a1)',
     *                    '(a20,a1,a15,a1,               t83,a1)',
     *                    '(a20,i9,                      t83,a1)',
     *                    '(a20,a1,a7,a1,                t83,a1)',
     *                    '(a20,a1,a6,a1,                t83,a1)',
     *                    '(a20,a1,a37,a1,1x,2(a20,a1),  t83,a1)',
     *                    '(a20,i6,                      t83,a1)',
     *                    '(f12.4,    2ES14.6,           t83,a1)'/

      data         keywrd/'table-name          ',
     *                    'contents            ',
     *                    'location            ',
     *                    'geo-coordinates     ',
     *                    'metric-coordinates  ',
     *                    'frequency           ',
     *                    'frequency-unit      ',
     *                    'time-function       ',
     *                    'reference-time      ',
     *                    'time-unit           ',
     *                    'time-step           ',
     *                    'interpolation       ',
     *                    'extrapolation       ',
     *                    'parameter           ',
     *                    'unit                ',
     *                    'records-in-table    '/

!----------------------------------------------------------------
!     Initialisation
!----------------------------------------------------------------
      write (*     ,'('' >>> Writing hydrodynamic boundary '',
     *                ''conditions <<<'')')
      write (lundia,'('' >>> Writing hydrodynamic boundary '',
     *                ''conditions <<<'')')
      cdum  = '   '
      quote = CHAR (39)
      eol   = ' ' 
!
      cntain = ' # at ends A&B of open boundary segment '

      do 10 ifmt = 1, 13
         write (fmtbct(ifmt)(29:29),'(a1)') 't'
         write (fmtbct(ifmt)(30:33),'(i4.4)') max(12 + 2*kmax*14 + 1,83)
   10 continue

!----------------------------------------------------------------------
!     Write to time-series file
!----------------------------------------------------------------------

      do 100 ibnd = 1, nobnd
         do 100 itim = 1, notims
            if (itim .eq. 1) then
                write(lun   ,fmtbct( 2)) keywrd( 1),
     *                quote,'Nested flow BC '//nambnd(ibnd)//
     *                '      ',
     *                cdum,quote,eol
                if (typbnd(ibnd) .eq. 'z' .or.
     *              typbnd(ibnd) .eq. 'u' ) then
                   write(lun   ,fmtbct( 3)) keywrd( 2),
     *                   quote,'uniform             ',quote,cntain,eol
                else
                   write(lun   ,fmtbct( 3)) keywrd( 2),
     *                   quote,'3d-profile          ',quote,cntain,eol
                endif
                write(lun   ,fmtbct( 4)) keywrd( 3),
     *                quote,nambnd(ibnd),quote,eol
                write(lun   ,fmtbct( 7)) keywrd( 8),
     *                quote,'non-equidistant',quote,eol
                write(lun   ,fmtbct( 8)) keywrd( 9),
     *                itdate,eol
                write(lun   ,fmtbct( 9)) keywrd(10),
     *                quote,'minutes',quote,eol
                write(lun   ,fmtbct(10)) keywrd(12),
     *                quote,'linear',quote,eol
                write(lun   ,fmtbct(11)) keywrd(14),
     *                quote,'time                '//
     *                      '                 ',
     *                quote,keywrd(15),quote,' ',quote,eol
                if (typbnd(ibnd) .eq. 'z') then
                   layer = ' End A           '
                   write(lun   ,fmtbct(11)) keywrd(14),
     *                   quote,'Water elevation (Z) '//
     *                   layer,
     *                   quote,keywrd(15),quote,' ',quote,eol
                   layer = ' End B           '
                   write(lun   ,fmtbct(11)) keywrd(14),
     *                   quote,'Water elevation (Z) '//
     *                   layer,
     *                   quote,keywrd(15),quote,' ',quote,eol
                else
                   layer = ' End A layer:    '
                   do 110 k = 1, nolay
                      write (layer(15:17),'(a3)') cdum
                      write (layer(15:17),'(i3)') k
                      write(lun   ,fmtbct(11)) keywrd(14),
     *                   quote,'Current         (C) '//
     *                   layer,
     *                   quote,keywrd(15),quote,' ',quote,eol
  110              continue
                   layer = ' End B layer:    '
                   do 120 k = 1, nolay
                      write (layer(15:17),'(a3)') cdum
                      write (layer(15:17),'(i3)') k
                      write(lun   ,fmtbct(11)) keywrd(14),
     *                   quote,'Current         (C) '//
     *                   layer,
     *                   quote,keywrd(15),quote,' ',quote,eol
  120              continue
                endif
                write(lun   ,fmtbct(12)) keywrd(16),
     *                notims,eol

            endif

            if (typbnd(ibnd) .eq. 'z') then
              write (fmtbct(13) (10:12),'(i3.3)') 2
              write(lun   ,fmtbct(13))
     *              tstart +  (itim -1)*dtmin,
     *              (bndval (ibnd,itim,1,1,isize), isize=1,2),eol
            else
               write (fmtbct(13) (10:12),'(i3.3)') 2*nolay
               write(lun   ,fmtbct(13))
     *              tstart +  (itim -1)*dtmin,
     *              ((bndval (ibnd,itim,k,1,isize), k=    1,nolay),
     *                                              isize=1,2    ),eol
            endif
  100 continue

      return
      end
