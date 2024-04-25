      subroutine wricon2(lundia,lun   ,bndval,itypc ,nobnd ,notims,
     *                   kmax  ,lstci ,nocon ,
     *                   tstart,dtmin ,
     *                   namcon,nambnd,itdate              )
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
! subroutine         : wricon2
! version            : v1.0
! date               : September 1997
! programmer         : Theo van der Kaaij
!
! function           : Writes transport bc. to TRISULA attribute file
!                      (new format)
! limitations        :
! subroutines called :
!***********************************************************************

      integer lundia, lun, notims, kmax, lstci, nocon, nobnd

      character(len=37), dimension(13) :: fmtbcc
      character(len=20), dimension(16) :: keywrd

      integer      itdate

      double precision bndval (nobnd ,notims,kmax  ,lstci ,2)
      double precision tstart
      double precision dtmin

      character*1  eol
      character*1  quote
      character*3  cdum
      character*17 layer
      character*20 nambnd (nobnd )
      character*20 namcon (lstci + 2 )
      character*40 cntain
      integer ifmt, ibnd, ic, itim, itypc, k

      data         fmtbcc/'(a1,i5,                       t89,a1)',
     *                    '(a20,a1,a63,a3,a1,            t89,a1)',
     *                    '(a20,a1,a10,a1,a40,           t89,a1)',
     *                    '(2(a20,a1),                   t89,a1)',
     *                    '(a20,                         t89,a1)',
     *                    '(a20,                         t89,a1)',
     *                    '(a20,a1,a15,a1,               t89,a1)',
     *                    '(a20,i9,                      t89,a1)',
     *                    '(a20,a1,a7,a1,                t89,a1)',
     *                    '(a20,a1,a6,a1,                t89,a1)',
     *                    '(a20,a1,a37,a1,1x,2(a10,a1),  t89,a1)',
     *                    '(a20,i6,                      t89,a1)',
     *                    '(f12.4,    5ES14.6,           t89,a1)'/

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
!----------------------------------------------------------------------
!     Initialisation
!----------------------------------------------------------------------
      write (*     ,
     *'('' >>> Writing transport boundary conditions <<<'')')
      write (lundia,
     *'('' >>> Writing transport boundary conditions <<<'')')

      cdum = '   '
      quote  = CHAR (39)
      eol    = ' '
!     eol    = CHAR (10)
!
      cntain = ' # at ends A&B of open boundary segment '

      do 10 ifmt = 1, 13
         write (fmtbcc(ifmt)(29:29),'(a1)') 't'
         write (fmtbcc(ifmt)(30:33),'(i4.4)') max(12 + 2*kmax*14 + 1,89)
   10 continue

!----------------------------------------------------------------------
!     Write dummy record to avoid binary characters on first record (NT)
!----------------------------------------------------------------------

      write (lun,fmtbcc(5)) ' ',eol
      backspace (lun)
!
!----------------------------------------------------------------------
!     Uniform and linear boundary conditions
!     (uniform also a linear profile with identical surface and bed
!      values)
!----------------------------------------------------------------------
      do 100 ibnd = 1, nobnd
         do 100 ic = 1, nocon
            do 100 itim = 1, notims
!               irec   = (ibnd-1)*(lstci*(notims+11)) +
!    *                            (ic-1)*(notims+11)  + 1
!---------------------------------------------------------------------
!---------------Write first 11 records with block information for
!               time IB=1
!               PARNAM starts at 2 (first id for DIS)
!---------------------------------------------------------------------
                if (itim .eq. 1) then
                  write(lun   ,fmtbcc( 2)) keywrd( 1),
     *                  quote,'Nested transport bc '//namcon(ic)//
     *                  ' '//nambnd(ibnd)//'  ',
     *                  cdum,quote,eol
                  if (itypc .eq. 1) then
                     write(lun   ,fmtbcc( 3)) keywrd( 2),
     *                     quote,'Uniform   '    ,quote,cntain,eol
                  elseif (itypc .eq. 2) then
                     write(lun   ,fmtbcc( 3)) keywrd( 2),
     *                     quote,'Linear    '    ,quote,cntain,eol
                  else
                     write(lun   ,fmtbcc( 3)) keywrd( 2),
     *                     quote,'3d-profile',quote,cntain,eol
                  endif
                  write(lun   ,fmtbcc( 4)) keywrd( 3),
     *                  quote,nambnd(ibnd),quote,eol
!                 write(lun   ,fmtbcc( 5)) keywrd( 4),eol
!                 write(lun   ,fmtbcc( 6)) keywrd( 5),eol
                  write(lun   ,fmtbcc( 7)) keywrd( 8),
     *                  quote,'non-equidistant',quote,eol
                  write(lun   ,fmtbcc( 8)) keywrd( 9),
     *                  itdate,eol
                  write(lun   ,fmtbcc( 9)) keywrd(10),
     *                  quote,'minutes',quote,eol
                  write(lun   ,fmtbcc(10)) keywrd(12),
     *                  quote,'linear',quote,eol
                  if (itypc .ne. 3) then
                     write(lun   ,fmtbcc(11)) keywrd(14),
     *                  quote,'time                '//
     *                        '                 ',
     *                  quote,keywrd(15),
     *                  quote,' ',quote,eol
                     write(lun   ,fmtbcc(11)) keywrd(14),
     *                  quote,namcon(ic)//' End A surface   ',
     *                  quote,keywrd(15),
     *                  quote,' ',quote,eol
                     if (itypc .eq. 2) then
                        write(lun   ,fmtbcc(11)) keywrd(14),
     *                     quote,namcon(ic)//' End A bed       ',
     *                     quote,keywrd(15),
     *                     quote,' ',quote,eol
                     endif
                     write(lun   ,fmtbcc(11)) keywrd(14),
     *                  quote,namcon(ic)//' End B surface   ',
     *                  quote,keywrd(15),
     *                  quote,' ',quote,eol
                     if (itypc .eq. 2) then
                        write(lun   ,fmtbcc(11)) keywrd(14),
     *                     quote,namcon(ic)//' End B bed       ',
     *                     quote,keywrd(15),
     *                     quote,' ',quote,eol
                     endif
                  else
                     write(lun   ,fmtbcc(11)) keywrd(14),
     *                  quote,'time                '//
     *                        '                 ',
     *                  quote,keywrd(15),
     *                  quote,' ',quote,eol
                     do 110 k = 1, kmax
                        layer = ' End A layer:    '
                        write (layer(15:17),'(a3)') cdum
                        write (layer(15:17),'(i3)') k
                        write(lun   ,fmtbcc(11)) keywrd(14),
     *                     quote,namcon(ic)//layer,
     *                     quote,keywrd(15),
     *                     quote,' ',quote,eol
  110                continue
                     do 120 k = 1, kmax
                        layer = ' End B layer:    '
                        write (layer(15:17),'(a3)') cdum
                        write (layer(15:17),'(i3)') k
                        write(lun   ,fmtbcc(11)) keywrd(14),
     *                     quote,namcon(ic)//layer,
     *                     quote,keywrd(15),
     *                     quote,' ',quote,eol
  120                continue
                  endif
                  write(lun   ,fmtbcc(12)) keywrd(16),
     *                  notims,eol
                endif
!----------------------------------------------------------------------
!---------------Write time depended data to block for constituent
!               L skipping first 11 records with block info
!----------------------------------------------------------------------
!               irec    = irec   + itim   + 11
                if     (itypc .eq. 1) then
                  write (fmtbcc(13) (10:12),'(i3.3)') 2
                  write (lun   ,fmtbcc(13))
     *                   tstart + (itim - 1)*dtmin,
     *                   bndval(ibnd,itim,1   ,ic,1),
     *                   bndval(ibnd,itim,1   ,ic,2),eol
                elseif     (itypc .eq. 2) then
                  write (fmtbcc(13) (10:12),'(i3.3)') 4
                  write (lun   ,fmtbcc(13))
     *                   tstart + (itim - 1)*dtmin,
     *                   bndval(ibnd,itim,1   ,ic,1),
     *                   bndval(ibnd,itim,kmax,ic,1),
     *                   bndval(ibnd,itim,1   ,ic,2),
     *                   bndval(ibnd,itim,kmax,ic,2),eol
                else
                   write (fmtbcc(13) (10:12),'(i3.3)') 2*kmax
                   write (lun   ,fmtbcc(13))
     *                   tstart + (itim - 1)*dtmin,
     *                  (bndval(ibnd,itim,k,ic,1),k=1,kmax),
     *                  (bndval(ibnd,itim,k,ic,2),k=1,kmax),eol
                endif
  100 continue

      return

      end
