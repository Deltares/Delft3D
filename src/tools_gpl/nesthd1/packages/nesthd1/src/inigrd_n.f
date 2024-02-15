      subroutine inigrd(lun      ,fout ,nmax ,mmax , icom  ,ipx ,ipy ,
     *                                  itotpx  ,itotpy , nrptot )
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
!-----------------------------------------------------------------------
!             Module: SUBROUTINE INIGRD
!           Function: - Reads the computational grid enclosure file
!                     - Lines formed by the vertices of the polygons are
!                       checked against the condition of 45 degrees
!                     - Fills ICOM with the appropriate values to iden-
!                       tify the active/inactive points
!        Method used:
!               Date: 26-08-1992
!         Programmer: H.H. Leepel, D.K. Vatvani
!-----------------------------------------------------------------------
!        Method adjusted to delft3d Graphical Editor requirements:
!                              - variable filename and nrptot added,
!                              - trisula specific message reporting removed,
!                              - RGF specific message reporting added
!                                (already present in the grid routines)
!                              - explicit variable declarations, etc.
!                              - arrays itotpx and itotpy added (plotting)
!               Date: 18-04-97
!         Programmer: R.Pribic, CSO
!-----------------------------------------------------------------------
!   Calling routines:              ReadEnclosure
!-----------------------------------------------------------------------
!   Called  routines:              INCREM
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var.    I/O  Type Dimensions
!   -------------------------
!
! filename  I   character 100        Name of the ASCII enclosure file
!                                    containing pairs of coordinates
! FOUT      IO  I*4                  >0 if an error is encountered
! ICOM      IO  I*4  MMAX,NMAX       Temporary work array containing:
!                                      0 = if a point is not active
!                                      1 = if a point is active
!                                      * = if a point is boundary
! ITOTPX    IO  I*4  *               X-coordinate of the comp. grid
!                                    enclosure
! ITOTPY    IO  I*4  *               Y-coordinate of the comp. grid
!                                    enclosure
! MMAX      I   I*4                  Number of gridpoints in the x-dir.
! NMAX      I   I*4                  Number of gridpoints in the y-dir.
! nrptot    O   I*4                  total number of enclosure points
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! ICODE       I*4                  Help var.
! INCX        I*4                  Increment step calculated from two
!                                  x-coordinates (= 0, -1 or 1)
! INCXA       I*4                  Absolute value of INCX
! INCY        I*4                  Increment step calculated from two
!                                  Y-coordinates (= 0, -1 or 1)
! INCY0       I*4                  Help var.
! INCY1       I*4                  Help var.
! INCYA       I*4                  Absolute value of INCY
! IPOLY       I*4                  Help var. - counter for the current
!                                  polygon number
! IVAL        I*4                  Help var.
! IX          I*4                  Help var.
! IY          I*4                  Help var.
! LUNGRD      I*4                  Unit number of the file containing
!                                  the computational grid enclosure
! M           I*4                  Current M-index of the active point
!                                  in the current computational ROW
! MAXINC      I*4                  Maximum of (INCXA,INCYA)
! MAXNRP      I*4                  Local dim. (max. of NRP allowed)
! IPX     IO  I*4  *               X-coordinate of the comp. grid
!                                    enclosure (temporary/work array)
! itotpx  O   I*4  *               x-coordinates of all enclosure points
! IPY     IO  I*4  *               Y-coordinate of the comp. grid
! itotpy  O   I*4  *               y-coordinates of all enclosure points
! nrptot  O   I*4                  total number of enclosure points

! MGRD        I*4                  Help var.
! N           I*4                  Current N-index of the active point
!                                  in the current computational COLUMN
! ND          I*4                  Current N-index minus 1 (see N)
! NDD         I*4                  ND-1
! NDUM        I*4                  Help var.
! NGRD        I*4                  Help var.
! NRP         I*4                  Counter for the number of points in
!                                  the current polygon
! NU          I*4                  Current N-index plus  1 (see N)
! NUU         I*4                  NU+1
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
!
      integer    nmax , mmax ,
     *           icode, incx , incxa , incy , incy0 , incy1 , incya ,
     *           ipoly , ival , ix , iy , lun    ,
     *           m , maxinc , maxnrp , nrptot , mgrd ,
     *           n , nd , ndd , ndum , ngrd , nrp , nu , nuu
!
      integer    icom (mmax ,* ), itotpx (* ), itotpy (* )
!
      integer    ipx (* ), ipy (* )
!
      integer    fout
      integer    KEOK, KEOPEN, KECLOSE, KEMEM, KEREAD, KEWRITE, KEEOF,
     *           KEERR, KEEXTR, KEOVER, KEA45, KEEND

      include 'params.inc'

      fout = 0 
!-----------------------------------------------------------------------
!-----initialisation local parameters
!     maxnrp is check for array dimensions
!-----------------------------------------------------------------------
      do 10 n = 1,nmax
         do 20 m = 1,mmax
            icom   (m,n) = 0
   20    continue
   10 continue
!
      ipoly  =  0
      ndum   =  0
      mgrd   =  0
      ngrd   =  0
      nrp    =  0
      nrptot =  0
!
      maxnrp = nmax * (mmax+4)

!-----------------------------------------------------------------------
!-----read first coordinate pair for first polygon
!     ****  no restriction so far ***
!-----------------------------------------------------------------------
      read (lun   , * , end=4444,err=4444) mgrd  ,ngrd
      ipx   (1) = mgrd
      ipy   (1) = ngrd
      itotpx(1) = mgrd
      itotpy(1) = ngrd
!
! --> read for all polygons
!
  110 continue
         nrp    = 1
         nrptot = nrptot+1
         ipoly  = ipoly  + 1
!
! --->   read all points of one polygon
!
  120    continue
            read (lun   , *, err=1000,end=1001) mgrd  ,ngrd
            ipx   (nrp+1) = mgrd
            ipy   (nrp+1) = ngrd
            itotpx(nrptot+1) = mgrd
            itotpy(nrptot+1) = ngrd
!-----------------------------------------------------------------------
!-----------check against 1,1 and mmax,nmax
!      ** restriction ?!:
!                        1 can, *max+1 not -> neither old nor new raster
!-----------------------------------------------------------------------
            if (mgrd .gt. mmax .or. ngrd+1 .gt. nmax+1 .or.
     *          mgrd .lt.      1 .or. ngrd   .lt.    1) then
               goto 5555
            endif
!
            if (ipx   (nrp+1) .ne. ipx   (1) .or.
     *          ipy   (nrp+1) .ne. ipy   (1)) then
                nrp    = nrp + 1
                nrptot = nrptot + 1
                if (nrp    .gt. maxnrp) then
                   goto 6666
                endif
! <---
               goto 120
            else
!------------- end of a polygon ----------------------------------------
               nrptot = nrptot + 1
            endif
!-----------------------------------------------------------------------
!--------fill non-horizontal "enclosure points"
!        all line segment checked (loop 1-nrp and segment (n,n+1))
!        NOTE: fout will never appear (ndum = ndum)
!-----------------------------------------------------------------------
         call increm (ndum  ,ipy   (nrp  ),ndum  ,ipy   (    1),
     *                incx  ,incy  ,maxinc, fout                )
         do 210 n = 1,nrp
            incy0  = incy
            call increm (ipx   (n    ),ipy   (n     ),ipx   (n+1  ),
     *                   ipy   (n+1  ),incx  ,incy   ,maxinc, fout  )
!-----------------------------------------------------------------------
!-----------if fout then vertice not multiple of 45 degrees
!-----------------------------------------------------------------------
            if (fout.gt.0) then
                goto 7777
            endif
!-----------------------------------------------------------------------
!-----------fill linesegment with a code = 2, for vertical enclosure
!           polygon
!-----------------------------------------------------------------------
            if (incy   .ne. 0) then
               incxa = abs (incx  )
               incya = abs (incy  )
               ix     = ipx   (n)
               iy     = ipy   (n)
               ival   = 2
               do 220 m = 1,maxinc-1
                  ix     = ix     + incx
                  iy     = iy     + incy
!-----------------------------------------------------------------------
!-----------------check if the lines intersects
!                 OPM. : Met de huidige aanpak levert het detecteren van
!                        hoekpunten die samenvallen, problemen op
!                        nog niet opgelost
!-----------------------------------------------------------------------
!                 if (ix .ne. ipx (npr+1) .and.
!    *                iy .ne. ipy (nrp+1)) then
!                    if (icom (ix,iy) .eq. 2 .or.
!    *                   icom (ix,iy) .eq. 3) then
!                        goto 8888
!                    endif
!                 endif
!-----------------------------------------------------------------------
                  icom  (ix,iy) = ival
  220          continue
!-----------------------------------------------------------------------
!--------------fill vertices
!              GEEN CHECK zie opm. boven
!-----------------------------------------------------------------------
               icom  (ipx   (n),ipy   (n)) = 2
               if (incy0 * incy   .lt. 0) then
                  icom  (ipx   (n),ipy   (n)) = 3
               endif
               if (incy0 .eq. 0) then
                  nd     = max (1,n-1 )
                  ndd    = max (1,nd-1)
                  incy1  = ipy   (nd ) - ipy   (ndd)
                  if (incy1 * incy   .lt. 0) then
                     icom  (ipx   (n),ipy   (n)) = 3
                  endif
               endif
            endif
  210    continue
!-----------------------------------------------------------------------
!--------fill horizontal enclosure points
!        NOTE: fout will never appear (ndum = ndum)
!-----------------------------------------------------------------------
         call increm (ndum  ,ipy   (nrp  ),ndum  ,ipy   (    1),
     *                incx  ,incy  ,maxinc,  fout                )
         do 310 n = 1,nrp
            incy0  = incy
            call increm (ipx   (n    ),ipy   (n    ),ipx   (n+1  ),
     *                   ipy   (n+1  ),incx  ,incy  ,maxinc, fout  )
!-----------------------------------------------------------------------
!-----------if fout then error appeared in loop 210 (=> impossible)
!-----------------------------------------------------------------------
            if (incy   .eq. 0) then
               ix     = ipx   (n)
               iy     = ipy   (n)
               ival   = -abs (ipx   (n+1) - ipx   (n))
               do 320 m = 1,maxinc
                  ix    = ix     + incx
                  iy    = iy     + incy
!-----------------------------------------------------------------------
!-----------------check if the lines intersects
!                 OPM. : Met de huidige aanpak levert het detecteren van
!                        hoekpunten die samenvallen, problemen op
!                        nog niet op gelost
!-----------------------------------------------------------------------
!                 if (ix .ne. ipx (npr+1) .and.
!    *                iy .ne. ipy (nrp+1)) then
!                    if (icom (ix,iy) .eq. 2 .or.
!    *                   icom (ix,iy) .eq. 3) then
!                        goto 8888
!                    endif
!                 endif
!-----------------------------------------------------------------------
                  icom  (ix,iy) = ival
  320          continue
!-----------------------------------------------------------------------
!--------------fill vertices
!              GEEN CHECK zie opm. boven
!-----------------------------------------------------------------------
               icom  (ipx   (n),ipy   (n)) = 2
               nu     = n      + 1
               if (nu     .gt. nrp) nu = 1
               nuu    = nu     + 1
               if (nuu    .gt. nrp) nuu = 1
               incy1  = ipy   (nuu) - ipy   (nu )
               if (incy1 * incy0  .lt. 0) then
                  icom  (ipx   (n),ipy   (n)) = 3
               endif
            endif
  310    continue
!-----------------------------------------------------------------------
!--------read new polygon, if any
!-----------------------------------------------------------------------
         read (lun   , *, err=1000,end=1001) mgrd  ,ngrd
         ipx   (1) = mgrd
         ipy   (1) = ngrd
         itotpx(nrptot+1) = mgrd
         itotpy(nrptot+1) = ngrd
      goto 110
!
! <--
 1000 fout   = KEREAD
 1001 continue
!-----------------------------------------------------------------------
!-----Begin and end points of polygon not identical -> error;
!     Fill icom if no error is encountered
!-----------------------------------------------------------------------
      if ((fout.gt.0) .or. (ipx   (nrp+1) .ne. ipx   (1)
     *                .or.  ipy   (nrp+1) .ne. ipy   (1))) then
         goto 8888
      else
         do 1110 n = 1,nmax
            icode  = 0
            m      = 1
! -->
 1120       continue
!-----------------------------------------------------------------------
!--------------build matrix with computational points
!-----------------------------------------------------------------------
               if (icom  (m,n) .lt. 0) then
                  m      = m      - icom   (m,n)
               else if (icom  (m,n) .eq. 2) then
                  icode  = 1      - icode
                  m      = m      + 1
               else if (icom  (m,n) .eq. 3) then
                  m      = m      + 1
               else
                  icom  (m,n) = icode
                  m      = m      + 1
               endif
            if (m      .lt. mmax  ) goto 1120
! <--
 1110    continue
      endif
      go to 9999
!-----------------------------------------------------------------------
 2222 fout   = KEOPEN
      go to 9999
 4444 fout   = KEREAD
      go to 9999
 5555 fout   = KEEXTR
      go to 9999
 6666 fout   = KEOVER
      go to 9999
 7777 fout   = KEA45
      go to 9999
 8888 fout   = KEEND
      go to 9999
 9999 continue

      end
