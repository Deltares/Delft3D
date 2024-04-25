      subroutine increm (mx1   ,ny1   ,mx2   ,ny2   ,incx  ,incy  ,
     *                   maxinc,fout                              )
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
!             Module: SUBROUTINE INCREM
!           Function: Computes increments (-1, 0 or 1) in the x- and y-
!                     coordinates of the given two points
!                     NOTE: The angle of the line spanned by these
!                           points with respect to the numerical grid
!                           must be a multiple of 45 deg.
!        Method used:
!               Date: 30-04-1992
!         Programmer: H.H. Leepel, D.K. Vatvani
!-----------------------------------------------------------------------
!        Method adjusted to delft3d Graphical Editor requirements:
!                         - explicit variable declaractions
!                         - trisula specific message reporting removed,
!                         - RGF specific message reporting added
!                           (already present in the grid routines)
!               Date: 18-04-97
!         Programmer: R.Pribic, CSO
!-----------------------------------------------------------------------
!   Calling routines:                INIGRD
!-----------------------------------------------------------------------
!   Called  routines:                NONE
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! FOUT     O  I*4                  >0 if an error is encountered
! INCX    IO  I*4                  Increment step calculated from two
!                                  x-coordinates (= 0, -1 or 1)
! INCY    IO  I*4                  Increment step calculated from two
!                                  Y-coordinates (= 0, -1 or 1)
! MAXINC  IO  I*4                  Maximum of (INCXA,INCYA)
! MX1     I   I*4                  M-coord. of the first  point
! MX2     I   I*4                  M-coord. of the second point
! NY1     I   I*4                  N-coord. of the first  point
! NY2     I   I*4                  N-coord. of the second point
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! INCXA       I*4                  Absolute value of INCX
! INCYA       I*4                  Absolute value of INCY
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
      integer mx1   ,ny1   ,mx2   ,ny2   ,incx  ,incy  , maxinc
      integer fout
      integer incxa , incya
      integer    KEOK, KEOPEN, KECLOSE, KEMEM, KEREAD, KEWRITE, KEEOF,
     *           KEERR, KEEXTR, KEOVER, KEA45, KEEND

      include 'params.inc'
!-----------------------------------------------------------------------
!-----bereken afstanden in x en y as
!-----------------------------------------------------------------------
      incx   = mx2    - mx1
      incy   = ny2    - ny1
      incxa  = abs (incx  )
      incya  = abs (incy  )
      maxinc = max (incxa ,incya )
!-----------------------------------------------------------------------
!-----test hoek veelvoud 45 graden
!-----------------------------------------------------------------------
      if (maxinc .ne. 0) then
         if (incx   .eq. 0) then
            incy   = incy   / maxinc
         else if (incy   .eq. 0) then
            incx   = incx   / maxinc
         else
            if (incxa  .ne. incya) then
               fout   = KEERR
            else
               incx   = incx   / maxinc
               incy   = incy   / maxinc
            endif
         endif
      endif
!-----------------------------------------------------------------------
      return
      end
