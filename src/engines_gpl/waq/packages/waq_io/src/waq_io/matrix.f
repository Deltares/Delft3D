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

      subroutine dmatrix ( ntot   , nvals  , nvarar , nvarnw , nobrk1 ,
     &                     nobrk2 , ibrk   , ibrknw , tab    , tabnw  ,
     &                     item   )

!       Deltares Software Centre

!>\file
!>             Merges a new table with a table with existing values
!>
!>             This is old style DELWAQ processing
!>             - The base table is ( ntot, nobrk1 ) with the second dimension
!>               an expandable number of breakpoints up to nobrk1+nobrk2
!>             - The first nvarar variables of ntot are filled, with nobrk1
!>               breakpoints at the timings of ibrk(nobrk1) that is also expandable
!>             - Added are nvarnw variables, with nobrk2 different breakpoints
!>               at the locations ibrknw(nobrk2)
!>             - Where needed, rows are added to the original table at locations
!>               of ibrknw(nobrk2) before breakpoint j ( statement 123-130 )./n
!>               The values at the new rows for the first nvarar variables are
!>               interpolated or expanded as block function depending on item()
!>               ( statement 131-138 )./n
!>               The new nvarnw variables are copied (142-144)
!>             - Also the values of the nvarnw varibales at the old breakpoints
!>               are interpolated  ( statement 101-108 )
!>             - if breakpoints equal, the new variables are added (142-144)
!>             - if the new breakpoints expand over the old set, the breakpoints
!>               are added and the old values are copied ( statement 113-118 )
!>             - The result is a wider table (nvarar is increased with nvarnw)
!>               the sum will always be smaller or equal to ntot, because that
!>               is the total amount of data that is expected
!>             - The result is also a deeper table with potentially more break
!>               points
!>             - Note that each variable has ndim2 numbers. A variable could be
!>               a wasteload and ndim2 is then the amount of substances
!>             This leads to very hughe tables ( river flows per hour, merged
!>             35 substance concentrations gives all 36 values per hour). That
!>             is why the new input processing stores the individual tables.

!     Created            : March 1988  by M.E. Sileon / L. Postma
!     Modified           : May   2011  by Leo Postma : Fortran 90 look and feel

!     Subroutines called : interpol  : to interpolate complete matrices

!     Logical units      : none

      use timers       !   performance timers
      use m_char1

      implicit none

!     Parameters

!     kind           function         name                          Description

      integer  ( 4), intent(in   ) :: ntot                        !< first dimension of tab and tabnw
      integer  ( 4), intent(in   ) :: nvals                       !< number of values per variable
      integer  ( 4), intent(inout) :: nvarar                      !< number of existing variables in tab
      integer  ( 4), intent(in   ) :: nvarnw                      !< number of variables to add to tab
      integer  ( 4), intent(inout) :: nobrk1                      !< number of existing breakpoints
      integer  ( 4), intent(in   ) :: nobrk2                      !< number of breakpoints to add
      integer  ( 4), intent(inout) :: ibrk  (nobrk1+nobrk2)       !< values of existing breakpoints
      integer  ( 4), intent(in   ) :: ibrknw(nobrk2)              !< values of breakpoints to add
      real     ( 4), intent(inout) :: tab   (   ntot     ,nobrk1+nobrk2)  !< existing table
      real     ( 4), intent(in   ) :: tabnw (nvarnw*nvals,nobrk2)         !< table to merge
      integer  ( 4), intent(in   ) :: item  (ntot)                !< for type of interpolation per variable

!     local decalations

      integer  i        ! loop counter for position in array IBRKNW
      integer  j        ! loop counter for position in array IBRK
      integer  k        ! help counter breakpoints
      integer  iv       ! help counter variables*values
      integer  iset     ! help variable for breakpoint
      integer  nposar   ! linear position first index for existing variables
      integer  nposnw   ! amount of values first index of variables to add
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "matrix", ithndl )

!          If no breakpoints in IBRK/TAB then IBRK/ITAB==IBRKNW/TABNW

      if ( nobrk1 .eq. 0 ) then
         nobrk1 = nobrk2
!jvb     nvarar = nvarnw
         ibrk   = ibrknw
         tab( 1:nvarnw*nvals, : ) = tabnw
         goto 9999
      endif

!     Some help variables

      nposar = nvarar*nvals
      nposnw = nvarnw*nvals

!          begin loop over all breakpoints to add

      j = 0
      do 40 i = 1, nobrk2
         iset = ibrknw(i)                     ! breakpoint to insert

!          find first J with IBRK(J) >= IBRKNW(I)

         do 10 k = j+1, nobrk1
            j = k
            if ( iset .lt. ibrk(j) ) goto 20
            if ( iset .eq. ibrk(j) ) goto 30
            if ( j .eq. 1 ) then
               do iv = 1 , nposnw                             ! initialize expanded collumns for row j
                  tab( nposar+iv, j ) = tabnw( iv, i )
               enddo
            else
               call interpol ( tab(nposar+1,j), tabnw(1,i), tab(nposar+1,j-1), ibrk(j)        , iset ,
     &                         ibrk(j-1)      , nvarnw    , nvals            , item(nvarar+1) )
            endif
   10    continue

!          add values if IBRKNW(I) > IBRK(NOBRK1)

         ibrk(j+1) = iset                                     ! make a new row, initialise it with
         do iv = 1,nposar                                     ! existing collumns and go to the
            tab( iv, j+1 ) = tab( iv, j )                     ! copy of the added collumns
         enddo
         j      = j      + 1
         nobrk1 = nobrk1 + 1
         goto 30

!             insert values if IBRKNW(I) < IBRK(J)

   20    do k = nobrk1, j, -1                                 ! shift existing values up one row
            ibrk(k+1) = ibrk (k)
            do iv = 1, nposar
               tab( iv, k+1 ) = tab( iv, k )
            enddo
         enddo
         nobrk1  = nobrk1 + 1                                 ! nr of breakpoints increases
         ibrk(j) = iset                                       ! added breakpoint at the start
         if ( j .eq. 1 ) then                                 ! copy existing values upfront
            do iv = 1 , nposar
               tab( iv, 1 ) = tab( iv, 2 )
            enddo
         else                                                 ! interpolate existing values
            call interpol ( tab (1,j) , tab(1,j+1) , tab(1,j-1) , ibrk(j) , ibrk(j+1) ,
     &                      ibrk(j-1) , nvarar     , nvals      , item    )
         endif

!             add values if IBRKNW(I) = IBRK(J)

   30    do iv = 1, nposnw                                    ! expand the row with the new
            tab( nposar+iv, j ) = tabnw( iv, i )              ! columns
         enddo

   40 continue

!          end the procedure with IBRKNW(NOBRK2) < IBRK(NOBRK1)

      do i = j+1, nobrk1
         do iv = 1 , nposnw
            tab( nposar+iv, i ) = tabnw( iv, nobrk2 )
         enddo
      enddo

 9999 if (timon) call timstop( ithndl )
      return
      end
