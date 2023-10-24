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
      module m_opt3
      use m_rwfunc


      implicit none

      contains


      subroutine opt3 ( lun    , lchar  , is     , nitem  , nvals  ,
     &                  nscal  , ifact  , dtflg  , dtflg3 , nrfunc ,
     &                  nrharm , iwidth , ioutpt , ierr   )

!       Deltares Software Centre

!>\file
!>             Read time dependent variables
!>
!>             Time depending data can come in 2 ways
!>             - a table with values at breakpoints
!>             - a table with harmonic or Fourier values
!>             The values at breakpoints require following input:
!>             - iopt, should be 1 (no defaults) or 2 (defaults and overridings)
!>             - number of items in this block    (nvarnw, read in rdpoin)
!>             - that many ID values of the items (itemId, read in rdpoin)
!>             - number of breakpoints (nobrk2, this in the number of time steps)
!>             - scale values to be applied for this block ( 1 or nval1 )
!>             - table of values in (nval1,nitem) order.
!>             The function option requires the following input
!>             - iopt, should be 3 (harmonics) or 4 (Fouriers)
!>             - number of items in this block    (nvarnw, read in rdpoin)
!>             - that many ID values of the items (itemId, read in rdpoin)
!>             - number of harmonics or Fourier components (nhar  , read in rwfunc)
!>             - nval1 values for the zero-th harmonic  (the mean , read in rwfunc)
!>             - nhar times:
!>               - a period of the harmonic    ( NOT for the Fouriers )
!>               - the phase of the harmonic, or Fourier
!>               - nval1 amplitudes of this component
!>             A number of these blocks are read, untill all items got a value for
!>             all nval1/n
!>             For the new processing of Bounds, Wastes and Funcs, the file is
!>             - initialised with a specific header
!>             - written per block with:
!>               - heading block information
!>               - breakpoint + matrix at the breakpoint for each breakpoint.
!>             For the old processing the blocks are merged to one big matrix. The
!>             information on the items is written in the system file. The big matrix
!>             of size (nval1,nitem,nobrkt) is written to the binary file. Because
!>             the size of this total matrix is not clear in advance, the matrix is
!>             reallocated for every new block. Previous versions that used a swap file
!>             for the matrix have been phased out, because memory is likely not a
!>             problem at the moment any more.

!     Created            : April 1988 by M.E. Sileon / Leo Postma

!     Modified           : April 1997 by R. Bruinsma : Tokenized input data file reading added
!                          May   2011 by Leo Postma  : Fortran90 look and feel, own memory

!     Subroutines called : rdpoin   - pointers of input variable to model items
!                          fmread   - read of a breakpoint series of matrices
!                          dmatrix  - merge of 2 matrices
!                          rwfunc   - read of a block of harmonics or Fourier series
!                          open_waq_files   - open a file

!     Functions called   : gettok   - tokenized input data file reading

!     Logical units      : lunut   = unit formatted output file
!                          lun( 3) = unit binary intermediate file for harmonics
!                          lun( 4) = unit binary intermediate file for pointers
!                          lun(is) = unit binary intermediate file for function

      use m_rdpoin
      use m_matrix
      use m_fmread
      use m_open_waq_files
      use timers       !   performance timers
      use rd_token
      use m_sysn          ! System characteristics


      implicit none

!     Parameters

!     kind           function         name                Descriptipon

      integer  ( 4), intent(inout) :: lun   (*)         !< array with unit numbers
      character( *), intent(in   ) :: lchar (*)         !< array with file names of the files
      integer  ( 4), intent(in   ) :: is                !< entry in lun for this call
      integer  ( 4), intent(in   ) :: nitem             !< number of required items
      integer  ( 4), intent(in   ) :: nvals             !< number of values per item
      integer  ( 4), intent(in   ) :: nscal             !< number of scale values
      integer  ( 4), intent(in   ) :: ifact             !< factor between clocks
      logical      , intent(in   ) :: dtflg             !< 'date'-format for output ?
      logical      , intent(in   ) :: dtflg3            !< 'date'-format (F;ddmmhhss,T;yydddhh)
      integer  ( 4), intent(  out) :: nrfunc            !< number of functions
      integer  ( 4), intent(  out) :: nrharm            !< number of harmonic functions
      integer  ( 4), intent(in   ) :: iwidth            !< width of the output file
      integer  ( 4), intent(in   ) :: ioutpt            !< flag for more or less output
      integer  ( 4), intent(inout) :: ierr              !< error count / switch



!     local decalations

      integer(4), pointer     :: breaks(:)        !  breakpoints
      integer(4), allocatable :: break2(:)        !  breakpoints of a block
      integer(4), pointer     :: break3(:)        !  help pointer for resizing
      real   (4), pointer     :: values(:,:)      !  values
      real   (4), allocatable :: value2(:,:)      !  values of a block
      real   (4), pointer     :: value3(:,:)      !  help pointer for resizing
      integer(4) itemId(nitem)     !  array for itemIds
      real   (4) factor(nvals)     !  array for scale factors
      integer(4) nval1             !  nval1 but at least 1
      logical(4) bound             !  boundary ?
      logical(4) waste             !  wastes ?
      logical(4) funcs             !  segment functions ?
      logical(4) found             !  help variable for finding strings
      integer(4) ierr2             !  local error variable
      integer(4) ifilsz            !  local counter of used integer array space
      integer(4) jfilsz            !  local counter of used real array space
      integer(4) ntot              !  nitem*nval1, real space of one breakpoint
      integer(4) ntotal            !  total number of items with input
      integer(4) nobrkt            !  total number of breakpoints
      integer(4) nobrk2            !  number of breakpoints in this block
      integer(4) newbrk            !  number of breakpoints for the new allocation
      integer(4) iopt3             !  option for this block
      integer(4) nvarnw            !  number of items in a block
      integer(4) lunuit            !  the unit of the binary file
      integer(4) i1, i2, k         !  loop counters
      integer(4) ibrk              !  loop counter breakpoints
      integer(4) iscal             !  loop counter scale values
      integer(4) nrec              !  total nr of rec's
      integer(4) nrec2             !  local nr of rec's
      integer(4) nvarar            !  number of items previous read
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "opt3", ithndl )

!          Initialisations

      breaks => null()
      break3 => null()
      values => null()
      value3 => null()

      bound  = .false.
      waste  = .false.
      funcs  = .false.
      if ( ierr .eq. -1 ) bound = .true.
      if ( ierr .eq. -2 ) waste = .true.
      if ( ierr .eq. -3 ) funcs = .true.
      nval1  = nvals
      if ( funcs .and. nvals .eq. 0 ) nval1 = 1
      ierr   = 0
      ifilsz = 0
      jfilsz = 0
      ntot   = nitem*nval1
      ntotal = 1
      nobrkt = 0
      nrec   = 0

!         write headers for new style time series files

      write ( lunut , 2000 )
!        open the output work file
!        write nr of items and nr of substances
!        write default values ( IORDER = 1 , NPNT = 0 )
      lunuit  = lun( 3)
      if ( .not. funcs ) call open_waq_files ( lun(is) , lchar(is) , is    , 1     , ierr2 )
      if ( bound ) then
         write ( lun(is) ) ' 4.900BOUND '
         write ( lun(is) ) nitem, nval1
         write ( lun(is) ) 1, 0, nval1, ( k , k = 1,nval1  ), 1, 0
      endif
      if ( waste ) then
         write ( lun(is) ) ' 4.900WASTE '
         write ( lun(is) ) nitem, nval1
         write ( lun(is) ) 1, 0, nval1, ( k , k = 0,nval1-1), 1, 0
      endif
      if ( bound .or. waste ) then
         write ( lun(is) ) 1
         write ( lun(is) ) 0, ( 0.0 , k = 1,nval1 )
         ifilsz = ifilsz + 2 + 3 + nval1 + 3 + 1
         jfilsz = jfilsz + nval1
      endif
      if ( bound .or. waste .or. funcs ) lunuit  = lun(is)

      do while ( ntotal-1 .lt. nitem )     ! loop over blocks till completion

!           read the type of block that comes

         if ( gettoken( iopt3, ierr2 ) .gt. 0 ) goto 100
         write ( lunut , 2010 ) iopt3
         if ( iopt3 .lt. 1 .or. iopt3 .gt. 4 ) then
            write ( lunut , 2020 )
            goto 100
         endif

!           the items in this block by itemnumber

         call rdpoin ( nitem  , iopt3  , ioutpt , itemId(ntotal), nvarnw ,
     &                 ierr   )

!           new style for boundaries and wastes

         if ( bound .or. funcs )
     &      write ( lun(is) ) 1, nvarnw, ( iabs(itemId(ntotal+k)), k=0,nvarnw-1),
     &                           nvals , ( k , k=1,nvals   ), iopt3 , 1
         if ( waste )
     &      write ( lun(is) ) 1, nvarnw, ( iabs(itemId(ntotal+k)), k=0,nvarnw-1),
     &                           nval1 , ( k , k=0,nval1-1 ), iopt3 , 1
         if ( bound .or. waste .or. funcs )
     &      ifilsz = ifilsz + 5 + nvarnw + nvals

         select case ( iopt3 )

            case ( 1, 2 )             !         Read time-dependent items on breakpoints
               if ( gettoken( nobrk2, ierr2 ) .gt. 0 ) goto 100
               write ( lunut, 2030 ) nobrk2
               allocate ( break2(nobrk2), value2(nvarnw*nval1,nobrk2) )
               do iscal = 1, nscal
                  if ( gettoken( factor(iscal), ierr2 ) .gt. 0 ) goto 100
               enddo
               call fmread ( nvarnw , itemId(ntotal), nval1  , nscal  , factor ,
     &                       nobrk2 , break2        , value2 , dtflg  , dtflg3 ,
     &                       ifact  , iwidth        , ioutpt , ierr   )

               if ( bound .or. waste .or. funcs ) then
                  write ( lun(is) ) nobrk2                      ! boundaries, wastes and
                  ifilsz = ifilsz + 1                           ! functions are written
                  do ibrk = 1, nobrk2                           ! directly per block
                     write ( lun(is) ) break2(ibrk), value2(:,ibrk)
                  enddo
                  ifilsz = ifilsz + nobrk2
                  jfilsz = jfilsz + nobrk2*nvarnw*nval1
               else                                             ! other are merged into
                  newbrk = nobrkt + nobrk2                      ! one big matrix that is
                  allocate ( break3(newbrk), value3(ntot,newbrk) )  ! written at the end
                  if ( nobrkt .gt. 0 ) then
                     break3(  1:nobrkt) = breaks(  1:nobrkt)
                     value3(:,1:nobrkt) = values(:,1:nobrkt)    ! expand the matrix to allow
                     deallocate( breaks, values )               ! for the new values to enter
                  endif
                  breaks => break3
                  values => value3
                  nvarar =  ntotal - 1
                  call dmatrix ( ntot   , nval1  , nvarar , nvarnw , nobrkt ,
     &                           nobrk2 , breaks , break2 , values , value2 ,
     &                           itemId(ntotal)  )
               endif
               deallocate ( break2, value2 )

            case ( 3, 4 )            !         Read items as functions
               write ( lunut , 2050 )
               nrec2 = 0                                        ! these function blocks are
               if ( bound .or. funcs ) then                     ! are written in the
                  ierr2 = -1                                    ! lunuit = lun(is) file
               endif                                            ! for bounds, wastes and funcs
               if ( waste ) then                                ! and to lunuit = lun(3), the
                  ierr2 = -2                                    ! system file, for others
               endif
               call rwfunc ( iopt3  , nvarnw , nval1  , itemId(ntotal), nrec2  ,
     &                       nharms , ifact  , dtflg  , dtflg3        , lunuit ,
     &                       iwidth , ioutpt , ierr2  )
               ierr = ierr + ierr2
               if ( bound .or. waste .or. funcs ) then
                  ifilsz = ifilsz + nrec2
                  jfilsz = jfilsz + nrec2*(nvarnw*nval1 + 1)
               else
                  nrec = nrec + nrec2
               endif

         end select

         ntotal = ntotal + nvarnw

      enddo

      if ( ntotal-1 .gt. nitem ) then
         write ( lunut , 2060 ) ntotal-1 , nitem
         ierr = ierr + 1
      endif

!      Check complete pointer structure

      do i1 = 1, nitem
         found = .false.
         do i2 = 1, nitem
            if ( iabs(itemid(i2)) .eq. i1 ) then
               if ( found ) then
                  write ( lunut , 2070 ) i1
                  ierr = ierr+1
               else
                  found = .true.
               endif
            endif
         enddo
         if ( .not. found ) then
            write ( lunut , 2080 ) i1
            ierr = ierr+1
         endif
      enddo

!      Finalize this input section

      if ( bound .or. waste .or. funcs ) then
         newrsp = newrsp + jfilsz
         newisp = newisp + ifilsz
      else                         !    write pointers and breakpoint matrix
         write ( lun(4) ) itemid,  0, 0, 0
         do ibrk = 1, nobrkt
            write ( lun(is)) breaks(ibrk), values(:,ibrk)
         enddo
         close ( lun(is) )
         nlines = nlines + ntot*2
         npoins = npoins + nitem  + 3
         nrfunc = ntot
         nrharm = nrec
         niharm = niharm + nrec
      endif
      ierr = ierr + ierr2
      if ( associated (breaks) ) deallocate ( breaks, values )
      if (timon) call timstop( ithndl )
      return

  100 ierr = ierr+1
      return

!       Output formats

 2000 format (    ' Time variable data.')
 2010 format (  /,' Option selected : ',I2 )
 2020 format (  /,' ERROR, option not implemented')
 2040 format (    ' Block with data at breakpoints.')
 2030 format (    ' Number of breakpoints:',I7 )
 2050 format (    ' Block with periodic functions.')
 2060 format (    ' ERROR, too many (',I5,') items, ',I5,' expected!')
 2070 format (    ' ERROR, duplicate item:',I5)
 2080 format (    ' ERROR, non-initialised item:',I5)
 2090 format (    ' ERROR end of file on unit:',I3,
     &          /,' Filename is: ',A )
 2100 format (    ' ERROR reading file on unit:',I3,
     &          /,' Filename is: ',A )

      end

      end module m_opt3
