!!  Copyright (C)  Stichting Deltares, 2012-2024.
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

module m_rdhyd
use m_stop_exit

   contains

      subroutine rdhyd  ( nfiles , lunit  , fnam   , hyd    , nolay  , zmodel , ihdel  ,      &
     &                    tcktot , zlbot  , zltop  , ndoms  , nbnds  , doms   , bnds   )

!       Deltares Software Centre

!>\file
!>                          Reads the hydrodynamic .hyd file

!       Created           : July      2011 by Leo Postma

!       Modified          : October   2011 by Leo Postma: support Domain Decomposition

!       Subroutines called:

!       Functions called  :

!       Logical units     : lun1 = the hydrodynamic .hyd file

      use m_waq_precision       ! flexible size definition
      use typos           ! the derived types
      use hydmod
      use timers          ! performance timers

      implicit none

!     Arguments

!     kind           function         name                 description

      integer  ( int_wp ), intent(in   ) :: nfiles            !< nr. of files
      integer  ( int_wp ), intent(inout) :: lunit(nfiles)     !< unit nrs of all files
      character(256), intent(inout) :: fnam (nfiles)     !< file names of all files
      type(t_hyd)   , intent(in   ) :: hyd               !< description of the hydrodynamics
      integer  ( int_wp ), intent(  out) :: nolay             !< number of hydrodynamic layers
      logical       , intent(in   ) :: zmodel            !< layer type
      integer  ( int_wp ), intent(  out) :: ihdel             !< hydrodynamic time step (s)
      real     ( sp), pointer       :: tcktot(:)         !< relative layer thickness
      real     ( sp), pointer       :: zlbot(:)          !< z-layer layer bottom level
      real     ( sp), pointer       :: zltop(:)          !< z-layer layer top level
      integer  ( int_wp ), intent(  out) :: ndoms             !< number of domains
      integer  ( int_wp ), intent(  out) :: nbnds             !< number of DD-boundaries
      type (domain) , pointer       :: doms  (:)         !< the domains
      type (boundp) , pointer       :: bnds  (:)         !< the domain boundaries

!     Locals

      integer                 error            ! integer indicating read errors
      integer                 nohydlay         ! number of hydrodynamic layers
      character(  80)         token1, token2   ! tokens read in the .hyd file
      character(1024)         line             ! one line
      integer                 i, j, k          ! loop counters
      integer                 ipath            ! if non zero, there a path to add
      real(sp)                rlay             ! helpvariable to read reals for nr of layers
      real(sp)                zdepth           ! distance between ztop and zbot
      real   , allocatable :: hydlay(:)        ! relative thickness hydrodynami layers
      integer, allocatable :: ilay  (:)        ! number of hydrodynamic layers
      type (domain), pointer ::  d2 (:)        ! help for the reallocation of domains
      type (boundp), pointer ::  b2 (:)        ! help for the reallocation of DD-boundaries

      integer(4) ithndl                        ! handle to time this subroutine
      data       ithndl / 0 /

      if ( timon ) call timstrt( "rdhyd", ithndl )

!     default no domain decomposition

      ndoms = 0
      nbnds = 0
      
!     open the .hyd file and read the required information

      ipath = scan(fnam(18),"\",.true.)
      if ( ipath .eq. 0 ) ipath = scan(fnam(18),"/",.true.)
      open ( newunit = lunit(18), file = fnam(18) )
      read ( lunit(18), '(A)', iostat=error ) line
      do while ( error .eq. 0 )
         if ( line .eq. ' ' ) then
            read ( lunit(18), '(A)', iostat=error ) line
            cycle
         endif
         read ( line, * ) token1
         select case ( token1 )
            case ( "conversion-timestep" )
               read ( line, * ) token1, token2
               read ( token2, * ) ihdel
               ihdel = (ihdel/1000000)*86400 + (mod(ihdel,1000000)/10000)*3600 +    &
     &                 (mod(ihdel,10000)/100)*60 + mod(ihdel,100)
            case ( "number-water-quality-layers" )
               read ( line, * ) token1, token2
               read ( token2, * ) nolay
               allocate ( tcktot(nolay+1), ilay(nolay) )
            case ( "number-hydrodynamic-layers" )
               read ( line, * ) token1, token2
               read ( token2, * ) nohydlay
               allocate ( hydlay(nohydlay) )
            case ( "hydrodynamic-layers" )
               do i = 1, nohydlay
                  read ( lunit(18), * ) hydlay(i)
               enddo
            case ( "pointers-file" )
               read ( line, * ) token1, token2
               if ( token2 .eq. 'none' .or. ipath .eq. 0 ) then
                  fnam(19) = token2
               else
                  fnam(19) = fnam (18)(1:ipath)//token2
               endif
               lunit(19) = lunit(18)+1
            case ( "vert-diffusion-file" )
               read ( line, * ) token1, token2
               if ( token2 .eq. 'none' .or. ipath .eq. 0 ) then
                  fnam(20) = token2
               else
                  fnam(20) = fnam (18)(1:ipath)//token2
               endif
               lunit(20) = lunit(18)+2
            case ( "shear-stresses-file" )
               read ( line, * ) token1, token2
               if ( token2 .eq. 'none' .or. ipath .eq. 0 ) then
                  fnam(21) = token2
               else
                  fnam(21) = fnam (18)(1:ipath)//token2
               endif
               lunit(21) = lunit(18)+3
            case ( "salinity-file" )
               read ( line, * ) token1, token2
               if ( token2 .eq. 'none' .or. ipath .eq. 0 ) then
                  fnam(22) = token2
               else
                  fnam(22) = fnam (18)(1:ipath)//token2
               endif
               lunit(22) = lunit(18)+4
            case ( "temperature-file" )
               read ( line, * ) token1, token2
               if ( token2 .eq. 'none' .or. ipath .eq. 0 ) then
                  fnam(23) = token2
               else
                  fnam(23) = fnam (18)(1:ipath)//token2
               endif
               lunit(23) = lunit(18)+5
            case ( "water-quality-layers" )
               do i = 1, nolay
                  read ( lunit(18), * ) rlay
                  ilay(i) = nint(rlay)
               enddo
            case ( "domains" )
               write ( lunit(2), * ) ' Domain decomposition detected.'
               write ( lunit(2), * ) ' Domain name:         mmax    nmax  aggregation'
               i     = 0
               do while ( token2 .ne. "end-domains" )
                  read  ( lunit(18), '(A)' ) line
                  read  ( line  , * ) token2
                  if ( token2 .ne. "end-domains" ) then
                     i = i + 1
                     if ( i .gt. ndoms ) then
                        allocate ( d2(ndoms+10) )
                        if ( ndoms .gt. 0 ) then
                           d2(1:ndoms) = doms(1:ndoms)
                           deallocate ( doms )
                        endif
                        doms => d2
                        ndoms = ndoms + 10
                     endif
                     read  ( line, * ) doms(i)%name, doms(i)%mmax, doms(i)%nmax, token2
                     write ( lunit(2), '(A,T20,2I8,2X,A40)' )                               &
     &                                   "  "//doms(i)%name(1:len_trim(doms(i)%name)),      &
     &                                         doms(i)%mmax, doms(i)%nmax, token2
                     if ( token2 .ne. "none" ) then
                        write ( lunit(2), * ) 'ERROR: aggregation not allowed !'
                        call stop_exit(1)
                     endif
                  endif
               enddo
               ndoms = i
               doms(1)%moff = 0
               do i = 2, ndoms
                  doms(i)%moff = doms(i-1)%moff + doms(i-1)%mmax
               enddo
            case ( "dd-boundaries" )
               i     = 0
               do while ( token2 .ne. "end-dd-boundaries" )
                  read  ( lunit(18), '(A)' ) line
                  read  ( line, * ) token2
                  if ( token2 .ne. "end-dd-boundaries" ) then
                     i = i + 1
                     if ( i .gt. nbnds ) then
                        allocate ( b2(nbnds+10) )
                        if ( nbnds .gt. 0 ) then
                           b2(1:nbnds) = bnds(1:nbnds)
                           deallocate ( bnds )
                        endif
                        bnds => b2
                        nbnds = nbnds + 10
                     endif
                     read ( line, * ) bnds(i)%r1%name, bnds(i)%r1%fm, bnds(i)%r1%fn,        &
     &                                                 bnds(i)%r1%tm, bnds(i)%r1%tn,        &
     &                                bnds(i)%r2%name, bnds(i)%r2%fm, bnds(i)%r2%fn,        &
     &                                                 bnds(i)%r2%tm, bnds(i)%r2%tn
                  endif
               enddo
               nbnds = i
               write ( lunit(2), * ) ' Number of domain boundaries =', nbnds
               do i = 1, nbnds              !     Find the corresponding domain numbers
                  bnds(i)%r1%did = 0
                  bnds(i)%r2%did = 0
                  do j = 1, ndoms
                     if ( bnds(i)%r1%name .eq. doms(j)%name ) bnds(i)%r1%did = j
                     if ( bnds(i)%r2%name .eq. doms(j)%name ) bnds(i)%r2%did = j
                  enddo
                  if ( bnds(i)%r1%did .eq. 0 .or. bnds(i)%r2%did .eq. 0 ) then
                     write ( lunit(2), * ) ' ERROR. A domain name not found: ',    &
     &                                       bnds(i)%r1%name, bnds(i)%r2%name
                     call stop_exit(1)
                  endif
               enddo
         end select
         read ( lunit(18), '(A)', iostat=error ) line
      enddo
      close ( lunit(18) )

!     make the tcktot table by aggregating the hydrodynamic coefficients

      tcktot = 0.0
      if ( nolay .eq. 1 ) then
         tcktot(1) = 1.0
      else
         k = 1
         do i = 1, nolay
            do j = 1, ilay(i)
               tcktot(i) = tcktot(i) + hydlay(k)
               k = k + 1
            enddo
         enddo
      endif

      if (zmodel) then
         allocate (zlbot(hyd%nolay))
         allocate (zltop(hyd%nolay))
         zlbot = 0.0
         zltop = 0.0
         zdepth = hyd%ztop - hyd%zbot
         zltop(1) = hyd%ztop
         zlbot(nolay) = hyd%zbot
         if ( nolay .gt. 1 ) then
            zlbot(1) = zltop(1) - tcktot(1) * zdepth
            do i = 2, nolay - 1
               zltop(i) = zlbot(i - 1)
               zlbot(i) = zltop(i) - tcktot(i) * zdepth
            enddo
            zltop(nolay) = zlbot(nolay - 1)
         endif
      endif

      if ( timon ) call timstop( ithndl )
      return
   end subroutine
   end module
