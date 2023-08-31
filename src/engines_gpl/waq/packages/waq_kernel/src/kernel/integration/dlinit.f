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
      module m_dlinit

      implicit none

      contains


      subroutine dlinit ( lunut   , noseg   , noq1    , noq2    , noq     ,
     &                    volume  , nopa    , paname  , param   , ilflag  ,
     &                    aleng   , flowpnt , nmax    , mmax    , kmax    ,
     &                    lgrida  , gsqs    , guv     , gvu     , cell_x  ,
     &                    cell_y  , guu     , gvv     , kcs     , thick   ,
     &                    sig     )

!     Deltares Software Centre

!>\File
!>           Initializes the constant Delft3d-FLOW arrays
!>
!>           Delwaq has a first, second and third direction:
!>           - the Delwaq first direction corresponds with:
!>             - the first index of 2D and 3D arrays in Delwaq
!>             - the first or n-nmax index in 2D and 3D arrays in D3D-Flow
!>             - the second <conceptual> index in D3D-Flow (in the manual). That is why:
!>               - the manual talks about (m,n) coordinates
!>               - the software implements them in (n,m) arrays
!>               - this keeps us from the street a bit longer
!>             - the y-direction
!>             - the v-velocity direction, running through v-points
!>             - the Eta-direction
!>           - the Delwaq second direction corresponds with:
!>             - m-mmax, x-direction, u-direction through u-points, the Ksi-direction in Flow
!>           - the Delwaq third direction corresponds with the vertical
!>             - the Delwaq layer numbering is from top to bottom and flow is positive downward
!>             - the D3D-Flow layer numbering is from top to bottom and velocities are positive upward
!>           Delwaq may feature an 'active only' grid instead of full matrices:
!>           - the lgrida(nmax,mmax) array indicates the Delwaq cell and boundary numbers in the
!>             horizontal plane
!>           - the cellpnt(noseg) array (not used here) contains the backpointer to lgrid coordinates
!>           - the flowpnt(noq) array contains the backpointering to (nmax,mmax,kmax) indices for the
!>             noq Delwaq flows. They cannot be used directly in a linear assignment because Flow arrays
!>             may have dimensions ( 1:nmax , -1:mmax+2 , 0:kmax ), to keep us from the street even longer.
!>           - guv should better be called gyv because it is the diffusion length in y direction in a v point
!>           - gvu should better be called gxu because it is the diffusion length in x direction in a u point
!>           - guu should better be called gyu because it is the grid distance dy in a u point
!>           - gvv should better be called gxv because it is the grid distance dx in a v point

!     Created             : August 2011    by Leo Postma

!     Subroutines called  : zoek20  - search for a string in an array of 20 character strings

!     Unit numbers        : lunut   - output monitoring report file

      use m_zoek
      use m_srstop
      use timers
      implicit none

!     Arguments           :

!     kind           function         name                      description

      integer  ( 4), intent(in   ) :: lunut                   !< unit number output report file
      integer  ( 4), intent(in   ) :: noseg                   !< number of Delwaq volumes
      integer  ( 4), intent(in   ) :: noq1                    !< number of Delwaq exchanges first direction
      integer  ( 4), intent(in   ) :: noq2                    !< number of Delwaq exchanges second direction
      integer  ( 4), intent(in   ) :: noq                     !< total number of Delwaq exchanges
      real     ( 4), intent(in   ) :: volume (noseg)          !< Delwaq volumes
      integer  ( 4), intent(in   ) :: nopa                    !< number of Delwaq parameters
      character(20), intent(in   ) :: paname (nopa)           !< parameter names
      real     ( 4), intent(in   ) :: param  (nopa,noseg)     !< parameter values
      integer  ( 4), intent(in   ) :: ilflag                  !< if 1 then spatially varying lengthes
      real     ( 4), intent(in   ) :: aleng  (2,*)            !< Delwaq half lengthes
      integer  ( 4), intent(in   ) :: flowpnt( noq )          !< backpointer from noq to 3*mnmaxk-mnmax
      integer  ( 4), intent(in   ) :: nmax                    !< first dimension regular matrix
      integer  ( 4), intent(in   ) :: mmax                    !< second dimension regular matrix
      integer  ( 4), intent(in   ) :: kmax                    !< third dimension regular matrix
      integer  ( 4), intent(in   ) :: lgrida (nmax,   mmax  ) !< active grid table
      real     ( 4), intent(  out) :: gsqs   (nmax,-1:mmax+2) !< horizontal surface area's
      real     ( 4), intent(  out) :: guv    (nmax,-1:mmax+2) !< distance in first  - y direction in v point
      real     ( 4), intent(  out) :: gvu    (nmax,-1:mmax+2) !< distance in second - x direction in u point
      real     ( 4), intent(in   ) :: cell_x (nmax,mmax)      !< x-value cell corners
      real     ( 4), intent(in   ) :: cell_y (nmax,mmax)      !< y-value cell corners
      real     ( 4), intent(  out) :: guu    (nmax,-1:mmax+2) !< width over u point (so dy locally)
      real     ( 4), intent(  out) :: gvv    (nmax,-1:mmax+2) !< width over v point (so dx locally)
      integer  ( 4), intent(  out) :: kcs    (nmax,-1:mmax+2) !< constant property array
      real     ( 4), intent(  out) :: thick  (kmax)           !< relative layer thickness
      real     ( 4), intent(  out) :: sig    (kmax)           !< relative position of layer centre

!     locals

      integer  ( 4) isurf           ! parameter number of the surface area parameter (required)
      integer  ( 4) ierr            ! local error variable
      integer  ( 4) n, m            ! loop variables
      integer  ( 4) iq, iseg, ilay  ! loop variables
      integer  ( 4) noq1l, noq2l    ! number of exdchanges in 1st and 2nd direction per layer
      integer  ( 4) mnmax           ! one layer mmax*nmax
      integer  ( 4) nosegl          ! number of segments per layer
      integer  ( 4) nq, mq          ! help variables for iq => n,m translation
      integer  ( 4) lgr             ! help variable lgrid table
      integer  ( 4) imax            ! help variable to determine maximum volume
      real     ( 4) amax            ! help variable to determine maximum volume


      integer  ( 4) ithandl /0/
      if ( timon ) call timstrt ( "flwinit", ithandl )

!        Some initialisation

      ierr = 0
      mnmax  = nmax*mmax
      noq1l  = noq1  / kmax
      noq2l  = noq2  / kmax
      nosegl = noseg / kmax

!        Fill the horizontal surface area array gsqs

      call zoek20 ( 'SURF      ', nopa  , paname, 10, isurf   )
      if ( isurf .le. 0 ) then
         write ( lunut, * ) 'ERROR: parameter SURF not found, simulation prohibited !'
         ierr = 1
         goto 9999
      endif
      gsqs = 0.0
      guu  = 0.0
      gvv  = 0.0
      do m = 1, mmax
         do n = 1, nmax
            lgr = lgrida( n, m )
            if ( lgr .gt. 0 ) gsqs( n, m ) = param( isurf, lgr )
            if ( lgr .lt. 0 ) gsqs( n, m ) = 1.0
            if ( n .gt. 1 .and. lgr .ne. 0 ) then
               guu( n, m ) = (cell_x(n,m)-cell_x(n-1,m))*(cell_x(n,m)-cell_x(n-1,m)) +
     &                       (cell_y(n,m)-cell_y(n-1,m))*(cell_y(n,m)-cell_y(n-1,m))
               if ( guu(n,m) .gt. 1.0e-25 ) guu(n,m) = sqrt(guu(n,m))
            endif
            if ( m .gt. 1 .and. lgr .ne. 0 ) then
               gvv( n, m ) = (cell_x(n,m)-cell_x(n,m-1))*(cell_x(n,m)-cell_x(n,m-1)) +
     &                       (cell_y(n,m)-cell_y(n,m-1))*(cell_y(n,m)-cell_y(n,m-1))
               if ( gvv(n,m) .gt. 1.0e-25 ) gvv(n,m) = sqrt(gvv(n,m))
            endif
            select case ( lgr )
               case ( :-1 )
                  kcs(n,m) = 2
               case ( 0   )
                  kcs(n,m) = 0
               case ( 1:  )
                  kcs(n,m) = 1
            end select
         enddo
      enddo

!        Fill the array with horizontal mesh sizes

      if ( ilflag .eq.1 ) then
         guv = 0.0
         gvu = 0.0
         do iq = 1, noq1l
            n = mod( flowpnt(iq)-1, nmax  ) + 1
            m = mod( flowpnt(iq)-1, mnmax ) / nmax + 1
            if ( lgrida( n, m ) .ne. 0 ) guv(n,m) = aleng(1,iq) + aleng(2,iq)
         enddo
         do iq = noq1 + 1, noq1+noq2l
            n = mod( flowpnt(iq)-1, nmax ) + 1
            m = mod( flowpnt(iq)-1, mnmax ) / nmax + 1
            if ( lgrida( n, m ) .ne. 0 ) gvu(n,m) = aleng(1,iq) + aleng(2,iq)
         enddo
      else
         guv = aleng(1,1)
         gvu = aleng(2,1)
      endif

!        Relative layer dimensions

      imax = 0
      amax = 0.0
      do iseg = 1, nosegl
         if ( volume(iseg) .gt. amax ) then
            amax = volume(iseg)
            imax = iseg
         endif
      enddo
      amax = 0.0
      do ilay = 1, kmax
         amax = amax + volume(imax+(ilay-1)*nosegl)
      enddo
      sig = 0.0
      do ilay = 1, kmax
         thick(ilay) = volume(imax+(ilay-1)*nosegl) / amax
         sig  (ilay) = sig(ilay) - thick(ilay)/2.0
         if ( ilay .ne. kmax ) sig(ilay+1) = sig(ilay) - thick(ilay)/2.0
      enddo

 9999 if ( ierr .ne. 0 ) call srstop(1)
      if ( timon ) call timstop ( ithandl )

      return
      end

      end module m_dlinit
