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
module m_dlwqg3

implicit none

contains


      subroutine dlwqg3 ( noseg  , nobnd  , noq1   , noq2   , noq    ,         &
     &                    ipoint , nodisp , novelo , idpnt  , ivpnt  ,         &
     &                    area   , flow   , aleng  , disp   , disper ,         &
     &                    velo   , isys   , iopt   , ilflag , nomat  ,         &
     &                    amat   , imat   , idiag  , diag   , diagcc ,         &
     &                    iscale , fmat   , tmat   , iknmrk )

!     Deltares - Delft Software Department

!     Created   : Sept.1996 by Leo Postma

!     Function  : fills off-diagonals of the matrix for GMRES fast solver
!                 horizontally according to backward differences in space
!                 vertically   according to central  differences in space
!                 NOTE !! the additional velocities in the vertical (like the settling
!                         velocities of suspended sediments) are maintained BACKWARD !!

!     Modified  : Feb. 1997, Robert Vos  : Check on zero's in the scaling
!                 Feb. 1997, Kian Tan    : central differences vertically
!                            IOPT 2,3 implemented by RJV for scheme 16
!                            IOPT > 3 implemented by RJV for scheme 16 in vertical
!                 July 2008, Leo Postma  : WAQ perfomance timers
!                 July 2009, Leo Postma  : double precission version

      use timers                         ! WAQ performance timers

      implicit none

!     Arguments           :

!     Kind        Function         Name                  Description

      integer(4), intent(in   ) :: noseg               ! Number of computational volumes
      integer(4), intent(in   ) :: nobnd               ! Number of open boundaries
      integer(4), intent(in   ) :: noq1                ! Number of fluxes first direction
      integer(4), intent(in   ) :: noq2                ! Number of fluxes second direction
      integer(4), intent(in   ) :: noq                 ! Total number fluxes in the water phase
      integer(4), intent(in   ) :: ipoint(4,noq)       ! from, to, from-1, to+1 volume numbers per flux
      integer(4), intent(in   ) :: nodisp              ! number of additional dispersion arrays
      integer(4), intent(in   ) :: novelo              ! number of additional velocity   arrays
      integer(4), intent(in   ) :: idpnt ( * )         ! dispersion array to be applied per substance
      integer(4), intent(in   ) :: ivpnt ( * )         ! velocity   array to be applied per substance
      real   (4), intent(in   ) :: area  ( noq )       ! crosssectional surface areas of the fluxes
      real   (4), intent(in   ) :: flow  ( noq )       ! fluxes
      real   (4), intent(in   ) :: aleng (2,noq)       ! from and to distances to the surface area
      real   (4), intent(in   ) :: disp  ( 3 )         ! default dispersions in the 3 directions
      real   (4), intent(in   ) :: disper(nodisp,noq)  ! additional dispersion arrays
      real   (4), intent(in   ) :: velo  (novelo,noq)  ! additional velocity arrays
      integer(4), intent(in   ) :: isys                ! substances number to be used for this matrix
      integer(4), intent(in   ) :: iopt                ! = 0 or 2 DISP at zero flow
                                                       ! = 1 or 3 no DISP at zero flow
                                                       ! = 0 or 1 DISP over boundary
                                                       ! = 2 or 3 no DISP over boundary
      integer(4), intent(in   ) :: ilflag              ! if 0 then only 3 length values
      integer(4), intent(in   ) :: nomat               ! dimension of off-diagonal matrix amat
      real   (8), intent(  out) :: amat  (nomat)       ! matrix with off-diagonal entries
      integer(4), intent(in   ) :: imat  (nomat)       ! pointers of the off-diagonals in amat
      integer(4), intent(in   ) :: idiag(0:noseg+nobnd) ! position of the diagonals in amat
      real   (8), intent(inout) :: diag  (noseg+nobnd) ! diagonal of the matrix
      real   (8), intent(inout) :: diagcc(noseg+nobnd) ! copy of (unscaled) diagonal of the matrix
      integer(4), intent(in   ) :: iscale              ! = 0 no row scaling of diagonal
                                                       ! = 1    row scaling of diagonal
      integer(4), intent(in   ) :: fmat  (  noq)       ! location from(iq) in matrix
      integer(4), intent(in   ) :: tmat  (  noq)       ! location to  (iq) in matrix
      integer(4), intent(in   ) :: iknmrk(noseg)       ! feature array

!     Local declarations

      logical    zerof     !  if true, then NO dispersion at zero flow
      logical    zerob     !  if true, then NO dispersion accross open boundaries
      logical    loword    !  if true, then apply lower order scheme at open boundaries
      logical    lscale    !  if true, then APPLY row scaling of the diagonal
      logical    length    !  if true, an array of lengthes is provided
      integer(4) iadd      !  extra offset for horizontal off-diagonals in the case of 3D
      integer(4) ifrom     !  from volume number
      integer(4) ito       !  to   volume number
      integer(4) ifr2      !  from row number
      integer(4) ito2      !  to   row number
      integer(4) ip        !  index in amat of the 'from' volume for this flux
      integer(4) jp        !  index in amat of the 'to'   volume for this flux
      real   (4) a         !  help variable for exchange surface area in m2
      real   (4) q         !  help variable for the flux in m3/s
      real   (4) e         !  help variable for diffusive flux in m3/s
      real   (4) dl        !  help variable for the diffusive multiplier area/leng in m
      integer(4) idp,ivp   !  help variables for idpnt(isys) and ivpnt(isys)
      real   (4) q1 , q2   !  help variables
      real   (4) qvel      !  help variable to dintinguish normal and additional vertical velocity
      real   (4) f1, f2    !  help variables for (weighed) central differences
      integer(4) iq , jq   !  loop counters

!     WAQ timers

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqg3", ithandl )

!     set the logicals for dispersion and scaling and other fixed items

      zerof  = btest( iopt, 0 )
      zerob  = btest( iopt, 1 )
      loword = btest( iopt, 2 )
      lscale = iscale .eq. 1
      length = ilflag .eq. 1
      idp    = idpnt(isys)
      ivp    = ivpnt(isys)

!        reset the entire matrix

      amat = 0.0D0

      do 10 iq = 1 , noq

!         pointer administration check for transport anyhow

         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)

         if ( ifrom .eq. 0 .or. ito .eq. 0 ) cycle
         if ( ifrom .gt. 0 ) then
            if ( .not. btest(iknmrk(ifrom),0) ) cycle   ! identified dry at start and end of timestep
         endif
         if ( ito   .gt. 0 ) then
            if ( .not. btest(iknmrk(ito  ),0) ) cycle
         endif

!         initialisations

         a    = area(iq)
         q    = flow(iq)
         if ( abs(q) .lt. 10.0e-25 .and. btest(iopt,0) ) cycle   ! thin dam option, no dispersion at zero flow
         if ( iq .le. noq1 ) then
            e  = disp(1)
            if ( length ) then
               if ( aleng(1,iq) + aleng(2,iq) .gt. 1.0e-25 ) then
                  dl = a / (aleng(1,iq) + aleng(2,iq))
               else
                  dl = 0.0
               endif
            else
               dl = a / aleng(1,1)         ! first element of the array
            endif
         else if ( iq .le. noq1+noq2 ) then
            e  = disp(2)
            if ( length ) then
               if ( aleng(1,iq) + aleng(2,iq) .gt. 1.0e-25 ) then
                  dl = a / (aleng(1,iq) + aleng(2,iq))
               else
                  dl = 0.0
               endif
            else
               dl = a / aleng(2,1)         ! second element of the array
            endif
         else
            e  = disp(3)
            if ( length ) then
               if ( aleng(1,iq) + aleng(2,iq) .gt. 1.0e-25 ) then
                  dl = a / (aleng(1,iq) + aleng(2,iq))
                  f1 = aleng(2,iq) / (aleng(1,iq) + aleng(2,iq))
                  f2 = aleng(1,iq) / (aleng(1,iq) + aleng(2,iq))
               else
                  dl = 0.0
                  f1 = 0.5
                  f2 = 0.5
               endif
            else
               dl = a / aleng(1,2)         ! third element of the array
               f1 = 0.5
               f2 = 0.5
            endif
         endif
         e  = e*dl

!             add additional dispersions and fluxes

         if ( idp .gt. 0 ) e = e + disper(idp,iq)*dl
         if ( ivp .gt. 0 ) then
            qvel =  velo(ivp,iq)*a
         else
            qvel =  0.0
         endif

!             Option zero disp over the boundaries (also for additonal dispersions)

         if ( zerob .and. ( ifrom .lt. 0 .or. ito .lt. 0 ) ) e = 0.0

         if ( iq .le. noq1+noq2 ) then

!   for the first two directions apply  first order Upwind

            q = q + qvel
            if ( q .gt. 0.0 ) then
              q1 =   q
              q2 = 0.0
            else
              q1 = 0.0
              q2 =   q
            endif

!   for the third direction apply central discretization

         else

!.. apply upwind at boundaries (these are vertical boundaries !) for loword option

            if ( loword .and. ( ifrom .lt. 0 .or. ito .lt. 0 ) ) then
               q = q + qvel
               if ( q .gt. 0.0 ) then
                 q1 =   q
                 q2 = 0.0
               else
                 q1 = 0.0
                 q2 =   q
               endif
            else
               q1 = q*f1
               q2 = q*f2
               if ( qvel .gt. 0.0 ) then
                 q1 = q1 + qvel
               else
                 q2 = q2 + qvel
               endif
            endif
         endif

!        fill the matrix

         if ( ifrom .gt. 0  ) then
            if ( .not. btest(iknmrk(ifrom),0) ) then
               if ( q + qvel .gt. 0.0 ) then
                  q1 =   q + qvel
                  q2 = 0.0
               else
                  q1 = 0.0
                  q2 =   q + qvel
               endif
            endif
            diag ( ifrom  ) = diag ( ifrom  ) + q1 + e
            amat (fmat(iq)) = amat (fmat(iq)) + q2 - e
         endif
         if ( ito   .gt. 0  ) then
            if ( .not. btest(iknmrk(ito  ),0) ) then
               if ( q + qvel .gt. 0.0 ) then
                  q1 =   q + qvel
                  q2 = 0.0
               else
                  q1 = 0.0
                  q2 =   q + qvel
               endif
            endif
            diag (  ito   ) = diag (  ito   ) - q2 + e
            amat (tmat(iq)) = amat (tmat(iq)) - q1 - e
         endif

!        end of the loop over exchanges

   10 continue

!     finally scale the matrix to avoid possible round-off errors in GMRES
!     this scaling may need some adaption for future domain decomposition b.c.

      if ( lscale ) then
         do iq = 1, noseg + nobnd
            if (iq .gt. 1) then
               ifrom = idiag(iq-1) + 1
            else
               ifrom = 1
            endif
            ito = idiag(iq)

!      check on zero's required for methods 17 and 18

            if ( abs(diag(iq)) .lt. 1.0d-100) diag(iq) = 1.0

            do jq = ifrom, ito
               amat(jq) = amat(jq) / diag(iq)
            enddo

!           copy of diag for later scaling purposes in DLWQF4

            diagcc(iq) = diag (iq)
            diag  (iq) = 1.0d00
         enddo
      else
         do iq = 1, noseg + nobnd
            diagcc(iq) = 1.0d00
         enddo
      endif

      if ( timon ) call timstop ( ithandl )

      return
      end

end module m_dlwqg3
