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
      module m_dlwqb9

      implicit none

      contains


      subroutine dlwqb9 ( disp   , disper , area   , flow   , aleng  ,
     &                    velo   , bound  , ipoint , nosys  , isys   ,
     &                    nsys   , noq1   , noq2   , noq    , nodisp ,
     &                    novelo , idpnt  , ivpnt  , deriv  , amat   ,
     &                                      jtrack , iopt   , ilflag )

!     Deltares - Delft Software Centre

!     Created   : June 1988 by Leo Postma
!     Modified  : June 2010 by Leo Postma more modern look and feel

!     Function            : Fills band matrix according to backward
!                                             differencing in space.

!     File IO             : none

!     Subroutines called  : none

      use timers                         ! WAQ performance timers

      implicit none

!     Arguments           :

!     Kind        Function         Name                  Description

      integer(4), intent(in   ) :: nosys               ! Number of transported substances
      integer(4), intent(in   ) :: isys                ! Start number of substances
      integer(4), intent(in   ) :: nsys                ! Number of substances with same matrix
      integer(4), intent(in   ) :: noq1                ! Number of fluxes first direction
      integer(4), intent(in   ) :: noq2                ! Number of fluxes second direction
      integer(4), intent(in   ) :: noq                 ! Total number fluxes in the water phase
      integer(4), intent(in   ) :: ipoint(4,noq)       ! from, to, from-1, to+1 volume numbers per flux
      integer(4), intent(in   ) :: nodisp              ! number of additional dispersion arrays
      integer(4), intent(in   ) :: novelo              ! number of additional velocity   arrays
      integer(4), intent(in   ) :: idpnt ( nosys )     ! dispersion array to be applied per substance
      integer(4), intent(in   ) :: ivpnt ( nosys )     ! velocity   array to be applied per substance
      real   (4), intent(in   ) :: area  ( noq )       ! crosssectional surface areas of the fluxes
      real   (4), intent(in   ) :: flow  ( noq )       ! fluxes
      real   (4), intent(in   ) :: aleng (2,noq)       ! from and to distances to the surface area
      real   (4), intent(in   ) :: disp  ( 3 )         ! default dispersions in the 3 directions
      real   (4), intent(in   ) :: disper(nodisp,noq)  ! additional dispersion arrays
      real   (4), intent(in   ) :: velo  (novelo,noq)  ! additional velocity arrays
      real   (4), intent(in   ) :: bound (nosys , * )  ! Values at the open boundaries
      real   (4), intent(inout) :: deriv ( nsys , * )  ! Right hand side of the equations
      integer(4), intent(in   ) :: jtrack              ! Number of codiagonals of amat
      integer(4), intent(in   ) :: iopt                ! = 0 or 2 DISP at zero flow
                                                       ! = 1 or 3 no DISP at zero flow
                                                       ! = 0 or 1 DISP over boundary
                                                       ! = 2 or 3 no DISP over boundary
      integer(4), intent(in   ) :: ilflag              ! If 0 then only 3 length values in the 3 direction
      real   (4), intent(  out) :: amat  (2*jtrack+1,*)! Matrix with transports

!     Local declarations

      logical    zerof     !  if true, then NO dispersion at zero flow
      logical    zerob     !  if true, then NO dispersion accross open boundaries
      logical    length    !  if true, an array of lengthes is provided
      integer(4) iq        !  loop counter over exchange surfaces
      integer(4) ifrom     !  from volume number
      integer(4) ito       !  to   volume number
      real   (4) a         !  help variable for exchange surface area in m2
      real   (4) q         !  help variable for the flux in m3/s
      real   (4) e         !  help variable for diffusive flux in m3/s
      real   (4) dl        !  help variable for the diffusive multiplier area/leng in m
      integer(4) idp,ivp   !  help variables for idpnt(isys) and ivpnt(isys)
      integer(4) idiag     !  help variable for the location of the diagonal in amat
      integer(4) i3, i4    !  help variables for the boundaries
      real   (4) q1 , q2   !  help variables for upwind flow schematisation

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq62", ithandl )

      zerof  = btest( iopt, 0 )
      zerob  = btest( iopt, 1 )
      length = ilflag .eq. 1
      idp    = idpnt(isys)
      ivp    = ivpnt(isys)
      idiag  = jtrack + 1

      do 50 iq = 1 , noq

!         initialisations , check for transport anyhow

         ifrom = ipoint(1,iq)
         ito   = ipoint(2,iq)
         if ( ifrom .eq. 0 .or. ito .eq. 0 ) cycle
         a     = area(iq)
         q     = flow(iq)
         if ( zerof .and. abs(q) .lt. 10.0e-25 ) goto 50
         if ( a .lt. 1.0e-25 )  a = 1.0
         if ( iq .le. noq1 ) then
            e  = disp(1)
            if ( length ) then
               dl = a / (aleng(1,iq) + aleng(2,iq))
            else
               dl = a / aleng(1,1)         ! first element of the array
            endif
         else if ( iq .le. noq1+noq2 ) then
            e  = disp(2)
            if ( length ) then
               dl = a / (aleng(1,iq) + aleng(2,iq))
            else
               dl = a / aleng(2,1)         ! second element of the array
            endif
         else
            e  = disp(3)
            if ( length ) then
               dl = a / (aleng(1,iq) + aleng(2,iq))
            else
               dl = a / aleng(1,2)         ! third element of the array
            endif
         endif
         e  = e*dl
         if ( idp .gt. 0 ) e = e + disper(idp,iq)*dl
         if ( ivp .gt. 0 ) q = q + velo  (ivp,iq)*a
         if ( q .gt. 0.0 ) then
              q1 =   q
              q2 = 0.0
         else
              q1 = 0.0
              q2 =   q
         endif
         if ( ifrom .lt. 0 ) goto 10
         if ( ito   .lt. 0 ) goto 30

!        the regular case

         e = min ( e, 1.0e+5*amat(idiag,ifrom), 1.0e+5*amat(idiag,ito) )        ! limitation of diffusion to
         amat(idiag          ,ifrom) = amat(idiag          ,ifrom) + q1 + e     ! avoid degradation of matrix
         amat(idiag+ito-ifrom,ifrom) = amat(idiag+ito-ifrom,ifrom) + q2 - e
         amat(idiag          ,ito  ) = amat(idiag          ,ito  ) - q2 + e
         amat(idiag+ifrom-ito,ito  ) = amat(idiag+ifrom-ito,ito  ) - q1 - e
         cycle

!        The 'from' volume is a boundary

   10    if ( ito  .lt. 0 ) cycle
         if ( zerob ) then
            e = 0.0
         else
            e = min ( e, 1.0e+5*amat(idiag,ito) )
         endif
         amat(idiag          ,ito  ) = amat(idiag          ,ito  ) - q2 + e
         do i3 = 1, nsys
            i4 = i3 + isys - 1
            deriv(i3,ito  ) = deriv(i3,ito  ) + (  q1 + e ) * bound(i4,-ifrom)
         enddo
         cycle

!        The 'to' element was a boundary.

   30    if ( zerob ) then
            e = 0.0
         else
            e = min ( e, 1.0e+5*amat(idiag,ifrom) )
         endif
         amat(idiag          ,ifrom) = amat(idiag          ,ifrom) + q1 + e
         do i3 = 1, nsys
            i4 = i3 + isys - 1
            deriv(i3,ifrom) = deriv(i3,ifrom) + ( -q2 + e ) * bound(i4,-ito  )
         enddo

!        end of the loop over exchanges

   50 continue

      if ( timon ) call timstop ( ithandl )
      return
      end

      end module m_dlwqb9
