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
      module m_dlback

      implicit none

      contains


      subroutine dlback (  dt     , nmax   , mmax   , kmax   , lgrida ,
     &                     noseg  , nosys  , notot  , gsqs   , r1     ,
     &                     dvol0  , dvol1  , conc   , amass  , deriv  )

!       Deltares Software Centre

!>\file
!>        Conversion of FLOW concentration array to DELWAQ concentration array
!>
!>        This routine
!>        - fills in the r1 FLOW array into the conc Delwaq array
!>        - sets a time step from deriv for the non transported substances
!>        - updates conc of non-transported substances in mass/m2 using g2qs
!>          from the FLOW routine
!>        - copies the new Delwaq volume in the old Delwaq volume location

!     Created             : August  1996 by Erik de Goede

!     Modified            : January 2011 by Leo Postma    conc of passive substances /m2
!                           August  2011 by Leo Postma    support of active only grid

      use timers

      implicit none

!     Arguments           :

!     type     kind  function         name                                description

      real      (4), intent(in   ) :: dt                                !< time step size in seconds
      integer   (4), intent(in   ) :: nmax                              !< first dimension in the FLOW grid
      integer   (4), intent(in   ) :: mmax                              !< second dimension in the FLOW grid
      integer   (4), intent(in   ) :: kmax                              !< third dimension in the FLOW grid
      integer   (4), intent(in   ) :: lgrida(nmax,mmax)                 !< matrix with active grid layout
      integer   (4), intent(in   ) :: noseg                             !< number of computational volumes
      integer   (4), intent(in   ) :: nosys                             !< number of transported substances
      integer   (4), intent(in   ) :: notot                             !< total number of substances
      real      (4), intent(in   ) :: gsqs  (nmax,-1:mmax+2)            !< Horizontal surface area at FLOW grid
      real      (4), intent(in   ) :: r1    (nmax,-1:mmax+2,kmax,nosys) !< FLOW result
      real      (4), intent(  out) :: dvol0 (noseg)                     !< volume at old time level
      real      (4), intent(in   ) :: dvol1 (noseg)                     !< volume at new time level
      real      (4), intent(inout) :: conc  (notot,noseg)               !< concentrations per substance per volume
      real      (4), intent(inout) :: amass (notot,noseg)               !< masses per substance per volume
      real      (4), intent(inout) :: deriv (notot,noseg)               !< derivatives per substance per volume

!     Locals

      integer   (4)     nosegl       !  number of Delwaq volumes per layer
      integer   (4)     m, n, k, l   !  loop variables in the 3 directions and over substances
      integer   (4)     j, k0        !  help variables in the grid


      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlback", ithandl )

      nosegl = noseg/kmax

!         loop accross the computational volumes

      do k = 1 , kmax
         do m = 1 , mmax
            do n = 1 , nmax
               j = lgrida( n, m )
               if ( j .le. 0 ) cycle
               k0 = j + (k-1)*nosegl
               do l = 1 , nosys                  !         transported substances first
                  conc (l,k0) = r1(n,m,k,l)
                  amass(l,k0) = conc(l,k0) * dvol1(k0)
               enddo
               do l = nosys+1 , notot            !         then the passive substances
                  amass(l,k0) = amass(l,k0) + dt*deriv(l,k0)
                  conc (l,k0) = amass(l,k0) / gsqs(n,m)
               enddo
            enddo
         enddo
      enddo

!         update the volumes and set derivative to zero

      dvol0 = dvol1
      deriv = 0.0

      if ( timon ) call timstop ( ithandl )

      return
      end

      end module m_dlback
