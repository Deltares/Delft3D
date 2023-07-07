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
!!  stichting deltares
!!  p.o. box 177
!!  2600 mh delft, the netherlands
!!
!!  all indications and logos of, and references to registered trademarks
!!  of stichting deltares remain the property of stichting deltares. all
!!  rights reserved.

      subroutine wq_processes_integrate_fluxes ( conc   , amass  , deriv  , volume , dts    , &
                                                 nosys  , notot  , noseg  , surfac )

!     Deltares Software Centre

!>\File
!>           Sets an explicit time step from DERIV.

      use timers

      implicit none

!     Parameters          :
!     type     kind  function         name                      description

      integer   (4), intent(in   ) :: nosys                   !< number of transported substances
      integer   (4), intent(in   ) :: notot                   !< total number of substances
      integer   (4), intent(in   ) :: noseg                   !< number of computational volumes
      real      (4), intent(inout) :: conc  (notot ,noseg)    !< concentrations per substance per volume
      real      (8), intent(inout) :: amass (notot ,noseg)    !< masses per substance per volume
      real      (8), intent(inout) :: deriv (noseg, notot)    !< derivatives per substance per volume
      real      (8), intent(inout) :: volume(noseg )          !< volumes of the segments
      real      (8), intent(in   ) :: dts                     !< integration time step size
      real      (4), intent(in   ) :: surfac(noseg)           !< horizontal surface

      ! local declarations

      integer                      :: iseg                    !  segment loop counter
      integer                      :: i                       !  substance loop counter
      real(8)                      :: v1                      !  segment volume
      real(8)                      :: s1                      !  segment surface
      real(8)                      :: a                       !  segment mass

      integer(4), save :: ithndl = 0
      if (timon) call timstrt( "wq_processes_integrate_fluxes", ithndl )

      ! loop accross the number of computational elements

      do iseg=1,noseg
         ! active substances first
         v1 = volume(iseg)
         if ( v1.gt.1.0d-25 ) then
            do i=1,nosys
               a           = amass(i,iseg) + dts*deriv(iseg,i)*v1
               amass(i,iseg) = a
               conc (i,iseg) = a / v1
               deriv(iseg,i) = 0.0
            enddo
         endif
         ! then the inactive substances
         s1 = surfac(iseg)
         if(s1.gt.0.0d0) then
            do i=nosys+1,notot
               a             = amass(i,iseg) + dts*deriv(iseg,i)*v1
               amass(i,iseg) = a
               conc (i,iseg) = a / s1
               deriv(iseg,i) = 0.0
            enddo
         endif
      enddo
      if (timon) call timstop( ithndl )
      return
      end
