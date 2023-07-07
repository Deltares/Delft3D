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

      subroutine setdvp ( nodisp, idpnt , ndspn , idpnw , nosys ,
     +                    ndspx , dsto  )
!
!     function            : sets new dispersion (or velocity) pointers
!
!     created:            : december 1994 by jan van beek
!
!     modified            : october  2010, jvb,  construct only unique new dispersion

      use timers       !   performance timers

      implicit none

      ! declaration of arguments

      integer, intent(in)       :: nodisp             ! number of dispersions from input
      integer, intent(in)       :: idpnt(nosys)       ! pointers to dispersion array
      integer, intent(inout)    :: ndspn              ! number of new dispersion array
      integer, intent(inout)    :: idpnw(nosys)       ! pointers to dispersion array
      integer, intent(in)       :: nosys              ! number of active substances
      integer, intent(in)       :: ndspx              ! number of dispersions from the processes
      real   , intent(in)       :: dsto(nosys,ndspx)  ! dispersion stochi factors

      ! local declarations

      real, allocatable         :: dsto_new(:,:)      ! stochi factors for the new dispersion array, to check if it is unique
      integer                   :: isys               ! index substances
      logical                   :: found              ! true if a matching new dispersion is found
      logical                   :: dsto_equal         ! true if the stochi factors of a new dispersion match
      integer                   :: i_dspn             ! index new dispersion
      integer                   :: idisp              ! index dispersion
      integer                   :: idspx              ! index dispersion from processes
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "setdvp", ithndl )

      ! only action if there are already new dispersions, we will reset the number of new dispersions ndspn

      if ( ndspn .gt. 0 ) then

         ndspn     = 0
         allocate(dsto_new(nosys,nodisp+ndspx))
         dsto_new = 0.0

         do isys = 1, nosys

            ! only if a dispersion acts on this substance

            if ( idpnt(isys) .gt. 0 .or. idpnw(isys) .gt. 0 ) then

               ! determine if there is already a new dispersion with equal (1e-20) stochi factors

               found = .false.
               do i_dspn = 1, ndspn
                  dsto_equal = .true.

                  ! the dispersion arrays from the input stochi always 0.0 (not used)  or 1.0 (used)

                  do idisp = 1, nodisp
                     if ( idpnt(isys) .eq. idisp ) then

                        ! stochi on dispersion array always 1.0

                        if ( abs(dsto_new(i_dspn,idisp)-1.0) .gt. 1.e-20 ) then
                           dsto_equal = .false.
                        endif
                     else
                        if ( abs(dsto_new(i_dspn,idisp)) .gt. 1.e-20 ) then
                           dsto_equal = .false.
                        endif
                     endif
                  enddo

                  do idspx = 1, ndspx
                     if ( abs(dsto(isys,idspx)-dsto_new(i_dspn,nodisp+idspx)) .gt. 1.e-20 ) then
                        dsto_equal = .false.
                     endif
                  enddo

                  if ( dsto_equal ) then
                     found = .true.
                     idpnw(isys) = i_dspn
                     exit
                  endif

               enddo

               ! if not found add a new dispersion

               if ( .not. found ) then

                  ndspn       = ndspn + 1
                  idpnw(isys) = ndspn

                  ! set stochi factors

                  idisp = idpnt(isys)
                  if ( idisp .gt. 0 ) then
                     dsto_new(ndspn,idisp) = 1.0
                  endif

                  do idspx = 1, ndspx
                     dsto_new(ndspn,nodisp+idspx) = dsto(isys,idspx)
                  enddo

               endif

            endif

         enddo

      endif

      if (timon) call timstop( ithndl )
      return
      end
