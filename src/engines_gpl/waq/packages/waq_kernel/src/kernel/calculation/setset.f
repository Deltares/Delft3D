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
      module m_setset

      implicit none

      contains


      subroutine setset ( lurep  , nocons , nopa   , nofun  , nosfun ,
     &                    nosys  , notot  , nodisp , novelo , nodef  ,
     &                    noloc  , ndspx  , nvelx  , nlocx  , nflux  ,
     &                    nopred , novar  , nogrid , vgrset )

!     Deltares Software Centre

!>\File
!>      Initialisation of Variables structure
!>
!>      Meaning is not documented by author

!     Created:    unknown date by Jan van Beek

      use timers
      implicit none

!     Parameters          :

!     kind           function         name                    description

      integer  ( 4), intent(in   ) :: lurep                 !< Unit number monitoring file (not used)
      integer  ( 4), intent(in   ) :: nocons                !< Number of constants
      integer  ( 4), intent(in   ) :: nopa                  !< Number of parameters
      integer  ( 4), intent(in   ) :: nofun                 !< Number of functions
      integer  ( 4), intent(in   ) :: nosfun                !< Number of segment functions
      integer  ( 4), intent(in   ) :: nosys                 !< Number of transported substances
      integer  ( 4), intent(in   ) :: notot                 !< Total number of substances
      integer  ( 4), intent(in   ) :: nodisp                !< Number of user-dispersions
      integer  ( 4), intent(in   ) :: novelo                !< Number of user-flows
      integer  ( 4), intent(in   ) :: nodef                 !< Number of default values
      integer  ( 4), intent(in   ) :: noloc                 !< Number of local values
      integer  ( 4), intent(in   ) :: ndspx                 !< Number of dspx
      integer  ( 4), intent(in   ) :: nvelx                 !< Number of velx
      integer  ( 4), intent(in   ) :: nlocx                 !< Number of locx
      integer  ( 4), intent(in   ) :: nflux                 !< Number of flux
      integer  ( 4), intent(in   ) :: nopred                !< Not used
      integer  ( 4), intent(in   ) :: novar                 !< Number of variables on the grids
      integer  ( 4), intent(in   ) :: nogrid                !< Number of grids
      integer  ( 4), intent(inout) :: vgrset(novar,nogrid)  !< Number of grids

!     Local declarations

      integer( 4) i, ivar, igrid      ! help variables for loop and index counting
      integer( 4) iset                ! help variable 1 for igrid = 1, 0 for igrid > 1

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "setset", ithandl )

      do igrid = 1 , nogrid
         iset = 0
         if ( igrid .eq. 1 ) iset = 1
         ivar = 0
         ivar = ivar + 1  ;  vgrset(ivar,igrid) = iset    ! volume
         ivar = ivar + 1  ;  vgrset(ivar,igrid) = iset    ! area
         ivar = ivar + 1  ;  vgrset(ivar,igrid) = iset    ! flow
         ivar = ivar + 1  ;  vgrset(ivar,igrid) = iset    ! length 1
         ivar = ivar + 1  ;  vgrset(ivar,igrid) = iset    ! length 2
         ivar = ivar + nocons                             ! constants
         ivar = ivar + nopa                               ! parameters
         do i = 1 , nofun                                 ! functions
            ivar = ivar + 1  ;  vgrset(ivar,igrid) = iset
         enddo
         do i = 1 , nosfun                                ! segment functions
            ivar = ivar + 1  ;  vgrset(ivar,igrid) = iset
         enddo
         do i = 1 , notot                                 ! concentrations
            ivar = ivar + 1  ;  vgrset(ivar,igrid) = iset
         enddo
         do i = 1 , notot                                 ! masses
            ivar = ivar + 1  ;  vgrset(ivar,igrid) = iset
         enddo
         do i = 1 , notot                                 ! derivatives
            ivar = ivar + 1  ;  vgrset(ivar,igrid) = iset
         enddo
         do i = 1 , nodisp                                ! dispersions
            ivar = ivar + 1  ;  vgrset(ivar,igrid) = iset
         enddo
         do i = 1 , novelo                                ! velocities
            ivar = ivar + 1  ;  vgrset(ivar,igrid) = iset
         enddo
         do i = 1 , nodef                                 ! default values
            ivar = ivar + 1  ;  vgrset(ivar,igrid) = iset
         enddo
         do i = 1 , noloc                                 ! local values
            ivar = ivar + 1  ;  vgrset(ivar,igrid) = iset
         enddo
         ivar = ivar + ndspx                           ! dspx
         ivar = ivar + nvelx                           ! velx
         ivar = ivar + nlocx                           ! locx
         ivar = ivar + nflux                           ! flux
      enddo

      if ( timon ) call timstop ( ithandl )
      return
      end

      end module m_setset
