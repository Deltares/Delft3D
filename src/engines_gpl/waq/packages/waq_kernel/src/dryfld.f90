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

      module dryfld_mod
       use m_zoek
       use m_dhkmst

          implicit none
          real(4), dimension(:), allocatable, save :: sumvol
      end module dryfld_mod

      subroutine dryfld ( nosegw , noseg  , nolay  , volume , noq12  ,                &
     &                    area   , nocons , coname , cons   , surface,                &
     &                    iknmrk , iknmkv )

!     Deltares Software Centre

!>\File
!>      Sets feature of dry cells to zero
!>
!>      Determines which cells were dry at start of time step.
!>      This is an explicit setting of the feature in the
!>      sense that it gives the state of the start of the time step.\n
!>      The DRY_THRESH variable is used as a thickness (default 1.0 mm) together with
!>      the SURF parameter of segment function. If SURF is absent, 1.0 m2 is
!>      assumed and the DRY_THRESH directly compares vomes in m3.\n
!>      A dry cell may have transport, because it may be wet at the
!>      end of the time step. It has however no processes yet. The wetting within the
!>      time step is tested by the dryfle routine later in this file.

!     Created             : September 2010 by Leo Postma

!     Files               : none

!     Routines            : zoek20  - to search the DRY_THRESH constant
!                           dhkmst  - to set features

      use timers
      use dryfld_mod

      implicit none

!     Parameters          :

!     kind           function         name                   description

      integer  ( 4), intent(in   ) :: nosegw               !< number of computational volumes water
      integer  ( 4), intent(in   ) :: noseg                !< number of computational volumes total
      integer  ( 4), intent(in   ) :: nolay                !< number of layers
      real     ( 4), intent(inout) :: volume (noseg)       !< volumes at start of time step
      integer  ( 4), intent(in   ) :: noq12                !< number of horizontal exchanges
      real     ( 4), intent(inout) :: area   (noq12)       !< areas at start of time step
      integer  ( 4), intent(in   ) :: nocons               !< number of constants
      character(20), intent(in   ) :: coname (nocons)      !< names of the constants
      real     ( 4), intent(in   ) :: cons   (nocons)      !< values of the constants
      real     ( 4), intent(in   ) :: surface(noseg)       !< horizontal surface area
      integer  ( 4), intent(in   ) :: iknmrk (noseg)       !< constant feature array
      integer  ( 4), intent(  out) :: iknmkv (noseg)       !< time varying feature array

!     Local declarations

      integer  ( 4)    idryfld         ! help variable to find dry_tresh constant
      real     ( 4)    threshold       ! drying and flooding value
      real     ( 4)    minvolume       ! minimum volume in a cell
      real     ( 4)    minarea         ! minimum exhange area of a horizontal exchange
      integer  ( 4)    nosegl          ! number of computational volumes per layer
      integer  ( 4)    isegl           ! loop variable volumes
      integer  ( 4)    ivol            ! index for this computational volumes
      integer  ( 4)    ilay            ! loop variable layers
      integer  ( 4)    ikm             ! feature
      real     ( 4)    sum             ! help variable

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dryfld", ithandl )

!        Initialisations

      nosegl = nosegw / nolay

      !
      ! Allocate the work array - reallocate if
      ! for some reason the model size has changed
      !
      if ( .not. allocated(sumvol) ) then
          allocate( sumvol(nosegl) )
      endif
      if ( size(sumvol) /= nosegl ) then
          deallocate( sumvol )
          allocate( sumvol(nosegl) )
      endif

      threshold = 0.001                                        ! default value of 1 mm
      call zoek20 ( 'DRY_THRESH', nocons, coname, 10, idryfld )
      if ( idryfld .gt. 0 ) threshold = cons(idryfld)          ! or the given value

      minvolume = 0.001                                        ! default value of 0.001 m3 = 1 L
      call zoek20 ( 'MIN_VOLUME', nocons, coname, 10, idryfld )
      if ( idryfld .gt. 0 ) minvolume = cons(idryfld)          ! or the given value

      ivol   = 0
      sumvol = 0.0
      do ilay = 1, nolay
         ! Use OpenMP? ikm, ivol private
         ! Is ikm important?
         !$omp parallel do private(ikm,ivol)
         do isegl = 1, nosegl
            ivol          = isegl + (ilay-1) * nosegl
            sumvol(isegl) = sumvol(isegl) + volume(ivol)
         enddo
      enddo

      ivol   = 0
      do ilay = 1, nolay
         ! Use OpenMP? ivol private
         !$omp parallel do private(ivol)
         do isegl = 1, nosegl
            ivol = isegl + (ilay-1) * nosegl
            if ( sumvol(isegl) .lt. surface(isegl)*threshold ) then
               call dhkmst(1, iknmkv(ivol), 0 )               ! zero the last bit
               call dhkmst(2, iknmkv(ivol), 0 )               ! and the second feature
               volume(ivol) = max( volume(ivol), minvolume )
            else
               iknmkv(ivol) = iknmrk(ivol)                    ! become wet again
               volume(ivol) = max( volume(ivol), minvolume )
            endif
         enddo
      enddo

      minarea = 1.00E-04                                      ! default value of 1.00E-04 m2 = 1 cm2
      call zoek20 ( 'MIN_AREA', nocons, coname, 8, idryfld )
      if ( idryfld .gt. 0 ) minarea = cons(idryfld)           ! or the given value
      area = max( area, minarea )                             ! set minimum area

      if ( timon ) call timstop ( ithandl )

      return
      end

      subroutine dryfle ( nosegw , noseg  , volume , nolay  , nocons ,                &
     &                    coname , cons   , surface, iknmrk , iknmkv )

!     Deltares Software Centre

!>\File
!>               Wettens cells that became wet during the time step
!>
!>               Determines which cells have become wet during the time step.
!>               A dry cell may have transport, because it may be wet at the
!>               end of the time step. It has however no processes yet in this step.\n
!>               NB. This routine does NOT set cells dry, it only wettens any dry cells.

!     Created             : September 2010 by Leo Postma
!     Modified            : August    2011 by Leo Postma: test the volume of the water column
!                           April     2013    Leo Postma  integrated with hsurf routine
!                           April     2014    Michelle Jeuken introduction of constant for
!                                             minimum value of volumes.

!     Files               : none

!     Routines            : none

      use timers
      use dryfld_mod

      implicit none

!     Parameters          :

!     kind           function         name                   description

      integer  ( 4), intent(in   ) :: nosegw               !< number of computational volumes water
      integer  ( 4), intent(in   ) :: noseg                !< number of computational volumes
      real     ( 4), intent(inout) :: volume (noseg)       !< volumes at end of time step
      integer  ( 4), intent(in   ) :: nolay                !< number of layers
      integer  ( 4), intent(in   ) :: nocons               !< number of constants
      character(20), intent(in   ) :: coname (nocons)      !< names of the constants
      real     ( 4), intent(in   ) :: cons   (nocons)      !< values of the constants
      real     ( 4), intent(in   ) :: surface(noseg)       !< horizontal surface area
      integer  ( 4), intent(in   ) :: iknmrk (noseg)       !< constant feature array
      integer  ( 4), intent(inout) :: iknmkv (noseg)       !< time varying feature array

      integer  ( 4)    idryfld         ! help variable to find dry_tresh constant
      real     ( 4)    threshold       ! drying and flooding value
      real     ( 4)    minvolume       ! minimum volume in a cell
      integer  ( 4)    nosegl          ! number of computational volumes per layer
      integer  ( 4)    isegl           ! loop variable
      integer  ( 4)    ivol            ! this computational volume
      integer  ( 4)    ilay            ! loop variable layers
      integer  ( 4)    ikm             ! feature
      real     ( 4)    sum             ! help variable

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dryfle", ithandl )

      nosegl = nosegw / nolay

      !
      ! Allocate the work array - reallocate if
      ! for some reason the model size has changed
      !
      if ( .not. allocated(sumvol) ) then
          allocate( sumvol(nosegl) )
      endif
      if ( size(sumvol) /= nosegl ) then
          deallocate( sumvol )
          allocate( sumvol(nosegl) )
      endif

      threshold = 0.001                                         ! default value of 1 mm
      call zoek20 ( 'DRY_THRESH', nocons, coname, 10, idryfld )
      if ( idryfld .gt. 0 ) threshold = cons(idryfld)           ! or the given value

      minvolume = 0.001                                        ! default value of 0.001 m3 = 1 L
      call zoek20 ( 'MIN_VOLUME', nocons, coname, 10, idryfld )
      if ( idryfld .gt. 0 ) minvolume = cons(idryfld)          ! or the given value

      sumvol = 0.0
      do ilay = 1, nolay
         ! Use OpenMP? ikm, ivol private
         !$omp parallel do private(ikm,ivol)
         do isegl = 1, nosegl
            ivol = isegl + (ilay-1)*nosegl
            sumvol(isegl) = sumvol(isegl) + volume(ivol)
         enddo
      enddo

      do ilay = 1, nolay
         ! Use OpenMP? ikm, ivol private
         !$omp parallel do private(ivol)
         do isegl = 1, nosegl
            ivol = isegl + (ilay-1)*nosegl
            if ( sumvol(isegl) .gt. surface(isegl)*threshold ) then
               iknmkv(ivol) = iknmrk(ivol)
            endif
            volume(ivol) = max( volume(ivol), minvolume )
         enddo
      enddo

      if ( timon ) call timstop ( ithandl )

      return
      end
