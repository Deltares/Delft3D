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
      module m_oil2waq

      implicit none

      contains


      subroutine oil2waq ( nopart  , nosys   , notot   , nosubs  , noseg   ,
     &                     nolay   , volume  , surface , nmax    , mmax    ,
     &                     lgrida  , syname  , itime   , iddtim  , npwndw  ,
     &                     iptime  , npart   , mpart   , kpart   , wpart   ,
     &                     amass   , conc    , iaflag  , intopt  , ndmps   ,
     &                     isdmp   , dmps    , amass2  )

!     Deltares Software Centre

!>\File
!>      Migrates particles from delpar to delwaq if their resedence time exceeds the take over time
!>
!>      At first call it is determined which part substance belongs to which waq substance.
!>      That is done by looking for a waq substance with the same name as the name of the
!>      part substance minus its last letter. It is advised to give the part substance the
!>      name of the corresponding waq substance plus the letter 'p'.\n
!>      If the corresponding waq substance is not transported, the mass contribution of the particle
!>      is divided by the horizontal surface area for the concentration per m^2, otherwise it is
!>      divided by the volume for the concentration per m^3.\n
!>      After the take over by Delwaq the location of the particle is set in the upper left corner
!>      of the grid, its weight is set to zero and the particle window counter is increased to the
!>      level of particles that are still to young to be migrated.

!     Created             : April     2013 by Leo Postma
!     Adapted             : May 2013 Frank Kleissen - adapted from par2waq.f: specifc for the oil module
!                           to transfer dispersed oil, no take over time
!     Files               : none

!     Routines            : zoek20  - to search the delwaq names

      use m_zoek
      use timers
!
      implicit none
      integer, parameter :: ip=4, rp=4

!     Parameters          :

!     kind           function         name                      description

      integer  (ip), intent(in   ) :: nopart                  !< total number of particles
      integer  (ip), intent(in   ) :: nosys                   !< transported substances in delwaq
      integer  (ip), intent(in   ) :: notot                   !< total substances in delwaq
      integer  (ip), intent(in   ) :: nosubs                  !< total substances in delpar
      integer  (ip), intent(in   ) :: noseg                   !< total number of gridcells in delwaq
      integer  (ip), intent(in   ) :: nolay                   !< number of layers in delwaq
      real     (rp), intent(in   ) :: volume (noseg)          !< delwaq volumes
      real     (rp), intent(in   ) :: surface(noseg)          !< delwaq horizontal surfaces
      integer  (ip), intent(in   ) :: nmax                    !< first grid dimension
      integer  (ip), intent(in   ) :: mmax                    !< second grid dimension
      integer  (ip), intent(in   ) :: lgrida (nmax,mmax)      !< active computational grid
      character(20), intent(in   ) :: syname (notot)          !< names of the substances
      integer  (ip), intent(in   ) :: itime                   !< current time
      integer  (ip), intent(in   ) :: iddtim                  !< delwaq take-over delay time
      integer  (ip), intent(inout) :: npwndw                  !< first active particle in array
      integer  (ip), intent(inout) :: iptime (nopart)         !< age of the particles
      integer  (ip), intent(inout) :: npart  (nopart)         !< first grid index particles
      integer  (ip), intent(inout) :: mpart  (nopart)         !< second grid index particles
      integer  (ip), intent(inout) :: kpart  (nopart)         !< third grid index particles
      real     (rp), intent(inout) :: wpart  (nosubs,nopart ) !< weight of the particles
      real     (rp), intent(inout) :: amass  (notot ,noseg  ) !< delwaq masses per cell
      real     (rp), intent(inout) :: conc   (notot ,noseg  ) !< delwaq concentrations per cell
      integer   (4), intent(in   ) :: iaflag                  !< if 1 then accumulation of balances
      integer   (4), intent(in   ) :: intopt                  !< integration suboptions
      integer   (4), intent(in   ) :: ndmps                   !< number of dumped volumes for balances
      integer   (4), intent(in   ) :: isdmp  (noseg )         !< volume to dump-location pointer
      real      (4), intent(inout) :: dmps   (notot ,ndmps,*) !< dumped segment fluxes if INTOPT > 7
      real      (4), intent(inout) :: amass2 (notot , 5 )     !< mass balance array

!     Local declarations

      integer, allocatable, save :: iwaqsub(:)        ! pointer from part substance to waq substance
      character(20)                 partsub           ! this particle substance
      integer                       isub, ipart       ! loop variables
      integer                       ic  , iseg , ilay ! help variable for segment location
      integer                       ioff              ! help variable start of delpar substances in delwaq
      integer                       nosegl            ! number of cells per layer
      logical                       fluxes            ! set .true. if intopt > 7
      logical                       massbal           ! set .true. if iaflag eq 1
      integer                       ipb, isys         ! help variables

      integer(4) ithandl /0/

      if ( timon ) call timstrt ( "oil2waq", ithandl )

      massbal = iaflag .eq. 1
      fluxes  = btest(intopt,3)

      if ( .not. allocated( iwaqsub ) ) then
         allocate ( iwaqsub(nosubs) )
         ioff = notot - nosubs
         do isub = 1, nosubs
            partsub = syname( ioff+isub ) ( 1 : len_trim(syname(ioff+isub))-1 ) ! cut the 'p' off
            call zoek20 ( partsub, ioff, syname, 20, iwaqsub(isub) )
            if ( iwaqsub(isub) .lt. 0 ) iwaqsub(isub) = 0 ! not found!
            if ( iwaqsub(isub) .gt. nosys ) iwaqsub(isub) = -iwaqsub(isub)      ! not dissolved
         enddo
      endif
         nosegl = noseg / nolay

      do ipart = npwndw, nopart
         ic = lgrida( npart(ipart), mpart(ipart) )
         if ( ic .gt.  0 ) then
            ilay = kpart(ipart)
            iseg = (ilay-1)*nosegl + ic
            ipb  = isdmp(iseg)
            do isub = 1, nosubs
               isys = iwaqsub(isub)
               if ( isys .eq. 0 ) cycle
                if (isub.eq.2.and.wpart(isub,ipart).gt.0) then
                  amass( isys,iseg) = amass( isys,iseg) + wpart(isub,ipart)
                  conc ( isys,iseg) = amass( isys,iseg) / volume (iseg)
                  if ( massbal ) amass2(isys,    3) = amass2(isys,    3) + wpart(isub,ipart)
                  if ( ipb .gt. 0 .and. fluxes )
     &                        dmps  (isys,ipb,2) = dmps  (isys,ipb,2) + wpart(isub,ipart)
                  npart (ipart) = 1
                  mpart (ipart) = 1
                  kpart (ipart) = 1
                  iptime(ipart) = 0
                endif
            enddo
         endif
      enddo

      if ( timon ) call timstop ( ithandl )

      return
      end

      end module m_oil2waq
