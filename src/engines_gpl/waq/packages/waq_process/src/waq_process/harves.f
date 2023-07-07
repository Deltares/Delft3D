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

      subroutine harves     ( pmsa   , fl     , ipoint , increm, noseg ,
     +                        noflux , iexpnt , iknmrk , noq1  , noq2  ,
     +                        noq3   , noq4   )
      use m_srstop
      use m_monsys
      use m_dhnoseg
      use m_dhnolay


      implicit none

      ! arguments

      real(4) pmsa(*)     !i/o process manager system array, window of routine to process library
      real(4) fl(*)       ! o  array of fluxes made by this process in mass/volume/time
      integer ipoint( 10) ! i  array of pointers in pmsa to get and store the data
      integer increm( 10) ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! i  number of computational elements in the whole model schematisation
      integer noflux      ! i  number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
      integer noq1        ! i  nr of exchanges in 1st direction, only horizontal dir if irregular mesh
      integer noq2        ! i  nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)

      ! variables from pmsa array

      real(4) cgrazer     ! i  calculated concentration of grazer                 (gC/m3)
      real(4) zharvegr    ! i  harvest flux of grazer                             (gC/m2/d)
      real(4) pharvegr    ! i  fraction harvest of grazer  per timestep           (-)
      real(4) tgrazer     ! i  threshold concentration of grazer                  (gC/m3)
      integer grunitsw    ! i  use gC/m3 (0) or gC/m2 (1) for grazer              (-)
      real(4) delt        ! i  timestep for processes                             (d)
      real(4) volume      ! i  volume of computational cell                       (m3)
      real(4) depth       ! i  depth of segment                                   (m)
      real(4) aharvegr    ! o  actual harvest flux grazer                         (gC/m2/d)
      real(4) bharvegr    ! o  cumulated harvest flux grazer                      (gC)

      ! local variables

      integer, parameter :: max_message = 1000 ! maximum messages on insufficient biomass attention this is for all instnces of this process
      integer, save      :: nr_message  = 0    ! actual number of messages on insufficient biomass attention this is for all instnces of this process
      integer            :: lunrep             ! unit number report file
      integer            :: nosegw             ! number of segment in the water
      integer            :: nolay              ! number of layers
      integer            :: nosegl             ! number of segment per layer
      integer            :: ikol               ! column number = segment number top layer
      integer            :: ilay               ! layer number
      integer            :: iseg               ! segment number
      real               :: tot_cgrazer        ! total grazer over the column     (gC/m2)
      real               :: tot_depth          ! column depth                     (m)
      real               :: harvest            ! harvest                          (gC/m2)
      real               :: fharvest           ! fraction harvest of total        (-)

      ! initialisation

      call getmlu(lunrep)
      call dhnoseg(nosegw)
      call dhnolay(nolay)
      nosegl = nosegw/nolay
      if ( nosegl*nolay .ne. nosegw ) then
         write(lunrep,*) ' ERROR: unstructured 3d application'
         write(lunrep,*) ' harvesting module not possible'
         call srstop(1)
      endif

      ! check if unit switch global, consbl allows it to be variable

      if ( increm(5) .ne. 0 ) then
         write(lunrep,*) ' ERROR: unit switch not a constant'
         write(lunrep,*) ' harvesting module not possible'
         call srstop(1)
      endif

      grunitsw   = nint(pmsa(ipoint(5)))
      delt       = pmsa(ipoint(6))

      ! loop over de kolommen

      do ikol = 1 , nosegl

         ! harvest is opgegeven per kolom neem de waarde van de top laag

         zharvegr   = pmsa(ipoint(2)+(ikol-1)*increm(2))
         pharvegr   = pmsa(ipoint(3)+(ikol-1)*increm(3))
         tgrazer    = pmsa(ipoint(4)+(ikol-1)*increm(4))

         ! doe alleen als harvest groter is dan 0

         if ( zharvegr .gt. 1e-20 .or. pharvegr .gt. 1e-20 ) then

            ! sommeer grazer per laag altijd in gC/m2

            tot_cgrazer = 0.0
            tot_depth   = 0.0

            do ilay = 1 , nolay

               iseg = ikol+(ilay-1)*nosegl
               cgrazer = pmsa(ipoint(1)+(iseg-1)*increm(1))
               depth   = pmsa(ipoint(8)+(iseg-1)*increm(8))

               if ( grunitsw .eq. 1 ) then
                  tot_cgrazer = tot_cgrazer + cgrazer
               else
                  tot_cgrazer = tot_cgrazer + cgrazer*depth
               endif
              tot_depth = tot_depth + depth

            enddo

            ! bereken de harvest in gC/m2

            harvest = zharvegr*delt + pharvegr*tot_cgrazer

            ! maximeer harvest tot de threshold (tgrazer )

            if ( grunitsw .eq. 0 ) then
               tgrazer = tgrazer*tot_depth
            endif
            if ( harvest .gt. tot_cgrazer-tgrazer  ) then
               if ( nr_message .lt. max_message ) then
                  nr_message = nr_message + 1
                  write(lunrep,*) ' WARNING: biomass not sufficient to support harvest rate'
                  write(lunrep,*) ' segment  : ',ikol
                  write(lunrep,*) ' harvest  : ',harvest     ,'gC/m2'
                  write(lunrep,*) ' biomass  : ',tot_cgrazer ,'gC/m2'
                  write(lunrep,*) ' threshold: ',tgrazer     ,'gC/m2'

                  if ( nr_message .eq. max_message ) then
                     write(lunrep,*) 'maximum message on harvesting reached, further messages suppressed'
                  endif
               endif
               harvest = max(0.0,tot_cgrazer-tgrazer)
            endif

            ! bepaal de harvest als fractie van het totaal

            if ( tot_cgrazer .gt. 1e-20 ) then
               fharvest = harvest/tot_cgrazer
            else
               fharvest = 0.0
            endif

            ! de actual harvest is per kolom in gC/m2/d

            aharvegr = harvest/delt

            ! bepaal nieuwe concentratie cgrazer en de geaccumuleerde harvest bharvegr
            ! zet uitvoer in pmsa, let op cgrazer en bharvegr zijn zowel input and output

            do ilay = 1 , nolay
               iseg = ikol+(ilay-1)*nosegl
               cgrazer = pmsa(ipoint(1)+(iseg-1)*increm(1))
               volume  = pmsa(ipoint(7)+(iseg-1)*increm(7))
               bharvegr= pmsa(ipoint(10)+(iseg-1)*increm(10))

               if ( grunitsw .eq. 1 ) then
                  bharvegr = bharvegr + cgrazer*fharvest*volume/depth
               else
                  bharvegr = bharvegr + cgrazer*fharvest*volume
               endif
               cgrazer = cgrazer*(1.0-fharvest)

               pmsa(ipoint(1)+(iseg-1)*increm(1)) = cgrazer
               pmsa(ipoint(9)+(iseg-1)*increm(9)) = aharvegr
               pmsa(ipoint(10)+(iseg-1)*increm(10)) = bharvegr
            enddo

         endif

      enddo

      return
      end
