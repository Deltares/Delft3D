!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
      module m_harves
      use m_waq_precision


      implicit none

      contains


      subroutine harves     ( process_space_real   , fl     , ipoint , increm, num_cells , &
                             noflux , iexpnt , iknmrk , num_exchanges_u_dir  , num_exchanges_v_dir  , &
                             num_exchanges_z_dir   , num_exchanges_bottom_dir   )
      use m_logger_helper, only : stop_with_error, get_log_unit_number
      use m_dhnoseg
      use m_dhnolay


      implicit none

      ! arguments

      real(kind=real_wp) ::process_space_real(*)     !i/o process manager system array, window of routine to process library
      real(kind=real_wp) ::fl(*)       ! o  array of fluxes made by this process in mass/volume/time
      integer(kind=int_wp) ::ipoint( 10) ! i  array of pointers in process_space_real to get and store the data
      integer(kind=int_wp) ::increm( 10) ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer(kind=int_wp) ::num_cells       ! i  number of computational elements in the whole model schematisation
      integer(kind=int_wp) ::noflux      ! i  number of fluxes, increment in the fl array
      integer(kind=int_wp) ::iexpnt(4,*) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
      integer(kind=int_wp) ::iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
      integer(kind=int_wp) ::num_exchanges_u_dir        ! i  nr of exchanges in 1st direction, only horizontal dir if irregular mesh
      integer(kind=int_wp) ::num_exchanges_v_dir        ! i  nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
      integer(kind=int_wp) ::num_exchanges_z_dir        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer(kind=int_wp) ::num_exchanges_bottom_dir        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)

      ! variables from process_space_real array

      real(kind=real_wp) ::cgrazer     ! i  calculated concentration of grazer                 (gC/m3)
      real(kind=real_wp) ::zharvegr    ! i  harvest flux of grazer                             (gC/m2/d)
      real(kind=real_wp) ::pharvegr    ! i  fraction harvest of grazer  per timestep           (-)
      real(kind=real_wp) ::tgrazer     ! i  threshold concentration of grazer                  (gC/m3)
      integer(kind=int_wp) ::grunitsw    ! i  use gC/m3 (0) or gC/m2 (1) for grazer              (-)
      real(kind=real_wp) ::delt        ! i  timestep for processes                             (d)
      real(kind=real_wp) ::volume      ! i  volume of computational cell                       (m3)
      real(kind=real_wp) ::depth       ! i  depth of segment                                   (m)
      real(kind=real_wp) ::aharvegr    ! o  actual harvest flux grazer                         (gC/m2/d)
      real(kind=real_wp) ::bharvegr    ! o  cumulated harvest flux grazer                      (gC)

      ! local variables

      integer(kind=int_wp), parameter  ::max_message = 1000 ! maximum messages on insufficient biomass attention this is for all instnces of this process
      integer(kind=int_wp), save       ::nr_message  = 0    ! actual number of messages on insufficient biomass attention this is for all instnces of this process
      integer(kind=int_wp) ::lunrep             ! unit number report file
      integer(kind=int_wp) ::nosegw             ! number of segment in the water
      integer(kind=int_wp) ::num_layers              ! number of layers
      integer(kind=int_wp) ::nosegl             ! number of segment per layer
      integer(kind=int_wp) ::ikol               ! column number = segment number top layer
      integer(kind=int_wp) ::ilay               ! layer number
      integer(kind=int_wp) ::iseg               ! segment number
      real(kind=real_wp) ::tot_cgrazer        ! total grazer over the column     (gC/m2)
      real(kind=real_wp) ::tot_depth          ! column depth                     (m)
      real(kind=real_wp) ::harvest            ! harvest                          (gC/m2)
      real(kind=real_wp) ::fharvest           ! fraction harvest of total        (-)

      ! initialisation

      call get_log_unit_number(lunrep)
      call dhnoseg(nosegw)
      call dhnolay(num_layers)
      nosegl = nosegw/num_layers
      if ( nosegl*num_layers /= nosegw ) then
         write(lunrep,*) ' ERROR: unstructured 3d application'
         write(lunrep,*) ' harvesting module not possible'
         call stop_with_error()
      endif

      ! check if unit switch global, consbl allows it to be variable

      if ( increm(5) /= 0 ) then
         write(lunrep,*) ' ERROR: unit switch not a constant'
         write(lunrep,*) ' harvesting module not possible'
         call stop_with_error()
      endif

      grunitsw   = nint(process_space_real(ipoint(5)))
      delt       = process_space_real(ipoint(6))

      ! loop over de kolommen

      do ikol = 1 , nosegl

         ! harvest is opgegeven per kolom neem de waarde van de top laag

         zharvegr   = process_space_real(ipoint(2)+(ikol-1)*increm(2))
         pharvegr   = process_space_real(ipoint(3)+(ikol-1)*increm(3))
         tgrazer    = process_space_real(ipoint(4)+(ikol-1)*increm(4))

         ! doe alleen als harvest groter is dan 0

         if ( zharvegr > 1e-20 .or. pharvegr > 1e-20 ) then

            ! sommeer grazer per laag altijd in gC/m2

            tot_cgrazer = 0.0
            tot_depth   = 0.0

            do ilay = 1 , num_layers

               iseg = ikol+(ilay-1)*nosegl
               cgrazer = process_space_real(ipoint(1)+(iseg-1)*increm(1))
               depth   = process_space_real(ipoint(8)+(iseg-1)*increm(8))

               if ( grunitsw == 1 ) then
                  tot_cgrazer = tot_cgrazer + cgrazer
               else
                  tot_cgrazer = tot_cgrazer + cgrazer*depth
               endif
              tot_depth = tot_depth + depth

            enddo

            ! bereken de harvest in gC/m2

            harvest = zharvegr*delt + pharvegr*tot_cgrazer

            ! maximeer harvest tot de threshold (tgrazer )

            if ( grunitsw == 0 ) then
               tgrazer = tgrazer*tot_depth
            endif
            if ( harvest > tot_cgrazer-tgrazer  ) then
               if ( nr_message < max_message ) then
                  nr_message = nr_message + 1
                  write(lunrep,*) ' WARNING: biomass not sufficient to support harvest rate'
                  write(lunrep,*) ' segment  : ',ikol
                  write(lunrep,*) ' harvest  : ',harvest     ,'gC/m2'
                  write(lunrep,*) ' biomass  : ',tot_cgrazer ,'gC/m2'
                  write(lunrep,*) ' threshold: ',tgrazer     ,'gC/m2'

                  if ( nr_message == max_message ) then
                     write(lunrep,*) 'maximum message on harvesting reached, further messages suppressed'
                  endif
               endif
               harvest = max(0.0,tot_cgrazer-tgrazer)
            endif

            ! bepaal de harvest als fractie van het totaal

            if ( tot_cgrazer > 1e-20 ) then
               fharvest = harvest/tot_cgrazer
            else
               fharvest = 0.0
            endif

            ! de actual harvest is per kolom in gC/m2/d

            aharvegr = harvest/delt

            ! bepaal nieuwe concentratie cgrazer en de geaccumuleerde harvest bharvegr
            ! zet uitvoer in process_space_real, let op cgrazer en bharvegr zijn zowel input and output

            do ilay = 1 , num_layers
               iseg = ikol+(ilay-1)*nosegl
               cgrazer = process_space_real(ipoint(1)+(iseg-1)*increm(1))
               volume  = process_space_real(ipoint(7)+(iseg-1)*increm(7))
               bharvegr= process_space_real(ipoint(10)+(iseg-1)*increm(10))

               if ( grunitsw == 1 ) then
                  bharvegr = bharvegr + cgrazer*fharvest*volume/depth
               else
                  bharvegr = bharvegr + cgrazer*fharvest*volume
               endif
               cgrazer = cgrazer*(1.0-fharvest)

               process_space_real(ipoint(1)+(iseg-1)*increm(1)) = cgrazer
               process_space_real(ipoint(9)+(iseg-1)*increm(9)) = aharvegr
               process_space_real(ipoint(10)+(iseg-1)*increm(10)) = bharvegr
            enddo

         endif

      enddo

      return
      end

      end module m_harves
