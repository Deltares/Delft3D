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

      subroutine init_3dl( noseg , nosegw, nosegl, nolay , ngro  , ntyp  )
      
      use m_srstop
      use m_monsys
      use bloom_data_3dl

      implicit none

!     FUNCTION : set dimensions and allocate memory for bloom_data_3dl

!     subroutines called

!     GETMLU, get the untit number of the report file
!     SRSTOP, stops execution

!     arguments
      integer  noseg      ! input, total number of segments
      integer  nosegw     ! input, number of segments in the water phase
      integer  nosegl     ! input, number of segments per layer
      integer  nolay      ! input, number of layers
      integer  ngro       ! input, number of BLOOM algae groups
      integer  ntyp       ! input, number of BLOOM algae types

!     local decalarations
      integer  ierr_alloc ! error number memory allocation
      integer  lunrep     ! unit number report file

      noseg_3dl  = noseg
      nosegl_3dl = nosegl
      nolay_3dl  = nolay
      ngro_3dl   = ngro
      ntyp_3dl   = ntyp

      allocate ( radsurf_3dl(noseg), effic_3dl(ntyp,noseg), stat = ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then
         call getmlu(lunrep)
         write ( lunrep , 1000 ) ierr_alloc
         write ( lunrep , 1001 ) noseg
         write ( lunrep , 1002 ) ntyp
         call srstop(1)
      endif
      effic_3dl    = 0.0

      return
 1000 format(' ERROR: allocating memory in INIT_3DL:',I10)
 1001 format(' NOSEG, number of segments           :',I10)
 1002 format(' NTYP , number of BLOOM  algae types :',I10)
      end subroutine init_3dl

      subroutine effi_3dl( effi , itype )

!     FUNCTION : Gives average effeiciency over the layers

!     use the results from the vertical distribution VTRANS

      use bloom_data_vtrans
      use bloom_data_3dl

      implicit none

!     arguments
      real*8   effi       ! output, average effieciency
      integer  itype      ! input , index number of BLOOM algae type

!     local decalarations
      integer  ilay       ! layer counter
      integer  iseg       ! segment number
      real*8   flay       ! time factor in a specific layer
      real*8   elay       ! efficiency in a specific layer

!     check if active
      if ( .not. active_3dl ) then
!        just take efficiency for this layer
         effi = effic_3dl(itype,iseg_3dl)
      else
!        accumulate efficiencies over the layers ELAY * the time fraction per layer FLAY
         effi = 0.0
         if(.not.fm_vtrans) then
            do ilay = 1 , nolay_3dl
               iseg  = (ilay-ilay_3dl)*nosegl_3dl + iseg_3dl
               flay  = fracv(ilay,iseg_3dl)
               elay  = effic_3dl(itype,iseg)
               effi  = effi + flay*elay
            enddo
         else
            do iseg = fmktop(iseg_3dl), fmkbot(iseg_3dl), -1
               ilay = fmlayer(iseg)
               flay  = fracv(ilay,iseg_3dl)
               elay  = effic_3dl(itype,iseg)
               effi  = effi + flay*elay
            enddo
         endif
         effi = effi
      endif
      return
      end subroutine effi_3dl

      subroutine effilay_3dl( surf, exttot, dep   , igroup, itype )

      use bloom_data_3dl

      implicit none

!     FUNCTION : calculate and store efficiency for this layer

!     arguments
      real*8   surf       ! input , corrected irradiation
      real*8   exttot     ! input , total extinction
      real*8   dep        ! input , depth of the layer
      integer  igroup     ! input , index number of BLOOM algae group
      integer  itype      ! input , index number of BLOOM algae type

!     local decalarations
      real*8   phi_s      ! x value tabulated function at surface
      real*8   fun_s      ! function at surface
      real*8   der_s      ! derivative at sutface
      real*8   phi_d      ! x value tabulated function at dep
      real*8   fun_d      ! function at surface at dep
      real*8   der_d      ! derivative at sutface at dep
      real*8   effi       ! calculated efficiency

      if ( surf .gt. 1.0 .and. exttot*dep .gt. 1.0d-10) then
         phi_s = - dlog(surf)
         call ebcalc(phi_s,fun_s,der_s,igroup)
         phi_d = exttot*dep - dlog(surf)
         call ebcalc(phi_d,fun_d,der_d,igroup)
         effi   = (fun_d-fun_s)/exttot/dep
         effi = max(effi,0.0)
      else
         effi = 0.0
      endif

!     store for later use
      effic_3dl(itype,iseg_3dl) = effi

      return
      end subroutine effilay_3dl
