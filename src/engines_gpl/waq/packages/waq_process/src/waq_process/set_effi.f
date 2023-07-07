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

      subroutine set_effi( temper, radiat, ext   , depthw, daylen,
     &                     id    )
!>\file
!>       calculate and store efficiency for all species

!     modules

      use      bloom_data_3dl   ! data and routine for 3D light approach
      
      use bloom_data_dim
      use bloom_data_size 
      use bloom_data_arran   
      use bloom_data_phyt    
      use bloom_data_putin

      implicit none

!     arguments

      real     temper     ! input , temperature
      real     radiat     ! input , radiation
      real     ext        ! input , total extinction
      real     depthw     ! input , depth of the layer
      real     daylen     ! input , daylength in hours
      integer  id         ! input , weeknumber

!     local decalarations

      real*8   alpha      ! reflection factor
      real*8   temp       ! temperature
      real*8   csol       ! radiation
      real*8   dsol       ! radiation
      real*8   dep        ! depth
      real*8   exttot     ! total extinction
      real*8   day        ! daylength in hours
      real*8   deat       ! DEAT
      real*8   tcorr      ! TCORR
      real*8   surf_typ   ! scaled, converted and corrected radiation for a type
      integer  igroup     ! index number of BLOOM algae group
      integer  itype      ! index number of BLOOM algae type
      real*8   pmax20(mt),sdmixn(mt)


      dep    = depthw
      exttot = ext
      temp   = temper
      csol   = solaco * radiat
      day    = daylen
      deat   = 0d0
      call natmor (deat, temp)
      do itype = 1,ntyp_3dl
         if (sdmix(itype) .lt. 0.0) then
            sdmixn(itype) = 1.0d0 + sdmix(itype)
!           dmix(k) = dabs(sdmix(itype)) * dep
         else
            sdmixn(itype) = 0.0d0
         endif
      enddo


      call maxprd ( tefcur )
      do itype = 1,ntyp_3dl
         pmax20(itype) = pmax(itype)
      enddo
      call maxprd ( temp  )

      dsol=1428.57*csol
      do igroup = 1 , ngro_3dl
         do itype = it2(igroup,1),it2(igroup,2)
            tcorr      = pmax20(itype)/pmax(itype)
            surf_typ   = tcorr * dsol * dexp (- exttot * sdmixn(itype) * dep)
            surf_typ   = surf_typ/day
            call effilay_3dl( surf_typ, exttot, dep   , igroup, itype )
         enddo
      enddo

      return
      end subroutine set_effi
