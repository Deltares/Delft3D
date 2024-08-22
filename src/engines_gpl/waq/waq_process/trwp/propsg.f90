!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
module m_propsg
use m_waq_precision


implicit none

contains


subroutine PROPSG   (  process_space_real  , fl    , ipoint, increm, num_cells , &
                       noflux, iexpnt, iknmrk, num_exchanges_u_dir  , num_exchanges_v_dir  , &
                       num_exchanges_z_dir  , num_exchanges_bottom_dir  )
use m_properties
use m_extract_waq_attribute


!>\file
!>       Properties of unaggregated particles (TRW and suspended solids)

!
!     Description of the module :
!
!     Calculate the sedimentation velocity and the critical shear stress
!     based on the particles' physical properties
!
! Name            T   Index   Description                                   Units
! ----            --- -       -------------------                            ----
! DIAMETER        R*4 1   diameter of the particles                            [um]
! DENSITY         R*4 2   density of the particles                          [kg/m3]
! BIOFILM_THK     R*4 3   thickness of the biofilm                             [um]
! BIOFILM_DENSITY R*4 4   density of the biofilm                            [kg/m3]
! SHAPE_FACTOR    R*4 5   shape factor of the particles                         [-]
!
! SETTLE_VEL      R*4 1   settling velocity                                   [m/d]
! TCR_SEDIM       R*4 2   critical shear stress for sedimentation              [Pa]
!
! nov 2021 Jos van Gils added a loop over the fractions, to avoid long lists of processes and to speed up ...

    implicit none

    real(kind=real_wp)      ::process_space_real  ( * ) , fl    (*)
    integer(kind=int_wp)   ::ipoint( * ) , increm(*) , num_cells , noflux, &
             iexpnt(4,*) , iknmrk(*) , num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
!
!   local declarations
!
    integer(kind=int_wp)   ::iseg, ikmrk1,ikmrk2, num_exchanges, iq, ifrom, ipp
    real(kind=real_wp)      ::diameter, density, biofilm_thk, biofilm_density, shape_factor
    real(kind=real_wp)      ::settle_vel, tcr_sedim
    
    integer(kind=int_wp)            ::ipnt(500)
    integer(kind=int_wp), parameter :: ip_nfrac = 1
    integer(kind=int_wp), parameter :: ip_BioFilmDen = 2
    integer(kind=int_wp), parameter :: ip_lastsingle = 2
   
    integer(kind=int_wp)  ::nfrac, ifrac, nitem, offset
    
    nfrac = process_space_real(ipoint(ip_nfrac))
    nitem = ip_lastsingle+7*nfrac ! 4x input and 3x output per fraction
    
!
!  Note: we only need to do this once, no looping over the segments
!  as all particles of the same size class have the same properties
!  JvG Unfortunately DELWAQ makes a PARAMETER if you pass variables between subroutines,
!      so a space loop is becessary
!
!  Note:
!  The routine is called for more than one fraction, so redo the
!  calculations for each time step.
!  JvG This is also not true, if a PARAMETER has its own name, it has its own memory

!
    ipnt(1:nitem) = ipoint(1:nitem)
    
    do iseg = 1 , num_cells
        call extract_waq_attribute(1,iknmrk(iseg),ikmrk1)
        if (ikmrk1==1) then
            call extract_waq_attribute(2,iknmrk(iseg),ikmrk2)
                
            ! input independentt of fractions
            biofilm_density = process_space_real(ipnt(ip_BioFilmDen))
            
            ! loop over active fractions
            do ifrac = 1,nfrac
                diameter        = process_space_real(ipnt(ip_lastsingle        +ifrac))
                density         = process_space_real(ipnt(ip_lastsingle+nfrac  +ifrac))
                shape_factor    = process_space_real(ipnt(ip_lastsingle+nfrac*2+ifrac))
                biofilm_thk     = process_space_real(ipnt(ip_lastsingle+nfrac*3+ifrac))

                call add_biofilm( diameter, density, biofilm_thk, biofilm_density )
                call calculate_sedim( diameter, density, shape_factor, settle_vel, tcr_sedim )

                process_space_real(ipnt(ip_lastsingle+nfrac*4+ifrac)) = settle_vel
                process_space_real(ipnt(ip_lastsingle+nfrac*5+ifrac)) = tcr_sedim
            enddo
                
        endif
            
        ipnt(1:nitem) = ipnt(1:nitem) + increm(1:nitem)

    enddo
    
    ! addition for use in 3D
    
    num_exchanges = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir
    offset = ip_lastsingle+nfrac*6
    ipnt(1:nitem) = ipoint(1:nitem)
    do IQ=1,num_exchanges_u_dir+num_exchanges_v_dir
        do ifrac = 1,nfrac
            process_space_real(ipnt(offset+ifrac)) = 0.0
        enddo
        ipnt(1:nitem) = ipnt(1:nitem) + increm(1:nitem)
    enddo
    do  IQ=num_exchanges_u_dir+num_exchanges_v_dir+1,num_exchanges
        ifrom = IEXPNT(1,IQ)
!
!       Sedimentation velocity from segment to exchange-area
!
        IF ( ifrom > 0 ) THEN
            do ifrac = 1,nfrac
                ipp = ip_lastsingle+nfrac*4+ifrac
                settle_vel = process_space_real( ipoint(ipp) + (ifrom-1) * increm(ipp) )
                process_space_real(ipnt(offset+ifrac)) = settle_vel
            enddo
        ENDIF
        ipnt(1:nitem) = ipnt(1:nitem) + increm(1:nitem)
    enddo
    
    return
end

end module m_propsg
