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
module m_floceq

implicit none

contains


      subroutine floceq     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
      use m_evaluate_waq_attribute

!
!*******************************************************************************
!
      implicit none
!
!     type    name         i/o description
!
      real(4) pmsa(*)     !i/o process manager system array, window of routine to process library
      real(4) fl(*)       ! o  array of fluxes made by this process in mass/volume/time
      integer ipoint(  9) ! i  array of pointers in pmsa to get and store the data
      integer increm(  9) ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! i  number of computational elements in the whole model schematisation
      integer noflux      ! i  number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
      integer noq1        ! i  nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! i  nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt(  9)   !    local work array for the pointering
      integer iseg        !    local loop counter for computational element loop
!
!*******************************************************************************
!
!     type    name         i/o description                                        unit
!
      real(4) im1         ! i  inorganic matter (im1)                             (gdm/m3)
      real(4) im2         ! i  inorganic matter (im2)                             (gdm/m3)
      real(4) im3         ! i  inorganic matter (im3)                             (gdm/m3)
      real(4) tpm         ! i  total particulate matter (including algae)         (gdw/m3)
      real(4) swfloceq    ! i  0=im1macro-im2micro,1=im2im1,2=im2im3,3=im3im2     (-)
      real(4) rcfloc      ! i  flocculation rate                                  (1/d)
      real(4) rcbreakup   ! i  floc break-up rate                                 (1/d)
      real(4) delt        ! i  timestep for processes                             (d)
      real(4) spmratioem  ! o  flocculation ratio macro:micro empirical model     (-)
      real(4) dflocim1    ! f  flocculation or break-up flux im1                  (g/m3/d)
      real(4) dflocim2    ! f  flocculation or break-up flux im2                  (g/m3/d)
      real(4) dflocim3    ! f  flocculation or break-up flux im3                  (g/m3/d)
      integer idflocim1   !    pointer to the flocculation or break-up flux im1
      integer idflocim2   !    pointer to the flocculation or break-up flux im2
      integer idflocim3   !    pointer to the flocculation or break-up flux im3
      integer ikmrk1      !    first segment attribute
      logical active      !    active segment
      logical bodem       !    sediment bed segment
      real(4) macro       !    concentration macro flocs                            (g/m3)
      real(4) micro       !    concentration micro flocs                            (g/m3)
      real(4) tim         !    total concentration flocs                            (g/m3)
      real(4) macroeq     !    concentration macro flocs in equilibrium             (g/m3)
      real(4) dfloc       !    flocculation or break-up flux                      (g/m3/d)
!
!*******************************************************************************
!
      ipnt        = ipoint
      idflocim1   = 1
      idflocim2   = 2
      idflocim3   = 3

      do 9000 iseg = 1 , noseg

         im1        = pmsa( ipnt(  1) )
         im2        = pmsa( ipnt(  2) )
         im3        = pmsa( ipnt(  3) )
         tpm        = pmsa( ipnt(  4) )
         swfloceq   = pmsa( ipnt(  5) )
         rcfloc     = pmsa( ipnt(  6) )
         rcbreakup  = pmsa( ipnt(  7) )
         delt       = pmsa( ipnt(  8) )

         ! only for active water segments

         active = btest(iknmrk(iseg),0)
         call evaluate_waq_attribute(1,iknmrk(iseg),ikmrk1)
         bodem  = ikmrk1.eq.3
         if ( active .and. .not. bodem ) then

            spmratioem = 0.815 + 3.18e-3*tpm - 1.4e-7*tpm*tpm
            spmratioem = max(0.815,spmratioem)

            if ( swfloceq .eq. 0 ) then
               macro = im1
               micro = im2
            elseif ( swfloceq .eq. 1 ) then
               macro = im2
               micro = im1
            elseif ( swfloceq .eq. 2 ) then
               macro = im2
               micro = im3
            else
               macro = im3
               micro = im2
            endif

            ! calculate flux and restrict flux to 50% in one timestep for stability

            tim     = macro+micro
            macroeq = spmratioem*tim/(1.+spmratioem)
            if ( macroeq .gt. macro ) then
               dfloc = (macroeq-macro)*rcfloc
               dfloc = min(dfloc,0.5*micro/delt)
            else
               dfloc = (macroeq-macro)*rcbreakup
               dfloc = max(dfloc,-0.5*macro/delt)
            endif

            if ( swfloceq .eq. 0 ) then
               dflocim1   =  dfloc
               dflocim2   = -dfloc
               dflocim3   =  0.0
            elseif ( swfloceq .eq. 1 ) then
               dflocim1   = -dfloc
               dflocim2   =  dfloc
               dflocim3   =  0.0
            elseif ( swfloceq .eq. 2 ) then
               macro = im2
               micro = im3
               dflocim1   =  0.0
               dflocim2   =  dfloc
               dflocim3   = -dfloc
            else
               dflocim1   =  0.0
               dflocim2   = -dfloc
               dflocim3   =  dfloc
            endif

         else

            spmratioem = -999.
            dflocim1   = 0.0
            dflocim2   = 0.0
            dflocim3   = 0.0

         endif

         ! pass values back to the system

         fl  ( idflocim1   ) = dflocim1
         fl  ( idflocim2   ) = dflocim2
         fl  ( idflocim3   ) = dflocim3
         pmsa( ipnt(  9)   ) = spmratioem

         idflocim1   = idflocim1   + noflux
         idflocim2   = idflocim2   + noflux
         idflocim3   = idflocim3   + noflux
         ipnt        = ipnt        + increm

 9000 continue

      return
      end subroutine

end module m_floceq
