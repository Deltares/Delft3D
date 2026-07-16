subroutine DECAYT(pmsa, fl, ipoint, increm, noseg,  noflux, iexpnt, iknmrk, &
                    noq1, noq2, noq3, noq4)

!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'DECAYT' :: DECAYT

!>  Implementation of the proces DecayTFoo and DecayTBoo (DECAYT)
!>
!>  temperature dependent decay of Foo and Boo

   use m_waq_precision
   use m_logger_helper
   use m_extract_waq_attribute

   implicit none

   integer, parameter :: number_params = 4 + 1 ! four input parameters and one output parameter

   real(real_wp) :: pmsa(*)               ! I/O Process Manager System Array, window of routine to process library
   real(real_wp) :: fl(*)                 ! O Array of fluxes made by this process in mass/volume/time
   integer       :: ipoint(number_params) ! I Array of pointers in pmsa to get and store the data
   integer       :: increm(number_params) ! I Increments in ipoint for segment loop, 0=constant, 1=spatially varying
   integer       :: noseg                 ! I Number of computational elements in the whole model schematisation
   integer       :: noflux                ! I Number of fluxes, increment in the fl array
   integer       :: iexpnt(4, *)          ! I From, To, From-1 and To+1 segment numbers of the exchange surfaces
   integer       :: iknmrk(*)             ! I Active-Inactive, Surface-water-bottom, see manual for use
   integer       :: noq1                  ! I Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
   integer       :: noq2                  ! I Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
   integer       :: noq3                  ! I Nr of exchanges in 3rd direction, vertical direction, pos. downward
   integer       :: noq4                  ! I Nr of exchanges in the bottom (bottom layers, specialist use only)

   integer       :: ipnt(number_params)   ! Local work array for the pointering
   integer       :: iseg                  ! Local loop counter for computational element loop

   real(real_wp) :: Substance   ! Decaying substance (g/m3)
   real(real_wp) :: RcDecay20   ! Decay rate at 20oC (1/d)
   real(real_wp) :: TcDecay     ! Temperature coefficient of decay of substance (-)
   real(real_wp) :: Temp        ! Water temperature (oC)
   real(real_wp) :: RcDecay     ! Actual decay rate (1/d)
   real(real_wp) :: DecayFlux   ! Decay flux (g/m3/d)

   integer       :: iflux       ! Pointer to flux1 in fl
   integer       :: attrib1     ! Value of the first attribute of a segment/cell

   ipnt = ipoint
   iflux = 0

   do iseg = 1, noseg

      ! Limit the calculation to active segments (cells)
      ! These have a first attribute with value 1

      call extract_waq_attribute( 1, iknmrk(iseg), attrib1 )

      if ( attrib1 == 1 ) then

         ! The cell is active, so get the values and do the calculation

         Substance = pmsa(ipnt(1))
         RcDecay20 = pmsa(ipnt(2))
         TcDecay = pmsa(ipnt(3))
         Temp = pmsa(ipnt(4))

         ! Check for valid values, if invalid, stop

         if ( rcdecay20 < 0.0 .or. tcdecay < 0.0 ) then
            call write_error_message( "Negative decay coefficient or temperature dependence" )
         endif

         RcDecay = RcDecay20 * TcDecay ** (Temp - 20.0)
         DecayFlux = RcDecay * Substance

         pmsa(ipnt(5)) = RcDecay
         fl(1 + iflux) = DecayFlux
      endif

      ! Always increment the pointers

      iflux = iflux + noflux
      ipnt  = ipnt + increm
   end do
end subroutine
