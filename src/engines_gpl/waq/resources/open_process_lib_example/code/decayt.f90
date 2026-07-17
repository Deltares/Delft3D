subroutine DECAYT(pmsa, fl, ipoint, increm, noseg,  noflux, iexpnt, iknmrk, &
                    noq1, noq2, noq3, noq4)

!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'DECAYT' :: DECAYT

!>  Implementation of the proces DecayTFoo and DecayTBoo (DECAYT)
!>
!>  temperature dependent decay of Foo and Boo

   implicit none

   real(4) pmsa(*) ! I/O Process Manager System Array, window of routine to process library
   real(4) fl(*) ! O Array of fluxes made by this process in mass/volume/time
   integer ipoint(4) ! I Array of pointers in pmsa to get and store the data
   integer increm(4) ! I Increments in ipoint for segment loop, 0=constant, 1=spatially varying
   integer noseg ! I Number of computational elements in the whole model schematisation
   integer noflux ! I Number of fluxes, increment in the fl array
   integer iexpnt(4, *) ! I From, To, From-1 and To+1 segment numbers of the exchange surfaces
   integer iknmrk(*) ! I Active-Inactive, Surface-water-bottom, see manual for use
   integer noq1 ! I Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
   integer noq2 ! I Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
   integer noq3 ! I Nr of exchanges in 3rd direction, vertical direction, pos. downward
   integer noq4 ! I Nr of exchanges in the bottom (bottom layers, specialist use only)
   integer ipnt(4) ! Local work array for the pointering
   integer iseg ! Local loop counter for computational element loop

   real(4) Substance ! Decaying substance (g/m3)
   real(4) RcDecay20 ! Decay rate at 20oC (1/d)
   real(4) TcDecay ! Temperature coefficient of decay of substance (-)
   real(4) Temp ! Water temperature (oC)
   real(4) RcDecay ! Actual decay rate (1/d)
   real(4) DecayFlux ! Decay flux (g/m3/d)

   integer iflux !    Pointer to flux1 in fl

   ipnt = ipoint
   iflux = 0

   do iseg = 1, noseg
      Substance = pmsa(ipnt(1))
      RcDecay20 = pmsa(ipnt(2))
      TcDecay = pmsa(ipnt(3))
      Temp = pmsa(ipnt(4))

      RcDecay = RcDecay20 * TcDecay ** (Temp - 20.0)
      DecayFlux = RcDecay * Substance

      pmsa(ipnt(5)) = RcDecay
      fl(1 + iflux) = DecayFlux

      iflux = iflux + noflux
      ipnt = ipnt + increm
   end do
end subroutine