module m_sedlnk
      implicit none
contains
      subroutine SEDLNK     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'SEDLNK' :: SEDLNK
!
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint( * ) ! I  Array of pointers in pmsa to get and store the data
      integer increm( * ) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
!     support variables
      integer,parameter :: npmsa = 8
      integer ipnt(npmsa) !    Local work array for the pointering
      integer iseg        !    Local loop counter for computational element loop
      integer iflux       !    Local loop counter for computational element loop
      integer ifrac       !    Local loop counter for sediment fractions

!     input items
      real(4) volume
      real(4) fero
      real(4) Ero

!
!*******************************************************************************
!
      ipnt        = ipoint(1:npmsa)
      iflux = 0

      ! loop over segments

      do iseg = 1 , noseg
        volume = pmsa(ipnt(1))
        fero   = pmsa(ipnt(2))

        ! particle erosion fluxes
        do ifrac = 1,6
            Ero = pmsa(ipnt(2+ifrac))
            fl  ( iflux + ifrac  ) = fero*Ero/volume
        enddo

        ipnt = ipnt + increm(1:npmsa)
        iflux = iflux + noflux
      enddo

      return
      end subroutine sedlnk
end module m_sedlnk
