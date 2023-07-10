module m_genwwm
      use m_evaluate_waq_attribute
      implicit none
contains
      subroutine GENWWM     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'GENWWM' :: GENWWM
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint(*)  ! I  Array of pointers in pmsa to get and store the data
      integer increm(*)  ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
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
!     D-EM Preprocessor to deal with WW management across Peru
!

!
!     Type    Name         I/O Description                                        Unit
!
      integer            :: iseg, iatt1
      real               :: FrUnManaged, FrUnTreated

    ! PMSA admin
      integer,parameter   :: lins = 15
      integer,parameter   :: louts = 8
      integer            :: ipnt(lins+louts)    !    Local work array for the pointering

      ! pointers to concrete items
      integer,parameter   :: ip_FrSewered = 1
      integer,parameter   :: ip_fOpenWater = 2
      integer,parameter   :: ip_fSldgRem = 3
      integer,parameter   :: ip_FrSeptic = 4
      integer,parameter   :: ip_FrTreat1 = 5
      integer,parameter   :: ip_FrTreat2 = 6
      integer,parameter   :: ip_FrTreat3 = 7
      integer,parameter   :: ip_Eff_Septic = 8
      integer,parameter   :: ip_Eff_Treat1 = 9
      integer,parameter   :: ip_Eff_Treat2 = 10
      integer,parameter   :: ip_Eff_Treat3 = 11
      integer,parameter   :: ip_Sld_Septic = 12
      integer,parameter   :: ip_Sld_Treat1 = 13
      integer,parameter   :: ip_Sld_Treat2 = 14
      integer,parameter   :: ip_Sld_Treat3 = 15

      integer,parameter   :: ip_Eff_WWTP = 16
      integer,parameter   :: ip_Sld_WWTP = 17
      integer,parameter   :: ip_WWtoSew = 18
      integer,parameter   :: ip_WWtoPav = 19
      integer,parameter   :: ip_WWtoUnp = 20
      integer,parameter   :: ip_WWtoStw = 21
      integer,parameter   :: ip_WWtoSfw = 22
      integer,parameter   :: ip_WWtoSoi = 23

      ! input and output items
      real   ::  FrSewered
      real   ::  fOpenWater
      real   ::  fSldgRem
      real   ::  FrSeptic
      real   ::  FrTreat1
      real   ::  FrTreat2
      real   ::  FrTreat3
      real   ::  Eff_Septic
      real   ::  Eff_Treat1
      real   ::  Eff_Treat2
      real   ::  Eff_Treat3
      real   ::  Sld_Septic
      real   ::  Sld_Treat1
      real   ::  Sld_Treat2
      real   ::  Sld_Treat3

      real   ::  Eff_WWTP
      real   ::  Sld_WWTP
      real   ::  WWtoSew
      real   ::  WWtoPav
      real   ::  WWtoUnp
      real   ::  WWtoStw
      real   ::  WWtoSfw
      real   ::  WWtoSoi


      logical first
      data first /.true./

      save
!
!******************************************************************************* INITIAL PROCESSING

      if (first) then

          ! loop for processing
          ipnt = ipoint(1:lins+louts)

          do iseg = 1 , noseg

              call evaluate_waq_attribute(1,iknmrk(iseg),iatt1) ! pick up first attribute
              if (iatt1.gt.0) then

                  FrSewered = pmsa(ipnt(ip_FrSewered))
                  fOpenWater = pmsa(ipnt(ip_fOpenWater))
                  fSldgRem = pmsa(ipnt(ip_fSldgRem))
                  FrSeptic = pmsa(ipnt(ip_FrSeptic))
                  FrTreat1 = pmsa(ipnt(ip_FrTreat1))
                  FrTreat2 = pmsa(ipnt(ip_FrTreat2))
                  FrTreat3 = pmsa(ipnt(ip_FrTreat3))
                  Eff_Septic = pmsa(ipnt(ip_Eff_Septic)) ! Interpret as losses to Sfw, before reaching WWTPs
                  Eff_Treat1 = pmsa(ipnt(ip_Eff_Treat1))
                  Eff_Treat2 = pmsa(ipnt(ip_Eff_Treat2))
                  Eff_Treat3 = pmsa(ipnt(ip_Eff_Treat3))
                  Sld_Septic = pmsa(ipnt(ip_Sld_Septic)) ! Interpret as losses to Soi, before reaching WWTPs
                  Sld_Treat1 = pmsa(ipnt(ip_Sld_Treat1))
                  Sld_Treat2 = pmsa(ipnt(ip_Sld_Treat2))
                  Sld_Treat3 = pmsa(ipnt(ip_Sld_Treat3))

                  ! unmanaged
                  FrUnManaged = max((1.0 - FrSewered - FrSeptic),0.0)

                  ! untreated
                  FrUnTreated = max((1.0 - FrTreat1 - FrTreat2 - FrTreat3),0.0)

                  ! fraction to effluent for WW collected and treated AND adopted from septic tanks
                  Eff_WWTP = FrUnTreated + FrTreat1*Eff_Treat1 + FrTreat2*Eff_Treat2 + FrTreat3*Eff_Treat3

                  ! fraction to sludge for WW collected and treated
                  Sld_WWTP = (FrTreat1*Sld_Treat1 + FrTreat2*Sld_Treat2 + FrTreat3*Sld_Treat3)*(1.0-fSldgRem)

                  ! Definition of receptors, sewered part + remaining part of septic tanks
                  WWtoSew = FrSewered + FrSeptic*(1-Eff_Septic-Sld_Septic)

                  ! N.A.
                  WWtoPav = 0.0
                  WWtoUnp = 0.0
                  WWtoStw = 0.0

                  ! fraction unsewered to surface waters
                  WWtoSfw = FrUnManaged*     fOpenWater  + FrSeptic*Eff_Septic
                  WWtoSoi = FrUnManaged*(1.0-fOpenWater) + FrSeptic*Sld_Septic

                  ! Store output
                  pmsa(ipnt(ip_Eff_WWTP)) = Eff_WWTP
                  pmsa(ipnt(ip_Sld_WWTP)) = Sld_WWTP
                  pmsa(ipnt(ip_WWtoSew)) = WWtoSew
                  pmsa(ipnt(ip_WWtoPav)) = WWtoPav
                  pmsa(ipnt(ip_WWtoUnp)) = WWtoUnp
                  pmsa(ipnt(ip_WWtoStw)) = WWtoStw
                  pmsa(ipnt(ip_WWtoSfw)) = WWtoSfw
                  pmsa(ipnt(ip_WWtoSoi)) = WWtoSoi

              ! end IF active column
              endif

              ipnt = ipnt + increm(1:lins+louts)

          enddo

          first = .false.
      endif
!******************************************************************************* NO PROCESSING in TIME LOOP

      return
      end subroutine genwwm
end module m_genwwm
