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
      module m_macrop

      implicit none

      contains


      SUBROUTINE MACROP     ( PMSA   , FL     , IPOINT , INCREM, NOSEG ,
     +                        NOFLUX , IEXPNT , IKNMRK , NOQ1  , NOQ2  ,
     +                        NOQ3   , NOQ4   )
      use m_monsys
      use m_evaluate_waq_attribute

!
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      REAL(4) PMSA(*)     !I/O Process Manager System Array, window of routine to process library
      REAL(4) FL(*)       ! O  Array of fluxes made by this process in mass/volume/time
      INTEGER IPOINT(66)   ! I  Array of pointers in PMSA to get and store the data
      INTEGER INCREM(66)   ! I  Increments in IPOINT for segment loop, 0=constant, 1=spatially varying
      INTEGER NOSEG       ! I  Number of computational elements in the whole model schematisation
      INTEGER NOFLUX      ! I  Number of fluxes, increment in the FL array
      INTEGER IEXPNT(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      INTEGER IKNMRK(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      INTEGER NOQ1        ! I  Nr of exchanges in 1st direction, only horizontal dir if irregular mesh
      INTEGER NOQ2        ! I  Nr of exchanges in 2nd direction, NOQ1+NOQ2 gives hor. dir. reg. grid
      INTEGER NOQ3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      INTEGER NOQ4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      INTEGER IPNT( 66)   !    Local work array for the pointering
      INTEGER ISEG        !    Local loop counter for computational element loop
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
      REAL(4) EM01        ! I  macrophyt emerged 01                               (gC)
      REAL(4) SM01        ! I  macrophyt submerged 01                             (gC)
      REAL(4) RH01        ! I  macrophyt rhizome 01                               (gC)
      REAL(4) NRH01       ! I  nitrogen content macrophyt rhizome 01              (gN)
      REAL(4) PRH01       ! I  phosphorus content macrophyt rhizome 01            (gP)
      REAL(4) MaxEM01     ! I  maximum biomass for macrophyt emerged 01           (gC)
      REAL(4) MaxSM01     ! I  maximum biomass for macrophyt submerged 01         (gC)
      REAL(4) PPmaxEM01   ! I  potential growth rate macrophyt emerged 01         (1/d)
      REAL(4) PPmaxSM01   ! I  potential growth rate macrophyt submerged 01       (1/d)
      REAL(4) EM01thresh  ! I  threshold biomass EM01 in growth                   (gC/m2)
      REAL(4) SM01thresh  ! I  threshold biomass SM01 in growth                   (gC/m2)
      REAL(4) RH01min     ! I  minimal biomass RH01                               (gC/m2)
      REAL(4) NRH01min    ! I  minimal NRH01                                      (gN/m2)
      REAL(4) PRH01min    ! I  minimal PRH01                                      (gP/m2)
      REAL(4) NH4crEM01   ! I  critical NH4 conc. macrophyt emerged 01            (gN/m3)
      REAL(4) NO3crEM01   ! I  critical NO3 conc. macrophyt emerged 01            (gN/m3)
      REAL(4) PO4crEM01   ! I  critical PO4 conc. macrophyt emerged 01            (gN/m3)
      REAL(4) CO2crSM01   ! I  critical CO2 conc. macrophyt submerged 01          (g/m3)
      REAL(4) NH4         ! I  Ammonium (NH4)                                     (gN/m3)
      REAL(4) NO3         ! I  Nitrate (NO3)                                      (gN/m3)
      REAL(4) PO4         ! I  Ortho-Phosphate (PO4)                              (gP/m3)
      REAL(4) LimNutSm01  ! I  nutrient (N,P,C) limitation                        (-)
      REAL(4) LimRadSM01  ! I  radiation limitation function SM01 <0-1>           (-)
      REAL(4) MinDLEM01   ! I  minimal daylength for growth EM01                  (d)
      REAL(4) OptDLEM01   ! I  daylength for growth saturation EM01               (d)
      REAL(4) MinDLSM01   ! I  minimal daylength for growth EM01                  (d)
      REAL(4) OptDLSM01   ! I  daylength for growth saturation EM01               (d)
      REAL(4) DayL        ! I  daylength <0-1>                                    (d)
      REAL(4) TcritEM01   ! I  critical temperature for growth EM01               (oC)
      REAL(4) TcPMxEM01   ! I  temperature coefficient for growth EM01            (-)
      REAL(4) TcritSM01   ! I  critical temperature for growth SM01               (oC)
      REAL(4) TcPMxSM01   ! I  temperature coefficient for growth SM01            (-)
      REAL(4) Temp        ! I  ambient water temperature                          (oC)
      REAL(4) K1DecaEM01  ! I  first order autumn decay rate EM01                 (1/d)
      REAL(4) TcDecaEM01  ! I  temperature coefficient for decay EM01             (-)
      REAL(4) K1DecaSM01  ! I  first order autumn decay rate SM01                 (1/d)
      REAL(4) TcDecaSM01  ! I  temperature coefficient for decay SM01             (-)
      REAL(4) FrEMtoRH01  ! I  fraction EM that becomes RH01                      (-)
      REAL(4) FrSMtoRH01  ! I  fraction SM that becomes RH01                      (-)
      REAL(4) NCRatEM01   ! I  N:C ratio EM01                                     (gN/gC)
      REAL(4) PCRatEM01   ! I  P:C ratio EM01                                     (gP/gC)
      REAL(4) NCRatSM01   ! I  N:C ratio SM01                                     (gN/gC)
      REAL(4) PCRatSM01   ! I  P:C ratio SM01                                     (gP/gC)
      REAL(4) NCRatRH01   ! I  N:C ratio RH01                                     (gN/gC)
      REAL(4) PCRatRH01   ! I  P:C ratio RH01                                     (gP/gC)
      REAL(4) FrPOC1EM01  ! I  fraction of decay EM01 that becomes POC1           (-)
      REAL(4) FrPOC2EM01  ! I  fraction of decay EM01 that becomes POC2           (-)
      REAL(4) FrPOC3EM01  ! I  fraction of decay EM01 that becomes POC3           (-)
      REAL(4) FrPOC1SM01  ! I  fraction of decay SM01 that becomes POC1           (-)
      REAL(4) FrPOC2SM01  ! I  fraction of decay SM01 that becomes POC2           (-)
      REAL(4) FrPOC3SM01  ! I  fraction of decay SM01 that becomes POC3           (-)
      REAL(4) Surf        ! I  horizontal surface area of a DELWAQ segment        (m2)
      REAL(4) DELT        ! I  timestep for processes                             (d)
      REAL(4) Depth       ! I  depth of segment                                   (m)
      REAL(4) EM01M2      ! O  emerged macrophyt 1 per square metre               (gC/m2)
      REAL(4) SM01M2      ! O  submerged macrophyt 1 per square metre             (gC/m2)
      REAL(4) RH01M2      ! O  rhizome macrophyt 1 per square metre               (gC/m2)
      REAL(4) LimNH4EM01  ! O  NH4 limitation function for EM01 <0-1>             (-)
      REAL(4) LimNO3EM01  ! O  NO3 limitation function for EM01 <0-1>             (-)
      REAL(4) LimPO4EM01  ! O  PO4 limitation function for EM01 <0-1>             (-)
      REAL(4) LimNutEM01  ! O  nutrient limitation function for EM01 <0-1>        (-)
      REAL(4) LimCO2SM01  ! O  CO2 limitation function for SM01 <0-1>             (-)
      REAL(4) LimDLEM01   ! O  daylength limitation function for EM01 <0-1>       (-)
      REAL(4) LimDLSM01   ! O  daylength limitation function for SM01 <0-1>       (-)
      REAL(4) LimTEM01    ! O  temperature limitation function for EM01 <0-1>     (-)
      REAL(4) LimTSM01    ! O  temperature limitation function for SM01 <0-1>     (-)
      REAL(4) dGrowEM01   ! F  growth of EM01 species                             (gC/m3/d)
      REAL(4) dGrowSM01   ! F  growth of SM01 species                             (gC/m3/d)
      REAL(4) dDecayEM01  ! F  decay of EM01 species                              (gC/m3/d)
      REAL(4) dDecaySM01  ! F  decay of SM01 species                              (gC/m3/d)
      REAL(4) dCtEMtRH01  ! F  translocation of C from EM to RH01                 (gC/m3/d)
      REAL(4) dCtSMtRH01  ! F  translocation of C from SM to RH01                 (gC/m3/d)
      REAL(4) dCtRHtEM01  ! F  translocation of C from RH to EM01                 (gC/m3/d)
      REAL(4) dCtRHtSM01  ! F  translocation of C from RH to SM01                 (gC/m3/d)
      REAL(4) dNtEMtRH01  ! F  translocation of N from EM to RH01                 (gN/m3/d)
      REAL(4) dNtSMtRH01  ! F  translocation of N from SM to RH01                 (gN/m3/d)
      REAL(4) dNtRHtEM01  ! F  translocation of N from RH to EM01                 (gN/m3/d)
      REAL(4) dNtRHtSM01  ! F  translocation of N from RH to SM01                 (gN/m3/d)
      REAL(4) dPtEMtRH01  ! F  translocation of P from EM to RH01                 (gP/m3/d)
      REAL(4) dPtSMtRH01  ! F  translocation of P from SM to RH01                 (gP/m3/d)
      REAL(4) dPtRHtEM01  ! F  translocation of P from RH to EM01                 (gP/m3/d)
      REAL(4) dPtRHtSM01  ! F  translocation of P from RH to SM01                 (gP/m3/d)
      REAL(4) dNH4upEM01  ! F  NH4 uptake by EM01                                 (gN/m3/d)
      REAL(4) dNO3upEM01  ! F  NO3 uptake by EM01                                 (gN/m3/d)
      REAL(4) dPO4upEM01  ! F  PO4 uptake by EM01                                 (gP/m3/d)
      REAL(4) dNupSM01    ! F  uptake flux nitrogen SM01                          (gN/m3/d)
      REAL(4) dPupSM01    ! F  uptake flux phosphorus SM01                        (gP/m3/d)
      REAL(4) dPrPOC1M01  ! F  POC1 production macrophyt 1                        (gC/m3/d)
      REAL(4) dPrPOC2M01  ! F  POC2 production macrophyt 1                        (gC/m3/d)
      REAL(4) dPrPOC3M01  ! F  POC3 production macrophyt 1                        (gC/m3/d)
      REAL(4) dPrPON1M01  ! F  PON1 production macrophyt 1                        (gN/m3/d)
      REAL(4) dPrPON2M01  ! F  PON2 production macrophyt 1                        (gN/m3/d)
      REAL(4) dPrPON3M01  ! F  PON3 production macrophyt 1                        (gN/m3/d)
      REAL(4) dPrPOP1M01  ! F  POP1 production macrophyt 1                        (gP/m3/d)
      REAL(4) dPrPOP2M01  ! F  POP2 production macrophyt 1                        (gP/m3/d)
      REAL(4) dPrPOP3M01  ! F  POP3 production macrophyt 1                        (gP/m3/d)
      REAL(4) dSM01OXY    ! F  oxygen production SM01                             (gO/m3/d)
      REAL(4) dSM01CO2    ! F  CO2 uptake SM01                                    (gO/m3/d)
      INTEGER IdGrowEM01  !    Pointer to the growth of EM01 species
      INTEGER IdGrowSM01  !    Pointer to the growth of SM01 species
      INTEGER IdDecayEM01 !    Pointer to the decay of EM01 species
      INTEGER IdDecaySM01 !    Pointer to the decay of SM01 species
      INTEGER IdCtEMtRH01 !    Pointer to the translocation of C from EM to RH01
      INTEGER IdCtSMtRH01 !    Pointer to the translocation of C from SM to RH01
      INTEGER IdCtRHtEM01 !    Pointer to the translocation of C from RH to EM01
      INTEGER IdCtRHtSM01 !    Pointer to the translocation of C from RH to SM01
      INTEGER IdNtEMtRH01 !    Pointer to the translocation of N from EM to RH01
      INTEGER IdNtSMtRH01 !    Pointer to the translocation of N from SM to RH01
      INTEGER IdNtRHtEM01 !    Pointer to the translocation of N from RH to EM01
      INTEGER IdNtRHtSM01 !    Pointer to the translocation of N from RH to SM01
      INTEGER IdPtEMtRH01 !    Pointer to the translocation of P from EM to RH01
      INTEGER IdPtSMtRH01 !    Pointer to the translocation of P from SM to RH01
      INTEGER IdPtRHtEM01 !    Pointer to the translocation of P from RH to EM01
      INTEGER IdPtRHtSM01 !    Pointer to the translocation of P from RH to SM01
      INTEGER IdNH4upEM01 !    Pointer to the NH4 uptake by EM01
      INTEGER IdNO3upEM01 !    Pointer to the NO3 uptake by EM01
      INTEGER IdPO4upEM01 !    Pointer to the PO4 uptake by EM01
      INTEGER IdNupSM01   !    Pointer to the uptake flux nitrogen SM01
      INTEGER IdPupSM01   !    Pointer to the uptake flux phosphorus SM01
      INTEGER IdPrPOC1M01 !    Pointer to the POC1 production macrophyt 1
      INTEGER IdPrPOC2M01 !    Pointer to the POC2 production macrophyt 1
      INTEGER IdPrPOC3M01 !    Pointer to the POC3 production macrophyt 1
      INTEGER IdPrPON1M01 !    Pointer to the PON1 production macrophyt 1
      INTEGER IdPrPON2M01 !    Pointer to the PON2 production macrophyt 1
      INTEGER IdPrPON3M01 !    Pointer to the PON3 production macrophyt 1
      INTEGER IdPrPOP1M01 !    Pointer to the POP1 production macrophyt 1
      INTEGER IdPrPOP2M01 !    Pointer to the POP2 production macrophyt 1
      INTEGER IdPrPOP3M01 !    Pointer to the POP3 production macrophyt 1
      INTEGER IdSM01OXY   !    Pointer to the oxygen production SM01
      INTEGER IdSM01CO2   !    Pointer to the CO2 uptake SM01

      REAL    NRH01M2     !    N contents RH01 (gN/m2)
      REAL    PRH01M2     !    N contents RH01 (gN/m2)
      REAL    EM01Grow    !    grow rate EM01 (gC/m2/d)
      REAL    SM01Grow    !    grow rate SM01 (gC/m2/d)
      REAL    CUPTAKE     !    total C uptake
      REAL    NUPTAKE     !    total N uptake
      REAL    PUPTAKE     !    total P uptake
      REAL    FRNH4EM01   !    fraction uptake NH4
      REAL    FRNO3EM01   !    fraction uptake NO3
      REAL    SUMFR       !    sum of the fractions - for normalization
      INTEGER IKMRK1
      INTEGER IKMRK2
      REAL    FACTORE, FACTORS

      INTEGER       :: LUNREP
      INTEGER, SAVE :: NR_MSG = 0

!
!*******************************************************************************
!
      IdGrowEM01  = 1
      IdGrowSM01  = 2
      IdDecayEM01 = 3
      IdDecaySM01 = 4
      IdCtEMtRH01 = 5
      IdCtSMtRH01 = 6
      IdCtRHtEM01 = 7
      IdCtRHtSM01 = 8
      IdNtEMtRH01 = 9
      IdNtSMtRH01 = 10
      IdNtRHtEM01 = 11
      IdNtRHtSM01 = 12
      IdPtEMtRH01 = 13
      IdPtSMtRH01 = 14
      IdPtRHtEM01 = 15
      IdPtRHtSM01 = 16
      IdNH4upEM01 = 17
      IdNO3upEM01 = 18
      IdPO4upEM01 = 19
      IdNupSM01   = 20
      IdPupSM01   = 21
      IdPrPOC1M01 = 22
      IdPrPOC2M01 = 23
      IdPrPOC3M01 = 24
      IdPrPON1M01 = 25
      IdPrPON2M01 = 26
      IdPrPON3M01 = 27
      IdPrPOP1M01 = 28
      IdPrPOP2M01 = 29
      IdPrPOP3M01 = 30
      IdSM01OXY   = 31
      IdSM01CO2   = 32

      !
      ! Loop over (essentially) the columns
      !
      IPNT        = IPOINT

      DO 9000 ISEG = 1 , NOSEG

         CALL evaluate_waq_attribute(1,IKNMRK(ISEG),IKMRK1)

         IF (IKMRK1.EQ.1) THEN

            CALL evaluate_waq_attribute(2,IKNMRK(ISEG),IKMRK2)

            IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN


               EM01       = PMSA( IPNT(  1) )
               SM01       = PMSA( IPNT(  2) )
               RH01       = PMSA( IPNT(  3) )
               NRH01      = PMSA( IPNT(  4) )
               PRH01      = PMSA( IPNT(  5) )
               MaxEM01    = PMSA( IPNT(  6) )
               MaxSM01    = PMSA( IPNT(  7) )
               PPmaxEM01  = PMSA( IPNT(  8) )
               PPmaxSM01  = PMSA( IPNT(  9) )
               EM01thresh = PMSA( IPNT( 10) )
               SM01thresh = PMSA( IPNT( 11) )
               RH01min    = PMSA( IPNT( 12) )
               NRH01min   = PMSA( IPNT( 13) )
               PRH01min   = PMSA( IPNT( 14) )
               NH4crEM01  = PMSA( IPNT( 15) ) + 1.0e-20
               NO3crEM01  = PMSA( IPNT( 16) ) + 1.0e-20
               PO4crEM01  = PMSA( IPNT( 17) ) + 1.0e-20
               CO2crSM01  = PMSA( IPNT( 18) ) + 1.0e-20
               NH4        = MAX(PMSA( IPNT( 19) ),0.0)
               NO3        = MAX(PMSA( IPNT( 20) ),0.0)
               PO4        = MAX(PMSA( IPNT( 21) ),0.0)
               LimNutSM01 = PMSA( IPNT( 22) )
               LimRadSM01 = PMSA( IPNT( 23) )
               MinDLEM01  = PMSA( IPNT( 24) )
               OptDLEM01  = PMSA( IPNT( 25) )
               MinDLSM01  = PMSA( IPNT( 26) )
               OptDLSM01  = PMSA( IPNT( 27) )
               DayL       = PMSA( IPNT( 28) )
               TcritEM01  = PMSA( IPNT( 29) )
               TcPMxEM01  = PMSA( IPNT( 30) )
               TcritSM01  = PMSA( IPNT( 31) )
               TcPMxSM01  = PMSA( IPNT( 32) )

               !
               ! Use the vertically averaged temperature (prepared by MACNUT)
               !
               Temp       = PMSA( IPNT( 33) )

               K1DecaEM01 = PMSA( IPNT( 34) )
               TcDecaEM01 = PMSA( IPNT( 35) )
               K1DecaSM01 = PMSA( IPNT( 36) )
               TcDecaSM01 = PMSA( IPNT( 37) )
               FrEMtoRH01 = PMSA( IPNT( 38) )
               FrSMtoRH01 = PMSA( IPNT( 39) )
               NCRatEM01  = PMSA( IPNT( 40) )
               PCRatEM01  = PMSA( IPNT( 41) )
               NCRatSM01  = PMSA( IPNT( 42) )
               PCRatSM01  = PMSA( IPNT( 43) )
               NCRatRH01  = PMSA( IPNT( 44) )
               PCRatRH01  = PMSA( IPNT( 45) )
               FrPOC1EM01 = PMSA( IPNT( 46) )
               FrPOC2EM01 = PMSA( IPNT( 47) )
               FrPOC3EM01 = PMSA( IPNT( 48) )
               FrPOC1SM01 = PMSA( IPNT( 49) )
               FrPOC2SM01 = PMSA( IPNT( 50) )
               FrPOC3SM01 = PMSA( IPNT( 51) )
               Surf       = PMSA( IPNT( 52) )
               DELT       = PMSA( IPNT( 53) )
               Depth      = PMSA( IPNT( 54) )

               ! check input

               IF ( SURF .LT. 1E-20 ) THEN
                  NR_MSG = NR_MSG + 1
                  CALL GETMLU( LUNREP )
                  IF ( NR_MSG <= 25 ) THEN
                     WRITE(LUNREP,*) 'MACROP: WARNING - surface zero or negative'
                     WRITE(LUNREP,*) '   Segment:', ISEG
                     WRITE(LUNREP,*) '   Surface:', SURF
                     IF ( NR_MSG == 25 ) THEN
                        WRITE(LUNREP,*) 'MACROP: 25 warnings written - further warnings suppressed'
                     ENDIF
                  ENDIF
               ENDIF
               IF ( DEPTH .LT. 1E-20 ) THEN
                  NR_MSG = NR_MSG + 1
                  CALL GETMLU( LUNREP )
                  IF ( NR_MSG <= 25 ) THEN
                     WRITE(LUNREP,*) 'MACROP: WARNING - depth zero or negative'
                     WRITE(LUNREP,*) '   Segment:', ISEG
                     WRITE(LUNREP,*) '   Depth:'  , DEPTH
                     IF ( NR_MSG == 25 ) THEN
                        WRITE(LUNREP,*) 'MACROP: 25 warnings written - further warnings suppressed'
                     ENDIF
                  ENDIF
               ENDIF

               ! fracties poc

               SUMFR = FrPOC1EM01+FrPOC2EM01+FrPOC3EM01
               IF ( ABS(SUMFR-1.0) > 0.01 ) THEN
                  NR_MSG = NR_MSG + 1
                  CALL GETMLU( LUNREP )
                  IF ( NR_MSG <= 25 ) THEN
                     WRITE(LUNREP,*) 'MACROP: WARNING - sum of fractions for POC (emerged) not close to 1'
                     WRITE(LUNREP,*) '   Segment:',          ISEG
                     WRITE(LUNREP,*) '   Sum of fractions:', SUMFR
                     IF ( NR_MSG == 25 ) THEN
                        WRITE(LUNREP,*) 'MACROP: 25 warnings written - further warnings suppressed'
                     ENDIF
                  ENDIF
               ENDIF
               FrPOC2EM01 = FrPOC2EM01 / SUMFR
               FrPOC3EM01 = FrPOC3EM01 / SUMFR
               FrPOC1EM01 = 1.0 - FrPOC2EM01 - FrPOC3EM01

               SUMFR = FrPOC1SM01+FrPOC2SM01+FrPOC3SM01
               IF ( ABS(SUMFR-1.0) > 0.01 ) THEN
                  NR_MSG = NR_MSG + 1
                  CALL GETMLU( LUNREP )
                  IF ( NR_MSG <= 25 ) THEN
                     WRITE(LUNREP,*) 'MACROP: WARNING - sum of fractions for POC (submerged) not close to 1'
                     WRITE(LUNREP,*) '   Segment:',          ISEG
                     WRITE(LUNREP,*) '   Sum of fractions:', SUMFR
                     IF ( NR_MSG == 25 ) THEN
                        WRITE(LUNREP,*) 'MACROP: 25 warnings written - further warnings suppressed'
                     ENDIF
                  ENDIF
               ENDIF
               FrPOC2SM01 = FrPOC2SM01 / SUMFR
               FrPOC3SM01 = FrPOC3SM01 / SUMFR
               FrPOC1SM01 = 1.0 - FrPOC2SM01 - FrPOC3SM01

               ! biomasses to m2

               EM01M2     = EM01
               SM01M2     = SM01
               RH01M2     = RH01
               NRH01M2    = NRH01
               PRH01M2    = PRH01

               ! nutrient limitation only for EM01

               LimNH4EM01 = MIN(NH4/NH4crEM01,1.0)
               LimNO3EM01 = MIN(NO3/NO3crEM01,1.0)
               LimPO4EM01 = MIN(PO4/PO4crEM01,1.0)
               LimNutEM01 = MIN(MAX(LimNH4EM01,LimNO3EM01),LimPO4EM01)

               LimCO2SM01 = 1.0 ! TODO: CO2 limitation is not implemented yet

               ! daylength limitation EM01 and SM01

               IF ( DayL .lt. MinDLEM01 ) THEN
                  LimDLEM01  = 0.0
               ELSEIF ( DayL .lt. OptDLEM01 ) THEN
                  LimDLEM01  = (Dayl - MinDLEM01)/(OptDLEM01 - MinDLEM01)
               ELSE
                  LimDLEM01  = 1.0
               ENDIF

               IF ( DayL .lt. MinDLSM01 ) THEN
                  LimDLSM01  = 0.0
               ELSEIF ( DayL .lt. OptDLSM01 ) THEN
                  LimDLSM01  = (Dayl - MinDLSM01)/(OptDLSM01 - MinDLSM01)
               ELSE
                  LimDLSM01  = 1.0
               ENDIF

               ! temperature limitation

               IF ( Temp .lt. TcritEM01 ) THEN
                  LimTEM01   = 0.0
               ELSE
                  LimTEM01   = TcPMxEM01**(Temp-20.)
               ENDIF

               IF ( Temp .lt. TcritSM01 ) THEN
                  LimTSM01   = 0.0
               ELSE
                  LimTSM01   = TcPMxSM01**(Temp-20.)
               ENDIF

               ! growth

               IF ( EM01M2 .LT. MaxEM01 ) THEN
                  EM01Grow   = MAX(EM01M2+RH01M2,EM01thresh)
                  dGrowEM01  = EM01Grow*PPmaxEM01*LimNutEM01*LimDLEM01*LimTEM01/Depth
               ELSE
                  dGrowEM01  = 0.0
               ENDIF

               IF ( SM01M2 .LT. MaxSM01 ) THEN
                  SM01Grow   = MAX(SM01M2+RH01M2,SM01thresh)
                  dGrowSM01  = SM01Grow*PPmaxSM01*LimRadSM01*LimNutSM01*LimDLSM01*LimTSM01/Depth
               ELSE
                  dGrowSM01  = 0.0
               ENDIF

               ! decay

               dDecayEM01 = EM01M2*K1DecaEM01*(1.0-LimDLEM01)*TcDecaEM01**(Temp-20.)/Depth
               dDecaySM01 = SM01M2*K1DecaSM01*(1.0-LimDLSM01)*TcDecaSM01**(Temp-20.)/Depth

               ! growth of rhizomes

               dCtEMtRH01 = dDecayEM01 * FrEMtoRH01
               dCtSMtRH01 = dDecaySM01 * FrSMtoRH01
               dNtEMtRH01 = dCtEMtRH01 * MIN(NCRatRH01,NCRatEM01)
               dNtSMtRH01 = dCtSMtRH01 * MIN(NCRatRH01,NCRatSM01)
               dPtEMtRH01 = dCtEMtRH01 * MIN(PCRatRH01,PCRatEM01)
               dPtSMtRH01 = dCtSMtRH01 * MIN(PCRatRH01,PCRatSM01)

               ! production organic material

               factorE    = dDecayEM01-dCtEMtRH01
               factorS    = dDecaySM01-dCtSMtRH01
               dPrPOC1M01 = factorE*FrPOC1EM01 + factorS*FrPOC1SM01
               dPrPOC2M01 = factorE*FrPOC2EM01 + factorS*FrPOC2SM01
               dPrPOC3M01 = factorE*FrPOC3EM01 + factorS*FrPOC3SM01

               factorE    = dDecayEM01*NCRatEM01-dNtEMtRH01
               factorS    = dDecaySM01*NCRatSM01-dNtSMtRH01
               dPrPON1M01 = factorE*FrPOC1EM01 + factorS*FrPOC1SM01
               dPrPON2M01 = factorE*FrPOC2EM01 + factorS*FrPOC2SM01
               dPrPON3M01 = factorE*FrPOC3EM01 + factorS*FrPOC3SM01

               factorE    = dDecayEM01*PCRatEM01-dPtEMtRH01
               factorS    = dDecaySM01*PCRatSM01-dPtSMtRH01
               dPrPOP1M01 = factorE*FrPOC1EM01 + factorS*FrPOC1SM01
               dPrPOP2M01 = factorE*FrPOC2EM01 + factorS*FrPOC2SM01
               dPrPOP3M01 = factorE*FrPOC3EM01 + factorS*FrPOC3SM01

               ! uptake from rhizomes

               Cuptake = (dGrowEM01+dGrowSM01)*Delt*Depth
               IF ( Cuptake .LT. RH01M2-RH01MIN ) THEN
                  dCtRHtEM01 = dGrowEM01
                  dCtRHtSM01 = dGrowSM01
               ELSE
                  dCtRHtEM01 = 0.0
                  dCtRHtSM01 = 0.0
               ENDIF

               Nuptake = (dGrowEM01*NCratEM01+dGrowSM01*NCratSM01)*Delt*Depth
               IF ( Nuptake .LT. NRH01M2-NRH01MIN ) THEN
                  dNtRHtEM01 = dGrowEM01 * NCratEM01
                  dNtRHtSM01 = dGrowSM01 * NCratSM01
               ELSE
                  dNtRHtEM01 = 0.0
                  dNtRHtSM01 = 0.0
               ENDIF

               Puptake = (dGrowEM01*PCratEM01+dGrowSM01*PCratSM01)*Delt*Depth
               IF ( Puptake .LT. PRH01M2-PRH01MIN ) THEN
                  dPtRHtEM01 = dGrowEM01 * PCratEM01
                  dPtRHtSM01 = dGrowSM01 * PCratSM01
               ELSE
                  dPtRHtEM01 = 0.0
                  dPtRHtSM01 = 0.0
               ENDIF

               ! uptake from water

               IF ( NH4/NH4crEM01 .GE. 1.0 ) THEN
                  FrNH4EM01 = 1.0
                  FrNO3EM01 = 0.0
               ELSE
                  IF ( (NH4+NO3) .GT. 1E-20 ) THEN
                     FrNH4EM01 = NH4/(NH4+NO3)
                     FrNO3EM01 = 1.0 - FrNH4EM01
                  ELSE
                     FrNH4EM01 = 0.5
                     FrNO3EM01 = 0.5
                  ENDIF
               ENDIF

               factorE    = dGrowEM01 * NCratEM01 - dNtRHtEM01
               dNH4upEM01 = factorE * FrNH4EM01
               dNO3upEM01 = factorE * FrNO3EM01
               dPO4upEM01 = dGrowEM01 * PCratEM01 - dPtRHtEM01

               ! uptake from sediment

               dNupSM01   = dGrowSM01 * NCratSM01 - dNtRHtSM01
               dPupSM01   = dGrowSM01 * PCratSM01 - dPtRHtSM01

               ! oxygen and CO2

               dSM01OXY   = dGrowSM01 - dCtRHtSM01
               dSM01CO2   = dGrowSM01 - dCtRHtSM01

               FL  ( IdGrowEM01  ) = dGrowEM01
               FL  ( IdGrowSM01  ) = dGrowSM01
               FL  ( IdDecayEM01 ) = dDecayEM01
               FL  ( IdDecaySM01 ) = dDecaySM01
               FL  ( IdCtEMtRH01 ) = dCtEMtRH01
               FL  ( IdCtSMtRH01 ) = dCtSMtRH01
               FL  ( IdCtRHtEM01 ) = dCtRHtEM01
               FL  ( IdCtRHtSM01 ) = dCtRHtSM01
               FL  ( IdNtEMtRH01 ) = dNtEMtRH01
               FL  ( IdNtSMtRH01 ) = dNtSMtRH01
               FL  ( IdNtRHtEM01 ) = dNtRHtEM01
               FL  ( IdNtRHtSM01 ) = dNtRHtSM01
               FL  ( IdPtEMtRH01 ) = dPtEMtRH01
               FL  ( IdPtSMtRH01 ) = dPtSMtRH01
               FL  ( IdPtRHtEM01 ) = dPtRHtEM01
               FL  ( IdPtRHtSM01 ) = dPtRHtSM01
               FL  ( IdNH4upEM01 ) = dNH4upEM01
               FL  ( IdNO3upEM01 ) = dNO3upEM01
               FL  ( IdPO4upEM01 ) = dPO4upEM01
               FL  ( IdNupSM01   ) = dNupSM01
               FL  ( IdPupSM01   ) = dPupSM01
               FL  ( IdPrPOC1M01 ) = dPrPOC1M01
               FL  ( IdPrPOC2M01 ) = dPrPOC2M01
               FL  ( IdPrPOC3M01 ) = dPrPOC3M01
               FL  ( IdPrPON1M01 ) = dPrPON1M01
               FL  ( IdPrPON2M01 ) = dPrPON2M01
               FL  ( IdPrPON3M01 ) = dPrPON3M01
               FL  ( IdPrPOP1M01 ) = dPrPOP1M01
               FL  ( IdPrPOP2M01 ) = dPrPOP2M01
               FL  ( IdPrPOP3M01 ) = dPrPOP3M01
               FL  ( IdSM01OXY   ) = dSM01OXY
               FL  ( IdSM01CO2   ) = dSM01CO2
               PMSA( IPNT( 55)   ) = EM01M2
               PMSA( IPNT( 56)   ) = SM01M2
               PMSA( IPNT( 57)   ) = RH01M2
               PMSA( IPNT( 58)   ) = LimNH4EM01
               PMSA( IPNT( 59)   ) = LimNO3EM01
               PMSA( IPNT( 60)   ) = LimPO4EM01
               PMSA( IPNT( 61)   ) = LimNutEM01
               PMSA( IPNT( 62)   ) = LimCO2SM01
               PMSA( IPNT( 63)   ) = LimDLEM01
               PMSA( IPNT( 64)   ) = LimDLSM01
               PMSA( IPNT( 65)   ) = LimTEM01
               PMSA( IPNT( 66)   ) = LimTSM01
            ENDIF
         ENDIF

         IdGrowEM01  = IdGrowEM01  + NOFLUX
         IdGrowSM01  = IdGrowSM01  + NOFLUX
         IdDecayEM01 = IdDecayEM01 + NOFLUX
         IdDecaySM01 = IdDecaySM01 + NOFLUX
         IdCtEMtRH01 = IdCtEMtRH01 + NOFLUX
         IdCtSMtRH01 = IdCtSMtRH01 + NOFLUX
         IdCtRHtEM01 = IdCtRHtEM01 + NOFLUX
         IdCtRHtSM01 = IdCtRHtSM01 + NOFLUX
         IdNtEMtRH01 = IdNtEMtRH01 + NOFLUX
         IdNtSMtRH01 = IdNtSMtRH01 + NOFLUX
         IdNtRHtEM01 = IdNtRHtEM01 + NOFLUX
         IdNtRHtSM01 = IdNtRHtSM01 + NOFLUX
         IdPtEMtRH01 = IdPtEMtRH01 + NOFLUX
         IdPtSMtRH01 = IdPtSMtRH01 + NOFLUX
         IdPtRHtEM01 = IdPtRHtEM01 + NOFLUX
         IdPtRHtSM01 = IdPtRHtSM01 + NOFLUX
         IdNH4upEM01 = IdNH4upEM01 + NOFLUX
         IdNO3upEM01 = IdNO3upEM01 + NOFLUX
         IdPO4upEM01 = IdPO4upEM01 + NOFLUX
         IdNupSM01   = IdNupSM01   + NOFLUX
         IdPupSM01   = IdPupSM01   + NOFLUX
         IdPrPOC1M01 = IdPrPOC1M01 + NOFLUX
         IdPrPOC2M01 = IdPrPOC2M01 + NOFLUX
         IdPrPOC3M01 = IdPrPOC3M01 + NOFLUX
         IdPrPON1M01 = IdPrPON1M01 + NOFLUX
         IdPrPON2M01 = IdPrPON2M01 + NOFLUX
         IdPrPON3M01 = IdPrPON3M01 + NOFLUX
         IdPrPOP1M01 = IdPrPOP1M01 + NOFLUX
         IdPrPOP2M01 = IdPrPOP2M01 + NOFLUX
         IdPrPOP3M01 = IdPrPOP3M01 + NOFLUX
         IdSM01OXY   = IdSM01OXY   + NOFLUX
         IdSM01CO2   = IdSM01CO2   + NOFLUX

         IPNT        = IPNT        + INCREM

 9000 CONTINUE

      RETURN
      END

      end module m_macrop
