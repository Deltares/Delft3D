!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------

 !  Deze source-code bevat de vier onderdelen uit de notitie
!  over de koppeling CAPSIM-SOBEK-RR
!
!  Ab Veldhuizen
!  30 juli 1998
!  21 april 1999

!  convert to F90 - Geert Prinsen, Oct 2008
!  double precision Oct 2012
!
! Benodigde invoer
!
! Onderdeel:
!



!
      SUBROUTINE SIMGRO_OVZ (DEBUG_UNIT, DEBUG_FILE, MESSAGE_UNIT, MESSAGE_FILE, STATUS, &
                             NXSPUN, NXTE, NXRZ, NXDPUN, NXFRSW,                         &
                             NS, NT, DPIN, DPROOT, DT, PN, VMRZIN, FMEVPT,               &
                             SRRZ, FMCA, SCSA,                                           &
                             DPRZUN, DPGWUN, NUDPUN, DPFRSW, FRSW, FREV,                 &
                             FMEVAC, FMPE, VMRZAC)

      use globals
                             !In/Out
      INTEGER      DEBUG_UNIT
      INTEGER      MESSAGE_UNIT
      INTEGER      STATUS
      CHARACTER(len=*) debug_file,message_file
!In
      INTEGER       NXTE               ! Maxumum number of land use forms
      INTEGER       NXSPUN             ! Maximum number of soil physical units
      INTEGER       NXRZ               ! Maximum number of root zones in table
      INTEGER       NXDPUN             ! Maximum number of groundwater depths in table
      INTEGER       NXFRSW             ! Maximum number of data in inundation table
      INTEGER       NT                 ! Gewasnummer
      INTEGER       NS                 ! Soil physical unit number
      Double Precision          DPIN               ! Diepte grondwater initieel (m-gl)
      Double Precision          DPROOT             ! Root zone depth (m)
      Double Precision          DT                 ! Time step (d)
      Double Precision          PN                 ! Net precipitation (m/d)
      Double Precision          VMRZIN             ! Initial moisture content root zone (m)
      Double Precision          FMEVPT             ! Potential evapotranspiration (m/d)
      INTEGER       NUDPUN(nxspun,nxrz)        ! number of groundwater depth data in table
      Double Precision          SRRZ(nxspun,nxrz,nxdpun)   ! Storage root zone table (m)
      Double Precision          FMCA(nxspun,nxrz,nxdpun)   ! Capillary rise table (m/d)
      Double Precision          SCSA(nxspun,nxrz,nxdpun)   ! Storage coefficient table (m)
      Double Precision          DPRZUN(nxrz)               ! Root zone depths in table (cm)
      Double Precision          DPGWUN(nxspun,nxrz,nxdpun) ! Groundwater depths in table (m-gl)
      Double Precision          DPFRSW(nxfrsw)     ! Depth of groundwater in inundation table (m-gl)
      Double Precision          FRSW(nxfrsw)       ! Inundation table (-)
      Double Precision          FREV(nxspun,nxte,5)! Evapotranspiration curves (-)
! Out
      Double Precision          FMEVAC             ! Actual evapotranspiration (m/d)
      Double Precision          FMPE               ! Percolation (m/d)
      Double Precision          VMRZAC             ! Actual storage in root zone (m)
!
! Local variables
!
      INTEGER     ib,id
      Double Precision        de
      LOGICAL     ISSW
      Double Precision  ::    Frrdca = 0.0d0
      Double Precision        DTGW
      Double Precision        ICSELO
      Double Precision        VMRZPT
      Double Precision        vmrzeq0
      Double Precision        vmrzeq1
      Double Precision        vmrzeq2
      Double Precision        vmrzeq10
      Double Precision        vmrzeq
      Double Precision        frpe,frtp
      Double Precision        frswtp
      Double Precision        dptp
      Double Precision        FMEVPTSW           ! Potential evapotranspiration surface water (m/d)
      INTEGER     ios
!
! Functions
!
      Double Precision ActualEvaporation
      Double Precision Unsa
!
! Common blocks
!
      integer nute
      integer nuspun
      integer nurz
      integer nufrsw
      common /NU_CMN/ nute, nurz, nuspun, nufrsw
!
      status = 0
      ib = message_unit
      id = debug_unit
      IF (id .gt. 0) THEN
         OPEN (newunit=id,FILE=debug_file,STATUS='unknown',ACCESS='append',IOSTAT=ios)
         if (ios.ne.0) then
            CALL OPENMESG (ib,message_file,status)
            write(ib,'(1x,2a)') 'Fout bij openen ', debug_file(1:len_trim(debug_file))
            Call CloseGP (ib)
!            status = status + 1
            GOTO 3900
         endif
         maxFileUnitNumber = max(maxFileUnitNumber, id)
         minFileUnitNumber = min(minFileUnitNumber, id)
         debug_unit = id
      ENDIF
!
! ***************************************************************
! *
! ************************* 1111111111111111 ***************************
!
!              Calculations of the actual evapotranspiration
!
!              For explanation of the methode see fig 6, pag 40 of the
!              user manual of SIMGRO
!                 BETA = Reduction factor for actual evaporation
!                        depending on soil moisture
!
!              According to the SWAP-method a 5 mm reduction point and
!              a 1 mm reduction point are given frev(,,3) and frev(,,4)
!              With the fmevpttte() the interpolation point is determined
!              linearly
!
! **    In:  fmevptte = potentiele verdamping
! **         vmrzac     = het actuele vochtgehalte van de wortelzone
! **         vmrzeq0    = maximale vochtvoorraad in wortelzone (bij grw op 1.0 m-mv)
! **         frev       = tabel met waarden voor knikpunten uit de grafiek (zie manual),
! **                    afhankelijk van gewas en bodem (tabellen kunnen wij aanmaken)
! **    Uit: fmevac     = actuele verdamping
!
      if (debug_unit .gt. 0) then
         write(debug_unit, '(1x,a)') 'Start SIMGRO_OVZ'
      endif
      IF (nt .LT. 0 .OR. nt .GT. nute) THEN
         status = status + 1
         CALL OPENMESG (ib,message_file,status)
         WRITE (ib,555) 'nt = land use type',nt,nute
         Call CloseGP (ib)
      ENDIF
      IF (ns .LT. 0 .OR. ns .GT. nuspun) THEN
         status = status + 1
         CALL OPENMESG (ib,message_file,status)
         WRITE (ib,555) 'ns = soil physical unit',ns,nuspun
         Call CloseGP (ib)
      ENDIF
      issw = .FALSE.
      icselo = 3
      dtgw = dt
      dptp = dpin
      frswtp = 0. ! Geen inundatie
!
      if (debug_unit .gt. 0) then
         write(debug_unit, '(1x,a)') 'Calculate eq.moisture 0'
      endif
!
      vmrzeq1 = unsa (ns,nt,dproot,dproot,srrz,              &
                      debug_unit, message_unit, status,      &
                      nxte, nxspun, nxrz, nxdpun,            &
                      nurz, dprzun, dpgwun, nudpun)
!
      if (debug_unit .gt. 0) then
         write(debug_unit, '(1x,a)') 'End calculate eq.moisture 0'
      endif
!
      fmevptsw = fmevpt
      if (issw) then
!
!        Oppervlaktewater
!
         de = fmevpt
      else
!
!        Geen oppervlaktewater
!
         if (debug_unit .gt. 0) then
            write(debug_unit, '(1x,a)') 'Calculate actual evap.'
         endif
!
         if (vmrzeq1 > 0.0d0) then
            frtp = vmrzin/vmrzeq1
            IF (frtp.GT.1.) THEN
               vmrzeq0 = unsa (ns,nt,0.d0,dproot,srrz,  &
                               debug_unit, message_unit, status,     &
                               nxte, nxspun, nxrz, nxdpun,           &
                               nurz, dprzun, dpgwun, nudpun)

               if (vmrzeq0 > 0.0d0) then
                  frtp = vmrzin/vmrzeq0
                  frtp = MAX(frtp,frev(ns,nt,3))
               else
                  frtp = 0.0d0
               endif
            ENDIF

         else
            frtp = 0.0d0
         endif
   !
         de = ActualEvaporation(fmevpt,        &
                                 frev(ns,nt,1),  &
                                 frev(ns,nt,2),  &
                                 frev(ns,nt,3),  &
                                 frev(ns,nt,4),  &
                                 frev(ns,nt,5),  &
                                 frtp,           &
                                 issw,           &
                                 fmevptsw,       &
                                 frswtp,icselo)
      endif
!
      if (debug_unit .gt. 0) then
         write(debug_unit, '(1x,a)') 'End calculate actual evap.'
      endif
!
      fmevac = de
!
! ******************** 2a2a2a2a2a2a2a2a2a ***************************
!
!            Berekening inhoud wortelzone
!
!      Initialisatie: inhoud wortelzone moet bij begin berekening
!                     op een reele waarde worden gezet, bijvoorbeeld
!                     opgegeven door gebruiker, anders het
!                     evenwichtsvochtgehalte bij grondwaterstand (nooit >)
!
!      2 fasen in berekening: potentiele en actuele inhoud wortelzone
!      Berekening potentiele inhoud:
!          In:  toestand vorige tijdstap
!               inkomende flux aan bovenkant (neerslag-interceptie-runoff-verdamping)
!          Uit: potentiele inhoud (vmrzpt)
!
       vmrzpt = vmrzin + (pn -fmevac)* dtgw
!
!
!  Benodigd:
!  - toestand vorige tijdstap vmrzin in m
!  - netto neerslag pn in m/d
!
!
!          Is de potentiele inhoud groter dan evenwichtsvochtgehalte dan
!          percolatie, anders capillaire opstijging, dit wordt berekend
!          door de connector capillaire opstijging/percolatie
!          deze is dus nu aan de beurt.
!
! *************** 333333333333333333333 ***************************
!
!       berekening capillaire opstijging of percolatie
!
!        In: potentiele inhoud wortelzone
!            grondwaterstand in meters -mv
!            vmrzeq10 = evenwichtsvochtgehalte bij grondwaterstand 10.0 m -mv
!            fmpe     = maximale capillaire opstijging
!
!        Uit:fmpe = capillaire opstijging/percolatie
!
      if (debug_unit .gt. 0) then
         write(debug_unit, '(1x,a)') 'Calculate eq.moisture dp'
      endif
!
      vmrzeq = unsa (ns,nt,dptp,dproot,srrz, &
                     DEBUG_UNIT, MESSAGE_UNIT, STATUS,    &
                     NXTE, NXSPUN, NXRZ, NXDPUN,          &
                     NURZ, DPRZUN, DPGWUN, NUDPUN)
!
      if (debug_unit .gt. 0) then
         write(debug_unit, '(1x,a)') 'End calculate eq.moisture dp'
      endif
!
!
!      Berekening capillaire opstijging/percolatie, alsmede 2b (inhoud wortelzone, moet uit elkaar)
!      In vtt = potentiele inhoud wortelzone
!         vmrzeq = evenwichtsvochtgehalte bij gws
!         dtgw  = grondwatertijdstap
!
      IF ( vmrzpt .GE. vmrzeq ) THEN
!                 Percolation as excess moisture -  FMPE neg
         IF ( dptp.LT.(2.d0+dproot) ) THEN
            fmpe = ( vmrzeq - vmrzpt ) / dtgw
         ELSE
!                 It is assumed that the equilibrium moisture content of
!                 the root zone stabilizes if the groundwater level becomes
!                 lower than 2.+dproot m-gl.
            vmrzeq2 = unsa (ns,nt,2.d0+dproot,dproot,srrz, &
                            debug_unit, message_unit, status,   &
                            nxte, nxspun, nxrz, nxdpun,        &
                            nurz, dprzun, dpgwun, nudpun)
            IF (vmrzeq2.GT.vmrzpt) THEN
               fmpe = 0.
            ELSE
               fmpe = ( vmrzeq2 - vmrzpt ) / dtgw
            ENDIF
         ENDIF
         vmrzac = vmrzpt + (dtgw*fmpe)
!
      ELSE
!
!                 Capillary rise  -  FMPE pos
!
         if (debug_unit .gt. 0) then
            write(debug_unit, '(1x,a)') 'Calculate eq.moisture 10'
         endif
!
         vmrzeq10 = unsa (ns,nt,10.0d0,dproot,srrz,           &
                          debug_unit, message_unit, status, &
                          nxte, nxspun, nxrz, nxdpun,       &
                          nurz, dprzun, dpgwun, nudpun)
!
         if (debug_unit .gt. 0) then
            write(debug_unit, '(1x,a)') 'Calculate capill. rise'
         endif
!
         fmpe = unsa (ns,nt,dptp,dproot,fmca,            &
                      debug_unit, message_unit, status,  &
                      nxte, nxspun, nxrz, nxdpun,        &
                      nurz, dprzun, dpgwun, nudpun)
!
         if (debug_unit .gt. 0) then
            write(debug_unit, '(1x,a)') 'End capillary rise'
         endif
!
!              Reduce the capillary rise in case of a small deficit
!              in the root zone (Paul van Walsum)
!
         IF (vmrzeq .GT. vmrzeq10+1.d-4 .AND.  vmrzin.GT.vmrzeq10+1.d-4) THEN
            frpe = (vmrzeq-vmrzin) / (vmrzeq-vmrzeq10)
            frpe = MAX(frpe,0.d0)
            fmpe = SQRT(frpe)*fmpe
         ENDIF
         vmrzac = vmrzpt + dtgw*fmpe
!
         IF ( vmrzac .GT. vmrzeq ) THEN
!                    Reduce capillary rise because moisture content
!                    exceeds equilibrium
            fmpe = ( (1.0d0-frrdca)*(vmrzeq - vmrzpt) ) / dtgw
            vmrzac = vmrzpt + dtgw * fmpe
         ENDIF
      ENDIF
!
      if (debug_unit .gt. 0) then
         write(debug_unit, '(1x,a)') 'End SIMGRO_OVZ'
         write(debug_unit,*)
      endif
!
      IF (id .ne. 0) Call CloseGP (id)
!
 3900 return
!
  555 FORMAT (/'Argument ',A,' out of range; is ',I4,' but should be 1 <= n <= ',I3)
!
      end


!
!  SUBPROGRAM:
!     ActualEvaporation
!     IJke van Randen
!
!  SYNOPSIS:
!       ActualEvaporation(fmevptte, frev1, frev2, frev3,
!    &                                frev4, frev5, epf,
!    &                                IsSurfaceWater,
!    &                                fmevptsw, frswtp, icselo)
!     IN: Double Precision     fmevptte
!     IN: Double Precision     frev1
!     IN: Double Precision     frev2
!     IN: Double Precision     frev3
!     IN: Double Precision     frev4
!     IN: Double Precision     frev5
!     IN: logical  IsSurfaceWater
!     IN: logical  IsSummer
!     IN: Double Precision     fmevptsw
!     IN: Double Precision     frswtp
!     IN: integer  icselo
!
!  DESCRIPTION:
!
!     Deze functie berekent de actuele verdamping. De verdamping wordt
!     berekend per landgebruik. Als het landgebruik open water is wordt geen correctie
!     factor voor de hoeveelheid vocht in de onverzadigde zone berekend. De potentiele
!     verdamping is dan de actuele.
!     Bij andere landgebruiken kan nog worden gecorrigeerd voor inundatie.
!
!  HISTORY:
!     17-aug-98 : IJke
!                 Initiele functie afgeleid van deel qundat (gemaakt door Dr Simgro)
!
!
      Double Precision function ActualEvaporation(fmevptte, frev1, frev2, frev3, &
                                      frev4, frev5, epf,             &
                                      IsSurfaceWater,                &
                                      fmevptsw, frswtp,icselo)
!
! Arguments
!
      Double Precision    fmevptte                      ! Potential evaporation
      Double Precision    frev1                         ! Reductionfactor at 100 % moisture in unsatured zone
      Double Precision    frev2                         ! Reductionfactor
      Double Precision    frev3                         ! Reductionfactor at 5 mm evaporation a day
      Double Precision    frev4                         ! Reductionfactor at 1 mm evaporation a day
      Double Precision    frev5                         ! Reductionfactor at zero contents of iunsat zone
      Double Precision    epf                           ! Actuele fractie van inhoude wortelzone

      logical IsSurfaceWater                ! Is dit gewas oppervlaktewater
      Double Precision    fmevptsw                      ! Evaporation surface water
      Double Precision    frswtp                        ! Fraction surfacewater (inundation)
      Double Precision    icselo                        ! Sensitivity for water logging (4 = reed)
!
! Local variables
!
      Double Precision    e1,e2,e3,e4                   ! Points of graph for calculating redunction
      Double Precision    beta                          ! Reductionfactor
      Double Precision    de
!
! Begin
! Als het oppervlaktewater is dan is de actuele verdamping gelijk aan de
! potentiele.
!
      if (IsSurfaceWater) then
!
!         Oppervlaktewater
!
          de = fmevptte
      else
!
!        Geen oppervlaktewater.
!
         IF (fmevptte .GT. 5.0d-3 ) THEN
!
!            Potentiele verdamping mag niet groter dan 5.0e-3
!
             e3 = frev3
         ELSE IF (fmevptte .LT. 1.0d-3 ) THEN
!
!            Potentiele verdamping mag niet kleiner dan 5.0e-3
!
             e3 = frev4
         ELSE
!
!            Interpoleer tussen 1.0e-3 en 5.0e-3 (die grafiek)
!
             e3 = frev4*(5.0d-3-fmevptte)/4.0d-3 + frev3*(fmevptte-1.0d-3)/4.0d-3
         ENDIF
!
! Bereken nu de correctiefactor. Deze ligt tussen 0.0 en 1.0
!
!      epf = vmrzdcte / veq0  ! Actuele fractie van de inhoud van wortelzone
         e1 = frev1
         e2 = frev2
         e4 = frev5
         beta = 1.0d0  ! Reductie factor van potentieel naar actueel
         IF ( epf .LT. e4 ) THEN  ! VARIABELE E4 <<<====================
              beta = 0.0
         ELSEIF ( epf .LT. e3 ) THEN
              beta = (epf-e4)/(e3-e4)
         ENDIF
!
         IF ( epf .GT. (e2+0.0001d0) .AND. epf .LT. e1 ) THEN
              beta = (e1-epf)/(e1-e2)
         ENDIF
         IF ( epf .GT. (e1+0.0001d0) ) THEN
             IF (e1 .GT. 0.99d0) THEN
                beta = 1.0d0
             ELSE
                beta = 0.0
             ENDIF
         ENDIF
!
         IF ( beta .GT. 1.00d0 ) beta = 1.0d0
!
!                 The actual evapotranspirations depends on
!                 potential evapotranspiration
!                 reduction factor
!                 fraction inudation
!
          IF ( icselo.EQ.4 ) THEN
              de = beta* fmevptte
          ELSE
              de = beta* fmevptte* (1.d0-frswtp) + fmevptsw*frswtp
          ENDIF
      ENDIF
!
! End
!
      ActualEvaporation = de
      end

      Double Precision function GetRRin()
      getRRIn = -9.99d9
      end

      integer function GetIIin()
      getIIin = -999999
      end
! *----------------------------------------------------------------------
!  FILE:
!     OPENFILE.PP
!     A.A.Veldhuizen
!
!  COPYRIGHT: 1998
!     DLO Winand Staring Centre for Integrated Land, Soil
!     and Water Research (SC-DLO), P.O.Box 125,
!     NL-6700 AC Wageningen, The Netherlands.
!
!     This PROGRAM, or parts thereof, may not be reproduced,
!     modified or transferred to third parties without the
!     written permission of the DLO Winand Staring Centre.
!
!  FILE IDENTIFICATION:
!     $Id: $
!     $Log: $
!
!  DESCRIPTION:
!     This file contains the source code of SUBROUTINE OPENFILE
!     and function ISBL
!
!
! ----------------------------------------------------------------------
!  SUBPROGRAM:
!     OPENFILE
!     A.A.Veldhuizen
!
!  SYNOPSIS:
!
!  DESCRIPTION:
!     Opens input file
!
!  HISTORY:
!     may-98     A.A.Veldhuizen
!                prototype
! ----------------------------------------------------------------------
!
      SUBROUTINE openfile (nm ,filenam,ex, ib ,id)
!
      use globals
! Arguments
!
      character(len=*) filenam
      integer     nm
      logical     ex
      integer     ib
      integer     id
!
! Local variables
!
      integer ios
!
! Begin
!
      if (id.gt.0) write(id,'(1x,a)') 'Using file '//filenam(1:len_trim(filenam))
      INQUIRE (FILE=filenam,EXIST=ex)
      IF (ex) THEN
         OPEN (newunit=nm,FILE=filenam,STATUS='OLD',iostat=ios)
         if (ios.ne.0) then
            write(ib,'(1x,2a)') 'Fout bij openen ', filenam(1:len_trim(filenam))
         else
            maxFileUnitNumber = max(maxFileUnitNumber, nm)
            minFileUnitNumber = min(minFileUnitNumber, nm)
         endif
      ELSE
         WRITE (ib,23) filenam(1:len_trim(filenam))
      ENDIF
!
   23 FORMAT (/,'** E23  readvar  **',1X,' Cannot open table ',A,'.inp for read')
!
      END
!
!
! ----------------------------------------------------------------------
!  SUBPROGRAM:
!     OPENFILE
!     A.A.Veldhuizen
!
!  SYNOPSIS:
!
!  DESCRIPTION:
!     Opens input file
!
!  HISTORY:
!     may-98     A.A.Veldhuizen
!                prototype
! ----------------------------------------------------------------------
!
      SUBROUTINE openmesg (nm ,filenam, status)

      use globals
!
! Arguments
!
      character(len=*) filenam
      integer       nm
      integer       status
!
! Local variables
!
      integer       ios
!
! Begin
!
      OPEN (newunit=nm,FILE=filenam,STATUS='unknown',ACCESS='append', IOSTAT=ios)
      IF (ios .NE. 0 ) THEN
         write(*,'(1x,2a)') 'Fout bij openen ', filenam(1:len_trim(filenam))
         status = status + 1
      else
         maxFileUnitNumber = max(maxFileUnitNumber, nm)
         minFileUnitNumber = min(minFileUnitNumber, nm)
      ENDIF
!
      END
!
! ----------------------------------------------------------------------
!  SUBPROGRAM:
!     ISBL
!     A.A.Veldhuizen
!
!  SYNOPSIS:
!
!  DESCRIPTION:
!     Checks whether string is blank
!
!  HISTORY:
!     may-98     A.A.Veldhuizen
!                prototype
! ----------------------------------------------------------------------
!
      LOGICAL* 4 FUNCTION isbl (chline,bg,ed)
!
      INTEGER*4     bg,ed,ii
      CHARACTER(len=*) chline
!
      isbl = .TRUE.
      DO 1000 ii= bg,ed
         IF (chline(ii:ii).NE.' ') THEN
            isbl = .FALSE.
         ENDIF
 1000 CONTINUE
!
      END
! *----------------------------------------------------------------------
!  FILE:
!     READluse.PP
!     A.A.Veldhuizen
!
!  COPYRIGHT: 1998
!     DLO Winand Staring Centre for Integrated Land, Soil
!     and Water Research (SC-DLO), P.O.Box 125,
!     NL-6700 AC Wageningen, The Netherlands.
!
!     This PROGRAM, or parts thereof, may not be reproduced,
!     modified or transferred to third parties without the
!     written permission of the DLO Winand Staring Centre.
!
!  FILE IDENTIFICATION:
!     $Id: $
!     $Log: $
!
!  DESCRIPTION:
!     This file contains the source code of SUBROUTINE READluse
!
!
! ----------------------------------------------------------------------
!  SUBPROGRAM:
!     READluse
!     A.A.Veldhuizen
!
!  SYNOPSIS:
!
!  DESCRIPTION:
!     Reads input file luse_sim.inp
!
!  HISTORY:
!     may-98     A.A.Veldhuizen
!                prototype
! ----------------------------------------------------------------------
!
      SUBROUTINE readluse (dirnam, ib,id, nm, nxspun, nxte,icselo, frev, Status)
!
!
! Arguments
!
      integer       ib  ! Message unit
      integer       id ! debug unit
      character(len=*) dirnam
      integer       nxte
      integer       nxspun
      integer       icselo(nxte)
      Double Precision          frev(nxspun, nxte, 5)
      integer       Status
!
! Local variables
!
      logical       ex
      logical       er
      character(len=132) nafi
      character(len=132) chline
      integer       nuwa
      integer       nuer
      logical       kyer
      integer       nmli
      integer       nmlicm
      integer       nm
      integer       nt
      integer       ns
      integer       iiin
      integer       r_nmteex
      character(len=15) r_tena
      integer       r_ipfa
      integer       r_ipagte
      Double Precision          r_frpric
      integer       r_icselo
      Double Precision          RRin
!
! Functions
!
      logical isbl
      logical ctrl_int
      integer GetIIin
      Double Precision    GetRRin
!
! Init
!
      NuWa = 0
      NuEr = 0
!
! Begin
!
      iiin = GetIIin()
      RRin = GetRRin()
!      nm = 11
      nafi = dirnam(1:len_trim(dirnam)) // 'luse_sim.inp'
      if (id.gt.0) write(id,'(1x,a)') 'Start READLUSE'
      CALL openfile (nm, nafi, ex, iB,ID)
!
      IF (.NOT. ex) GOTO 3900
!
!        Initialize arrays of table TECH
!
 2400 DO 2410 nt = 1,nxte
!         tena(nt) = '               '
!         ipfa(nt) = iiin
!         frpric(nt) = rrin
         icselo(nt) = iiin
!         durospte(nt) = rrin
!         idspbgte(nt) = rrin
!         idspedte(nt) = rrin
!         frspte(nt) = rrin
!         ipagte(nt) = iiin
!         idssbgte(nt) = rrin
!         dusste(nt) = rrin
 2410 CONTINUE
!
!        Read records from LUSE
!        Check whether variable is okay
!        And put the variable in its array
!
      nmli = 0
      nmlicm = 0
!
 2455 READ (nm,501,END=2450) chline
  501 FORMAT (A132)
      IF ( chline(1:1) .EQ. '*' .OR. chline(2:2).EQ.'*') THEN
         nmlicm = nmlicm + 1
         GOTO 2455
      ELSE
         nmli = nmli + 1
         kyer = .FALSE.
!
!         READ (chline,614) r_nmteex,r_tena,r_ipfa,r_ipagte,r_frpric,
!     &   r_icselo,r_durospte,r_idspbgte,r_idspedte,r_frspte
         READ (chline,614) r_nmteex,r_tena,r_ipfa,r_ipagte,r_frpric, r_icselo
  614    FORMAT (I6,A15,5X,2I6,F8.0,I6,F8.0,2F6.0,3F8.0)

!c     &   ,r_idssbgte,dusste
!
            IF (.NOT. ctrl_int ('luse_sim',nmli+nmlicm,r_nmteex,1,999,3,'r_nmteex','external technology number',ib) ) THEN
               nuer = nuer + 1
               kyer = .TRUE.
               GOTO 3900
            ELSE
!               nmteex(nmli) = r_nmteex
               r_nmteex = nmli
            ENDIF
!
!            tena(r_nmteex) = r_tena
!
!            IF (.NOT. ctrl_val ('luse_sim',nmli+nmlicm,r_ipfa,'na',
!     &      'r_ipfa','pointer for evaporation factor') ) THEN
!               nuer = nuer + 1
!            ELSEIF (.NOT. kyer) THEN
!               ipfa(r_nmteex) = r_ipfa
!            ENDIF
!
!            IF (.NOT.ctrl_real ('luse_sim',nmli+nmlicm,r_frpric,0.,1.,1,
!     &      'r_frpric','interception fraction') ) THEN
!               nuwa = nuwa + 1
!            ELSEIF (.NOT. kyer) THEN
!               frpric(r_nmteex) = r_frpric
!            ENDIF
!
         IF (.NOT. isbl(chline,47,52) ) THEN
            IF (.NOT.ctrl_int ('luse_sim',nmli+nmlicm,r_icselo,1,4,3,'r_icselo','sensitivity for water logging',ib) ) THEN
               nuwa = nuwa +1
            ELSEIF (.NOT. kyer) THEN
               icselo(r_nmteex) = r_icselo
            ENDIF
         ENDIF
!
!         IF (.NOT. isbl(chline,53,60) ) THEN
!            IF (.NOT. ctrl_real ('luse_sim',nmli+nmlicm,r_durospte,1.,
!     &      91.,2,'r_durospte','duration of rotational period') ) THEN
!               nuwa = nuwa + 1
!            ELSEIF (.NOT. kyer) THEN
!               durospte(r_nmteex) = r_durospte
!            ENDIF
!         ENDIF
!
!         IF (.NOT. isbl(chline,61,66) ) THEN
!            IF (.NOT. ctrl_real('luse_sim',nmli+nmlicm,r_idspbgte,0.,
!     &      366.,3,'r_idspbgte','day number to begin sprinkling') ) THEN
!               nuwa = nuwa +1
!            ELSEIF (.NOT. kyer) THEN
!               idspbgte(r_nmteex) = r_idspbgte
!            ENDIF
!         ENDIF
!
!         IF (.NOT. isbl(chline,67,72) ) THEN
!            IF (.NOT. ctrl_real('luse_sim',nmli+nmlicm,r_idspedte,0.,
!     &      366.,3,'r_idspedte','day number to end sprinkling') ) THEN
!               nuwa = nuwa +1
!            ELSEIF (.NOT. kyer) THEN
!               idspedte(r_nmteex) = r_idspedte
!            ENDIF
!         ENDIF
!
!            IF (.NOT.ctrl_real ('luse_sim',nmli+nmlicm,r_frspte,0.,0.85,
!     &      3,'r_frspte','production level') ) THEN
!               nuwa = nuwa + 1
!            ELSEIF (.NOT. kyer) THEN
!               frspte(r_nmteex) = r_frspte
!            ENDIF
!
!            IF (.NOT. ctrl_int ('luse_sim',nmli+nmlicm,r_ipagte,0,6,3,
!     &      'r_ipagte','type of land use pointer') ) THEN
!               nuwa = nuwa +1
!            ELSEIF (.NOT. kyer) THEN
!               ipagte(r_nmteex) = r_ipagte
!            ENDIF
!
!         IF (.NOT. isbl(chline,89,96) ) THEN
!            IF (.NOT. ctrl_real ('luse_sim',nmli+nmlicm,r_dusste,1.,
!     &      91.,2,'r_dusste','duration of stress period') ) THEN
!               nuwa = nuwa + 1
!            ELSEIF (.NOT. kyer) THEN
!               dusste(r_nmteex) = r_dusste
!            ENDIF
!        ENDIF
!
!         IF (.NOT. isbl(chline,81,88) ) THEN
!            IF (.NOT. ctrl_real('luse_sim',nmli+nmlicm,r_idssbgte,0.,
!     &      366.,3,'r_idssbgte','day number begin stress period') ) THEN
!               nuwa = nuwa +1
!            ELSEIF (.NOT. kyer) THEN
!               idssbgte(r_nmteex) = r_idssbgte
!            ENDIF
!        ENDIF
!
      ENDIF
!
      GOTO 2455
 2450 CONTINUE
!
!        The pointers for urban area, decedious forest, pine fores and
!        surface water may not be identical, but have to exist.
!
!      artp(1)=ipub
!      artp(2)=ipdf
!      artp(3)=ippf
!      artp(4)=ipsw
!      DO 2470 ii=1,4
!         kyer = .TRUE.
!        IF ( artp(ii) .EQ. 0) kyer = .FALSE.
!         DO 2465 nt=1,nute
!            IF ( artp(ii).EQ.nmteex(nt) ) THEN
!               kyer = .FALSE.
!            ENDIF
! 2465    CONTINUE
!
!         IF (kyer) THEN
!            ncer(48) = ncer(48) + 1
!            WRITE (ib,48) artp(ii)
!            nuer = nuer + 1
!            GOTO 3900
!         ENDIF
!
 2470 CONTINUE
!
!      DO 2475 nt = 1,3
!         DO 2480 ii=nt+1,4
!            IF (artp(nt).NE.0) THEN
!               IF ( artp(nt) .EQ. artp(ii) ) THEN
!                  ncer(49) = ncer(49) + 1
!                  WRITE (ib,49) artp(nt)
!                  nuer = nuer + 1
!                  GOTO 3900
!               ENDIF
!            ENDIF
! 2480    CONTINUE
! 2475 CONTINUE
!
!      IF (ipsw .GT. 0) THEN
!         ipagte(ipsw) = 2
!      ENDIF
!      IF (ipdf .GT. 0) THEN
!         ipagte(ipdf) = 3
!      ENDIF
!      IF (ippf .GT. 0) THEN
!         ipagte(ippf) = 4
!      ENDIF
!      IF (ipub .GT. 0) THEN
!         ipagte(ipub) = 5
!      ENDIF
!      ipsw = 2
!      ipdf = 3
!      ippf = 4
!      ipub = 5
!
!        Test whether arrays are complete
!
      DO 2460 nt = 1,nxte
!         IF ( ( ipfa(nt) - iiin ) .EQ. 0 ) THEN
!            ncer(26) = ncer(26) + 1
!            WRITE (ib,26) 'luse_sim','ipfa','nt',nt
!            nuwa = nuwa + 1
!         ENDIF
!         IF ( ABS( frpric(nt) - rrin ) .LT. 1.e-4 ) THEN
!            frpric(nt) = 0.0
!c            ncer(26) = ncer(26) + 1
!c            WRITE (ib,26) 'luse_sim','frpric','nt',nt
!c            nuwa = nuwa + 1
!         ENDIF
         IF ( ( icselo(nt) - iiin ) .EQ. 0 ) THEN
            icselo(nt) = 3
!c            WRITE (ib,26) 'luse_sim','icselo','nt',nt
!c            nuwa = nuwa + 1
         ENDIF
!         IF ( ABS( durospte(nt) - rrin ) .LT. 1.e-4 ) THEN
!c            WRITE (ib,26) 'luse_sim','durosp','nt',nt
!c            nuwa = nuwa + 1
!              durospte(nt) = 0.0
!              idspbgte(nt) = 1.
!              idspedte(nt) = 1.
!              frspte(nt) = 0.0
!         ENDIF
!         IF ( ( idspbgte(nt) - rrin ) .LT. 1.e-4 ) THEN
!c            WRITE (ib,26) 'luse_sim','idspbg','nt',nt
!c            nuwa = nuwa + 1
!              durospte(nt) = 0.0
!              idspbgte(nt) = 1.
!              idspedte(nt) = 1.
!              frspte(nt) = 0.0
!         ENDIF
!         IF ( ( idspedte(nt) - rrin ) .LT. 1.e-4 ) THEN
!c            WRITE (ib,26) 'luse_sim','idsped','nt',nt
!c            nuwa = nuwa + 1
!              durospte(nt) = 0.0
!              idspbgte(nt) = 1.
!              idspedte(nt) = 1.
!              frspte(nt) = 0.0
!         ENDIF
!         IF ( ABS( frspte(nt) - rrin ) .LT. 1.e-4 ) THEN
!              durospte(nt) = 0.0
!              idspbgte(nt) = 1.
!              idspedte(nt) = 1.
!              frspte(nt) = 0.0
!c            ncer(26) = ncer(26) + 1
!c            WRITE (ib,26) 'luse_sim','frspte','nt',nt
!c            nuwa = nuwa + 1
!         ENDIF
!         IF ( ( ipagte(nt) - iiin ) .EQ. 0 ) THEN
!            ipagte(nt) = 0
!c            ncer(26) = ncer(26) + 1
!c            WRITE (ib,26) 'luse_sim','ipagte','nt',nt
!c            nuwa = nuwa + 1
!         ENDIF
!         IF ( ( idssbgte(nt) - rrin ) .LT. 1.e-4 ) THEN
!              dusste(nt) = 366.0
!              idssbgte(nt) = 366.0
!         ENDIF
!         IF ( ( dusste(nt) - rrin ) .LT. 1.e-4 ) THEN
!              dusste(nt) = 366.0
!              idssbgte(nt) = 366.0
!         ENDIF
!         IF ( ncer(26).GT.0 ) GOTO 3900
 2460 CONTINUE
!
!        Initialize array frev, frirbg, frired
!
      DO 2490 nt = 1,nxte
      DO 2492 ns = 1,nxspun
         frev(ns,nt,3) = 0.60
         frev(ns,nt,4) = 0.20
         frev(ns,nt,5) = 0.05
         IF ( icselo(nt) .EQ. 1 ) THEN
            frev(ns,nt,1) = 1.00
            frev(ns,nt,2) = 0.95
         ELSEIF (icselo(nt) .EQ. 2) THEN
            frev(ns,nt,1) = 0.95
            frev(ns,nt,2) = 0.90
         ELSE
            frev(ns,nt,1) = 1.00
            frev(ns,nt,2) = 1.00
         ENDIF
!         frirbg(ns,nt) = frspte(nt)
!         frired(ns,nt) = frspte(nt) + 0.15
 2492 CONTINUE
 2490 CONTINUE
!
!        end of read-statements
!
!        If the number of fatal errors is 1 or more, the PROGRAM will
!        be discontinued
!
 3900 IF (nuwa .GT. 0 .OR. nuer .GT.0 ) THEN
         er = .TRUE.
      ELSE
         er = .FALSE.
      ENDIF
!
      STATUS =  NUWA + NUER

      IF (ex .or. .not.ex) Call CloseGP (nm)
!

!
      END
! *----------------------------------------------------------------------
!  FILE:
!     READroot.PP
!     A.A.Veldhuizen
!
!  COPYRIGHT: 1998
!     DLO Winand Staring Centre for Integrated Land, Soil
!     and Water Research (SC-DLO), P.O.Box 125,
!     NL-6700 AC Wageningen, The Netherlands.
!
!     This PROGRAM, or parts thereof, may not be reproduced,
!     modified or transferred to third parties without the
!     written permission of the DLO Winand Staring Centre.
!
!  FILE IDENTIFICATION:
!     $Id: $
!     $Log: $
!
!  DESCRIPTION:
!     This file contains the source code of SUBROUTINE READroot
!
!
! ----------------------------------------------------------------------
!  SUBPROGRAM:
!     READroot
!     A.A.Veldhuizen
!
!  SYNOPSIS:
!
!  DESCRIPTION:
!     Reads input file root_sim.inp
!
!  HISTORY:
!     may-98     A.A.Veldhuizen
!                prototype
! ----------------------------------------------------------------------
!
      SUBROUTINE readroot (file_unit,filenam,message_unit,message_file, &
                           debug_unit,debug_file, Status, nxspun, nxte,  dprz, frev)

      use globals
!
!
! Arguments
!
      integer nxspun
      integer nxte
      Double Precision    dprz(nxspun, nxte)
      Double Precision    frev(nxspun, nxte, 5)
      integer file_unit,nm
      integer message_unit,ib
      integer debug_unit,id
      character(len=*) filenam,message_file,debug_file
      integer Status
!
! Local variables
!
      logical er
      logical ex
      integer :: nmli = 0
      integer :: nmlicm = 0
      integer :: nuwa = 0 
      integer :: nuer = 0
      character(len=132) chline
      logical kyer
      integer ns
      integer nt
      Double Precision    r_dprz
      Double Precision    r_frev01
      Double Precision    r_frev02
      Double Precision    r_frev03
      Double Precision    r_frev04
      Double Precision    r_frev05
      integer r_spun
      integer r_nmteex
      integer ii
      integer ios
!
! Functions
!
      logical ctrl_real
      logical isbl
!
! Common blocks
!
      integer nute
      integer nuspun
      integer nurz
      integer nufrsw
      common /NU_CMN/ nute, nurz, nuspun, nufrsw
!
! Init
!
      NuWa = 0
      NuEr = 0
!
      nm = file_unit
      ib = message_unit
      id = debug_unit
      OPEN (newunit=ib,FILE=message_file,STATUS='unknown',ACCESS='append',IOSTAT=ios)
      IF (ios .NE. 0 ) THEN
         write(*,'(1x,2a)') 'Fout bij openen ', message_file(1:len_trim(message_file))
         nuer = nuer + 1
         GOTO 3900
      ENDIF
      maxFileUnitNumber = max(maxFileUnitNumber, ib)
      minFileUnitNumber = min(minFileUnitNumber, ib)
      IF (id .ne. 0) THEN
         OPEN (newunit=id,FILE=debug_file,STATUS='unknown',ACCESS='append',IOSTAT=ios)
         if (ios.ne.0) then
            write(ib,'(1x,2a)') 'Fout bij openen ', debug_file(1:len_trim(debug_file))
            nuer = nuer + 1
            GOTO 3900
         endif
         maxFileUnitNumber = max(maxFileUnitNumber, id)
         minFileUnitNumber = min(minFileUnitNumber, id)
         debug_unit = id
      ENDIF
!
! Begin
!
      CALL openfile (nm,filenam,ex, ib, id)
!
      IF (.NOT. ex) THEN
         nuer = nuer + 1
         GOTO 3900
      ENDIF
!
!        Initialize arrays of table ROOT
!
! 3000 DO 3010 ns = 1,nxspun
!      DO 3020 nt = 1,nxte
!         dprz(ns,nt) = rrin
!         frss(ns,nt,1) = 0.0
!         frss(ns,nt,2) = 1.0
! 3020 CONTINUE
! 3010 CONTINUE
!
!        Read records from ROOT
!        Check whether variable is okay
!        And put the variable in its array
!
      nmli = 0
      nmlicm = 0
      nuspun = 0
      nute = 0
!
 3055 READ (nm,501,END=3050) chline
  501 FORMAT (A132)

      IF ( chline(1:1) .EQ. '*' .OR. chline(2:2).EQ.'*') THEN
         nmlicm = nmlicm + 1
         GOTO 3055
      ELSE
         nmli = nmli + 1
         kyer = .FALSE.
!
         READ (chline,618) r_spun,r_nmteex,r_dprz,r_frev01,r_frev02,r_frev03,r_frev04,r_frev05 !,r_frirbg,r_frired,
  618    FORMAT (2I6,10F8.0)

!     &   r_frss01,r_frss02
!
!            IF (.NOT. ctrl_val ('root_sim',nmli+nmlicm,r_spun,'ns',
!     &      'r_spunex','external soil physical UNIT number') ) THEN
         ns = r_spun
         IF (ns .LT. 1 .OR. ns .GT. nxspun) THEN
            WRITE (ib,28) 'soil physical units','root_sim',nxspun
   28       FORMAT (/'** E28  readvar  **',1X,' Maximum number of ',A,' exceeded in table ',A,'.inp',/,20X,&
                    ' According to input parameter maximum is: ',I5)
            nuer = nuer + 1
         ELSE
            IF (ns .GT. nuspun) nuspun = ns
         ENDIF
!
         nt = r_nmteex
         IF (nt .LT. 1 .OR. nt .GT. nxte) THEN
            nuer = nuer + 1
            WRITE (ib,28) 'land use types','root_sim',nxte
         ELSE
            IF (nt .GT. nute) nute = nt
         ENDIF
!
!               nuer = nuer + 1
!               kyer = .TRUE.
!               GOTO 3900
!            ENDIF
!
!            IF (.NOT. ctrl_val ('root_sim',nmli+nmlicm,r_nmteex,'nt',
!     &      'r_nmteex','external technology number') ) THEN
!               nuer = nuer + 1
!               kyer = .TRUE.
!               GOTO 3900
!            ENDIF
!
            IF (.NOT. ctrl_real ('root_sim',nmli+nmlicm,r_dprz,1.d-1,1.d1,2,'r_dprz','root zone depth',ib) ) THEN
               nuwa = nuwa + 1
            ELSEIF (.NOT. kyer) THEN
               dprz(r_spun,r_nmteex) = r_dprz
            ENDIF
!
         IF (.NOT. isbl(chline,21,28) ) THEN
            IF (.NOT. ctrl_real ('root_sim',nmli+nmlicm,r_frev01,0.d0,1.d0,2,'r_frev','evaporation fraction',ib) ) THEN
               nuwa = nuwa + 1
            ELSEIF (.NOT. kyer) THEN
               frev(r_spun,r_nmteex,1) = r_frev01
            ENDIF
         ENDIF
!
         IF (.NOT. isbl(chline,29,36) ) THEN
            IF (.NOT. ctrl_real ('root_sim',nmli+nmlicm,r_frev02,0.d0,1.d0,2,'r_frev','evaporation fraction',ib) ) THEN
               nuwa = nuwa + 1
            ELSEIF (.NOT. kyer) THEN
               frev(r_spun,r_nmteex,2) = r_frev02
            ENDIF
         ENDIF
!
         IF (.NOT. isbl(chline,37,44) ) THEN
            IF (.NOT. ctrl_real ('root_sim',nmli+nmlicm,r_frev03,0.d0,1.d0,2,'r_frev','evaporation fraction',ib) ) THEN
               nuwa = nuwa + 1
            ELSEIF (.NOT. kyer) THEN
               frev(r_spun,r_nmteex,3) = r_frev03
            ENDIF
         ENDIF
!
         IF (.NOT. isbl(chline,45,52) ) THEN
            IF (.NOT. ctrl_real ('root_sim',nmli+nmlicm,r_frev04,0.d0, 1.d0,2,'r_frev','evaporation fraction',ib) ) THEN
               nuwa = nuwa + 1
            ELSEIF (.NOT. kyer) THEN
               frev(r_spun,r_nmteex,4) = r_frev04
            ENDIF
         ENDIF
!
         IF (.NOT. isbl(chline,53,60) ) THEN
            IF (.NOT. ctrl_real ('root_sim',nmli+nmlicm,r_frev05,0.d0,1.d0,2,'r_frev','evaporation fraction',ib) ) THEN
               nuwa = nuwa + 1
            ELSEIF (.NOT. kyer) THEN
               frev(r_spun,r_nmteex,5) = r_frev05
            ENDIF
         ENDIF
!
!         IF (.NOT. isbl(chline,61,68) ) THEN
!            IF (.NOT. ctrl_real ('root_sim',nmli+nmlicm,r_frirbg,0.,
!     &      1.,2,'r_frirbg','relative cont. start sprinkling') ) THEN
!               nuwa = nuwa + 1
!            ELSEIF (.NOT. kyer) THEN
!               frirbg(r_spun,r_nmteex) = r_frirbg
!            ENDIF
!         ENDIF
!
!         IF (.NOT. isbl(chline,69,76) ) THEN
!            IF (.NOT. ctrl_real ('root_sim',nmli+nmlicm,r_frired,0.,
!     &      1.,2,'r_frired','relative cont. start sprinkling') ) THEN
!               nuwa = nuwa + 1
!            ELSEIF (.NOT. kyer) THEN
!               frired(r_spun,r_nmteex) = r_frired
!            ENDIF
!         ENDIF
!
!         IF (.NOT. isbl(chline,77,84) ) THEN
!            IF (.NOT. ctrl_real ('root_sim',nmli+nmlicm,r_frss01,0.,
!     &      1.,2,'r_frss','stress fraction') ) THEN
!               nuwa = nuwa + 1
!            ELSEIF (.NOT. kyer) THEN
!               frss(r_spun,r_nmteex,1) = r_frss01
!            ENDIF
!         ENDIF
!
!         IF (.NOT. isbl(chline,85,92) ) THEN
!            IF (.NOT. ctrl_real ('root_sim',nmli+nmlicm,r_frss02,0.,
!     &      1.,2,'r_frss','stress fraction') ) THEN
!               nuwa = nuwa + 1
!            ELSEIF (.NOT. kyer) THEN
!               frss(r_spun,r_nmteex,2) = r_frss02
!            ENDIF
!         ENDIF
!
      ENDIF
!
      GOTO 3055
 3050 CONTINUE
!
!        Test whether arrays are complete
!        and test whether the soil moisture fractions are consistent
!
      DO 3060 ns = 1,nxspun
      DO 3070 nt = 1,nxte
!         IF ( ABS( dprz(ns,nt) - rrin ) .LT. 1.e-4 ) THEN
!            ncer(26) = ncer(26) + 1
!            WRITE(ib,26) 'root_sim','dprz','ns',spunex(ns),'nt',
!     &      nmteex(nt)
!            nuwa = nuwa + 1
!         ENDIF
         DO 3080 ii = 1,4
            IF ( (frev(ns,nt,ii) - frev(ns,nt,ii+1)).LT.-1.d-4) THEN
!               ncer(58) = ncer(58) + 1
               WRITE (ib,58) ns,nt!spunex(ns),nmteex(nt)
   58          FORMAT (/,'** E58  readvar  **',1X, &
                         ' Inconsistency in root_sim.inp ',/,20X, &
                         ' in evapotranspiration or irrigation fractions',/,20X, &
                         ' for soil physical unit ',I5,' and land use ',I5)
            ENDIF
 3080    CONTINUE
!         IF ( (frired(ns,nt)-frirbg(ns,nt)) .LT. -1.e-4 ) THEN
!            ncer(58) = ncer(58) + 1
!            WRITE (ib,58) spunex(ns),nmteex(nt)
!         ENDIF
!         IF ( (frss(ns,nt,2)-frss(ns,nt,1)) .LT. -1.e-4 ) THEN
!            ncer(58) = ncer(58) + 1
!            WRITE (ib,58) spunex(ns),nmteex(nt)
!         ENDIF
 3070 CONTINUE
 3060 CONTINUE
!
!        Complete array dprzndte() <<<*********************************************
!
!      DO 3090 nd = nundbd+1,nund
!      ns = spun(nd)
!      DO 3095 nt = 1,nute
!         IF (frarte(nd,nt).GT.1.e-4 .AND. dprzndte(nd,nt).LT.1.e-4) THEN
!            dprzndte(nd,nt) = dprz(ns,nt)
!         ENDIF
! 3095 CONTINUE
! 3090 CONTINUE

!
!        Put FRSW and SCSA together per subregion
!
!      DO 2590 nd = nundbd+1,nund
!
!         DO 2592 ii = 1,nuscsa
!         IF (ii.LE.nufrsw(nd)) THEN
!            dptp = dpfrsw(nd,ii)
!         ELSE
!            dptp = dpfrsw(nd,nufrsw(nd)) + (ii-nufrsw(nd)) * .10
!         ENDIF
!
!         sctt = 0.0
!         scpltt = 0.0
!
!         DO 2594 nt = 1,nute
!            IF (frarte(nd,nt) .GT. 1.e-4) THEN
!               CALL unsa (nd,nt,dptp,va01,va02,va03,sctp,scpltp)
!               sctt = sctt + frarte(nd,nt)*sctp
!               scpltt = scpltt + frarte(nd,nt)*scpltp
!            ENDIF
! 2594    CONTINUE
!
!         IF (sctt.GT.1.e-4) THEN
!            sctt = sctt
!         ELSE
!            sctt = 0.0
!         ENDIF
!
!         IF (scpltt.GT.1.e-4) THEN
!            scpltt = scpltt
!         ELSE
!            scpltt = 0.0
!         ENDIF
!
!         IF (ii.LE.nufrsw(nd)) THEN
!            sctt = sctt + frsw(nd,ii)
!            scpltt = scpltt + frsw(nd,ii)
!         ENDIF
!
!         scsafr(nd,ii) = sctt
!         scsafrpl(nd,ii) = scpltt
!         dpscsafr(nd,ii) = dptp
!         scsafr(nd,ii) = MAX(scsafr(nd,ii),scsafrmn)
!         scsafrpl(nd,ii) = MAX(scsafrpl(nd,ii),scsafrmn)
!
! 2592    CONTINUE
!
! 2590 CONTINUE
!
!        end of read-statements
!
!        If the number of fatal errors is 1 or more, the PROGRAM will
!        be discontinued
!
 3900 IF (nuwa .GT. 0 .OR. nuer .GT.0 ) THEN
         er = .TRUE.
      ELSE
         er = .FALSE.
      ENDIF
      Status = Nuer + Nuwa
!
      IF (ex .or. .not.ex) Call CloseGP (nm)
!

!
      END
! *----------------------------------------------------------------------
!  FILE:
!     READunsa.PP
!     A.A.Veldhuizen
!
!  COPYRIGHT: 1998
!     DLO Winand Staring Centre for Integrated Land, Soil
!     and Water Research (SC-DLO), P.O.Box 125,
!     NL-6700 AC Wageningen, The Netherlands.
!
!     This PROGRAM, or parts thereof, may not be reproduced,
!     modified or transferred to third parties without the
!     written permission of the DLO Winand Staring Centre.
!
!  FILE IDENTIFICATION:
!     $Id$
!     $Log$
!
!  DESCRIPTION:
!     This file contains the source code of SUBROUTINE READunsa
!
!
! ----------------------------------------------------------------------
!  SUBPROGRAM:
!     READunsa
!     A.A.Veldhuizen
!
!  SYNOPSIS:
!
!  DESCRIPTION:
!     Reads input file unsa_sim.inp
!
!  HISTORY:
!     may-98     A.A.Veldhuizen
!                prototype
! ----------------------------------------------------------------------

      SUBROUTINE readunsa (file_unit,filenam,message_unit,message_file, &
                           debug_unit,debug_file, Status, &
                           nxspun, nxte, nxrz, nxdpun, srrz, fmca, scsa, &
                           dpgwun, dprzun, nudpun)
      use globals
! Arguments
!
      integer message_unit, ib
      integer debug_unit, id
      integer nxrz
      integer nxspun
      integer nxte
      integer Status
      integer nxdpun
      character(len=*) filenam, message_file, debug_file
      integer file_unit, nm
      Double Precision    srrz(nxspun, nxrz, nxdpun )
      Double Precision    fmca(nxspun, nxrz, nxdpun)
      Double Precision    dpgwun(nxspun, nxrz, nxdpun)
      Double Precision    dprzun(nxrz)
      Double Precision    scsa(nxspun, nxrz, nxdpun)
      integer nudpun(nxspun, nxrz)
!
! Local variables
!
      logical ex, er
      integer ii
      Double Precision    rrin
      integer nuwa
      integer nuer
      logical kyer
      integer nmli
      integer nmlicm
      character(len=132) chline
      integer rz
      integer iiin
      integer ns
      integer r_spun
      integer r_dprzun
      Double Precision    r_dpgwun
      Double Precision    r_srrz
      Double Precision    r_srrzwc
      Double Precision    r_fmca
      Double Precision    r_scsa
      Double Precision    r_scsapl
      integer artp(nxrz+nxdpun) ! Dynamic array
      integer ix(nxrz+nxdpun)   ! Dynamic array
      Double Precision    fmcatp(nxrz+nxdpun)
      Double Precision    scsatp(nxrz+nxdpun)
      Double Precision    srrztp(nxrz+nxdpun)
      Double Precision    dpgwuntp(nxrz+nxdpun)
      integer i378
      integer ios
!
! Functions
!
      logical ctrl_real
      Double Precision    GetRRin
      integer GetIIin
      integer IndexInReallist
!
! Common blocks
!
      integer nute
      integer nuspun
      integer nurz
      integer nufrsw
      common /NU_CMN/ nute, nurz, nuspun, nufrsw
!
! Init
!
      NuWa = 0
      NuEr = 0
!
      nm = file_unit
      ib = message_unit
      id = debug_unit
      if (message_unit >= 0) then
         OPEN (newunit=ib,FILE=message_file,STATUS='unknown',ACCESS='append',IOSTAT=ios)
         IF (ios .NE. 0 ) THEN
            write(*,'(1x,2a)') 'Fout bij openen ',message_file(1:len_trim(message_file))
            nuer = nuer + 1
            GOTO 3900
         ENDIF
         message_unit = ib
         maxFileUnitNumber = max(maxFileUnitNumber, ib)
         minFileUnitNumber = min(minFileUnitNumber, ib)
      endif
      IF (id .gt. 0) THEN
         OPEN (newunit=id,FILE=debug_file,STATUS='unknown',ACCESS='append', IOSTAT=ios)
         if (ios.ne.0) then
            write(ib,'(1x,2a)') 'Fout bij openen ',debug_file(1:len_trim(debug_file))
            nuer = nuer + 1
            GOTO 3900
         endif
         maxFileUnitNumber = max(maxFileUnitNumber, id)
         minFileUnitNumber = min(minFileUnitNumber, id)
         debug_unit = id
      ENDIF
!
! Begin
!
      RRin = GetRRin()
      IIin = GetIIin()
      nurz = 0
!
      nm = 11
      CALL openfile (nm,filenam,ex,ib, id)
!
      IF (.NOT. ex) THEN
         nuer = nuer + 1
         GOTO 3900
      ENDIF
!
!        Initialize arrays of table UNSA
!
      nurz = 0
 2500 DO 2510 rz = 1,nxrz
      dprzun(rz) = iiin
      DO 2520 ns = 1,nxspun
      nudpun(ns,rz) = 0
      DO 2530 ii = 1,nxdpun
         dpgwun(ns,rz,ii) = rrin
         srrz(ns,rz,ii) = rrin
!          srrzwc(ns,rz,ii) = rrin
         fmca(ns,rz,ii) = rrin
         scsa(ns,rz,ii) = rrin
!         scsapl(ns,rz,ii) = rrin
 2530 CONTINUE
 2520 CONTINUE
 2510 CONTINUE
!
!        Read records from UNSA
!        Check whether variable is okay
!        And put the variable in its array
!
      nmli = 0
      nmlicm = 0
!
 2555 READ (nm,501,END=2550) chline
  501 FORMAT (A132)

      IF ( chline(1:1) .EQ. '*' .OR. chline(2:2).EQ.'*') THEN
         nmlicm = nmlicm + 1
         GOTO 2555
      ELSE
         nmli = nmli + 1
         kyer = .FALSE.
!
         READ (chline,615) r_spun,r_dprzun,r_dpgwun,r_srrz,r_srrzwc,r_fmca,r_scsa,r_scsapl
  615    FORMAT (2I6,6F8.0)
!
!            IF (.NOT. ctrl_val ('unsa_sim',nmli+nmlicm,r_spun,'ns',
!     &      'r_spun','external soil physical UNIT number') ) THEN
!               nuer = nuer + 1
!               kyer = .TRUE.
!               GOTO 3900
!            ENDIF
!
         IF (r_spun .LT. 1 .OR. r_spun .GT. nxspun) THEN
            nuer = nuer + 1
            WRITE (ib,28) 'soil physical units','unsa_sim',nxspun
   28       FORMAT (/,'** E28  readvar  **',1X,' Maximum number of ',A,' exceeded in table ',A,'.inp',/,20X,&
                      ' According to input parameter maximum is: ',I5)
         ENDIF
!
!            IF (.NOT. ctrl_val ('unsa_sim',nmli+nmlicm,r_dprzun,'rz',
!     &      'r_dprzun','thickness of root zone') ) THEN
!               IF (r_dprzun.GT.-1.e-4.AND. ABS(r_dprzun-iiin).GT.1.e-4)
!     &         THEN
               if (r_dprzun .ge. 0) then
                  i378 = IndexInRealList(dble(r_dprzun), dprzun, nurz)
                  if (i378 .lt. 0) then
                     nurz = nurz + 1
                     IF (nurz.GT.nxrz) THEN
                        nurz = nurz - 1
!                        ncer(28) = ncer(28) + 1
                        WRITE (ib,28) 'root zones','unsa_sim',nxrz
                        nuer = nuer + 1
                        GOTO 3900
                     ENDIF
                     dprzun(nurz) = r_dprzun
                     r_dprzun = nurz
                  else
                     r_dprzun = i378
                  endif
               ELSE
                  nuer = nuer + 1
                  kyer = .TRUE.
                  GOTO 3900
               ENDIF
!            ENDIF
!
            IF (.NOT.ctrl_real('unsa_sim',nmli+nmlicm,r_dpgwun,-1.d1,1.d2,2,'r_dpgwun','depth of groundwater',ib) ) THEN
               kyer = .TRUE.
               nuer = nuer + 1
            ELSEIF (.NOT. kyer) THEN
               nudpun(r_spun,r_dprzun) = nudpun(r_spun,r_dprzun) + 1
               IF (nudpun(r_spun,r_dprzun) .GT. nxdpun) THEN
!                  ncer(28) = ncer(28) + 1
                  WRITE (ib,28) 'depths','unsa_sim',nxdpun
                  nudpun(r_spun,r_dprzun)=nudpun(r_spun,r_dprzun) -1
                  nuwa = nuwa + 1
               ENDIF
               ii = nudpun(r_spun,r_dprzun)
               dpgwun(r_spun,r_dprzun,ii) = r_dpgwun
            ENDIF
!
            IF (.NOT.ctrl_real ('unsa_sim',nmli+nmlicm,r_srrz,0.d0,5.d3,2,'r_srrz','storage root zone drying conditions',ib) ) THEN
               nuwa = nuwa + 1
            ELSEIF (.NOT. kyer) THEN
               srrz(r_spun,r_dprzun,ii) = r_srrz
            ENDIF
!
!         IF (.NOT. isbl(chline,29,36) ) THEN
!            IF (.NOT.ctrl_real ('unsa_sim',nmli+nmlicm,r_srrzwc,0.,5.e3,
!     &      2,'r_srrzwc','storage root zone wetting conditions') ) THEN
!               nuwa = nuwa + 1
!            ELSEIF (.NOT. kyer) THEN
!               srrzwc(r_spun,r_dprzun,ii) = r_srrzwc
!            ENDIF
!         ENDIF
!
            IF (.NOT.ctrl_real ('unsa_sim',nmli+nmlicm,r_fmca,0.d0,1.d2,2,'r_fmca','capillary rise flux (mm/d)',ib) ) THEN
               nuwa = nuwa + 1
            ELSEIF (.NOT. kyer) THEN
               fmca(r_spun,r_dprzun,ii) = r_fmca
            ENDIF
!
            IF (.NOT.ctrl_real ('unsa_sim',nmli+nmlicm,r_scsa,0.d0,1.d2,2,'r_scsa','storage coefficient',ib) ) THEN
               nuwa = nuwa + 1
            ELSEIF (.NOT. kyer) THEN
               scsa(r_spun,r_dprzun,ii) = r_scsa
            ENDIF
!
!         IF (.NOT. isbl(chline,53,60) ) THEN
!            IF (.NOT.ctrl_real ('unsa_sim',nmli+nmlicm,r_scsapl,0.,1.e2,
!     &      2,'r_scsapl','storage coefficient perched layer') ) THEN
!               nuwa = nuwa + 1
!            ELSEIF (.NOT. kyer) THEN
!               scsapl(r_spun,r_dprzun,ii) = r_scsapl
!            ENDIF
!         ENDIF
!
      ENDIF
!
      GOTO 2555
 2550 CONTINUE
!
!        Sort the arrays on both root zone depths and ground water depths
!
      DO 2552 ns = 1,nuspun
      DO 2554 rz = 1,nurz
!
         IF (nudpun(ns,rz).GT.0) THEN
            DO 2556 ii = 1,nudpun(ns,rz)
               artp(ii) = NINT (1000*dpgwun(ns,rz,ii))
               srrztp(ii) = srrz(ns,rz,ii)
!               srrzwctp(ii) = srrzwc(ns,rz,ii)
               fmcatp(ii) = fmca(ns,rz,ii)
               scsatp(ii) = scsa(ns,rz,ii)
!               scsapltp(ii) = scsapl(ns,rz,ii)
 2556       CONTINUE
            CALL indexx (nudpun(ns,rz),artp,ix)
            DO 2558 ii =1,nudpun(ns,rz)
               dpgwun(ns,rz,ii) = artp(ix(ii))/1000.
               srrz(ns,rz,ii) = srrztp(ix(ii))
!               srrzwc(ns,rz,ii) = srrzwctp(ix(ii))
               fmca(ns,rz,ii) = fmcatp(ix(ii))
               scsa(ns,rz,ii) = scsatp(ix(ii))
!               scsapl(ns,rz,ii) = scsapltp(ix(ii))
 2558       CONTINUE
         ENDIF
!
 2554 CONTINUE
 2552 CONTINUE
!
      DO 2560 rz = 1,nurz
         artp(rz) = dprzun(rz)
 2560 CONTINUE
!
      CALL indexx(nurz,artp,ix)
!
      DO 2561 rz = 1,nurz
         dprzun(rz)=artp(ix(rz))
 2561 CONTINUE
!
      DO 2562 ns = 1,nuspun
      DO 2563 ii = 1,nxdpun
!
         DO 2564 rz = 1,nurz
               dpgwuntp(rz) = dpgwun(ns,rz,ii)
               srrztp(rz) = srrz(ns,rz,ii)
!               srrzwctp(rz) = srrzwc(ns,rz,ii)
               fmcatp(rz) = fmca(ns,rz,ii)
               scsatp(rz) = scsa(ns,rz,ii)
!               scsapltp(rz) = scsapl(ns,rz,ii)
 2564    CONTINUE

         DO 2565 rz = 1,nurz
               dpgwun(ns,rz,ii) = dpgwuntp(ix(rz))
               srrz(ns,rz,ii) = srrztp(ix(rz))
!               srrzwc(ns,rz,ii) = srrzwctp(ix(rz))
               fmca(ns,rz,ii) = fmcatp(ix(rz))
               scsa(ns,rz,ii) = scsatp(ix(rz))
!               scsapl(ns,rz,ii) = scsapltp(ix(rz))
 2565    CONTINUE
!
 2563 CONTINUE
 2562 CONTINUE
!
      DO 2566 ns = 1,nuspun
      DO 2567 rz = 1,nurz
         artp(rz) = nudpun(ns,rz)
 2567 CONTINUE
!
      DO 2568 rz = 1,nurz
         nudpun(ns,rz) = artp(ix(rz))
 2568 CONTINUE
!
 2566 CONTINUE
!
!        Test whether arrays are complete
!
      DO 2569 ns = 1,nuspun
      DO 2570 rz = 1,nurz
      IF (nudpun(ns,rz) .EQ.0) THEN
!         ncer(37) = ncer(37) + 1
         WRITE (ib,37) 'unsa_sim',ns,NINT(dprzun(rz))
   37    FORMAT (/,'** E37  readvar  **',1X,1X,A,'.inp; missing input',/,20X, &
                   ' soil physical unit ',I5,' and root zone depth ',I4)
         nuer = nuer + 1
!
      ELSE
         IF ( dpgwun(ns,rz,1)  .GT. 1.d-4 ) THEN
!            ncer(26) = ncer(26) + 1
            WRITE(ib,26) 'unsa_sim','dpgwun','ns',ns,'rz',NINT(dprzun(rz))
   26       FORMAT (/,'** E26  readvar  **',1X, &
                      ' Table ',A,'.inp; Missing variable ',A,' with position:',&
                      5(/21X,A,' = ',I5) )
            nuwa = nuwa + 1
         ENDIF
!
         DO 2572 ii=2,nudpun(ns,rz)
         IF ( (srrz(ns,rz,ii-1) - srrz(ns,rz,ii)) .LT.-1.d-4 ) THEN
!            ncer(38) = ncer(38) + 1
            WRITE (ib,38) 'unsa_sim',ns,NINT(dprzun(rz)),  &
                   dpgwun(ns,rz,ii-1),srrz(ns,rz,ii-1),dpgwun(ns,rz,ii),srrz(ns,rz,ii)
   38       FORMAT (/,'** E38  readvar  **',1X,1X,A,'.inp; unit ',I5,' rzd ',I5,' drying conditions',/,20X, &
                      ' Storage in root zone inconsistent with groundwater depth:', &
					  2(/20X,' depth = ',F10.3,' storage = ',F10.3))
            nuwa = nuwa + 1
         ENDIF
 2572    CONTINUE
!
!         DO 2574 ii=1,nudpun(ns,rz)
!         IF ( ABS(srrzwc(ns,rz,ii)-rrin) .LT. 1.e-4 ) THEN
!            srrzwc(ns,rz,ii) = srrz(ns,rz,ii)
!         ENDIF
! 2574    CONTINUE
!
!         DO 2576 ii=2,nudpun(ns,rz)
!         IF ( (srrzwc(ns,rz,ii-1) - srrzwc(ns,rz,ii)) .LT.-1.e-4 ) THEN
!            ncer(39) = ncer(39) + 1
!            WRITE (ib,39) 'unsa_sim',spunex(ns),dprzun(rz),
!     &      dpgwun(ns,rz,ii-1),srrzwc(ns,rz,ii-1),
!     &      dpgwun(ns,rz,ii),srrzwc(ns,rz,ii)
!            nuwa = nuwa + 1
!         ENDIF
! 2576    CONTINUE
!
         DO 2578 ii=2,nudpun(ns,rz)
         IF ( INT(100.*dpgwun(ns,rz,ii)) .LE. dprzun(rz)) THEN
            fmca(ns,rz,ii) = fmca(ns,rz,1)
         ENDIF
         IF ( (fmca(ns,rz,ii-1) - fmca(ns,rz,ii)) .LT.-1.d-4 ) THEN
!            ncer(40) = ncer(40) + 1
            WRITE (ib,40) 'unsa_sim',ns,NINT(dprzun(rz)), &
                  dpgwun(ns,rz,ii-1),fmca(ns,rz,ii-1),dpgwun(ns,rz,ii),fmca(ns,rz,ii)
   40       FORMAT (/,'** E40  readvar  **',1X,1X,A,'.inp; unit ',I5,' rzd ',I5/20X, &
                      ' Capillary rise flux inconsistent with groundwater depth:',&
					  2(/20X,' depth = ',F10.3,' cap. rise = ',F10.3))
            nuwa = nuwa + 1
         ENDIF
 2578    CONTINUE
!
         DO 2580 ii=1,nudpun(ns,rz)
         IF ( INT(100.*dpgwun(ns,rz,ii)) .LE. dprzun(rz)) THEN
!c            scsa(ns,rz,ii) = 0.0
!            scsapl(ns,rz,ii) = 0.0
         ENDIF
 2580    CONTINUE
!
         DO 2582 ii=1,nudpun(ns,rz)
!         IF ( ABS(scsapl(ns,rz,ii)-rrin) .LT. 1.e-4 ) THEN
!            scsapl(ns,rz,ii) = scsa(ns,rz,ii)
!         ENDIF
!         IF ( ABS(scsa(ns,rz,ii)-rrin) .LT. 1.e-4 ) THEN
!            scsa(ns,rz,ii) = scsapl(ns,rz,ii)
!         ENDIF
         IF ( ABS(scsa(ns,rz,ii)-rrin) .LT. 1.d-4 ) THEN
!            ncer(46) = ncer(46) + 1
!            WRITE (ib,46) 'unsa_sim',spunex(ns),dprzun(rz),
!     &      dpgwun(ns,rz,ii)
         ENDIF
         IF ( srrz(ns,rz,ii) .GT. 1.d-4 ) THEN
            srrz(ns,rz,ii) = srrz(ns,rz,ii)/1000.
         ENDIF
!         IF ( srrzwc(ns,rz,ii) .GT. 1.e-4 ) THEN
!            srrzwc(ns,rz,ii) = srrzwc(ns,rz,ii)/1000.
!         ENDIF
         IF ( fmca(ns,rz,ii) .GT. 1.d-4 ) THEN
            fmca(ns,rz,ii) = fmca(ns,rz,ii)/1000.
         ENDIF
 2582    CONTINUE
!
      ENDIF
 2570 CONTINUE
 2569 CONTINUE
!
!        end of read-statements
!
!        If the number of fatal errors is 1 or more, the PROGRAM will
!        be discontinued
!
 3900 IF (nuwa .GT. 0 .OR. nuer .GT.0 ) THEN
         er = .TRUE.
      ELSE
         er = .FALSE.
      ENDIF
      Status = Nuer + Nuwa
!
      IF (ex .or. .not.ex) Call CloseGP (nm)
!

!
      END
! **********************************************************************
!     IndexInRealList Function
!     Yke van Randen
!     aug 1998
! **********************************************************************
!
      integer function IndexInRealList(x, l, n)
!
! Arguments
!
      Double Precision    x
      Double Precision    l(*)
      integer n
!
! local variables
!
      integer i
!
! Begin
!
      IndexInRealList = -1
      do i = 1 , n
         if (abs(l(i)-x) .lt. 1.0e-6)  then
            IndexInRealList = i
            return
         endif
      enddo
!
! End
!
      end
!
      Double Precision FUNCTION unsa (ns,nt,dp,dproot,UNDA,  &
                            DEBUG_UNIT, MESSAGE_UNIT, STATUS,          &
                            NXTE, NXSPUN, NXRZ, NXDPUN,                &
                            NURZ, DPRZUN, DPGWUN, NUDPUN)
! ----------------------------------------------------------------------
!  SUBPROGRAM:
!     scfudp
!     A.A.Veldhuizen
!
!
!  DESCRIPTION:
!     FUNCTION UNSA calculates the capillary rise,
!     equilibrium moisture content for drying conditions or storage coefficient
!     for groundwater depth dp of soil physical unit ns and land use nt
!
!     If the groundwater depth is above or below the tabulates depths,
!     the outcome is kept constant at the extreme value
!
!
!  FILE ID : $Id: rootzone.for 1.1 1998/08/05 10:10:35 unknown Exp unknown $
!
!  HISTORY:
!     jul-96     A.A.Veldhuizen
!                changes in surface water system
!                and many other small changes
!     jul-96     A.A.Veldhuizen
!                changes in surface water system
!                and many other small changes
!     feb-96     A.A.Veldhuizen
!                prototype
!     aug-98     IJke
!                afhankelijkheid van SIMGRO data eruit.
!                Nieuwe argumenten:
!                nurz, dprzun, srrzdc usw
!                and many other small changes
! ----------------------------------------------------------------------
!
!     INTEGER*4     nt             ! Techonlogy number
!     Double Precision        dp             ! Grondwaterstand
!
      INTEGER  DEBUG_UNIT
      INTEGER  MESSAGE_UNIT
      INTEGER  STATUS
      INTEGER  NXTE
      INTEGER  NXSPUN
      INTEGER  NXRZ
      INTEGER  NXDPUN

      INTEGER  Nt
      Double Precision     Dp,dproot

      INTEGER  Nurz
      Double Precision     UNDA(nxspun, nxrz, nxdpun)   ! Unsaturated data (Eq. moist. cont. etc)
      Double Precision     Dprzun(nxrz)
      Double Precision     Dpgwun(nxspun, nxrz, nxdpun)
      INTEGER      Nudpun(nxspun, nxrz)

      INTEGER*4     ii,i1
      INTEGER*4     ns             ! Nummer soil physical unit
      Double Precision        dprztp         ! wortelzone in cm
      Double Precision        dpgwtp         ! Lokale variabele voor diepte grondwaterstand
      Double Precision        va01,va02
      Double Precision        fr
      Double Precision        unfudp
!
!        Setup of SUBROUTINE:
!
!        1 initialize temporary variables
!        2 determine both root zone depths for interpolation
!        3 calculate eq. moisture content/capill. rise for both root zones
!        4 interpolate between root zones
!
!        The variable temporary depth (DPGWTP) is used to manipulate DP only
!        within this SUBROUTINE
!
!        1 Set variables
!
      dprztp = dproot*100
      dpgwtp = dp
!
!        2 Calculate root zones
!
!
!  Uit UNSA_SIM.INP komt de lijst dprzun. Deze lijst bevat per spun
!  een rootzone en een gwst.
!  Key = (spun, dprz, dpgw).
!
      if (nurz > 1) then

         DO 1000 ii=1,nurz
            i1=ii-1
            IF ( dprztp .LT. dprzun(ii) ) GO TO 2000
 1000    CONTINUE
!
!        Obviously the root zone depth is below the minumum tabulated
!        value; the VALU is therefore calculated for the
!        deepest possible value;
!
         i1 = nurz-1
         dprztp = dprzun(nurz)
!
 2000    IF (ii.EQ.1) THEN
!
!           The  root zone is above the highest tabulated one;
!           the variables are therefore calculated for the
!           highest possible value
!
            i1 = 1
            dprztp = dprzun(1)
         ENDIF

!
!        3 determine values for both root zones
!
         va01   = unfudp(ns,i1+1,dpgwtp,UNDA, dpgwun, nudpun, nxspun,nxrz,nxdpun)
         va02   = unfudp(ns,i1,dpgwtp,UNDA, dpgwun, nudpun, nxspun,nxrz,nxdpun)
!
!        4 Perform interpolations
!
         fr = dprzun(i1+1) - dprztp
         fr = fr / ( dprzun(i1+1) - dprzun(i1) )
         UNSA  = va01   + ( fr*(va02   - Va01  ) )

      else
         UNSA = 0.0d0
      endif
!
      END
!
! ----------------------------------------------------------------------
!  SUBPROGRAM:
!     UNFUDP          - Calculates value for the unsaturated zone
!     A.A.Veldhuizen
!
!
!  DESCRIPTION:
!     Function UNFUDP calculates the capillary rise flux, equilibrium
!     moisture content of the root zone for drying or storage coefficient.
!     The matrix for the interpolation is UNDA (unsaturated data).
!
!
!
!  HISTORY:
!     jul-96     A.A.Veldhuizen
!                changes in surface water system
!                and many other small changes
!     jul-96     A.A.Veldhuizen
!                changes in surface water system
!                and many other small changes
!     feb-96     A.A.Veldhuizen
!                prototype
! ----------------------------------------------------------------------
!
      Double Precision FUNCTION unfudp (ns,rz,dp,unda, dpgwun, nudpun, nxspun,nxrz,nxdpun)
!
!       INCLUDE 'simgro.in0'
!       INCLUDE 'simgro.in5'
!
!  begin
      integer       nxspun,nxrz,nxdpun
      Double Precision          dpgwun(nxspun, nxrz, nxdpun)
      integer       nudpun(nxspun, nxrz)
!  end
      INTEGER*4     ns,rz
      Double Precision        dp,unda(nxspun,nxrz,nxdpun)

!
      INTEGER*4     i1,ii
      Double Precision        dpgwtp,fr
!
!
!        1 Set variables
!
      dpgwtp = dp
!
!        2 Calculate groundwater levels for interpolation
!
      if (nudpun(ns,rz) > 1) then

         DO 1000 ii=1,nudpun(ns,rz)
            i1=ii-1
            IF ( dpgwtp .LT. dpgwun(ns,rz,ii) ) GO TO 2000
 1000    CONTINUE
!
!        Obviously the groundwater table is below the minumum tabulated
!        value; the eq. moisture content is therefore calculated for the
!        deepest possible value; as is the capillary rise flux.
!
         i1 = nudpun(ns,rz)-1
         dpgwtp = dpgwun(ns,rz,nudpun(ns,rz))
!
 2000    IF (ii.EQ.1) THEN
!
!           The  root zone is above the highest tabulated one;
!           the variables are therefore calculated for the
!           highest possible value
!
            i1 = 1
            dpgwtp = dpgwun(ns,rz,1)
         ENDIF

!
!        4 Perform interpolation
!
         fr = dpgwun(ns,rz,i1+1) - dpgwtp
         fr = fr / ( dpgwun(ns,rz,i1+1) - dpgwun(ns,rz,i1) )
         unfudp = unda(ns,rz,i1+1) + fr * ( unda(ns,rz,i1) - unda(ns,rz,i1+1) )

      else
         unfudp = 0.0d0
      endif
!
      END
!

!----------------------------------------------------------------------
!  FILE:
!     CTRL_LIM.PP
!     A.A.Veldhuizen
!
!  COPYRIGHT: 1995
!     DLO Winand Staring Centre for Integrated Land, Soil
!     and Water Research (SC-DLO), P.O.Box 125,
!     NL-6700 AC Wageningen, The Netherlands.
!
!     This PROGRAM, or parts thereof, may not be reproduced,
!     modified or transferred to third parties without the
!     written permission of the DLO Winand Staring Centre.
!
!  FILE IDENTIFICATION:
!     $Id: CTRL_LIM.FOR 1.2 1998/06/24 11:38:49 unknown Exp unknown $
!     $Log: CTRL_LIM.FOR $
!     Revision 1.2  1998/06/24 11:38:49  unknown
!     *** empty log message ***
!     Revision 1.1  1998/06/11 16:33:53  unknown
!     Initial revision
!   Revision 4.0  1998/02/24  00:00:00  AV
!   version 4
!
!   Revision 3.1  1998/02/15  00:00:00  AV
!   official release
!
!   Revision 3.0  1998/01/26  00:00:00  AV
!   official release
!
!   Revision 1.809  1997/01/30  00:00:00  AV
!   3rd version new style
!
!   Revision 1.808  1996/10/08  00:00:00  AV
!   2nd version new style
!
!   Revision 1.807  1996/09/11  00:00:00  AV
!   1st version new style
!
!
!  DESCRIPTION:
!     This file contains the source code of functions
!     CTRL_Double Precision
!     CTRL_INT
!     CTRL_VAL
!
!
! ----------------------------------------------------------------------
!  SUBPROGRAM:
!     CTRL_Double Precision     - Controls limits of variables
!     A.A.Veldhuizen
!
!  SYNOPSIS:
!     See manual.
!
!  DESCRIPTION:
!     Controls limits of variables, sets to max/min values if necessary.
!     If no error condition occurs, function is TRUE
!
!
!
!  HISTORY:
!     jul-96     A.A.Veldhuizen
!                changes in surface water system
!                and many other small changes
!     jul-96     A.A.Veldhuizen
!                changes in surface water system
!                and many other small changes
!     dec-95     A.A.Veldhuizen
!                prototype
! ----------------------------------------------------------------------
!
      LOGICAL FUNCTION ctrl_real(nafi,nmli,va,vamn,vamx,tyva,nava,deva, ib)
!
      integer ib
      INTEGER*4     tyva,nmli
      Double Precision        va,vamn,vamx,vaod
      CHARACTER(len=*) nava
      CHARACTER(len=*) nafi
      CHARACTER(len=*) deva
!
!
      ctrl_real = .TRUE.
      vaod = va
!
      IF (va.GT.vamx) THEN
         IF (tyva.EQ.1) THEN
            va=vamx
!            ncwn(8) = ncwn(8) + 1
!            IF (ipwn) WRITE (ib,108) nmli,nafi,nava,deva,vaod,vamx
         ELSEIF (tyva.EQ.2) THEN
!            ncwn(9) = ncwn(9) + 1
!            IF (ipwn) WRITE (ib,109) nmli,nafi,nava,deva,vaod,vamx
         ELSEIF (tyva.EQ.3) THEN
            IF (nmli.GT.0) THEN
!              ncer(11) = ncer(11) + 1
               WRITE (ib,11) nmli,nafi,nava,deva,vaod,vamx
            ELSE
!               ncer(12) = ncer(12) + 1
               WRITE (ib,12) nava,deva,vaod,vamx
            ENDIF
            ctrl_real=.FALSE.
         ENDIF
      ELSEIF (va.LT.vamn) THEN
         IF (tyva.EQ.1) THEN
            va=vamn
!            ncwn(10) = ncwn(10) + 1
!            IF (ipwn) WRITE (ib,110) nmli,nafi,nava,deva,vaod,vamn
         ELSEIF (tyva.EQ.2) THEN
!            ncwn(11) = ncwn(11) + 1
!            IF (ipwn) WRITE (ib,111) nmli,nafi,nava,deva,vaod,vamn
         ELSEIF (tyva.EQ.3) THEN
            IF (nmli.GT.0) THEN
!               ncer(13) = ncer(13) + 1
               WRITE (ib,13) nmli,nafi,nava,deva,vaod,vamn
            ELSE
!               ncer(14) = ncer(14) + 1
               WRITE (ib,14) nava,deva,vaod,vamn
            ENDIF
            ctrl_real=.FALSE.
         ENDIF
      ENDIF
!
  108 FORMAT (/'** W08  ctrl_rea **',1X,                    &
              ' Line ',I5,' of file ',A,'.inp'/20X,         &
              ' Variable ',A,1X,A/20X,                      &
              ' is                        ',G11.4/20X,      &
              ' and exceeds maximum value ',G11.4/20X,      &
              ' Therefore the variable is set to maximum')
  109 FORMAT (/'** W09  ctrl_rea **',1X,                     &
              ' Line ',I5,' of file ',A,'.inp'/20X,         &
              ' Variable ',A,1X,A/20X,                      &
              ' is                        ',G11.4/20X,      &
              ' and exceeds maximum value ',G11.4/20X,      &
              ' Check your input, run will continue with old value')
   11 FORMAT (/'** E11  ctrl_rea **',1X,                      &
              ' Line ',I5,' of file ',A,'.inp'/20X,          &
              ' Variable ',A,1X,A/20X,                       &
              ' is                        ',G11.4/20X,       &
              ' and exceeds maximum value ',G11.4/20X,       &
              ' Run will stop due to error condition')
   12 FORMAT (/'** E12  ctrl_rea **',1X,                      &
              ' parameter file'/20X,                         &
              ' Variable ',A,1X,A/20X,                       &
              ' is                        ',G11.4/20X,       &
              ' and exceeds maximum value ',G11.4/20X,       &
              ' Run will stop due to error condition')
!
  110 FORMAT (/'** W10  ctrl_rea **',1X,                      &
              ' Line ',I5,' of file ',A,'.inp'/20X,          &
              ' Variable ',A,1X,A/20X,                       &
              ' is                             ',G11.4/20X,  &
              ' and is less than minimum value ',G11.4/20X,  &
              ' Therefore the variable is set to minimum')
  111 FORMAT (/'** W11  ctrl_rea **',1X,                      &
              ' Line ',I5,' of file ',A,'.inp'/20X,          &
              ' Variable ',A,1X,A/20X,                       &
              ' is                             ',G11.4/20X,  &
              ' and is less than minimum value ',G11.4/20X,  &
              ' Check your input, run will continue with old value')
   13 FORMAT (/'** E13  ctrl_rea **',1X,                      &
              ' Line ',I5,' of file ',A,'.inp'/20X,          &
              ' Variable ',A,1X,A/20X,                       &
              ' is                             ',G11.4/20X,  &
              ' and is less than minimum value ',G11.4/20X,  &
              ' Run will stop due to error condition')
   14 FORMAT (/'** E14  ctrl_rea **',1X,                      &
              ' Parameter file'/20X,                         &
              ' Variable ',A,1X,A/20X,                       &
              ' is                             ',G11.4/20X,  &
              ' and is less than minimum value ',G11.4/20X,  &
              ' Run will stop due to error condition')
!
      END
!
!
! ----------------------------------------------------------------------
!  SUBPROGRAM:
!     CTRL_INT      - Controls limits of variables
!     A.A.Veldhuizen
!
!  SYNOPSIS:
!     See manual.
!
!  DESCRIPTION:
!     Controls limits of variables, sets to max/min values if necessary.
!     If no error condition occurs, function is TRUE
!
!
!
!  HISTORY:
!     jul-96     A.A.Veldhuizen
!                changes in surface water system
!                and many other small changes
!     jul-96     A.A.Veldhuizen
!                changes in surface water system
!                and many other small changes
!     dec-95     A.A.Veldhuizen
!                prototype
! ----------------------------------------------------------------------
!
      LOGICAL FUNCTION ctrl_int (nafi,nmli,va,vamn,vamx,tyva,nava,deva, ib)
!
      integer ib
      INTEGER*4     tyva,nmli
      INTEGER*4     va,vamn,vamx,vaod
      CHARACTER(len=*) nava
      CHARACTER(len=*) nafi
      CHARACTER(len=*) deva
!
!
      ctrl_int = .TRUE.
      vaod = va
!
      IF (va.GT.vamx) THEN
         IF (tyva.EQ.1) THEN
            va=vamx
!            ncwn(8) = ncwn(8) + 1
!            IF (ipwn) WRITE (ib,108) nmli,nafi,nava,deva,vaod,vamx
         ELSEIF (tyva.EQ.2) THEN
!            ncwn(9) = ncwn(9) + 1
!            IF (ipwn) WRITE (ib,109) nmli,nafi,nava,deva,vaod,vamx
         ELSEIF (tyva.EQ.3) THEN
            IF (nmli.GT.0) THEN
!               ncer(11) = ncer(11) + 1
               WRITE (ib,11) nmli,nafi,nava,deva,vaod,vamx
            ELSE
!               ncer(12) = ncer(12) + 1
               WRITE (ib,12) nava,deva,vaod,vamx
            ENDIF
            ctrl_int=.FALSE.
         ENDIF
      ELSEIF (va.LT.vamn) THEN
         IF (tyva.EQ.1) THEN
            va=vamn
!            ncwn(10) = ncwn(10) + 1
!           IF (ipwn) WRITE (ib,110) nmli,nafi,nava,deva,vaod,vamn
         ELSEIF (tyva.EQ.2) THEN
!            ncwn(11) = ncwn(11) + 1
!            IF (ipwn) WRITE (ib,111) nmli,nafi,nava,deva,vaod,vamn
         ELSEIF (tyva.EQ.3) THEN
            IF (nmli.GT.0) THEN
!               ncer(13) = ncer(13) + 1
               WRITE (ib,13) nmli,nafi,nava,deva,vaod,vamn
            ELSE
!               ncer(14) = ncer(14) + 1
               WRITE (ib,14) nava,deva,vaod,vamn
            ENDIF
            ctrl_int=.FALSE.
         ENDIF
      ENDIF
!
  108 FORMAT (/'** W08  ctrl_int **',1X,             &
              ' Line ',I5,' of file ',A,'.inp'/20X,  &
              ' Variable ',A,1X,A/20X,               &
              ' is                        ',I5/20X,  &
              ' and exceeds maximum value ',I5/20X,  &
              ' Therefore the variable is set to maximum')
  109 FORMAT (/'** W09  ctrl_int **',1X,             &
               ' Line ',I5,' of file ',A,'.inp'/20X, &
               ' Variable ',A,1X,A/20X,              &
               ' is                        ',I5/20X, &
               ' and exceeds maximum value ',I5/20X, &
               ' Check your input, run will continue with old value')
   11 FORMAT (/'** E11  ctrl_int **',1X,             &
               ' Line ',I5,' of file ',A,'.inp'/20X, &
               ' Variable ',A,1X,A/20X,              &
               ' is                        ',I5/20X, &
               ' and exceeds maximum value ',I5/20X, &
               ' Run will stop due to error condition')
   12 FORMAT (/'** E12  ctrl_int **',1X,             &
               ' Parameter file'/20X,                &
               ' Variable ',A,1X,A/20X,              &
               ' is                        ',I5/20X, &
               ' and exceeds maximum value ',I5/20X, &
               ' Run will stop due to error condition')
!
  110 FORMAT (/'** W10  ctrl_int **',1X,             &
               ' Line ',I5,' of file ',A,'.inp'/20X, &
               ' Variable ',A,1X,A/20X,              &
               ' is                             ',I5/20X, &
               ' and is less than minimum value ',I5/20X, &
               ' Therefore the variable is set to minimum')
  111 FORMAT (/'** W11  ctrl_int **',1X,                  &
               ' Line ',I5,' of file ',A,'.inp'/20X,      &
               ' Variable ',A,1X,A/20X,                   &
               ' is                             ',I5/20X, &
               ' and is less than minimum value ',I5/20X, &
               ' Check your input, run will continue with old value')
   13 FORMAT (/'** E13  ctrl_int **',1X,                  &
               ' Line ',I5,' of file ',A,'.inp'/20X,      &
               ' Variable ',A,1X,A/20X,                   &
               ' is                             ',I5/20X, &
               ' and is less than minimum value ',I5/20X, &
               ' Run will stop due to error condition')
   14 FORMAT (/'** E14  ctrl_int **',1X,                  &
               ' Parameter file'/20X,                     &
               ' Variable ',A,1X,A/20X,                   &
               ' is                             ',I5/20X, &
               ' and is less than minimum value ',I5/20X, &
               ' Run will stop due to error condition')
!
      END
! *----------------------------------------------------------------------
!  FILE:
!     INDEXX.PP
!     A.A.Veldhuizen
!
!  COPYRIGHT: 1996
!     DLO Winand Staring Centre for Integrated Land, Soil
!     and Water Research (SC-DLO), P.O.Box 125,
!     NL-6700 AC Wageningen, The Netherlands.
!
!     This PROGRAM, or parts thereof, may not be reproduced,
!     modified or transferred to third parties without the
!     written permission of the DLO Winand Staring Centre.
!
!  FILE IDENTIFICATION:
!     $Id: INDEXX.FOR 1.2 1998/06/24 11:39:00 unknown Exp unknown $
!     $Log: INDEXX.FOR $
!     Revision 1.2  1998/06/24 11:39:00  unknown
!     *** empty log message ***
!     Revision 1.1  1998/06/11 16:33:54  unknown
!     Initial revision
!   Revision 4.0  1998/02/24  00:00:00  AV
!   version 4
!
!   Revision 3.1  1998/02/15  00:00:00  AV
!   official release
!
!   Revision 3.0  1998/01/26  00:00:00  AV
!   official release
!
!   Revision 1.809  1997/01/30  00:00:00  AV
!   3rd version new style
!
!   Revision 1.808  1996/10/08  00:00:00  AV
!   2nd version new style
!
!   Revision 1.807  1996/09/11  00:00:00  AV
!   1st version new style
!
!
!  DESCRIPTION:
!     This file contains the source code of SUBROUTINE INDEXX
!
!
! ----------------------------------------------------------------------
!  SUBPROGRAM:
!     INDEXX
!     A.A.Veldhuizen
!
!  SYNOPSIS:
!     IN:        NB
!     IN:        NE
!     IN:        ARRIN
!     OUT:       INDX
!
!  COMMON:
!
!  DESCRIPTION:
!     SUBROUTINE INDEXX indexes in array ARRIN (nb:ne), ie outputs the
!     array INDX such that ARRIN(INDX(J)) is in ascending order for
!     j=NB,NB+1,...,NE
!
!
!  HISTORY:
!     jul-96     A.A.Veldhuizen
!                changes in surface water system
!                and many other small changes
!     jul-96     A.A.Veldhuizen
!                changes in surface water system
!                and many other small changes
!     feb-96     A.A.Veldhuizen
!                standardisation and implementation of NB
!     1986       Numerical recipes, Press et al.
!                rough code
! ----------------------------------------------------------------------
!
      SUBROUTINE indexx(n,arrin,indx)
!
      INTEGER*4     i,j,n,l,nr,q
      INTEGER*4     indxt
      INTEGER*4     arrin(*)
      INTEGER*4     indx(*)
!
      DO 11 j=1,n
        indx(j) = j
11    CONTINUE
      l = n/2+1
      nr = n
10    CONTINUE
         IF ( l.GT.1 ) THEN
            l = l-1
            indxt = indx(l)
            q = arrin(indxt)
         ELSE
            indxt = indx(nr)
            q = arrin(indxt)
            indx(nr) = indx(1)
            nr = nr-1
            IF ( nr.EQ.1 ) THEN
               indx(1) = indxt
               RETURN
            ENDIF
         ENDIF
         i = l
         j = l+l
20       IF ( j.LE.nr ) THEN
            IF ( j.LT.nr) THEN
               IF ( arrin(indx(j)) .LT. arrin(indx(j+1)) ) j = j+1
            ENDIF
            IF ( q.LT.arrin(indx(j)) ) THEN
               indx(i) = indx(j)
               i = j
               j = j+j
            ELSE
               j = nr+1
            ENDIF
         GO TO 20
         ENDIF
         indx(i) = indxt
      GO TO 10
!
      END
