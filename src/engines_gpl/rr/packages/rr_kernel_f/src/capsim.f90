! MIT License
! 
! Copyright (c) 2025 Stichting Deltares
! Copyright (c) 1995 DLO Winand Staring Centre for Integrated Land, Soil and Water Research (SC-DLO)
! 
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
! 
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
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
module m_capsim
   use precision_basics, only : dp
   implicit none
   private
   
   public simgro_ovz
   public readunsa
   public readroot

   integer :: nute
   integer :: nuspun
   integer :: nurz
   integer :: nufrsw

!  Deze source-code bevat de vier onderdelen uit de notitie
!  over de koppeling CAPSIM-SOBEK-RR
!
contains
!
   subroutine simgro_ovz(debug_unit, debug_file, message_unit, message_file, status, &
                         nxspun, nxte, nxrz, nxdpun, nxfrsw, &
                         ns, nt, dpin, dproot, dt, pn, vmrzin, fmevpt, &
                         srrz, fmca, scsa, &
                         dprzun, dpgwun, nudpun, dpfrsw, frsw, frev, &
                         fmevac, fmpe, vmrzac)

      use globals
      !In/Out
      integer, intent(inout) :: debug_unit
      character(len=*), intent(inout) :: debug_file
      integer, intent(inout) :: message_unit
      character(len=*), intent(inout) :: message_file
      integer, intent(inout) :: status
!In
      integer, intent(in) :: nxspun !< Maximum number of soil physical units
      integer, intent(in) :: nxte !< Maxumum number of land use forms
      integer, intent(in) :: nxrz !< Maximum number of root zones in table
      integer, intent(in) :: nxdpun !< Maximum number of groundwater depths in table
      integer, intent(in) :: nxfrsw !< Maximum number of data in inundation table
      integer, intent(in) :: ns !< Soil physical unit number
      integer, intent(in) :: nt !< Gewasnummer
      real(kind=dp), intent(in) :: dpin !< Diepte grondwater initieel (m-gl)
      real(kind=dp), intent(in) :: dproot !< Root zone depth (m)
      real(kind=dp), intent(in) :: dt !< Time step (d)
      real(kind=dp), intent(in) :: pn !< Net precipitation (m/d)
      real(kind=dp), intent(in) :: vmrzin !< Initial moisture content root zone (m)
      real(kind=dp), intent(in) :: fmevpt !< Potential evapotranspiration (m/d)
      real(kind=dp), intent(in) :: srrz(nxspun, nxrz, nxdpun) !< Storage root zone table (m)
      real(kind=dp), intent(in) :: fmca(nxspun, nxrz, nxdpun) !< Capillary rise table (m/d)
      real(kind=dp), intent(in) :: scsa(nxspun, nxrz, nxdpun) !< Storage coefficient table (m)
      real(kind=dp), intent(in) :: dprzun(nxrz) !< Root zone depths in table (cm)
      real(kind=dp), intent(in) :: dpgwun(nxspun, nxrz, nxdpun) !< Groundwater depths in table (m-gl)
      integer, intent(in) :: nudpun(nxspun, nxrz) !< number of groundwater depth data in table
      real(kind=dp), intent(in) :: dpfrsw(nxfrsw) !< Depth of groundwater in inundation table (m-gl)
      real(kind=dp), intent(in) :: frsw(nxfrsw) !< Inundation table (-)
      real(kind=dp), intent(in) :: frev(nxspun, nxte, 5) !< Evapotranspiration curves (-)
! Out
      real(kind=dp), intent(out) :: fmevac !< Actual evapotranspiration (m/d)
      real(kind=dp), intent(out) :: fmpe !< Percolation (m/d)
      real(kind=dp), intent(out) :: vmrzac !< Actual storage in root zone (m)
!
! Local variables
!
      integer :: ib, id
      real(kind=dp) :: de
      logical :: issw
      real(kind=dp) :: Frrdca = 0.0_dp
      real(kind=dp) :: dtgw
      real(kind=dp) :: icselo
      real(kind=dp) :: vmrzpt
      real(kind=dp) :: vmrzeq0
      real(kind=dp) :: vmrzeq1
      real(kind=dp) :: vmrzeq2
      real(kind=dp) :: vmrzeq10
      real(kind=dp) :: vmrzeq
      real(kind=dp) :: frpe, frtp
      real(kind=dp) :: frswtp
      real(kind=dp) :: dptp
      real(kind=dp) :: fmevptsw ! Potential evapotranspiration surface water (m/d)
      integer :: ios
!
! Functions
!
      status = 0
      ib = message_unit
      id = debug_unit
      if (id > 0) then
         open (newunit=id, file=debug_file, status='unknown', access='append', iostat=ios)
         if (ios /= 0) then
            call openmesg(ib, message_file, status)
            write (ib, '(1x,2a)') 'Fout bij openen ', debug_file(1:len_trim(debug_file))
            call closegp(ib)
!            status = status + 1
            goto 3900
         end if
         maxFileUnitNumber = max(maxFileUnitNumber, id)
         minFileUnitNumber = min(minFileUnitNumber, id)
         debug_unit = id
      end if
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
      if (debug_unit > 0) then
         write (debug_unit, '(1x,a)') 'Start SIMGRO_OVZ'
      end if
      if (nt < 0 .or. nt > nute) then
         status = status + 1
         call openmesg(ib, message_file, status)
         write (ib, 555) 'nt = land use type', nt, nute
         call CloseGP(ib)
      end if
      if (ns < 0 .or. ns > nuspun) then
         status = status + 1
         call openmesg(ib, message_file, status)
         write (ib, 555) 'ns = soil physical unit', ns, nuspun
         call CloseGP(ib)
      end if
      issw = .false.
      icselo = 3
      dtgw = dt
      dptp = dpin
      frswtp = 0. ! Geen inundatie
!
      if (debug_unit > 0) then
         write (debug_unit, '(1x,a)') 'Calculate eq.moisture 0'
      end if
!
      vmrzeq1 = unsa(ns, nt, dproot, dproot, srrz, &
                     debug_unit, message_unit, status, &
                     nxte, nxspun, nxrz, nxdpun, &
                     nurz, dprzun, dpgwun, nudpun)
!
      if (debug_unit > 0) then
         write (debug_unit, '(1x,a)') 'End calculate eq.moisture 0'
      end if
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
         if (debug_unit > 0) then
            write (debug_unit, '(1x,a)') 'Calculate actual evap.'
         end if
!
         if (vmrzeq1 > 0.0_dp) then
            frtp = vmrzin / vmrzeq1
            if (frtp > 1.) then
               vmrzeq0 = unsa(ns, nt, 0._dp, dproot, srrz, &
                              debug_unit, message_unit, status, &
                              nxte, nxspun, nxrz, nxdpun, &
                              nurz, dprzun, dpgwun, nudpun)

               if (vmrzeq0 > 0.0_dp) then
                  frtp = vmrzin / vmrzeq0
                  frtp = max(frtp, frev(ns, nt, 3))
               else
                  frtp = 0.0_dp
               end if
            end if

         else
            frtp = 0.0_dp
         end if
         !
         de = ActualEvaporation(fmevpt, &
                                frev(ns, nt, 1), &
                                frev(ns, nt, 2), &
                                frev(ns, nt, 3), &
                                frev(ns, nt, 4), &
                                frev(ns, nt, 5), &
                                frtp, &
                                issw, &
                                fmevptsw, &
                                frswtp, icselo)
      end if
!
      if (debug_unit > 0) then
         write (debug_unit, '(1x,a)') 'End calculate actual evap.'
      end if
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
      vmrzpt = vmrzin + (pn - fmevac) * dtgw
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
      if (debug_unit > 0) then
         write (debug_unit, '(1x,a)') 'Calculate eq.moisture dp'
      end if
!
      vmrzeq = unsa(ns, nt, dptp, dproot, srrz, &
                    debug_unit, message_unit, status, &
                    nxte, nxspun, nxrz, nxdpun, &
                    nurz, dprzun, dpgwun, nudpun)
!
      if (debug_unit > 0) then
         write (debug_unit, '(1x,a)') 'End calculate eq.moisture dp'
      end if
!
!
!      Berekening capillaire opstijging/percolatie, alsmede 2b (inhoud wortelzone, moet uit elkaar)
!      In vtt = potentiele inhoud wortelzone
!         vmrzeq = evenwichtsvochtgehalte bij gws
!         dtgw  = grondwatertijdstap
!
      if (vmrzpt >= vmrzeq) then
!                 Percolation as excess moisture -  fmpe neg
         if (dptp < (2.0_dp + dproot)) then
            fmpe = (vmrzeq - vmrzpt) / dtgw
         else
!                 It is assumed that the equilibrium moisture content of
!                 the root zone stabilizes if the groundwater level becomes
!                 lower than 2.+dproot m-gl.
            vmrzeq2 = unsa(ns, nt, 2.0_dp + dproot, dproot, srrz, &
                           debug_unit, message_unit, status, &
                           nxte, nxspun, nxrz, nxdpun, &
                           nurz, dprzun, dpgwun, nudpun)
            if (vmrzeq2 > vmrzpt) then
               fmpe = 0.
            else
               fmpe = (vmrzeq2 - vmrzpt) / dtgw
            end if
         end if
         vmrzac = vmrzpt + (dtgw * fmpe)
!
      else
!
!                 Capillary rise  -  FMPE pos
!
         if (debug_unit > 0) then
            write (debug_unit, '(1x,a)') 'Calculate eq.moisture 10'
         end if
!
         vmrzeq10 = unsa(ns, nt, 10.0_dp, dproot, srrz, &
                         debug_unit, message_unit, status, &
                         nxte, nxspun, nxrz, nxdpun, &
                         nurz, dprzun, dpgwun, nudpun)
!
         if (debug_unit > 0) then
            write (debug_unit, '(1x,a)') 'Calculate capill. rise'
         end if
!
         fmpe = unsa(ns, nt, dptp, dproot, fmca, &
                     debug_unit, message_unit, status, &
                     nxte, nxspun, nxrz, nxdpun, &
                     nurz, dprzun, dpgwun, nudpun)
!
         if (debug_unit > 0) then
            write (debug_unit, '(1x,a)') 'End capillary rise'
         end if
!
!              Reduce the capillary rise in case of a small deficit
!              in the root zone (Paul van Walsum)
!
         if (vmrzeq > vmrzeq10 + 1.0e-4_dp .and. vmrzin > vmrzeq10 + 1.0e-4_dp) then
            frpe = (vmrzeq - vmrzin) / (vmrzeq - vmrzeq10)
            frpe = max(frpe, 0.0_dp)
            fmpe = sqrt(frpe) * fmpe
         end if
         vmrzac = vmrzpt + dtgw * fmpe
!
         if (vmrzac > vmrzeq) then
!                    Reduce capillary rise because moisture content
!                    exceeds equilibrium
            fmpe = ((1.0_dp - frrdca) * (vmrzeq - vmrzpt)) / dtgw
            vmrzac = vmrzpt + dtgw * fmpe
         end if
      end if
!
      if (debug_unit > 0) then
         write (debug_unit, '(1x,a)') 'End SIMGRO_OVZ'
         write (debug_unit, *)
      end if
!
      if (id /= 0) call CloseGP(id)
!
3900  return
!
555   format(/'Argument ', A, ' out of range; is ', I4, ' but should be 1 <= n <= ', I3)
!
   end

!>     Deze functie berekent de actuele verdamping. De verdamping wordt
!>     berekend per landgebruik. Als het landgebruik open water is wordt geen correctie
!>     factor voor de hoeveelheid vocht in de onverzadigde zone berekend. De potentiele
!>     verdamping is dan de actuele.
!>     Bij andere landgebruiken kan nog worden gecorrigeerd voor inundatie.
   function ActualEvaporation(fmevptte, frev1, frev2, frev3, &
                                            frev4, frev5, epf, &
                                            IsSurfaceWater, &
                                            fmevptsw, frswtp, icselo) result(res)
   
!
! Arguments
!
      real(kind=dp), intent(in) :: fmevptte !< Potential evaporation
      real(kind=dp), intent(in) :: frev1 !< Reductionfactor at 100 % moisture in unsatured zone
      real(kind=dp), intent(in) :: frev2 !< Reductionfactor
      real(kind=dp), intent(in) :: frev3 !< Reductionfactor at 5 mm evaporation a day
      real(kind=dp), intent(in) :: frev4 !< Reductionfactor at 1 mm evaporation a day
      real(kind=dp), intent(in) :: frev5 !< Reductionfactor at zero contents of iunsat zone
      real(kind=dp), intent(in) :: epf !< Actuele fractie van inhoude wortelzone

      logical, intent(in) :: IsSurfaceWater !< Is dit gewas oppervlaktewater
      real(kind=dp), intent(in) :: fmevptsw !< Evaporation surface water
      real(kind=dp), intent(in) :: frswtp !< Fraction surfacewater (inundation)
      real(kind=dp), intent(in) :: icselo !< Sensitivity for water logging (4 = reed)

      real(kind=dp) :: res !< Result value for actual evaporation.
!
! Local variables
!
      real(kind=dp) :: e1, e2, e3, e4 ! Points of graph for calculating redunction
      real(kind=dp) :: beta ! Reductionfactor
      real(kind=dp) :: de
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
         if (fmevptte > 5.0e-3_dp) then
!
!            Potentiele verdamping mag niet groter dan 5.0e-3
!
            e3 = frev3
         else if (fmevptte < 1.0e-3_dp) then
!
!            Potentiele verdamping mag niet kleiner dan 5.0e-3
!
            e3 = frev4
         else
!
!            Interpoleer tussen 1.0e-3 en 5.0e-3 (die grafiek)
!
            e3 = frev4 * (5.0e-3_dp - fmevptte) / 4.0e-3_dp + frev3 * (fmevptte - 1.0e-3_dp) / 4.0e-3_dp
         end if
!
! Bereken nu de correctiefactor. Deze ligt tussen 0.0 en 1.0
!
!      epf = vmrzdcte / veq0  ! Actuele fractie van de inhoud van wortelzone
         e1 = frev1
         e2 = frev2
         e4 = frev5
         beta = 1.0_dp ! Reductie factor van potentieel naar actueel
         if (epf < e4) then ! VARIABELE E4 <<<====================
            beta = 0.0
         elseif (epf < e3) then
            beta = (epf - e4) / (e3 - e4)
         end if
!
         if (epf > (e2 + 0.0001_dp) .and. epf < e1) then
            beta = (e1 - epf) / (e1 - e2)
         end if
         if (epf > (e1 + 0.0001_dp)) then
            if (e1 > 0.99_dp) then
               beta = 1.0_dp
            else
               beta = 0.0
            end if
         end if
!
         if (beta > 1.00_dp) beta = 1.0_dp
!
!                 The actual evapotranspirations depends on
!                 potential evapotranspiration
!                 reduction factor
!                 fraction inudation
!
         if (icselo == 4) then
            de = beta * fmevptte
         else
            de = beta * fmevptte * (1._dp - frswtp) + fmevptsw * frswtp
         end if
      end if
!
! End
!
      res = de
   end

   function GetRRin() result(res)
      real(kind=dp)  res
      res = -9.99e9_dp
   end

   integer function GetIIin()
      getIIin = -999999
   end

!>     Opens input file.
   subroutine openfile(nm, filenam, ex, ib, id)
!
      use globals
! Arguments
!
      character(len=*) :: filenam
      integer :: nm
      logical :: ex
      integer :: ib
      integer :: id
!
! Local variables
!
      integer :: ios
!
! Begin
!
      if (id > 0) write (id, '(1x,a)') 'Using file '//filenam(1:len_trim(filenam))
      inquire (file=filenam, exist=ex)
      if (ex) then
         open (newunit=nm, file=filenam, status='OLD', iostat=ios)
         if (ios /= 0) then
            write (ib, '(1x,2a)') 'Fout bij openen ', filenam(1:len_trim(filenam))
         else
            maxFileUnitNumber = max(maxFileUnitNumber, nm)
            minFileUnitNumber = min(minFileUnitNumber, nm)
         end if
      else
         write (ib, 23) filenam(1:len_trim(filenam))
      end if
!
23    format(/, '** E23  readvar  **', 1X, ' Cannot open table ', A, '.inp for read')
!
   end
!
!>      Opens input file.
   subroutine openmesg(nm, filenam, status)

      use globals
!
! Arguments
!
      character(len=*) :: filenam
      integer :: nm
      integer :: status
!
! Local variables
!
      integer :: ios
!
! Begin
!
      open (newunit=nm, file=filenam, status='unknown', access='append', iostat=ios)
      if (ios /= 0) then
         write (*, '(1x,2a)') 'Fout bij openen ', filenam(1:len_trim(filenam))
         status = status + 1
      else
         maxFileUnitNumber = max(maxFileUnitNumber, nm)
         minFileUnitNumber = min(minFileUnitNumber, nm)
      end if
!
   end
!>     Checks whether string is blank.
    function isbl(chline, bg, ed) result(res)
!
      logical * 4 :: res
      integer*4 :: bg, ed, ii
      character(len=*) :: chline
!
      res = .true.
      do ii = bg, ed
         if (chline(ii:ii) /= ' ') then
            res = .false.
         end if
      end do
!
   end

!>     Reads input file luse_sim.inp
   subroutine readluse(dirnam, ib, id, nm, nxspun, nxte, icselo, frev, Status)
!
!
! Arguments
!
      integer :: ib ! Message unit
      integer :: id ! debug unit
      character(len=*) :: dirnam
      integer :: nxte
      integer :: nxspun
      integer :: icselo(nxte)
      real(kind=dp) :: frev(nxspun, nxte, 5)
      integer :: Status
!
! Local variables
!
      logical :: ex
      logical :: er
      character(len=132) :: nafi
      character(len=132) :: chline
      integer :: nuwa
      integer :: nuer
      logical :: kyer
      integer :: nmli
      integer :: nmlicm
      integer :: nm
      integer :: nt
      integer :: ns
      integer :: iiin
      integer :: r_nmteex
      character(len=15) :: r_tena
      integer :: r_ipfa
      integer :: r_ipagte
      real(kind=dp) :: r_frpric
      integer :: r_icselo
      real(kind=dp) :: RRin
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
      nafi = dirnam(1:len_trim(dirnam))//'luse_sim.inp'
      if (id > 0) write (id, '(1x,a)') 'Start readLUSE'
      call openfile(nm, nafi, ex, iB, id)
!
      if (.not. ex) goto 3900
!
!        Initialize arrays of table TECH
!
2400  do nt = 1, nxte
         icselo(nt) = iiin
      end do
!
!        Read records from LUSE
!        Check whether variable is okay
!        And put the variable in its array
!
      nmli = 0
      nmlicm = 0
!
2455  read (nm, 501, end=2450) chline
501   format(A132)
      if (chline(1:1) == '*' .or. chline(2:2) == '*') then
         nmlicm = nmlicm + 1
         goto 2455
      else
         nmli = nmli + 1
         kyer = .false.
!
         read (chline, 614) r_nmteex, r_tena, r_ipfa, r_ipagte, r_frpric, r_icselo
614      format(I6, A15, 5X, 2I6, F8.0, I6, F8.0, 2F6.0, 3F8.0)

!
         if (.not. ctrl_int('luse_sim', nmli + nmlicm, r_nmteex, 1, 999, 3, 'r_nmteex', 'external technology number', ib)) then
            nuer = nuer + 1
            kyer = .true.
            goto 3900
         else
!               nmteex(nmli) = r_nmteex
            r_nmteex = nmli
         end if

         if (.not. isbl(chline, 47, 52)) then
            if (.not. ctrl_int('luse_sim', nmli + nmlicm, r_icselo, 1, 4, 3, 'r_icselo', 'sensitivity for water logging', ib)) then
               nuwa = nuwa + 1
            elseif (.not. kyer) then
               icselo(r_nmteex) = r_icselo
            end if
         end if
!
      end if
!
      goto 2455
2450  continue
!
!        The pointers for urban area, decedious forest, pine fores and
!        surface water may not be identical, but have to exist.
!
!
2470  continue
!
!        Test whether arrays are complete
!
      do nt = 1, nxte
         if ((icselo(nt) - iiin) == 0) then
            icselo(nt) = 3
         end if
      end do
      !
!        Initialize array frev, frirbg, frired
!
      do nt = 1, nxte
         do ns = 1, nxspun
            frev(ns, nt, 3) = 0.60
            frev(ns, nt, 4) = 0.20
            frev(ns, nt, 5) = 0.05
            if (icselo(nt) == 1) then
               frev(ns, nt, 1) = 1.00
               frev(ns, nt, 2) = 0.95
            elseif (icselo(nt) == 2) then
               frev(ns, nt, 1) = 0.95
               frev(ns, nt, 2) = 0.90
            else
               frev(ns, nt, 1) = 1.00
               frev(ns, nt, 2) = 1.00
            end if
         end do
      end do
!
!        end of read-statements
!
!        If the number of fatal errors is 1 or more, the PROGRAM will
!        be discontinued
!
3900  if (nuwa > 0 .or. nuer > 0) then
         er = .true.
      else
         er = .false.
      end if
!
      status = nuwa + nuer

      if (ex .or. .not. ex) call CloseGP(nm)
!

!
   end

   !>     Reads input file root_sim.inp
   subroutine readroot(file_unit, filenam, message_unit, message_file, &
                       debug_unit, debug_file, Status, nxspun, nxte, dprz, frev)

      use globals
!
!
! Arguments
!
      integer :: nxspun
      integer :: nxte
      real(kind=dp) :: dprz(nxspun, nxte)
      real(kind=dp) :: frev(nxspun, nxte, 5)
      integer :: file_unit, nm
      integer :: message_unit, ib
      integer :: debug_unit, id
      character(len=*) :: filenam, message_file, debug_file
      integer :: Status
!
! Local variables
!
      logical :: er
      logical :: ex
      integer :: nmli = 0
      integer :: nmlicm = 0
      integer :: nuwa = 0
      integer :: nuer = 0
      character(len=132) :: chline
      logical :: kyer
      integer :: ns
      integer :: nt
      real(kind=dp) :: r_dprz
      real(kind=dp) :: r_frev01
      real(kind=dp) :: r_frev02
      real(kind=dp) :: r_frev03
      real(kind=dp) :: r_frev04
      real(kind=dp) :: r_frev05
      integer :: r_spun
      integer :: r_nmteex
      integer :: ii
      integer :: ios
!
! Functions
!
!
! Init
!
      NuWa = 0
      NuEr = 0
!
      nm = file_unit
      ib = message_unit
      id = debug_unit
      open (newunit=ib, file=message_file, status='unknown', access='append', iostat=ios)
      if (ios /= 0) then
         write (*, '(1x,2a)') 'Fout bij openen ', message_file(1:len_trim(message_file))
         nuer = nuer + 1
         goto 3900
      end if
      maxFileUnitNumber = max(maxFileUnitNumber, ib)
      minFileUnitNumber = min(minFileUnitNumber, ib)
      if (id /= 0) then
         open (newunit=id, file=debug_file, status='unknown', access='append', iostat=ios)
         if (ios /= 0) then
            write (ib, '(1x,2a)') 'Fout bij openen ', debug_file(1:len_trim(debug_file))
            nuer = nuer + 1
            goto 3900
         end if
         maxFileUnitNumber = max(maxFileUnitNumber, id)
         minFileUnitNumber = min(minFileUnitNumber, id)
         debug_unit = id
      end if
!
! Begin
!
      call openfile(nm, filenam, ex, ib, id)
!
      if (.not. ex) then
         nuer = nuer + 1
         goto 3900
      end if
!
!        Initialize arrays of table ROOT
!
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
3055  read (nm, 501, end=3050) chline
501   format(A132)

      if (chline(1:1) == '*' .or. chline(2:2) == '*') then
         nmlicm = nmlicm + 1
         goto 3055
      else
         nmli = nmli + 1
         kyer = .false.
!
         read (chline, 618) r_spun, r_nmteex, r_dprz, r_frev01, r_frev02, r_frev03, r_frev04, r_frev05 !,r_frirbg,r_frired,
618      format(2I6, 10F8.0)

         ns = r_spun
         if (ns < 1 .or. ns > nxspun) then
            write (ib, 28) 'soil physical units', 'root_sim', nxspun
28          format(/'** E28  readvar  **', 1X, ' Maximum number of ', A, ' exceeded in table ', A, '.inp', /, 20X, &
                    ' According to input parameter maximum is: ', I5)
            nuer = nuer + 1
         else
            if (ns > nuspun) nuspun = ns
         end if
!
         nt = r_nmteex
         if (nt < 1 .or. nt > nxte) then
            nuer = nuer + 1
            write (ib, 28) 'land use types', 'root_sim', nxte
         else
            if (nt > nute) nute = nt
         end if
!
         if (.not. ctrl_real('root_sim', nmli + nmlicm, r_dprz, 1.0e-1_dp, 1.0e1_dp, 2, 'r_dprz', 'root zone depth', ib)) then
            nuwa = nuwa + 1
         elseif (.not. kyer) then
            dprz(r_spun, r_nmteex) = r_dprz
         end if
!
         if (.not. isbl(chline, 21, 28)) then
            if (.not. ctrl_real('root_sim', nmli + nmlicm, r_frev01, 0.0_dp, 1.0_dp, 2, 'r_frev', 'evaporation fraction', ib)) then
               nuwa = nuwa + 1
            elseif (.not. kyer) then
               frev(r_spun, r_nmteex, 1) = r_frev01
            end if
         end if
!
         if (.not. isbl(chline, 29, 36)) then
            if (.not. ctrl_real('root_sim', nmli + nmlicm, r_frev02, 0.0_dp, 1.0_dp, 2, 'r_frev', 'evaporation fraction', ib)) then
               nuwa = nuwa + 1
            elseif (.not. kyer) then
               frev(r_spun, r_nmteex, 2) = r_frev02
            end if
         end if
!
         if (.not. isbl(chline, 37, 44)) then
            if (.not. ctrl_real('root_sim', nmli + nmlicm, r_frev03, 0.0_dp, 1.0_dp, 2, 'r_frev', 'evaporation fraction', ib)) then
               nuwa = nuwa + 1
            elseif (.not. kyer) then
               frev(r_spun, r_nmteex, 3) = r_frev03
            end if
         end if
!
         if (.not. isbl(chline, 45, 52)) then
            if (.not. ctrl_real('root_sim', nmli + nmlicm, r_frev04, 0.0_dp, 1.0_dp, 2, 'r_frev', 'evaporation fraction', ib)) then
               nuwa = nuwa + 1
            elseif (.not. kyer) then
               frev(r_spun, r_nmteex, 4) = r_frev04
            end if
         end if
!
         if (.not. isbl(chline, 53, 60)) then
            if (.not. ctrl_real('root_sim', nmli + nmlicm, r_frev05, 0.0_dp, 1.0_dp, 2, 'r_frev', 'evaporation fraction', ib)) then
               nuwa = nuwa + 1
            elseif (.not. kyer) then
               frev(r_spun, r_nmteex, 5) = r_frev05
            end if
         end if
!

      end if
!
      goto 3055
3050  continue
!
!        Test whether arrays are complete
!        and test whether the soil moisture fractions are consistent
!
      do ns = 1, nxspun
         do nt = 1, nxte
            do ii = 1, 4
               if ((frev(ns, nt, ii) - frev(ns, nt, ii + 1)) < -1.0e-4_dp) then
                  write (ib, 58) ns, nt !spunex(ns),nmteex(nt)
58                format(/, '** E58  readvar  **', 1X, &
                          ' Inconsistency in root_sim.inp ', /, 20X, &
                          ' in evapotranspiration or irrigation fractions', /, 20X, &
                          ' for soil physical unit ', I5, ' and land use ', I5)
               end if
            end do
         end do
      end do

!        If the number of fatal errors is 1 or more, the PROGRAM will
!        be discontinued
!
3900  if (nuwa > 0 .or. nuer > 0) then
         er = .true.
      else
         er = .false.
      end if
      Status = Nuer + Nuwa
!
      if (ex .or. .not. ex) call CloseGP(nm)
   end

   !>     Reads input file unsa_sim.inp.
   subroutine readunsa(file_unit, filenam, message_unit, message_file, &
                       debug_unit, debug_file, Status, &
                       nxspun, nxte, nxrz, nxdpun, srrz, fmca, scsa, &
                       dpgwun, dprzun, nudpun)
      use globals
      
! Arguments
!
      integer :: message_unit, ib
      integer :: debug_unit, id
      integer :: nxrz
      integer :: nxspun
      integer :: nxte
      integer :: Status
      integer :: nxdpun
      character(len=*) :: filenam, message_file, debug_file
      integer :: file_unit, nm
      real(kind=dp) :: srrz(nxspun, nxrz, nxdpun)
      real(kind=dp) :: fmca(nxspun, nxrz, nxdpun)
      real(kind=dp) :: dpgwun(nxspun, nxrz, nxdpun)
      real(kind=dp) :: dprzun(nxrz)
      real(kind=dp) :: scsa(nxspun, nxrz, nxdpun)
      integer :: nudpun(nxspun, nxrz)
!
! Local variables
!
      logical :: ex, er
      integer :: ii
      real(kind=dp) :: rrin
      integer :: nuwa
      integer :: nuer
      logical :: kyer
      integer :: nmli
      integer :: nmlicm
      character(len=132) :: chline
      integer :: rz
      integer :: iiin
      integer :: ns
      integer :: r_spun
      integer :: r_dprzun
      real(kind=dp) :: r_dpgwun
      real(kind=dp) :: r_srrz
      real(kind=dp) :: r_srrzwc
      real(kind=dp) :: r_fmca
      real(kind=dp) :: r_scsa
      real(kind=dp) :: r_scsapl
      integer :: artp(nxrz + nxdpun) ! Dynamic array
      integer :: ix(nxrz + nxdpun) ! Dynamic array
      real(kind=dp) :: fmcatp(nxrz + nxdpun)
      real(kind=dp) :: scsatp(nxrz + nxdpun)
      real(kind=dp) :: srrztp(nxrz + nxdpun)
      real(kind=dp) :: dpgwuntp(nxrz + nxdpun)
      integer :: i378
      integer :: ios
!
! Functions
!
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
         open (newunit=ib, file=message_file, status='unknown', access='append', iostat=ios)
         if (ios /= 0) then
            write (*, '(1x,2a)') 'Fout bij openen ', message_file(1:len_trim(message_file))
            nuer = nuer + 1
            goto 3900
         end if
         message_unit = ib
         maxFileUnitNumber = max(maxFileUnitNumber, ib)
         minFileUnitNumber = min(minFileUnitNumber, ib)
      end if
      if (id > 0) then
         open (newunit=id, file=debug_file, status='unknown', access='append', iostat=ios)
         if (ios /= 0) then
            write (ib, '(1x,2a)') 'Fout bij openen ', debug_file(1:len_trim(debug_file))
            nuer = nuer + 1
            goto 3900
         end if
         maxFileUnitNumber = max(maxFileUnitNumber, id)
         minFileUnitNumber = min(minFileUnitNumber, id)
         debug_unit = id
      end if
!
! Begin
!
      RRin = GetRRin()
      IIin = GetIIin()
      nurz = 0
!
      nm = 11
      call openfile(nm, filenam, ex, ib, id)
!
      if (.not. ex) then
         nuer = nuer + 1
         goto 3900
      end if
!
!        Initialize arrays of table unsa
!
      nurz = 0
2500  do rz = 1, nxrz
         dprzun(rz) = iiin
         do ns = 1, nxspun
            nudpun(ns, rz) = 0
            do ii = 1, nxdpun
               dpgwun(ns, rz, ii) = rrin
               srrz(ns, rz, ii) = rrin
               fmca(ns, rz, ii) = rrin
               scsa(ns, rz, ii) = rrin
            end do
         end do
      end do

      !
!        Read records from unsa
!        Check whether variable is okay
!        And put the variable in its array
!
      nmli = 0
      nmlicm = 0
!
2555  read (nm, 501, end=2550) chline
501   format(A132)

      if (chline(1:1) == '*' .or. chline(2:2) == '*') then
         nmlicm = nmlicm + 1
         goto 2555
      else
         nmli = nmli + 1
         kyer = .false.
!
         read (chline, 615) r_spun, r_dprzun, r_dpgwun, r_srrz, r_srrzwc, r_fmca, r_scsa, r_scsapl
615      format(2I6, 6F8.0)
!
         if (r_spun < 1 .or. r_spun > nxspun) then
            nuer = nuer + 1
            write (ib, 28) 'soil physical units', 'unsa_sim', nxspun
28          format(/, '** E28  readvar  **', 1X, ' Maximum number of ', A, ' exceeded in table ', A, '.inp', /, 20X, &
                    ' According to input parameter maximum is: ', I5)
         end if
!
         if (r_dprzun >= 0) then
            i378 = IndexInRealList(dble(r_dprzun), dprzun, nurz)
            if (i378 < 0) then
               nurz = nurz + 1
               if (nurz > nxrz) then
                  nurz = nurz - 1
                  write (ib, 28) 'root zones', 'unsa_sim', nxrz
                  nuer = nuer + 1
                  goto 3900
               end if
               dprzun(nurz) = r_dprzun
               r_dprzun = nurz
            else
               r_dprzun = i378
            end if
         else
            nuer = nuer + 1
            kyer = .true.
            goto 3900
         end if
!
         if (.not. ctrl_real('unsa_sim', nmli + nmlicm, r_dpgwun, -1.0_dp, 1.0_dp, 2, 'r_dpgwun', 'depth of groundwater', ib)) then
            kyer = .true.
            nuer = nuer + 1
         elseif (.not. kyer) then
            nudpun(r_spun, r_dprzun) = nudpun(r_spun, r_dprzun) + 1
            if (nudpun(r_spun, r_dprzun) > nxdpun) then
               write (ib, 28) 'depths', 'unsa_sim', nxdpun
               nudpun(r_spun, r_dprzun) = nudpun(r_spun, r_dprzun) - 1
               nuwa = nuwa + 1
            end if
            ii = nudpun(r_spun, r_dprzun)
            dpgwun(r_spun, r_dprzun, ii) = r_dpgwun
         end if
!
         if (.not. ctrl_real('unsa_sim', nmli + nmlicm, r_srrz, 0.0_dp, 5.0_dp, 2, 'r_srrz', 'storage root zone drying conditions', ib)) then
            nuwa = nuwa + 1
         elseif (.not. kyer) then
            srrz(r_spun, r_dprzun, ii) = r_srrz
         end if
         if (.not. ctrl_real('unsa_sim', nmli + nmlicm, r_fmca, 0.0_dp, 1.0_dp, 2, 'r_fmca', 'capillary rise flux (mm/d)', ib)) then
            nuwa = nuwa + 1
         elseif (.not. kyer) then
            fmca(r_spun, r_dprzun, ii) = r_fmca
         end if
!
         if (.not. ctrl_real('unsa_sim', nmli + nmlicm, r_scsa, 0.0_dp, 1.0_dp, 2, 'r_scsa', 'storage coefficient', ib)) then
            nuwa = nuwa + 1
         elseif (.not. kyer) then
            scsa(r_spun, r_dprzun, ii) = r_scsa
         end if
!
      end if
!
      goto 2555
2550  continue
!
!        Sort the arrays on both root zone depths and ground water depths
!
      do ns = 1, nuspun
         do rz = 1, nurz
!
            if (nudpun(ns, rz) > 0) then
               do ii = 1, nudpun(ns, rz)
                  artp(ii) = nint(1000 * dpgwun(ns, rz, ii))
                  srrztp(ii) = srrz(ns, rz, ii)
                  fmcatp(ii) = fmca(ns, rz, ii)
                  scsatp(ii) = scsa(ns, rz, ii)
               end do
               call indexx(nudpun(ns, rz), artp, ix)
               do ii = 1, nudpun(ns, rz)
                  dpgwun(ns, rz, ii) = artp(ix(ii)) / 1000.
                  srrz(ns, rz, ii) = srrztp(ix(ii))
                  fmca(ns, rz, ii) = fmcatp(ix(ii))
                  scsa(ns, rz, ii) = scsatp(ix(ii))
               end do
            end if
!
         end do
      end do
!
      do rz = 1, nurz
         artp(rz) = dprzun(rz)
      end do
!
      call indexx(nurz, artp, ix)
!
      do rz = 1, nurz
         dprzun(rz) = artp(ix(rz))
      end do
!
      do ns = 1, nuspun
         do ii = 1, nxdpun
!
            do rz = 1, nurz
               dpgwuntp(rz) = dpgwun(ns, rz, ii)
               srrztp(rz) = srrz(ns, rz, ii)
               fmcatp(rz) = fmca(ns, rz, ii)
               scsatp(rz) = scsa(ns, rz, ii)
            end do
            do rz = 1, nurz
               dpgwun(ns, rz, ii) = dpgwuntp(ix(rz))
               srrz(ns, rz, ii) = srrztp(ix(rz))
               fmca(ns, rz, ii) = fmcatp(ix(rz))
               scsa(ns, rz, ii) = scsatp(ix(rz))
            end do
         end do
      end do
!
      do ns = 1, nuspun
         do rz = 1, nurz
            artp(rz) = nudpun(ns, rz)
         end do
!
         do rz = 1, nurz
            nudpun(ns, rz) = artp(ix(rz))
         end do
      end do
!
!        Test whether arrays are complete
!
      do ns = 1, nuspun
         do rz = 1, nurz
            if (nudpun(ns, rz) == 0) then
               write (ib, 37) 'unsa_sim', ns, nint(dprzun(rz))
37             format(/, '** E37  readvar  **', 1X, 1X, A, '.inp; missing input', /, 20X, &
                       ' soil physical unit ', I5, ' and root zone depth ', I4)
               nuer = nuer + 1
!
            else
               if (dpgwun(ns, rz, 1) > 1.0e-4_dp) then
                  write (ib, 26) 'unsa_sim', 'dpgwun', 'ns', ns, 'rz', nint(dprzun(rz))
26                format(/, '** E26  readvar  **', 1X, &
                          ' Table ', A, '.inp; Missing variable ', A, ' with position:', &
                          5(/21X, A, ' = ', I5))
                  nuwa = nuwa + 1
               end if
!
               do ii = 2, nudpun(ns, rz)
                  if ((srrz(ns, rz, ii - 1) - srrz(ns, rz, ii)) < -1.0e-4_dp) then
                     write (ib, 38) 'unsa_sim', ns, nint(dprzun(rz)), &
                        dpgwun(ns, rz, ii - 1), srrz(ns, rz, ii - 1), dpgwun(ns, rz, ii), srrz(ns, rz, ii)
38                   format(/, '** E38  readvar  **', 1X, 1X, A, '.inp; unit ', I5, ' rzd ', I5, ' drying conditions', /, 20X, &
                             ' Storage in root zone inconsistent with groundwater depth:', &
                             2(/20X, ' depth = ', F10.3, ' storage = ', F10.3))
                     nuwa = nuwa + 1
                  end if
               end do
!
               do ii = 2, nudpun(ns, rz)
                  if (int(100.*dpgwun(ns, rz, ii)) <= dprzun(rz)) then
                     fmca(ns, rz, ii) = fmca(ns, rz, 1)
                  end if
                  if ((fmca(ns, rz, ii - 1) - fmca(ns, rz, ii)) < -1.0e-4_dp) then
                     write (ib, 40) 'unsa_sim', ns, nint(dprzun(rz)), &
                        dpgwun(ns, rz, ii - 1), fmca(ns, rz, ii - 1), dpgwun(ns, rz, ii), fmca(ns, rz, ii)
40                   format(/, '** E40  readvar  **', 1X, 1X, A, '.inp; unit ', I5, ' rzd ', I5 / 20X, &
                             ' Capillary rise flux inconsistent with groundwater depth:', &
                             2(/20X, ' depth = ', F10.3, ' cap. rise = ', F10.3))
                     nuwa = nuwa + 1
                  end if
               end do
!
               do ii = 1, nudpun(ns, rz)
                  if (abs(scsa(ns, rz, ii) - rrin) < 1.0e-4_dp) then
                  end if
                  if (srrz(ns, rz, ii) > 1.0e-4_dp) then
                     srrz(ns, rz, ii) = srrz(ns, rz, ii) / 1000.
                  end if
                  if (fmca(ns, rz, ii) > 1.0e-4_dp) then
                     fmca(ns, rz, ii) = fmca(ns, rz, ii) / 1000.
                  end if
               end do
!
            end if
         end do
      end do
!
!        end of read-statements
!
!        If the number of fatal errors is 1 or more, the PROGRAM will
!        be discontinued
!
3900  if (nuwa > 0 .or. nuer > 0) then
         er = .true.
      else
         er = .false.
      end if
      Status = Nuer + Nuwa
!
      if (ex .or. .not. ex) call CloseGP(nm)
!

!
   end
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
      real(kind=dp) :: x
      real(kind=dp) :: l(*)
      integer :: n
!
! local variables
!
      integer :: i
!
! Begin
!
      IndexInRealList = -1
      do i = 1, n
         if (abs(l(i) - x) < 1.0e-6) then
            IndexInRealList = i
            return
         end if
      end do
!
! End
!
   end
!
!>     function unsa calculates the capillary rise,
!>     equilibrium moisture content for drying conditions or storage coefficient
!>     for groundwater depth dp of soil physical unit ns and land use nt
!>
!>     If the groundwater depth is above or below the tabulates depths,
!>     the outcome is kept constant at the extreme value
   function unsa(ns, nt, groundwater_depth, dproot, unda, &
                               debug_unit, message_unit, status, &
                               nxte, nxspun, nxrz, nxdpun, &
                               nurz, dprzun, dpgwun, nudpun) result(res)
      
      real(kind=dp) :: res
      integer*4 :: ns, nt
      integer :: debug_unit
      integer :: message_unit
      integer :: status
      integer :: nxte
      integer :: nxspun
      integer :: nxrz, nurz
      integer :: nxdpun

      real(kind=dp) :: groundwater_depth, dproot

      real(kind=dp) :: unda(nxspun, nxrz, nxdpun) ! Unsaturated data (Eq. moist. cont. etc)
      real(kind=dp) :: Dprzun(nxrz)
      real(kind=dp) :: Dpgwun(nxspun, nxrz, nxdpun)
      integer :: Nudpun(nxspun, nxrz)

      integer*4 :: ii, i1
      real(kind=dp) :: dprztp ! wortelzone in cm
      real(kind=dp) :: dpgwtp ! Lokale variabele voor diepte grondwaterstand
      real(kind=dp) :: va01, va02
      real(kind=dp) :: fr
!
!        Setup of subroutine:
!
!        1 initialize temporary variables
!        2 determine both root zone depths for interpolation
!        3 calculate eq. moisture content/capill. rise for both root zones
!        4 interpolate between root zones
!
!        The variable temporary depth (DPGWTP) is used to manipulate groundwater_depth only
!        within this subroutine
!
!        1 Set variables
!
      dprztp = dproot * 100
      dpgwtp = groundwater_depth
!
!        2 Calculate root zones
!
!
!  Uit unsa_SIM.INP komt de lijst dprzun. Deze lijst bevat per spun
!  een rootzone en een gwst.
!  Key = (spun, dprz, dpgw).
!
      if (nurz > 1) then

         do ii = 1, nurz
            i1 = ii - 1
            if (dprztp < dprzun(ii)) go to 2000
         end do
!
!        Obviously the root zone depth is below the minumum tabulated
!        value; the VALU is therefore calculated for the
!        deepest possible value;
!
         i1 = nurz - 1
         dprztp = dprzun(nurz)
!
2000     if (ii == 1) then
!
!           The  root zone is above the highest tabulated one;
!           the variables are therefore calculated for the
!           highest possible value
!
            i1 = 1
            dprztp = dprzun(1)
         end if

!
!        3 determine values for both root zones
!
         va01 = unfudp(ns, i1 + 1, dpgwtp, unda, dpgwun, nudpun, nxspun, nxrz, nxdpun)
         va02 = unfudp(ns, i1, dpgwtp, unda, dpgwun, nudpun, nxspun, nxrz, nxdpun)
!
!        4 Perform interpolations
!
         fr = dprzun(i1 + 1) - dprztp
         fr = fr / (dprzun(i1 + 1) - dprzun(i1))
         res = va01 + (fr * (va02 - Va01))

      else
         res = 0.0_dp
      end if
!
   end

!>     Calculates the capillary rise flux, equilibrium
!>     moisture content of the root zone for drying or storage coefficient.
!>     The matrix for the interpolation is unda (unsaturated data).
   real(kind=dp) function unfudp(ns, rz, groundwater_depth, unda, dpgwun, nudpun, nxspun, nxrz, nxdpun)
      integer :: nxspun, nxrz, nxdpun
      real(kind=dp) :: dpgwun(nxspun, nxrz, nxdpun)
      integer :: nudpun(nxspun, nxrz)
      integer*4 :: ns, rz
      real(kind=dp) :: groundwater_depth, unda(nxspun, nxrz, nxdpun)

!
      integer*4 :: i1, ii
      real(kind=dp) :: dpgwtp, fr

      !        1 Set variables
!
      dpgwtp = groundwater_depth
!
!        2 Calculate groundwater levels for interpolation
!
      if (nudpun(ns, rz) > 1) then

         do ii = 1, nudpun(ns, rz)
            i1 = ii - 1
            if (dpgwtp < dpgwun(ns, rz, ii)) go to 2000
         end do
!
!        Obviously the groundwater table is below the minumum tabulated
!        value; the eq. moisture content is therefore calculated for the
!        deepest possible value; as is the capillary rise flux.
!
         i1 = nudpun(ns, rz) - 1
         dpgwtp = dpgwun(ns, rz, nudpun(ns, rz))
!
2000     if (ii == 1) then
!
!           The  root zone is above the highest tabulated one;
!           the variables are therefore calculated for the
!           highest possible value
!
            i1 = 1
            dpgwtp = dpgwun(ns, rz, 1)
         end if

!
!        4 Perform interpolation
!
         fr = dpgwun(ns, rz, i1 + 1) - dpgwtp
         fr = fr / (dpgwun(ns, rz, i1 + 1) - dpgwun(ns, rz, i1))
         unfudp = unda(ns, rz, i1 + 1) + fr * (unda(ns, rz, i1) - unda(ns, rz, i1 + 1))

      else
         unfudp = 0.0_dp
      end if
!
   end
!

!>     Controls limits of variables, sets to max/min values if necessary.
!>     If no error condition occurs, function is true
   logical function ctrl_real(nafi, nmli, va, vamn, vamx, tyva, nava, deva, ib)
!
      integer :: ib
      integer*4 :: tyva, nmli
      real(kind=dp) :: va, vamn, vamx, vaod
      character(len=*) :: nava
      character(len=*) :: nafi
      character(len=*) :: deva
!
!
      ctrl_real = .true.
      vaod = va
!
      if (va > vamx) then
         if (tyva == 1) then
            va = vamx
         elseif (tyva == 3) then
            if (nmli > 0) then
               write (ib, 11) nmli, nafi, nava, deva, vaod, vamx
            else
               write (ib, 12) nava, deva, vaod, vamx
            end if
            ctrl_real = .false.
         end if
      elseif (va < vamn) then
         if (tyva == 1) then
            va = vamn
         elseif (tyva == 3) then
            if (nmli > 0) then
               write (ib, 13) nmli, nafi, nava, deva, vaod, vamn
            else
               write (ib, 14) nava, deva, vaod, vamn
            end if
            ctrl_real = .false.
         end if
      end if
!
108   format(/'** W08  ctrl_rea **', 1X, &
              ' Line ', I5, ' of file ', A, '.inp'/20X, &
              ' Variable ', A, 1X, A / 20X, &
              ' is                        ', G11.4 / 20X, &
              ' and exceeds maximum value ', G11.4 / 20X, &
              ' Therefore the variable is set to maximum')
109   format(/'** W09  ctrl_rea **', 1X, &
              ' Line ', I5, ' of file ', A, '.inp'/20X, &
              ' Variable ', A, 1X, A / 20X, &
              ' is                        ', G11.4 / 20X, &
              ' and exceeds maximum value ', G11.4 / 20X, &
              ' Check your input, run will continue with old value')
11    format(/'** E11  ctrl_rea **', 1X, &
              ' Line ', I5, ' of file ', A, '.inp'/20X, &
              ' Variable ', A, 1X, A / 20X, &
              ' is                        ', G11.4 / 20X, &
              ' and exceeds maximum value ', G11.4 / 20X, &
              ' Run will stop due to error condition')
12    format(/'** E12  ctrl_rea **', 1X, &
              ' parameter file'/20X, &
              ' Variable ', A, 1X, A / 20X, &
              ' is                        ', G11.4 / 20X, &
              ' and exceeds maximum value ', G11.4 / 20X, &
              ' Run will stop due to error condition')
!
110   format(/'** W10  ctrl_rea **', 1X, &
              ' Line ', I5, ' of file ', A, '.inp'/20X, &
              ' Variable ', A, 1X, A / 20X, &
              ' is                             ', G11.4 / 20X, &
              ' and is less than minimum value ', G11.4 / 20X, &
              ' Therefore the variable is set to minimum')
111   format(/'** W11  ctrl_rea **', 1X, &
              ' Line ', I5, ' of file ', A, '.inp'/20X, &
              ' Variable ', A, 1X, A / 20X, &
              ' is                             ', G11.4 / 20X, &
              ' and is less than minimum value ', G11.4 / 20X, &
              ' Check your input, run will continue with old value')
13    format(/'** E13  ctrl_rea **', 1X, &
              ' Line ', I5, ' of file ', A, '.inp'/20X, &
              ' Variable ', A, 1X, A / 20X, &
              ' is                             ', G11.4 / 20X, &
              ' and is less than minimum value ', G11.4 / 20X, &
              ' Run will stop due to error condition')
14    format(/'** E14  ctrl_rea **', 1X, &
              ' Parameter file'/20X, &
              ' Variable ', A, 1X, A / 20X, &
              ' is                             ', G11.4 / 20X, &
              ' and is less than minimum value ', G11.4 / 20X, &
              ' Run will stop due to error condition')
!
   end

!>     Controls limits of variables, sets to max/min values if necessary.
!>     If no error condition occurs, function is true
   logical function ctrl_int(nafi, nmli, va, vamn, vamx, tyva, nava, deva, ib)
!
      integer :: ib
      integer*4 :: tyva, nmli
      integer*4 :: va, vamn, vamx, vaod
      character(len=*) :: nava
      character(len=*) :: nafi
      character(len=*) :: deva
!
!
      ctrl_int = .true.
      vaod = va
!
      if (va > vamx) then
         if (tyva == 1) then
            va = vamx
         elseif (tyva == 3) then
            if (nmli > 0) then
               write (ib, 11) nmli, nafi, nava, deva, vaod, vamx
            else
               write (ib, 12) nava, deva, vaod, vamx
            end if
            ctrl_int = .false.
         end if
      elseif (va < vamn) then
         if (tyva == 1) then
            va = vamn
         elseif (tyva == 3) then
            if (nmli > 0) then
               write (ib, 13) nmli, nafi, nava, deva, vaod, vamn
            else
               write (ib, 14) nava, deva, vaod, vamn
            end if
            ctrl_int = .false.
         end if
      end if
!
108   format(/'** W08  ctrl_int **', 1X, &
              ' Line ', I5, ' of file ', A, '.inp'/20X, &
              ' Variable ', A, 1X, A / 20X, &
              ' is                        ', I5 / 20X, &
              ' and exceeds maximum value ', I5 / 20X, &
              ' Therefore the variable is set to maximum')
109   format(/'** W09  ctrl_int **', 1X, &
              ' Line ', I5, ' of file ', A, '.inp'/20X, &
              ' Variable ', A, 1X, A / 20X, &
              ' is                        ', I5 / 20X, &
              ' and exceeds maximum value ', I5 / 20X, &
              ' Check your input, run will continue with old value')
11    format(/'** E11  ctrl_int **', 1X, &
              ' Line ', I5, ' of file ', A, '.inp'/20X, &
              ' Variable ', A, 1X, A / 20X, &
              ' is                        ', I5 / 20X, &
              ' and exceeds maximum value ', I5 / 20X, &
              ' Run will stop due to error condition')
12    format(/'** E12  ctrl_int **', 1X, &
              ' Parameter file'/20X, &
              ' Variable ', A, 1X, A / 20X, &
              ' is                        ', I5 / 20X, &
              ' and exceeds maximum value ', I5 / 20X, &
              ' Run will stop due to error condition')
!
110   format(/'** W10  ctrl_int **', 1X, &
              ' Line ', I5, ' of file ', A, '.inp'/20X, &
              ' Variable ', A, 1X, A / 20X, &
              ' is                             ', I5 / 20X, &
              ' and is less than minimum value ', I5 / 20X, &
              ' Therefore the variable is set to minimum')
111   format(/'** W11  ctrl_int **', 1X, &
              ' Line ', I5, ' of file ', A, '.inp'/20X, &
              ' Variable ', A, 1X, A / 20X, &
              ' is                             ', I5 / 20X, &
              ' and is less than minimum value ', I5 / 20X, &
              ' Check your input, run will continue with old value')
13    format(/'** E13  ctrl_int **', 1X, &
              ' Line ', I5, ' of file ', A, '.inp'/20X, &
              ' Variable ', A, 1X, A / 20X, &
              ' is                             ', I5 / 20X, &
              ' and is less than minimum value ', I5 / 20X, &
              ' Run will stop due to error condition')
14    format(/'** E14  ctrl_int **', 1X, &
              ' Parameter file'/20X, &
              ' Variable ', A, 1X, A / 20X, &
              ' is                             ', I5 / 20X, &
              ' and is less than minimum value ', I5 / 20X, &
              ' Run will stop due to error condition')
!
   end

!>     Indexes in array ARRIN (1:n), i.e. outputs the
!>     array INDX such that ARRIN(INDX(J)) is in ascending order for
!>     j=1,N
   subroutine indexx(n, arrin, indx)
!
      integer*4, intent(in) :: n
      integer*4, dimension(:), intent(in) :: arrin
      integer*4, dimension(:), intent(out) :: indx

      integer*4 :: i, j, l, nr, q
      integer*4 :: indxt
!
      do j = 1, n
         indx(j) = j
      end do
      l = n / 2 + 1
      nr = n
10    continue
      if (l > 1) then
         l = l - 1
         indxt = indx(l)
         q = arrin(indxt)
      else
         indxt = indx(nr)
         q = arrin(indxt)
         indx(nr) = indx(1)
         nr = nr - 1
         if (nr == 1) then
            indx(1) = indxt
            return
         end if
      end if
      i = l
      j = l + l
20    if (j <= nr) then
         if (j < nr) then
            if (arrin(indx(j)) < arrin(indx(j + 1))) j = j + 1
         end if
         if (q < arrin(indx(j))) then
            indx(i) = indx(j)
            i = j
            j = j + j
         else
            j = nr + 1
         end if
         go to 20
      end if
      indx(i) = indxt
      go to 10
!
   end
end module m_capsim
