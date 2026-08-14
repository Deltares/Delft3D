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
module m_espgss
   use m_waq_precision
   use m_calcorder
   use m_logger_helper, only: write_error_message
   use m_demissions_input_checks, only: check_fraction

   implicit none

contains

   subroutine espgss(pmsa, fl, ipoint, increm, noseg, &
                     noflux, iexpnt, iknmrk, noq1, noq2, &
                     noq3, noq4)
!     D-EM Generic Model
!
!
!     Type    Name         I/O Description
!
      real(kind=real_wp) :: pmsa(*) !I/O Process Manager System Array, window of routine to process library
      real(kind=real_wp) :: fl(*) ! O  Array of fluxes made by this process in mass/volume/time
      integer(kind=int_wp) :: ipoint(*) ! I  Array of pointers in pmsa to get and store the data
      integer(kind=int_wp) :: increm(*) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer(kind=int_wp) :: noseg ! I  Number of computational elements in the whole model schematisation
      integer(kind=int_wp) :: noflux ! I  Number of fluxes, increment in the fl array
      integer(kind=int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer(kind=int_wp) :: iknmrk(*) ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer(kind=int_wp) :: noq1 ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer(kind=int_wp) :: noq2 ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer(kind=int_wp) :: noq3 ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer(kind=int_wp) :: noq4 ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      !
      !     NOTE every receptor is a substance!
      !     NOTE all time is in seconds
      !#####################################################################################################
      !###############************************** SO FLUXES IN G/S ******************########################
      !#####################################################################################################
      !
      !     Type    Name         I/O Description                                        Unit
      !
      !     support variables
      integer(kind=int_wp) :: iseg, iflux, ip, ipp, isrc, irec, isegadmin, noriv, ipr, ipd, ierr
      real(kind=real_wp) :: emisvar, losses, flux, ro_mmperday, ra_mmperday, roun_mmperday, &
                            in_mmperday, fwashoff, ferosion, froun, finf, fdisp, over_mmperday
      real(kind=real_wp) :: fluxloss, mass, fluxbound, fluxunbound, fluxinf, fluxroun, fluxero, fluxwash, fluxleak, &
                            fluxexp, fluxrem, fluxeff, fluxsld, fluxbur, fluxeroa, fluxerop, &
                            fluximmo, fluxunc, fluxexfa, fluxexfp, fluxssfa, fluxssfp, fluxmax, fdownstream, fldown
      real(kind=real_wp) :: fluxin, limit, maxflux, pool, solids, qtopact, qtoppas, concact
      real(kind=real_wp) :: FrUnManaged, FrTreated, courant, fr_unc, fr_eff, fr_sld, fr2sew, fr2sfw, fr2soi

      ! fixed quantities
      integer(kind=int_wp), parameter :: scu = 1
      integer(kind=int_wp), parameter :: nrec = 7 !count of compartments (able to receive releases, so "receptors"
      integer(kind=int_wp), parameter :: rec_dww = 1
      integer(kind=int_wp), parameter :: rec_sew = 2
      integer(kind=int_wp), parameter :: rec_pav = 3
      integer(kind=int_wp), parameter :: rec_unp = 4
      integer(kind=int_wp), parameter :: rec_stw = 5
      integer(kind=int_wp), parameter :: rec_sfw = 6
      integer(kind=int_wp), parameter :: rec_soi = 7
      integer(kind=int_wp), parameter :: nopar_srca = 3 ! count of parameters for type A sources
      integer(kind=int_wp), parameter :: ipar_srca_ev = 1
      integer(kind=int_wp), parameter :: ipar_srca_loc = 2
      integer(kind=int_wp), parameter :: ipar_srca_ef = 3
      integer(kind=int_wp), parameter :: nopar_srcb = 2 ! count of parameters for type B sources
      integer(kind=int_wp), parameter :: ipar_srcb_ev = 1
      integer(kind=int_wp), parameter :: ipar_srcb_ef = 2

      ! PMSA admin
      integer(kind=int_wp) :: offset_srca
      integer(kind=int_wp) :: offset_srcb
      integer(kind=int_wp) :: offset_conc
      integer(kind=int_wp) :: lins
      integer(kind=int_wp), parameter :: line = 0
      integer(kind=int_wp), parameter :: loute = 0

      ! Flux admin
      integer(kind=int_wp) :: fl0_atm ! offset atmospheric deposition
      integer(kind=int_wp) :: fl0_srca ! offset releases from sources type A
      integer(kind=int_wp) :: fl0_srcb ! offset releases from sources type B
      integer(kind=int_wp) :: fl0_rest ! offset fluxes along pathways
      integer(kind=int_wp), parameter :: pav2sew = 1 ! definition of fluxes along pathways
      integer(kind=int_wp), parameter :: pav2stw = 2
      integer(kind=int_wp), parameter :: pav2sfw = 3
      integer(kind=int_wp), parameter :: pav2soi = 4
      integer(kind=int_wp), parameter :: pav2dec = 5
      integer(kind=int_wp), parameter :: unp2sfw_er = 6
      integer(kind=int_wp), parameter :: unp2sfw_ro = 7
      integer(kind=int_wp), parameter :: unp2soi_in = 8
      integer(kind=int_wp), parameter :: unp2soi_bu = 9
      integer(kind=int_wp), parameter :: unp2dec = 10
      integer(kind=int_wp), parameter :: sew2sfw_cso = 11
      integer(kind=int_wp), parameter :: sew2stp = 12
      integer(kind=int_wp), parameter :: sew2sfw_eff = 13
      integer(kind=int_wp), parameter :: sew2soi = 14
      integer(kind=int_wp), parameter :: sew2sfw_unc = 15
      integer(kind=int_wp), parameter :: stw2stp = 16
      integer(kind=int_wp), parameter :: stw2sfw = 17
      integer(kind=int_wp), parameter :: stw2soi = 18
      integer(kind=int_wp), parameter :: soi2sfw_ex = 19
      integer(kind=int_wp), parameter :: sop2sfw_ex = 20
      integer(kind=int_wp), parameter :: soi2dec = 21
      integer(kind=int_wp), parameter :: soi2sfw_er = 22
      integer(kind=int_wp), parameter :: sop2sfw_er = 23
      integer(kind=int_wp), parameter :: soi2sop_imm = 24
      integer(kind=int_wp), parameter :: sfw2exp = 25
      integer(kind=int_wp), parameter :: horsfw = 26
      integer(kind=int_wp), parameter :: horsoi = 27
      integer(kind=int_wp), parameter :: horsop = 28
      integer(kind=int_wp), parameter :: dww2sew = 29
      integer(kind=int_wp), parameter :: dww2sfw = 30
      integer(kind=int_wp), parameter :: dww2soi = 31
      integer(kind=int_wp), parameter :: nofluxrest = 31

      ! pointers to input items
      integer(kind=int_wp), parameter :: ip_nsrca = 1
      integer(kind=int_wp), parameter :: ip_nsrcb = 2
      integer(kind=int_wp), parameter :: ip_nrecin = 3
      integer(kind=int_wp), parameter :: ip_delt = 4
      integer(kind=int_wp), parameter :: ip_itime = 5
      integer(kind=int_wp), parameter :: ip_totsurf = 6
      integer(kind=int_wp), parameter :: ip_fpaved = 7
      integer(kind=int_wp), parameter :: ip_funpaved = 8
      integer(kind=int_wp), parameter :: ip_fwater = 9
      integer(kind=int_wp), parameter :: ip_rainfall = 10
      integer(kind=int_wp), parameter :: ip_ropaved = 11
      integer(kind=int_wp), parameter :: ip_rounpaved = 12
      integer(kind=int_wp), parameter :: ip_infilt = 13
      integer(kind=int_wp), parameter :: ip_facerod = 14
      integer(kind=int_wp), parameter :: ip_ero1 = 15
      integer(kind=int_wp), parameter :: ip_ero2 = 16
      integer(kind=int_wp), parameter :: ip_ero3 = 17
      integer(kind=int_wp), parameter :: ip_fcomsew = 18
      integer(kind=int_wp), parameter :: ip_leakage = 19
      integer(kind=int_wp), parameter :: ip_soilthick = 20
      integer(kind=int_wp), parameter :: ip_soilpor = 21
      integer(kind=int_wp), parameter :: ip_kbur = 22
      integer(kind=int_wp), parameter :: ip_kpaved = 23
      integer(kind=int_wp), parameter :: ip_kunpaved = 24
      integer(kind=int_wp), parameter :: ip_kdunpaved = 25
      integer(kind=int_wp), parameter :: ip_ksoil = 26
      integer(kind=int_wp), parameter :: ip_kimmo = 27
      integer(kind=int_wp), parameter :: ip_kdsoil = 28
      integer(kind=int_wp), parameter :: ip_ro_lothr = 29
      integer(kind=int_wp), parameter :: ip_ro_hithr = 30
      integer(kind=int_wp), parameter :: ip_ra_lothr = 31
      integer(kind=int_wp), parameter :: ip_ra_hithr = 32
      integer(kind=int_wp), parameter :: ip_disp_hithr = 33
      integer(kind=int_wp), parameter :: ip_drydep = 34
      integer(kind=int_wp), parameter :: ip_rainconc = 35
      integer(kind=int_wp), parameter :: ip_eff_rs = 36
      integer(kind=int_wp), parameter :: ip_sld_rs = 37
      integer(kind=int_wp), parameter :: ip_frainsew = 38
      integer(kind=int_wp), parameter :: ip_rhodm = 39
      integer(kind=int_wp), parameter :: ip_masspas = 40
      integer(kind=int_wp), parameter :: ip_pop = 41
      integer(kind=int_wp), parameter :: ip_pcww = 42
      integer(kind=int_wp), parameter :: ip_fwwsew = 43
      integer(kind=int_wp), parameter :: ip_fSldgRem = 44
      integer(kind=int_wp), parameter :: ip_FrSeptic = 45
      integer(kind=int_wp), parameter :: ip_FrTreat1 = 46
      integer(kind=int_wp), parameter :: ip_FrTreat2 = 47
      integer(kind=int_wp), parameter :: ip_FrTreat3 = 48
      integer(kind=int_wp), parameter :: ip_Eff_Septic = 49
      integer(kind=int_wp), parameter :: ip_Eff_Treat1 = 50
      integer(kind=int_wp), parameter :: ip_Eff_Treat2 = 51
      integer(kind=int_wp), parameter :: ip_Eff_Treat3 = 52
      integer(kind=int_wp), parameter :: ip_Sld_Septic = 53
      integer(kind=int_wp), parameter :: ip_Sld_Treat1 = 54
      integer(kind=int_wp), parameter :: ip_Sld_Treat2 = 55
      integer(kind=int_wp), parameter :: ip_Sld_Treat3 = 56
      integer(kind=int_wp), parameter :: ip_Exfilt = 57
      integer(kind=int_wp), parameter :: ip_river = 58
      integer(kind=int_wp), parameter :: ip_downstream = 59
      integer(kind=int_wp), parameter :: ip_over_hithr = 60
      integer(kind=int_wp), parameter :: ip_ssurf = 61
      integer(kind=int_wp), parameter :: ip_overland = 62
      integer(kind=int_wp), parameter :: ip_concpas = 63
      integer(kind=int_wp), parameter :: lastsingle = 63

      ! output items
      integer(kind=int_wp), parameter :: louts = 7
      integer(kind=int_wp), parameter :: ip_ew = 1
      integer(kind=int_wp), parameter :: ip_ro2sew = 2
      integer(kind=int_wp), parameter :: ip_ww2sew = 3
      integer(kind=int_wp), parameter :: ip_concwash = 4
      integer(kind=int_wp), parameter :: ip_concroun = 5
      integer(kind=int_wp), parameter :: ip_concsew = 6
      integer(kind=int_wp), parameter :: ip_concdrain = 7

      ! input items
      integer(kind=int_wp) :: nsrca, nsrcb, nrecin
      real(kind=real_wp) :: delt
      integer(kind=int_wp) :: itime
      real(kind=real_wp) :: totsurf, fpaved, funpaved, fwater
      real(kind=real_wp) :: rainfall, ropaved, rounpaved, infilt, exfilt, ssurf, overland
      real(kind=real_wp) :: ero1, ero2, ero3, facerod
      real(kind=real_wp) :: fcomsew, leakage
      real(kind=real_wp) :: rhodm, soilthick, soilpor
      real(kind=real_wp) :: kbur, kpaved, kunpaved, kdunpaved, ksoil, kimmo, kdsoil
      real(kind=real_wp) :: drydep, rainconc
      real(kind=real_wp) :: eff_rs, sld_rs, frainsew
      real(kind=real_wp) :: ro_lothr, ro_hithr, ra_lothr, ra_hithr, disp_hithr, over_hiThr
      real(kind=real_wp) :: masspas
      real(kind=real_wp) :: pop, pcww, fwwsew, fSldgRem, FrSeptic
      real(kind=real_wp) :: FrTreat1, FrTreat2, FrTreat3
      real(kind=real_wp) :: Eff_Septic, Eff_Treat1, Eff_Treat2, Eff_Treat3
      real(kind=real_wp) :: Sld_Septic, Sld_Treat1, Sld_Treat2, Sld_Treat3
      integer(kind=int_wp) :: river
      real(kind=real_wp) :: concpas

      ! output
      real(kind=real_wp) :: ro2sew
      real(kind=real_wp) :: ww2sew
      real(kind=real_wp) :: concwash
      real(kind=real_wp) :: concroun
      real(kind=real_wp) :: concsew
      real(kind=real_wp) :: concdrain

      ! specific other variables

      ! work arrays
      real(kind=real_wp), allocatable :: frac2rec(:) ! allocation of atmospheric deposition
      real(kind=real_wp), allocatable :: frac2recA(:, :, :) ! allocation of releases type A
      real(kind=real_wp), allocatable :: frac2recB(:, :, :) ! allocation of releases type B
      real(kind=real_wp), allocatable :: sumlocator(:) ! sum of the spatial "locator" variable to define type A releases
      real(kind=real_wp), allocatable :: locator(:, :) ! spatial "locator" variable to define type A releases
      real(kind=real_wp), allocatable :: totflxin(:) ! inflow to receptor by releases and from upstream receptors
      real(kind=real_wp), allocatable :: currentmass(:) ! current mass for all receptors
      real(kind=real_wp), allocatable :: emis(:) ! calculated emissions
      real(kind=real_wp), allocatable :: flhorsoi(:), flhorsop(:), flhorsfw(:) ! "buckets" for horizontal transport
      integer(kind=int_wp), allocatable :: seg2riv(:), downstream(:), simorder(:) ! WQ model river cell number (0 - land cell); downstream cell, simulation order

      ! constants for substances
      real(kind=real_wp), allocatable :: emisfacA(:, :)
      real(kind=real_wp) :: emisfacB

      ! files
      integer(kind=int_wp) :: lu_bin
      integer(kind=int_wp) :: lu_txt
      character(len=80), parameter :: filbin = 'outdata_em.bin'
      character(len=80), parameter :: filtxt = 'outdata_em.txt'
      character(len=80) :: par_name

      ! other
      logical first
      data first/.true./

      save
      !
      !******************************************************************************* INITIAL PROCESSING
      if (first) then

         ! pick up actual dimensions
         nsrca = nint(pmsa(ipoint(ip_nsrca)))
         nsrcb = nint(pmsa(ipoint(ip_nsrcb)))
         nrecin = nint(pmsa(ipoint(ip_nrecin)))
         if (nrecin /= nrec) call write_error_message('Receptors inconsistent')

         ! pick up constants
         delt = pmsa(ipoint(ip_delt))
         kbur = pmsa(ipoint(ip_kbur))
         rhodm = pmsa(ipoint(ip_rhodm))
         ro_lothr = pmsa(ipoint(ip_ro_lothr))
         ro_hithr = pmsa(ipoint(ip_ro_hithr))
         ra_lothr = pmsa(ipoint(ip_ra_lothr))
         ra_hithr = pmsa(ipoint(ip_ra_hithr))
         disp_hithr = pmsa(ipoint(ip_disp_hithr))
         facerod = pmsa(ipoint(ip_facerod))
         over_hithr = pmsa(ipoint(ip_over_hithr))

         ! PMSA admin
         offset_srca = lastsingle ! data for sources type A
         offset_srcb = offset_srca + nsrca * (nopar_srca + nrec) ! data for sources type B
         offset_conc = offset_srcb + nsrcb * (nopar_srcb + nrec) ! Concentration
         lins = offset_conc + nrec ! SUM
         if (noflux < (nrec * (1 + nsrca + nsrcb) + nofluxrest)) then ! if other processes have fluxes too, we can not demand equality
            write (*, *) 'nrec  = ', nrec
            write (*, *) 'nsrca = ', nsrca
            write (*, *) 'nsrcb = ', nsrcb
            write (*, *) 'noseg = ', noseg
            write (*, *) 'nofluxrest = ', nofluxrest
            write (*, *) 'noflux = ', noflux
            call write_error_message('NOFLUX has unexpected value')
         end if

         ! Fluxes Admin
         fl0_atm = 0
         fl0_srca = fl0_atm + nrec
         fl0_srcb = fl0_srca + nsrca * nrec
         fl0_rest = fl0_srcb + nsrcb * nrec

         ! set non-time variable properties
         ! Emission factors
         allocate (emisfacA(nsrca, noseg))
         ! allocate(emisfacB(nsrcb,noseg))
         do isrc = 1, nsrca
            ip = offset_srca + (isrc - 1) * (nopar_srca + nrec) + ipar_srca_ef
            ipp = ipoint(ip)
            do iseg = 1, noseg
               emisfacA(isrc, iseg) = pmsa(ipp)
               ipp = ipp + increm(ip)
            end do
         end do

         ! distribution over receptors
         allocate (currentmass(nrec))
         allocate (frac2rec(nrec))
         allocate (frac2recA(nrec, nsrca, noseg))
         allocate (frac2recB(nrec, nsrcb, noseg))
         do isrc = 1, nsrca
            ip = offset_srca + (isrc - 1) * (nopar_srca + nrec) + nopar_srca
            do irec = 1, nrec
               ip = ip + 1
               ipp = ipoint(ip)
               do iseg = 1, noseg
                  write (par_name, '("Released fraction of source A",i2.2," to receptor ", i0)') isrc, irec
                  frac2recA(irec, isrc, iseg) = pmsa(ipp)
                  call check_fraction(frac2recA(irec, isrc, iseg), par_name, iseg)
                  ipp = ipp + increm(ip)
               end do
            end do
         end do
         do isrc = 1, nsrcb
            ip = offset_srcb + (isrc - 1) * (nopar_srcb + nrec) + nopar_srcb
            do irec = 1, nrec
               ip = ip + 1
               ipp = ipoint(ip)
               do iseg = 1, noseg
                  write (par_name, '("Released fraction of source B",i2.2," to receptor ", i0)') isrc, irec
                  frac2recB(irec, isrc, iseg) = pmsa(ipp)
                  call check_fraction(frac2recB(irec, isrc, iseg), par_name, iseg)
                  ipp = ipp + increm(ip)
               end do
            end do
         end do

         ! prepare distribution according to locators of type A sources (CONSTANT IN TIME)
         allocate (totflxin(nrec))

         ! loop over sources types A to store values of and calculate the sum of locators
         allocate (locator(nsrca, noseg))
         allocate (sumlocator(nsrca))
         sumlocator = 0.0
         do isrc = 1, nsrca
            ip = offset_srca + (isrc - 1) * (nopar_srca + nrec) + ipar_srca_loc
            ipp = ipoint(ip)
            do iseg = 1, noseg
               locator(isrc, iseg) = pmsa(ipp)
               sumlocator(isrc) = sumlocator(isrc) + locator(isrc, iseg)
               ipp = ipp + increm(ip)
            end do
         end do

         ! prepare schematization aspects
         allocate (seg2riv(noseg))
         allocate (simorder(noseg))
         allocate (downstream(noseg))
         noriv = 0
         seg2riv = 0
         ipr = ipoint(ip_river)
         ipd = ipoint(ip_downstream)
         do iseg = 1, noseg
            river = nint(pmsa(ipr))
            downstream(iseg) = nint(pmsa(ipd))
            if (river > 0) then
               noriv = noriv + 1
               seg2riv(iseg) = noriv
            end if
            ipr = ipr + increm(ip_river)
            ipd = ipd + increm(ip_downstream)
         end do
         call calcorder(noseg, downstream, simorder)
         allocate (emis(noriv))
         allocate (flhorsoi(noseg))
         allocate (flhorsop(noseg))
         allocate (flhorsfw(noseg))

         ! prepare for output
         open (newunit=lu_bin, file=filbin, form='unformatted', access='stream', status='replace', iostat=ierr)
         if (ierr /= 0) call write_error_message('ERROR: Could not open outdata_em.bin for writing')
         open (newunit=lu_txt, file=filtxt, status='replace', iostat=ierr)
         if (ierr /= 0) call write_error_message('ERROR: Could not open outdata_em.txt for writing')
         write (lu_txt, '(''Emission metadata'')')
         write (lu_txt, '(''Emissions in g/s'')')
         write (lu_txt, '(''Nr of segments:     '',i10)') noriv
         write (lu_txt, '(''Nr of layers  :              1'')')
         write (lu_txt, '(''Water layer   :              1'')')
         write (lu_txt, '(''Nr of subst   :              1'')')
         write (lu_txt, '(''Unknown       :              1'')')
      end if

      !******************************************************************************* PROCESSING in TIME LOOP

      emis = 0.0
      flhorsoi = 0.0
      flhorsop = 0.0
      flhorsfw = 0.0

      !****** horizontal routing requires going through the cells in another order than the administrative order
      !       this order has been determined above

      do isegadmin = 1, noseg
         iseg = simorder(isegadmin)

         ! collect all relevant input from PMSA
         totsurf = max(1.0, pmsa(ipoint(ip_totsurf) + (iseg - 1) * increm(ip_totsurf)))
         fpaved = max(0.001, pmsa(ipoint(ip_fpaved) + (iseg - 1) * increm(ip_fpaved)))
         call check_fraction(fpaved, "fPaved", iseg)
         funpaved = max(0.001, pmsa(ipoint(ip_funpaved) + (iseg - 1) * increm(ip_funpaved)))
         call check_fraction(funpaved, "fUnpaved", iseg)
         fwater = pmsa(ipoint(ip_fwater) + (iseg - 1) * increm(ip_fwater))
         call check_fraction(fwater, "fOpenWater", iseg)
         rainfall = pmsa(ipoint(ip_rainfall) + (iseg - 1) * increm(ip_rainfall)) * 86400. ! m3/s to m3/d
         drydep = pmsa(ipoint(ip_drydep) + (iseg - 1) * increm(ip_drydep))
         rainconc = pmsa(ipoint(ip_rainconc) + (iseg - 1) * increm(ip_rainconc))
         kpaved = pmsa(ipoint(ip_kpaved) + (iseg - 1) * increm(ip_kpaved))
         kunpaved = pmsa(ipoint(ip_kunpaved) + (iseg - 1) * increm(ip_kunpaved))
         kdunpaved = pmsa(ipoint(ip_kdunpaved) + (iseg - 1) * increm(ip_kdunpaved))
         call check_fraction(kdunpaved, "KdUnpa", iseg)
         ksoil = pmsa(ipoint(ip_ksoil) + (iseg - 1) * increm(ip_ksoil))
         kdsoil = pmsa(ipoint(ip_kdsoil) + (iseg - 1) * increm(ip_kdsoil))
         call check_fraction(kdsoil, "KdSoi", iseg)
         kimmo = pmsa(ipoint(ip_kimmo) + (iseg - 1) * increm(ip_kimmo))
         ropaved = pmsa(ipoint(ip_ropaved) + (iseg - 1) * increm(ip_ropaved))
         fcomsew = pmsa(ipoint(ip_fcomsew) + (iseg - 1) * increm(ip_fcomsew))
         call check_fraction(fcomsew, "fComSew", iseg)
         frainsew = pmsa(ipoint(ip_frainsew) + (iseg - 1) * increm(ip_frainsew))
         call check_fraction(frainsew, "FrRainSew", iseg)
         rounpaved = pmsa(ipoint(ip_rounpaved) + (iseg - 1) * increm(ip_rounpaved))
         infilt = pmsa(ipoint(ip_infilt) + (iseg - 1) * increm(ip_infilt))
         leakage = pmsa(ipoint(ip_leakage) + (iseg - 1) * increm(ip_leakage))
         pop = pmsa(ipoint(ip_pop) + (iseg - 1) * increm(ip_pop))
         pcww = pmsa(ipoint(ip_pcww) + (iseg - 1) * increm(ip_pcww))
         fwwsew = pmsa(ipoint(ip_fwwsew) + (iseg - 1) * increm(ip_fwwsew))
         call check_fraction(fwwsew, "FrSewered", iseg)
         eff_rs = pmsa(ipoint(ip_eff_rs) + (iseg - 1) * increm(ip_eff_rs))
         call check_fraction(eff_rs, "Eff_RS", iseg)
         sld_rs = pmsa(ipoint(ip_sld_rs) + (iseg - 1) * increm(ip_sld_rs))
         call check_fraction(sld_rs, "Sld_RS", iseg)
         soilthick = pmsa(ipoint(ip_soilthick) + (iseg - 1) * increm(ip_soilthick)) / 1000. ! Convert from mm to m
         soilpor = pmsa(ipoint(ip_soilpor) + (iseg - 1) * increm(ip_soilpor))
         call check_fraction(soilpor, "SoilPoros", iseg)
         exfilt = max(pmsa(ipoint(ip_exfilt) + (iseg - 1) * increm(ip_exfilt)), 0.0)
         ssurf = max(pmsa(ipoint(ip_ssurf) + (iseg - 1) * increm(ip_ssurf)), 0.0)
         ero1 = pmsa(ipoint(ip_ero1) + (iseg - 1) * increm(ip_ero1)) ! g/d
         ero2 = pmsa(ipoint(ip_ero2) + (iseg - 1) * increm(ip_ero2))
         ero3 = pmsa(ipoint(ip_ero3) + (iseg - 1) * increm(ip_ero3))
         masspas = pmsa(ipoint(ip_masspas) + (iseg - 1) * increm(ip_masspas)) ! mass in passive pool
         concpas = pmsa(ipoint(ip_concpas) + (iseg - 1) * increm(ip_concpas))
         overland = pmsa(ipoint(ip_overland) + (iseg - 1) * increm(ip_overland))
         FrSeptic = pmsa(ipoint(ip_FrSeptic) + (iseg - 1) * increm(ip_FrSeptic))
         call check_fraction(FrSeptic, "FrSeptic", iseg)
         Eff_Septic = pmsa(ipoint(ip_Eff_Septic) + (iseg - 1) * increm(ip_Eff_Septic)) ! Interpret as losses to Sfw, before reaching WWTPs
         call check_fraction(Eff_Septic, "Eff_Septic", iseg)
         Sld_Septic = pmsa(ipoint(ip_Sld_Septic) + (iseg - 1) * increm(ip_Sld_Septic)) ! Interpret as losses to Soi, before reaching WWTPs
         call check_fraction(Sld_Septic, "Sld_Septic", iseg)
         fSldgRem = pmsa(ipoint(ip_fSldgRem) + (iseg - 1) * increm(ip_fSldgRem))
         call check_fraction(fSldgRem, "FrSldgRem", iseg)
         FrTreat1 = pmsa(ipoint(ip_FrTreat1) + (iseg - 1) * increm(ip_FrTreat1))
         call check_fraction(FrTreat1, "FrTreat1", iseg)
         FrTreat2 = pmsa(ipoint(ip_FrTreat2) + (iseg - 1) * increm(ip_FrTreat2))
         call check_fraction(FrTreat2, "FrTreat2", iseg)
         FrTreat3 = pmsa(ipoint(ip_FrTreat3) + (iseg - 1) * increm(ip_FrTreat3))
         call check_fraction(FrTreat3, "FrTreat3", iseg)
         Eff_Treat1 = pmsa(ipoint(ip_Eff_Treat1) + (iseg - 1) * increm(ip_Eff_Treat1))
         call check_fraction(Eff_Treat1, "Eff_Treat1", iseg)
         Eff_Treat2 = pmsa(ipoint(ip_Eff_Treat2) + (iseg - 1) * increm(ip_Eff_Treat2))
         call check_fraction(Eff_Treat2, "Eff_Treat2", iseg)
         Eff_Treat3 = pmsa(ipoint(ip_Eff_Treat3) + (iseg - 1) * increm(ip_Eff_Treat3))
         call check_fraction(Eff_Treat3, "Eff_Treat3", iseg)
         Sld_Treat1 = pmsa(ipoint(ip_Sld_Treat1) + (iseg - 1) * increm(ip_Sld_Treat1))
         call check_fraction(Sld_Treat1, "Sld_Treat1", iseg)
         Sld_Treat2 = pmsa(ipoint(ip_Sld_Treat2) + (iseg - 1) * increm(ip_Sld_Treat2))
         call check_fraction(Sld_Treat2, "Sld_Treat2", iseg)
         Sld_Treat3 = pmsa(ipoint(ip_Sld_Treat3) + (iseg - 1) * increm(ip_Sld_Treat3))
         call check_fraction(Sld_Treat3, "Sld_Treat3", iseg)

         do irec = 1, nrec
            ip = offset_conc + irec
            currentmass(irec) = pmsa(ipoint(ip) + (iseg - 1) * increm(ip))
         end do

         totflxin = 0.0

         !*******************************************************************************
         ! Now follows the RELEASE PART
         !*******************************************************************************

         ! Type A sources -------------------------------------------------------------
         iflux = fl0_srca
         do isrc = 1, nsrca

            ! pick up (total domain) EV and calculate domain losses per substance
            ip = offset_srca + (isrc - 1) * (nopar_srca + nrec) + ipar_srca_ev
            emisvar = pmsa(ipoint(ip))
            losses = emisvar * emisfacA(isrc, iseg) * 1000. ! kg/d to g/d

            ! losses per sc
            if (sumlocator(isrc) > 0.0) then
               do irec = 1, nrec
                  flux = losses * frac2recA(irec, isrc, iseg) * locator(isrc, iseg) / sumlocator(isrc) / 86400.
                  if (flux < 0.0) then ! extraction is prescribed, limit to store plus earlier sources
                     mass = currentmass(irec)
                     limit = mass / delt + totflxin(irec) ! pool plus earlier sources
                     flux = max(flux, -limit)
                  end if
                  iflux = iflux + 1
                  fl(iflux + (iseg - 1) * noflux) = flux
                  totflxin(irec) = totflxin(irec) + flux
               end do
            end if
         end do

         ! Type B sources ------------------------------------------------------------
         iflux = fl0_srcb
         do isrc = 1, nsrcb

            ! losses
            ip = offset_srcb + (isrc - 1) * (nopar_srcb + nrec) + ipar_srcb_ev
            emisvar = pmsa(ipoint(ip) + (iseg - 1) * increm(ip))
            ip = offset_srcb + (isrc - 1) * (nopar_srcb + nrec) + ipar_srcb_ef
            emisfacB = pmsa(ipoint(ip) + (iseg - 1) * increm(ip))
            losses = emisvar * emisfacB * 1000. ! kg/d to g/d

            ! fluxes
            do irec = 1, nrec
               iflux = iflux + 1
               flux = losses * frac2recB(irec, isrc, iseg) / 86400.
               if (flux < 0.0) then ! extraction is prescribed, limit to store plus earlier sources
                  mass = currentmass(irec)
                  limit = mass / delt + totflxin(irec) ! pool plus earlier sources
                  flux = max(flux, -limit)
               end if
               fl(iflux + (iseg - 1) * noflux) = flux
               totflxin(irec) = totflxin(irec) + flux
            end do
         end do

         ! Atmospheric deposition ------------------------------------------------------------------
         frac2rec = 0.0 ! default
         frac2rec(rec_pav) = fpaved
         frac2rec(rec_unp) = funpaved
         frac2rec(rec_sfw) = fwater

         ! total dep
         losses = drydep * totsurf + rainfall * rainconc ! g/d

         ! fluxes
         iflux = fl0_atm
         do irec = 1, nrec
            iflux = iflux + 1
            flux = losses * frac2rec(irec) / 86400. ! /d to /s
            fl(iflux + (iseg - 1) * noflux) = flux
            totflxin(irec) = totflxin(irec) + flux
         end do

         !*******************************************************************************
         ! Now follows the ROUTING PART (WITHIN COLUMN, OR HORIZONTAL TO DS CELL)
         !*******************************************************************************

         ! DOMESTIC WASTEWATER
         ! unmanaged
         FrUnManaged = max((1.0 - fWWsew - FrSeptic), 0.0)
         ! allocate wastewater
         fluxin = totflxin(rec_dww) ! no storage, so mass is zero
         fr2sew = fWWsew + FrSeptic * (1 - Eff_Septic - Sld_Septic)
         fr2sfw = FrUnManaged * fWater + FrSeptic * Eff_Septic
         fr2soi = FrUnManaged * (1.0 - fWater) + FrSeptic * Sld_Septic

         ! set routing fluxes
         iflux = fl0_rest + dww2sew + (iseg - 1) * noflux
         fl(iflux) = fluxin * fr2sew
         iflux = fl0_rest + dww2sfw + (iseg - 1) * noflux
         fl(iflux) = fluxin * fr2sfw
         iflux = fl0_rest + dww2soi + (iseg - 1) * noflux
         fl(iflux) = fluxin * fr2soi
         ! update admin for next compartments
         totflxin(rec_sew) = totflxin(rec_sew) + fluxin * fr2sew
         totflxin(rec_sfw) = totflxin(rec_sfw) + fluxin * fr2sfw
         totflxin(rec_soi) = totflxin(rec_soi) + fluxin * fr2soi

         ! PAVED SYSTEM --------
         ! discharge to sew OR stw depending on fraction of combined sewers
         !               m3/s  m2
         ro_mmperday = ropaved / (totsurf * fpaved) * 1000.*86400.
         fwashoff = (ro_mmperday - ro_lothr) / (ro_hithr - ro_lothr)
         fwashoff = max(min(fwashoff, 1.0), 0.0)

         ! input
         mass = max(currentmass(rec_pav), 0.0)
         ! fluxes
         fluxloss = kpaved * mass / 86400.
         fluxwash = (mass / delt + totflxin(rec_pav) - fluxloss) * fwashoff

         ! some extra output
         if (ropaved > 0.0) then
            concwash = fluxwash / ropaved ! g/m3 = g/s * m3/s
         else
            concwash = 0.0
         end if
         ro2sew = ropaved * frainsew * fcomsew ! m3/s with scale factors

         ! to mixed sewers
         iflux = fl0_rest + pav2sew + (iseg - 1) * noflux
         fl(iflux) = fluxwash * frainsew * fcomsew
         ! and next to separated sewers
         iflux = fl0_rest + pav2stw + (iseg - 1) * noflux
         fl(iflux) = fluxwash * frainsew * (1.-fcomsew)
         ! to surface waters
         iflux = fl0_rest + pav2sfw + (iseg - 1) * noflux
         fl(iflux) = fluxwash * (1.-frainsew) * fwater
         ! to soils
         iflux = fl0_rest + pav2soi + (iseg - 1) * noflux
         fl(iflux) = fluxwash * (1.-frainsew) * (1.-fwater)
         ! now set the decay flux
         iflux = fl0_rest + pav2dec + (iseg - 1) * noflux
         fl(iflux) = fluxloss
         ! update admin for next compartments
         totflxin(rec_sew) = totflxin(rec_sew) + fluxwash * frainsew * fcomsew
         totflxin(rec_stw) = totflxin(rec_stw) + fluxwash * frainsew * (1.-fcomsew)
         totflxin(rec_sfw) = totflxin(rec_sfw) + fluxwash * (1.-frainsew) * fwater
         totflxin(rec_soi) = totflxin(rec_soi) + fluxwash * (1.-frainsew) * (1.-fwater)

         ! UNPAVED SYSTEM ------------------------------------------------------------------------------------

         roun_mmperday = rounpaved / (totsurf * funpaved) * 1000.*86400.
         in_mmperday = infilt / (totsurf * funpaved) * 1000.*86400. ! BUGFIX 3 jan 2023 correction funpaved added
         ra_mmperday = rainfall / totsurf * 1000. ! already in m3/d
         ferosion = (ra_mmperday - ra_lothr) / (ra_hithr - ra_lothr)
         ferosion = max(min(ferosion, 1.0), 0.0)
         fdisp = (roun_mmperday + in_mmperday) / disp_hithr
         fdisp = max(min(fdisp, 1.0), 0.0)
         froun = roun_mmperday / (roun_mmperday + in_mmperday)
         froun = max(min(froun, 1.0), 0.0)
         finf = in_mmperday / (roun_mmperday + in_mmperday)
         finf = max(min(finf, 1.0), 0.0)
         mass = max(0.0, currentmass(rec_unp))

         ! fluxes
         pool = mass + totflxin(rec_unp) * delt
         fluxloss = kunpaved * pool / 86400.
         fluxbur = kbur * pool / 86400.
         maxflux = mass / delt + totflxin(rec_unp) - fluxloss - fluxbur
         ! kd is now defined as the unbound share NOT the bound share
         ! the trick with the double precision is to make the subtraction valid
         fluxbound = sngl(1d0 - dble(kdunpaved)) * maxflux ! Bound substance  flux
         fluxunbound = kdunpaved * maxflux ! Unbound substance  flux
         fluxero = fluxbound * ferosion
         fluxinf = fluxunbound * fdisp * finf
         fluxroun = fluxunbound * fdisp * froun

         ! some extra output
         if (rounpaved > 0.0) then
            concroun = fluxroun / rounpaved ! g/m3 = g/s * m3/s
         else
            concroun = 0.0
         end if

         iflux = fl0_rest + unp2sfw_er + (iseg - 1) * noflux
         fl(iflux) = fluxero
         iflux = fl0_rest + unp2sfw_ro + (iseg - 1) * noflux
         fl(iflux) = fluxroun
         iflux = fl0_rest + unp2soi_in + (iseg - 1) * noflux
         fl(iflux) = fluxinf
         iflux = fl0_rest + unp2soi_bu + (iseg - 1) * noflux
         fl(iflux) = fluxbur
         iflux = fl0_rest + unp2dec + (iseg - 1) * noflux
         fl(iflux) = fluxloss
         ! to next compartments
         totflxin(rec_sfw) = totflxin(rec_sfw) + fluxero + fluxroun
         totflxin(rec_soi) = totflxin(rec_soi) + fluxinf + fluxbur

         ! COMBINED SEWERS ---------------------------------------------------------------------------------
         ! (local leakage) (or CSO): postive is a fraction, negative a rainfall threshold
         if (leakage < -0.1) then
            if (ra_mmperday > (-leakage)) then ! threshold on the mm per day value
               leakage = 1.0
            else
               leakage = 0.0
            end if
         else
            call check_fraction(leakage, "SewLeakage", iseg)
         end if

         ! (un)treated
         FrTreated = FrTreat1 + FrTreat2 + FrTreat3
         fr_unc = max(1.0 - FrTreated, 0.0)

         ! fraction to effluent/sludge for WW collected and treated
         if (FrTreated > 0.0001) then
            fr_Eff = FrTreat1 / FrTreated * Eff_Treat1 + FrTreat2 / FrTreated * Eff_Treat2 + FrTreat3 / FrTreated * Eff_Treat3
            fr_Sld = (FrTreat1 / FrTreated * Sld_Treat1 + FrTreat2 / FrTreated * Sld_Treat2 + FrTreat3 / FrTreated * Sld_Treat3) * (1.0 - fSldgRem)
         else
            fr_Eff = 1.0
            fr_Sld = 0.0
         end if

         ! fluxes
         fluxleak = leakage * totflxin(rec_sew)
         fluxin = (1.-leakage) * totflxin(rec_sew)
         fluxunc = fluxin * fr_unc
         fluxin = fluxin - fluxunc
         fluxrem = fluxin * (1.-fr_eff - fr_sld)
         fluxeff = fluxin * fr_eff
         fluxsld = fluxin * fr_sld

         ! water balance to be able to calculate a concentration
         ww2sew = pop * pcww * fwwsew / 1000./86400. ! convert from l/cap/d to m3/s
         if (ww2sew + ro2sew > 0.0) then
            concsew = totflxin(rec_sew) / (ww2sew + ro2sew)
         else
            concsew = 0.0
         end if

         ! output
         iflux = fl0_rest + sew2sfw_cso + (iseg - 1) * noflux
         fl(iflux) = fluxleak
         iflux = fl0_rest + sew2stp + (iseg - 1) * noflux
         fl(iflux) = fluxrem
         iflux = fl0_rest + sew2sfw_eff + (iseg - 1) * noflux
         fl(iflux) = fluxeff
         iflux = fl0_rest + sew2soi + (iseg - 1) * noflux
         fl(iflux) = fluxsld
         iflux = fl0_rest + sew2sfw_unc + (iseg - 1) * noflux
         fl(iflux) = fluxunc

         ! to next compartments
         totflxin(rec_sfw) = totflxin(rec_sfw) + fluxleak + fluxeff + fluxunc
         totflxin(rec_soi) = totflxin(rec_soi) + fluxsld

         ! SEPARATED SEWERS --------------------------------------------------------------------------------

         ! fluxes
         fluxin = totflxin(rec_stw)
         fluxrem = fluxin * (1.-eff_rs - sld_rs)
         fluxeff = fluxin * eff_rs
         fluxsld = fluxin * sld_rs

         ! output
         iflux = fl0_rest + stw2stp + (iseg - 1) * noflux
         fl(iflux) = fluxrem
         iflux = fl0_rest + stw2sfw + (iseg - 1) * noflux
         fl(iflux) = fluxeff
         iflux = fl0_rest + stw2soi + (iseg - 1) * noflux
         fl(iflux) = fluxsld

         ! to next compartments
         totflxin(rec_sfw) = totflxin(rec_sfw) + fluxeff
         totflxin(rec_soi) = totflxin(rec_soi) + fluxsld

         ! SOILS (including a passive pool) -------------------------------------------

         mass = currentmass(rec_soi)
         ! concentration in exfiltration and subsurface flow
         concact = mass * kdsoil / (totsurf * funpaved) / (soilthick * soilpor) ! g/m3
         concdrain = concact + concpas

         !erosion
         ! kg         m                      kg/m3     m2
         solids = (soilthick * (1.-soilpor)) * rhodm * (totsurf * funpaved)
         ! g/g      g              kg      g/kg
         qtopact = mass / (solids * 1000.)
         qtoppas = masspas / (solids * 1000.)

         ! fluxes
         fluxloss = ksoil * mass / 86400.
         fluxexfa = exfilt * concact
         fluxexfp = exfilt * concpas
         fluxssfa = ssurf * concact
         fluxssfp = ssurf * concpas
         fluximmo = kimmo * mass / 86400.
         fluxeroa = facerod * (ero1 + ero2 + ero3) / 86400.*qtopact
         fluxerop = facerod * (ero1 + ero2 + ero3) / 86400.*qtoppas

         ! a limiter is needed
         ! for Soi
         maxflux = mass / delt + totflxin(rec_soi)
         courant = (fluxloss + fluxexfa + fluxssfa + fluximmo + fluxeroa) / maxflux
         if (courant > 0.99) then
            fluxloss = fluxloss / courant
            fluxexfa = fluxexfa / courant
            fluxssfa = fluxssfa / courant
            fluximmo = fluximmo / courant
            fluxeroa = fluxeroa / courant
         end if
         ! for Soilpass
         maxflux = masspas / delt + fluximmo
         courant = (fluxexfp + fluxssfp + fluxerop) / maxflux
         if (courant > 0.99) then
            fluxexfp = fluxexfp / courant
            fluxssfp = fluxssfp / courant
            fluxerop = fluxerop / courant
         end if

         ! output
         iflux = fl0_rest + soi2dec + (iseg - 1) * noflux
         fl(iflux) = fluxloss
         iflux = fl0_rest + soi2sop_imm + (iseg - 1) * noflux
         fl(iflux) = fluximmo
         iflux = fl0_rest + soi2sfw_ex + (iseg - 1) * noflux
         fl(iflux) = fluxexfa
         iflux = fl0_rest + sop2sfw_ex + (iseg - 1) * noflux
         fl(iflux) = fluxexfp
         iflux = fl0_rest + soi2sfw_er + (iseg - 1) * noflux
         fl(iflux) = fluxeroa
         iflux = fl0_rest + sop2sfw_er + (iseg - 1) * noflux
         fl(iflux) = fluxerop

         ! horizontal fluxes (net effect of IN and OUT)
         iflux = fl0_rest + horsoi + (iseg - 1) * noflux
         fl(iflux) = flhorsoi(iseg) - fluxssfa
         iflux = fl0_rest + horsop + (iseg - 1) * noflux
         fl(iflux) = flhorsop(iseg) - fluxssfp

         ! to next compartments in this column and same compartment in next cell
         totflxin(rec_sfw) = totflxin(rec_sfw) + fluxeroa + fluxerop + fluxexfa + fluxexfp
         if (downstream(iseg) > 0) then
            flhorsoi(downstream(iseg)) = flhorsoi(downstream(iseg)) + fluxssfa
            flhorsop(downstream(iseg)) = flhorsop(downstream(iseg)) + fluxssfp
         end if

         ! ENDPOINT SURFACE WATER --------------------------------------

         if (seg2riv(iseg) > 0 .or. downstream(iseg) <= 0) then

            ! river cell, emissions output, no storage
            fluxexp = totflxin(rec_sfw) + flhorsfw(iseg)
            emis(seg2riv(iseg)) = fluxexp
            fldown = 0.0
         else

            ! non-river cell -> part storage part downstream
            mass = max(currentmass(rec_sfw), 0.0)
            fluxmax = mass / delt + totflxin(rec_sfw) + flhorsfw(iseg)
            over_mmperday = overland / totsurf * 1000.*86400.
            fdownstream = max(min(over_mmperday / over_hithr, 1.0), 0.0)
            fldown = fdownstream * fluxmax

            ! to next cell
            flhorsfw(downstream(iseg)) = flhorsfw(downstream(iseg)) + fldown
            fluxexp = 0.0
         end if
         ! horizontal fluxes (net effect of IN and OUT)
         iflux = fl0_rest + horsfw + (iseg - 1) * noflux
         fl(iflux) = flhorsfw(iseg) - fldown
         ! emission flux
         iflux = fl0_rest + sfw2exp + (iseg - 1) * noflux
         fl(iflux) = fluxexp

         ! Save segment output to PMSA --------------------------------------
         ip = lins + line + ip_ro2sew
         pmsa(ipoint(ip) + (iseg - 1) * increm(ip)) = ro2sew
         ip = lins + line + ip_concwash
         pmsa(ipoint(ip) + (iseg - 1) * increm(ip)) = concwash
         ip = lins + line + ip_concroun
         pmsa(ipoint(ip) + (iseg - 1) * increm(ip)) = concroun
         ip = lins + line + ip_ww2sew
         pmsa(ipoint(ip) + (iseg - 1) * increm(ip)) = ww2sew
         ip = lins + line + ip_concsew
         pmsa(ipoint(ip) + (iseg - 1) * increm(ip)) = concsew
         ip = lins + line + ip_concdrain
         pmsa(ipoint(ip) + (iseg - 1) * increm(ip)) = concdrain
         ip = lins + line + ip_ew
         pmsa(ipoint(ip) + (iseg - 1) * increm(ip)) = fluxexp

         ! end segment loop
      end do

      ! write output
      itime = nint(pmsa(ipoint(ip_itime)))
      write (lu_txt, '(''Output written for relative time: '',i20)') itime
      write (lu_bin) itime, emis

      first = .false.

   end subroutine espgss
end module m_espgss
