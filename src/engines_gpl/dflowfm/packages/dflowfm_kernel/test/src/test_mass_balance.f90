!> Regression tests for fixed-bed exchange, moving geometry and sediment balance labels.
module test_mass_balance
   use precision, only: dp, fp
   use assertions_gtest
   use m_mass_balance_area_data
   use mass_balance_areas_routines, only: mba_init, mba_update, mba_final, comp_fixedbed_fluxmba, comp_bedflowmba, comp_bedload_fluxmba
   use m_sediment, only: stmpar, sedtra, stmtype, stm_included
   use morphology_data_module, only: morpar_type, fluffy_type, sedtra_type
   use bedcomposition_module, only: bedcomp_data
   use m_flow, only: vol1, kmx, kbot, ktop
   use m_flowtimes, only: time1, tstart_user, tfac, dts
   use m_transport, only: constituents, sinksetot, sinkftot

   implicit none(type, external)
   private
   public :: test_mba_spinup, test_mba_fixed_cmp, test_mba_bedload_start
   public :: test_mba_fluff_spin, test_mba_moving_volume, test_mba_moving_3d, test_mba_labels
   public :: test_mba_cmp_only, test_mba_bed_first

   real(dp), parameter :: tolerance = 1.0e-10_dp
   type(stmtype) :: saved_stmpar
   type(sedtra_type) :: saved_sedtra
   type(morpar_type), target :: local_morpar
   type(fluffy_type), target :: local_fluff
   type(bedcomp_data), target :: local_bed
   integer, target :: local_reference(4, 1)
   real(fp), target :: local_source(4, 1), local_bedload(3, 1), local_fluff_source(1, 4)
   integer, pointer :: saved_lsed, saved_lsedtot, saved_ifluff
   real(fp), pointer :: saved_morfac, saved_bedload(:, :)
   integer, allocatable :: saved_ln(:, :), saved_lne2ln(:), saved_kbot(:), saved_ktop(:)
   integer, allocatable :: saved_bnd_counts(:), saved_bnd_links(:)
   real(dp), allocatable :: saved_ba(:), saved_wu(:), saved_vol1(:), saved_constituents(:, :)
   real(dp), allocatable :: saved_sinksetot(:, :), saved_sinkftot(:, :)
   character(len=:), allocatable :: saved_const_names(:), saved_bnd_names(:)

contains

   !$gtest
   !> Account for fixed-bed exchange and stop that term exactly when composition updates start.
   subroutine test_mba_spinup() bind(C)
      call setup_mba([1, 1])
      local_source(:, 1) = [0.2_fp, 0.0_fp, 0.0_fp, 0.0_fp]
      sinksetot(1, 1:2) = [0.1_dp, 0.2_dp]
      constituents(1, 1:2) = [1.3_dp, 13.0_dp / 15.0_dp]
      call comp_fixedbed_fluxmba()
      call f90_expect_near(mbafluxfixbed(:, 1, 1), [6.0_dp, 4.0_dp], tolerance)

      call mba_update(time1)
      call expect_entry(const_flux(1)%bal_area(1), 'External forcing', 'Fixed bed exchange', [6.0_dp, 4.0_dp])
      call f90_expect_near(const_flux(1)%bal_error, [0.0_dp, 0.0_dp], tolerance)
      call f90_expect_near(sum(abs(mbafluxfixbed)), 0.0_dp, tolerance, 'The interval accumulator must be reset')

      time1 = tstart_user + local_morpar%tcmp * tfac
      call comp_fixedbed_fluxmba()
      call f90_expect_near(sum(abs(mbafluxfixbed)), 0.0_dp, tolerance, 'The activation boundary belongs to the active bed')
      constituents(1, 1) = constituents(1, 1) + 0.1_dp
      local_bed%state%bodsed(1, 1) = local_bed%state%bodsed(1, 1) - 2.0_dp
      call mba_update(time1)
      call f90_expect_near(const_flux(1)%bal_error, [0.0_dp, 0.0_dp], tolerance)
      call expect_entry(const_flux(1)%bal_area(1), 'External forcing', 'Fixed bed exchange', [0.0_dp, 0.0_dp])
      call mba_final(time1)
      call f90_expect_near(const_flux(1)%bal_error, [0.0_dp, 0.0_dp], tolerance, 'The whole-period balance must include spin-up exchange')
      call expect_entry(const_flux(1)%bal_area(2), 'External forcing', 'Fixed bed exchange', [6.0_dp, 4.0_dp])
      call cleanup_mba()
   end subroutine test_mba_spinup

   !$gtest
   !> A permanently fixed composition remains an external reservoir after the start time.
   subroutine test_mba_fixed_cmp() bind(C)
      call setup_mba([1, 1])
      time1 = tstart_user + 600.0_dp
      local_morpar%cmpupd = .false.
      local_source(1, 1) = 0.2_fp
      sinksetot(1, 1) = 0.1_dp
      call comp_fixedbed_fluxmba()
      call f90_expect_near(mbafluxfixbed(:, 1, 1), [6.0_dp, 0.0_dp], tolerance)
      local_bedload = 1.0_fp
      call comp_bedload_fluxmba()
      call f90_expect_near(sum(abs(mbasedflux)), 0.0_dp, tolerance)
      call cleanup_mba()
   end subroutine test_mba_fixed_cmp

   !$gtest
   !> Composition changes inventory through exchange and bedload while the bed level remains fixed.
   subroutine test_mba_cmp_only() bind(C)
      call setup_mba([1, 1])
      local_morpar%bedupd = .false.
      local_morpar%tcmp = 0.0_fp
      local_source(1, 1) = 0.2_fp
      sinksetot(1, 1) = 0.1_dp
      constituents(1, 1) = 1.3_dp
      ! Erosion removes 6 kg and incoming bedload adds 20 kg to the bed inventory.
      local_bed%state%bodsed(1, 1) = local_bed%state%bodsed(1, 1) + 14.0_dp
      local_bedload(2, 1) = 1.0_fp
      call comp_fixedbed_fluxmba()
      call comp_bedload_fluxmba()
      call comp_bedflowmba(before_update=.true.)
      call comp_bedflowmba(before_update=.false.)
      call mba_update(time1)
      call f90_expect_near(const_flux(1)%bal_error, [0.0_dp, 0.0_dp], tolerance)
      call f90_expect_near(water_flow%bal_error, [0.0_dp, 0.0_dp], tolerance)
      call expect_entry(const_flux(1)%bal_area(1), 'From/to storage', 'Bed', [0.0_dp, 14.0_dp])
      call expect_entry(const_flux(1)%bal_area(1), 'External forcing', 'Fixed bed exchange', [0.0_dp, 0.0_dp])
      call expect_entry(water_flow%bal_area(1), 'External forcing', 'Moving bed', [0.0_dp, 0.0_dp])
      call cleanup_mba()
   end subroutine test_mba_cmp_only

   !$gtest
   !> Moving-bed water volume and fixed-composition exchange are independent contributions.
   subroutine test_mba_bed_first() bind(C)
      call setup_mba([1, 1])
      local_morpar%tmor = 0.0_fp
      local_morpar%tcmp = 10.0_fp
      local_source(1, 1) = 0.2_fp
      sinksetot(1, 1) = 0.1_dp
      constituents(1, 1) = 1.3_dp
      call comp_fixedbed_fluxmba()
      call comp_bedflowmba(before_update=.true.)
      constituents(1, 1) = constituents(1, 1) * vol1(1) / 2.5_dp
      vol1(1) = 2.5_dp
      call comp_bedflowmba(before_update=.false.)
      local_bedload = 1.0_fp
      call comp_bedload_fluxmba()
      call f90_expect_near(sum(abs(mbasedflux)), 0.0_dp, tolerance)
      call mba_update(time1)
      call f90_expect_near(const_flux(1)%bal_error, [0.0_dp, 0.0_dp], tolerance)
      call f90_expect_near(water_flow%bal_error, [0.0_dp, 0.0_dp], tolerance)
      call expect_entry(const_flux(1)%bal_area(1), 'External forcing', 'Fixed bed exchange', [6.0_dp, 0.0_dp])
      call expect_entry(water_flow%bal_area(1), 'External forcing', 'Moving bed', [0.5_dp, 0.0_dp])
      call cleanup_mba()
   end subroutine test_mba_bed_first

   !$gtest
   !> Potential boundary bedload becomes an inventory transfer when composition updates start.
   subroutine test_mba_bedload_start() bind(C)
      call setup_mba([1, 1])
      local_bedload(:, 1) = [0.0_fp, 2.0_fp, -3.0_fp]
      call comp_bedload_fluxmba()
      call f90_expect_near(sum(abs(mbasedflux)), 0.0_dp, tolerance)

      time1 = tstart_user + local_morpar%tcmp * tfac
      call comp_bedload_fluxmba()
      call f90_expect_near(mbasedflux(:, 1, 1, 2), [40.0_dp, 0.0_dp], tolerance)
      call f90_expect_near(mbasedflux(:, 1, 1, 3), [0.0_dp, 60.0_dp], tolerance)
      call cleanup_mba()
   end subroutine test_mba_bedload_start

   !$gtest
   !> Include frozen fluff exchange and exclude cells owned by another domain.
   subroutine test_mba_fluff_spin() bind(C)
      call setup_mba([1, 2])
      local_fluff%iflufflyr = 1
      local_fluff_source(1, 1:2) = [0.3_fp, 100.0_fp]
      sinkftot(1, 1) = 0.2_dp
      mbadefdomain(2) = 0
      call comp_fixedbed_fluxmba()
      call f90_expect_near(mbafluxfixbed(:, 1, 1), [8.0_dp, 0.0_dp], tolerance)
      call f90_expect_near(sum(abs(mbafluxfixbed(:, :, 2))), 0.0_dp, tolerance)
      call cleanup_mba()
   end subroutine test_mba_fluff_spin

   !$gtest
   !> Actual geometry changes close area and whole-model water balances across output resets.
   subroutine test_mba_moving_volume() bind(C)
      call setup_mba([1, 2])
      call comp_bedflowmba(before_update=.true.)
      vol1(1:2) = [1.5_dp, 4.0_dp]
      call comp_bedflowmba(before_update=.false.)
      call mba_update(time1)
      call f90_expect_near(water_flow%bal_error, [0.0_dp, 0.0_dp, 0.0_dp], tolerance)
      call expect_entry(water_flow%bal_area(1), 'External forcing', 'Moving bed', [0.0_dp, 0.5_dp])
      call expect_entry(water_flow%bal_area(2), 'External forcing', 'Moving bed', [1.0_dp, 0.0_dp])
      call expect_entry(water_flow%bal_area(3), 'External forcing', 'Moving bed', [1.0_dp, 0.5_dp])
      call f90_expect_near(sum(abs(mbaflowbed)), 0.0_dp, tolerance)

      call comp_bedflowmba(before_update=.true.)
      vol1(1:2) = [2.0_dp, 3.0_dp]
      call comp_bedflowmba(before_update=.false.)
      call mba_update(time1 + dts)
      call f90_expect_near(water_flow%bal_error, [0.0_dp, 0.0_dp, 0.0_dp], tolerance)
      call mba_final(time1 + dts)
      call f90_expect_near(water_flow%bal_error, [0.0_dp, 0.0_dp, 0.0_dp], tolerance)
      call expect_entry(water_flow%bal_area(3), 'External forcing', 'Moving bed', [1.5_dp, 1.5_dp])
      call cleanup_mba()
   end subroutine test_mba_moving_volume

   !$gtest
   !> Sum all layers and skip non-owned cells when measuring moving-bed water volumes.
   subroutine test_mba_moving_3d() bind(C)
      call setup_mba([1, 2])
      kmx = 2
      kbot(1:2) = [1, 3]
      ktop(1:2) = [2, 4]
      vol1 = [1.0_dp, 1.0_dp, 1.0_dp, 2.0_dp]
      mbadefdomain(2) = 0
      call comp_bedflowmba(before_update=.true.)
      vol1 = [0.25_dp, 0.75_dp, 2.0_dp, 2.0_dp]
      call comp_bedflowmba(before_update=.false.)
      call f90_expect_near(mbaflowbed(:, 1), [0.0_dp, 1.0_dp], tolerance)
      call f90_expect_near(mbaflowbed(:, 2), [0.0_dp, 0.0_dp], tolerance)
      call cleanup_mba()
   end subroutine test_mba_moving_3d

   !$gtest
   !> Names and values agree for every combination of boundary and inter-area lumping.
   subroutine test_mba_labels() bind(C)
      use m_flowparameters, only: jambalumpmba, jambalumpbnd
      integer :: lump_mba, lump_bnd, imba, expected_entries

      do lump_mba = 0, 1
         do lump_bnd = 0, 1
            call setup_mba([1, 1], lump_mba, lump_bnd)
            mbafluxhor(:, 1, 1, 2) = [1.0_dp, 2.0_dp]
            mbafluxhor(:, 1, 1, 3) = [3.0_dp, 4.0_dp]
            mbasedflux(:, 1, 1, 2) = [5.0_dp, 6.0_dp]
            mbasedflux(:, 1, 1, 3) = [7.0_dp, 8.0_dp]
            local_bed%state%bodsed(1, 1) = local_bed%state%bodsed(1, 1) + 10.0_dp
            call mba_update(time1)
            expected_entries = 6
            if (lump_bnd == 0) then
               expected_entries = 8
            end if
            do imba = 1, 2
               call f90_expect_eq(const_flux(1)%bal_area(imba)%n_entries, expected_entries)
               call expect_entry(const_flux(1)%bal_area(imba), 'From/to storage', 'Bed', [0.0_dp, 10.0_dp])
               if (lump_bnd == 0) then
                  call expect_entry(const_flux(1)%bal_area(imba), 'Boundary as bedload', 'Boundary left', [5.0_dp, 6.0_dp])
                  call expect_entry(const_flux(1)%bal_area(imba), 'Boundary as bedload', 'Boundary right', [7.0_dp, 8.0_dp])
               else
                  call expect_entry(const_flux(1)%bal_area(imba), 'Boundary as bedload', 'All', [12.0_dp, 14.0_dp])
               end if
            end do
            call f90_expect_eq(jambalumpmba, lump_mba)
            call f90_expect_eq(jambalumpbnd, lump_bnd)
            call cleanup_mba()
         end do
      end do
   end subroutine test_mba_labels

   !> Build two internal cells, two boundaries and one suspended sediment fraction.
   subroutine setup_mba(area_map, lump_mba, lump_bnd)
      use m_alloc, only: realloc
      use m_resetfullflowmodel, only: resetFullFlowModel
      use m_flowgeom, only: ndx, ndxi, lnx, lnxi, ln, lne2ln, ba, wu_mor
      use m_flowparameters, only: jambawritetxt, jambawritecsv, jambawritenetcdf, jambalumpmba, jambalumpbnd
      use m_transport, only: numconst, ised1, isedn, itemp, const_names
      use m_fm_erosed, only: lsed, lsedtot, morfac, iflufflyr, e_sbn
      use m_fm_wq_processes, only: numwqbots, num_substances_transported, num_fluxes, flxdmp, flxdmptot
      use m_partitioninfo, only: jampi
      use fm_external_forcings_data, only: nopenbndsect, nopenbndlin, openbndlin, openbndname

      integer, intent(in) :: area_map(2)
      integer, optional, intent(in) :: lump_mba, lump_bnd

      call resetFullFlowModel()
      saved_stmpar = stmpar
      saved_sedtra = sedtra
      saved_lsed => lsed
      saved_lsedtot => lsedtot
      saved_ifluff => iflufflyr
      saved_morfac => morfac
      saved_bedload => e_sbn
      call move_alloc(ln, saved_ln)
      call move_alloc(lne2ln, saved_lne2ln)
      call move_alloc(kbot, saved_kbot)
      call move_alloc(ktop, saved_ktop)
      call move_alloc(nopenbndlin, saved_bnd_counts)
      call move_alloc(openbndlin, saved_bnd_links)
      call move_alloc(openbndname, saved_bnd_names)
      call move_alloc(ba, saved_ba)
      call move_alloc(wu_mor, saved_wu)
      call move_alloc(vol1, saved_vol1)
      call move_alloc(constituents, saved_constituents)
      call move_alloc(sinksetot, saved_sinksetot)
      call move_alloc(sinkftot, saved_sinkftot)
      call move_alloc(const_names, saved_const_names)
      local_morpar%morfac = 10.0_fp
      local_morpar%tcmp = 5.0_fp
      local_morpar%tmor = 5.0_fp
      local_morpar%cmpupd = .true.
      local_morpar%bedupd = .true.
      local_morpar%flufflyr => local_fluff
      local_fluff%iflufflyr = 0
      local_fluff_source = 0.0_fp
      local_fluff%sourf => local_fluff_source
      allocate (local_bed%settings, local_bed%state)
      local_bed%settings%iunderlyr = 1
      allocate (local_bed%state%bodsed(1, 4), local_bed%state%sedshort(1, 4))
      local_bed%state%bodsed = 100.0_dp
      local_bed%state%sedshort = 0.0_fp
      stmpar%morpar => local_morpar
      stmpar%morlyr => local_bed
      stmpar%lsedsus = 1
      stmpar%lsedtot = 1
      lsed => stmpar%lsedsus
      lsedtot => stmpar%lsedtot
      iflufflyr => local_fluff%iflufflyr
      morfac => local_morpar%morfac
      local_reference(:, 1) = [1, 2, 3, 4]
      local_source = 0.0_fp
      local_bedload = 0.0_fp
      sedtra%kmxsed => local_reference
      sedtra%sourse => local_source
      e_sbn => local_bedload
      stm_included = .true.
      jampi = 0
      ndxi = 2
      ndx = 4
      lnx = 3
      lnxi = 1
      kmx = 0
      nomba = maxval(area_map)
      numconst = 1
      ised1 = 1
      isedn = 1
      itemp = 0
      numwqbots = 0
      num_substances_transported = 0
      num_fluxes = 0
      tstart_user = 1200.0_dp
      time1 = 1202.0_dp
      tfac = 60.0_dp
      dts = 2.0_dp
      jambawritetxt = 0
      jambawritecsv = 0
      jambawritenetcdf = 0
      jambalumpmba = 0
      jambalumpbnd = 1
      if (present(lump_mba)) then
         jambalumpmba = lump_mba
      end if
      if (present(lump_bnd)) then
         jambalumpbnd = lump_bnd
      end if
      call realloc(ln, [2, 3], keepExisting=.false., fill=0)
      ln(:, 1) = [1, 2]
      ln(:, 2) = [3, 1]
      ln(:, 3) = [4, 2]
      call realloc(lne2ln, 2, keepExisting=.false., fill=0)
      lne2ln = [2, 3]
      call realloc(ba, 4, keepExisting=.false., fill=1.0_dp)
      call realloc(wu_mor, 3, keepExisting=.false., fill=1.0_dp)
      call realloc(vol1, 4, keepExisting=.false., fill=0.0_dp)
      vol1(1:2) = [2.0_dp, 3.0_dp]
      call realloc(kbot, 4, keepExisting=.false., fill=0)
      call realloc(ktop, 4, keepExisting=.false., fill=0)
      call realloc(constituents, [1, 4], keepExisting=.false., fill=1.0_dp)
      call realloc(sinksetot, [1, 4], keepExisting=.false., fill=0.0_dp)
      call realloc(sinkftot, [1, 4], keepExisting=.false., fill=0.0_dp)
      call realloc(const_names, 1, keepExisting=.false., fill='sand')
      call realloc(mbadef, 2, keepExisting=.false., fill=0)
      call realloc(mbadefdomain, 2, keepExisting=.false., fill=0)
      mbadef = area_map
      mbadefdomain = area_map
      call realloc(mbaname, nomba, keepExisting=.false., fill='test area')
      call realloc(mbalnused, [nomba, nomba + 2], keepExisting=.false., fill=0)
      call realloc(flxdmp, [2, 0, nomba], keepExisting=.false., fill=0.0_dp)
      call realloc(flxdmptot, [2, 0, nomba], keepExisting=.false., fill=0.0_dp)
      nopenbndsect = 2
      call realloc(nopenbndlin, 2, keepExisting=.false., fill=0)
      call realloc(openbndlin, 2, keepExisting=.false., fill=0)
      call realloc(openbndname, 2, keepExisting=.false., fill='')
      nopenbndlin = [1, 2]
      openbndlin = [1, 2]
      openbndname = ['left ', 'right']
      call mba_init()
   end subroutine setup_mba

   !> Restore pointer associations before the ordinary model teardown runs.
   subroutine cleanup_mba()
      use m_fm_erosed, only: lsed, lsedtot, iflufflyr, morfac, e_sbn
      use m_flowgeom, only: ln, lne2ln, ba, wu_mor
      use m_transport, only: const_names
      use fm_external_forcings_data, only: nopenbndlin, openbndlin, openbndname

      stmpar = saved_stmpar
      sedtra = saved_sedtra
      lsed => saved_lsed
      lsedtot => saved_lsedtot
      iflufflyr => saved_ifluff
      morfac => saved_morfac
      e_sbn => saved_bedload
      call move_alloc(saved_ln, ln)
      call move_alloc(saved_lne2ln, lne2ln)
      call move_alloc(saved_kbot, kbot)
      call move_alloc(saved_ktop, ktop)
      call move_alloc(saved_bnd_counts, nopenbndlin)
      call move_alloc(saved_bnd_links, openbndlin)
      call move_alloc(saved_bnd_names, openbndname)
      call move_alloc(saved_ba, ba)
      call move_alloc(saved_wu, wu_mor)
      call move_alloc(saved_vol1, vol1)
      call move_alloc(saved_constituents, constituents)
      call move_alloc(saved_sinksetot, sinksetot)
      call move_alloc(saved_sinkftot, sinkftot)
      call move_alloc(saved_const_names, const_names)
      deallocate (local_bed%state%bodsed, local_bed%state%sedshort)
      deallocate (local_bed%settings, local_bed%state)
      deallocate (water_flow%bal_area, water_flow%bal_error, water_flow%bal_cumerror, const_flux)
      jamba = 0
      nomba = 0
      nombs = 0
   end subroutine cleanup_mba

   !> Check an output entry by both its group and its name, including duplicate names such as All.
   subroutine expect_entry(balance, group, name, values)
      type(balance_type), intent(in) :: balance
      character(len=*), intent(in) :: group, name
      real(dp), intent(in) :: values(2)
      integer :: i

      do i = 1, balance%n_entries
         if (balance%group(i) == group .and. balance%name(i) == name) then
            call f90_expect_near(balance%values(:, i), values, tolerance, trim(group)//': '//name)
            return
         end if
      end do
      call f90_expect_true(.false., 'Missing balance entry '//trim(group)//': '//name)
   end subroutine expect_entry
end module test_mass_balance
