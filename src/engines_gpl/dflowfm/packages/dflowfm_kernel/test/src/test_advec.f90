!> Regression for pipe velocity feeding back through 2D velocity reconstruction.
module test_advec
   use assertions_gtest, only: F90_EXPECT_LE
   use iso_c_utils, only: cstr
   use precision, only: dp

   implicit none(type, external)
   private
   public :: pipe_self_feedback

contains

   !$f90tw TESTCODE(TEST, test_advec, pipe_self_feedback, pipe_self_feedback,
   !> Unforced advection must not accelerate a pipe between otherwise stationary 2D cells.
   subroutine pipe_self_feedback() bind(C)
      use m_advec, only: advec
      use m_alloc, only: realloc
      use m_flow
      use m_flowgeom
      use m_flowtimes, only: dts
      use m_setlinktocenterweights, only: setlinktocenterweights
      use m_setucxucyucxuucyunew, only: setucxucyucxuucyunew
      use m_sferic, only: jsferic, jasfer3d
      use m_sobekdfm, only: nbnd1d2d
      use m_source_sink, only: source_sinks
      use network_data, only: LINK_1D2D_STREETINLET

      real(kind=dp) :: updated_velocity

      call default_flowparameters()
      kmx = 0
      icorio = 0
      limtypmom = 0
      ChangeVelocityAtStructures = .false.
      jsferic = 0
      jasfer3d = 0
      nbndz = 0
      nbndu = 0
      nbndt = 0
      nbndn = 0
      nbnduxy = 0
      nbnd1d2d = 0
      call source_sinks%reset()
      ndx = 2
      ndxi = 2
      ndx2d = 2
      lnx = 1
      lnxi = 1
      lnx1d = 1
      mxwalls = 0
      wetLinkCount = 1
      wetLink2D = 2
      dts = 0.1_dp

      if (allocated(nd)) then
         deallocate (nd)
      end if
      allocate (nd(2))
      nd%lnx = 1
      nd%nwx = 0
      nd(1)%ln = [-1]
      nd(2)%ln = [1]
      call realloc(ln, [2, 1], fill=0)
      ln(:, 1) = [1, 2]
      call realloc(kcu, 1, fill=LINK_1D2D_STREETINLET)
      call realloc(iadv, 1, fill=IADV_ORIGINAL_LATERAL_OVERFLOW)
      call realloc(onlyWetLinks, 1, fill=1)
      call realloc(acl, 1, fill=0.5_dp)
      call realloc(csu, 1, fill=1.0_dp)
      call realloc(snu, 1, fill=0.0_dp)

      ! The old pipe contribution W*L/(2*cell area) is 50 at each endpoint.
      call realloc(wu, 1, fill=100.0_dp)
      call realloc(dx, 1, fill=100.0_dp)
      call realloc(ba, 2, fill=100.0_dp)
      call realloc(bai, 2, fill=0.01_dp)
      call realloc(wcx1, 1, fill=0.0_dp)
      call realloc(wcy1, 1, fill=0.0_dp)
      call realloc(wcx2, 1, fill=0.0_dp)
      call realloc(wcy2, 1, fill=0.0_dp)
      call realloc(wcL, [2, 1], fill=0.0_dp)
      call realloc(wcxy, [2, 2], fill=0.0_dp)
      call realloc(wc, 2, fill=0.0_dp)
      call realloc(hu, 1, fill=1.0_dp)
      call realloc(au, 1, fill=100.0_dp)
      call realloc(hs, 2, fill=1.0_dp)
      call realloc(vol1, 2, fill=100.0_dp)
      call realloc(u0, 1, fill=0.3_dp)
      call realloc(u1, 1, fill=0.3_dp)
      call realloc(qa, 1, fill=30.0_dp)
      call realloc(ucx, 2, fill=0.0_dp)
      call realloc(ucy, 2, fill=0.0_dp)
      call realloc(ucxq, 2, fill=0.0_dp)
      call realloc(ucyq, 2, fill=0.0_dp)
      call realloc(uc1D, 2, fill=0.0_dp)
      call realloc(ucxu, 1, fill=0.0_dp)
      call realloc(ucyu, 1, fill=0.0_dp)
      call realloc(uqcx, 2, fill=0.0_dp)
      call realloc(uqcy, 2, fill=0.0_dp)
      call realloc(sqa, 2, fill=0.0_dp)
      call realloc(advi, 1, fill=0.0_dp)
      call realloc(adve, 1, fill=0.0_dp)

      call setlinktocenterweights()
      call setucxucyucxuucyunew()
      call advec()

      ! Apply only the assembled advection terms, without pressure or friction.
      updated_velocity = (u0(1) - dts * adve(1)) / (1.0_dp + dts * advi(1))
      call F90_EXPECT_LE(abs(updated_velocity), abs(u0(1)), &
                         cstr("Unforced pipe advection must not amplify its velocity"))
   end subroutine pipe_self_feedback
   !$f90tw)
end module test_advec
