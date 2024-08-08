!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

subroutine xbeach_wave_input
!! Start logging
!! Read input from params.txt
   use m_flowgeom
   use m_xbeach_data
   use m_xbeach_readkey
   use m_xbeach_filefunctions

   implicit none

   logical, save :: init = .false.

   if (.not. init) then
      ! Start logging
      call start_logfiles(0)
      call writelog_startup()
      call xbeach_all_input()
      call writelog('ls', '', 'Initializing .....')
   else
      call writelog_startup()
      call xbeach_all_input()
      call writelog('ls', '', 'Reinitialized model .....')
   end if
   init = .true.
end subroutine xbeach_wave_input

subroutine xbeach_all_input()
   use m_physcoef
   use m_flowgeom
   use m_xbeach_data
   use m_xbeach_readkey
   use m_xbeach_filefunctions
   use m_xbeach_errorhandling
   use m_xbeach_paramsconst
   use m_flowtimes
   use m_samples
   use m_missing
   !use m_wind, only: jawind
   use unstruc_model

   implicit none

   character(slen) :: dummystring
   character(slen), dimension(:), allocatable :: allowednames, oldnames

   integer :: filetype

   call writelog('sl', '', 'Reading input parameters: ')
   !
   ! Check params.txt exists
   !
   call check_file_exist(md_surfbeatfile)
   !
   !
   ! Physical processes
   call writelog('l', '', '--------------------------------')
   call writelog('l', '', 'Physical processes: ')
   swave = readkey_int(md_surfbeatfile, 'swave', 1, 0, 1, strict=.true.)
   lwave = readkey_int(md_surfbeatfile, 'lwave', 1, 0, 1, strict=.true.)
   !windmodel   = readkey_int (md_surfbeatfile,'windmodel',     0,        0,     1, strict=.true.)
   single_dir = readkey_int(md_surfbeatfile, 'single_dir', 0, 0, 1, strict=.true.)
   !
   ! Grid parameters
   call writelog('l', '', '--------------------------------')
   call writelog('l', '', 'Directional wave grid parameters: ')
   thetamin = readkey_dbl(md_surfbeatfile, 'thetamin', -90.d0, -180.d0, 180.d0, required=(swave == 1))
   thetamax = readkey_dbl(md_surfbeatfile, 'thetamax', 90.d0, -180.d0, 180.d0, required=(swave == 1))
   dtheta = readkey_dbl(md_surfbeatfile, 'dtheta', 10.d0, 0.1d0, 20.d0, required=(swave == 1))
   thetanaut = readkey_int(md_surfbeatfile, 'thetanaut', 0, 0, 1)
   if (single_dir == 1) then
      call writelog('ls', '', 'dtheta will automatically be computed from thetamin and thetamax for single_dir = 1')
      dtheta_s = readkey_dbl(md_surfbeatfile, 'dtheta_s', 10.d0, 0.1d0, 20.d0, required=.true.)
   else
      dtheta = readkey_dbl(md_surfbeatfile, 'dtheta', 10.d0, 0.1d0, 180.d0, required=.true.)
   end if
   !
   !
   ! Wave boundary condition parameters
   call writelog('l', '', '--------------------------------')
   call writelog('l', '', 'Wave boundary condition parameters: ')
   allocate (allowednames(12), oldnames(12))
   allowednames = (/'stat        ', 'bichrom     ', 'ts_1        ', 'ts_2        ', 'jons        ', 'swan        ', &
                    'vardens     ', 'reuse       ', 'off         ', 'stat_table  ', 'jons_table  '/)
   oldnames = (/'0 ', '1 ', '2 ', '3 ', '4 ', '5 ', '6 ', '7 ', '9 ', '40', '41'/)
   !             function =   file         key      default  n allowed  n old allowed  allowed names  old allowed names
   instat = readkey_str(md_surfbeatfile, 'instat', 'bichrom', 11, 11, allowednames, oldnames, required=(swave == 1))
   deallocate (allowednames, oldnames)
   if (isSetParameter(md_surfbeatfile, 'Tlong')) then
      ! we will read it in later
   else
      Tlong = 80
   end if
   !
   taper = readkey_dbl(md_surfbeatfile, 'taper', 100.d0, 0.0d0, 1000.d0)
   nwavmax = readkey_dbl(md_surfbeatfile, 'nmax', 0.8d0, 0.5d0, 1.d0)
   dir0 = readkey_dbl(md_surfbeatfile, 'dir0', 270.d0, 0d0, 360.d0)
   if (trim(instat) == 'stat' .or. single_dir == 1) then
      Hrms = readkey_dbl(md_surfbeatfile, 'Hrms', 1.d0, 0.d0, 10.d0)
      Tm01 = readkey_dbl(md_surfbeatfile, 'Tm01', 10.d0, 1.d0, 20.d0)
      Trep = readkey_dbl(md_surfbeatfile, 'Trep', Tm01, 1.d0, 20.d0)
      m = readkey_int(md_surfbeatfile, 'm', 10, 2, 128)
   elseif (trim(instat) == 'bichrom') then
      Hrms = readkey_dbl(md_surfbeatfile, 'Hrms', 1.d0, 0.d0, 10.d0)
      Tm01 = readkey_dbl(md_surfbeatfile, 'Tm01', 10.d0, 1.d0, 20.d0)
      Trep = readkey_dbl(md_surfbeatfile, 'Trep', Tm01, 1.d0, 20.d0)
      Tlong = readkey_dbl(md_surfbeatfile, 'Tlong', 80.d0, 20.d0, 300.d0)
      m = readkey_int(md_surfbeatfile, 'm', 10, 2, 128)
   elseif (trim(instat) == 'ts_1' .or. trim(instat) == 'ts_2') then
      Hrms = readkey_dbl(md_surfbeatfile, 'Hrms', 1.d0, 0.d0, 10.d0)
      Tm01 = readkey_dbl(md_surfbeatfile, 'Tm01', 10.d0, 1.d0, 20.d0)
      Trep = readkey_dbl(md_surfbeatfile, 'Trep', Tm01, 1.d0, 20.d0)
      m = readkey_int(md_surfbeatfile, 'm', 10, 2, 128)
      call check_file_exist('bc/gen.ezs')
   end if
   !
   !
   ! Wave-spectrum boundary condition parameters
   if (trim(instat) == 'jons' .or. &
       trim(instat) == 'swan' .or. &
       trim(instat) == 'vardens' .or. &
       trim(instat) == 'jons_table') then

      call writelog('l', '', '--------------------------------')
      call writelog('l', '', 'Wave-spectrum boundary condition parameters: ')

      random = readkey_int(md_surfbeatfile, 'random', 1, 0, 1, strict=.true.)
      fcutoff = readkey_dbl(md_surfbeatfile, 'fcutoff', 0.d0, 0.d0, 0.025d0)
      nspr = readkey_int(md_surfbeatfile, 'nspr', 0, 0, 1)
      trepfac = readkey_dbl(md_surfbeatfile, 'trepfac', 0.01d0, 0.d0, 1.d0)
      sprdthr = readkey_dbl(md_surfbeatfile, 'sprdthr', 0.08d0, 0.d0, 0.15d0)
      correctHm0 = readkey_int(md_surfbeatfile, 'correctHm0', 1, 0, 1)
      Tm01switch = readkey_int(md_surfbeatfile, 'Tm01switch', 0, 0, 1)
      swkhmin = readkey_dbl(md_surfbeatfile, 'swkhmin', -0.01d0, -0.01d0, 0.35d0)

      nspectrumloc = readkey_int(md_surfbeatfile, 'nspectrumloc', 1, 1, 10000)

      wbcEvarreduce = readkey_dbl(md_surfbeatfile, 'wbcEvarreduce', 1.d0, 0.d0, 1.d0, strict=.true., silent=.true.)
      wbcQvarreduce = readkey_dbl(md_surfbeatfile, 'wbcQvarreduce', 1.d0, 0.d0, 1.d0, strict=.true., silent=.true.)
      wbcScaleEnergy = readkey_int(md_surfbeatfile, 'wbcScaleEnergy', 1, 0, 1, strict=.true., silent=.true.)
      wbcRemoveStokes = readkey_int(md_surfbeatfile, 'wbcRemoveStokes', 1, 0, 1, strict=.true., silent=.true.)

   end if
   !
   if (trim(instat) == 'jons' .or. &
       trim(instat) == 'swan' .or. &
       trim(instat) == 'vardens' .or. &
       trim(instat) == 'stat_table' .or. &
       trim(instat) == 'jons_table' &
       ) then
      filetype = 0 ! JRE to check
      bcfile = readkey_name(md_surfbeatfile, 'bcfile')
      call check_file_exist(bcfile)
      call checkbcfilelength(tstop_user - tstart_user, instat, bcfile, nspectrumloc, filetype)

   elseif (trim(instat) == 'reuse') then
      ! TO DO: check file length is done after recomputation of tstop due to morfacopt
      ! at the end of this subroutine.
      ! JRE: TO DO: implement reuse bc
      !inquire(file='ebcflist.bcf',exist=fe1)
      !inquire(file='qbcflist.bcf',exist=fe2)
      !
      !if (.not. (fe1 .and. fe2)) then
      !   call writelog('lswe','', &
      !      'If ''instat=reuse'' the model directory may not contain sufficient boundary definition files.')
      !   if (.not. fe1) then
      !      call writelog('lswe','','Model currently missing ebcflist.bcf')
      !   elseif (.not. fe2) then
      !      call writelog('lswe','','Model currently missing qbcflist.bcf')
      !   endif
      !   call xbeach_errorhandler()
      !else
      !   call writelog('lswe','','If ''instat=reuse'' the model directory must contain boundary definition files.')
      !   call writelog('lswe','','Use ebcflist.bcf and qbcflist.bcf')
      !   call xbeach_errorhandler()
      !endif
   else
      filetype = -1
   end if
   !
   if (filetype == 0) then
      rt = readkey_dbl(md_surfbeatfile, 'rt', min(3600.d0, tstop_user), 1200.d0, 7200.d0) !! to do
      dtbc = readkey_dbl(md_surfbeatfile, 'dtbc', 1.0d0, 0.1d0, 2.0d0)
   end if

   if (trim(instat) == 'swan') then
      dthetaS_XB = readkey_dbl(md_surfbeatfile, 'dthetaS_XB', 0.0d0, -360.d0, 360.0d0)
   end if
   !
   !
   ! Flow boundary condition parameters
   ! front
   call writelog('l', '', '--------------------------------')
   call writelog('l', '', 'Flow boundary condition parameters: ')
   ARC = readkey_int(md_surfbeatfile, 'ARC', 1, 0, 1)
   order = readkey_dbl(md_surfbeatfile, 'order', 2.d0, 1.d0, 2.d0)
   freewave = readkey_int(md_surfbeatfile, 'freewave', 0, 0, 1)
   !epsi        = readkey_dbl (md_surfbeatfile,'epsi',     -1.d0,          -1.d0,   0.2d0   )
   hminlw = readkey_dbl(md_surfbeatfile, 'hmin', 0.2d0, 0.001d0, 1.d0)
   allocate (allowednames(2), oldnames(0))
   allowednames = (/'abs_1d', 'abs_2d'/)
   absgentype = readkey_str(md_surfbeatfile, 'absgentype', 'abs_1d', 2, 0, allowednames, oldnames)
   if (allocated(allowednames)) deallocate (allowednames, oldnames)
   allocate (allowednames(2), oldnames(0))
   allowednames = (/'instant ', 'velocity'/)
   tidetype = readkey_str(md_surfbeatfile, 'tidetype', 'velocity', 2, 0, allowednames, oldnames)
   deallocate (allowednames, oldnames)
   !
   ! Wave field initialization parameters
   !Trepini = readkey_dbl (md_surfbeatfile,'Trepini',    1.d-5,         1.d-5,    1.d3   )
   !Eini    = readkey_dbl (md_surfbeatfile,'Eini',      1.0d-5,         1.d-5,    1.d10  )

   ! Wave breaking parameters

   if (swave == 1) then
      call writelog('l', '', '--------------------------------')
      call writelog('l', '', 'Wave dissipation parameters: ')
      allocate (allowednames(5), oldnames(5))
      allowednames = (/'roelvink1    ', 'baldock      ', 'roelvink2    ', 'roelvink_daly', 'janssen      '/)
      oldnames = (/'1', '2', '3', '4', '5'/)
      if (trim(instat) == 'stat' .or. trim(instat) == 'stat_table') then
         break = readkey_str(md_surfbeatfile, 'break', 'baldock', 5, 5, allowednames, oldnames)
         gamma = readkey_dbl(md_surfbeatfile, 'gamma', 0.78d0, 0.4d0, 0.9d0)
         gammaxxb = readkey_dbl(md_surfbeatfile, 'gammax', 0.6d0, .4d0, 5.d0)
      else
         break = readkey_str(md_surfbeatfile, 'break', 'roelvink2', 5, 5, allowednames, oldnames)
         gamma = readkey_dbl(md_surfbeatfile, 'gamma', 0.55d0, 0.4d0, 0.9d0)
         gammaxxb = readkey_dbl(md_surfbeatfile, 'gammax', 2.d0, .4d0, 5.d0)
      end if
      deallocate (allowednames, oldnames)
      if (trim(break) == 'roelvink_daly') then
         gamma2 = readkey_dbl(md_surfbeatfile, 'gamma2', 0.3d0, 0.0d0, 0.5d0)
      end if
      rollergammax = readkey_int(md_surfbeatfile, 'rollergammax', 1, 0, 1, strict=.true.)
      alpha = readkey_dbl(md_surfbeatfile, 'alpha', 1.0d0, 0.5d0, 2.0d0)
      nroelvink = readkey_dbl(md_surfbeatfile, 'n', 10.0d0, 5.0d0, 20.0d0)
      deltaH = readkey_dbl(md_surfbeatfile, 'delta', 0.0d0, 0.0d0, 1.0d0)
      wavefricfile = readkey_name(md_surfbeatfile, 'fwfile')
      wavefricval = readkey_dbl(md_surfbeatfile, 'fw', 0.d0, 0d0, 1.0d0)
      fwcutoff = readkey_dbl(md_surfbeatfile, 'fwcutoff', 1000.d0, 0d0, 1000.d0)
      !
      !
      ! Roller parameters
      call writelog('l', '', '--------------------------------')
      call writelog('l', '', 'Roller parameters: ')
      roller = readkey_int(md_surfbeatfile, 'roller', 1, 0, 1, strict=.true.)
      beta = readkey_dbl(md_surfbeatfile, 'beta', 0.10d0, 0.05d0, 0.3d0)
      varbeta = readkey_int(md_surfbeatfile, 'varbeta', 1, 0, 1, strict=.true.)
      rfb = readkey_int(md_surfbeatfile, 'rfb', 0, 0, 1, strict=.true.)
      !
      !
      ! Wave-current interaction parameters
      call writelog('l', '', '--------------------------------')
      call writelog('l', '', 'Wave-current interaction parameters: ')
      wci = readkey_int(md_surfbeatfile, 'wci', 0, 0, 1, strict=.true.)
      hwci = readkey_dbl(md_surfbeatfile, 'hwci', 0.1d0, 0.001d0, 1.d0)
      hwcimax = readkey_dbl(md_surfbeatfile, 'hwcimax', 100.d0, 0.01d0, 100.d0)
      cats = readkey_dbl(md_surfbeatfile, 'cats', 4.d0, 1.d0, 50.d0)
   end if
   !
   !
   ! Wave numerics parameters
   call writelog('l', '', '--------------------------------')
   call writelog('l', '', 'Wave numerics parameters: ')
   if (trim(instat) == 'stat' .or. trim(instat) == 'stat_table' .or. single_dir > 0) then
      wavint = readkey_dbl(md_surfbeatfile, 'wavint', 600.d0, 1.d0, 3600.d0)
      maxerror = readkey_dbl(md_surfbeatfile, 'maxerror', 0.001d0, 0.00001d0, 1d0)
      maxiter = readkey_int(md_surfbeatfile, 'maxiter', 500, 2, 1000)
      dtmaximp = readkey_dbl(md_surfbeatfile, 'dtmax', 1000d0, 1d0, 2500d0)
   end if
   waveps = readkey_dbl(md_surfbeatfile, 'waveps', 0.005d0, 0.001d0, 0.1d0)
   oldhmin = readkey_int(md_surfbeatfile, 'oldhmin', 0, 0, 1, strict=.true.)
   deltahmin = readkey_dbl(md_surfbeatfile, 'deltahmin', 0.1d0, 0.05d0, 0.3d0, strict=.true.)
   !
   !
   ! Windmodel parameters
   !if (windmodel .eq. 1) then
   !   call writelog('l','','--------------------------------')
   !   call writelog('l','','Wind source parameters: ')
   !   mwind       = readkey_dbl (md_surfbeatfile,'mwind',   1.d0,    0.5d0,   1.d0)
   !   jawsource   = readkey_int (md_surfbeatfile,'windsource',   0,    0,   1, required=(swave==1 .and. jawind==1), strict=.true.)
   !   jagradcg    = readkey_int (md_surfbeatfile,'jagradcg',   1,    0,   1, required=((swave==1 .and. jawind==1) .and. jawsource==1), strict=.true.)
   !   advecmod    = readkey_int (md_surfbeatfile,'advecmod',    1,         1,      2)
   !   ndissip     = readkey_dbl (md_surfbeatfile,'ndissip',  3.d0,         1.d0,      10.d0)
   !   coefdispT   = readkey_dbl (md_surfbeatfile,'coefdispT',  3.5d0,         0.d0,      1000.d0)
   !   coefdispk   = readkey_dbl (md_surfbeatfile,'coefdispk',  1.d0,         0.d0,      1000.d0)
   !endif
   !
   !
   ! Roller turbulence parameters
   call writelog('l', '', '--------------------------------')
   call writelog('l', '', 'Roller turbulence parameters: ')

   BRfac = readkey_dbl(md_surfbeatfile, 'BRfac', 1.0d0, 0.d0, 1.d0)
   call setallowednames('none', TURB_NONE, &
                        'wave_averaged', TURB_WAVE_AVERAGED, &
                        'bore_averaged', TURB_BORE_AVERAGED)
   call setoldnames('0', '1', '2')
   call parmapply('turb', 3, turb)

   Tbfac = readkey_dbl(md_surfbeatfile, 'Tbfac  ', 1.0d0, 0.0d0, 1.0d0) ! this is setting for van Thiel. Rest: 0.3
   nuhfac = readkey_dbl(md_surfbeatfile, 'nuhfac  ', 1.0d0, 0.0d0, 1.0d0)
   !
   !
   ! Finish
   call writelog('l', '', '--------------------------------')
   call writelog('sl', '', 'Finished reading input parameters')
   call writelog('l', '', '--------------------------------')
   !
   !
   ! -------------------   Post-input processing -------------------------
   !
   !
   ! Set taper to non-zero
   taper = max(taper, 1.d-6)
   !
   ! Only allow Baldock in stationary mode and Roelvink in non-stationary
   if (trim(instat) == 'stat' .or. trim(instat) == 'stat_table') then
      if (trim(break) /= 'baldock' .and. trim(break) /= 'janssen') then
         if (trim(break) == 'roelvink_daly') then
            call writelog('lwse', '', 'Error: Roelvink-Daly formulations not implemented in stationary wave mode,')
            call writelog('lwse', '', '         use Baldock or Janssen formulation.')
            call xbeach_errorhandler()
         else
            call writelog('lwse', '', 'Error: Roelvink formulations not implemented in stationary wave mode,')
            call writelog('lwse', '', '         use Baldock or Janssen formulation.')
            call xbeach_errorhandler()
         end if
      end if
   else
      if (trim(break) == 'baldock') then
         call writelog('lwse', '', 'Error: Baldock formulation not allowed in non-stationary mode, use a Roelvink')
         call writelog('lwse', '', '       formulation.')
         call xbeach_errorhandler()
      end if
      if (trim(break) == 'janssen') then
         call writelog('lwse', '', 'Error: Janssen formulation not allowed in non-stationary mode, use a Roelvink')
         call writelog('lwse', '', '       formulation.')
         call xbeach_errorhandler()
      end if
   end if
   !
   !facmax = 0.25d0*sqrt(ag)*rhomean*gamma**2
   !
   !
   ! Wave-current interaction with non-stationary waves still experimental
   !if ((trim(instat)/='stat' .and. trim(instat)/='stat_table') .and. wci.ne.0) then
   !   call writelog('lws','','Warning: Wave-current interaction with non-stationary waves is still')
   !   call writelog('lws','','         experimental, continue with computation nevertheless')
   !endif
   if (wci /= 0) then
      call writelog('lws', '', 'Warning: Wave-current interaction is not operational yet. Switched off.')
      wci = 0
   end if
   !
   ! Check for unknown parameters
   call readkey(md_surfbeatfile, 'checkparams', dummystring)

   !   check swave and Lwave
   if (swave == 0) then
      lwave = 0
      call writelog('lws', '', 'Warning: swave is 0, so lwave set to 0.')
   end if

end subroutine xbeach_all_input

subroutine xbeach_wave_init()
   use m_flowgeom
   use fm_external_forcings_data
   use m_xbeach_data
   use m_sferic, only: pi, twopi
   use m_physcoef
   use network_data
   use m_flow, only: hs, ucx, ucy
   use m_waves, only: rlabda

   implicit none

   integer, allocatable, dimension(:) :: idum, kcstore
   double precision, allocatable, dimension(:, :) :: thetalocal

   integer :: itheta, i, k, L, ierror
   integer :: nthetalocal
   integer, parameter :: np = 12

   ! Set some initial values
   if (trim(instat) == 'jons' .or. &
       trim(instat) == 'jons_table' .or. &
       trim(instat) == 'swan' .or. &
       trim(instat) == 'vardens' .or. &
       trim(instat) == 'reuse' &
       ) Trep = 10.d0

   ! Init values water levels and velocities
   hhw = hs
   if (single_dir > 0) then
      hhws = hs
      ucxws = ucx
      ucyws = ucy
   end if

   if (wci > 0) then
      hhwwci = hs
      umwci = ucx
      vmwci = ucy
   end if

   ! Make spectral boundary administration
   if (trim(instat) == 'jons' .or. &
       trim(instat) == 'jons_table' .or. &
       trim(instat) == 'swan' .or. &
       trim(instat) == 'vardens') then
      call xbeach_spectral_wave_init()
   end if

   if (ntheta > 0) then
      ! dispersion
      !if (windmodel .eq. 1) then
      !   tt1 = Trepini
      !   sigt = twopi / tt1
      !   ee1 = Eini
      !else
      do itheta = 1, ntheta
         sigt(itheta, :) = twopi / Trep
      end do
      !endif

      !if (windmodel.eq.0) then
      do k = 1, ndx
         sigmwav(k) = sum(sigt(:, k), dim=1) / dble(ntheta)
         L0(k) = 2 * pi * ag / (sigmwav(k)**2)
         L1(k) = L0(k)
         Ltemp(k) = L0(k)
      end do
      !else
      !   L0t = 2*pi*ag/(sigt**2)
      !   L1t = L0t
      !   Ltempt = L0t
      !endif
      !
      ! for setexternalforcings
      rlabda = L1

      ! initialize celerities
      !if (windmodel .eq. 1) then
      !    call xbeach_dispersion_windmodel()
      !else
      call xbeach_dispersion(hhw)
      !endif

   end if

   ! map boundary types to each other's positions
   if (allocated(kbndu2kbndw)) deallocate (kbndu2kbndw)
   allocate (kbndu2kbndw(nbndu))

   if (allocated(kbndw2kbndu)) deallocate (kbndw2kbndu)
   allocate (kbndw2kbndu(nbndw))

   if (allocated(kbndz2kbndw)) deallocate (kbndz2kbndw)
   allocate (kbndz2kbndw(nbndz))

   allocate (idum(Lnx))

   idum = 0

   !< Map velocity to wave
   do i = 1, nbndw
      L = kbndw(3, i)
      idum(L) = i
   end do

   do i = 1, nbndu
      L = kbndu(3, i)
      kbndu2kbndw(i) = idum(L)
   end do

   !< map wl to wave
   do i = 1, nbndz
      L = kbndz(3, i)
      kbndz2kbndw(i) = idum(L)
   end do

   idum = 0

   !< Map wave to velocity
   do i = 1, nbndu
      L = kbndu(3, i)
      idum(L) = i
   end do

   do i = 1, nbndw
      L = kbndw(3, i)
      kbndw2kbndu(i) = idum(L)
   end do

   if (.not. allocated(uave)) then
      maxnumbnds = 100
      allocate (uave(maxnumbnds), vave(maxnumbnds), dlengthrm(maxnumbnds), stat=ierror)
      allocate (umeanrm(maxnumbnds), vmeanrm(maxnumbnds), stat=ierror)
      uave = 0d0
      vave = 0d0
      dlengthrm = 0d0
      umeanrm = 0d0
      vmeanrm = 0d0
   end if

   !if ( windmodel.eq.1) then
   !   if (jawsource.eq.1) then
   !   !define source term coefficients
   !      CE1 = 8d0/(aa1*aa1*bb1 ) * (16d0/(aa1*aa1 ) )**(1d0/(2d0* bb1) -1d0 )
   !      CE2 = 1d0/(2d0* bb1) -1d0
   !      CT1 = 1d0/(aa2*bb2 ) * (1d0/(aa2 ) )**(1d0/bb2 -1d0 )
   !      CT2 = 1d0/bb2 -1d0
   !   endif
   !   !map wind field to cell centers
   !   call xbeach_map_wind_field(wx, wy, mwind, wmagcc, windspreadfac)
   !endif

   if (trim(instat) == 'stat' .or. trim(instat) == 'stat_table' .or. single_dir > 0) then
      !
      if (.not. allocated(nb)) then
         if (allocated(kcstore)) deallocate (kcstore)
         allocate (kcstore(numk))
         kcstore = kc
         kc = 1
         call makenetnodescoding()
         kc = kcstore
      end if
      !
      ! set inner mask based on netnode coding
      where (nb == 1)
         inner = .true.
      end where
      !
      ! set thetabin according to functionality
      if (single_dir > 0) then
         nthetalocal = ntheta_s
         if (allocated(thetalocal)) deallocate (thetalocal)
         allocate (thetalocal(ntheta_s, numk), stat=ierror)
         do k = 1, numk
            thetalocal(:, k) = thetabin_s
         end do
      else
         nthetalocal = ntheta
         if (allocated(thetalocal)) deallocate (thetalocal)
         allocate (thetalocal(ntheta, numk), stat=ierror)
         do k = 1, numk
            thetalocal(:, k) = thetabin
         end do
      end if

      !
      call getbndwzcornerpts(ierror) ! sets wmask, nmmask, seapoints
      ! get nodes per netcell
      call fill_connected_nodes(ierror)
      ! Find calculation kernel around points
      call fm_surrounding_points(numk, connected_nodes, no_connected_nodes, nump, kp, ierror)
      ! Find upwind neighbours for each grid point and wave direction
      call find_upwind_neighbours(xk, yk, numk, thetalocal, nthetalocal, kp, np, w, prev, ds, ierror)
      ! set e01 (filling of zbndw not necessary for statsolver)
      call xbeach_wave_bc()
      ! compute advection and refraction velocities, will be interpolated to netnodes in solve_wave_stationary
      if (single_dir > 0) then
         call xbeach_wave_dispersion(1)
      else
         call xbeach_wave_dispersion(0)
      end if

      newstatbc = 0 !to check if needed
   end if

   if (allocated(idum)) deallocate (idum)

   return
end subroutine xbeach_wave_init

!> make the thetagrid, in init_flowgeom
subroutine xbeach_makethetagrid()
   use m_flowgeom
   use m_xbeach_data
   use m_sferic
   use m_alloc
   implicit none

   integer :: itheta, ierr, k
   double precision :: thetaminloc

   if (swave == 1) then
      theta0 = (1.5d0 * pi) - dir0 * atan(1.d0) / 45d0
      do while (theta0 < -2d0 * pi)
         theta0 = theta0 + 2.d0 * pi
      end do
      do while (theta0 > 2d0 * pi)
         theta0 = theta0 - 2.d0 * pi
      end do

      if (thetanaut == 1) then
         thetaminloc = thetamin
         thetamin = (270.d0 - thetamax) * dg2rd
         thetamax = (270.d0 - thetaminloc) * dg2rd
      else
         thetamin = thetamin * dg2rd
         thetamax = thetamax * dg2rd
      end if

      thetamin = mod(thetamin, 2.d0 * pi)
      thetamax = mod(thetamax, 2.d0 * pi)

      if (thetamin >= thetamax) then
         if (thetamax >= 0.d0) then
            do while (thetamin >= thetamax)
               thetamin = thetamin - 2.d0 * pi
            end do
         else
            do while (thetamin > thetamax)
               thetamax = thetamax + 2.d0 * pi
            end do
         end if
      elseif (thetamax > thetamin + 2.d0 * pi) then
         do while (thetamax > thetamin + 2.d0 * pi)
            thetamin = thetamin + 2.d0 * pi
         end do
      end if

      if (single_dir == 0) then
         dtheta = dtheta * dg2rd
         if (dtheta > 0d0) then ! safety
            ntheta = max(nint((thetamax - thetamin) / dtheta), 1)
         else
            ntheta = 1
            dtheta = thetamax - thetamin
         end if
      else
         ntheta = 1
         dtheta = thetamax - thetamin
      end if
   else
      dtheta = 2d0 * pi
      ntheta = 1
   end if

   call realloc(csx, ntheta, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('csx  (ntheta)', ierr, ntheta)
   call realloc(snx, ntheta, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('snx  (ntheta)', ierr, ntheta)
   call realloc(thet, (/ntheta, ndx/), stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('thet  (ntheta,ndx)', ierr, ntheta * ndx)
   call realloc(costh, (/ntheta, ndx/), stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('costh  (ntheta,ndx)', ierr, ntheta * ndx)
   call realloc(sinth, (/ntheta, ndx/), stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('sinth  (ntheta,ndx)', ierr, ntheta * ndx)
   call realloc(thetabin, ntheta, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('thetabin  (ntheta)', ierr, ntheta)

   if (single_dir == 1) then
      dtheta_s = dtheta_s * dg2rd
      ntheta_s = nint((thetamax - thetamin) / dtheta_s)
      call realloc(thetabin_s, ntheta_s, stat=ierr, keepExisting=.false., fill=0d0)
      call aerr('thetabin_s  (ntheta_s)', ierr, ntheta_s)
      call realloc(thet_s, (/ntheta_s, ndx/), stat=ierr, keepExisting=.false., fill=0d0)
      call aerr('thet_s  (ntheta_s,ndx)', ierr, ntheta_s * ndx)
      call realloc(costh_s, (/ntheta_s, ndx/), stat=ierr, keepExisting=.false., fill=0d0)
      call aerr('costh_s  (ntheta_s,ndx)', ierr, ntheta_s * ndx)
      call realloc(sinth_s, (/ntheta_s, ndx/), stat=ierr, keepExisting=.false., fill=0d0)
      call aerr('sinth_s  (ntheta_s,ndx)', ierr, ntheta_s * ndx)

   else
      dtheta_s = 2d0 * pi
      ntheta_s = 0
      allocate (thetabin_s(0))
      allocate (thet_s(0, 0))
      allocate (costh_s(0, 0))
      allocate (sinth_s(0, 0))
   end if

   do itheta = 1, ntheta
      thetabin(itheta) = thetamin + dtheta / 2d0 + dtheta * (itheta - 1)
   end do

   do itheta = 1, ntheta
      csx(itheta) = cos(thetabin(itheta))
      snx(itheta) = sin(thetabin(itheta))
      do k = 1, ndx
         thet(itheta, k) = thetabin(itheta)
         costh(itheta, k) = cos(thetabin(itheta))
         sinth(itheta, k) = sin(thetabin(itheta))
      end do
   end do

   if (single_dir == 1) then
      do itheta = 1, ntheta_s
         thetabin_s(itheta) = mod(thetamin + dtheta_s / 2.0 + dtheta_s * (itheta - 1), 2d0 * pi)
      end do

      do itheta = 1, ntheta_s
         do k = 1, ndx
            thet_s(itheta, k) = thetabin_s(itheta)
            costh_s(itheta, k) = cos(mod(thetabin_s(itheta), 2d0 * pi))
            sinth_s(itheta, k) = sin(mod(thetabin_s(itheta), 2d0 * pi))
         end do
      end do
   end if

end subroutine xbeach_makethetagrid

subroutine xbeach_dispersion(hh)
   use m_xbeach_filefunctions
   use m_flowgeom
   use m_flowparameters, only: epshu
   use m_sferic, only: pi
   use m_xbeach_data, only: sigmwav, L0, L1, Ltemp, cwav, nwav, cgwav, kwav
   use m_physcoef, only: ag
   use m_flowtimes, only: time0
   use fm_external_forcings_data

   implicit none

   double precision, dimension(ndx), intent(in) :: hh ! case dependent water depth

   integer :: k, L, k1, k2
   double precision :: kh
   double precision, external :: iteratedispersion

   do k = 1, ndx
      if (hh(k) > epshu) then
         L0(k) = 2 * pi * ag / (sigmwav(k)**2)
      else
         L0(k) = 0d0
      end if
   end do
   L1 = L0

   do k = 1, ndxi
      if (hh(k) > epshu) then
         if (2 * pi / L0(k) * hh(k) > 5d0) then
            Ltemp(k) = L0(k)
         else
            Ltemp(k) = iteratedispersion(L0(k), Ltemp(k), pi, hh(k))
            if (Ltemp(k) < 0.d0) then ! this is an error from iteratedispersion
               Ltemp(k) = -Ltemp(k)
               call writelog('lws', '', 'Warning: no convergence in dispersion relation iteration at t = ', &
                             time0)
            end if
         end if
         L1(k) = Ltemp(k)
      end if
   end do

   do L = 1, nbndz
      k1 = kbndz(1, L); k2 = kbndz(2, L)
      L1(k1) = L1(k2)
   end do

   do L = 1, nbndu
      k1 = kbndu(1, L); k2 = kbndu(2, L)
      L1(k1) = L1(k2)
   end do

   do k = 1, ndx
      kwav(k) = 2 * pi / max(L1(k), epshu)
      cwav(k) = sigmwav(k) / kwav(k)
      kh = min(kwav(k) * hh(k), 10.0d0)
      nwav(k) = 0.5d0 + kh / max(sinh(2d0 * kh), epshu)
      cgwav(k) = cwav(k) * nwav(k)
   end do

   where (hh <= epshu)
      kwav = 25d0
      cwav = sqrt(ag * epshu)
      nwav = 1.d0
      cgwav = sqrt(ag * epshu)
   end where

end subroutine xbeach_dispersion

function iteratedispersion(L0, Lestimate, px, h) result(L)

   implicit none
   ! input
   double precision, intent(in) :: L0
   double precision, intent(in) :: Lestimate
   double precision, intent(in) :: px
   double precision, intent(in) :: h
   ! output
   double precision :: L
   ! internal
   double precision :: L1, L2
   integer :: iter
   double precision :: err
   double precision, parameter :: aphi = 1.d0 / (((1.0d0 + sqrt(5.0d0)) / 2) + 1)
   double precision, parameter :: bphi = ((1.0d0 + sqrt(5.0d0)) / 2) / (((1.0d0 + sqrt(5.0d0)) / 2) + 1)
   integer, parameter :: itermax = 150
   double precision, parameter :: errmax = 0.00001d0

   err = huge(0.0d0)
   iter = 0
   L1 = Lestimate
   do while (err > errmax .and. iter < itermax)
      iter = iter + 1
      L2 = L0 * tanh(2 * px * h / L1)
      L1 = (L1 * aphi + L2 * bphi) ! Golden ratio
      err = abs(L2 - L1)
   end do

   if (iter <= itermax) then
      L = L1
   else
      ! signal this went wrong
      L = -L1
   end if

end function iteratedispersion

subroutine getcellcentergradients(hh, dhsdx, dhsdy)
   use m_flow
   use m_flowgeom

   implicit none

   double precision, intent(in), dimension(ndx) :: hh
   double precision, intent(out), dimension(ndx) :: dhsdx, dhsdy

   integer :: L, k1, k2, k, kb, ki
   double precision :: hs1, hs2

   ! Tegeltjesdiepte approach is eenvoudiger en onnauwkeuriger, maar werkt altijd, ook met morfologie
   dhsdx = 0d0
   dhsdy = 0d0
   do L = 1, Lnx
      if (hu(L) > epshu) then ! link flows
         k1 = ln(1, L)
         k2 = ln(2, L)
         hs1 = hh(k1)
         hs2 = hh(k2)

         dhsdx(k1) = dhsdx(k1) + wcx1(L) * (hs2 - hs1) * dxi(L) ! dimension m/m
         dhsdy(k1) = dhsdy(k1) + wcy1(L) * (hs2 - hs1) * dxi(L)
         dhsdx(k2) = dhsdx(k2) + wcx2(L) * (hs2 - hs1) * dxi(L)
         dhsdy(k2) = dhsdy(k2) + wcy2(L) * (hs2 - hs1) * dxi(L)
      end if
   end do

   do k = 1, nbndu
      kb = kbndu(1, k)
      ki = kbndu(2, k)
      dhsdx(kb) = dhsdx(ki)
      dhsdy(kb) = dhsdy(ki)
   end do

   do k = 1, nbndz
      kb = kbndz(1, k)
      ki = kbndz(2, k)
      dhsdx(kb) = dhsdx(ki)
      dhsdy(kb) = dhsdy(ki)
   end do

end subroutine getcellcentergradients

subroutine xbeach_wave_instationary()
   use m_sferic, only: pi, rd2dg
   use m_physcoef, only: ag
   use m_flowgeom
   use m_flow, only: epshu, vol1, rhomean, epshu, hs
   use m_flowparameters, only: limtypw
   use m_xbeach_data, m_xbeach_data_hminlw => hminlw
   use m_xbeach_paramsconst
   use m_partitioninfo
   use m_timer
   use m_alloc
   use m_waves, only: hwav, twav, phiwav, rlabda, uorb, jauorb
   use m_flowtimes, only: dts
   use m_debug

   implicit none

   integer :: k, itheta, ierr, L, Lf, k1, k2, n
   logical, allocatable :: gammax_correct(:)
   integer, allocatable :: wete(:)
   double precision, allocatable :: hh(:), ddlok(:, :), dd(:, :), drr(:, :)
   double precision, allocatable :: dfac(:)
   !double precision, allocatable  :: Tdeplim(:)
   double precision, allocatable :: RH(:)
   double precision, external :: sinhsafei

   double precision :: cost, sint, rsl, rhog8

   allocate (hh(1:ndx), ddlok(1:ntheta, 1:ndx), dd(1:ntheta, 1:ndx), wete(1:ndx), drr(1:ntheta, 1:ndx), stat=ierr)
   allocate (dfac(1:ndx), stat=ierr)
   !allocate(Tdeplim(1:ndx), stat = ierr)
   allocate (RH(1:ndx), stat=ierr)
   allocate (gammax_correct(1:ndx), stat=ierr)

   xb_started = 1
   !ee_eps = 0.00001d0
   !tt_eps = waveps    !important to limit wave celerities to 1 in case of cells for which hs<epshu

   hh = 0.d0
   ddlok = 0.d0
   wete = 0
   drr = 0.d0
   horadvec = 0d0
   !horadvec2=0d0
   thetaadvec = 0d0
   !thetaadvec2=0d0
   RH = 0d0
   gammax_correct = .false.
   !
   if (wci > 0) then
      hh = hhwwci
   else
      hh = hhw
   end if
   !
   where (hh > epshu)
      wete = 1
   elsewhere
      wete = 0
   end where
   !
   if (wci > 0) then
      call xbeach_wave_dispersion(2)
   else
      call xbeach_wave_dispersion(0)
   end if

   if (single_dir == 0) then
      do k = 1, ndx ! stack
         thetamean(k) = sum(ee1(:, k) * thet(:, k), dim=1) / max(sum(ee1(:, k), dim=1), 0.00001d0) ! energy weighted wave direction
      end do
   else
      ! thetamean determined in wave_stationary, ntheta=1
      do itheta = 1, ntheta
         do k = 1, ndx
            thet(itheta, k) = thetamean(k)
            costh(itheta, k) = cos(thetamean(k))
            sinth(itheta, k) = sin(thetamean(k))
         end do
      end do
   end if

   !  construct and solve system
   !if (windmodel.eq.1) then
   !   if (advecmod.eq.1) then
   !       !define
   !        ma=ee1/sigt
   !        mb=ee1
   !   elseif (advecmod.eq.2) then
   !       !define moments
   !       ma=ee1
   !       mb=sigt*ee1
   !   endif
   !
   !   call advec_horz_windmodel(dtmaxwav, snx, csx, limtypw, ma, cgwavt, horadvec)
   !   call advec_horz_windmodel(dtmaxwav, snx, csx, limtypw, mb, cgwavt, horadvec2)
   !   call advec_dir(ma, ctheta, thetaadvec)
   !   call advec_dir(mb, ctheta, thetaadvec2)
   !
   !   do k = 1,ndxi
   !      do itheta = 1,ntheta
   !         if ( vol1(k) > epshu*ba(k) ) then
   !               ma(itheta,k) = ma(itheta,k) - dtmaxwav*(horadvec(itheta,k)  * bai(k) + thetaadvec(itheta,k))
   !               mb(itheta,k) = mb(itheta,k) - dtmaxwav*(horadvec2(itheta,k)  * bai(k) + thetaadvec2(itheta,k))
   !            else
   !               ma(itheta,k)=ee_eps*tt_eps/twopi
   !               mb(itheta,k)=ee_eps
   !            endif
   !         enddo
   !   enddo
   !
   !   if (advecmod.eq.1) then
   !      sigt=min(mb/ma,twopi/tt_eps)
   !      ee1=max(mb,ee_eps)
   !      tt1=twopi/sigt
   !  else
   !     sigt=min(mb/ma,twopi/tt_eps)
   !     ee1=max(ma,ee_eps)
   !     tt1=twopi/sigt
   !   endif
   !
   !   call xbeach_wave_dispersion(0)
   !
   !else !regular xbeach approach, fixed period
   !
   ! Slopes of water depth
   call getcellcentergradients(hh, dhsdx, dhsdy)
   if (wci > 0) then
      call getcellcentergradients(umwci, xbducxdx, xbducxdy)
      call getcellcentergradients(vmwci, xbducydx, xbducydy)
   else
      xbducxdx = 0.d0
      xbducydx = 0.d0
      xbducxdy = 0.d0
      xbducydy = 0.d0
   end if
   !
   ! Calculate sinh(2kh)
   where (2d0 * hh * kwav <= 3000.d0)
      sinh2kh = sinh(min(2d0 * kwav * hh, 10.0d0))
   elsewhere
      sinh2kh = 3000.d0
   end where
   !
   call xbeach_compute_wave_velocities(1, dhsdx, dhsdy, xbducxdx, xbducxdy, xbducydx, xbducydy, sinh2kh)
   !
   ee1 = ee1 / sigt
   call advec_horz(dts, sinth, costh, limtypw, ee1, cgwav, horadvec)
   call advec_dir(ee1, ctheta, thetaadvec)

   do k = 1, ndxi
      do itheta = 1, ntheta
         if (vol1(k) > epshu * ba(k)) then
            ee1(itheta, k) = ee1(itheta, k) - dts * (horadvec(itheta, k) * bai(k) + thetaadvec(itheta, k))
         else
            ee1(itheta, k) = 0d0
         end if
      end do
   end do

   ee1 = ee1 * sigt ! Back to wave energy
   ee1 = max(ee1, 0.0d0)

   !endif
   !
   where (wete == 1)
      E = sum(ee1, dim=1) * dtheta
   elsewhere
      E = 0.d0
   end where
   H = sqrt(8.d0 * E / rhomean / ag)
   !
   ! Correct for gammax in these areas
   where (H > gammaxxb * hs .and. wete == 1)
      gammax_correct = .true.
   elsewhere
      gammax_correct = .false.
   end where
   !
   do itheta = 1, ntheta
      where (gammax_correct)
         ee1(itheta, :) = ee1(itheta, :) / (H / (gammaxxb * hs))**2
      end where
   end do
   !
   where (gammax_correct)
      H = min(H, gammaxxb * hs)
   end where
   !
   rhog8 = rhomean * ag / 8d0
   where (gammax_correct)
      E = rhog8 * H**2
   end where

   !   Breaker dissipation
   !call xbeach_wave_breaker_dissipation(dts, break, DeltaH, waveps, hhw, kwav, km, gamma, gamma2, nroelvink, QB, alpha, Trep, cwav, thetamean, E, D, sigmwav, wci, 0)
   call xbeach_wave_breaker_dissipation(dts, break, DeltaH, waveps, hhw, kwav, km, gamma, gamma2, nroelvink, QB, alpha, Trep, cwav, thetamean, H, D, sigmwav, wci, 0)

   !   Dissipation by bed friction
   dfac = 2.d0 * fw * rhomean / (3.d0 * pi)
   do k = 1, Ndx
      uorb(k) = H(k) * sigmwav(k) / 2d0 * sinhsafei(kwav(k) * hh(k)) ! uorb uit XBeach
      Df(k) = dfac(k) * uorb(k)**3
   end do

   if (jauorb == 0) then ! old d3d convention
      uorb = uorb * sqrt(pi) / 2d0 ! only on hrms derived value, not on SWAN read uorb
   end if

   where (hh > fwcutoff)
      Df = 0.d0
   end where
   !
   !   Distribution of total dissipation over directions
   !
   do itheta = 1, ntheta
      ddlok(itheta, :) = ee1(itheta, :) * D / max(E, 1d-5) ! breaking
      dd(itheta, :) = ddlok(itheta, :) + ee1(itheta, :) * Df / max(E, 1d-5) ! breaking plus friction
   end do

   !if (windmodel.eq.1) then
   !  ! wave period depth limitation
   !   call xbeach_wave_compute_period_depth_limitation( 1.d0/8.d0*rhomean*ag*(gammaxxb*hh**2) , Tdeplim)
   !   do itheta=1,ntheta
   !      tt1(itheta,:) = min(tt1(itheta,:) , Tdeplim )
   !   enddo
   !   sigt=twopi/tt1
   !   sigmwav=sum(sigt,dim=1)/ntheta
   !
   !   ! wave period breaker dissipation
   !   call xbeach_wave_period_breaker_dissipation( D, E, sigmwav, cgwav, kwav, ddT)
   !
   !   !  Wind source term
   !   if (jawsource.eq.1) then
   !      call xbeach_windsource(ee1, E, tt1, sigmwav , cgwavt, cgwav, hh, dtmaxwav, wsorE, wsorT,egradcg,SwE,SwT)
   !   else
   !       wsorE=0.d0
   !       wsorT=0.d0
   !       SwE=0.d0
   !       SwT=0.d0
   !   endif
   !endif ! windmodel

   ! Roller energy balance
   call advec_horz(dts, sinth, costh, limtypw, rr, cwav, rrhoradvec)
   call advec_dir(rr, ctheta, rrthetaadvec)

   do k = 1, ndxi
      do itheta = 1, ntheta
         if (vol1(k) > epshu * ba(k)) then
            rr(itheta, k) = rr(itheta, k) - dts * (rrhoradvec(itheta, k) * bai(k) + rrthetaadvec(itheta, k))
         else
            rr(itheta, k) = 0d0
         end if
      end do
   end do

   rr = max(rr, 0.0d0)

   !  euler step roller energy dissipation
   !if (windmodel.eq.1) then
   !    do k = 1,ndx
   !      do itheta=1,ntheta
   !         if (wete(k)==1) then
   !            ee1(itheta,k) = ee1(itheta, k) + min( dtmaxwav * (wsorE(itheta, k) -  dd(itheta, k) )  , ee1(itheta, k) )
   !            tt1(itheta,k) = tt1(itheta, k) + min( dtmaxwav * (wsorT(itheta, k) -  ddT(k) ) , tt1(itheta, k) )
   !            !
   !            if(roller==1) then
   !                  drr(itheta, k) = 2*ag*BR(k)*max(rr(itheta, k),0.0d0)/ cwav(k)
   !                  rr(itheta, k)=rr(itheta, k)+dtmaxwav*(ddlok(itheta, k) -drr(itheta, k))
   !            else if (roller==0) then
   !               rr(itheta, k)  = 0.0d0
   !               drr(itheta, k) = 0.0d0
   !            endif
   !            !
   !            ee1(itheta, k)    = max(ee1(itheta, k),ee_eps)
   !            tt1(itheta, k)    = max(tt1(itheta, k), tt_eps)
   !            rr(itheta, k)     = max(rr(itheta, k),0.0d0)
   !         else
   !            ee1(itheta, k)    = ee_eps
   !            tt1(itheta, k)    = tt_eps
   !            rr(itheta, k)     = 0.0d0
   !         end if
   !      end do
   !    end do
   !
   !    tt1=max(tt1,tt_eps)
   !    ee1=max(ee1,ee_eps)
   !    sigt=twopi/tt1
   !
   !else !if windmodel=0

   do k = 1, ndx
      !
      do itheta = 1, ntheta
         if (wete(k) == 1) then
            ee1(itheta, k) = ee1(itheta, k) - dts * dd(itheta, k) ! totale dissipatie
            if (roller > 0) then
               drr(itheta, k) = 2.0 * ag * BR(k) * max(rr(itheta, k), 0.0d0) / cwav(k)
               rr(itheta, k) = rr(itheta, k) + dts * (ddlok(itheta, k) - drr(itheta, k)) ! only wave breaker dissipation
            else
               rr(itheta, k) = 0.0d0
               drr(itheta, k) = 0.0d0
            end if
            ee1(itheta, k) = max(ee1(itheta, k), 0.0d0)
            rr(itheta, k) = max(rr(itheta, k), 0.0d0)
         else
            ee1(itheta, k) = 0.0d0
            rr(itheta, k) = 0.0d0
         end if ! wete
      end do
   end do
   !endif !windmodel

   if (jampi == 1) then
      if (jatimer == 1) call starttimer(IXBEACH)
      call update_ghosts(ITYPE_Sall, Ntheta, Ndx, ee1, ierr)
      call update_ghosts(ITYPE_Sall, Ntheta, Ndx, rr, ierr)
      if (jatimer == 1) call stoptimer(IXBEACH)
   end if

   !   OUTPUT Bulk quantities
   do k = 1, ndx ! stack
      E(k) = sum(ee1(:, k), dim=1) * dtheta
      H(k) = sqrt(8.d0 * E(k) / rhomean / ag)
   end do
   Dtot = D + Df

   if (roller > 0) then
      do k = 1, ndx
         R(k) = sum(rr(:, k), dim=1) * dtheta
         DR(k) = sum(drr(:, k), dim=1) * dtheta
      end do
   else ! need something here for mor
      rsl = sin(beta)
      do k = 1, ndx
         R(k) = 9d-1 * rhomean * ag * rsl * H(k)**2 ! Martins 2018
         DR(k) = 2.0 * ag * beta * R(k) / cwav(k)
      end do
   end if
   !
   ! Only calculate thetamean when pure instationary
   ! For single_dir, this is done in stationary part
   if (single_dir == 0) then
      do k = 1, ndx
         thetamean(k) = (sum(ee1(:, k) * thet(:, k), dim=1) / dble(ntheta)) / (max(sum(ee1(:, k), dim=1), 0.00001d0) / dble(ntheta))
      end do
      !
      ! Copy thetamean to first dry cells next to waterline by simple averaging
      do k = 1, ndx
         if (hs(k) > epshu) cycle
         n = 0
         cost = 0d0
         sint = 0d0
         do L = 1, nd(k)%lnx
            Lf = abs(L)
            k1 = ln(1, Lf)
            if (k1 == k) then
               k2 = ln(2, Lf)
               if (hs(k2) <= epshu) cycle
               n = n + 1
               cost = cost + cos(thetamean(k2))
               sint = sint + sin(thetamean(k2))
            else
               k2 = k
               if (hs(k1) <= epshu) cycle
               n = n + 1
               cost = cost + cos(thetamean(k1))
               sint = sint + sin(thetamean(k1))
            end if
         end do
         if (n > 0) then
            thetamean(k) = atan2(sint / dble(n), cost / dble(n))
         end if
      end do
   end if
   !
   ! Energy limitation roller; strictly speaking, I would expect this after advection step
   gammax_correct = .false.
   if (rollergammax == 1) then
      RH = sqrt(8d0 * R / rhomean / ag)
      where (RH > gammaxxb * hhw .and. wete == 1)
         gammax_correct = .true.
      elsewhere
         gammax_correct = .false.
      end where
      !
      do itheta = 1, ntheta
         where (gammax_correct)
            rr(itheta, :) = rr(itheta, :) / (RH / (gammaxxb * hhw))**2
         end where
      end do
      where (gammax_correct)
         R = min(R, gammaxxb * hhw)
      end where
      !
      ! Correct roller dissipation, check with Dano
      !where (hs>epshu)
      !   DR = 2d0*ag*beta1*R/cwav
      !endwhere
   end if

   if (roller == 1 .and. turb /= TURB_NONE) then
      call borecharacter() ! calculates BR and Tbore using Rieneck&Fenton approach
   end if

   ! En voor de uniformiteit van de golfkoppelingetjes:
   hwav = H
   twav = 2.0 * pi / sigmwav
   phiwav = thetamean * rd2dg
   rlabda = L1

   deallocate (hh, ddlok, wete, drr, stat=ierr)
   !deallocate(Tdeplim, stat=ierr)
   deallocate (RH, stat=ierr)
   deallocate (gammax_correct, stat=ierr)

end subroutine xbeach_wave_instationary

subroutine xbeach_wave_compute_flowforcing2D()
   use m_flowgeom
   use m_flow
   use m_xbeach_data
   use m_flowgeom
   use m_partitioninfo
   !use m_turbulence, only:rho
   use m_physcoef
   use m_timer

   implicit none

   integer :: k, n, L, k1, k2
   double precision :: rhoL
   integer :: ierror
   double precision, allocatable, save :: dsxxdx(:), dsyydy(:), dsxydx(:), dsxydy(:)

   if (.not. allocated(dsxxdx)) then
      allocate (dsxxdx(1:ndx), dsyydy(1:ndx), dsxydx(1:ndx), dsxydy(1:ndx))
   end if

   !   Radiation stresses
   nwav = cgwav / max(cwav, sqrt(ag * epshu))
   do k = 1, ndx ! stack
      Sxx(k) = (nwav(k) * sum((1.d0 + costh(:, k)**2) * ee1(:, k), dim=1) - .5d0 * sum(ee1(:, k), dim=1)) * dtheta ! wave energy contribution
      Syy(k) = (nwav(k) * sum((1.d0 + sinth(:, k)**2) * ee1(:, k), dim=1) - .5d0 * sum(ee1(:, k), dim=1)) * dtheta
      Sxy(k) = nwav(k) * sum(sinth(:, k) * costh(:, k) * ee1(:, k), dim=1) * dtheta

      Sxx(k) = Sxx(k) + sum((costh(:, k)**2) * rr(:, k), dim=1) * dtheta ! Roller contribution
      Syy(k) = Syy(k) + sum((sinth(:, k)**2) * rr(:, k), dim=1) * dtheta
      Sxy(k) = Sxy(k) + sum(sinth(:, k) * costh(:, k) * rr(:, k), dim=1) * dtheta
   end do

   !   Wave forces Fx, Fy, value on links
   Fx_cc = 0d0
   Fy_cc = 0d0
   dsxxdx = 0d0
   dsyydy = 0d0
   dsxydy = 0d0
   dsxydx = 0d0
   ! Jipjanneke
   do L = 1, lnx
      k1 = ln(1, L)
      k2 = ln(2, L)
      dsxxdx(k1) = dsxxdx(k1) + wcx1(L) * (Sxx(k2) - Sxx(k1)) * dxi(L)
      dsxxdx(k2) = dsxxdx(k2) + wcx2(L) * (Sxx(k2) - Sxx(k1)) * dxi(L)

      dsyydy(k1) = dsyydy(k1) + wcy1(L) * (Syy(k2) - Syy(k1)) * dxi(L)
      dsyydy(k2) = dsyydy(k2) + wcy2(L) * (Syy(k2) - Syy(k1)) * dxi(L)

      dsxydx(k1) = dsxydx(k1) + wcx1(L) * (Sxy(k2) - Sxy(k1)) * dxi(L)
      dsxydx(k2) = dsxydx(k2) + wcx2(L) * (Sxy(k2) - Sxy(k1)) * dxi(L)

      dsxydy(k1) = dsxydy(k1) + wcy1(L) * (Sxy(k2) - Sxy(k1)) * dxi(L)
      dsxydy(k2) = dsxydy(k2) + wcy2(L) * (Sxy(k2) - Sxy(k1)) * dxi(L)
   end do

   do k = 1, ndx
      Fx_cc(k) = -dsxxdx(k) - dsxydy(k)
      Fy_cc(k) = -dsxydx(k) - dsyydy(k)
   end do

   ! Open boundaries: Neumann
   do n = 1, nbndw ! not necessary, no wave bnd without open flow bnd
      k1 = kbndw(1, n); k2 = kbndw(2, n)
      Fx_cc(k1) = Fx_cc(k2)
      Fy_cc(k1) = Fy_cc(k2)
   end do

   do n = 1, nbndz
      k1 = kbndz(1, n); k2 = kbndz(2, n)
      Fx_cc(k1) = Fx_cc(k2)
      Fy_cc(k1) = Fy_cc(k2)
   end do

   do n = 1, nbndu
      k1 = kbndu(1, n); k2 = kbndu(2, n)
      Fx_cc(k1) = Fx_cc(k2)
      Fy_cc(k1) = Fy_cc(k2)
   end do

   if (jampi == 1) then
      if (jatimer == 1) call starttimer(IXBEACH)
      call update_ghosts(ITYPE_SALL, 1, ndx, Fx_cc, ierror)
      call update_ghosts(ITYPE_SALL, 1, ndx, Fy_cc, ierror)
      if (jatimer == 1) call stoptimer(IXBEACH)
   end if

   ! Compute wavfu for 2D runs;
   ! For 3D, this is done in setwavfu using results of xbeach_wave_compute_flowforcing3D; we only need the Fx_cc, Fy_cc values
   if (kmx == 0) then
      do L = 1, Lnx
         k1 = ln(1, L); k2 = ln(2, L)
         Fx(L) = (acL(L) * Fx_cc(k1) + (1d0 - acL(L)) * Fx_cc(k2))
         Fy(L) = (acL(L) * Fy_cc(k1) + (1d0 - acL(L)) * Fy_cc(k2))
         !rhoL     = ( acL(L)*rho(k1) + (1d0-acL(L))*rho(k2) )
         rhoL = rhomean
         wavfu(L) = (Fx(L) * csu(L) + Fy(L) * snu(L)) / (rhoL * max(hu(L), hminlw))
         wavfv(L) = (-Fx(L) * snu(L) + Fy(L) * csu(L)) / (rhoL * max(hu(L), hminlw))
      end do

      where (hu <= epshu)
         wavfu = 0d0
         wavfv = 0d0
      end where
   end if

end subroutine xbeach_wave_compute_flowforcing2D

subroutine xbeach_wave_maxtimestep()
   use m_flowtimes
   use m_flow
   use m_flowgeom
   use m_xbeach_data
   use m_partitioninfo

   implicit none

   integer :: k, k1, k2, kk, L, itheta
   double precision :: dum, cgwavL, cwuL, dt, kkcflmxloc

   dtmaxwav = huge(0d0)
   kkcflmxloc = 0

   ! Calculate max CFL based timestep for wave calculation
   do k = 1, ndx
      if (jampi == 1) then
         !            do not include ghost cells
         if (idomain(k) /= my_rank) cycle
      end if
      do itheta = 1, ntheta
         dum = 0.d0
         do kk = 1, nd(k)%lnx
            L = abs(nd(k)%ln(kk))
            k1 = ln(1, L)
            k2 = ln(2, L)

            cgwavL = acL(L) * cgwav(k1) + (1 - acL(L)) * cgwav(k2)
            cwuL = cgwavL * (csu(L) * csx(itheta) + snu(L) * snx(itheta))

            if (ln(2, L) == k) cwuL = -cwuL

            if (cwuL >= 0.) then ! outgoing velocities only
               dum = dum + cwuL * wu(L)
            end if
         end do
         if (dum > tiny(0d0)) then
            dt = cflmx * ba(k) / dum
            if (dt < dtmaxwav) then
               dtmaxwav = dt
            end if
         end if
         dum = ctheta(itheta, k) / dtheta
         if (dum > tiny(0d0)) then
            dt = cflmx / dum
            if (dt < dtmaxwav) then
               dtmaxwav = dt
               kkcflmxloc = k
            end if
         end if
      end do
   end do
   !
   if (dtmaxwav > dts) then
      dtmaxwav = dts
   else
      kkcflmx = kkcflmxloc ! overwrite cell number for numlimdt when new smallest timestep
      if (jamapFlowAnalysis > 0) then
         limitingTimestepEstimation(kkcflmx) = limitingTimestepEstimation(kkcflmx) + 1
      end if
   end if

   dts = dtmaxwav

end subroutine xbeach_wave_maxtimestep

subroutine xbeach_wave_dispersion(callType)
   use m_flow, only: ucx, ucy
   use m_flowgeom
   use m_flowparameters, only: epshu
   use m_flowtimes
   use m_sferic, only: pi
   use m_xbeach_data
   use fm_external_forcings_data, only: nbndw, kbndw, nbndz, kbndz, nbndu, kbndu
   use m_physcoef, only: ag

   implicit none

   integer, intent(in) :: callType

   integer :: k, k1, k2, kb, itheta, L, n
   double precision, save :: Trepold
   double precision, allocatable, save :: hh(:), ulocal(:), vlocal(:)
   double precision, allocatable, save :: dkmydx(:), dkmydy(:), dkmxdx(:), dkmxdy(:)
   double precision, allocatable, save :: arg(:), cgxm(:), cgym(:), fac(:), kmx(:), kmy(:), wm(:), wmadvec(:)
   double precision, allocatable, save :: advel(:), advec(:)

   ! allocate arrays
   if (.not. allocated(kmx)) then
      allocate (kmx(1:ndx))
      allocate (kmy(1:ndx))
      allocate (arg(1:ndx))
      allocate (fac(1:ndx))
      allocate (cgym(1:ndx))
      allocate (cgxm(1:ndx))
      allocate (dkmxdx(1:ndx))
      allocate (dkmxdy(1:ndx))
      allocate (dkmydx(1:ndx))
      allocate (dkmydy(1:ndx))
      allocate (wmadvec(1:ndx))
      allocate (wm(1:ndx))
      allocate (advel(1:ndx))
      allocate (advec(1:ndx))
      allocate (hh(1:ndx))
      if (wci > 0) then
         allocate (ulocal(1:ndx))
         allocate (vlocal(1:ndx))
         L0 = 0.d0
         L1 = -huge(0.d0)
      end if
      Trepold = 0.d0
      call xbeach_dispersion(hhw) ! at initialisation, water depth is always hhw==hs
      km = kwav
   end if

   ! set appropriate water depth and velocities according to mechanisms in model
   select case (callType)
   case (0) ! instantaneous waterdepths and velocities for regular stationary, instationary runs
      hh = hhw
      if (wci > 0) then
         ulocal = ucx
         vlocal = ucy
      end if
   case (1) ! single_dir relaxation for stationary part
      hh = hhws
      if (wci > 0) then
         ulocal = ucxws
         vlocal = ucyws
      end if
   case (2) ! wci relaxation for instationary runs
      hh = hhwwci
      ulocal = umwci
      vlocal = vmwci
   end select

   if (wci > 0) then
      arg = min(100.0d0, km * hh)
      fac = (1.d0 + ((km * H / 2.d0)**2)) ! use deep water correction
      do n = 1, nbndw
         kb = kbndw(1, n)
         sigmwav(kb) = sqrt(ag * km(kb) * tanh(arg(kb)))
      end do
      !  calculate change in intrinsic frequency
      kmx = km * cos(thetamean)
      kmy = km * sin(thetamean)
      wm = sigmwav + kmx * ulocal * min( &
           min(hh / hwci, 1.d0), &
           min(1.d0, (1.d0 - hh / hwcimax)) &
           ) + &
           kmy * vlocal * min( &
           min(hh / hwci, 1.d0), &
           min(1.d0, (1.d0 - hh / hwcimax)) &
           )
!
      where (km > 0.01d0)
         cwav = sigmwav / km
         cgwav = cwav * (0.5d0 + arg / sinh(2 * arg)) * sqrt(fac) ! &  to include more
         !                                   + km*(H/2)**2*sqrt(max(par%g*km*tanh(arg),0.001d0))/sqrt(max(fac,0.001d0)) ! include wave steepness
         nwav = 0.5d0 + km * hh / sinh(2 * max(km, 0.00001d0) * hh)
      elsewhere
         cwav = sqrt(ag * epshu)
         cgwav = sqrt(ag * epshu)
         nwav = 1.d0
      end where
      !
      cgym = cgwav * sin(thetamean) + vlocal * min(min(hh / hwci, 1.d0), min(1.d0, (1.d0 - hh / hwcimax)))
      cgxm = cgwav * cos(thetamean) + ulocal * min(min(hh / hwci, 1.d0), min(1.d0, (1.d0 - hh / hwcimax)))
      !
      ! Compute slopes of wave number in cell centres
      dkmydx = 0d0
      dkmxdy = 0d0
      do L = 1, lnx
         k1 = ln(1, L)
         k2 = ln(2, L)
         dkmydx(k1) = dkmydx(k1) + wcx1(L) * (kmy(k2) - kmy(k1)) * dxi(L)
         dkmydx(k2) = dkmydx(k2) + wcx2(L) * (kmy(k2) - kmy(k1)) * dxi(L)
         dkmxdy(k1) = dkmxdy(k1) + wcy1(L) * (kmx(k2) - kmx(k1)) * dxi(L)
         dkmxdy(k2) = dkmxdy(k2) + wcy2(L) * (kmx(k2) - kmx(k1)) * dxi(L)
      end do
      !
      ! Calculate advection part
      advel = 1d0
      advec = 0d0
      call advec_upw_bulk(thetamean, wm, advel, advec)
      do k = 1, ndx
         km(k) = km(k) - dts * advec(k) * bai(k)
         kmx(k) = kmx(k) - dts * cgym(k) * (dkmydx(k) - dkmxdy(k))
         kmy(k) = kmy(k) + dts * cgxm(k) * (dkmydx(k) - dkmxdy(k))
         km(k) = km(k) + hypot(kmx(k), kmy(k))
         km(k) = min(km(k), 25d0)
      end do
      !
      do n = 1, nbndw
         k1 = kbndw(1, n); k2 = kbndw(2, n)
         km(k1) = km(k2)
      end do
      !
      do n = 1, nbndz
         k1 = kbndz(1, n); k2 = kbndz(2, n)
         km(k1) = km(k2)
      end do
      !
      do n = 1, nbndu
         k1 = kbndu(1, n); k2 = kbndu(2, n)
         km(k1) = km(k2)
      end do
      !
      ! non-linear dispersion
      arg = min(100.0d0, km * hh)
      arg = max(arg, 0.0001d0)
      !
      fac = (1.d0 + ((km * H / 2.d0)**2))
      !
      sigmwav = sqrt(ag * km * tanh(arg) * fac)
      sigmwav = max(sigmwav, 0.010d0)
      !  update intrinsic frequency
      do itheta = 1, ntheta
         sigt(itheta, :) = sigmwav
      end do
      !
      !  update k
      kwav = km
   else
      ! check if we need to recompute sigm and sigt
      if (abs(Trep - Trepold) / Trep > 1d-4) then
         Trepold = Trep
         sigmwav = 2d0 * pi / Trep
         do itheta = 1, ntheta
            sigt(itheta, :) = sigmwav
         end do
      end if
      call xbeach_dispersion(hh) ! gives k, c, cg, n for non-wci
   end if ! wci>0
end subroutine ! xbeach_wave_dispersion

!> compute wave boundary conditions
subroutine xbeach_wave_bc()
   use m_flowgeom
   use m_xbeach_data
   use fm_external_forcings_data
   use wave_boundary_main_module
   use m_flowtimes, only: time0, tstop_user, tstart_user
   use m_physcoef, only: rhomean, ag
   use m_sferic, only: pi
   use m_flowparameters, only: epshu
   use m_flow, only: hs
   use m_alloc
   use m_xbeach_filefunctions
   use wave_boundary_datastore
   use interp
   use m_partitioninfo
   use m_alloc

   implicit none

   integer, save :: nt
   integer, save :: curline
   integer :: i, it, itheta, E_idx, ier, ierror, clock, idum(nwbnd)
   integer, save :: bctype
   double precision :: E1, ei, dum, Hm0, dum1, spreadpar, bcdur, dum2, cgwavin
   double precision, save :: bcendtime
   double precision :: em, tshifted, hboundary(nwbnd)
   double precision, save :: Emean, Llong
   double precision :: hh, ht
   character(len=1) :: bline
   logical :: startbcf

   double precision, allocatable, save :: dist(:), factor(:)

   double precision :: E0
   double precision :: bl1, bl2
   double precision, dimension(nbndw) :: qxbc, qybc
   double precision, dimension(nbndw, ntheta) :: eeout
   double precision, dimension(:, :), allocatable :: ees

   double precision :: Hbc, Tbc, Dbc

   logical :: isRecomputed

   integer :: kb, ki, Lb, nw
   integer :: LL1, LL2, n, lunfil

   ierror = 1
   if (.not. allocated(dist)) allocate (dist(1:ntheta), factor(1:ntheta), e01(1:ntheta))
   !
   eeout = 0d0
   uin = 0d0
   vin = 0d0
   qxbc = 0d0
   qybc = 0d0
   !
   !  note: also in xbeach_spectral_wave_init
   call get_hboundary(hboundary)
   !
   startbcf = .false.
   !
   if (.not. (trim(instat) == 'stat' .or. &
              trim(instat) == 'bichrom' .or. &
              trim(instat) == 'ts_1' .or. &
              trim(instat) == 'ts_2' .or. &
              trim(instat) == 'stat_table' &
              )) then

      select case (trim(instat))
      case ('jons')
         bctype = 4
      case ('jons_table')
         bctype = 11
      end select

      ! Regenerate random seed
      if (random == 1) then
         do i = 1, nwbnd
            call system_clock(count=clock)
            randomseed(i) = clock
         end do
         !
         if (jampi == 1) then
            idum = randomseed
            call reduce_int_max(nwbnd, idum)
            randomseed = idum
         end if
      else
         randomseed = -999
      end if
      !
      do n = 1, nwbnd
         Trep = 1d-1 ! safety for max reduction below
         LL1 = L1wbnd(n)
         LL2 = L2wbnd(n)
         if (LL1 > LL2) cycle
         if (jampi == 1) then
            !k = ln(2,LL1)
            !if (.not.(idomain(k)==my_rank) .or. LL2==0) then     ! then not a boundary domain, second check is safety
            if (LL2 == 0) then
               cycle
            end if
         end if
         waveBoundaryParameters(n)%hboundary = hboundary(n)
         waveBoundaryParameters(n)%randomseed = randomseed(n)

         call realloc(ees, (/ntheta_s, LL2 - LL1 + 1/), keepExisting=.false., fill=0d0)

         call create_incident_waves_surfbeat(LL2 - LL1 + 1, n, xbndw(LL1:LL2), ybndw(LL1:LL2), &
                                             waveBoundaryParameters(n)%ntheta, waveBoundaryParameters(n)%dtheta, waveBoundaryParameters(n)%theta, time0, &
                                             bctype, bcfile, &
                                             waveBoundaryParameters(n)%x0, waveBoundaryParameters(n)%y0, waveBoundaryParameters(n)%hboundary, &
                                             waveBoundaryParameters(n)%randomseed, &
                                             eeout(LL1:LL2, :), qxbc(LL1:LL2), qybc(LL1:LL2), &
                                             Hbc, Tbc, Dbc, isRecomputed, single_dir, ntheta_s, thetabin_s, ees, &
                                             nspr=nspr, sprdthr=sprdthr, &
                                             trepfac=trepfac, nmax=nwavmax, fcutoff=fcutoff, rho=rhomean, &
                                             Tm01switch=Tm01switch, swkhmin=swkhmin, &
                                             wbcScaleEnergy=wbcScaleEnergy, wbcEvarreduce=wbcEvarreduce, &
                                             wbcQvarreduce=wbcQvarreduce, wbcRemoveStokes=wbcRemoveStokes &
                                             )
         ! Watch out: Hbc is Hm0
         Trep = Tbc
         dir0 = mod(270d0 - Dbc / pi * 180d0, 360d0) ! for single_dir and absgen_bc
         if (single_dir > 0) then
            ee_s(:, LL1:LL2) = ees
         end if
      end do
      !
      if (jampi > 0) then
         dum = Trep
         call reduce_double_max(dum) ! JRE to check what this does in domains without wave energy bnds
         Trep = dum ! should be okay with LL2==0 statement above
      end if
      !
      do n = 1, nbndu !! for absgen bnd's
         nw = kbndu2kbndw(n)
         if (nw > 0) then
            hh = max(zbndu(n) - bl(kbndw(1, nw)), epshu) ! divide by slowly varying depth signal, not wave group time scale depth
            uin(n) = qxbc(nw) / hh ! cartesian x and y, are oriented later according to link direction in absgen_bc
            vin(n) = qybc(nw) / hh
         else
            uin(n) = 0d0
            vin(n) = 0d0
         end if
      end do
      !
      do n = 1, nbndw
         zbndw(1:ntheta, n) = eeout(n, 1:ntheta)
      end do

   else !! instat = stat, stat_table, ts_1, ts_2, bichrom
      if (.not. bccreated) then
         call writelog('ls', '', 'Setting up boundary conditions')
         bccreated = .true.
         startbcf = .true. ! trigger read from bcf for instat 3,4,5,7
         bcendtime = huge(0.0d0) ! initial assumption for instat 3,4,5,7
         newstatbc = 1

         call get_refpoint(xref0, yref0)

         if (trim(instat) == 'ts_1') then
            open (newunit=lunfil, file='bc/gen.ezs')
5           continue
            read (lunfil, '(a)', iostat=ier) bline
            if (ier /= 0) then
               call report_file_read_error('bc/gen.ezs')
            end if
            if (bline == '*') goto 5
            read (lunfil, *, iostat=ier) nt ! no of timesteps
            if (ier /= 0) then
               call report_file_read_error('bc/gen.ezs')
            end if

            allocate (dataE(nt))
            allocate (tE(nt))
            do i = 1, nt
               read (lunfil, *, iostat=ier) tE(i), dum, dataE(i)
               if (ier /= 0) then
                  call report_file_read_error('bc/gen.ezs')
               end if
            end do
            close (lunfil)
            Emean = sum(dataE) / nt
            call xbeach_dispersion(hs)

         elseif (trim(instat) == 'ts_2') then
            open (newunit=lunfil, file='bc/gen.ezs')
6           continue
            read (lunfil, '(a)', iostat=ier) bline
            if (ier /= 0) then
               call report_file_read_error('bc/gen.ezs')
            end if
            if (bline == '*') goto 6
            read (lunfil, *, iostat=ier) nt
            if (ier /= 0) then
               call report_file_read_error('bc/gen.ezs')
            end if

            allocate (dataE(nt))
            allocate (databi(nt))
            allocate (tE(nt))
            do it = 1, nt
               read (lunfil, *, iostat=ier) tE(it), databi(it), dataE(it)
               if (ier /= 0) then
                  call report_file_read_error('bc/gen.ezs')
               end if
            end do
            close (lunfil)
            Emean = sum(dataE) / nt
            call xbeach_dispersion(hs)

         elseif (trim(instat) == 'stat_table') then
            open (newunit=lunfil, file=bcfile)
            read (lunfil, *, iostat=ier) Hm0, Trep, dir0, dum1, spreadpar, bcendtime, dum2
            if (ier /= 0) then
               call report_file_read_error(bcfile)
            end if
            Hrms = Hm0 / sqrt(2.d0)
            m = 2.0d0 * spreadpar
            theta0 = (1.5d0 * pi) - dir0 * atan(1.d0) / 45.d0
            if (theta0 > pi) theta0 = theta0 - 2d0 * pi
            if (theta0 < -pi) theta0 = theta0 + 2d0 * pi
            newstatbc = 1

            do itheta = 1, ntheta
               sigt(itheta, :) = 2.d0 * pi / Trep
            end do
            sigmwav = max(sum(sigt, 1) / dble(ntheta), waveps)
            call xbeach_dispersion(hs)
         end if
         !
         ! Directional distribution
         !
         dist = (cos(thetabin - theta0))**m
         do itheta = 1, ntheta
            if (cos(thetabin(itheta) - theta0) < 0.d0) then
               dist(itheta) = 0.0d0
            end if
         end do
         if (trim(instat) == 'ts_1' .or. trim(instat) == 'ts_2') then
            Hrms = sqrt(8d0 * Emean / (rhomean * ag))
         end if
         E0 = 0.125d0 * ag * rhomean * Hrms**2

         ! energy density distribution
         if (sum(dist) > 0.d0) then
            factor = (dist / sum(dist)) / dtheta
         else
            factor = 0.d0
         end if
         e01 = factor * E0; ! 1:ntheta ding
         e01 = max(e01, 0.0d0); 
         if (jampi == 0) then
            if (nbndw > 0) then
               Llong = Tlong * maxval(cgwav(kbndw(1, 1:nbndw))) !! cg at some boundary point, xbeach_dispersion(). This implies that this value is the same everywhere!!
            else
               Llong = -huge(0d0) !! Llong only for bichrom waves
            end if
         else
            if (nbndw > 0) then ! may give different results for parallel runs
               Llong = Tlong * maxval(cgwav(kbndw(1, 1:nbndw)))
            else
               Llong = -huge(0d0)
            end if
            call reduce_double_max(Llong)
         end if

         call writelog('sl', '', 'Boundary conditions complete, starting computation')
      end if

      if ((time0 - tstart_user) >= bcendtime) then ! Recalculate bcf-file
         if (trim(instat) == 'stat_table') then
            call writelog('ls', '', 'Reading new wave conditions')
            read (lunfil, *, iostat=ier) Hm0, Trep, dir0, dum1, spreadpar, bcdur, dum2
            if (ier /= 0) then
               call report_file_read_error(bcfile)
            end if
            Hrms = Hm0 / sqrt(2.d0)
            taper = 0.d0
            m = 2.0d0 * spreadpar
            bcendtime = bcendtime + bcdur
            theta0 = (1.5d0 * pi) - dir0 * atan(1.d0) / 45.d0

            if (theta0 > 2d0 * pi) theta0 = theta0 - 2d0 * pi
            if (theta0 < -2d0 * pi) theta0 = theta0 + 2d0 * pi
            newstatbc = 1

            !if (windmodel==0) then
            do itheta = 1, ntheta
               sigt(itheta, :) = 2d0 * pi / Trep
            end do
            !endif

            do n = 1, nbndw
               kb = kbndw(1, n)
               do itheta = 1, ntheta
                  sigt(itheta, kb) = 2.d0 * pi / Trep
               end do
            end do

            sigmwav = max(sum(sigt, 1) / dble(ntheta), epshu)
            call xbeach_dispersion(hs)

            dist = (cos(thetabin - theta0))**m
            do itheta = 1, ntheta
               if (abs(thetabin(itheta) - theta0) > pi / 2.d0) then
                  dist(itheta) = 0d0
               end if
            end do
            E0 = 0.125d0 * ag * rhomean * Hrms**2

            ! energy density distribution

            if (sum(dist) > 0.d0) then
               factor = (dist / sum(dist)) / dtheta
            else
               factor = 0.d0
            end if
            e01 = factor * E0; 
            e01 = max(e01, 0.0d0); 
         elseif (trim(instat) == 'reuse') then
            close (71) ! to do, newlun
            close (72)
            startbcf = .true.
            if (time0 <= (tstop_user - time0)) then
               curline = curline + 1
            end if
         end if
         !
      end if
      !
      !!> Calculate boundary wave energy bc
      if (trim(instat) == 'stat' .or. trim(instat) == 'stat_table') then
         if (newstatbc == 1) then
            do itheta = 1, ntheta
               ee1(itheta, :) = e01(itheta)
            end do
         end if
         do n = 1, nbndw
            kb = kbndw(1, n)
            !if (windmodel.eq.1) then
            !   zbndw(:,n)=max(e01,Eini)
            !else
            zbndw(:, n) = e01
            !endif
            bi(n) = 0.0d0
         end do

         if (nbndu > 0) then
            do n = 1, nbndw
               if (kbndw2kbndu(n) /= 0) then
                  uin(kbndw2kbndu(n)) = 0d0
                  vin(kbndw2kbndu(n)) = 0d0
               end if
            end do
         end if

         ! to check: MPI compliancy - okay for xref0, yref0
      elseif (trim(instat) == 'bichrom') then
         theta0 = (1.5d0 * pi) - dir0 * atan(1.d0) / 45.d0
         do n = 1, nbndw
            kb = kbndw(1, n)
            ki = kbndw(2, n)
            zbndw(:, n) = e01 * 0.5d0 * &
                          (1.d0 + cos(2 * pi * (time0 / Tlong - (sin(theta0) * (ybndw(n) - yref0) &
                                                                 + cos(theta0) * (xbndw(n) - xref0)) / Llong))) * &
                          min(time0 / taper, 1.d0)
            if (nbndu > 0) then
               Lb = kbndw2kbndu(n)
               bl1 = bl(kb); bl2 = bl(ki); 
               ht = max(zbndu(Lb) - 0.5d0 * (bl1 + bl2), epshu) ! mean depth, not instantaneous
               em = (sum(0.5d0 * e01)) * dtheta * min(time0 / taper, 1.d0)
               ei = sum(zbndw(:, n), dim=1) * dtheta
               bi(n) = -(2d0 * cgwav(kb) / cwav(kb) - 0.5d0) * (em - ei) / (cgwav(kb)**2 - ag * ht) / rhomean
               uin(kbndw2kbndu(n)) = cgwav(kb) * bi(n) / ht * cos(theta0)
               vin(kbndw2kbndu(n)) = cgwav(kb) * bi(n) / ht * sin(theta0)
            end if
         end do

      elseif (trim(instat) == 'ts_1') then
         theta0 = (1.5d0 * pi) - dir0 * atan(1.d0) / 45.d0
         do n = 1, nbndw
            kb = kbndw(1, n)
            ki = kbndw(2, n)
            !
            if (abs(theta0) < 1e-3) then
               call linear_interp(tE, dataE, nt, time0, E1, E_idx)
            else
               if (jampi == 0) then
                  cgwavin = maxval(cgwav(kbndw(1, 1:nbndw)))
               else
                  if (nbndw > 0) then ! to check for different results for parallel runs
                     cgwavin = maxval(cgwav(kbndw(1, 1:nbndw)))
                  else
                     cgwavin = -huge(0d0)
                  end if
                  call reduce_double_max(cgwavin)
               end if
               !
               tshifted = max(time0 - (ybndw(n) - ybndw(1)) * sin(theta0) / cgwav(kbndw(1, 1)) &
                              - (xbndw(n) - xbndw(1)) * cos(theta0) / cgwavin, 0.d0)
               call linear_interp(tE, dataE, nt, tshifted, E1, E_idx)
            end if

            !if (windmodel .eq. 1) then
            !   zbndw(:,n)=max(e01*E1/max(Emean,0.000001d0)*min(time0/taper,1.d0),Eini)
            !else
            zbndw(:, n) = e01 * E1 / max(Emean, 0.000001d0) * min(time0 / taper, 1.d0)
            !endif

            if (nbndu > 0) then
               Lb = kbndw2kbndu(n)
               bl1 = bl(kb); bl2 = bl(ki); 
               ht = max(zbndu(Lb) - 0.5d0 * (bl1 + bl2), epshu) ! mean depth, not instantaneous
               em = Emean * min(time0 / taper, 1.d0)
               ei = sum(zbndw(:, n), dim=1) * dtheta
               bi(n) = -(2.0 * cgwav(kb) / cwav(kb) - 0.5d0) * (em - ei) / (cgwav(kb)**2 - ag * ht) / rhomean
               uin(kbndw2kbndu(n)) = cgwav(kb) * bi(n) / ht * cos(theta0)
               vin(kbndw2kbndu(n)) = cgwav(kb) * bi(n) / ht * sin(theta0)
            end if
         end do

      elseif (trim(instat) == 'ts_2') then

         theta0 = (1.5d0 * pi) - dir0 * atan(1.d0) / 45.d0
         do n = 1, nbndw
            kb = kbndw(1, n)
            ki = kbndw(2, n)

            if (abs(theta0) < 1e-3) then ! perpendicularly incoming
               call linear_interp(tE, dataE, nt, time0, E1, E_idx) ! basically only flumes with waves from left
               call linear_interp(tE, databi, nt, time0, bi(n), E_idx)
            else

               if (jampi == 0) then
                  cgwavin = maxval(cgwav(kbndw(1, 1:nbndw)))
               else
                  if (nbndw > 0) then ! to check for different results for parallel runs
                     cgwavin = maxval(cgwav(kbndw(1, 1:nbndw)))
                  else
                     cgwavin = -huge(0d0)
                  end if
                  call reduce_double_max(cgwavin)
               end if

               tshifted = max(time0 - (ybndw(n) - ybndw(1)) * sin(theta0) / cgwav(kbndw(1, 1)) &
                              - (xbndw(n) - xbndw(1)) * cos(theta0) / cgwavin, 0.d0)
               call linear_interp(tE, dataE, nt, tshifted, E1, E_idx)
               call linear_interp(tE, databi, nt, tshifted, bi(n), E_idx)
            end if

            zbndw(:, n) = e01 * E1 / max(Emean, 0.000001d0) * min(time0 / taper, 1.d0)
            if (nbndu > 0) then
               Lb = kbndw2kbndu(n)
               bl1 = bl(kb); bl2 = bl(ki); 
               ht = max(zbndu(Lb) - 0.5d0 * (bl1 + bl2), epshu) ! mean depth, not instantaneous
               if (freewave == 1) then
                  uin(kbndw2kbndu(n)) = sqrt(ag / ht) * bi(n)
                  vin(kbndw2kbndu(n)) = 0d0 ! for completeness
               else
                  uin(kbndw2kbndu(n)) = cgwav(kb) * bi(n) / ht * cos(theta0) * min(time0 / taper, 1.d0)
                  vin(kbndw2kbndu(n)) = cgwav(kb) * bi(n) / ht * sin(theta0) * min(time0 / taper, 1.d0)
               end if

            end if

         end do
         !
      end if
   end if

   ! safety on processes included
   if (allocated(uin)) uin = lwave * (order - 1) * uin
   if (allocated(vin)) vin = lwave * (order - 1) * vin
   if (allocated(zbndw)) zbndw = swave * zbndw

   ierror = 0

1234 continue
   return
end subroutine xbeach_wave_bc

!> apply computed boundary conditions
subroutine xbeach_apply_wave_bc()
   use m_sferic
   use m_flowgeom
   use fm_external_forcings_data
   use m_xbeach_data
   use m_physcoef

   implicit none

   integer :: k, itheta, kb, ki, L

   ! initially: all boundaries have Neumann boundary conditions
   do L = Lnxi + 1, Lnx
      kb = ln(1, L)
      ki = ln(2, L)
      do itheta = 1, ntheta
         ee1(itheta, kb) = ee1(itheta, ki)
         rr(itheta, kb) = rr(itheta, ki)

         !if (windmodel.eq.1) then
         !    tt1(itheta,kb) = tt1(itheta,ki)
         !endif

      end do
   end do
   !
   do k = 1, nbndw
      ! overwrite with stored boundary conditions
      kb = kbndw(1, k)
      do itheta = 1, ntheta
         ee1(itheta, kb) = zbndw(itheta, k)

         !if (windmodel.eq.1) then
         !   tt1(itheta,kb) = Trep
         !endif

      end do
   end do
   !
   !if (windmodel.eq.1) then
   !    sigt  = twopi/tt1
   !endif

end subroutine xbeach_apply_wave_bc

subroutine xbeach_wave_breaker_dissipation(dtmaxwav, break, deltaH, waveps, hhw, kwav, km, gamma, gamma2, nroelvink, &
                                           & QB, alpha, Trep, cwav, thetamean, hwav, D, sigmwav, wci, windmodel)
   use m_flow
   use m_flowgeom
   use m_sferic, only: pi
   use m_physcoef, only: rhomean
   use math_tools, only: xerf
   use m_xbeach_typesandkinds, only: slen
   use m_debug

   implicit none

   double precision, intent(in) :: dtmaxwav
   character(len=slen), intent(inout) :: break
   double precision, intent(inout) :: deltaH
   double precision, intent(inout) :: waveps
   double precision, dimension(Ndx), intent(in) :: hhw
   double precision, dimension(Ndx), intent(in) :: kwav
   double precision, dimension(Ndx), intent(in) :: km
   double precision, intent(in) :: gamma
   double precision, intent(in) :: gamma2
   double precision, intent(in) :: nroelvink
   double precision, dimension(Ndx), intent(inout) :: QB
   double precision, intent(in) :: alpha
   double precision, intent(in) :: Trep
   double precision, dimension(Ndx), intent(in) :: cwav
   double precision, dimension(Ndx), intent(in) :: thetamean
   double precision, dimension(Ndx), intent(in) :: hwav
   double precision, dimension(Ndx), intent(out) :: D
   double precision, dimension(Ndx), intent(in) :: sigmwav
   integer, intent(in) :: wci
   integer, intent(in) :: windmodel

   integer :: ierr, k
   double precision, allocatable, save :: hh(:), hr(:), kmr(:), arg(:), kh(:), Hb(:), Qb_advec(:), ka(:), f(:), gam(:), H(:), R(:)

   call realloc(hh, ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(hr, ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(kmr, ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(arg, ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(kh, ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(Hb, ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(Qb_advec, ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(ka, ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(f, ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(gam, ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(H, ndx, stat=ierr, fill=0d0, keepExisting=.false.)
   call realloc(R, ndx, stat=ierr, fill=0d0, keepExisting=.false.)

   break = trim(break)

   if (break == 'roelvink1') then ! Dissipation according to Roelvink (1993)
      !H   = sqrt(8.d0*E/rhomean/ag)
      H = hwav
      hr = hhw
      kmr = min(max(kwav, 0.01d0), 100.d0)
      !
      if (wci /= 0 .or. windmodel == 1) then
         arg = -(H / (gamma * tanh(kmr * hr) / kmr))**nroelvink
      else
         arg = -(H / (gamma * hr))**nroelvink
      end if
      !
      Qb = min(1.d0 - exp(max(arg, -100.d0)), 1.d0)
      D = Qb * 2.d0 * alpha * rhomean * ag * H**2 / 8d0
      !
      if (wci /= 0 .or. windmodel == 1) then
         D = D * sigmwav / 2.d0 / pi; 
      else
         D = D / Trep
      end if

   elseif (break == 'baldock') then ! Dissipation according to Baldock et al. (1998), only in stationary mode
      if (wci /= 0) then
         f = sigmwav / 2.d0 / pi
         ka = km
      elseif (windmodel == 2) then
         f = sigmwav / 2.d0 / pi
         ka = kwav
      else
         f = 1.d0 / Trep
         ka = kwav
      end if

      kh = ka * hhw

      if (wci /= 0) then
         gam = 0.76d0 * kh + 0.29d0
      else
         gam = gamma
      end if

      !H   = sqrt(8.d0/rhomean/ag*E)
      H = hwav
      Hb = tanh(gam * kh / 0.88d0) * (0.88d0 / max(kwav, 1d-10))
      R = Hb / max(H, 0.00001d0)

      Qb = exp(-R**2)
      D = 0.25d0 * alpha * f * rhomean * ag * (Hb**2 + H**2) * Qb

   elseif (break == 'roelvink2') then
      !H   = sqrt(8.d0*E/rhomean/ag)
      H = hwav
      hr = hhw
      hh = max(hs, waveps)
      kmr = min(max(kwav, 0.01d0), 100.d0)
      !
      if (wci /= 0) then
         arg = -(H / (gamma * tanh(kmr * hr) / kmr))**nroelvink
      else
         arg = -(H / (gamma * hr))**nroelvink
      end if
      !
      Qb = min(1.d0 - exp(max(arg, -100.d0)), 1.d0)
      D = Qb * 2.d0 * alpha * rhomean * ag * H**2 / 8d0
      !
      if (wci /= 0 .or. windmodel == 1) then
         D = D * sigmwav / 2.d0 / pi * H / hh
      else
         D = D / Trep * H / hh
      end if
   elseif (trim(break) == 'roelvink_daly') then
      !H   = sqrt(8.d0*E/rhomean/ag)
      H = hwav
      call advec_upw_bulk(thetamean, Qb, cwav, Qb_advec) ! first order upwind, with mean direction
      do k = 1, ndxi
         Qb(k) = Qb(k) - dtmaxwav * Qb_advec(k) * bai(k)
      end do
      hr = hhw
      hh = max(hs, waveps)
      kmr = min(max(kwav, 0.01d0), 100.d0)
      where (H > gamma * hr) Qb = 1.d0
      where (H < gamma2 * hr) Qb = 0.d0
      Qb = max(Qb, 0.d0)
      D = Qb * 2.d0 * alpha * rhomean * ag * H**2 / 8d0
      !
      if (wci /= 0 .or. windmodel == 1) then
         D = D * sigmwav / 2.d0 / pi * H / hh
      else
         D = D / Trep * H / hh
      end if

   elseif (break == 'janssen') then ! Dissipation according to Janssen and Battjes (2007)
      !H   = sqrt(8.d0*E/rhomean/ag)
      H = hwav
      if (wci /= 0) then
         f = sigmwav / 2.d0 / pi
         ka = km
      elseif (windmodel == 1) then
         f = sigmwav / 2.d0 / pi
         ka = kwav
      else
         f = 1.d0 / Trep
         ka = kwav
      end if

      hh = max(hs, waveps)
      kh = ka * hhw
      Hb = tanh(gamma * kh / 0.88d0) * (0.88d0 / kwav)
      R = Hb / max(H, 0.00001d0)

      Qb = 1d0 + 4 / (3 * sqrt(pi)) * (R**3 + 1.5d0 * R) * exp(-R**2) - xerf(R)
      D = 3d0 * sqrt(pi) / 16d0 * alpha * f * rhomean * ag * (H**3) / hh * Qb ! alpha is B from the paper, same as Roelvink 1993
   end if

   !deallocate(hh, hr, kmr, arg, kh, Hb, Qb_advec, H, R, stat = ierr)

end subroutine xbeach_wave_breaker_dissipation

subroutine advec_horz(dtmaxwav, snx, csx, limtypw, quant, veloc, advec)
   use m_sferic
   use m_physcoef
   use m_flowgeom
   use m_flowparameters, only: eps10

   implicit none

   double precision, intent(in) :: dtmaxwav
   double precision, intent(in), dimension(ntheta, ndx) :: snx, csx
   integer, intent(in) :: limtypw
   double precision, intent(in), dimension(ndx) :: veloc
   double precision, intent(in), dimension(ntheta, ndx) :: quant
   double precision, intent(out), dimension(ntheta, ndx) :: advec
   double precision, external :: dslim
   double precision, external :: dlimiter_nonequi

   integer :: L, k, k1, k2, itheta, ku, kl2s, kl2, kl1, kd, is, ip
   double precision :: velocL, qds, qst, half, fluxvel1, waku, sl1, sl2, sl3
   double precision :: cf, ds2, ds1, ds, cwuL, csxL, snxL
   double precision :: cs, sn, wuL

   integer :: nwalls

   advec = 0d0
   do L = 1, lnx ! upwind (supq) + limited high order (dsq), loop over link
      k1 = ln(1, L); k2 = ln(2, L) ! linker en rechtercelnr geassocieerd aan de links
      velocL = acL(L) * veloc(k1) + (1d0 - acL(L)) * veloc(k2)

      do itheta = 1, ntheta
         csxL = acL(L) * csx(itheta, k1) + (1d0 - acL(L)) * csx(itheta, k2)
         snxL = acL(L) * snx(itheta, k1) + (1d0 - acL(L)) * snx(itheta, k2)
         cwuL = velocL * (csu(L) * csxL + snu(L) * snxL) ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
         ! inproduct cgx*csu+cgy*snu

         if (cwuL > 0) then !   ->      ds1   ds2
            k = k1; kd = k2; is = 1; half = 1d0 - acl(L); ip = 0 !   ->   ku     k     kd
         else !   <-      ds2   ds1
            k = k2; kd = k1; is = -1; half = acl(L); ip = 3 !   <-   kd     k     ku
         end if ! acL = linkse dx fractie van afstand tussen flownodes (slide 83)

         fluxvel1 = is * cwuL * wu(L) ! snelheidsbijdrage linkse cel
         qst = fluxvel1 * quant(itheta, k) ! cg*E voor link L, sector itheta
         advec(itheta, kd) = advec(itheta, kd) - qst ! downwind cel krijgt bijdrage
         advec(itheta, k) = advec(itheta, k) + qst ! centrale cel verliest bijdrage

         if (limtypw > 0) then ! hogere orde, tijdstapafhankelijk!
            ku = klnup(1 + ip, L) ! pointer upwind cel horende bij link L

            if (ku /= 0) then
               kl2s = klnup(2 + ip, L); kl2 = abs(kl2s) !

               if (ku < 0) then
                  waku = quant(itheta, abs(ku)) ! pointer naar cel negatief?
               else
                  kl1 = ku
                  sl1 = slnup(1 + ip, L); sl2 = slnup(2 + ip, L) ! link upwind cell weight
                  waku = quant(itheta, kl1) * sl1 + quant(itheta, kl2) * sl2 ! gewogen gemiddelde upwind waarden
               end if

               sl3 = slnup(3 + ip, L)
               cf = dtmaxwav * abs(cwuL) * dxi(L)
               cf = half * max(0d0, 1d0 - cf)
               ds2 = quant(itheta, kd) - quant(itheta, k) ! ds1 = voorlopende slope, ds2 = eigen slope
               ds1 = (quant(itheta, k) - waku) * sl3

               if (abs(ds2) > eps10 .and. abs(ds1) > eps10) then
                  ds = cf * dslim(ds1, ds2, limtypw) ! reconstructie van totale slope volgens 1 van de 4 schema's                                            ! centraal schema
                  !
                  if (limtypw == 99) then
                     ds = cf * dlimiter_nonequi(ds1, ds2, half, 1d0) * ds2
                  end if
                  !
                  if (abs(ds) > eps10) then ! als celgemiddelde niet volstaat
                     qds = ds * fluxvel1 ! slope * linkse celbijdrage
                     advec(itheta, kd) = advec(itheta, kd) - qds ! downwind cel krijgt bijdrage
                     advec(itheta, k) = advec(itheta, k) + qds ! cel verliest bijdrage
                  end if
               end if
            end if
         end if
      end do ! directions
   end do ! links

!  account for outflow at closed boundaries
   do nwalls = 1, mxwalls
      k1 = walls(1, nwalls)

      cs = walls(8, nwalls) ! outward positive
      sn = -walls(7, nwalls)
      wuL = walls(9, nwalls)

      do itheta = 1, ntheta
         cwuL = veloc(k1) * (cs * csx(itheta, k1) + sn * snx(itheta, k1)) ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
         fluxvel1 = cwuL * wuL

         if (fluxvel1 > 0) then
            advec(itheta, k1) = advec(itheta, k1) + fluxvel1 * quant(itheta, k1)
         end if
      end do
   end do

! account for thin dams
   do nwalls = 1, nthd
      k1 = thindam(1, nwalls)

      cs = thindam(5, nwalls)
      sn = -thindam(4, nwalls)
      wuL = thindam(6, nwalls)

      do itheta = 1, ntheta
         cwuL = veloc(k1) * (cs * csx(itheta, k1) + sn * snx(itheta, k1)) ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
         fluxvel1 = cwuL * wuL

         if (fluxvel1 > 0) then
            advec(itheta, k1) = advec(itheta, k1) + fluxvel1 * quant(itheta, k1)
         end if
      end do
   end do

end subroutine advec_horz

subroutine advec_upw_bulk(thetamean, quant, veloc, advec)
   use m_sferic
   use m_physcoef
   use m_flowgeom
   use m_flow

   implicit none

   integer :: L, k, k1, k2, kd, is, ip, nwalls
   double precision :: velocL, qst, half, fluxvel, cs, sn, wul
   double precision :: cwuL, fluxvel1
   double precision, intent(in), dimension(ndx) :: thetamean
   double precision, intent(in), dimension(ndx) :: veloc
   double precision, intent(in), dimension(ndx) :: quant
   double precision, intent(out), dimension(ndx) :: advec

   advec = 0d0
   do L = 1, lnx ! upwind (supq) + limited high order (dsq), loop over link
      k1 = ln(1, L); k2 = ln(2, L) ! linker en rechtercelnr geassocieerd aan de links
      velocL = acL(L) * veloc(k1) + (1d0 - acL(L)) * veloc(k2)

      cwuL = velocL * (csu(L) * cos(thetamean(k1)) + snu(L) * sin(thetamean(k1))) ! met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )

      if (cwuL > 0) then !   ->      ds1   ds2
         k = k1; kd = k2; is = 1; half = 1d0 - acl(L); ip = 0 !   ->   ku     k     kd
      else !   <-      ds2   ds1
         k = k2; kd = k1; is = -1; half = acl(L); ip = 3 !   <-   kd     k     ku
      end if ! acL = linkse dx fractie van afstand tussen flownodes (slide 83)

      fluxvel = is * cwuL * wu(L)
      qst = fluxvel * quant(k)
      advec(kd) = advec(kd) - qst
      advec(k) = advec(k) + qst
   end do

   do nwalls = 1, mxwalls
      k1 = walls(1, nwalls)
      cs = walls(8, nwalls) ! outward positive
      sn = -walls(7, nwalls)
      wuL = walls(9, nwalls)

      cwuL = veloc(k1) * (cs * cos(thetamean(k1)) + sn * sin(thetamean(k1))) ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
      fluxvel1 = cwuL * wuL

      if (fluxvel1 > 0) then
         advec(k1) = advec(k1) + fluxvel1 * quant(k1)
      end if
   end do

   ! account for thin dams
   do nwalls = 1, nthd
      k1 = thindam(1, nwalls)

      cs = thindam(5, nwalls)
      sn = -thindam(4, nwalls)
      wuL = thindam(6, nwalls)

      cwuL = veloc(k1) * (cs * cos(thetamean(k1)) + sn * sin(thetamean(k1))) ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
      fluxvel1 = cwuL * wuL

      if (fluxvel1 > 0) then
         advec(k1) = advec(k1) + fluxvel1 * quant(k1)
      end if
   end do

end subroutine advec_upw_bulk

subroutine advec_dir(quan, veloc, advec)
   use m_sferic
   use m_physcoef
   use m_flowgeom
   use m_flow
   use m_xbeach_data

   implicit none

   integer :: k, itheta
   double precision :: ctheta_between, eeup
   double precision, dimension(ntheta) :: fluxtheta
   double precision, dimension(ntheta, ndx), intent(in) :: veloc, quan
   double precision, dimension(ntheta, ndx), intent(out) :: advec

   advec = 0d0
   if (ntheta > 1) then
      do k = 1, ndx
         do itheta = 2, ntheta - 2
            ctheta_between = 0.5d0 * (veloc(itheta, k) + veloc(itheta + 1, k))
            if (ctheta_between > 0) then
               eeup = 1.5d0 * quan(itheta, k) - .5 * quan(itheta - 1, k)
               if (eeup < 0.d0) then
                  eeup = quan(itheta, k)
               end if
               fluxtheta(itheta) = eeup * ctheta_between
            else
               eeup = 1.5d0 * quan(itheta + 1, k) - .5 * quan(itheta + 2, k)
               if (eeup < 0.d0) then
                  eeup = quan(itheta + 1, k)
               end if
               fluxtheta(itheta) = eeup * ctheta_between
            end if
         end do

         itheta = 1 ! only compute for itheta==1
         ctheta_between = .5 * (veloc(itheta + 1, k) + veloc(itheta, k))
         if (ctheta_between > 0) then
            fluxtheta(itheta) = quan(itheta, k) * ctheta_between
         else
            eeup = 1.5d0 * quan(itheta + 1, k) - .5 * quan(itheta + 2, k)
            if (eeup < 0.d0) eeup = quan(itheta + 1, k)
            fluxtheta(itheta) = eeup * ctheta_between
         end if

         itheta = ntheta - 1 ! only compute for itheta==ntheta-1
         ctheta_between = .5 * (veloc(itheta + 1, k) + veloc(itheta, k))
         if (ctheta_between > 0) then
            eeup = 1.5d0 * quan(itheta, k) - .5 * quan(itheta - 1, k)
            if (eeup < 0.d0) eeup = quan(itheta, k)
            fluxtheta(itheta) = eeup * ctheta_between
         else
            eeup = quan(itheta + 1, k)
            fluxtheta(itheta) = eeup * ctheta_between
         end if

         advec(1, k) = (fluxtheta(1) - 0.d0) / dtheta ! No flux across lower boundary theta grid
         do itheta = 2, ntheta - 1
            advec(itheta, k) = (fluxtheta(itheta) - fluxtheta(itheta - 1)) / dtheta
         end do
         advec(ntheta, k) = (0.d0 - fluxtheta(ntheta - 1)) / dtheta ! No flux across upper boundary theta grid
      end do
   end if

end subroutine advec_dir

subroutine advec_horzho_bulk(thetamean, quant, veloc, advec)
   ! advection with velocity u1 of wave turbulence, cfl with dt = dts
   use m_sferic
   use m_physcoef
   use m_flowgeom
   use m_flow
   use m_flowtimes

   implicit none

   integer :: L, k, k1, k2, ku, kl2s, kl2, kl1, kd, is, ip, limtypt, nwalls
   double precision :: qds, qst, half, fluxvel1, waku, sl1, sl2, sl3, wul
   double precision :: cf, ds2, ds1, ds, cwuL, cs, sn
   double precision, intent(in), dimension(lnx) :: veloc
   double precision, intent(in), dimension(ndx) :: quant
   double precision, intent(in), dimension(ndx) :: thetamean
   double precision, intent(out), dimension(ndx) :: advec
   double precision, external :: dslim

   advec = 0d0
   do L = 1, lnx ! upwind (supq) + limited high order (dsq), loop over link
      k1 = ln(1, L); k2 = ln(2, L) ! linker en rechtercelnr geassocieerd aan de links

      if (veloc(L) > 0) then !   ->      ds1   ds2
         k = k1; kd = k2; is = 1; half = 1d0 - acl(L); ip = 0 !   ->   ku     k     kd
      else !   <-      ds2   ds1
         k = k2; kd = k1; is = -1; half = acl(L); ip = 3 !   <-   kd     k     ku
      end if ! acL = linkse dx fractie van afstand tussen flownodes (slide 83)

      fluxvel1 = is * veloc(L) * wu(L) ! snelheidsbijdrage linkse cel
      qst = fluxvel1 * quant(k) ! cg*E voor link L, sector itheta
      advec(kd) = advec(kd) - qst ! downwind cel krijgt bijdrage
      advec(k) = advec(k) + qst ! centrale cel verliest bijdrage
      limtypt = 4 ! Mon Central
      if (limtypt > 0) then ! hogere orde, tijdstapafhankelijk door cfl
         ku = klnup(1 + ip, L) ! pointer upwind cel horende bij link L

         if (ku /= 0) then
            kl2s = klnup(2 + ip, L); kl2 = abs(kl2s) !

            if (ku < 0) then
               waku = quant(abs(ku)) ! pointer naar cel negatief?
            else
               kl1 = ku
               sl1 = slnup(1 + ip, L); sl2 = slnup(2 + ip, L) ! link upwind cell weight
               waku = quant(kl1) * sl1 + quant(kl2) * sl2 ! gewogen gemiddelde upwind waarden
            end if

            sl3 = slnup(3 + ip, L)
            cf = dts * abs(veloc(L)) * dxi(L)
            cf = half * max(0d0, 1d0 - cf) ! cf  =  half* (1d0-cf)
            ds2 = quant(kd) - quant(k) ! ds1 = voorlopende slope, ds2 = eigen slope
            ds1 = (quant(k) - waku) * sl3

            if (abs(ds2) > eps10 .and. abs(ds1) > eps10) then
               ds = cf * dslim(ds1, ds2, limtypt) ! reconstructie van totale slope volgens 1 van de 4 schema's

               if (abs(ds) > eps10) then ! als celgemiddelde niet volstaat
                  qds = ds * fluxvel1 ! slope * linkse celbijdrage
                  advec(kd) = advec(kd) - qds ! downwind cel krijgt bijdrage
                  advec(k) = advec(k) + qds ! cel verliest bijdrage
               end if
            end if
         end if
      end if
   end do ! links

   !  account for outflow at closed boundaries
   do nwalls = 1, mxwalls
      k1 = walls(1, nwalls)
      cs = walls(8, nwalls)
      sn = -walls(7, nwalls)

      wuL = walls(9, nwalls)
      cwuL = veloc(k1) * (cs * cos(thetamean(k1)) + sn * sin(thetamean(k1))) ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
      fluxvel1 = cwuL * wuL

      if (fluxvel1 > 0) then
         advec(k1) = advec(k1) + fluxvel1 * quant(k1)
      end if
   end do

! account for thin dams
   do nwalls = 1, nthd
      k1 = thindam(1, nwalls)

      cs = thindam(5, nwalls)
      sn = -thindam(4, nwalls)
      wuL = thindam(6, nwalls)

      cwuL = veloc(k1) * (cs * cos(thetamean(k1)) + sn * sin(thetamean(k1))) ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
      fluxvel1 = cwuL * wuL

      if (fluxvel1 > 0) then
         advec(k1) = advec(k1) + fluxvel1 * quant(k1)
      end if
   end do

end subroutine advec_horzho_bulk

!> reset XBeach wave data
subroutine xbeach_reset()
   use m_xbeach_readkey ! for reset_paramfile
   implicit none

   call reset_paramfile()

end subroutine xbeach_reset

!> compute flow boundary conditions
subroutine xbeach_flow_bc()
   use fm_external_forcings_data, only: nbndu
   use m_partitioninfo, only: jampi
   implicit none

   integer :: ierror

   ierror = 1

   if (nbndu < 1 .and. jampi == 0) goto 1234

   call xbeach_absgen_bc()

   ierror = 0
1234 continue

   return
end subroutine xbeach_flow_bc

!> initialize wave spectra
subroutine xbeach_spectral_wave_init()

   use m_xbeach_filefunctions
   use wave_boundary_datastore
   use m_xbeach_data
   use fm_external_forcings_data
   use m_flowgeom
   use m_xbeach_errorhandling
   use m_polygon
   use m_missing
   use m_sferic, only: twopi, jsferic, jasfer3D
   use timespace_triangle
   use m_flowtimes, only: time0
   use m_partitioninfo
   use m_alloc
   use stdlib_sorting, only: sort_index
   use geometry_module, only: dbdistance

   implicit none

   character(len=256) :: dum

   integer :: fid, err, seed
   integer :: i, itheta, ii
   integer, dimension(nwbnd) :: idum
   character(slen) :: testline
   integer, dimension(:), allocatable :: iperm, kpl, kL, kR, kLspec
   double precision, dimension(:), allocatable :: drL, wL, wR, wLspec
   double precision, dimension(nwbnd) :: hboundary
   double precision :: xa, ya, xb, yb, xt, yt
   double precision :: disall, dis, xn, yn, rL, darc
   double precision :: dum1, dum2

   double precision, dimension(:), allocatable :: dist
   integer, dimension(:), allocatable :: ibndspec
   double precision, dimension(:), allocatable :: xx, yy

   integer :: ibnd, minp, ip, ja
   integer :: k, L, j, LL
   integer :: ierr
   integer :: LL1, LL2

   type(filenames), dimension(:), allocatable :: tempspecfiles

   ierr = 1

   if (.not. bccreated) then
      bccreated = .true.
      allocate (waveBoundaryParameters(nwbnd), stat=ierr)
      if (ierr == 0) allocate (randomseed(nwbnd), stat=ierr)
      if (ierr == 0) allocate (waveBoundaryAdministration(nwbnd), stat=ierr)
      if (ierr == 0) allocate (waveBoundaryTimeSeries(nwbnd), stat=ierr)
      if (ierr == 0) allocate (waveSpectrumAdministration(nwbnd), stat=ierr)
   end if

   if (random == 1) then
      do i = 1, nwbnd
         call init_seed(seed)
         randomseed(i) = seed
      end do
      !
      if (jampi == 1) then
         idum = randomseed
         call reduce_int_max(nwbnd, idum)
         randomseed = idum
      end if
   else
      randomseed = -999
   end if

   call get_hboundary(hboundary)

   call get_refpoint(xref0, yref0)

!  determine which boundary belongs to a spectrum
   allocate (dist(nspectrumloc))
   allocate (ibndspec(nspectrumloc))
   allocate (xx(nspectrumloc))
   allocate (yy(nspectrumloc))
   dist = 1d99
   ibndspec = 0
   xx = dmiss
   yy = dmiss

   open (newunit=fid, file=bcfile)
   ! check for LOCLIST
   read (fid, *) testline
   if (trim(testline) == 'LOCLIST') then
      !
      do i = 1, nspectrumloc
         !        read x, y once
         read (fid, *, IOSTAT=err) xt, yt, dum
         if (err /= 0) then
            ! something has gone wrong during the read of this file
            call writelog('lswe', 'a,i0,a,a)', 'Error reading line ', i + 1, ' of file ', bcfile)
            call writelog('lswe', '', 'Check file for format errors and ensure the number of  ', &
                          'lines is equal to nspectrumloc.')
            call xbeach_errorhandler()
         end if
         xx(i) = xt
         yy(i) = yt
      end do
      !
      do ibnd = 1, nwbnd
         !  read boundary polyline
         call oldfil(minp, fnamwbnd(ibnd))
         call delpol()
         call reapol(minp, 0)

         do i = 1, nspectrumloc
            !  determine distance to boundary polyline
            do ip = 1, NPL - 1
               xt = xx(i)
               yt = yy(i)
               xa = XPL(ip)
               ya = YPL(ip)
               xb = XPL(ip + 1)
               yb = YPL(ip + 1)
               if (xa /= dmiss .and. xb /= dmiss) then
                  call dlinedis3(xt, yt, xa, ya, xb, yb, ja, dis, xn, yn, rL)
                  if (abs(dis) < abs(dist(i))) then ! new closest boundary polygon found for this spectrum
                     dist(i) = abs(dis)
                     ibndspec(i) = ibnd
                  end if
               end if
            end do
         end do
      end do
   end if
   close (fid)

   do ibnd = 1, nwbnd
      LL1 = L1wbnd(ibnd)
      LL2 = L2wbnd(ibnd)
      if (LL1 > LL2) cycle

      waveBoundaryParameters(ibnd)%masterFileName = bcfile
      waveBoundaryParameters(ibnd)%np = LL2 - LL1 + 1
      waveBoundaryParameters(ibnd)%ntheta = ntheta
      waveBoundaryParameters(ibnd)%dtheta = dtheta
      waveBoundaryParameters(ibnd)%x0 = xref0
      waveBoundaryParameters(ibnd)%y0 = yref0
      waveBoundaryParameters(ibnd)%hboundary = hboundary(ibnd)

      if (allocated(waveBoundaryParameters(ibnd)%xb)) deallocate (waveBoundaryParameters(ibnd)%xb)
      if (allocated(waveBoundaryParameters(ibnd)%yb)) deallocate (waveBoundaryParameters(ibnd)%yb)
      if (allocated(waveBoundaryParameters(ibnd)%theta)) deallocate (waveBoundaryParameters(ibnd)%theta)
      if (allocated(waveBoundaryParameters(ibnd)%theta_s)) deallocate (waveBoundaryParameters(ibnd)%theta_s)

      ! Now allocate arrays to the correct size and set values
      allocate (waveBoundaryParameters(ibnd)%xb(waveBoundaryParameters(ibnd)%np))
      allocate (waveBoundaryParameters(ibnd)%yb(waveBoundaryParameters(ibnd)%np))
      allocate (waveBoundaryParameters(ibnd)%theta(waveBoundaryParameters(ibnd)%ntheta))
      if (single_dir > 0) then
         allocate (waveBoundaryParameters(ibnd)%theta_s(ntheta_s))
         waveBoundaryParameters(ibnd)%theta_s = thetabin_s
      end if
      if (nbndw > 0) then
         waveBoundaryParameters(ibnd)%xb = xbndw(LL1:LL2)
         waveBoundaryParameters(ibnd)%yb = ybndw(LL1:LL2)
      end if
      waveBoundaryParameters(ibnd)%theta = thetabin

      ! Ensure all theta directions are between 0 and 2pi, required for some trig. on some compilers
      do itheta = 1, ntheta
         waveBoundaryParameters(ibnd)%theta(itheta) = mod(waveBoundaryParameters(ibnd)%theta(itheta) + twopi, 8.d0 * atan(1.d0))
      end do

      ! Allocate space for the random seed. This seed should be identical on all processes
      waveBoundaryParameters(ibnd)%randomseed = randomseed(ibnd)

      if (.not. waveBoundaryAdministration(ibnd)%initialized) then

         call writelog('l', '', '--------------------------------')
         call writelog('l', '', 'Initializing spectral wave boundary conditions for boundary ', ibnd)
         ! Initialize that wave boundary conditions need to be calculated (first time at least)
         ! Stored and defined in spectral_wave_bc_module
         waveSpectrumAdministration(ibnd)%repeatwbc = .false.
         ! Initialize the number of times wave boundary conditions have been generated.
         ! Stored and defined in spectral_wave_bc_module
         waveSpectrumAdministration(ibnd)%bccount = 0
         ! Initialize bcendtime to zero.
         ! Stored and defined in spectral_wave_bc_module
         waveSpectrumAdministration(ibnd)%spectrumendtime = 0.d0
         ! Initialise lastwaveheight to zero
         ! Stored and defined in wave_boundary_main_module
         allocate (waveSpectrumAdministration(ibnd)%lastwaveelevation(waveBoundaryParameters(ibnd)%np, &
                                                                      waveBoundaryParameters(ibnd)%ntheta))

         if (nspectrumloc < 1) then
            call writelog('ewls', '', 'Number of boundary spectra (''nspectrumloc'') may not be less than 1 for spectral boundary conditions.')
            call xbeach_errorhandler()
         end if

         ! open location list file
         open (newunit=fid, file=bcfile)
         ! check for LOCLIST
         read (fid, *) testline
         if (trim(testline) == 'LOCLIST') then
            allocate (wavespectrumadministration(ibnd)%kL(LL2 - LL1 + 1))
            allocate (wavespectrumadministration(ibnd)%wL(LL2 - LL1 + 1))
            allocate (wavespectrumadministration(ibnd)%kR(LL2 - LL1 + 1))
            allocate (wavespectrumadministration(ibnd)%wR(LL2 - LL1 + 1))

            ii = 0
            do i = 1, nspectrumloc
               if (ibndspec(i) /= ibnd) then
                  read (fid, *, IOSTAT=err) dum1, dum2, dum ! line should be read anyway
                  cycle
               end if

               ii = ii + 1
               waveSpectrumAdministration(ibnd)%nspectra = ii
               call realloc(waveSpectrumAdministration(ibnd)%ispectra, ii, keepExisting=.true., fill=-999)
               call realloc(waveSpectrumAdministration(ibnd)%xspec, ii, keepExisting=.true., fill=-999d0)
               call realloc(waveSpectrumAdministration(ibnd)%yspec, ii, keepExisting=.true., fill=-999d0)
!              ugly as hell, but no realloc of derived types available
!              as number of locations usually small, this should not kill performance
               if (ii == 1) then
                  allocate (waveSpectrumAdministration(ibnd)%bcfiles(ii))
               else
                  allocate (tempspecfiles(ii - 1))
                  tempspecfiles = waveSpectrumAdministration(ibnd)%bcfiles
                  deallocate (waveSpectrumAdministration(ibnd)%bcfiles)
                  allocate (waveSpectrumAdministration(ibnd)%bcfiles(ii))
                  waveSpectrumAdministration(ibnd)%bcfiles(1:ii - 1) = tempspecfiles
                  deallocate (tempspecfiles)
               end if
               waveSpectrumAdministration(ibnd)%ispectra(ii) = i
!              x, y and file name per location
               read (fid, *, IOSTAT=err) wavespectrumadministration(ibnd)%xspec(ii), wavespectrumadministration(ibnd)%yspec(ii), wavespectrumadministration(ibnd)%bcfiles(ii)%fname
               wavespectrumadministration(ibnd)%bcfiles(ii)%listline = 0

               if (err /= 0) then
                  ! something has gone wrong during the read of this file
                  call writelog('lswe', 'a,i0,a,a)', 'Error reading line ', i + 1, ' of file ', bcfile)
                  call writelog('lswe', '', 'Check file for format errors and ensure the number of  ', &
                                'lines is equal to nspectrumloc')
                  call xbeach_errorhandler()
               end if

            end do

            !     sort spectra in increasing arclength along the wave-energy boundary
            allocate (drL(waveSpectrumAdministration(ibnd)%nspectra), iperm(waveSpectrumAdministration(ibnd)%nspectra), kpl(waveSpectrumAdministration(ibnd)%nspectra))
            allocate (kL(LL2 - LL1 + 1), kR(LL2 - LL1 + 1), wL(LL2 - LL1 + 1), wR(LL2 - LL1 + 1))
            allocate (kLspec(nspectrumloc), wLspec(nspectrumloc))

            call oldfil(minp, fnamwbnd(ibnd))
            call delpol()
            call reapol(minp, 0)

            do LL = LL1, LL2
               L = kbndw(3, LL)
               i = LL - LL1 + 1
               call polyindexweight(xu(L), yu(L), xy2bndw(1, LL), xy2bndw(2, LL), &
                                    xpl, ypl, (/(1, k=1, NPL)/), NPL, &
                                    kL(i), wL(i), kR(i), wR(i))
            end do

!           project spectrum locations on polyline
            drL = 1d99
            do i = 1, waveSpectrumAdministration(ibnd)%nspectra

!              find nearest point on polyline
               disall = 1d99
               darc = 0d0
               do ip = 1, NPL - 1
                  xa = XPL(ip)
                  ya = YPL(ip)
                  xb = XPL(ip + 1)
                  yb = YPL(ip + 1)
                  xt = wavespectrumadministration(ibnd)%xspec(i)
                  yt = wavespectrumadministration(ibnd)%yspec(i)
                  if (xa /= dmiss .and. xb /= dmiss) then
                     call dlinedis3(xt, yt, xa, ya, xb, yb, ja, dis, xn, yn, rL)
                     if (dis < disall) then
                        disall = dis
                        drL(i) = darc + dbdistance(xa, ya, xn, yn, jsferic, jasfer3D, dmiss)
                        !kLspec(i) = ip
                        !wLspec(i) = rL
                     end if
                  end if

                  darc = darc + dbdistance(xa, ya, xb, yb, jsferic, jasfer3D, dmiss)
               end do ! ip
            end do ! i

            ! sort drL, order is in iperm
            call sort_index(drL, iperm)

            !  compute weights from mesh to spectrum locations
            do i = 1, LL2 - LL1 + 1
               !  determine arc length along polyline
               darc = 0d0
               do ip = 1, kL(i) - 1
                  xa = XPL(ip)
                  ya = YPL(ip)
                  xb = XPL(ip + 1)
                  yb = YPL(ip + 1)
                  darc = darc + dbdistance(xa, ya, xb, yb, jsferic, jasfer3D, dmiss)
               end do
               ip = kL(i)
               xa = XPL(ip)
               ya = YPL(ip)
               xb = XPL(ip + 1)
               yb = YPL(ip + 1)
               darc = darc + wR(i) * dbdistance(xa, ya, xb, yb, jsferic, jasfer3D, dmiss)

               !  determine weights from spectrum locations to boundary links
               j = 1
               do while (drL(j) < darc .and. j < waveSpectrumAdministration(ibnd)%nspectra)
                  j = j + 1 ! j is right pointer
               end do
               if (j > 1) then
                  j = j - 1 ! j is left pointer
               end if

               wavespectrumadministration(ibnd)%kL(i) = wavespectrumadministration(ibnd)%ispectra(iperm(j))
               !wavespectrumadministration(ibnd)%kL(i) = iperm(j)
               wavespectrumadministration(ibnd)%kR(i) = wavespectrumadministration(ibnd)%ispectra(iperm(j))
               !wavespectrumadministration(ibnd)%kR(i) = iperm(j)
               wavespectrumadministration(ibnd)%wL(i) = 1d0
               wavespectrumadministration(ibnd)%wR(i) = 0d0
               if (j + 1 <= waveSpectrumAdministration(ibnd)%nspectra) then
                  wavespectrumadministration(ibnd)%kR(i) = wavespectrumadministration(ibnd)%ispectra(iperm(j + 1))
                  wavespectrumadministration(ibnd)%wL(i) = min(max(1d0 - (darc - drL(j)) / (drL(j + 1) - drL(j)), 0d0), 1d0)
                  wavespectrumadministration(ibnd)%wR(i) = 1d0 - wavespectrumadministration(ibnd)%wL(i)
               end if
            end do ! i

            deallocate (drL, iperm, kpl)
            deallocate (kL, kR, wL, wR)

         else ! no LOCLIST
            if (nspectrumloc == 1) then
               allocate (waveSpectrumAdministration(ibnd)%bcfiles(nspectrumloc))
               allocate (waveSpectrumAdministration(ibnd)%xspec(nspectrumloc))
               allocate (waveSpectrumAdministration(ibnd)%yspec(nspectrumloc))
               waveSpectrumAdministration(ibnd)%xspec = xref0
               waveSpectrumAdministration(ibnd)%yspec = yref0
               waveSpectrumAdministration(ibnd)%nspectra = nspectrumloc
               waveSpectrumAdministration(ibnd)%bcfiles(1)%fname = bcfile
               waveSpectrumAdministration(ibnd)%bcfiles(1)%listline = 0 ! for files that have multiple lines, set listline to 0
               allocate (wavespectrumadministration(ibnd)%kL(nbndw))
               allocate (wavespectrumadministration(ibnd)%wL(nbndw))
               allocate (wavespectrumadministration(ibnd)%kR(nbndw))
               allocate (wavespectrumadministration(ibnd)%wR(nbndw))
               wavespectrumadministration(ibnd)%kL = 1
               wavespectrumadministration(ibnd)%wL = 1d0
               wavespectrumadministration(ibnd)%kR = 1
               wavespectrumadministration(ibnd)%wR = 0d0
            else
               call writelog('ewls', '', 'If nspectrumloc>1 then bcfile should contain spectra locations with LOCLIST header')
               close (fid)
               call xbeach_errorhandler()
            end if
         end if

         waveBoundaryAdministration(ibnd)%initialized = .true.

         waveSpectrumAdministration(ibnd)%Hbc = 0d0
         waveSpectrumAdministration(ibnd)%Tbc = 0d0
         waveSpectrumAdministration(ibnd)%Dbc = 0d0

         close (fid)

      end if

      if (single_dir > 0) then
         call realloc(waveSpectrumAdministration(ibnd)%ee_s, (/ntheta_s, LL2 - LL1 + 1/), keepExisting=.false., fill=0d0)
      end if

      ! Set time to recompute new boundary condition time series to
      ! now so boundary conditions are generated in first time step
      waveBoundaryAdministration(ibnd)%startComputeNewSeries = time0

      call writelog('l', '', '--------------------------------')
      call delpol()
   end do

   ierr = 0
1234 continue
   return
end subroutine xbeach_spectral_wave_init

!> get reference point for wave energy bc
subroutine get_refpoint(xref0, yref0)
   use fm_external_forcings_data
   use m_partitioninfo
   implicit none

   double precision, intent(out) :: xref0, yref0

   xref0 = huge(0d0)
   yref0 = huge(0d0)
   if (nbndw > 0) then
      xref0 = minval(xbndw(1:nbndw))
      yref0 = minval(ybndw(1:nbndw))
   end if
   if (jampi == 1) then
      call reduce_double_min(xref0)
      call reduce_double_min(yref0)
   end if

   if (xref0 == huge(0d0)) then ! nbndw=0 for all subdomains, or in sequential run
      xref0 = 0d0
      yref0 = 0d0
   end if
end subroutine get_refpoint

!> determine average height along wave energy boundary
subroutine get_hboundary(hboundary)
   use m_flow
   use m_flowgeom
   use m_flowparameters
   use m_xbeach_data
   use m_partitioninfo
   implicit none

   double precision, dimension(nwbnd), intent(out) :: hboundary

   double precision, dimension(nwbnd) :: dlength

   double precision, dimension(2, nwbnd) :: dum

   integer :: i, k, k2
   integer :: LL1, LL2, n

   hboundary = 0d0
   dlength = 0d0
   if (jampi == 0) then
      do n = 1, nwbnd
         !     integrate along wave boundary
         LL1 = L1wbnd(n)
         LL2 = L2wbnd(n)

         do i = LL1, LL2
            hboundary(n) = hboundary(n) + max(s1(kbndw(1, i)) - bl(kbndw(1, i)), epshu) * wu(kbndw(3, i))
            dlength(n) = dlength(n) + wu(kbndw(3, i))
         end do

         !  compute average
         if (dlength(n) > 0d0) then
            hboundary(n) = hboundary(n) / dlength(n)
         else
            hboundary(n) = 0d0
         end if
      end do
   else
      !     integrate along wave boundary
      do n = 1, nwbnd
         LL1 = L1wbnd(n)
         LL2 = L2wbnd(n)
         if (LL1 > LL2) cycle
         do i = LL1, LL2
            k2 = kbndw(2, i)
            if (idomain(k2) == my_rank) then
               hboundary(n) = hboundary(n) + max(s1(kbndw(1, i)) - bl(kbndw(1, i)), epshu) * wu(kbndw(3, i))
               dlength(n) = dlength(n) + wu(kbndw(3, i))
            end if
         end do
      end do

      !     global reduction
      do n = 1, nwbnd
         dum(1, n) = hboundary(n)
         dum(2, n) = dlength(n)
      end do
      call reduce_sum(2 * nwbnd, dum)

      !     compute average
      k = 0
      do k = 1, nwbnd
         if (dum(2, k) > 0d0) then
            hboundary(k) = dum(1, k) / dum(2, k)
         else
            hboundary(k) = 0d0
         end if
      end do
   end if

   return
end subroutine get_hboundary

subroutine xbeach_waves(ierr)
   use m_flowtimes
   use m_xbeach_data, m_xbeach_data_hminlw => hminlw
   use m_xbeach_netcdf
   use m_flowparameters
   use m_flow, only: hs, kmx
   use m_flowgeom, only: ndx

   implicit none

   integer, intent(out) :: ierr

   integer :: k
   double precision :: gammal

   ierr = 1

   !> Prepare
   ! set basic water depth for all wave calculations, dependent on wci, single_dir
   if (deltaH > 0.d0) then
      hhw = max(hs + deltaH * H, epshu)
   else
      hhw = max(hs, epshu)
   end if
   !
   if (oldhmin == 1) then
      hstokes = max(hs, m_xbeach_data_hminlw)
   else
      do k = 1, ndx
         gammal = H(k) / hhw(k)
         if (gammal > 1.d0) then
            hstokes(k) = deltahmin * (gammal - 1.d0) * H(k) + hhw(k)
         else
            hstokes(k) = hhw(k)
         end if
      end do
   end if
   !
   ! to do: investigate influence of moving wave RHS to flow_ini_timestep
   ! rhs is calculated based on time0==time1, and dts==dtprev
   select case (trim(instat))
   case ('stat', 'stat_table')
      if ((abs(mod(time0, wavint)) < 0.001d0 * dts) .or. newstatbc == 1) then
         call xbeach_wave_dispersion(0)
         call xbeach_wave_stationary(0)
         newstatbc = 0
      end if
   case default
      if (single_dir > 0) then
         !
         call update_means_wave_flow()
         !
         ! do refraction
         if ((abs(mod(time0, wavint)) < 0.001d0 * dts) .or. newstatbc == 1 .or. time0 == dts) then
            call xbeach_wave_dispersion(1)
            call xbeach_wave_stationary(1)
            newstatbc = 0
         end if
         !
         newstatbc = 0
         !
         ! need to call dispersion again, because dispersion is different in timestep and direction computation
         if (wci > 0) then
            call xbeach_wave_dispersion(2)
         else
            call xbeach_wave_dispersion(0)
         end if
         call xbeach_wave_instationary() ! update wave groups along thetamean
      else
         newstatbc = 0
         if (wci > 0) then
            !
            call update_means_wave_flow()
            call xbeach_wave_dispersion(2)
         else
            call xbeach_wave_dispersion(0)
         end if
         call xbeach_wave_instationary()
      end if
   end select

   ! update stokes drift on every timestep
   if (kmx == 0) then ! otherwise postponed to update_verticalprofiles
      call xbeach_compute_stokesdrift()
   end if

   !if (jaavgwavquant .eq. 1) then
   !   call xbeach_makeaverages(dts)          ! time-averaged stats
   !end if

end subroutine xbeach_waves

!> compute bc for absorbing generating boundary
subroutine xbeach_absgen_bc()
   use m_sferic
   use m_xbeach_data
   use m_flowgeom
   use m_flow
   use m_physcoef, only: ag, rhomean
   use fm_external_forcings_data
   use m_alloc
   use unstruc_messages
   use m_xbeach_errorhandling
   use m_missing
   use m_partitioninfo
   use m_flowtimes, only: dts
   use m_waves, only: ustokes, vstokes
   use network_data
   use geometry_module

   implicit none

   integer :: ierror

   integer, parameter :: MAXLNX = 100

   integer :: numbnd
   integer :: idum(1)
   double precision, allocatable :: idum2(:, :)

   double precision :: uin_loc, vin_loc, hum, cgbound, cg0, c, umean, vmean, dum
   double precision :: factime
   double precision :: hsk
   double precision :: ht(2)

   double precision :: un, Fn, Fwin, Ftau, ur, betaki, vert, betak1, betak2
   double precision :: dhdn, dvds, dbetads, dbetadn, dbetadt, betanp1
   double precision :: alpha2, alphanew, thetai

   integer :: n, Lb, L, kb, ki, k1, k2, i, jj
   integer :: NLNX, nw

   ierror = 1

   !if (windmodel .eq. 0) then
   factime = 1d0 / cats / Trep * dts
   !else
   !   factime = 1d0/cats/minval(sigmwav)/2d0/pi*dts
   !endif

!  compute boundary-averaged velocities
   numbnd = 0
   uave = 0d0
   vave = 0d0
   dlengthrm = 0d0

   do n = 1, nbndu
      if (kbndu(4, n) == 5) then
         Lb = kbndu(3, n)
         numbnd = kbndu(5, n)
         if (numbnd > maxnumbnds) then
            maxnumbnds = max(int(1.2d0 * numbnd), maxnumbnds + 1)
            !
            if (jampi == 1) then
               idum(1) = maxnumbnds
               call reduce_int_max(1, idum)
               maxnumbnds = idum(1)
            end if
            !
            call realloc(uave, maxnumbnds, keepExisting=.true., fill=0d0)
            call realloc(vave, maxnumbnds, keepExisting=.true., fill=0d0)
            call realloc(dlengthrm, maxnumbnds, keepExisting=.true., fill=0d0)
            call realloc(umeanrm, maxnumbnds, keepExisting=.true., fill=0d0)
            call realloc(vmeanrm, maxnumbnds, keepExisting=.true., fill=0d0)
         end if
         !
         if (jampi == 0) then
            uave(numbnd) = uave(numbnd) + wu(Lb) * u1rm(n)
            vave(numbnd) = vave(numbnd) + wu(Lb) * v(Lb) ! for now 1d
            dlengthrm(numbnd) = dlengthrm(numbnd) + wu(Lb)
         else
            if (idomain(kbndu(2, n)) == my_rank) then
               uave(numbnd) = uave(numbnd) + wu(Lb) * u1rm(n)
               vave(numbnd) = vave(numbnd) + wu(Lb) * v(Lb)
               dlengthrm(numbnd) = dlengthrm(numbnd) + wu(Lb)
            end if
         end if
         !
      end if
   end do

   if (jampi == 1) then
      if (nubnd > 0) then
         allocate (idum2(3, nubnd))
         idum2(1, :) = uave
         idum2(2, :) = vave
         idum2(3, :) = dlengthrm
         call reduce_sum(3 * nubnd, idum2)

         uave = idum2(1, :)
         vave = idum2(2, :)
         dlengthrm = idum2(3, :)
      end if
   end if

   if (nubnd > 0) then
      do n = 1, nubnd
         uave(n) = uave(n) / max(dlengthrm(n), 1d-16)
         vave(n) = vave(n) / max(dlengthrm(n), 1d-16)
         umeanrm(n) = factime * uave(n) + (1d0 - factime) * umeanrm(n)
         vmeanrm(n) = factime * vave(n) + (1d0 - factime) * vmeanrm(n)
      end do
   end if

   do n = 1, nbndu
      if (kbndu(4, n) == 5) then ! absgen boundary
         kb = kbndu(1, n)
         ki = kbndu(2, n)
         Lb = kbndu(3, n)
         numbnd = kbndu(5, n)

         NLNX = nd(ki)%lnx

         nw = kbndu2kbndw(n) ! wave-boundary index

         if (nw > 0) then
            uin_loc = uin(n) * csu(Lb) + vin(n) * snu(Lb)
            vin_loc = vin(n) * csu(Lb) - uin(n) * snu(Lb)
         else
            uin_loc = 0d0
            vin_loc = 0d0
         end if

         !  check array size
         if (NLNX > MAXLNX) then
            call mess(LEVEL_ERROR, 'xbeach_absgen_bc: array size error')
            call xbeach_errorhandler()
         end if

         if (trim(absgentype) == 'abs_1d') then
            ! zbndu for absgen bc is slowly varying tide+surge water level
            hsk = s1(ki) - bl(ki)
            u1(Lb) = (1d0 + sqrt(ag * hsk) / cgwav(ki)) * uin_loc - sqrt(ag / hsk) * (s1(ki) - zbndu(n)) + umeanrm(numbnd)
            s0(kb) = s0(ki)
            s1(kb) = s1(ki)

            u1rm(n) = u1(Lb)
         end if

         if (trim(absgentype) == 'abs_2d') then
            !
            ht(1) = zbndu(n) - bl(kb)
            ht(2) = zbndu(n) - bl(ki)
            hum = max(epshu, 0.5d0 * (ht(1) + ht(2)))
            !
            umean = umeanrm(numbnd)
            vmean = vmeanrm(numbnd)
            !
            if (ARC == 0) then
               u1(Lb) = (order - 1d0) * uin_loc + umean
               s1(kb) = s1(ki)
               u1rm(n) = u1(Lb)
               cycle ! all done
            end if
            !
            if (hu(Lb) <= epshu) then
               s1(kb) = max(zbndu(n), bl(kb))
               u1(Lb) = ucx(ki) * csu(Lb) + ucy(ki) * snu(Lb)
               cycle
            end if
            !
            xbducxdx = 0d0
            xbducxdy = 0d0
            xbducydx = 0d0
            xbducydy = 0d0
            dbetadx = 0d0
            dbetady = 0d0
            !
            do i = 1, NLNX
               L = abs(nd(ki)%ln(i))
               !
               k1 = ln(1, L)
               k2 = ln(2, L)
               !
               ! Pragmatic way
               betak1 = ucx(k1) * csu(Lb) + ucy(k1) * snu(Lb) - 2d0 * sqrt(ag * (s1(k1) - bl(k1)))
               betak2 = ucx(k2) * csu(Lb) + ucy(k2) * snu(Lb) - 2d0 * sqrt(ag * (s1(k2) - bl(k2)))
               !
               dbetadx(k1) = dbetadx(k1) + wcx1(L) * (betak2 - betak1) * dxi(L)
               dbetadx(k2) = dbetadx(k2) + wcx2(L) * (betak2 - betak1) * dxi(L)
               dbetady(k1) = dbetady(k1) + wcy1(L) * (betak2 - betak1) * dxi(L)
               dbetady(k2) = dbetady(k2) + wcy2(L) * (betak2 - betak1) * dxi(L)
               !
               xbducxdx(k1) = xbducxdx(k1) + wcx1(L) * (ucx(k2) - ucx(k1)) * dxi(L)
               xbducxdx(k2) = xbducxdx(k2) + wcx2(L) * (ucx(k2) - ucx(k1)) * dxi(L)
               xbducxdy(k1) = xbducxdy(k1) + wcy1(L) * (ucx(k2) - ucx(k1)) * dxi(L)
               xbducxdy(k2) = xbducxdy(k2) + wcy2(L) * (ucx(k2) - ucx(k1)) * dxi(L)
               !
               xbducydx(k1) = xbducydx(k1) + wcx1(L) * (ucy(k2) - ucy(k1)) * dxi(L)
               xbducydx(k2) = xbducydx(k2) + wcx2(L) * (ucy(k2) - ucy(k1)) * dxi(L)
               xbducydy(k1) = xbducydy(k1) + wcy1(L) * (ucy(k2) - ucy(k1)) * dxi(L)
               xbducydy(k2) = xbducydy(k2) + wcy2(L) * (ucy(k2) - ucy(k1)) * dxi(L)
            end do

            dbetadn = csu(Lb) * dbetadx(ki) + snu(Lb) * dbetady(ki) ! 1-2 direction
            dbetads = -snu(Lb) * dbetadx(ki) + csu(Lb) * dbetady(ki) ! 3-4 direction
            !
            dvds = -snu(Lb) * (-snu(Lb) * xbducxdx(ki) + csu(Lb) * xbducxdy(ki)) + &
                   csu(Lb) * (-snu(Lb) * xbducydx(ki) + csu(Lb) * xbducydy(ki))
            !
            dhdn = (ht(2) - ht(1)) * dxi(Lb) ! 1-2 direction (inward positive)
            !
            Fn = csu(Lb) * Fx(Lb) + snu(Lb) * Fy(Lb) ! 1-2 direction (inward positive)
            !  compute bed friction
            Ftau = cfuhi(Lb) * sqrt((u1(Lb) - ustokes(Lb))**2 + (v(Lb) - vstokes(Lb))**2) * (u1(Lb) - ustokes(Lb)) ! formally, not exactly correct, since we also need u1L (see furu). JRE: but we can use u1rm old timelevel
            !
            if (jawind > 0) then
               Fwin = wdsu(Lb) * huvli(Lb)
            else
               Fwin = 0d0
            end if
            c = sqrt(ag * hu(Lb))
            !
            !
            dbetadt = -(u1(Lb) - c) * dbetadn - v(Lb) * dbetads + c * dvds + ag * dhdn + Fn / (rhomean * hu(Lb)) - Ftau + Fwin
            beta = u1(Lb) - 2d0 * sqrt(ag * hu(Lb))
            !
            thetai = atan2(vin_loc, uin_loc) ! cartesian angle wrt X
            !
            betanp1 = beta + dbetadt * dts
            alpha2 = (270d0 - dir0) * dg2rd ! first guess, theta0 not set for spectral bc as dir0 not defined
            alphanew = 0.d0
            !
            cg0 = sqrt(ag * hum)
            !
            do jj = 1, 50
               !
               if (freewave == 1) then ! assuming incoming long wave propagates at sqrt(g*h) (free wave)
                  ur = cos(alpha2) / (cos(alpha2) + 1.d0) &
                       * (betanp1 - umean + 2.d0 * cg0 &
                          - uin_loc * (cos(thetai) - 1.d0) / cos(thetai))
               else ! assuming incoming long wave propagates at group velocity (bound wave)
                  cgbound = max(0.5d0 * (cgwav(kb) + cgwav(ki)), eps10)
                  dum = uin_loc * (cgbound * cos(thetai) - cg0) / (cgbound * cos(thetai))
                  ur = cos(alpha2) / (cos(alpha2) + 1.d0) &
                       * (betanp1 - umean + 2.d0 * cg0 - dum)
               end if
               !
               vert = v(Lb) - vmean - vin_loc ! tangential component along cell face
               !
               alphanew = atan2(vert, (ur + 1.d-16))
               if (alphanew > (pi * 0.5d0)) alphanew = alphanew - pi
               if (alphanew <= (-pi * 0.5d0)) alphanew = alphanew + pi
               !
               if (abs(alphanew - alpha2) < 1d-3) then
                  exit
               end if
               alpha2 = alphanew
            end do
            !
            u1(Lb) = (order - 1.d0) * uin_loc + ur + umean
            u1rm(n) = u1(Lb)
            !
            ! try from cell centre, uses value at old time level anyhow
            betaki = ucx(ki) * csu(Lb) + ucy(ki) * snu(Lb) - 2d0 * sqrt(ag * (s1(ki) - bl(ki)))
            un = ucx(ki) * csu(Lb) + ucy(ki) * snu(Lb)
            s1(kb) = 1.5d0 * ((betanp1 - u1rm(n))**2 / 4.d0 / ag + .5d0 * (bl(kb) + bl(ki))) - &
                     0.5d0 * ((betaki - un)**2 / 4.d0 / ag + bl(ki))
         end if

      end if ! riemannpuntje
   end do ! loop snelheidslinks

   ierror = 0

1234 continue
   return
end subroutine xbeach_absgen_bc

subroutine rollerturbulence(k)
   use m_xbeach_data
   use m_xbeach_paramsconst
   use m_waves
   use m_physcoef
   use m_sferic
   use m_flow
   use m_flowgeom
   use m_flowparameters

   implicit none

   integer, intent(in) :: k

   double precision :: disrol, rol, Tw, Tb, cw, ktrb, hloc
   double precision :: dcf, dcfin, ML, twothird

   if (hs(k) <= epshu) then
      ktb(k) = 0d0
      return
   end if
   !
   if (jawave == 3 .or. jawave == 6) then
      cw = rlabda(k) / max(1d-1, twav(k))
      rol = 9d-1 * rhomean * ag * sin(1d-1) * hwav(k)**2 ! Martins 2018
      disrol = 2d-1 * ag * rol / cw ! 2.0*beta = 2d-1
      Tw = twav(k)
      Tb = twav(k)
   end if
   !
   if (jawave == 4) then
      disrol = DR(k)
      rol = R(k)
      cw = max(cwav(k), sqrt(ag * epshu))
      Tw = max(2.*pi / sigmwav(k), 1d0)
      if (turb == TURB_BORE_AVERAGED) then
         Tb = Tbore(k)
      else
         Tb = Trep
      end if
   end if

   twothird = 2d0 / 3d0
   ktrb = (disrol / rhomean)**twothird ! See Battjes, 1975 / 1985

   hloc = max(s1(k) - bl(k), 0.01d0)
   ! compute mixing length
   ML = sqrt(2 * rol * Tw / (rhomean * cw))
   ML = min(ML, hloc); 
   ! exponential decay turbulence over depth
   dcfin = exp(min(100.d0, hloc / max(ML, 1d-10)))
   dcf = min(1.d0, 1.d0 / (dcfin - 1.d0))
   !
   ktb(k) = ktrb * dcf * Tw / max(1d-1, Tb)

end subroutine rollerturbulence

subroutine borecharacter()
   use m_xbeach_data
   use m_flow, only: s1, epshu
   use m_flowgeom, only: ndx, bl
   use m_physcoef
   use m_waves, only: uorb
   use m_debug
   use precision_basics, only: dp

   implicit none

   integer :: nh, nt, k, ierr
   integer :: ih0, it0, ih1, it1
   double precision :: p, q
   double precision :: f0, f1, f2, f3
   double precision :: t0fac
   double precision :: duddtmax, dudtmax, detadxmean, siguref, duddtmean, dudtmean
   double precision :: dh, dt
   double precision, allocatable, save :: h0(:), t0(:), hh(:), detadxmax(:)

   include 'RF.inc'

   if (.not. allocated(h0)) then
      allocate (h0(1:ndx), stat=ierr)
      allocate (t0(1:ndx), stat=ierr)
      allocate (hh(1:ndx), stat=ierr)
      allocate (detadxmax(1:ndx), stat=ierr)
   end if

   dh = 0.03d0
   dt = 1.25d0
   nh = floor(0.99d0 / dh); 
   nt = floor(50.d0 / dt); 
   hh = max(s1 - bl, epshu)

   ! compute dimensionless wave height and wave period in each grid point..
   h0 = min(nh * dh, max(dh, min(H, hh) / hh))
   t0 = min(nt * dt, max(dt, Trep * sqrt(ag / hh)))
   do k = 1, ndx
      if (hh(k) < epshu) then ! some sensible defaults
         Tbore(k) = Trep
         BR(k) = beta
         cycle
      end if
      ih0 = floor(h0(k) / dh); 
      it0 = floor(t0(k) / dt); 
      ih1 = min(ih0 + 1, nh); 
      it1 = min(it0 + 1, nt); 
      p = (h0(k) - ih0 * dh) / dh; 
      q = (T0(k) - it0 * dt) / dt; 
      f0 = (1 - p) * (1 - q); 
      f1 = p * (1 - q); 
      f2 = q * (1 - p); 
      f3 = p * q; 
      if (t0(k) == 50.d0) then
         t0fac = 50.d0 / max((Trep * sqrt(ag / hh(k))), 50.d0)
      elseif (t0(k) == 1.25) then
         t0fac = 1.25d0 / min((Trep * sqrt(ag / hh(k))), 1.25d0)
      else
         t0fac = 1.d0
      end if
      !
      duddtmax = f0 * RF(3, ih0, it0) + f1 * RF(3, ih1, it0) + f2 * RF(3, ih0, it1) + f3 * RF(3, ih1, it1)
      siguref = f0 * RF(4, ih0, it0) + f1 * RF(4, ih1, it0) + f2 * RF(4, ih0, it1) + f3 * RF(4, ih1, it1)
      !
      dudtmax = uorb(k) / sqrt(2.0) / max(waveps, siguref) * sqrt(ag / hh(k)) * t0fac * duddtmax ! urms_cc is uorb, not urms. Checked, set jauorb=1 in mdu for match
      detadxmax(k) = dudtmax * sinh(min(kwav(k) * hh(k), 10d0)) / max(cwav(k), sqrt(H(k) * ag)) / sigmwav(k) ! checked JRE
      !
      if (rfb == 1) then
         duddtmean = f0 * RF(5, ih0, it0) + f1 * RF(5, ih1, it0) + f2 * RF(5, ih0, it1) + f3 * RF(5, ih1, it1)
         dudtmean = uorb(k) / sqrt(2.0) / max(waveps, siguref) * sqrt(ag / hh(k)) * t0fac * duddtmean
         detadxmean = dudtmean * sinh(kwav(k) * hh(k)) / max(cwav(k), sqrt(H(k) * ag)) / sigmwav(k)
         BR(k) = BRfac * sin(atan(detadxmean)) ! checked to be consistent w XB JRE
      end if
   end do

   Tbore = Tbfac * max(Trep / 25.d0, min(Trep / 4.d0, H / (max(cwav, sqrt(H * ag)) * max(detadxmax, 5d-3))))
end subroutine borecharacter

!   subroutine xbeach_map_wind_field(wx, wy, mwind, wmagcc, windspreadfac)
!   use m_flowgeom, only: ln, wcl, lnx, ndx, thetabin, ntheta, dtheta
!
!   implicit none
!
!   double precision, dimension(lnx)        , intent(in) :: wx
!   double precision, dimension(lnx)        , intent(in) :: wy
!   double precision                        , intent(in) :: mwind
!
!   double precision, dimension(ndx)        , intent(inout):: wmagcc
!   double precision, dimension(ntheta,ndx) , intent(inout):: windspreadfac
!
!   integer                                          :: ierr, L, k1, k2, itheta, k
!   double precision, dimension(:), allocatable      :: wxcc            !  [m/s] x-component windspeed cell centered
!   double precision, dimension(:), allocatable      :: wycc            !  [m/s] y-component windspeed cell centered
!   double precision, dimension(:), allocatable      :: wdir            !  [rad] wind speed direction cell centered
!   double precision, dimension(:,:), allocatable    :: dist2           !< temp array for windspreadfac
!   double precision, dimension(:), allocatable      :: dist0           !< temp array for windspreadfac
!
!   ierr = 1
!
!   if (.not.allocated(dist2))   allocate(dist2(1:ntheta , 1:ndx),  stat = ierr)
!   if (.not.allocated(dist0)) allocate(dist0(1:ntheta), stat = ierr)
!   if (.not.allocated(wxcc)) allocate(wxcc(1:ndx), stat = ierr)
!   if (.not.allocated(wycc)) allocate(wycc(1:ndx), stat = ierr)
!   if (.not.allocated(wdir))    allocate(wdir(1:ndx),              stat = ierr)
!
!   wxcc=0d0
!   wycc=0d0
!   wdir=0d0
!   wmagcc=0d0
!   dist2=0d0
!   dist0=0d0
!   windspreadfac=0d0
!
!   do L = 1, lnx                                                              ! interpolate face values to cell centered values
!      k1 = ln(1,L); k2 = ln(2,L)
!      wxcc(k1) = wxcc(k1) + wcl(1,L)*wx(L)
!      wxcc(k2) = wxcc(k2) + wcl(2,L)*wx(L)
!      wycc(k1) = wycc(k1) + wcl(1,L)*wy(L)
!      wycc(k2) = wycc(k2) + wcl(2,L)*wy(L)
!   end do
!
!   wdir = atan2(wycc, wxcc)                               ! assume  cartesian CCW
!   !etilde = rhomean*ag*3.6d-3
!   do k = 1, ndx
!      do itheta = 1,ntheta
!       dist2(itheta, k)=(cos(thetabin(itheta)-wdir(k)))**mwind
!         if(cos(thetabin(itheta)-wdir(k))<0.d0) then
!          dist2(itheta,k)=0.0d0
!         end if
!      end do
!    if (sum(dist2(:,k))>0.d0) then
!       dist0 = dist2(:,k)
!       windspreadfac(:,k) = (dist0/sum(dist0))/dtheta
!    else
!       windspreadfac(:,k)=0.d0
!    endif
!   end do
!
!   do k = 1, ndx
!      wmagcc(k)=sqrt(wxcc(k) * wxcc(k) + wycc(k) * wycc(k))
!   end do
!
!   ierr = 0
!
!1234 continue
!   deallocate(dist2,dist0, stat=ierr)
!   deallocate(wxcc,wycc,wdir, stat=ierr)
!   return
!
!  end subroutine xbeach_map_wind_field
!
!  subroutine xbeach_windsource(ee1, E, tt1, sigmwav , cgwavt, cgwav, hh, dtmaxwav, wsorE, wsorT,egradcg,SwE ,SwT )
!   use m_flowgeom, only: ndx, ndxi, lnx, wcl, ln, thetabin,ntheta, dtheta, bai
!   use m_xbeach_data, only: mwind, Eini, Trepini, snx, csx, wmagcc, windspreadfac, Eful, Tful,CE1, CT1, CE2, CT2, jagradcg
!   use m_physcoef, only: rhomean, ag
!   use m_sferic, only: twopi, pi
!!   use m_growth_curves
!
!   implicit none
!
!   double precision, dimension(ntheta, ndx), intent(in) :: ee1              !<   wave energy/rad
!   double precision, dimension(ndx)        , intent(in) :: E                !<   nodal wave energy
!   double precision, dimension(ntheta, ndx), intent(in) :: tt1              !<   wave period in directional bin
!   double precision, dimension(ndx)        , intent(in) :: sigmwav             !<   nodal wave period
!   double precision, dimension(ntheta, ndx), intent(in) :: cgwavt           !<   group celerity per bin
!   double precision, dimension(ndx)        , intent(in) :: cgwav            !<   nodal group celerity
!   double precision, dimension(ndx)        , intent(in) :: hh               !<   water depth
!   double precision,                         intent(in) :: dtmaxwav         !<   time step
!
!   double precision, dimension(ntheta, ndx), intent(out):: wsorT            !<   wind input period per second
!   double precision, dimension(ntheta, ndx), intent(out):: wsorE            !<   wind input energy per second
!   double precision, dimension(ntheta, ndx), intent(out):: egradcg            !<   wind input energy per second
!   double precision, dimension(ndx),         intent(out):: SwE              !<   nodal wind input energy per second
!   double precision, dimension(ndx),         intent(out):: SwT              !<   nodal wind input period per second
!
!   integer                                          :: ierr
!   integer                                          :: itheta, k, k1, k2, L
!
!   double precision                                 :: Edmlss, Tdmlss, cgdmlss, Ddmlss, wsorTdlss, wsorEdlss, dtdmlss
!
!   double precision                                 :: dir0
!   double precision                                 :: fE, fT, dE, dT, dEful, dTful
!   double precision,  allocatable                   :: gradcg(:,:)
!   double precision                                 :: tgradcg
!
!
!   ierr = 1
!
!   allocate( gradcg( 1:ntheta, 1:ndx), stat = ierr)
!   fE=0d0; fT=0d0; dE=0d0; dT=0d0;
!   wsorE=0d0; wsorT=0d0;
!   gradcg=0d0; tgradcg=0d0; gradcg=0d0;
!
!
!   ! velocity gradient operator
!   call advec_horz_cg(dtmaxwav, snx, csx, cgwavt, gradcg)
!
!   do k = 1, ndxi
!
!        dEful = (Tful / (4.0d0 * pi)) / (CE1 * Eful ** CE2) !d
!
!        do itheta = 1, ntheta
!
!           !compute dimensionless wave state
!           !Edmlss  =   ag / rhomean / wmagcc(k)**4 * E(k)
!           !Tdmlss  =   ag / wmagcc(k) * tt1(itheta,k)
!           !cgdmlss =   cgwavt(itheta ,k) / wmagcc(k)
!
!!          if decoupled growth in each wave bin replace with:
!           Edmlss=ag / rhomean / wmagcc(k)**4 * ee1(itheta,k) / windspreadfac(itheta,k)
!Tdmlss  =   ag / wmagcc(k) * tt1(itheta,k)
!cgdmlss =   cgwavt(itheta ,k) / wmagcc(k)
!
!
!           ! dimensionless magnitude of source terms
!           fE =        CE1* Edmlss**CE2
!           dE =        cgdmlss/ fE
!
!           fT =        CT1* Tdmlss**CT2
!           dT =        cgdmlss/ fT
!
!           wsorEdlss = min(dE , dEful)
!           wsorTdlss = dT !max(dT,dTful) !windspreadfac(itheta,k)  * dT * dtheta !max(dT , dTful)
!
!           SwE(k)= max(wmagcc(k)**3 * rhomean * wsorEdlss, 0.d0) !
!           SwT(k)= max(wsorTdlss , 0.d0) !
!
!           !distribute growth over the wave bins, add gradcg component and make dimensional
!
!            if (jagradcg .eq. 1) then
!              ! egradcg = max(-windspreadfac(itheta,k) * ee0(itheta,k) / windspreadfac(itheta,k) * bai(k) * gradcg(itheta,k) , 0d0) ! * Etaper  perhaps use gradcg(nodal)?
!              egradcg(itheta,k) = max(- ee1(itheta, k) * bai(k) * gradcg(itheta,k) , 0.d0)!
!              !tgradcg = max(-windspreadfac(itheta,k) * twopi / sigmwav(k) * bai(k) * gradcg(itheta,k)  , 0d0)
!            else
!                egradcg(itheta,k) = 0.d0
!                !tgardcg = 0.d0
!           endif
!
!           wsorE(itheta,k) = max(windspreadfac(itheta,k) * SwE(k)   + egradcg(itheta,k), 0.d0 )
!           wsorT(itheta,k) = max(windspreadfac(itheta,k) * dtheta * SwT(k) , 0.d0 )
!
!        enddo
!
!    end do
!
!   ierr = 0
!
!1234 continue
!    deallocate(gradcg, stat = ierr )
!
!   return
!end subroutine xbeach_windsource

!subroutine advec_horz_cg(dtmaxwav, snx, csx, veloc, gradcg)
!   use m_sferic
!   use m_physcoef
!   use m_flowgeom
!   use m_flowparameters, only:eps10
!
!   implicit none
!
!   integer                                                  :: L, k, k1, k2, itheta, ku, kl2s, kl2, kl1, kd, is, ip
!   double precision                                         :: velocL, qds, qst, half, fluxvel1, waku, sl1, sl2, sl3
!   double precision                                         :: cf, ds2, ds1, ds, cwuL
!   double precision, intent(in)                             :: dtmaxwav
!   double precision, intent(in), dimension(ntheta)          :: snx, csx
!   double precision, intent(in), dimension(ntheta, ndx)     :: veloc
!   double precision, intent(out), dimension(ntheta, ndx)    :: gradcg
!   double precision, external                               :: dslim
!
!   double precision                                         :: cs, sn, wuL
!
!   integer                                                  :: nwalls
!
!   gradcg = 0d0
!   velocL = 0d0
!   cwuL   = 0d0
!
!   do L  = 1,lnx                                                              ! upwind (supq) + limited high order (dsq), loop over link
!        k1  = ln(1,L) ; k2 = ln(2,L)                                          ! linker en rechtercelnr geassocieerd aan de links
!
!        do itheta = 1,ntheta
!
!            velocL = acL(L)*veloc(itheta,k1) + (1d0-acL(L))*veloc(itheta,k2)
!
!            cwuL    = velocL * wu(L) * ( csu(L)*csx(itheta) + snu(L)*snx(itheta) )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
!                                                                                     ! inproduct cgx*csu+cgy*snu
!            gradcg(itheta,k1) = gradcg(itheta,k1) - cwul                       ! left cell outward facing normal
!            gradcg(itheta,k2) = gradcg(itheta,k2)  + cwul                       ! right cell inward facing normal
!
!        enddo ! directions
!    enddo ! links
!
!
!!  account for outflow at closed boundaries
!   do nwalls=1,mxwalls
!     k1 = walls(1,nwalls)
!
!     if (k1==7420) then
!        continue
!     end if
!
!     cs =  walls(8,nwalls) ! outward positive
!     sn = -walls(7,nwalls)
!     wuL = walls(9,nwalls)
!
!     do itheta = 1,ntheta
!         cwuL    = veloc(itheta,k1) * wuL* ( cs*csx(itheta) + sn*snx(itheta) )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
!
!         gradcg(itheta,k1) = gradcg(itheta,k1) - cwuL ! minus because of outward positive, like Left adjacent cell
!      end do
!   end do
!
!! account for thin dams
!   do nwalls=1,nthd
!     k1 = thindam(1,nwalls)
!
!     cs = thindam(5,nwalls)  ! outward facing positive? Check with JRE
!     sn = -thindam(4,nwalls)
!     wuL = thindam(6,nwalls)
!
!     do itheta = 1,ntheta
!         cwuL    = veloc(itheta,k1)* wuL * ( cs*csx(itheta) + sn*snx(itheta) )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
!
!         gradcg(itheta,k1) = gradcg(itheta,k1) - cwuL
!     end do
!   end do
!
!end subroutine advec_horz_cg

!subroutine xbeach_wave_period_breaker_dissipation( Df, E, sigmwav, cgwav, kwav, DtotT)
!   use m_flowgeom, only: ndx
!   use m_xbeach_data, only: ndissip, coefdispT, coefdispk
!   use m_physcoef, only: rhomean, ag
!   use m_sferic, only: twopi
!   implicit none
!
!   double precision, dimension(ndx)        , intent(in)  :: Df
!   double precision, dimension(ndx)        , intent(in)  :: E
!   double precision, dimension(ndx)        , intent(in)  :: sigmwav
!   double precision, dimension(ndx)        , intent(in)  :: cgwav
!   double precision, dimension(ndx)        , intent(in)  :: kwav
!   double precision, dimension(ndx)        , intent(out) :: DtotT
!
!   DtotT = - coefdispT * tanh(coefdispk * kwav) * 1.d0 /(1.d0 -ndissip) * (twopi) / sigmwav / sigmwav * cgwav * kwav / E * Df
!
!1234 continue
!   return
!
!end subroutine xbeach_wave_period_breaker_dissipation
!
!subroutine xbeach_wave_compute_period_depth_limitation(E, Tmaxdep )
!   use m_flowgeom, only: ndx
!   use m_xbeach_data, only: wmagcc, aa1, aa2, bb1, bb2
!   use m_physcoef, only: rhomean, ag
!
!   implicit none
!
!   double precision, dimension(ndx)        , intent(in)  :: E
!   double precision, dimension(ndx)        , intent(out) :: Tmaxdep
!
!   integer                                               :: k
!   integer                                               :: ierr
!
!   double precision, allocatable                         :: Edls(:)
!   double precision, allocatable                         :: Tdls(:)
!
!   allocate(Edls(1:ndx), Tdls(1:ndx), stat = ierr)
!
!   Edls=ag / rhomean / wmagcc**4d0 * E
!   Tdls=aa2*(16d0* Edls / (aa1*aa1) )**(bb2/(2*bb1))
!   Tmaxdep = wmagcc * Tdls / ag
!
!1234 continue
!   deallocate(Edls, Tdls, stat=ierr)
!   return
! end subroutine xbeach_wave_compute_period_depth_limitation
!
!
! subroutine advec_horz_windmodel(dtmaxwav, snx, csx, limtypw, quant, veloc, advec)
!   use m_sferic
!   use m_physcoef
!   use m_flowgeom
!   use m_flowparameters, only:eps10
!
!   implicit none
!
!   integer                                                :: L, k, k1, k2, itheta, ku, kl2s, kl2, kl1, kd, is, ip
!   double precision                                       :: velocL, qds, qst, half, fluxvel1, waku, sl1, sl2, sl3
!   double precision                                       :: cf, ds2, ds1, ds, cwuL
!   double precision, intent(in)                           :: dtmaxwav
!   double precision, intent(in), dimension(ntheta)        :: snx, csx
!   integer,          intent(in)                           :: limtypw
!   double precision, intent(in), dimension(ntheta, ndx)   :: veloc
!   double precision, intent(in), dimension(ntheta,ndx)    :: quant
!   double precision, intent(out), dimension(ntheta, ndx)  :: advec
!   double precision, external                             :: dslim
!
!   double precision                                       :: cs, sn, wuL
!
!   integer                                                :: nwalls
!
!   advec = 0d0
!   do L  = 1,lnx                                                              ! upwind (supq) + limited high order (dsq), loop over link
!        k1  = ln(1,L) ; k2 = ln(2,L)                                       ! linker en rechtercelnr geassocieerd aan de links
!
!        do itheta = 1,ntheta
!            velocL  = acL(L)*veloc(itheta,k1) + (1d0-acL(L))*veloc(itheta,k2)
!            cwuL    = velocL*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
!                                                                           ! inproduct cgx*csu+cgy*snu
!            if (cwuL > 0) then                                              !   ->      ds1   ds2
!                k = k1 ; kd = k2 ; is =  1 ; half = 1d0 - acl(L) ; ip = 0   !   ->   ku     k     kd
!            else                                                            !   <-      ds2   ds1
!                k = k2 ; kd = k1 ; is = -1 ; half = acl(L)       ; ip = 3   !   <-   kd     k     ku
!            endif                                                           ! acL = linkse dx fractie van afstand tussen flownodes (slide 83)
!
!            fluxvel1  = is*cwuL*wu(L)                                       ! snelheidsbijdrage linkse cel
!            qst = fluxvel1*quant(itheta,k)                                  ! cg*E voor link L, sector itheta
!            advec(itheta,kd) = advec(itheta,kd) - qst                       ! downwind cel krijgt bijdrage
!            advec(itheta,k)  = advec(itheta,k)  + qst                       ! centrale cel verliest bijdrage
!
!            if (limtypw > 0 ) then                                          ! hogere orde, tijdstapafhankelijk!
!                ku  = klnup(1+ip,L)                                         ! pointer upwind cel horende bij link L
!
!                if (ku .ne. 0 ) then
!                    kl2s = klnup(2+ip,L) ; kl2 = abs(kl2s)                 !
!
!                    if (ku < 0) then
!                        waku = quant(itheta,abs(ku))                        ! pointer naar cel negatief?
!                    else
!                        kl1  = ku
!                        sl1  = slnup(1+ip,L) ; sl2  = slnup(2+ip,L)             ! link upwind cell weight
!                        waku  = quant(itheta,kl1)*sl1 + quant(itheta,kl2)*sl2   ! gewogen gemiddelde upwind waarden
!                    endif
!
!                    sl3 = slnup(3+ip,L)
!                    cf  =  dtmaxwav*abs(cwuL)*dxi(L)
!                    cf  =  half*max( 0d0,1d0-cf )
!                    ds2  =  quant(itheta,kd) - quant(itheta,k)        ! ds1 = voorlopende slope, ds2 = eigen slope
!                    ds1  = (quant(itheta,k)  - waku )*sl3
!
!                    if (abs(ds2)  > eps10 .and. abs(ds1) > eps10) then
!                        ds  =  cf*dslim(ds1, ds2, limtypw)                  ! reconstructie van totale slope volgens 1 van de 4 schema's                                            ! centraal schema
!
!                        if (abs(ds) > eps10) then                           ! als celgemiddelde niet volstaat
!                            qds      =  ds*fluxvel1                         ! slope * linkse celbijdrage
!                            advec(itheta,kd) =  advec(itheta,kd) - qds        ! downwind cel krijgt bijdrage
!                            advec(itheta,k ) =  advec(itheta,k ) + qds        ! cel verliest bijdrage
!                        endif
!                    endif
!                endif
!            endif
!        enddo ! directions
!    enddo ! links
!
!
!!  account for outflow at closed boundaries
!   do nwalls=1,mxwalls
!     k1 = walls(1,nwalls)
!     cs =  walls(8,nwalls) ! outward positive
!     sn = -walls(7,nwalls)
!     wuL = walls(9,nwalls)
!
!     do itheta = 1,ntheta
!         cwuL    = veloc(itheta, k1)*( cs*csx(itheta) + sn*snx(itheta) )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
!         fluxvel1 = cwuL*wuL
!
!         if ( fluxvel1.gt.0 ) then
!           advec(itheta,k1) = advec(itheta,k1) + fluxvel1*quant(itheta,k1)
!         end if
!      end do
!   end do
!
!! account for thin dams
!   do nwalls=1,nthd
!     k1 = thindam(1,nwalls)
!     cs = thindam(5,nwalls)
!     sn = -thindam(4,nwalls)
!     wuL = thindam(6,nwalls)
!
!     do itheta = 1,ntheta
!         cwuL    = veloc(itheta, k1)*( cs*csx(itheta) + sn*snx(itheta) )   ! *au(L)   met cwi: u1(L) + cg*( csu(L)*csx(itheta) + snu(L)*snx(itheta) )
!         fluxvel1 = cwuL*wuL
!
!         if ( fluxvel1.gt.0 ) then
!           advec(itheta,k1) = advec(itheta,k1) + fluxvel1*quant(itheta,k1)
!         end if
!      end do
!   end do
!
!end subroutine advec_horz_windmodel
!
!
!subroutine xbeach_dispersion_windmodel()
!   use m_xbeach_filefunctions
!   use m_flowgeom
!   use m_flow, only: s1, hu
!   use m_flowparameters, only: epshu, epshu
!   use m_sferic, only: pi
!   use m_xbeach_data, only: hdisp, deltaH, H, waveps, sigt, sigmwav, L0t, L1t, Ltempt, cwavt, nwavt, cgwavt, kwavt, cwav, nwav, cgwav, kwav, ee1
!   use m_physcoef, only: ag
!   use m_flowtimes, only: time0
!   use fm_external_forcings_data
!
!   implicit none
!
!   integer                                          :: i,j,j1,j2,k,L,k1,k2,itheta
!   double precision                                 :: kh, hh
!   double precision, external                       :: iteratedispersion
!
!
!   do k=1,ndx
!      hh=max(s1(k)-bl(k),epshu)
!      if (hh > epshu) then
!         hdisp(k) = max(hh + deltaH*H(k), waveps)
!         do itheta = 1,ntheta
!            L0t(itheta,k) = 2*pi*ag/(sigt(itheta,k)**2)
!         enddo
!      else
!         hdisp(k) = waveps
!         do itheta=1,ntheta
!            L0t(itheta,k)    = waveps
!         enddo
!      end if
!   enddo
!   L1t=L0t
!
!   do k=1,ndxi
!      if(hdisp(k).ge.waveps) then
!          do itheta = 1,ntheta
!             if (2*pi/L0t(itheta,k)*hdisp(k) > 5d0) then
!                 Ltempt(itheta,k) = L0t(itheta,k)
!              else
!                 !Ltempt(k) = (2d0*pi*ag/(sigt(itheta,k)**2))*(1-exp(-(sigt(itheta,k)*sqrt(hdisp(k)/ag))**(5d0/2d0)))**(2d0/5d0)
!                 Ltempt(itheta,k) = iteratedispersion(L0t(itheta,k),Ltempt(itheta,k),pi,hdisp(k))
!                 if (Ltempt(itheta,k)<0.d0) then   ! this is an error from iteratedispersion
!                    Ltempt(itheta,k) = -Ltempt(itheta,k)
!                    call writelog('lws','','Warning: no convergence in dispersion relation iteration at t = ', &
!                       time0)
!                 endif
!              endif
!              L1t(itheta,k)=Ltempt(itheta,k)
!          enddo
!      endif
!   end do
!
!   do L=1,nbndz
!      k1=kbndz(1,L); k2=kbndz(2,L)
!      do itheta=1,ntheta
!         L1t(itheta,k1) = L1t(itheta,k2)
!      enddo
!   end do
!
!   do L=1,nbndu
!      k1=kbndu(1,L); k2=kbndu(2,L)
!      do itheta=1,ntheta
!         L1t(itheta,k1) = L1t(itheta,k2)
!      enddo
!   end do
!
!   do k=1,ndx
!       do itheta=1,ntheta
!         kwavt(itheta,k)  = 2*pi/max(L1t(itheta,k),waveps)
!         cwavt(itheta,k)  = sigt(itheta,k)/kwavt(itheta,k)
!         kh   = min(kwavt(itheta,k)*hdisp(k),10.0d0)
!         nwavt(itheta,k)=0.5d0+kh/max(sinh(2d0*kh),waveps)
!         cgwavt(itheta,k)=cwavt(itheta,k)*nwavt(itheta,k)
!      enddo
!   end do
!
!   do k=1,ndx
!      hh=s1(k)-bl(k)
!      if (hh<epshu) then
!         do itheta=1,ntheta
!            kwavt(itheta,k)=0d0
!         enddo
!      endif
!   enddo
!
!   ! define thetabin-energy-weighted average values for propagationspeeds and wave numbers
!   cgwav = max(sum(ee1*cgwavt,1),1d-6)/max(sum(ee1,1),1d-6)
!   cwav  = max(sum(ee1*cwavt,1),1d-6) /max(sum(ee1,1),1d-6)
!   nwav  = max(sum(ee1*nwavt,1),1d-6) /max(sum(ee1,1),1d-6)
!   kwav  = max(sum(ee1*kwavt,1),1d-6) /max(sum(ee1,1),1d-6)
!
!   end subroutine xbeach_dispersion_windmodel

! ======================
! Stationary solver
! ======================

subroutine xbeach_solve_wave_stationary(callType, ierr)
   ! Based on wave energy balance for directionally spread waves, single representative frequency
   ! 4-sweep implicit method
   ! At the main level arrays are two-dimensional in
   ! space but at lower level they are 1D.
   !
   ! (c) 2020 Dano Roelvink, Johan Reyns IHE Delft
   !
   use m_xbeach_paramsconst, only: TURB_NONE
   use m_xbeach_data, m_xbeach_data_hminlw => hminlw
   use m_flowgeom
   use m_flowtimes, only: dnt
   use m_flow, only: epshu, flowwithoutwaves
   use network_data, only: xk, yk, numk
   use m_sferic, only: pi, dg2rd, rd2dg
   use m_physcoef, only: ag, rhomean
   use m_waves, only: hwav, twav, phiwav, uorb, rlabda
   use fm_external_forcings_data, only: nbndw, kbndw
   use m_alloc
   use unstruc_display

   implicit none

   integer, intent(in) :: callType
   integer, intent(out) :: ierr

   ! Local variables
   integer, parameter :: callTypeStationary = 0
   integer, parameter :: callTypeDirections = 1
   integer, parameter :: np = 12

   integer :: k, L, cn1, cn2, n
   integer :: itheta
   integer :: nthetalocal
   double precision :: dthetalocal
   double precision :: wavdir ! incident angle in radians
   double precision :: t0, t1, t2, t3, t4 ! timers
   !
   logical, allocatable, dimension(:) :: gammax_correct
   double precision, allocatable, dimension(:) :: costemp, costemp2, sintemp, sintemp2, hh
   double precision, allocatable, dimension(:) :: RH
   double precision, allocatable, dimension(:, :) :: eebc
   double precision, allocatable, dimension(:) :: thetabinlocal
   double precision, allocatable, dimension(:) :: cgwavlocal
   double precision, allocatable, dimension(:) :: cwavlocal
   double precision, allocatable, dimension(:, :) :: cthetalocal
   !
   ierr = 1
   !
   if (allocated(costemp)) deallocate (costemp, costemp2, sintemp, sintemp2, hh, &
                                       eebc, gammax_correct, stat=ierr)
   allocate (costemp(1:ndx), costemp2(1:ndx), sintemp(1:ndx), sintemp2(1:ndx), hh(1:ndx), stat=ierr)
   allocate (gammax_correct(1:numk), RH(1:numk), stat=ierr)
   costemp = 0d0
   costemp2 = 0d0
   sintemp = 0d0
   sintemp2 = 0d0
   gammax_correct = .false.
   RH = 0d0
   cgwavlocal = 0d0
   cthetalocal = 0d0
   !
   call timer(t0)
   wavdir = mod(270d0 - dir0, 360d0) * dg2rd ! cartesisch, in rad
   !
   select case (callType)
   case (callTypeStationary)
      hh = hhw
      nthetalocal = ntheta
      call realloc(thetabinlocal, nthetalocal, keepExisting=.false., fill=0d0)
      call realloc(cgwavlocal, ndx, keepExisting=.false., fill=0d0)
      call realloc(cwavlocal, ndx, keepExisting=.false., fill=0d0)
      call realloc(cthetalocal, (/nthetalocal, ndx/), keepExisting=.false., fill=0d0)
      thetabinlocal = thetabin
      cwavlocal = cwav
      cgwavlocal = cgwav
      cthetalocal = ctheta
      dthetalocal = dtheta
   case (callTypeDirections)
      hh = hhws
      nthetalocal = ntheta_s
      call realloc(thetabinlocal, nthetalocal, keepExisting=.false., fill=0d0)
      call realloc(cgwavlocal, ndx, keepExisting=.false., fill=0d0)
      call realloc(cwavlocal, ndx, keepExisting=.false., fill=0d0)
      call realloc(cthetalocal, (/nthetalocal, ndx/), keepExisting=.false., fill=0d0)
      thetabinlocal = thetabin_s
      cgwavlocal = cgwav_s
      cwavlocal = cwav_s
      cthetalocal = ctheta_s
      dthetalocal = dtheta_s
   end select
   !
   if (allocated(eebc)) deallocate (eebc)
   allocate (eebc(1:nthetalocal, 1:numk), stat=ierr)
   eebc = 0d0
   !
   ! Transfer cell center values to corners
   call flownod2corner(hh, ndx, hhstat, numk, ierr) ! mode dependent water depth to corners
   hhstat = max(hhstat, m_xbeach_data_hminlw)
   !
   call flownod2corner(cwavlocal, ndx, cstat, numk, ierr)
   cstat = max(cstat, sqrt(ag * epshu)) ! this should not be necessary
   call flownod2corner(cgwavlocal, ndx, cgstat, numk, ierr)
   call flownod2corner(kwav, ndx, kwavstat, numk, ierr)
   !
   do itheta = 1, nthetalocal
      call flownod2corner(cthetalocal(itheta, :), ndx, cthetastat(itheta, :), numk, ierr)
   end do
   !
   call flownod2corner(fw, ndx, fwstat, numk, ierr)
   where (hhstat > fwcutoff)
      fwstat = 0d0
   end where
   !
   ! Assign wave boundary signal to boundary cell corners
   select case (callType)
   case (callTypeStationary)
      ! bc is constant, assign to corner nodes
      do k = 1, numk
         eebc(:, k) = e01
      end do
   case (callTypeDirections)
      ! bc vary along the boundary: project to links and then to corners
      ! flownode2corner does not work here, as inner domain cell centers are 0d0 or have old value
      ! values come from wave_bc: ee_s(ntheta_s,nbndw)
      !
      do n = 1, nbndw
         L = kbndw(3, n)
         cn1 = lncn(1, L)
         cn2 = lncn(2, L)
         ! this assumes that netnode numbering follows corner numbering
         eebc(:, cn1) = eebc(:, cn1) + wcLn(1, L) * ee_s(:, n)
         eebc(:, cn2) = eebc(:, cn2) + wcLn(2, L) * ee_s(:, n)
      end do
   end select
   !
   Hmaxstat = 0.88 / kwavstat * tanh(gamma * kwavstat * hhstat / 0.88d0)
   !
   call timer(t1)
   !
   ! Solve the directional wave energy balance on an unstructured grid
   call solve_energy_balance2Dstat(xk, yk, numk, w, ds, inner, prev, seapts, noseapts, nmmask, &
                                   eebc, thetabinlocal, nthetalocal, wavdir, &
                                   hhstat, kwavstat, cgstat, cthetastat, fwstat, Hmaxstat, Trep, dtmaximp, rhomean, ag, alpha, gamma, m_xbeach_data_hminlw, maxiter, &
                                   Hstat, Dwstat, Dfstat, thetam, uorbstat, eestat)
   call timer(t2)
   !
   ! Correct Dw for gammax before roller computation
   where (Hstat > gammaxxb * hhstat .and. hhstat > m_xbeach_data_hminlw)
      gammax_correct = .true.
   elsewhere
      gammax_correct = .false.
   end where
   !
   do itheta = 1, ntheta
      where (gammax_correct)
         eestat(itheta, :) = eestat(itheta, :) / (Hstat / (gammaxxb * hhstat))**2
      end where
   end do
   !
   where (gammax_correct)
      Hstat = min(Hstat, gammaxxb * hhstat)
   end where
   !
   !
   ! Recompute dissipation after limiting wave energy by gammax
   do k = 1, numk
      call baldock(ag, rhomean, alpha, gamma, hhstat(k), Hstat(k), Hmaxstat(k), Trep, 1, Dwstat(k))
      uorbstat(k) = pi * Hstat(k) / Trep / sinh(min(kwavstat(k) * hhstat(k), 10d0))
      Dfstat(k) = 0.28d0 * rhomean * fwstat(k) * uorbstat(k)**3
   end do
   !
   if (roller > 0 .and. callType == callTypeStationary) then
      !
      call find_upwind_neighbours(xk, yk, numk, thetam, 1, kp, np, wmean, prevmean, dsmean, ierr)
      call solve_roller_balance(xk, yk, numk, prevmean, hhstat, cstat, Dwstat, thetam, beta, ag, maxiter, seapts, noseapts, inner, nmmask, Erstat, Drstat)
      !
      ! Add influence of gammax on roller energy
      if (rollergammax == 1) then
         RH = sqrt(8d0 * Erstat / rhomean / ag)
         where (RH > gammaxxb * hhstat .and. hhstat > m_xbeach_data_hminlw)
            gammax_correct = .true.
         elsewhere
            gammax_correct = .false.
         end where
         !
         where (gammax_correct)
            Erstat = min(Erstat, gammaxxb * hhstat)
         end where
         !
         Drstat = 2.d0 * ag * beta * Erstat / cstat
      end if
   end if
   !
   call timer(t3)
   !
   ! Compute mean wave direction
   costemp = cos(thetam)
   sintemp = sin(thetam)
   call corner2flownod(costemp, numk, costemp2, ndxi, ndx, .false., ierr)
   call corner2flownod(sintemp, numk, sintemp2, ndxi, ndx, .false., ierr)
   thetamean = atan2(sintemp2, costemp2)
   !
   if (calltype == callTypeStationary) then
      ! copy values to flow nodes
      call corner2flownod(Hstat, numk, H, ndxi, ndx, .false., ierr)
      call corner2flownod(Dwstat, numk, D, ndxi, ndx, .false., ierr)
      call corner2flownod(Dfstat, numk, Df, ndxi, ndx, .false., ierr)
      if (roller > 0) then
         call corner2flownod(Erstat, numk, R, ndxi, ndx, .false., ierr)
         call corner2flownod(Drstat, numk, DR, ndxi, ndx, .false., ierr)
      else
         R = 0d0
         DR = D
      end if
      call corner2flownod(uorbstat, numk, uorb, ndxi, ndx, .false., ierr)
      call corner2flownod(kwavstat, numk, kwav, ndxi, ndx, .false., ierr)
      !
      do itheta = 1, ntheta
         costemp = eestat(itheta, :) ! stack
         call corner2flownod(costemp, numk, costemp2, ndxi, ndx, .false., ierr)
         ee1(itheta, :) = costemp2
      end do
      E = sum(ee1, dim=1) * dthetalocal
      costemp = 0d0; costemp2 = 0d0
      !
      call corner2flownod(cstat, numk, cwav, ndxi, ndx, .false., ierr)
      !
      if (roller == 1 .and. turb /= TURB_NONE) then
         call borecharacter() ! calculates BR and Tbore using Rieneck&Fenton approach
      end if
      !
      ! Assign values to wave communication arrays
      hwav = H
      twav = 2d0 * pi / sigmwav
      phiwav = thetamean * rd2dg
      rlabda = 2d0 * pi / kwav
   end if
   !
   ! this part is for online interacter visualisation
   if (jaGUI == 1 .and. .not. flowWithoutWaves) then
      if (ntek > 0) then
         if (mod(int(dnt), ntek) == 0) then
            call wave_makeplotvars()
         end if
      end if
   end if
   !
   call timer(t4)
   !
   write (*, *) 'Stationary wave action computation:        ', t2 - t1, ' seconds'
   if (roller > 0 .and. callType == callTypeStationary) then
      write (*, *) 'Stationary roller computation:          ', t3 - t2, ' seconds'
   end if
   write (*, *) 'Total time stationary wave computation:    ', t4 - t0, ' seconds'
   !
   ierr = 0
1234 continue
   return
end subroutine xbeach_solve_wave_stationary
!
subroutine find_upwind_neighbours(x, y, mn, theta, ntheta, kp, np, w, prev, ds, ierr)

   implicit none

   integer, intent(in) :: mn, ntheta ! length of x,y; length of theta
   integer, intent(in) :: np ! max no surrounding points
   double precision, dimension(mn), intent(in) :: x, y ! x, y coordinates of grid
   integer, dimension(np, mn), intent(in) :: kp ! grid indices of surrounding points per grid point
   double precision, dimension(ntheta, mn), intent(in) :: theta ! array of wave angles
   double precision, dimension(2, ntheta, mn), intent(out) :: w ! per grid point and direction, weight of upwind points
   integer, dimension(2, ntheta, mn), intent(out) :: prev ! per grid point and direction, indices of upwind points
   double precision, dimension(ntheta, mn), intent(out) :: ds ! upwind distance to intersection point for each direction
   integer, intent(out) :: ierr

   double precision, dimension(2) :: xsect, ysect, ww
   double precision :: pi, dss, xi, yi
   integer :: ind1, ind2
   integer :: ip, nploc
   integer :: k, itheta

   ierr = 1

   ! find upwind neighbours for each cell in an unstructured grid x,y (1d
   ! vectors) given vector of directions theta

   pi = 4d0 * atan(1.d0)

   do k = 1, mn
      dss = 0d0
      call findlocpos(kp(:, k), np, 0, nploc)
      nploc = nploc - 1
      if (kp(1, k) /= 0) then
         do itheta = 1, ntheta
            do ip = 1, nploc - 1
               ind1 = kp(ip, k)
               ind2 = kp(ip + 1, k)
               xsect = [x(ind1), x(ind2)]
               ysect = [y(ind1), y(ind2)]
               call intersect_angle(x(k), y(k), theta(itheta, k) + pi, xsect, ysect, ww, dss, xi, yi)
               if (dss >= 1d-10) then
                  w(1, itheta, k) = ww(1)
                  w(2, itheta, k) = ww(2)
                  ds(itheta, k) = dss
                  prev(1, itheta, k) = kp(ip, k)
                  prev(2, itheta, k) = kp(ip + 1, k)
                  exit
               end if
            end do
            if (dss < 1d-10) then
               prev(1, itheta, k) = 1
               prev(2, itheta, k) = 1
               w(1, itheta, k) = 0
               w(2, itheta, k) = 0
               ds(itheta, k) = 1d-3 ! avoid division by zero in matrix
            end if
         end do
      end if
   end do

   ierr = 0
1234 continue
   return

end subroutine find_upwind_neighbours
!
subroutine intersect_angle(x0, y0, phi, x, y, W, ds, xi, yi)

   implicit none

   double precision, intent(in) :: x0, y0, phi
   double precision, dimension(2), intent(in) :: x, y
   double precision, dimension(2), intent(out) :: W
   double precision, intent(out) :: ds, xi, yi

   double precision :: m, a, b, n, L, d1, d2
   double precision :: err
   double precision, parameter :: eps = 1d-3

   if (abs(x(2) - x(1)) > eps) then
      m = (y(2) - y(1)) / (x(2) - x(1))
      a = y(1) - m * x(1)
      n = tan(phi)
      b = y0 - n * x0
      xi = (b - a) / sign(max(abs(m - n), 1d-10), m - n)
      yi = a + m * xi
   else
      yi = (x(1) - x0) * tan(phi) + y0
      xi = x(1)
   end if

   L = hypot(x(2) - x(1), y(2) - y(1))
   d1 = hypot(xi - x(1), yi - y(1))
   d2 = hypot(xi - x(2), yi - y(2))
   ds = hypot(xi - x0, yi - y0)
   err = hypot((x0 - xi) + ds * cos(phi), (y0 - yi) + ds * sin(phi))
   if (abs(L - d1 - d2) < eps .and. err < eps) then
      W(1) = d2 / L
      W(2) = d1 / L
   else
      W(1) = 0.d0
      W(2) = 0.d0
      ds = 0.d0
   end if

end subroutine intersect_angle
!
subroutine solve_energy_balance2Dstat(x, y, mn, w, ds, inner, prev, seapts, noseapts, neumannconnected, &
                                      ee0, theta, ntheta, thetamean, &
                                      hh, kwav, cg, ctheta, fw, Hmaxstat, T, dt, rho, ag, alfa, gamma, hmin, maxiter, &
                                      H, Dw, Df, thetam, uorb, ee)

   use m_partitioninfo
   use precision_basics, only: dp

   implicit none

   ! In/output variables and arrays
   integer, intent(in) :: mn, ntheta !< number of grid points, number of directions
   real(dp), dimension(mn), intent(in) :: x, y !< x,y coordinates of grid
   real(dp), dimension(2, ntheta, mn), intent(in) :: w !< weights of upwind grid points, 2 per grid point and per wave direction
   real(dp), dimension(ntheta, mn), intent(in) :: ds !< distance to interpolated upwind point, per grid point and direction
   real(dp), dimension(ntheta, mn), intent(in) :: ee0 !< distribution of wave energy density
   real(dp), intent(in) :: thetamean !< mean offshore wave direction (rad)
   real(dp), dimension(ntheta), intent(in) :: theta !< distribution of wave angles
   logical, dimension(mn), intent(in) :: inner !< mask of inner grid points (not on boundary)
   integer, dimension(2, ntheta, mn), intent(in) :: prev !< two upwind grid points per grid point and wave direction
   integer, intent(in) :: noseapts !< number of offshore wave boundary points
   integer, dimension(noseapts) :: seapts !< indices of offshore wave boundary points
   integer, dimension(mn), intent(in) :: neumannconnected !< number of neumann boundary point if connected to inner point
   real(dp), dimension(mn), intent(in) :: hh !< water depth
   real(dp), dimension(mn), intent(in) :: kwav !< wave number
   real(dp), dimension(mn), intent(in) :: cg !< group velocity
   real(dp), dimension(ntheta, mn), intent(in) :: ctheta !< refraction speed
   real(dp), dimension(mn), intent(in) :: fw !< wave friction factor
   real(dp), intent(in) :: T !< wave period
   real(dp), intent(in) :: dt !< time step (s)
   real(dp), intent(in) :: rho !< water density
   real(dp), intent(in) :: ag !< grav acceleration
   real(dp), intent(in) :: alfa, gamma !< coefficients in Baldock wave breaking dissipation
   real(dp), intent(in) :: hmin !< limiting depth for wet points in system
   integer, intent(in) :: maxiter !< maximun no of iterations
   real(dp), dimension(mn), intent(in) :: Hmaxstat !< max wave height (Miche)
   real(dp), dimension(mn), intent(out) :: H !< wave height
   real(dp), dimension(mn), intent(out) :: Dw !< wave breaking dissipation
   real(dp), dimension(mn), intent(out) :: Df !< wave friction dissipation
   real(dp), dimension(mn), intent(out) :: thetam !< mean wave direction
   real(dp), dimension(mn), intent(out) :: uorb !< orbital velocity
   real(dp), dimension(ntheta, mn), intent(out) :: ee !< wave energy distribution

   ! Local variables and arrays
   real(dp), dimension(:), allocatable :: ok !< mask for fully iterated points
   real(dp) :: eemax, dtheta !< maximum wave energy density, directional resolution
   integer :: sweep, niter !< sweep number, number of iterations
   integer :: k, k1, k2, i !< counters (k is grid index)
   integer, dimension(:, :), allocatable :: indx !< index for grid sorted per sweep direction
   real(dp), dimension(:, :), allocatable :: eeold !< wave energy density, energy density previous iteration
   real(dp), dimension(:, :), allocatable :: dee !< difference with energy previous iteration
   real(dp), dimension(:), allocatable :: eeprev, cgprev !< energy density and group velocity at upwind intersection point
   real(dp), dimension(:, :), allocatable :: A, B, C, R !< coefficients in the tridiagonal matrix solved per point
   real(dp), dimension(:), allocatable :: DoverE !< ratio of mean wave dissipation over mean wave energy
   real(dp), dimension(:), allocatable :: diff !< maximum difference of wave energy relative to previous iteration
   real(dp), dimension(:), allocatable :: ra !< coordinate in sweep direction
   integer, dimension(4) :: shift
   integer :: iter
   integer :: count
   integer :: itheta
   integer :: ierr
   real(dp) :: percok
   real(dp) :: error
   real(dp) :: Ek
   real(dp) :: Hk
   real(dp) :: Dfk
   real(dp) :: Dwk
   real(dp) :: uorbk
   real(dp), parameter :: pi = 4.d0 * atan(1.d0)
   real(dp), parameter :: crit = 0.001 !< relative accuracy
   integer, parameter :: solverNotConverged = 0
   integer, parameter :: solverConverged = 1

   ! Allocate local arrays
   allocate (ok(mn))
   allocate (indx(4, mn))
   allocate (eeold(ntheta, mn)); eeold = 0d0
   allocate (dee(ntheta, mn)); dee = 0d0
   allocate (eeprev(ntheta)); eeprev = 0d0
   allocate (cgprev(ntheta)); cgprev = 0d0
   allocate (A(ntheta, mn)); A = 0d0
   allocate (B(ntheta, mn)); B = 0d0
   allocate (C(ntheta, mn)); C = 0d0
   allocate (R(ntheta, mn)); R = 0d0
   allocate (DoverE(mn)); DoverE = 0d0
   allocate (diff(mn)); diff = 0d0
   allocate (ra(mn)); ra = 0d0

   ok = solverNotConverged
   indx = 0
   ee = 0d0
   eemax = 1.d0; 
   dtheta = theta(2) - theta(1)
   niter = min(maxiter, 400)

   ! Sort coordinates in sweep directions
   shift = [0, 1, -1, 2]
   do sweep = 1, 4
      ra = x * cos(thetamean + shift(sweep) * pi / 2.d0) + y * sin(thetamean + shift(sweep) * pi / 2.d0)
      call hpsort_eps_epw(mn, ra, indx(sweep, :), 1.d-6)
   end do

   ! Boundary condition at sea side
   do i = 1, noseapts
      k = seapts(i)
      ee(:, k) = ee0(:, k)
   end do
   !
   ! Start iteration
   do iter = 1, niter
      if (jampi == 1) then
         call update_ghosts(ITYPE_CN, ntheta, mn, ee, ierr)
         call update_ghosts(ITYPE_CN, 1, mn, DoverE, ierr)
         call update_ghosts(ITYPE_CN, 1, mn, ok, ierr)
      end if
      sweep = mod(iter, 4)
      if (sweep == 0) then
         sweep = 4; 
      end if
      if (sweep == 1) then
         eeold = ee; 
      end if
      !  Loop over all points depending on sweep direction
      do count = 1, mn
         k = indx(sweep, count)
         !
         if (inner(k)) then
            if (hh(k) > 1.1 * hmin) then
               if (ok(k) == solverNotConverged) then
                  ! Only perform computations on wet inner points that are not yet converged (ok)
                  do itheta = 1, ntheta
                     k1 = prev(1, itheta, k)
                     k2 = prev(2, itheta, k)
                     eeprev(itheta) = w(1, itheta, k) * ee(itheta, k1) + w(2, itheta, k) * ee(itheta, k2)
                     cgprev(itheta) = w(1, itheta, k) * cg(k1) + w(2, itheta, k) * cg(k2)
                  end do
                  !
                  Ek = sum(ee(:, k)) * dtheta
                  Hk = sqrt(8 * Ek / rho / ag)
                  if (Hk > 0.2d0 * Hmaxstat(k)) then
                     call baldock(ag, rho, alfa, gamma, hh(k), Hk, Hmaxstat(k), T, 1, Dwk)
                  else
                     Dwk = 0d0
                  end if
                  uorbk = pi * Hk / T / sinh(min(kwav(k) * hh(k), 10d0))
                  Dfk = 0.28d0 * rho * fw(k) * uorbk**3
                  DoverE(k) = (Dwk + Dfk) / max(Ek, 1.d-6)
                  !
                  do itheta = 2, ntheta - 1
                     A(itheta, k) = -0.5d0 * ctheta(itheta - 1, k) / dtheta
                     B(itheta, k) = 1 / dt + cg(k) / ds(itheta, k) + DoverE(k)
                     C(itheta, k) = 0.5d0 * ctheta(itheta + 1, k) / dtheta
                     R(itheta, k) = ee(itheta, k) / dt + cgprev(itheta) * eeprev(itheta) / ds(itheta, k)
                  end do
                  if (ctheta(1, k) < 0) then
                     A(1, k) = 0.d0
                     B(1, k) = 1 / dt - ctheta(1, k) / dtheta + cg(k) / ds(1, k) + DoverE(k)
                     C(1, k) = ctheta(2, k) / dtheta
                     R(1, k) = ee(1, k) / dt + cgprev(itheta) * eeprev(1) / ds(1, k)
                  else
                     A(1, k) = 0.d0
                     B(1, k) = 1.d0 / dt
                     C(1, k) = 0.d0
                     R(1, k) = 0.d0
                  end if
                  if (ctheta(ntheta, k) > 0) then
                     A(ntheta, k) = -ctheta(ntheta - 1, k) / dtheta
                     B(ntheta, k) = 1 / dt + ctheta(ntheta, k) / dtheta + cg(k) / ds(ntheta, k) + DoverE(k)
                     C(ntheta, k) = 0d0
                     R(ntheta, k) = ee(ntheta, k) / dt + cgprev(itheta) * eeprev(ntheta) / ds(ntheta, k)
                  else
                     A(ntheta, k) = 0.d0
                     B(ntheta, k) = 1.d0 / dt
                     C(ntheta, k) = 0.d0
                     R(ntheta, k) = 0.d0
                  end if
                  ! Solve tridiagonal system per point
                  call solve_tridiag(A(:, k), B(:, k), C(:, k), R(:, k), ee(:, k), ntheta)
                  ee(:, k) = max(ee(:, k), 0.d0)
               end if
            else
               ee(:, k) = 0d0
            end if
            if (neumannconnected(k) /= 0) then
               ee(:, neumannconnected(k)) = ee(:, k)
            end if
         end if
      end do
      !
      if (sweep == 4) then
         ! Check convergence after all 4 sweeps
         do k = 1, mn
            dee(:, k) = ee(:, k) - eeold(:, k)
            diff(k) = maxval(abs(dee(:, k)))
            if (diff(k) / eemax < crit) ok(k) = solverConverged
         end do
         !
         ! Percentage of converged points
         percok = sum(ok) / dble(mn) * 100.d0
         eemax = maxval(ee)
         ! Relative maximum error
         error = maxval(diff) / eemax
         if (jampi == 0) then
            write (*, '(a,i6,a,f10.5,a,f7.2)') 'iteration ', iter / 4, ' error = ', error, '   %ok = ', percok
            if (error < crit .and. iter > 4) then
               write (*, *) 'Stationary wave computation converged, continuing...'
               exit
            end if
         else
            write (*, '(a,i6,a,f7.2)') 'iteration ', iter / 4, '   %ok = ', percok
            call reduce_double_min(percok)
            if (percok > 99.99 .and. iter > 4) then
               write (*, *) 'Stationary wave computation converged, continuing...'
               exit
            end if
         end if
      end if
   end do

   do k = 1, mn
      ! Compute directionally integrated parameters
      ee(:, k) = max(ee(:, k), 0.d0)
      Ek = sum(ee(:, k)) * dtheta
      H(k) = sqrt(8 * Ek / rho / ag)
      call baldock(ag, rho, alfa, gamma, hh(k), H(k), Hmaxstat(k), T, 1, Dw(k))
      uorb(k) = pi * H(k) / T / sinh(min(kwav(k) * hh(k), 10d0))
      Df(k) = 0.28d0 * rho * fw(k) * uorb(k)**3
      thetam(k) = atan2(sum(ee(:, k) * sin(theta)), sum(ee(:, k) * cos(theta)))
   end do

end subroutine solve_energy_balance2Dstat
!
subroutine solve_roller_balance(x, y, mn, prev, hh, c, Dw, thetam, beta, ag, maxiter, seapts, noseapts, inner, neumannconnected, Er, Dr)

   use m_partitioninfo
   use precision_basics, only: dp

   implicit none

   integer, intent(in) :: mn !< no nodes
   real(dp), dimension(mn), intent(in) :: x, y !< node coordinates
   integer, dimension(2, mn), intent(in) :: prev !< upwind indexes
   real(dp), dimension(mn), intent(in) :: hh !< water depth
   real(dp), dimension(mn), intent(in) :: c !< wave celerity
   real(dp), dimension(mn), intent(in) :: Dw !< wave breaker dissipation
   real(dp), dimension(mn), intent(in) :: thetam !< mean wave direction
   real(dp), intent(in) :: beta !< roller slope
   real(dp), intent(in) :: ag !< grav acceleration
   integer, intent(in) :: maxiter
   integer, intent(in) :: noseapts
   integer, dimension(noseapts), intent(in) :: seapts

   logical, dimension(mn), intent(in) :: inner
   integer, dimension(mn), intent(in) :: neumannconnected ! number of neumann boundary point if connected to inner point
   real(dp), dimension(mn), intent(out) :: Er
   real(dp), dimension(mn), intent(out) :: Dr

! Local constants
   real(dp) :: hmin = 0.1d0
   real(dp) :: thetamean, sinthmean, costhmean
   integer, dimension(4) :: shift
   real(dp), dimension(:), allocatable :: ok
   real(dp), dimension(:), allocatable :: ra
   real(dp), dimension(:), allocatable :: F
   integer, dimension(:, :), allocatable :: indx
   integer :: niter
   integer :: sweep, k, iter, count, k1, k2, ierr
   real(dp) :: Afac, Bfac, Cfac, Drst, percok
   real(dp) :: x1, x2, xk, y1, y2, yk
   real(dp) :: costh1, costh2, costhk, sinth1, sinth2, sinthk
   real(dp) :: dtol
   real(dp) :: pi
   integer, parameter :: solverConverged = 1
   integer, parameter :: solverNotConverged = 0

   allocate (ok(mn))
   allocate (ra(mn))
   allocate (indx(4, mn))
   allocate (F(mn))

   niter = min(maxiter, 200)
   pi = 4d0 * atan(1.d0)
   indx = 0
   dtol = 1d-6
   percok = 0d0

   sinthmean = sum(sin(thetam(seapts)))
   costhmean = sum(cos(thetam(seapts)))
   thetamean = atan2(sinthmean, costhmean)
   ! Sort coordinates in sweep directions
   shift = [0, 1, -1, 2]
   do sweep = 1, 4
      ra = x * cos(thetamean + shift(sweep) * pi / 2.d0) + y * sin(thetamean + shift(sweep) * pi / 2.d0)
      call hpsort_eps_epw(mn, ra, indx(sweep, :), 1.d-6)
   end do

   ok = solverNotConverged
   do k = 1, noseapts
      ok(seapts(k)) = solverConverged
      F(seapts(k)) = 0.d0
   end do

   ! Start iteration
   do iter = 1, niter
      if (jampi == 1) then
         call update_ghosts(ITYPE_CN, 1, mn, Er, ierr)
         call update_ghosts(ITYPE_CN, 1, mn, Dr, ierr)
         call update_ghosts(ITYPE_CN, 1, mn, F, ierr)
         call update_ghosts(ITYPE_CN, 1, mn, ok, ierr)
      end if
      sweep = mod(iter, 4)
      if (sweep == 0) then
         sweep = 4; 
      end if
      !  Loop over all points depending on sweep direction
      do count = 1, mn
         k = indx(sweep, count)
         if (inner(k)) then
            if (hh(k) > 1.1d0 * hmin) then
               if (ok(k) == solverNotConverged) then
                  k1 = prev(1, k)
                  k2 = prev(2, k)
                  if (ok(k1) == solverConverged .and. ok(k2) == solverConverged) then
                     x1 = x(k1)
                     y1 = y(k1)
                     x2 = x(k2)
                     y2 = y(k2)
                     xk = x(k)
                     yk = y(k)
                     costh1 = cos(thetam(k1))
                     sinth1 = sin(thetam(k1))
                     costh2 = cos(thetam(k2))
                     sinth2 = sin(thetam(k2))
                     costhk = cos(thetam(k))
                     sinthk = sin(thetam(k))
                     Cfac = x1 * (y2 - yk) + x2 * (yk - y1) + xk * (y1 - y2)
                     Afac = (F(k1) * costh1 * (y2 - yk) + F(k2) * costh2 * (yk - y1) &
                             - F(k1) * sinth1 * (x2 - xk) - F(k2) * sinth2 * (xk - x1)) / max(Cfac, dtol)
                     Bfac = (costhk * (y1 - y2) - sinthk * (x1 - x2)) / sign(max(abs(Cfac), dtol), Cfac)
                     Drst = 2.d0 * ag * beta / c(k)**2
                     F(k) = (Dw(k) - Afac) / (Bfac + Drst)
                     Er(k) = F(k) / c(k)
                     Er(k) = max(Er(k), 0d0)
                     Dr(k) = Drst * F(k)
                     ok(k) = solverConverged
                     if (neumannconnected(k) /= 0) then
                        F(neumannconnected(k)) = F(k)
                        Er(neumannconnected(k)) = Er(k)
                        Dr(neumannconnected(k)) = Dr(k)
                        ok(neumannconnected(k)) = ok(k)
                     end if
                  end if
               end if
            else
               ok(k) = solverConverged
               F(k) = 0.d0
               Er(k) = 0.d0
               Dr(k) = 0.d0
            end if
         else
            ok(k) = solverConverged
         end if
      end do
      !
      if (sweep == 4) then
         percok = sum(ok) / dble(mn) * 100.d0
         write (*, *) 'iteration: ', iter / 4, '   % ok: ', percok
         if (jampi > 0) then
            call reduce_double_min(percok)
         end if
      end if
      !
      if (percok > 99 .and. iter > 4) then
         write (*, *) 'iteration: ', iter / 4, '   % ok: ', percok
         write (*, *) 'Stationary roller computation converged, continuing...'
         exit
      end if
   end do

end subroutine solve_roller_balance
!
!
subroutine solve_tridiag(a, b, c, d, x, n)
   implicit none
   !         a - sub-diagonal (means it is the diagonal below the main diagonal)
   !         b - the main diagonal
   !         c - sup-diagonal (means it is the diagonal above the main diagonal)
   !         d - right part
   !         x - the answer
   !         n - number of equations

   integer, intent(in) :: n
   double precision, dimension(n), intent(in) :: a, b, c, d
   double precision, dimension(n), intent(out) :: x
   double precision, dimension(n) :: cp, dp
   double precision :: m

   integer :: i
   integer, parameter :: r8 = kind(1.d0)

   ! initialize c-prime and d-prime
   cp(1) = c(1) / b(1)
   dp(1) = d(1) / b(1)
   ! solve for vectors c-prime and d-prime
   do i = 2, n
      m = b(i) - cp(i - 1) * a(i)
      cp(i) = c(i) / m
      dp(i) = (d(i) - dp(i - 1) * a(i)) / m
   end do
   ! initialize x
   x(n) = dp(n)
   ! solve for x from the vectors c-prime and d-prime
   do i = n - 1, 1, -1
      x(i) = dp(i) - cp(i) * x(i + 1)
   end do

end subroutine solve_tridiag
!
subroutine baldock(rho, g, alfa, gamma, hh, H, Hmax, T, opt, Dw)

   implicit none

   double precision, intent(in) :: rho !< water density
   double precision, intent(in) :: g !< gravitational acceleration
   double precision, intent(in) :: alfa !< proportionality factor wave breaker dissipation
   double precision, intent(in) :: gamma !< breaker index
   double precision, intent(in) :: hh !< water depth
   double precision, intent(in) :: H !< wave height
   double precision, intent(in) :: Hmax !< maximum wave height
   double precision, intent(in) :: T !< wav period
   integer, intent(in) :: opt !< dissipation scaled with H^2 (1) or H^3 (!=1)
   double precision, intent(out) :: Dw !< wave breaker dissipation

   double precision, parameter :: dtol = 1d-8
   integer, parameter :: scalingWaveheightSq = 1

   if (H < dtol) then
      Dw = 0d0
      return
   end if
   ! Compute dissipation according to Baldock
   if (opt == scalingWaveheightSq) then
      Dw = 0.25 * alfa * rho * g / T * exp(-(Hmax / H)**2) * (Hmax**2 + H**2)
   else
      Dw = 0.25 * alfa * rho * g / T * exp(-(Hmax / H)**2) * (Hmax**3 + H**3) / gamma / hh
   end if
end subroutine baldock
!
!
! Copyright (C) 2010-2024 Samuel Ponce', Roxana Margine, Carla Verdi, Feliciano Giustino
! Copyright (C) 2007-2024 Jesse Noffsinger, Brad Malone, Feliciano Giustino
!
! This file is distributed under the terms of the GNU General Public
! License. See the file `LICENSE' in the root directory of the
! present distribution, or http://www.gnu.org/copyleft.gpl.txt .
!
! Adapted from flib/hpsort_eps
!---------------------------------------------------------------------
subroutine hpsort_eps_epw(n, ra, ind, eps)
   !---------------------------------------------------------------------
   ! sort an array ra(1:n) into ascending order using heapsort algorithm,
   ! and considering two elements being equal if their values differ
   ! for less than "eps".
   ! n is input, ra is replaced on output by its sorted rearrangement.
   ! create an index table (ind) by making an exchange in the index array
   ! whenever an exchange is made on the sorted data array (ra).
   ! in case of equal values in the data array (ra) the values in the
   ! index array (ind) are used to order the entries.
   ! if on input ind(1)  = 0 then indices are initialized in the routine,
   ! if on input ind(1) != 0 then indices are assumed to have been
   !                initialized before entering the routine and these
   !                indices are carried around during the sorting process
   !
   ! no work space needed !
   ! free us from machine-dependent sorting-routines !
   !
   ! adapted from Numerical Recipes pg. 329 (new edition)
   !
   implicit none

   !-input/output variables
   integer, intent(in) :: n
   double precision, intent(in) :: eps
   integer, dimension(n), intent(inout) :: ind
   double precision, dimension(n), intent(inout) :: ra

   !-local variables
   integer :: i, ir, j, l, iind
   double precision :: rra
   !
   ! initialize index array
   if (ind(1) == 0) then
      do i = 1, n
         ind(i) = i
      end do
   end if
   ! nothing to order
   if (n < 2) return
   ! initialize indices for hiring and retirement-promotion phase
   l = n / 2 + 1

   ir = n

   sorting: do

      ! still in hiring phase
      if (l > 1) then
         l = l - 1
         rra = ra(l)
         iind = ind(l)
         ! in retirement-promotion phase.
      else
         ! clear a space at the end of the array
         rra = ra(ir)
         !
         iind = ind(ir)
         ! retire the top of the heap into it
         ra(ir) = ra(1)
         !
         ind(ir) = ind(1)
         ! decrease the size of the corporation
         ir = ir - 1
         ! done with the last promotion
         if (ir == 1) then
            ! the least competent worker at all !
            ra(1) = rra
            !
            ind(1) = iind
            exit sorting
         end if
      end if
      ! wheter in hiring or promotion phase, we
      i = l
      ! set up to place rra in its proper level
      j = l + l
      !
      do while (j <= ir)
         if (j < ir) then
            ! compare to better underling
            if (hslt(ra(j), ra(j + 1))) then
               j = j + 1
            end if
         end if
         ! demote rra
         if (hslt(rra, ra(j))) then
            ra(i) = ra(j)
            ind(i) = ind(j)
            i = j
            j = j + j
         else
            ! set j to terminate do-while loop
            j = ir + 1
         end if
      end do
      ra(i) = rra
      ind(i) = iind

   end do sorting
contains

   !  internal function
   !  compare two real number and return the result

   logical function hslt(a, b)
      double precision :: a, b
      if (abs(a - b) < eps) then
         hslt = .false.
      else
         hslt = (a < b)
      end if
   end function hslt
   !
end subroutine hpsort_eps_epw
!
!
subroutine disper_approx(h, T, k, n, C, Cg, mn)
   integer, intent(in) :: mn
   double precision, dimension(mn), intent(in) :: h
   double precision, intent(in) :: T
   double precision, dimension(mn), intent(out) :: k, n, C, Cg

   double precision :: sigma, g, pi
   g = 9.81d0
   pi = 4.d0 * atan(1.d0)
   sigma = 2.d0 * pi / T
   k = sigma**2 / g * (1 - exp(-(sigma * sqrt(h / g))**(2.5)))**(-0.4)
   C = sigma / k
   n = 0.5d0 + k * h / sinh(2 * k * h)
   Cg = n * C

end subroutine disper_approx
!
subroutine fm_surrounding_points(no_nodes, connected_nodes, no_connected_nodes, no_cells, kp, ierr)
   use m_flowgeom

   implicit none

   integer, intent(in) :: no_nodes ! number of network nodes
   integer, intent(in) :: no_connected_nodes ! max node numbers connected to each cell
   integer, dimension(no_cells, no_connected_nodes), intent(in) :: connected_nodes ! node numbers connected to each cell
   integer, intent(in) :: no_cells ! number of cells
   integer, dimension(12, no_nodes), intent(out) :: kp ! sorted surrounding node numbers for each node
   integer, intent(out) :: ierr

   ! Local variables
   integer, dimension(:), allocatable :: no_connected_cells
   integer, dimension(:, :), allocatable :: connected_cells
   integer, dimension(4) :: kpts, edge
   integer, dimension(20) :: surr_points
   integer, dimension(12) :: surr_pts

   integer :: k
   integer :: ip, j, jj
   integer :: inode, knode, kcell, kn, isp, isp2
   integer :: next

   ierr = 1

   if (allocated(no_connected_cells)) deallocate (no_connected_cells, connected_cells)
   allocate (no_connected_cells(no_nodes))
   allocate (connected_cells(no_nodes, 12))
   no_connected_cells = 0
   connected_cells = 0
   kp = 0

   do k = 1, no_cells
      do inode = 1, no_connected_nodes
         knode = connected_nodes(k, inode)
         if (knode > 0) then
            no_connected_cells(knode) = no_connected_cells(knode) + 1
            connected_cells(knode, no_connected_cells(knode)) = k
         end if
      end do
   end do

   do kn = 1, no_nodes ! for each node (numk)
      surr_points = 0
      surr_pts = 0
      isp = 1
      do kcell = 1, no_connected_cells(kn) ! for each cell connected to node
         k = connected_cells(kn, kcell) ! get cell number
         kpts = connected_nodes(k, :) ! all nodes in that cell
         if (kpts(4) == 0) then
            jj = 0
            do j = 1, 3
               if (kpts(j) /= kn) then
                  jj = jj + 1
                  edge(jj) = kpts(j)
               end if
            end do
            surr_points(isp:isp + 1) = edge(1:2)
            isp = isp + 2
         else
            ip = minloc(abs(kpts - kn), 1)
            if (ip == 1) then
               edge = [kpts(2), kpts(3), kpts(3), kpts(4)]
            elseif (ip == 2) then
               edge = [kpts(3), kpts(4), kpts(4), kpts(1)]
            elseif (ip == 3) then
               edge = [kpts(4), kpts(1), kpts(1), kpts(2)]
            else
               edge = [kpts(1), kpts(2), kpts(2), kpts(3)]
            end if
            surr_points(isp:isp + 3) = edge(1:4)
            isp = isp + 4
         end if
      end do
      isp = isp - 1 ! number of surrounding points
      !   now connect the edges
      if (isp >= 2) then
         surr_pts(1:2) = surr_points(1:2)
         surr_points = [surr_points(3:), 0, 0]
         isp2 = 2
         isp = isp - 2
         do while (isp >= 2)
            call findlocpos(surr_points, isp, surr_pts(isp2), next)
            if (next /= -1) then
               if (mod(next, 2) == 1) then
                  surr_pts(1:isp2 + 1) = [surr_pts(1:isp2), surr_points(next + 1)]
                  surr_points = [surr_points(1:next - 1), surr_points(next + 2:)]
               else
                  surr_pts(1:isp2 + 1) = [surr_pts(1:isp2), surr_points(next - 1)]
                  surr_points = [surr_points(1:next - 2), surr_points(next + 1:)]
               end if
            else
               call findlocpos(surr_points, isp, surr_pts(1), next)
               if (mod(next, 2) == 1) then
                  surr_pts(1:isp2 + 1) = [surr_points(next + 1), surr_pts(1:isp2)]
                  surr_points = [surr_points(1:next - 1), surr_points(next + 2:)]
               else
                  surr_pts(1:isp2 + 1) = [surr_points(next - 1), surr_pts(1:isp2)]
                  surr_points = [surr_points(1:next - 2), surr_points(next + 1:)]
               end if

            end if

            isp2 = isp2 + 1
            isp = isp - 2
         end do
      else
         surr_pts = 0
      end if
      kp(:, kn) = surr_pts

   end do

   ierr = 0
1234 continue
   return

end
!
subroutine findlocpos(a, n, b, indx)

   implicit none

   integer, intent(in) :: n, b
   integer, intent(out) :: indx
   integer, dimension(n), intent(in) :: a

   integer :: i

   indx = -1
   do i = 1, n
      if (a(i) == b) then
         indx = i
         return
      end if
   end do
end

subroutine fill_connected_nodes(ierr)
   use network_data
   use m_xbeach_data

   implicit none

   integer, intent(out) :: ierr

   integer :: k, kk

   ierr = 1
   ! get maximum number of connected nodes over model domain
   no_connected_nodes = 0
   do k = 1, nump
      no_connected_nodes = max(no_connected_nodes, netcell(k)%n)
   end do

   no_connected_nodes = no_connected_nodes + 1 ! possible ghost node
   if (allocated(connected_nodes)) deallocate (connected_nodes)
   allocate (connected_nodes(nump, no_connected_nodes))
   connected_nodes = 0

   ! fill array with netnodes per netcell
   do k = 1, nump
      do kk = 1, netcell(k)%n
         connected_nodes(k, kk) = netcell(k)%nod(kk)
      end do
   end do

   ierr = 0
1234 continue
   return

end subroutine fill_connected_nodes

subroutine getbndwzcornerpts(ierr)
   use fm_external_forcings_data
   use m_flowgeom
   use network_data
   use m_xbeach_data, only: kbndu2kbndw, kbndz2kbndw, noseapts, seapts, wmask, nmmask, inner
   use gridoperations, only: othernode

   implicit none

   integer, intent(out) :: ierr

   ! local variables
   integer :: cn1, cn2
   integer :: k, ko
   integer :: L, LL
   integer :: cnt
   integer :: iwalls

   double precision :: sumw

   ierr = 1
   ! 1: weight 1 of attached wave bnd wmask(2,L), 3: weight 2 of attached bnd of attached wave bnd wmask(4,L)
   wmask = 0

   ! bnd point connected to inner point of neumann bnd;
   nmmask = 0

   !> wave energy boundaries
   !  set mask
   do k = 1, nbndw
      L = kbndw(3, k)
      cn1 = lncn(1, L)
      cn2 = lncn(2, L)
      wmask(1, cn1) = wcLn(1, L); wmask(2, cn1) = k
      wmask(3, cn2) = wcLn(2, L); wmask(4, cn2) = k
      !
      inner(cn1) = .false.
      inner(cn2) = .false.
      !
   end do
   !
   ! Normalize weights
   do k = 1, numk
      sumw = wmask(1, k) + wmask(3, k)
      if (sumw == 0d0) cycle
      wmask(1, k) = wmask(1, k) / sumw
      wmask(3, k) = wmask(3, k) / sumw
   end do
   !
   !> get indexes of edge nodes with wave boundary
   noseapts = 0
   do k = 1, numk
      if (wmask(2, k) > 0 .or. wmask(4, k) > 0) noseapts = noseapts + 1
   end do
   !
   ! allocate seapts array and fill
   if (allocated(seapts)) deallocate (seapts)
   allocate (seapts(noseapts))
   cnt = 0
   do k = 1, numk
      if (wmask(2, k) > 0 .or. wmask(4, k) > 0) then
         cnt = cnt + 1
         seapts(cnt) = k
      end if
   end do

   !> non-wave open u and z boundaries get neumann flag
   do k = 1, nbndz
      if (kbndz2kbndw(k) /= 0) cycle ! then wave dirichlet bnd
      L = kbndz(3, k)
      cn1 = lncn(1, L)
      cn2 = lncn(2, L)
      !
      ! Find internal node attached to bnd node (implicitly assumes *no* triangular borders, if so takes last one)
         !! network_data::NB values: 1=INTERN, 2=RAND, 3=HOEK, 0/-1=DOET NIET MEE OF 1D
      !  (KN(3,L) == 2) 2D
      do LL = 1, size(nod(cn1)%lin)
         if (kn(3, abs(nod(cn1)%lin(LL))) /= 2) cycle ! not 2d link
         call othernode(cn1, abs(nod(cn1)%lin(LL)), ko)
         if (nb(ko) /= 1) cycle ! not inner net node
         nmmask(ko) = cn1
      end do
      !
      do LL = 1, size(nod(cn2)%lin)
         if (kn(3, abs(nod(cn2)%lin(LL))) /= 2) cycle
         call othernode(cn2, abs(nod(cn2)%lin(LL)), ko)
         if (nb(ko) /= 1) cycle
         nmmask(ko) = cn2
      end do
   end do
   !
   do k = 1, nbndu
      if (kbndu2kbndw(k) /= 0) cycle ! then wave dirichlet bnd
      L = kbndu(3, k)
      cn1 = lncn(1, L)
      cn2 = lncn(2, L)
      !
      ! Find internal node
      do LL = 1, size(nod(cn1)%lin)
         if (kn(3, abs(nod(cn1)%lin(LL))) /= 2) cycle
         call othernode(cn1, abs(nod(cn1)%lin(LL)), ko)
         if (nb(ko) /= 1) cycle
         nmmask(ko) = cn1
      end do
      !
      do LL = 1, size(nod(cn2)%lin)
         if (kn(3, abs(nod(cn2)%lin(LL))) /= 2) cycle
         call othernode(cn2, abs(nod(cn2)%lin(LL)), ko)
         if (nb(ko) /= 1) cycle
         nmmask(ko) = cn2
      end do
   end do
   !
   ! walls: could be simpler using attached links, but not here for consistency
   do iwalls = 1, mxwalls
      cn1 = walls(2, iwalls) ! first corner
      cn2 = walls(3, iwalls) ! second corner
      ! Find internal node
      if (wmask(2, cn1) > 0 .or. wmask(4, cn1) > 0) goto 10 ! do next node
      do LL = 1, size(nod(cn1)%lin)
         if (kn(3, abs(nod(cn1)%lin(LL))) /= 2) cycle
         call othernode(cn1, abs(nod(cn1)%lin(LL)), ko)
         if (nb(ko) /= 1) cycle
         nmmask(ko) = cn1
      end do
      !
10    if (wmask(2, cn2) > 0 .or. wmask(4, cn2) > 0) cycle
      do LL = 1, size(nod(cn2)%lin)
         if (kn(3, abs(nod(cn2)%lin(LL))) /= 2) cycle
         call othernode(cn2, abs(nod(cn2)%lin(LL)), ko)
         if (nb(ko) /= 1) cycle
         nmmask(ko) = cn2
      end do
   end do

   ierr = 0
1234 continue
   return
end subroutine getbndwzcornerpts

subroutine allocstatsolverarrays(callType, ierr)
   use m_alloc
   use m_xbeach_data
   use m_flowgeom, only: ntheta, ntheta_s
   use network_data, only: numk

   implicit none

   integer, intent(in) :: callType
   integer, intent(out) :: ierr

   integer :: ntheta_local

   ierr = 1

   select case (callType)
   case (0) ! regular stationary run
      ntheta_local = ntheta
   case (1) ! single_dir run
      ntheta_local = ntheta_s
   end select

   call realloc(w, (/2, ntheta_local, numk/), stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('w  (2,ntheta_local,numk)', ierr, ntheta_local * numk * 2)
   call realloc(ds, (/ntheta_local, numk/), stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('ds  (ntheta_local,numk)', ierr, ntheta_local * numk)
   call realloc(inner, numk, stat=ierr, keepExisting=.false., fill=.false.)
   call aerr('inner  (numk)', ierr, numk)
   call realloc(prev, (/2, ntheta_local, numk/), stat=ierr, keepExisting=.false., fill=0)
   call aerr('prev  (2,ntheta_local,numk)', ierr, ntheta_local * numk * 2)
   call realloc(hhstat, numk, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('hhstat  (numk)', ierr, numk)
   call realloc(kwavstat, numk, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('kwavstat  (numk)', ierr, numk)
   call realloc(cgstat, numk, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('cgstat  (numk)', ierr, numk)
   call realloc(cstat, numk, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('cstat  (numk)', ierr, numk)
   call realloc(cthetastat, (/ntheta_local, numk/), stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('cthetastat  (ntheta_local,numk)', ierr, numk * ntheta_local)
   call realloc(eestat, (/ntheta_local, numk/), stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('eestat  (ntheta_local,numk)', ierr, numk * ntheta_local)
   call realloc(fwstat, numk, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('fwstat  (numk)', ierr, numk)
   call realloc(Hstat, numk, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('Hstat  (numk)', ierr, numk)
   call realloc(Dwstat, numk, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('Dwstat  (numk)', ierr, numk)
   call realloc(Dfstat, numk, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('Dfstat  (numk)', ierr, numk)
   call realloc(thetam, numk, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('thetam  (numk)', ierr, numk)
   call realloc(uorbstat, numk, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('uorbstat  (numk)', ierr, numk)
   call realloc(dhdxstat, numk, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('dhdxstat  (numk)', ierr, numk)
   call realloc(dhdystat, numk, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('dhdystat  (numk)', ierr, numk)
   call realloc(dhdystat, numk, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('dhdystat  (numk)', ierr, numk)
   call realloc(wmask, (/4, numk/), stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('wmask  (4,numk)', ierr, 4 * numk)
   call realloc(nmmask, numk, stat=ierr, keepExisting=.false., fill=0)
   call aerr('nmmask  (numk)', ierr, numk)
   call realloc(kp, (/12, numk/), stat=ierr, keepExisting=.false., fill=0)
   call aerr('kp  (12,numk)', ierr, numk * 12)
   call realloc(Hmaxstat, numk, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('Hmaxstat  (numk)', ierr, numk)
   !
   if (roller > 0) then
      call realloc(Erstat, numk, stat=ierr, keepExisting=.false., fill=0d0)
      call aerr('Erstat  (numk)', ierr, numk)
      call realloc(wmean, (/2, 1, numk/), stat=ierr, keepExisting=.false., fill=0d0)
      call aerr('wmean  (2,1,numk)', ierr, numk * 2)
      call realloc(dsmean, (/1, numk/), stat=ierr, keepExisting=.false., fill=0d0)
      call aerr('dsmean  (1,numk)', ierr, numk)
      call realloc(prevmean, (/2, 1, numk/), stat=ierr, keepExisting=.false., fill=0)
      call aerr('prevmean  (2,1,numk)', ierr, numk * 2)
      call realloc(Drstat, numk, stat=ierr, keepExisting=.false., fill=0d0)
      call aerr('Drstat  (numk)', ierr, numk)
   end if
   !
   if (ierr > 0) goto 1234
   ierr = 0
1234 continue
   if (ierr > 0) write (*, *) 'xbeachwaves::allocstatsolverarrays :: Error.'
   return
end subroutine allocstatsolverarrays

subroutine corner2flownod(quantin, numk, quantout, ndxi, ndx, skipnbndw, ierr)
   use m_flowgeom, only: banf, ba, mxban, nban
   use fm_external_forcings_data
   use m_xbeach_data, only: kbndz2kbndw, kbndu2kbndw

   implicit none

   logical, intent(in) :: skipnbndw
   integer, intent(in) :: numk
   integer, intent(in) :: ndxi
   integer, intent(in) :: ndx
   double precision, dimension(numk), intent(in) :: quantin
   double precision, dimension(ndx), intent(out) :: quantout
   integer, intent(out) :: ierr

   ! local variables
   integer :: k, kb, ki
   integer :: n, mx

   ierr = 1

   quantout = 0d0
   do mx = 1, mxban
      k = nban(1, mx) ! corner
      n = nban(2, mx) ! cell centre
      !
      quantout(n) = quantout(n) + banf(mx) * quantin(k)
   end do

   do k = 1, ndxi
      quantout(k) = quantout(k) / ba(k)
   end do

   ! open boundaries that are not wave energy bnds get inner domain value
   do k = 1, nbndz
      if (skipnbndw .and. allocated(zbndw)) then
         if (kbndz2kbndw(k) > 0) cycle
      end if
      kb = kbndz(1, k)
      ki = kbndz(2, k)
      quantout(kb) = quantout(ki)
   end do

   do k = 1, nbndu
      if (skipnbndw .and. allocated(zbndw)) then
         if (kbndu2kbndw(k) > 0) cycle
      end if
      kb = kbndu(1, k)
      ki = kbndu(2, k)
      quantout(kb) = quantout(ki)
   end do

   ierr = 0
1234 continue
   return

end subroutine corner2flownod

subroutine flownod2corner(quantin, ndx, quantout, numk, ierr)
   use m_flowgeom, only: ban, banf, mxban, nban

   implicit none

   integer, intent(in) :: ndx
   integer, intent(in) :: numk
   double precision, dimension(ndx), intent(in) :: quantin
   double precision, dimension(numk), intent(out) :: quantout
   integer, intent(out) :: ierr

   ! local variables
   integer :: k, n, mx

   ierr = 1

   quantout = 0d0
   do mx = 1, mxban
      k = nban(1, mx) ! corner
      n = nban(2, mx) ! cell centre
      !
      quantout(k) = quantout(k) + banf(mx) * quantin(n)
   end do

   do k = 1, numk
      quantout(k) = quantout(k) / ban(k)
   end do

   ierr = 0
1234 continue
   return

end subroutine flownod2corner

subroutine timer(t)
   double precision, intent(out) :: t
   integer :: count, count_rate, count_max
   call system_clock(count, count_rate, count_max)
   t = dble(count) / count_rate
end subroutine timer

! -----------------------------------------------------------
! --- Small subroutine to reseed random number generator ----
! -----------------------------------------------------------
subroutine init_seed(outseed)
   integer :: i, n, clock
   integer, dimension(:), allocatable :: seed
   integer, intent(out) :: outseed
   ! RANDOM_SEED does not result in a random seed for each compiler, so we better make sure that we have a pseudo random seed.
   ! I am not sure what is a good n
   n = 40
   i = 1
   ! not sure what size means here
   ! first call with size
   call random_seed(size=n)
   ! define a seed array of size n
   allocate (seed(n))
   ! what time is it
   call system_clock(COUNT=clock)
   ! define the seed vector based on a prime, the clock and the set of integers
   seed = clock + 37 * (/(i - 1, i=1, n)/)
   ! if mpi do we need a different seed on each node or the same???
   ! if we do need different seeds on each node
   ! seed *= some big prime * rank ?
   ! now use the seed
   call random_seed(PUT=seed)
   outseed = seed(1)
end subroutine init_seed

! Compute flow fields with relaxation for singledir and wci
subroutine update_means_wave_flow()
   use m_xbeach_data
   use m_flowgeom
   use m_flowtimes
   use m_flow

   implicit none

   double precision :: factime

   ! Stationary part single_dir
   if (single_dir > 0) then
      factime = 1.d0 / (wavint * 2d0) * dts
      hhws = max(factime * (s1 - bl) + (1d0 - factime) * hhws, epshu)
      if (wci > 0) then
         ucxws = factime * ucx + (1d0 - factime) * ucxws
         ucyws = factime * ucy + (1d0 - factime) * ucyws
      end if
   end if

   ! for the instationary part icw wci
   if (swave > 0 .and. wci > 0) then
      ! we need smoothed water depth and velocities for wci in wave instationary
      if (single_dir > 0) then
         ! maintain consistency with stationary wave directions model
         factime = 1.d0 / (wavint * 2d0) * dts
      else
         ! maintain consistency with boundary conditions smoothing
         factime = 1.d0 / cats / Trep * dts
      end if
      hhwwci = max(factime * hhw + (1 - factime) * hhwwci, epshu)
      umwci = factime * ucx + (1d0 - factime) * umwci
      vmwci = factime * ucy + (1d0 - factime) * vmwci
   end if

end subroutine update_means_wave_flow

subroutine xbeach_compute_wave_velocities(callType, dhdx, dhdy, dudx, dudy, dvdx, dvdy, sinh2kh)
   use m_xbeach_data, only: cwav, cgwav, ctheta, cwav_s, cgwav_s, ctheta_s, sigmwav, costh, sinth, sinth_s, costh_s, Trep, &
                            wci, ucxws, ucyws, umwci, vmwci
   use m_flow, only: ucx, ucy, hs
   use m_flowgeom, only: ndx, ntheta, ntheta_s
   use m_sferic, only: pi
   use m_flowparameters, only: epshu
   use m_physcoef, only: ag

   implicit none

   integer, intent(in) :: callType
   double precision, dimension(ndx), intent(in) :: dhdx, dhdy, dudx, dudy, dvdx, dvdy, sinh2kh

   double precision, dimension(:), allocatable, save :: uwci, vwci

   integer :: itheta, k
   double precision :: cs, sn

   if (wci > 0) then
      if (.not. allocated(uwci)) then
         allocate (uwci(ndx))
         allocate (vwci(ndx))
      end if
   end if

   ! set local variables according to callType
   if (wci > 0) then
      select case (callType)
      case (0) ! stationary
         uwci = ucx
         vwci = ucy
      case (1) ! instationary
         uwci = umwci
         vwci = vmwci
      case (2) ! single_dir
         uwci = ucxws
         vwci = ucyws
      end select
   end if
   !
   select case (callType)
   case (0, 1) ! regular stationary, instationary
      do k = 1, ndx
         if (hs(k) > epshu) then
            if (wci > 0) then ! non-wci: see dispersion
               cgwav(k) = cgwav(k) + hypot(uwci(k), vwci(k)) ! directions taken into account in advection routine
               cwav(k) = cwav(k) + hypot(uwci(k), vwci(k)) ! To check: only in wave direction?
               do itheta = 1, ntheta
                  cs = costh(itheta, k)
                  sn = sinth(itheta, k)
                  ctheta(itheta, k) = sigmwav(k) / sinh2kh(k) * (dhdx(k) * sn - dhdy(k) * cs) + &
                                      (cs * (sn * dudx(k) - cs * dudy(k)) + sn * (sn * dvdx(k) - cs * dvdy(k)))
               end do
            else
               ! cwav, cgwav from dispersion
               do itheta = 1, ntheta
                  cs = costh(itheta, k)
                  sn = sinth(itheta, k)
                  ctheta(itheta, k) = sigmwav(k) / sinh2kh(k) * (dhdx(k) * sn - dhdy(k) * cs)
               end do
            end if
         else
            cgwav(k) = sqrt(ag * epshu)
            cwav(k) = sqrt(ag * epshu)
            ctheta(:, k) = 0d0
         end if
      end do
      ctheta = sign(1.d0, ctheta) * min(abs(ctheta), .5 * pi / Trep)
   case (2) ! single_dir stationary part
      do k = 1, ndx
         if (hs(k) > epshu) then
            if (wci > 0) then
               cgwav_s(k) = cgwav(k) + hypot(uwci(k), vwci(k))
               cwav_s(k) = cwav(k) + hypot(uwci(k), vwci(k))
               do itheta = 1, ntheta_s
                  cs = costh_s(itheta, k)
                  sn = sinth_s(itheta, k)
                  ctheta_s(itheta, k) = sigmwav(k) / sinh2kh(k) * (dhdx(k) * sn - dhdy(k) * cs) + &
                                        (cs * (sn * dudx(k) - cs * dudy(k)) + sn * (sn * dvdx(k) - cs * dvdy(k)))
               end do
            else
               cgwav_s(k) = cgwav(k)
               cwav_s(k) = cwav(k)
               do itheta = 1, ntheta_s
                  cs = costh_s(itheta, k)
                  sn = sinth_s(itheta, k)
                  ctheta_s(itheta, k) = sigmwav(k) / sinh2kh(k) * (dhdx(k) * sn - dhdy(k) * cs)
               end do
            end if
         else
            cgwav_s(k) = sqrt(ag * epshu)
            cwav_s(k) = sqrt(ag * epshu)
            ctheta_s(:, k) = 0d0
         end if
      end do
      ctheta_s = sign(1.d0, ctheta_s) * min(abs(ctheta_s), .5 * pi / Trep)
   end select
end subroutine ! xbeach_compute_wave_velocities

subroutine xbeach_wave_stationary(callType)
   use m_xbeach_data
   use m_flowgeom, only: dtheta, ntheta, dtheta_s, ntheta_s, ndx
   use m_flow, only: ucx, ucy, epshu

   implicit none

   integer, intent(in) :: callType

   integer, parameter :: callTypeStationary = 0
   integer, parameter :: callTypeDirections = 1

   integer :: ierr
   integer :: itheta, k
   integer :: ntheta_local
   double precision :: dtheta_local
   double precision, allocatable, save :: hhwlocal(:)
   double precision, allocatable, save :: ee_local(:, :)

   ierr = 0

   ! directional bin size
   select case (callType)
   case (callTypeStationary)
      ntheta_local = ntheta
   case (callTypeDirections)
      ntheta_local = ntheta_s
   end select

   ! Allocate local arrays
   if (.not. allocated(hhwlocal)) then
      allocate (hhwlocal(1:ndx), ee_local(1:ntheta_local, 1:ndx), stat=ierr)
   end if

   select case (callType)
   case (callTypeStationary)
      hhwlocal = hhw
      ee_local = ee1
      dtheta_local = dtheta
   case (callTypeDirections)
      hhwlocal = hhws
      ee_local = ee_s
      dtheta_local = dtheta_s
   end select

   ! Set slope of the water depth
   call getcellcentergradients(hhwlocal, dhsdx, dhsdy)
   dhsdx = sign(1.d0, dhsdx) * min(abs(dhsdx), 0.1d0)
   dhsdy = sign(1.d0, dhsdy) * min(abs(dhsdy), 0.1d0)

   ! Set slope of the velocities if needed
   if (wci > 0) then
      select case (callType)
      case (callTypeStationary)
         call getcellcentergradients(ucx, xbducxdx, xbducxdy)
         call getcellcentergradients(ucy, xbducydx, xbducydy)
      case (callTypeDirections)
         call getcellcentergradients(ucxws, xbducxdx, xbducxdy)
         call getcellcentergradients(ucyws, xbducydx, xbducydy)
      end select
   else
      xbducxdx = 0.d0
      xbducxdy = 0.d0
      xbducydx = 0.d0
      xbducydy = 0.d0
   end if
   !
   ! Calculate sinh(2kh)
   where (hhwlocal > epshu .and. 2d0 * hhwlocal * kwav <= 3000.d0) ! to check: hhwlocal or hs
      sinh2kh = sinh(min(2d0 * kwav * hhwlocal, 10.0d0))
   elsewhere
      sinh2kh = 3000.d0
   end where
   !
   ! all dry cells have zero energy
   do itheta = 1, ntheta_local
      where (hhwlocal <= epshu)
         ee_local(itheta, :) = 0.d0
      end where
   end do
   where (hhwlocal <= epshu)
      E = 0.d0
      H = 0.d0
   end where
   !
   ! wave directions
   select case (callType)
   case (callTypeStationary)
      do concurrent (k=1:ndx, hhwlocal(k) > epshu)
         thetamean(k) = (sum(ee_local(:, k) * thet(:, k)) / ntheta_local) / &
                        (max(sum(ee_local(:, k)), 0.00001d0) / ntheta_local)
      end do
   case (callTypeDirections)
      do concurrent (k=1:ndx, hhwlocal(k) > epshu)
         thetamean(k) = (sum(ee_local(:, k) * thet_s(:, k)) / ntheta_local) / &
                        (max(sum(ee_local(:, k)), 0.00001d0) / ntheta_local)
      end do
   end select
   !
   ! Compute wave velocities
   select case (callType)
   case (callTypeStationary)
      call xbeach_compute_wave_velocities(0, dhsdx, dhsdy, xbducxdx, xbducxdy, xbducydx, xbducydy, sinh2kh)
   case (callTypeDirections)
      call xbeach_compute_wave_velocities(2, dhsdx, dhsdy, xbducxdx, xbducxdy, xbducydx, xbducydy, sinh2kh)
   end select
   !
   ! Solve wave energy balance, and potentially roller balance
   call xbeach_solve_wave_stationary(callType, ierr)

1234 continue
   return

end subroutine ! xbeach_wave_stationary

! Determine surface forces and body forces for 3D applications
subroutine xbeach_wave_compute_flowforcing3D()
   use m_xbeach_data
   use m_waves
   use m_flowgeom, only: ndx
   use mathconsts, only: degrad
   use fm_external_forcings_data
   use m_alloc

   implicit none

   integer :: k, kb, ki
   double precision :: frc, dir
   double precision, allocatable :: diss(:)

   call realloc(diss, ndx, keepExisting=.false., fill=0d0)

   if (roller == 1) then
      diss = DR
   else
      diss = D
   end if

   ! distribute dissipation
   do k = 1, ndx
      frc = diss(k) * twav(k) / L1(k)
      !
      dir = degrad * phiwav(k) ! cartesian angle, deg
      sxwav(k) = cos(dir) * frc ! fmax limitation done in setwavfu
      sywav(k) = sin(dir) * frc
      !
      sbxwav(k) = Fx_cc(k) - sxwav(k)
      sbywav(k) = Fy_cc(k) - sywav(k)
   end do
   !
   do k = 1, nbndz
      kb = kbndz(1, k); ki = kbndz(2, k)
      sxwav(kb) = sxwav(ki)
      sywav(kb) = sywav(ki)
      sbxwav(kb) = sbxwav(ki)
      sbywav(kb) = sbywav(ki)
   end do
   !
   do k = 1, nbndu
      kb = kbndu(1, k); ki = kbndu(2, k)
      sxwav(kb) = sxwav(ki)
      sywav(kb) = sywav(ki)
      sbxwav(kb) = sbxwav(ki)
      sbywav(kb) = sbywav(ki)
   end do
   !
   call setwavfu() ! fill wavfu using values sx, sbx, sy, sby

1234 continue
   return
end subroutine xbeach_wave_compute_flowforcing3D

subroutine xbeach_compute_stokesdrift()
   use m_xbeach_data, m_xbeach_data_hminlw => hminlw
   use m_flowgeom
   use m_waves
   use m_flow, only: hu
   use m_flowparameters, only: jawavestokes
   use m_physcoef

   implicit none

   integer :: ierr
   integer :: L, k, k1, k2
   double precision, allocatable :: hh(:), uwf(:), vwf(:), ustr(:), urf(:), vrf(:), ustw(:)

   ierr = 0

   allocate (hh(1:ndx), ustw(1:ndx), uwf(1:ndx), vwf(1:ndx), ustr(1:ndx), stat=ierr)
   allocate (urf(1:ndx), vrf(1:ndx), stat=ierr)
   hh = 0d0
   ustw = 0d0
   uwf = 0d0
   vwf = 0d0
   ustr = 0d0
   urf = 0d0
   vrf = 0d0

   if (.not. (trim(instat) == 'stat' .or. trim(instat) == 'stat_table') .and. wci > 0) then
      hh = hhwwci
   else
      hh = hhw
   end if

   ! shortcut to switch off stokes drift
   if (jawavestokes == 0) then
      ustokes = 0d0; vstokes = 0d0
      ustx_cc(k) = 0d0; usty_cc(k) = 0d0 ! output
      return
   end if

   do k = 1, ndx
      if (hh(k) > m_xbeach_data_hminlw) then
         ustw(k) = E(k) / max(cwav(k), 0.01d0) / rhomean / hstokes(k) ! waves
         uwf(k) = ustw(k) * cos(thetamean(k))
         vwf(k) = ustw(k) * sin(thetamean(k))
         if (roller > 0) then
            ustr(k) = 2d0 * R(k) / max(cwav(k), 0.01d0) / rhomean / hstokes(k) ! roller
            urf(k) = ustr(k) * cos(thetamean(k))
            vrf(k) = ustr(k) * sin(thetamean(k))
         end if
      end if
   end do

   do L = 1, lnx ! facenormal decomposition
      if (hu(L) > m_xbeach_data_hminlw) then
         k1 = ln(1, L); k2 = ln(2, L)
         ustokes(L) = acL(L) * (csu(L) * (uwf(k1) + urf(k1)) + snu(L) * (vwf(k1) + vrf(k1))) + &
                      (1d0 - acL(L)) * (csu(L) * (uwf(k2) + urf(k2)) + snu(L) * (vwf(k2) + vrf(k2)))

         vstokes(L) = acL(L) * (-snu(L) * (uwf(k1) + urf(k1)) + csu(L) * (vwf(k1) + vrf(k1))) + &
                      (1d0 - acL(L)) * (-snu(L) * (uwf(k2) + urf(k2)) + csu(L) * (vwf(k2) + vrf(k2)))
      else
         ustokes(L) = 0d0
         vstokes(L) = 0d0
      end if
   end do

1234 continue
   deallocate (ustw, ustr, uwf, vwf, urf, vrf, stat=ierr)
   return
end subroutine
