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
module wrwaq

!include preprocessing flags from autotools
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

   use unstruc_files

   implicit none

contains

   function openwaqbinfile(filename) result(lun)
      character(len=*), intent(in) :: filename !< Output filename.
      integer :: lun

      !
      !           WARNING: WAQ input files must be written using form=binary
      !                    instead of unformatted.
      !                    Although it is not standard Fortran
      !
#ifdef HAVE_FC_FORM_BINARY
      open (newunit=lun, file=filename, form='binary', status='replace')
#else
      ! standardized way if binary is not available
      open (newunit=lun, file=filename, form='unformatted', access='stream', status='replace')
#endif
      call reg_file_open(lun, filename)
      call mess(LEVEL_INFO, 'Opened file : ', filename)
   end function openwaqbinfile

   function openasciifile(filename) result(lun)

      character(len=*), intent(in) :: filename
      integer :: lun
      !
      !           NOTE: Opens a simple ASCII-file. Function is intended only
      !                 to isolate newlun_nogdp dependency.
      !
#ifdef HAVE_FC_FORM_BINARY
      open (newunit=lun, file=filename, SHARED)
#else
      open (newunit=lun, file=filename, access='stream')
#endif
      call reg_file_open(lun, filename)
      call mess(LEVEL_INFO, 'Opened file : ', filename)
   end function openasciifile
!
!------------------------------------------------------------------------------

!> Write ASCII or binary pointer file for WAQ.
   subroutine wrwaqpoi(ifrmto, num_exchanges, filename, ascii)
      implicit none
      integer, intent(in) :: num_exchanges !< Nr. of linkages (pointers) between computational cells.
      integer, dimension(:, :), intent(in) :: ifrmto !< Pointer table with all linkages.
                                                   !! ifrmto(1,:) = 'from'   cell number
                                                   !! ifrmto(2,:) = 'to'     cell number
                                                   !! ifrmto(3,:) = 'from-1' cell number
                                                   !! ifrmto(4,:) = 'to+1'   cell number
      logical, intent(in) :: ascii !< Produce ascii file or not (then binary).
      character(*), intent(in) :: filename !< Name for output pointer file.
      !
      !           Local variables
      !
      integer :: i
      integer :: lunout
      integer :: q
      !
   !! executable statements -------------------------------------------------------
      !
      if (ascii) then
         !
         ! ascii output
         !
         lunout = openasciifile(filename)
         do q = 1, num_exchanges
            write (lunout, '(4i10)') (ifrmto(i, q), i=1, 4)
         end do
         close (lunout)
      else
         !
         ! binary output
         !
         lunout = openwaqbinfile(filename)
         write (lunout) ((ifrmto(i, q), i=1, 4), q=1, num_exchanges)
         close (lunout)
      end if
   end subroutine wrwaqpoi
!
!------------------------------------------------------------------------------

!> Write (binary) from/to length file for DelWAQ.
   subroutine wrwaqlen(num_exchanges, lenex, filename, ascii)
      use precision
      !
      implicit none
      !
      !           Global variables
      !
      integer, intent(in) :: num_exchanges !< Nr. of linkages (pointers) between computational cells.
      double precision, intent(in) :: lenex(2, num_exchanges) !< Dispersion half-lengths of computational cells, segment
   !! centre to exchange point. (2 values: from/to direction)
      logical, intent(in) :: ascii !< Produce ascii file or not (then binary).
      character(*), intent(in) :: filename !< Output filename.
      !
      !           Local variables
      !
      integer :: i
      integer :: lunout
      integer :: q
      !
   !! executable statements -------------------------------------------------------
      !
      if (ascii) then
         !
         ! ascii output
         !
         lunout = openasciifile(filename)
         do q = 1, num_exchanges
            write (lunout, '(i10,2f18.8)') q, (lenex(i, q), i=1, 2)
         end do
         close (lunout)
      else
         !
         ! binary output
         !
         lunout = openwaqbinfile(filename)
         write (lunout) num_exchanges
         write (lunout) ((real(lenex(i, q), sp), i=1, 2), q=1, num_exchanges)
         close (lunout)
      end if
   end subroutine wrwaqlen
!
!------------------------------------------------------------------------------

!> Write (binary) horizontal surface file for DelWAQ.
   subroutine wrwaqsrf(srf, nosegl, num_layers, filename, ascii)
      use precision
      implicit none
      !
      !           Global variables
      !
      integer, intent(in) :: nosegl !< Nr. of segment per layer.
      integer, intent(in) :: num_layers !< Nr. of layers.
      double precision, dimension(:), intent(in) :: srf !< Horizontal surfaces of computational cells. (size nosegl)
      logical, intent(in) :: ascii !< Produce ascii file or not (then binary).
      character(*), intent(in) :: filename !< Output filename.
      character(256) :: fileold !< Old output filename.
      !
      !           Local variables
      !
      integer :: is, il
      integer :: lunout
      !
   !! executable statements -------------------------------------------------------
      !
      if (ascii) then
         !
         ! ascii output
         !
         lunout = openasciifile(filename)
         ! nmax, mmax, nosegl, nosegl, nosegl, 0.0
         ! num_cells in all places is a dummy: no aggr yet, and no three m-n-k dimensions
         write (lunout, '(i10)') 0
         do il = 1, num_layers
            do is = 1, nosegl
               write (lunout, '(i10,f18.8)') is + (il - 1) * nosegl, srf(is)
            end do
         end do
         close (lunout)
      else
         !
         ! binary output
         !
         ! new style surf file for NGHS WAQ UI
         lunout = openwaqbinfile(filename)
         write (lunout) 0
         write (lunout) ((real(srf(is), sp), is=1, nosegl), il=1, num_layers)
         close (lunout)

         ! temporaraly keep writing then old style surf file for WAQ_GUI
         fileold = trim(filename)//'old'
         lunout = openwaqbinfile(fileold)
         write (lunout) nosegl, 1, nosegl, nosegl, nosegl, 0.0
         write (lunout) (real(srf(is), sp), is=1, nosegl)
         close (lunout)
      end if
   end subroutine wrwaqsrf
!
!------------------------------------------------------------------------------

!> Write ASCII attributes file for WAQ.
   subroutine wrwaqatr(nosegl, num_layers, kmk1, kmk2, filename)
      implicit none
      integer, intent(in) :: nosegl !< Nr. of segments per layer
      integer, intent(in) :: num_layers !< Nr. of layers
      integer, dimension(:), intent(in) :: kmk1 !< First WAQ segment features at start of calculation
      integer, dimension(:), intent(in) :: kmk2 !< Second WAQ segment features at start of calculation
      character(*), intent(in) :: filename !< Name for output pointer file.
      !
      !           Local variables
      !
      integer :: il, is
      integer :: lunatr
      character(2) kenout(nosegl) !!  this is now allocated on the stack !!!
      !
   !! executable statements -------------------------------------------------------
      !
      call newfil(lunatr, trim(filename))
      write (lunatr, '(a)') '         ; DELWAQ_COMPLETE_ATTRIBUTES'
      write (lunatr, '(a)') '    2    ; two blocks with input     '
      write (lunatr, '(a)') '    1    ; number of attributes, they are :'
      write (lunatr, '(a)') '    1    ;  ''1'' is active ''0'' is not'
      write (lunatr, '(a)') '    1    ; data follows in this file '
      write (lunatr, '(a)') '    1    ; all data is given without defaults'
      do il = 1, num_layers
         write (lunatr, *) '  ;    layer: ', il
         do is = 1, nosegl
            kenout(is) = '  '
            write (kenout(is), '(I2)') kmk1(is + (il - 1) * nosegl)
         end do
         write (lunatr, '(500a2)') kenout
      end do
      write (lunatr, '(a)') '    1    ; number of attributes, they are :'
      write (lunatr, '(a)') '    2    ;  ''1'' has surface ''3'' has bottom'
      write (lunatr, '(a)') '         ;  ''0'' has both    ''2'' has none  '
      write (lunatr, '(a)') '    1    ; data follows in this file '
      write (lunatr, '(a)') '    1    ; all data is given without defaults'
      do il = 1, num_layers
         write (lunatr, *) '  ;    layer: ', il
         do is = 1, nosegl
            kenout(is) = '  '
            write (kenout(is), '(I2)') kmk2(is + (il - 1) * nosegl)
         end do
         write (lunatr, '(500a2)') kenout
      end do
      write (lunatr, '(a)') '    0    ; no time dependent attributes'
      close (lunatr)
   end subroutine wrwaqatr
!
!------------------------------------------------------------------------------

!> Write (binary) exchange file(s) for DelWAQ: area and fluxes.
   subroutine wrwaqbin(itim, quant, nquant, filename, ascii, lunout)
      use precision
      implicit none
      !
      !           Global variables
      !
      integer, intent(in) :: itim !< Time for new data block
      integer, intent(in) :: nquant !< Size of quant(ity) array.
      double precision, dimension(:), intent(in) :: quant !< Quantity array to be written.
      logical, intent(in) :: ascii !< Produce ascii file or not (then binary).
      character(*), intent(in) :: filename !< Output filename (only used if lunout not connected yet).
      integer, intent(inout) :: lunout !< File pointer for output file. Used if already connected,
   !! or set to new value for filename.
      !
      !           Local variables
      !
      integer :: q
      !
   !! executable statements -------------------------------------------------------
      !
      if (ascii) then
         !
         ! ascii output
         !
         if (lunout == 0) then
            lunout = openasciifile(filename)
         end if
         write (lunout, '(a,i10)') 'Time = ', itim
         do q = 1, nquant
            write (lunout, '(i10,f18.8)') q, quant(q)
         end do
         !close(lunout)
      else
         !
         ! binary output
         !
         if (lunout == 0) then
            lunout = openwaqbinfile(filename)
         end if
         write (lunout) itim
         write (lunout) (real(quant(q), sp), q=1, nquant)
         !close(lunout)
      end if
   end subroutine wrwaqbin
!
!------------------------------------------------------------------------------

end module wrwaq

!> Module for coupling with WAQ.
!! Currently only writing of WAQ-files.
module waq
   use unstruc_messages
   use m_getkbotktopmax

   implicit none

   logical, parameter :: waq_format_ascii = .false. !< For debugging: .true. produces ascii files. .false. the binary files used for DELWAQ input.
   type gd_waqpar
      integer :: aggre !  0: no aggregation (=active cells only in FM), 1: aggregation according to content of flhoraggr
      integer :: aggrel !  0: no layer aggregation, 1: layer aggregation
      integer :: lunvol !  file unit number to an output file
      integer :: lunvel !  file unit number to an output file
      integer :: lunare !  file unit number to an output file
      integer :: lunflo !  file unit number to an output file
      integer :: lunsal !  file unit number to an output file
      integer :: luntem !  file unit number to an output file
      integer :: lunvdf !  file unit number to an output file
      integer :: luntau !  file unit number to an output file
      integer :: num_cells !  number of WAQ segments
      integer :: nosegl !  number of WAQ segments per layer
      integer :: num_exchanges !  total number of WAQ exchanges
      integer :: noql !  number of horizontal WAQ exchanges per layer
      integer :: noq12 !  number of horizontal WAQ exchanges (excluding sink/sources and laterals)
      integer :: noq12s !  number of horizontal WAQ exchanges (including sink/sources, excluding laterals)
      integer :: noq12sl !  number of horizontal WAQ exchanges (including sink/sources and laterals)
      integer :: numsrcbnd !  number of sinks/sources that are boundary conditions
      integer :: numsrcwaq !  number of adition sources/sinks exchanges in waq (based on posible combinations)
      integer :: numlatsectwaq !  number of lateral boundary sections
      integer, allocatable :: numlatinsectwaq(:) !  number of lateral boundaries in this section
      integer :: numlatwaq !  number of lateral boundary links
      integer :: kmxnx ! maximum number of active layers
      integer :: kmxnxa ! maximum number of aggregated layers
      integer :: ndkxi ! nr of internal flowcells (3D)
      integer, allocatable :: ilaggr(:) ! layer aggregation pointer
      integer, allocatable :: ilaggrhyd(:) ! layer aggregation table for hyd-file
      integer, allocatable :: isaggr(:) ! segment aggregation pointer
      integer, allocatable :: iapnt(:) ! flow-to-waq pointer (mapping of flow cells to waq cells)
      integer, allocatable :: iqaggr(:) ! exchange aggregation pointer
      integer, allocatable :: iqwaggr(:) ! exchange aggregation pointer for the vertical
      integer, allocatable :: ifrmto(:, :) ! from-to pointer table
      integer, allocatable :: ifrmtosrc(:, :) ! from-to pointer table for sources
      integer, allocatable :: ifrmtolat(:, :) ! from-to pointer table for sources
      integer, allocatable :: ifsmin(:) ! first active layer (counting from the top) (z-model)
      integer, allocatable :: ifsmax(:) ! maximum active layer (counting from the top) (z-model)
      integer, allocatable :: nosega(:) ! no of segments aggregated into WAQ segments
      integer, allocatable :: kmk1(:) ! First WAQ segment features at start of calculation (1 is active 0 is not)
      integer, allocatable :: kmk2(:) ! Second WAQ segment features at start of calculation (1 surface, 3 bottom, 0 both, 2 neither)
      double precision, allocatable :: horsurf(:) ! horizontal surfaces of segments
      double precision, allocatable :: vol(:) ! WAQ (aggregated) volumes
      double precision, allocatable :: vel(:) ! WAQ (aggregated) velocities
      double precision, allocatable :: sal(:) ! WAQ (aggregated) salinity
      double precision, allocatable :: tem(:) ! WAQ (aggregated) temperature
      double precision, allocatable :: tau(:) ! WAQ (aggregated) taus
      double precision, allocatable :: vdf(:) ! WAQ (aggregated) vertical diffusion
      double precision, allocatable :: qag(:) ! WAQ (aggregated) flux
      double precision, allocatable :: area(:) ! WAQ (aggregated) exchange areas
      character(256) :: flhoraggr !  Name of input aggregation file
      character(256) :: flvertaggr !  Name of input aggregation file
   end type gd_waqpar

   type(gd_waqpar) :: waqpar ! all waq data
   logical :: horaggr ! horizontal aggregation is on
   logical :: vertaggr ! vertical aggregation is on

   !-- Start subroutines -------------
contains

   subroutine reset_waq()
!
!! executable statements -------------------------------------------------------
!
      implicit none

      call close_and_reset(waqpar%lunvol)
      call close_and_reset(waqpar%lunvel)
      call close_and_reset(waqpar%lunsal)
      call close_and_reset(waqpar%luntem)
      call close_and_reset(waqpar%lunare)
      call close_and_reset(waqpar%lunflo)
      call close_and_reset(waqpar%luntau)
      call close_and_reset(waqpar%lunvdf)

   contains

      subroutine close_and_reset(lun)
         implicit none
         integer, intent(inout) :: lun
         if (lun > 0) then
            call doclose(lun)
         end if
         lun = 0
      end subroutine close_and_reset

   end subroutine reset_waq
!
!------------------------------------------------------------------------------

!> Write the coupled hydrodynamics information file (.hyd)
   subroutine waq_wri_hyd()
      use dflowfm_version_module, only: getfullversionstring_dflowfm
      use unstruc_files
      use m_flowparameters
      use m_flowtimes
      use m_flow
      use fm_external_forcings_data
      use m_flowgeom
      use unstruc_model
      use time_module, only: ymd2jul
      use m_dateandtimenow

      implicit none
      !
      !           Local variables
      !
      integer :: lunhyd
      character(len=255) :: filename, stmp
      character(len=160) :: version_id
      character tex * 80, datetime * 20
      integer :: i, ibnd, isrc, kk1, kk2
      integer :: itdate, julday, idatum, itijd, iyea, imon, iday, ihou, imin, isec
      double precision :: anl
      double precision :: x1, y1, x2, y2
      double precision, parameter :: rmissval = -999.0d0
      !
   !! executable statements -------------------------------------------------------
      !
      call getfullversionstring_dflowfm(version_id)

      filename = defaultFilename('hyd')
      call newfil(lunhyd, trim(filename))
      call dateandtimenow(iyea, imon, iday, ihou, imin, isec)
      write (datetime, '(i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)') iyea, '-', imon, '-', iday, ', ', ihou, ':', imin, ':', isec
      tex = 'file-creation-date  '//datetime
      write (lunhyd, '(a/a)') 'file-created-by  '//trim(version_id), tex

      write (lunhyd, '(a,a)') 'task      ', 'full-coupling'

      if (layertype == LAYTP_SIGMA) then ! all sigma layers
         write (lunhyd, '(a,a)') 'geometry  ', 'unstructured'
      elseif (layertype == LAYTP_Z) then ! all z layers
         write (lunhyd, '(a,a)') 'geometry  ', 'unstructured z-layers'
      elseif (layertype == LAYTP_LEFTSIGMA) then ! mixed sigma/z layers
         write (lunhyd, '(a,a)') 'geometry  ', 'unstructured left-sigma-layers'
      elseif (layertype == LAYTP_LEFTZ) then ! mixed sigma/z layers
         write (lunhyd, '(a,a)') 'geometry  ', 'unstructured left-z-layers'
      else ! other?
         write (lunhyd, '(a,a)') 'geometry  ', 'unstructured other'
      end if

      !if ( ! TODO: include check on aggregation
      if (waqpar%aggre <= 0) then
         write (lunhyd, '(a,a)') 'horizontal-aggregation ', 'no'
      else
         write (lunhyd, '(a,a)') 'horizontal-aggregation ', 'yes'
      end if

   !! Time block
      read (refdat, '(i8)') itdate
      julday = ymd2jul(itdate)
      write (lunhyd, '(A,I8,A )') 'reference-time              ''', itdate, '000000'''
      call timdat(julday, tstart_user, idatum, itijd)
      write (lunhyd, '(A,I8,I6.6,A)') 'hydrodynamic-start-time     ''', idatum, itijd, ''''
      call timdat(julday, tstop_user, idatum, itijd)
      write (lunhyd, '(A,I8,I6.6,A)') 'hydrodynamic-stop-time      ''', idatum, itijd, ''''
      call timdat(julday, ti_waq, idatum, itijd) ! TODO: niet ti_waq maar een soort comp.dt
      write (lunhyd, '(A,I6.6,A )') 'hydrodynamic-timestep       ''00000000', itijd, ''''
      write (lunhyd, '(A,I8,A )') 'conversion-ref-time         ''', itdate, '000000'''
      call timdat(julday, ti_waqs, idatum, itijd)
      write (lunhyd, '(A,I8,I6.6,A)') 'conversion-start-time       ''', idatum, itijd, ''''
      call timdat(julday, ti_waqe, idatum, itijd)
      write (lunhyd, '(A,I8,I6.6,A)') 'conversion-stop-time        ''', idatum, itijd, ''''
      call timdat(julday, ti_waq, idatum, itijd)
      idatum = idatum - itdate
      write (lunhyd, '(A,I8.8,I6.6,A )') 'conversion-timestep         ''', idatum, itijd, ''''

   !! Grid dimensions block
      write (lunhyd, '(a,i10)') 'grid-cells-first-direction              ', waqpar%nosegl
      write (lunhyd, '(a,i10)') 'grid-cells-second-direction             ', 1
      write (lunhyd, '(a,i10)') 'number-hydrodynamic-segments-per-layer  ', ndxi
      write (lunhyd, '(a,i10)') 'number-hydrodynamic-layers              ', waqpar%kmxnx
      write (lunhyd, '(a,i10)') 'number-water-quality-segments-per-layer ', waqpar%nosegl
      write (lunhyd, '(a,i10)') 'number-water-quality-layers             ', waqpar%kmxnxa
      write (lunhyd, '(a,i10)') 'number-horizontal-exchanges             ', waqpar%noq12sl ! Including open boundaries and laterals
      if (waqpar%kmxnxa > 1) then
         write (lunhyd, '(a,i10)') 'number-vertical-exchanges               ', waqpar%num_exchanges - waqpar%noq12sl
      else
         write (lunhyd, '(a,i10)') 'number-vertical-exchanges               ', 0
      end if

      write (lunhyd, '(A,A    )') 'hydrodynamic-file           ', ''''//trim(md_ident)//''''
      if (horaggr) then
         write (lunhyd, '(A,A    )') 'aggregation-file            ', ''''//trim(waqpar%flhoraggr)//''''
      else
         write (lunhyd, '(A,A    )') 'aggregation-file            ', 'none'
      end if
      if (vertaggr) then
         write (lunhyd, '(A,A    )') 'vertical-aggregation-file   ', ''''//trim(waqpar%flvertaggr)//''''
      else
         write (lunhyd, '(A,A    )') 'vertical-aggregation-file   ', 'none'
      end if

      ! Grid and boundary conditions
      write (lunhyd, '(a,a)') 'boundaries-file             ', ''''//trim(defaultFilename('bnd', prefixWithDirectory=.false.))//''''
      write (lunhyd, '(a,a)') 'waqgeom-file                ', ''''//trim(defaultFilename('waqgeom', prefixWithDirectory=.false.))//''''

   !! MJ: strange, the real grid-indices-file is the lga file!!, not the bnd-file....       'com-tut_fti_waq.lga'
   !! MJ: keep this until the old WAQ_GUI is updated
      write (lunhyd, '(a,a)') 'grid-indices-file           ', ''''//trim(defaultFilename('bnd', prefixWithDirectory=.false.))//''''
      write (lunhyd, '(a,a)') 'grid-coordinates-file       ', ''''//trim(defaultFilename('waqgeom', prefixWithDirectory=.false.))//''''

   !! Coupling files block
      write (lunhyd, '(a,a)') 'volumes-file                ', ''''//trim(defaultFilename('vol', prefixWithDirectory=.false.))//''''
      write (lunhyd, '(a,a)') 'areas-file                  ', ''''//trim(defaultFilename('are', prefixWithDirectory=.false.))//''''
      write (lunhyd, '(a,a)') 'flows-file                  ', ''''//trim(defaultFilename('flo', prefixWithDirectory=.false.))//''''
      write (lunhyd, '(a,a)') 'velocities-file             ', ''''//trim(defaultFilename('vel', prefixWithDirectory=.false.))//''''
      write (lunhyd, '(a,a)') 'pointers-file               ', ''''//trim(defaultFilename('poi', prefixWithDirectory=.false.))//''''
      write (lunhyd, '(a,a)') 'lengths-file                ', ''''//trim(defaultFilename('len', prefixWithDirectory=.false.))//''''

      stmp = ' '
      if (jasal > 0) then
         stmp = ''''//trim(defaultFilename('sal', prefixWithDirectory=.false.))//''''
      else
         stmp = 'none'
      end if
      write (lunhyd, '(a,a)') 'salinity-file               ', trim(stmp)

      stmp = ' '
      if (jatem > 0) then
         stmp = ''''//trim(defaultFilename('tem', prefixWithDirectory=.false.))//''''
      else
         stmp = 'none'
      end if
      write (lunhyd, '(a,a)') 'temperature-file            ', trim(stmp)

      if (waqpar%kmxnx == 1) then
         write (lunhyd, '(a,a)') 'vert-diffusion-file         ', 'none'
      else
         write (lunhyd, '(a,a)') 'vert-diffusion-file         ', ''''//trim(defaultFilename('vdf', prefixWithDirectory=.false.))//''''
      end if
      ! new style surf file for NGHS WAQ UI
      write (lunhyd, '(a,a)') 'horizontal-surfaces-file    ', ''''//trim(defaultFilename('srf', prefixWithDirectory=.false.))//''''
      ! temporaraly keep writing then old style surf file for WAQ_GUI
      write (lunhyd, '(a,a)') 'surfaces-file               ', ''''//trim(defaultFilename('srf', prefixWithDirectory=.false.))//'old'''
      !discharges-file          'com-f34.src'
      !chezy-coefficients-file  'com-f34.chz'
      write (lunhyd, '(a,a)') 'shear-stresses-file         ', ''''//trim(defaultFilename('tau', prefixWithDirectory=.false.))//''''
      !walking-discharges-file  'com-f34.wlk'
      !minimum-vert-diffusion
      !   upper-layer       0.0000E+00
      !   lower-layer       0.0000E+00
      !   interface-depth   0.0000E+00
      !end-minimum-vert-diffusion
      !constant-dispersion
      !   first-direction    0.0000E+00
      !   second-direction   0.0000E+00
      !   third-direction    0.0000E+00
      !end-constant-dispersion
      write (lunhyd, '(a,a)') 'attributes-file             ', ''''//trim(defaultFilename('atr', prefixWithDirectory=.false.))//''''

      if (layertype == LAYTP_Z) then ! all z layers
         !       do j = 1, mxlaydefs
         !         do k = 0, laymx(j)
         !            write ( 1972, '(I,I,F15.6)') j, k, zslay(k, j)
         !         enddo
         !       enddo
         if (mxlaydefs == 1) then
            write (lunhyd, '(A, F15.6 )') 'z-layers-ztop ', zslay(laymx(1), 1)
            write (lunhyd, '(A, F15.6 )') 'z-layers-zbot ', zslay(0, 1)
         else
            ! we do not yet know what to do with multiple defintions of z-layers
            write (lunhyd, '(A        )') 'z-layers-ztop multi-layerdefinition '
            write (lunhyd, '(A        )') 'z-layers-zbot multi-layerdefinition '
         end if
      elseif (layertype /= LAYTP_SIGMA) then ! all other options
         ! we do not yet know what to do with z/sigma combinations
         write (lunhyd, '(A        )') 'z-layers-ztop mixed-layerdefinition '
         write (lunhyd, '(A        )') 'z-layers-zbot mixed-layerdefinition '
      end if
      write (lunhyd, '(A      )') 'hydrodynamic-layers'
      do i = 1, waqpar%kmxnx
         write (lunhyd, '(F15.6  )') 1.0e0 / real(waqpar%kmxnx)
      end do
      write (lunhyd, '(A      )') 'end-hydrodynamic-layers'
      write (lunhyd, '(A      )') 'water-quality-layers   '
      anl = 1.000
      do i = 1, waqpar%kmxnxa
         write (lunhyd, '(F15.6  )') real(waqpar%ilaggrhyd(i))
      end do
      write (lunhyd, '(A      )') 'end-water-quality-layers'
      !discharges
      !    2   14    1   '(14,2)'
      !end-discharges
      if (numsrc > 0) then
         ibnd = 0
         if (nopenbndsect > 0) ibnd = nopenbndlin(nopenbndsect)
         write (lunhyd, '(A      )') 'sink-sources'
         do isrc = 1, numsrc
            kk1 = ksrc(1, isrc)
            kk2 = ksrc(4, isrc)
            if ((kk1 == 0 .and. kk2 > 0) .or. &
                (kk2 == 0 .and. kk1 > 0) .or. &
                (kk1 > 0 .and. kk2 > 0)) then
               ! include boundaries when one of the cells is external and the other in the domain, or both are in this domain
               x1 = rmissval
               y1 = rmissval
               x2 = rmissval
               y2 = rmissval
               if (kk1 > 0) then
                  x1 = xz(kk1)
                  y1 = yz(kk1)
               else if (kk1 == 0) then
                  ibnd = ibnd + 1
                  kk1 = -ibnd
               end if
               if (kk2 > 0) then
                  x2 = xz(kk2)
                  y2 = yz(kk2)
               else if (kk1 == 0) then
                  ibnd = ibnd + 1
                  kk2 = -ibnd
               end if
               write (lunhyd, '(3I10,4F18.6,2X,A)') isrc, kk1, kk2, x1, y1, x2, y2, trim(srcname(isrc))
            end if
         end do
         write (lunhyd, '(A      )') 'end-sink-sources'
      end if

      call doclose(lunhyd)

   end subroutine waq_wri_hyd
!
!------------------------------------------------------------------------------

!> Write flow geometry information for use in WAQ-GUI.
   subroutine waq_wri_geom()
      use unstruc_netcdf
      use unstruc_files
      use unstruc_model, only: md_unc_conv
      implicit none
      !
      !           Local variables
      !
      character(len=255) :: filename
      !
   !! executable statements -------------------------------------------------------
      !
      filename = defaultFilename('waqgeom')

      if (md_unc_conv == UNC_CONV_UGRID) then
         ! Write UGRID format file (this supports aggregation of mesh geometry).
         call waq_write_waqgeom_ugrid(filename)
      else
         ! Write old CF format file (this does not support aggregation of mesh geometry).
         call unc_write_net_flowgeom(filename)
      end if

      ! add segment aggregation table to this file! label with delwaq_role = "segment_aggregation_table" and a "segdim"
      !    ierr = unc_open(filename, nf90_write, inetfile); call check_error(ierr, 'file '''//trim(filename)//'''')
      !    if (nerr_ > 0) return
      ! write waqpar%iapnt(1:ndxi) to a new table

   end subroutine waq_wri_geom

!> Writes the (possibly aggregated) unstructured network and edge type to a netCDF file for DelWAQ.
!! If file exists, it will be overwritten.
   subroutine waq_write_waqgeom_ugrid(filename)
      use unstruc_netcdf

      implicit none

      character(len=*), intent(in) :: filename

      integer :: igeomfile, ierr

      ierr = unc_create(filename, 0, igeomfile)
      if (ierr /= nf90_noerr) then
         call mess(LEVEL_ERROR, 'Could not create waq geometry file '''//trim(filename)//'''.')
         call check_error(ierr)
         return
      end if

      call waq_write_waqgeom_filepointer_ugrid(igeomfile)

      ierr = unc_close(igeomfile)
      call check_error(ierr)
   end subroutine waq_write_waqgeom_ugrid

!> Writes the (possibly aggregated) unstructured network and edge type to an already opened netCDF dataset for DelWAQ.
   subroutine waq_write_waqgeom_filepointer_ugrid(igeomfile)
      use io_ugrid
      use network_data, only: numl1d
      use m_flowgeom, only: ndxi, ndx2d
      use unstruc_netcdf
      use m_partitioninfo, only: jampi, idomain, iglobal_s
      use m_alloc
      use m_wall_clock_time

      implicit none

      integer, intent(in) :: igeomfile !< file pointer to netcdf file to write to.

      character(len=255) :: message !< Temporary variable for writing log messages.
      type(t_ug_meshgeom) :: meshgeom, aggregated_meshgeom !< Mesh geometry to be written to the NetCDF file.
      type(t_ug_mesh) :: meshids !< Set of NetCDF-ids for all mesh geometry variables.
      type(t_ug_network) :: networkids !< Set of NetCDF-ids for all network variables
      type(t_unc_mapids) :: geomids !< Set of file and variable ids for this file.
      integer, dimension(:), allocatable :: edge_type, aggregated_edge_type !< Edge type array to be written to the NetCDF file.
      integer :: ierr !< Result status (UG_NOERR==NF90_NOERR if successful).
      logical :: success !< Helper variable.
      double precision :: startTime, endTime !< Timers.

      ierr = UG_NOERR

      ! Add global attributes to NetCDF file.
      ierr = ug_addglobalatts(igeomfile, ug_meta_fm)
      call check_error(ierr)

      if (ndx2d > 0) then
         ! Get mesh geometry and edge type array.
         ierr = create_ugrid_geometry(meshgeom, edge_type)
         call check_error(ierr)

         ! Aggregate mesh geometry, if needed.
         if (waqpar%aggre == 1) then
            ! Create empty meshgeom.
            ierr = t_ug_meshgeom_destructor(aggregated_meshgeom)
            aggregated_meshgeom%meshName = 'mesh2d'
            aggregated_meshgeom%start_index = 1
            call check_error(ierr)

            ! Aggregate.
            call wall_clock_time(startTime)
            success = aggregate_ugrid_geometry(meshgeom, aggregated_meshgeom, edge_type, aggregated_edge_type, waqpar%iapnt)
            call wall_clock_time(endTime)
            if (success) then ! If no errors occurred.
               write (message, "('Aggregated grid for waq geometry file, elapsed time: ', F10.3, ' s.')") endTime - startTime
               call mess(LEVEL_INFO, trim(message))

               ! Replace meshgeom and edge_type variables with their aggregated counterparts.
               meshgeom = aggregated_meshgeom
               call realloc(edge_type, size(aggregated_edge_type))
               edge_type = aggregated_edge_type
            end if

            !TODO deallocate aggregated_meshgeom
            if (allocated(aggregated_edge_type)) deallocate (aggregated_edge_type)
         end if

         ! Write mesh geometry.
         ierr = ug_write_mesh_struct(igeomfile, meshids, networkids, crs, meshgeom) ! NOTE: UNST-5477: this call is not valid yet for 3D models with ocean_sigma_z combined layering
         call check_error(ierr)

         ! Write edge type variable (this is an extra variable that is not part of the UGRID standard).
         call write_edge_type_variable(igeomfile, meshids, meshgeom%meshName, edge_type)
         deallocate (edge_type)

         ! when in mpi mode, add face domain numbers and global face numbers
         if (jampi == 1) then
            ! face domain numbers
            call write_face_domain_number_variable(igeomfile, meshids, meshgeom%meshName, idomain(1:ndxi))
            ! global face numbers
            call write_face_global_number_variable(igeomfile, meshids, meshgeom%meshName, iglobal_s(1:ndxi))
         end if
      end if

      if (numl1d > 0) then
         ! number of 1D links (numl1d) if greater than zero
         ! use the generic routine to write the 1D grid, the 2D grid might need extra data, and contains extra info.
         call unc_write_flowgeom_filepointer_ugrid(igeomfile, geomids%id_tsp, ja2D=.false.)
      end if

   end subroutine waq_write_waqgeom_filepointer_ugrid

!> Writes the given face domain number to the given netcdf file.
   subroutine write_face_domain_number_variable(igeomfile, meshids, meshName, idomain)

      use io_ugrid

      implicit none

      integer, intent(in) :: igeomfile !< file pointer to netcdf file to write to.
      type(t_ug_mesh), intent(inout) :: meshids !< Set of NetCDF-ids for all mesh geometry variables.
      character(len=*), intent(in) :: meshName !< Name of the mesh.
      integer, intent(in) :: idomain(:) !< Face domainnumber variable to be written to the NetCDF file.

      integer :: id_facedomainnumber !< Variable ID for face domain number variable.
      integer :: was_in_define_mode
      integer :: ierr !< Result status (UG_NOERR==NF90_NOERR if successful).

      ierr = UG_NOERR

      ! Put netcdf file in define mode.
      was_in_define_mode = 0
      ierr = nf90_redef(igeomfile)
      if (ierr == nf90_eindefine) then
         was_in_define_mode = 1 ! If was still in define mode.
      end if
      ierr = UG_NOERR

      ! Define face domain number variable.
      ierr = ug_def_var(igeomfile, id_facedomainnumber, (/meshids%dimids(mdim_face)/), nf90_int, UG_LOC_FACE, &
                        meshName, 'face_domain_number', '', 'Face partition domain number', '', '', '', ifill=-999)

      ! Put netcdf file in write mode.
      ierr = nf90_enddef(igeomfile)

      ! Write domain number variable.
      ierr = nf90_put_var(igeomfile, id_facedomainnumber, idomain)

      ! Leave the dataset in the same mode as we got it.
      if (was_in_define_mode == 1) then
         ierr = nf90_redef(igeomfile)
      end if

   end subroutine write_face_domain_number_variable

!> Writes the given global face number to the given netcdf file.
   subroutine write_face_global_number_variable(igeomfile, meshids, meshName, iglobal_s)

      use io_ugrid

      implicit none

      integer, intent(in) :: igeomfile !< file pointer to netcdf file to write to.
      type(t_ug_mesh), intent(inout) :: meshids !< Set of NetCDF-ids for all mesh geometry variables.
      character(len=*), intent(in) :: meshName !< Name of the mesh.
      integer, intent(in) :: iglobal_s(:) !< Global face number variable to be written to the NetCDF file.

      integer :: id_faceglobalnumber !< Variable ID for global face number variable.
      integer :: was_in_define_mode
      integer :: ierr !< Result status (UG_NOERR==NF90_NOERR if successful).

      ierr = UG_NOERR

      ! Put netcdf file in define mode.
      was_in_define_mode = 0
      ierr = nf90_redef(igeomfile)
      if (ierr == nf90_eindefine) then
         was_in_define_mode = 1 ! If was still in define mode.
      end if
      ierr = UG_NOERR

      ! Define global face number variable.
      ierr = ug_def_var(igeomfile, id_faceglobalnumber, (/meshids%dimids(mdim_face)/), nf90_int, UG_LOC_FACE, &
                        meshName, 'face_global_number', '', 'Global face number (as it was in the full grid, before partitioning)', '', '', '', ifill=-999)

      ! Put netcdf file in write mode.
      ierr = nf90_enddef(igeomfile)

      ! Write global face number variable.
      ierr = nf90_put_var(igeomfile, id_faceglobalnumber, iglobal_s)

      ! Leave the dataset in the same mode as we got it.
      if (was_in_define_mode == 1) then
         ierr = nf90_redef(igeomfile)
      end if
   end subroutine write_face_global_number_variable

!> Creates and initializes mesh geometry that contains the 2D (layered) unstructured network and edge type array.
!!
!! NOTE: do not pass already filled mesh geometries to this function,
!! since array pointers will become disassociated, possibly causing memory leaks.
   function create_ugrid_geometry(meshgeom, edge_type) result(ierr)
      use network_data
      use m_flow
      use io_ugrid
      use m_flowgeom, only: ndx2d
      use unstruc_netcdf, only: check_error, get_2d_edge_data
      use m_missing
      use m_alloc

      implicit none

      type(t_ug_meshgeom), intent(out) :: meshgeom !< The mesh geometry that is to be created and filled.
      integer, dimension(:), allocatable, intent(out) :: edge_type !< Edge type array that is to be created and filled.

      integer, dimension(:), allocatable :: edge_mapping_table, reverse_edge_mapping_table !< Mapping tables.
      integer :: node, edge, face, maxNodesPerFace, nodesPerFace !< Counters.
      integer, parameter :: missing_value = -999
      integer :: ierr !< Result status (UG_NOERR==NF90_NOERR if successful).
      integer :: numk1d ! Number of 1D nodes
      integer :: n1d2dcontacts ! Number of 1D2D contacts (exchanges)
      integer :: L, n1, n2

      ierr = UG_NOERR

      ! count number of 1D nodes and 1D2D contacts
      numk1d = 0
      n1d2dcontacts = 0
      if (numl1d > 0) then ! there is a 1D grid
         ! count 1d mesh nodes and 1d2d contacts
         do L = 1, numl1d
            if (kn(3, L) == 1 .or. kn(3, L) == 6) then
               ! Regular 1D net link, or: when no cells, all 1D2D-type net links will also be included with both start and end node.
               numk1d = max(numk1d, kn(1, l), kn(2, l))
            else
               ! 1D2D-type net links, with cell info available.
               n1d2dcontacts = n1d2dcontacts + 1
               n1 = abs(lne(1, l))
               n2 = abs(lne(2, l))
               if (n1 > nump) then ! First point of 1D link is 1D cell (nump = number of 2D flow cells)
                  numk1d = max(numk1d, netcell(n1)%nod(1))
               end if
               if (n2 > nump) then ! Second point of 1D link is 1D cell
                  numk1d = max(numk1d, netcell(n2)%nod(1))
               end if
            end if
         end do
      end if

      if (ndx2d > 0) then ! there is a 2D grid
         ! Create 2D (layered) mesh geometry that contains all 2D faces, edges and nodes.
         ierr = t_ug_meshgeom_destructor(meshgeom)
         call check_error(ierr)
         meshgeom%meshName = 'mesh2d'
         meshgeom%start_index = 1
         meshgeom%dim = 2

         ! Nodes.
         ! Use only 2D net nodes (= nodes).
         meshgeom%numNode = numk - numk1d - n1d2dcontacts

         ! Get node coordinates.
         call reallocP(meshgeom%nodex, meshgeom%numNode, fill=dmiss)
         call reallocP(meshgeom%nodey, meshgeom%numNode, fill=dmiss)
         call reallocP(meshgeom%nodez, meshgeom%numNode, fill=dmiss)
         do node = 1, meshgeom%numNode
            meshgeom%nodex(node) = xk(numk1d + node)
            meshgeom%nodey(node) = yk(numk1d + node)
            meshgeom%nodez(node) = zk(numk1d + node)
         end do

         ! Edges.
         ! Use only 2D net links (= edges).
         meshgeom%numEdge = NUML - NUML1d

         ! Get edge nodes connectivity, edge types and edge coordinates (ordered as follows: first flow links, then closed edges).
         call reallocP(meshgeom%edge_nodes, (/2, meshgeom%numEdge/), fill=missing_value)
         call realloc(edge_type, meshgeom%numEdge, fill=missing_value)
         call reallocP(meshgeom%edgex, meshgeom%numEdge, fill=dmiss)
         call reallocP(meshgeom%edgey, meshgeom%numEdge, fill=dmiss)
         call realloc(edge_mapping_table, meshgeom%numEdge, fill=missing_value)
         call realloc(reverse_edge_mapping_table, meshgeom%numEdge, fill=missing_value)
         call get_2d_edge_data(meshgeom%edge_nodes, null(), edge_type, meshgeom%edgex, meshgeom%edgey, edge_mapping_table, reverse_edge_mapping_table)
         ! shift node numbers by numk1d
         do edge = 1, meshgeom%numEdge
            if (meshgeom%edge_nodes(1, edge) /= missing_value) then
               meshgeom%edge_nodes(1, edge) = meshgeom%edge_nodes(1, edge) - numk1d
            end if
            if (meshgeom%edge_nodes(2, edge) /= missing_value) then
               meshgeom%edge_nodes(2, edge) = meshgeom%edge_nodes(2, edge) - numk1d
            end if
         end do
         ! Edge z coordinates are unknown.

         ! Get edge faces connectivity.
         call reallocP(meshgeom%edge_faces, (/2, meshgeom%numEdge/))
         ! Here need to use reverse_edge_mapping_table to map edges to net links, because edges are ordered as follows: first flow links, then closed edges.
         do edge = 1, meshgeom%numEdge
            meshgeom%edge_faces(1:2, edge) = lne(1:2, reverse_edge_mapping_table(edge))

            ! 0 means no face, i.e. edge is on the boundary of the mesh.
            ! Replace zeroes with missing values.
            if (meshgeom%edge_faces(1, edge) == 0) meshgeom%edge_faces(1, edge) = missing_value
            if (meshgeom%edge_faces(2, edge) == 0) meshgeom%edge_faces(2, edge) = missing_value
         end do

         ! Faces.
         ! Use only 2D internal net cells = 2D internal flow nodes (= faces).
         meshgeom%numFace = nump

         ! Get face coordinates.
         meshgeom%facex => xzw(1:nump)
         meshgeom%facey => yzw(1:nump)
         ! Face z coordinates are unknown.

         ! Determine max nr of net nodes per 2D net cell = face.
         maxNodesPerFace = 0
         do face = 1, nump
            maxNodesPerFace = max(maxNodesPerFace, netcell(face)%n)
         end do

         ! Get face nodes connectivity, face edges connectivity and face-face connectivity.
         call reallocP(meshgeom%face_nodes, (/maxNodesPerFace, meshgeom%numFace/), fill=missing_value)
         call reallocP(meshgeom%face_edges, (/maxNodesPerFace, meshgeom%numFace/), fill=missing_value)
         call reallocP(meshgeom%face_links, (/maxNodesPerFace, meshgeom%numFace/), fill=missing_value)
         do face = 1, nump
            nodesPerFace = netcell(face)%n
            ! shift node numbers by numk1d
            meshgeom%face_nodes(1:nodesPerFace, face) = netcell(face)%nod - numk1d

            ! Here need to use edge_mapping_table to map net links to edges, because edges are ordered as follows: first flow links, then closed edges.
            ! shift node numbers by numk1d
            meshgeom%face_edges(1:nodesPerFace, face) = edge_mapping_table(netcell(face)%lin - numl1d)

            ! Get faces that are adjacent to the current face.
            call get_adjacent_faces(face, meshgeom%face_edges, meshgeom%edge_faces, meshgeom%face_links(1:nodesPerFace, face))
         end do

         ! Layers.
         ierr = add_layers_to_meshgeom(meshgeom)
         call check_error(ierr)

         deallocate (edge_mapping_table)
         deallocate (reverse_edge_mapping_table)
      end if

   end function create_ugrid_geometry

!> Add layer information to the given mesh geometry (only if layers are used).
!!
!! NOTE: do not pass already filled mesh geometries to this function,
!! since array pointers will become disassociated, possibly causing memory leaks.
   function add_layers_to_meshgeom(meshgeom) result(ierr)
      use io_ugrid
      use m_flow, only: kmx, mxlaydefs, laymx, numtopsig
      use m_alloc
      use m_missing
      use unstruc_netcdf, only: get_layer_data_ugrid
      use dfm_error

      implicit none

      type(t_ug_meshgeom), intent(inout) :: meshgeom !< The mesh geometry to add layer information to.

      integer :: layer_count, layer_type
      real(kind=dp), dimension(:), allocatable :: layer_zs, interface_zs
      integer :: ierr !< Result status (UG_NOERR==NF90_NOERR if successful).

      ierr = UG_NOERR

      if (kmx <= 0) then ! If no layers present.
         ! Do nothing.
         return
      end if

      ! If layers present.
      if (mxlaydefs > 1) then
         call mess(LEVEL_WARN, 'Multiple layer definitions cannot be handled for layer variables. Layer variables will not be added to meshgeom.')
         ierr = DFM_NOTIMPLEMENTED
         return
      end if

      ! Get layer info.
      layer_count = laymx(1)
      call realloc(layer_zs, layer_count, fill=dmiss, keepExisting=.false.)
      call realloc(interface_zs, layer_count + 1, fill=dmiss, keepExisting=.false.)
      call get_layer_data_ugrid(layer_count, layer_type, layer_zs, interface_zs)

      ! Store layer info in mesh geometry.
      meshgeom%num_layers = layer_count
      meshgeom%layertype = layer_type
      call reallocP(meshgeom%layer_zs, layer_count, fill=dmiss)
      call reallocP(meshgeom%interface_zs, layer_count + 1, fill=dmiss)
      meshgeom%layer_zs = layer_zs
      meshgeom%interface_zs = interface_zs

      select case (meshgeom%layertype)
      case (LAYERTYPE_OCEANSIGMA)
         ! Set trivial value for numtopsig: same as total layer count
         meshgeom%numtopsig = meshgeom%num_layers
      case (LAYERTYPE_Z)
         ! Set trivial value for numtopsig: 0, all layers are fixed z.
         meshgeom%numtopsig = 0
      case (LAYERTYPE_OCEAN_SIGMA_Z)
         meshgeom%numtopsig = numtopsig
         call mess(LEVEL_WARN, 'Combined Z-Sigma not implemented yet in Waq.')
         ierr = DFM_NOTIMPLEMENTED
         return
      case default
         call mess(LEVEL_WARN, 'Unknown layer type.')
         ierr = DFM_NOTIMPLEMENTED
         return
      end select

      deallocate (layer_zs)
      deallocate (interface_zs)

   end function add_layers_to_meshgeom

!> Aggregates the given mesh geometry and edge type array using the given aggregation table.
!! The mesh aggregation algorithm removes edges, but preserves the order of the edges. The edge type of a given edge stays the same.
!! So if the edges in the un-aggregated mesh are ordered (first flow links, then closed edges),
!! then the edges in the aggregated mesh will still be ordered (first flow links, then closed edges).
!!
!! NOTE: do not pass already filled output mesh geometries to this function,
!! since array pointers will become disassociated, possibly causing memory leaks.
   function aggregate_ugrid_geometry(input, output, input_edge_type, output_edge_type, face_mapping_table) result(success)

      use io_ugrid
      use m_alloc
      use m_sferic, only: jsferic, jasfer3D
      use m_missing, only: dmiss
      use geometry_module, only: comp_masscenter

      implicit none

      type(t_ug_meshgeom), intent(in) :: input !< The mesh geometry to be aggregated.
      type(t_ug_meshgeom), intent(inout) :: output !< Aggregated mesh geometry.
      integer, dimension(:), intent(in) :: input_edge_type !< The edge type array to be aggregated.
      integer, dimension(:), allocatable, intent(out) :: output_edge_type !< Aggregated edge type array.
      integer, dimension(:), intent(in) :: face_mapping_table !< Mapping table flow cells -> waq cells.
      logical :: success !< Result status, true if successful.

      character(len=255) :: message !< Temporary variable for writing log messages.
      integer, parameter :: missing_value = -999
      integer, dimension(:), allocatable :: node_mapping_table, reverse_node_mapping_table, reverse_edge_mapping_table !< Mapping tables.
      integer :: input_edge_count, output_edge_count, output_node_count, output_face_count, max_nodes_per_face, node_count !< Counters.
      integer :: i, input_edge, output_edge, input_node, output_node, output_face !< Counters.
      integer, dimension(2) :: faces !< Helper array.
      integer, dimension(:, :), allocatable :: input_edge_output_faces !< Helper array.
      integer, dimension(:), allocatable :: face_edge_count, nodes !< Helper arrays.
      double precision :: area !< Output of subroutine comp_masscenter (not used here).
      integer :: counterclockwise !< Output of subroutine comp_masscenter (not used here).

      success = .false.

      ! 1. Determine output edge_faces and edge_nodes.
      ! Apply face mapping table to edge faces.
      input_edge_count = input%numEdge
      call realloc(input_edge_output_faces, (/2, input_edge_count/), fill=missing_value)
      do input_edge = 1, input_edge_count
         do i = 1, 2
            if (input%edge_faces(i, input_edge) /= missing_value) then
               input_edge_output_faces(i, input_edge) = face_mapping_table(input%edge_faces(i, input_edge))
            end if
         end do ! i
      end do ! input_edge
      ! Create edge mapping table and output edge_faces and edge_nodes.
      call realloc(reverse_edge_mapping_table, input_edge_count)
      call reallocP(output%edge_faces, (/2, input_edge_count/))
      call reallocP(output%edge_nodes, (/2, input_edge_count/))
      output_edge = 0
      do input_edge = 1, input_edge_count
         ! If edge points to the same aggregated face on either side, then edge is not needed anymore in the aggregated mesh.
         if (input_edge_output_faces(1, input_edge) /= input_edge_output_faces(2, input_edge)) then ! Edge that should stay.
            ! The remaining output edges have a different numbering.
            output_edge = output_edge + 1
            reverse_edge_mapping_table(output_edge) = input_edge
            output%edge_faces(1:2, output_edge) = input_edge_output_faces(1:2, input_edge)
            output%edge_nodes(1:2, output_edge) = input%edge_nodes(1:2, input_edge)
         end if
      end do
      output_edge_count = output_edge
      if (output_edge_count < 3) then
         call mess(LEVEL_ERROR, 'Edge count in aggregated mesh < 3. Mesh will not be aggregated.')
         return
      end if
      ! At this point edges have been renumbered automatically from input edge numbers to output edge numbers.
      ! Truncate arrays.
      call realloc(reverse_edge_mapping_table, output_edge_count, keepExisting=.true.)
      call reallocP(output%edge_faces, (/2, output_edge_count/), keepExisting=.true.)
      call reallocP(output%edge_nodes, (/2, output_edge_count/), keepExisting=.true.)

      ! 2. Determine output edge coordinates and types.
      call reallocP(output%edgex, output_edge_count)
      call reallocP(output%edgey, output_edge_count)
      call realloc(output_edge_type, output_edge_count)
      do output_edge = 1, output_edge_count
         output%edgex(output_edge) = input%edgex(reverse_edge_mapping_table(output_edge))
         output%edgey(output_edge) = input%edgey(reverse_edge_mapping_table(output_edge))
         ! Edge z coordinates are unknown.
         output_edge_type(output_edge) = input_edge_type(reverse_edge_mapping_table(output_edge))
      end do

      ! 3. Create node mapping table.
      call realloc(node_mapping_table, input%numNode, fill=missing_value)
      ! All nodes that are present in output edge_nodes should remain, all other nodes are not needed anymore in the aggregated mesh.
      ! First create mask of remaining nodes in node_mapping_table.
      do output_edge = 1, output_edge_count
         node_mapping_table(output%edge_nodes(1:2, output_edge)) = 1
      end do
      output_node_count = count(node_mapping_table == 1)
      if (output_node_count < 3) then
         call mess(LEVEL_ERROR, 'Node count in aggregated mesh < 3. Mesh will not be aggregated.')
         return
      end if
      ! Change mask into mapping table.
      call realloc(reverse_node_mapping_table, output_node_count)
      output_node = 0
      do input_node = 1, input%numNode
         if (node_mapping_table(input_node) == 1) then ! Node that should stay.
            ! The remaining output nodes have a different numbering.
            output_node = output_node + 1
            node_mapping_table(input_node) = output_node
            reverse_node_mapping_table(output_node) = input_node
         end if
      end do
      ! Renumber input node numbers to output node numbers in output edge_nodes, using node_mapping_table.
      do output_edge = 1, output_edge_count
         output%edge_nodes(1, output_edge) = node_mapping_table(output%edge_nodes(1, output_edge))
         output%edge_nodes(2, output_edge) = node_mapping_table(output%edge_nodes(2, output_edge))
      end do

      ! 4. Determine output node coordinates.
      call reallocP(output%nodex, output_node_count)
      call reallocP(output%nodey, output_node_count)
      call reallocP(output%nodez, output_node_count)
      do output_node = 1, output_node_count
         output%nodex(output_node) = input%nodex(reverse_node_mapping_table(output_node))
         output%nodey(output_node) = input%nodey(reverse_node_mapping_table(output_node))
         output%nodez(output_node) = input%nodez(reverse_node_mapping_table(output_node))
      end do

      ! 5. Determine output face_edges.
      !    ! Convert output edge_faces to a flat table with two columns: edges column and faces column.
      !    call realloc(edges_column, output_edge_count * 2)
      !    call realloc(faces_column, output_edge_count * 2)
      !    forall (i = 1:output_edge_count*2)
      !        edges_column(i) = (i + 1) / 2
      !    end forall
      !    faces_column = reshape(output_edge_faces, (/ output_edge_count * 2 /))
      !    ! Sort table on faces column.
      !    ! TODO use quicksort? AK
      !    qsort(faces_column, sorted_faces_column, sorted_indices)
      !    sorted_edges_column = edges_column(sorted_indices)
      ! This code assumes that output faces are numbered 1, 2, 3, etc. without gaps.
      ! TODO remove -1, -2, etc. by making temp pointer to first part of face_mapping_table
      output_face_count = maxval(face_mapping_table)
      if (output_face_count < 1) then
         call mess(LEVEL_ERROR, 'Face count in aggregated mesh < 1. Mesh will not be aggregated.')
         return
      end if
      ! Count edges for each face.
      call realloc(face_edge_count, output_face_count, fill=0)
      do output_edge = 1, output_edge_count
         faces = output%edge_faces(1:2, output_edge)
         ! Add 1 edge for both faces.
         do i = 1, 2
            if (faces(i) == missing_value) then
               cycle
            end if

            face_edge_count(faces(i)) = face_edge_count(faces(i)) + 1
         end do ! i
      end do ! output_edge
      do output_face = 1, output_face_count
         if (face_edge_count(output_face) < 3) then
            write (message, *) 'Face edge count in aggregated mesh < 3 for face ', output_face, '. Mesh will not be aggregated.'
            call mess(LEVEL_ERROR, trim(message))
            return
         end if
      end do
      ! Determine max_nodes_per_face.
      max_nodes_per_face = maxval(face_edge_count)
      ! Determine nodes, edges and faces for each output face.
      call reallocP(output%face_edges, (/max_nodes_per_face, output_face_count/), fill=missing_value)
      ! Re-use face_edge_count array to put edges in the next available spot in the output%face_edges array.
      face_edge_count = 0
      do output_edge = 1, output_edge_count
         faces = output%edge_faces(1:2, output_edge)
         do i = 1, 2
            if (faces(i) == missing_value) then
               cycle
            end if

            ! Keep track of current number of edges for this face.
            face_edge_count(faces(i)) = face_edge_count(faces(i)) + 1
            ! Put current edge in the next available spot in output%face_edges for this face.
            output%face_edges(face_edge_count(faces(i)), faces(i)) = output_edge
         end do ! i
      end do ! output_edge
      ! At this point the edges for each face are in random order.

      ! 6. Sort edges for each face in counter clockwise order.
      ! At the same time store sorted nodes of sorted edges in output%face_nodes array.
      call reallocP(output%face_nodes, (/max_nodes_per_face, output_face_count/), fill=missing_value)
      do output_face = 1, output_face_count
         ! Sort edges for current output face.
         call sort_edges(output_face, output%face_edges(1:face_edge_count(output_face), output_face), output%face_nodes(1:face_edge_count(output_face), output_face), &
                         input%edge_nodes, input%face_nodes, input%edge_faces, face_mapping_table, reverse_edge_mapping_table, node_mapping_table, output%edge_nodes)
      end do

      ! 7. Determine output face_links.
      call reallocP(output%face_links, (/max_nodes_per_face, output_face_count/), fill=missing_value)
      do output_face = 1, output_face_count
         ! Get output faces that are adjacent to the current output_face.
         call get_adjacent_faces(output_face, output%face_edges, output%edge_faces, output%face_links(1:face_edge_count(output_face), output_face))
      end do

      ! 8. Determine output face coordinates.
      ! Here calculate the cell centroids (cell "centers of mass").
      call realloc(nodes, max_nodes_per_face)
      call reallocP(output%facex, output_face_count)
      call reallocP(output%facey, output_face_count)
      do output_face = 1, output_face_count
         node_count = face_edge_count(output_face)

         ! Reset nodes.
         nodes = missing_value
         nodes(1:node_count) = output%face_nodes(1:node_count, output_face)

         ! Note that passed xs and ys arrays are larger than the passed polygon size (extra elements are not used in subroutine comp_masscenter).
         call comp_masscenter(node_count, output%nodex(nodes(1:node_count)), output%nodey(nodes(1:node_count)), &
                              output%facex(output_face), output%facey(output_face), area, counterclockwise, jsferic, jasfer3D, dmiss)
         ! Face z coordinates are unknown.
      end do

      ! Store remaining output variables in output mesh geometry.
      output%meshName = trim(input%meshName)//'_agg'
      output%dim = input%dim
      output%num_layers = input%num_layers
      output%layertype = input%layertype
      output%layer_zs => input%layer_zs
      output%interface_zs => input%interface_zs

      output%numNode = output_node_count
      output%numEdge = output_edge_count
      output%numFace = output_face_count

      !TODO deallocate temporary arrays

      success = .true.

   end function aggregate_ugrid_geometry

!> Sorts the given edges of the current face in counter clockwise order.
!! At the same time stores the sorted nodes of the current face in the given nodes array.
!! In this subroutine input means "from the un-aggregated mesh" and output means "from the aggregated mesh".
   subroutine sort_edges(current_face, edges, nodes, input_edge_nodes, input_face_nodes, input_edge_faces, face_mapping_table, reverse_edge_mapping_table, node_mapping_table, output_edge_nodes)

      implicit none

      integer, intent(in) :: current_face !< Current face.
      integer, dimension(:), intent(inout) :: edges !< Edges of the current face.
      integer, dimension(:), intent(out) :: nodes !< Array to store the nodes of the current face.
      integer, dimension(:, :), intent(in) :: input_edge_nodes, input_face_nodes, input_edge_faces, output_edge_nodes !< Connectivity arrays.
      integer, dimension(:), intent(in) :: face_mapping_table, reverse_edge_mapping_table, node_mapping_table !< Mapping tables.

      character(len=255) :: message !< Temporary variable for writing log messages.
      integer :: first_node, current_node, number_of_edges, k, i !< Counters.
      integer, dimension(2) :: next_nodes !< Helper array.
      logical :: found

      ! Start with the edge that happens to be listed first, this will stay in the first position.
      ! First sort the two nodes of the first edge in CCW order, so that all subsequent edges will also be sorted in CCW order.
      next_nodes = sort_first_two_nodes(current_face, edges(1), input_edge_nodes, input_face_nodes, input_edge_faces, face_mapping_table, reverse_edge_mapping_table, node_mapping_table)

      first_node = next_nodes(1)
      current_node = next_nodes(2)
      nodes(1) = first_node
      number_of_edges = size(edges)
      do k = 2, number_of_edges
         nodes(k) = current_node

         ! Error if arrive at the first edge and there are still un-used edges leftover.
         if (current_node == first_node) then
            write (message, *) 'For face ', current_face, ' there are unconnected edges in aggregated mesh. &
               &This can happen if the aggregated cell consists of cells that are not connected or if the aggregated cell is shaped like a ring. Mesh will not be aggregated.'
            call mess(LEVEL_ERROR, trim(message))
            return
         end if

         ! Get next neighbor edge, i.e. another edge that is connected to the current node.
         found = .false.
         do i = k, number_of_edges
            next_nodes = output_edge_nodes(1:2, edges(i))

            if (next_nodes(1) == current_node) then
               found = .true.
               if (i /= k) then
                  call swap(edges(i), edges(k))
               end if
               ! Continue with node on the other side of next edge.
               current_node = next_nodes(2)
               exit
            else if (next_nodes(2) == current_node) then
               found = .true.
               if (i /= k) then
                  call swap(edges(i), edges(k))
               end if
               ! Continue with node on the other side of next edge.
               current_node = next_nodes(1)
               exit
            end if
         end do ! i

         if (.not. found) then
            write (message, *) 'For face ', current_face, ' cannot find edge connected to node ', current_node, ' of edge ', edges(k - 1), ' in aggregated mesh. Mesh will not be aggregated.'
            call mess(LEVEL_ERROR, trim(message))
            return
         end if
      end do ! k

      ! Error if last edge is not connected to first edge.
      if (current_node /= first_node) then
         write (message, *) 'For face ', current_face, ' node ', current_node, ' of last edge ', edges(number_of_edges), &
            ' is not connected to node ', first_node, ' of first edge ', edges(1), ' in aggregated mesh. Mesh will not be aggregated.'
         call mess(LEVEL_ERROR, trim(message))
         return
      end if

   end subroutine sort_edges

   !> The given edge in the aggregated mesh has two nodes. The returned array contains these two nodes sorted in CCW order,
 !! i.e. in the same order as these two nodes would be encountered when traversing the nodes of the given face in CCW order.
 !! The order will be opposite for the two faces that the given edge connects, therefore the given face is also needed as input.
 !! In this subroutine input means "from the un-aggregated mesh" and output means "from the aggregated mesh".
   function sort_first_two_nodes(output_face, output_edge, input_edge_nodes, input_face_nodes, input_edge_faces, face_mapping_table, reverse_edge_mapping_table, node_mapping_table) result(sorted_output_nodes)
      use m_alloc

      implicit none

      integer, intent(in) :: output_face !< Current face.
      integer, intent(in) :: output_edge !< First edge of the current face.
      integer, dimension(:, :), intent(in) :: input_edge_nodes, input_face_nodes, input_edge_faces !< Connectivity arrays.
      integer, dimension(:), intent(in) :: face_mapping_table, reverse_edge_mapping_table, node_mapping_table !< Mapping tables.

      character(len=255) :: message !< Temporary variable for writing log messages.
      integer, parameter :: missing_value = -999
      integer :: input_edge, input_face, max_nodes_per_face, nodes_per_face, node, next_node, previous_node
      integer, dimension(2) :: input_nodes, input_faces
      integer, dimension(:), allocatable :: nodes
      integer :: i !< Counter.
      logical :: sorted
      integer, dimension(2) :: sorted_output_nodes !< The two nodes of the first edge of the current face in sorted order.

      ! Get input edge, nodes and faces that correspond to the given output edge.
      input_edge = reverse_edge_mapping_table(output_edge)
      input_nodes = input_edge_nodes(1:2, input_edge)
      input_faces = input_edge_faces(1:2, input_edge)

      ! Get the input face of the input edge that is part of the given output face.
      if (input_faces(1) /= missing_value .and. face_mapping_table(input_faces(1)) == output_face) then
         input_face = input_faces(1)
      else if (input_faces(2) /= missing_value .and. face_mapping_table(input_faces(2)) == output_face) then
         input_face = input_faces(2)
      else
         write (message, *) 'Cannot find input face for output face ', output_face, ' and output edge ', output_edge, ' in un-aggregated mesh. Mesh will not be aggregated.'
         call mess(LEVEL_ERROR, trim(message))
         return
      end if

      ! Get input nodes of input face.
      max_nodes_per_face = size(input_face_nodes(:, input_face))
      call realloc(nodes, max_nodes_per_face)
      nodes = input_face_nodes(:, input_face)
      ! Determine nodes_per_face.
      nodes_per_face = max_nodes_per_face
      do i = 1, max_nodes_per_face
         if (nodes(i) == missing_value) then
            nodes_per_face = i - 1
            exit
         end if
      end do
      if (nodes_per_face < 3) then
         call mess(LEVEL_ERROR, 'Nodes per face in un-aggregated mesh < 3. Mesh will not be aggregated.')
         return
      end if

      ! Sort input_nodes.
      ! Find input nodes in input face nodes of input face and sort the input nodes in the same order as they are found in input face nodes.
      sorted = .false.
      do i = 1, nodes_per_face
         node = nodes(i)

         if (node == input_nodes(1)) then
            next_node = nodes(modulo(i, nodes_per_face) + 1)
            if (next_node == input_nodes(2)) then
               sorted = .true.
               exit
            end if

            previous_node = nodes(modulo(i + nodes_per_face - 2, nodes_per_face) + 1)
            if (previous_node == input_nodes(2)) then
               call swap(input_nodes(1), input_nodes(2))
               sorted = .true.
               exit
            end if

            write (message, *) 'Cannot find node ', input_nodes(2), ' of face ', input_face, ' in face nodes in un-aggregated mesh. Mesh will not be aggregated.'
            call mess(LEVEL_ERROR, trim(message))
            return
         end if
      end do
      if (.not. sorted) then
         write (message, *) 'Cannot find node ', input_nodes(1), ' of face ', input_face, ' in face nodes in un-aggregated mesh. Mesh will not be aggregated.'
         call mess(LEVEL_ERROR, trim(message))
         return
      end if

      ! Get output nodes corresponding to sorted input nodes.
      sorted_output_nodes = node_mapping_table(input_nodes)

      deallocate (nodes)

   end function sort_first_two_nodes

!> All faces that are adjacent to the given face are stored in the given array adjacent_faces.
!! The length of the given array adjacent_faces must be equal to the number of edges of the given face.
   subroutine get_adjacent_faces(face, face_edges, edge_faces, adjacent_faces)

      implicit none

      integer, intent(in) :: face !< Input face.
      integer, dimension(:, :), intent(in) :: face_edges !< Face edge connectivity.
      integer, dimension(:, :), intent(in) :: edge_faces !< Edge face connectivity.
      integer, dimension(:), intent(out) :: adjacent_faces !< Output array.

      integer :: edge, i
      integer, dimension(2) :: faces

      ! Determine faces that are adjacent to the current face.
      do i = 1, size(adjacent_faces)
         edge = face_edges(i, face)

         ! Store neighboring face for this edge.
         ! Note that some face links can be out_of_mesh (i.e. missing value).
         faces = edge_faces(1:2, edge)
         ! Of the two faces, one is the given face and the other is the neighboring face (or missing value).
         if (faces(1) == face) then
            adjacent_faces(i) = faces(2)
         else ! If faces(2) == face
            adjacent_faces(i) = faces(1)
         end if
      end do ! i

   end subroutine get_adjacent_faces

!> Swap the values of the given integers a and b.
   subroutine swap(a, b)

      implicit none

      integer, intent(inout) :: a, b !< Integers to swap.

      integer :: temp

      temp = a
      a = b
      b = temp

   end subroutine swap
!
!------------------------------------------------------------------------------

!> Write boundary information for use in WAQ-GUI.
   subroutine waq_wri_bnd()

      use m_flowgeom
      use network_data
      use m_partitioninfo, only: is_ghost_node
      use fm_external_forcings_data
      use m_laterals, only: numlatsg, n1latsg, n2latsg, nnlat, lat_ids
      use unstruc_files
      use m_sferic, only: jsferic, jasfer3D
      use m_missing, only: dmiss, dxymis
      use geometry_module, only: normalout

      implicit none
      !
      !           Local variables
      !
      integer :: LL, L, Lf, n, i, istart, n1, n2
      integer :: ibnd, isrc, ilat, k1, kk, nopenbndsectnonempty
      integer :: lunbnd
      character(len=255) :: filename
      double precision :: x1, y1, x2, y2, xn, yn
      character(len=20) :: sectionname
      !
   !! executable statements -------------------------------------------------------
      !
      filename = defaultFilename('bnd')
      call newfil(lunbnd, trim(filename))

      ! Count how many bnd segments are truly open (# links/exchanges > 0)
      istart = 0
      nopenbndsectnonempty = 0
      do i = 1, nopenbndsect
         if (nopenbndlin(i) - istart == 0) then
            cycle
         end if
         istart = nopenbndlin(i)
         nopenbndsectnonempty = nopenbndsectnonempty + 1
      end do

      write (lunbnd, '(i8)') nopenbndsectnonempty + waqpar%numsrcbnd + waqpar%numlatsectwaq ! Nr of open boundary sections, sink sources and laterals.
      istart = 0
      do i = 1, nopenbndsect
         if (nopenbndlin(i) - istart == 0) then
            cycle
         end if
         sectionname = makesectionname('bnd_', openbndname(i))
         write (lunbnd, '(a)') sectionname ! Section name
         write (lunbnd, '(i8)') nopenbndlin(i) - istart ! Nr of links in section

         do LL = istart + 1, nopenbndlin(i)
            L = openbndlin(LL)
            Lf = lne2ln(L)

            if (Lf <= 0 .or. Lf > lnx) then
               n = 0
               x1 = 0d0
               y1 = 0d0
               x2 = 0d0
               y2 = 0d0
            else
               n = ln(1, Lf)
               if (kn(3, L) == 1) then ! 1D link
                  ! TODO: AvD: this is probably wrong for 1D2D links (kcu==3 or 4)
                  !                    n1 = abs(lne(1,L))             ! external 1D flow node
                  !                    n2 = abs(lne(2,L))             ! internal 1D flow node
                  n1 = abs(ln(1, Lf)) ! external 1D flow node
                  n2 = abs(ln(2, Lf)) ! internal 1D flow node

                  !   For 1D links: produce fictious 'cross/netlink'
                  call normalout(xz(n1), yz(n1), xz(n2), yz(n2), xn, yn, jsferic, jasfer3D, dmiss, dxymis)
                  xn = wu(Lf) * xn
                  yn = wu(Lf) * yn

                  x1 = .5d0 * (xz(n1) + xz(n2)) - .5d0 * xn
                  y1 = .5d0 * (yz(n1) + yz(n2)) - .5d0 * yn
                  x2 = .5d0 * (xz(n1) + xz(n2)) + .5d0 * xn
                  y2 = .5d0 * (yz(n1) + yz(n2)) + .5d0 * yn
               else
                  x1 = xk(kn(1, L))
                  y1 = yk(kn(1, L))
                  x2 = xk(kn(2, L))
                  y2 = yk(kn(2, L))
               end if
            end if
            ! Bnd flow nodes as delwaq bnd segments (<0)
            ! and 2 points of net link
            write (lunbnd, '(i8,4f18.8)') - (n - ndxi), x1, y1, x2, y2
         end do
         istart = nopenbndlin(i)
      end do
      ibnd = ndx - ndxi
      do isrc = 1, numsrc
         if ((ksrc(1, isrc) == 0 .and. ksrc(4, isrc) > 0) .or. (ksrc(4, isrc) == 0 .and. ksrc(1, isrc) > 0)) then
            ! This is a boundary condition within the current domain
            ibnd = ibnd + 1
            if (ksrc(1, isrc) /= 0) then
               kk = ksrc(1, isrc)
            else
               kk = ksrc(4, isrc)
            end if
            sectionname = makesectionname('src_', srcname(isrc))
            write (lunbnd, '(a)') sectionname ! Section name
            write (lunbnd, '(i8)') 1 ! Nr of source links in section
            write (lunbnd, '(i8,4f18.8)') - (ibnd), xz(kk), yz(kk), xz(kk), yz(kk)
         end if
      end do

      if (numlatsg > 0) then
         do ilat = 1, numlatsg
            if (waqpar%numlatinsectwaq(ilat) > 0) then
               sectionname = makesectionname('lat_', lat_ids(ilat))
               write (lunbnd, '(a)') trim(sectionname) ! Section name
               write (lunbnd, '(i8)') waqpar%numlatinsectwaq(ilat) ! Nr of lateral links in section
               do k1 = n1latsg(ilat), n2latsg(ilat)
                  kk = nnlat(k1)
                  if (kk > 0) then
                     if (.not. is_ghost_node(kk)) then
                        ! This is a lateral within the current domain
                        ibnd = ibnd + 1
                        ! use coordinates of the nodes the lateral is connected to for now. not geomXLat, geomYLat
                        write (lunbnd, '(i8,4f18.8)') - (ibnd), xz(kk), yz(kk), xz(kk), yz(kk)
                     end if
                  end if
               end do
            end if
         end do
      end if

      call doclose(lunbnd)
   end subroutine waq_wri_bnd
!
!------------------------------------------------------------------------------

!> Writes all necessary time-independent model schematisation files for DelWAQ.
!! (.len, .poi, .srf)
   subroutine waq_wri_model_files()
      use m_flowgeom
      use unstruc_files, only: defaultFilename

      implicit none

      !
      !           Local variables
      !

      !
   !! executable statements -------------------------------------------------------
      !
      write (msgbuf, '(a)') 'In waq_wri_model_files().'
      call msg_flush()

      ! Automatically prepare aggration and mapping tables.
      call waq_prepare_aggr()

      ! Hyd file (hydrodynamic coupling)

      call waq_wri_hyd()

      ! (Unstructured) grid file
      call waq_wri_geom()

      ! Boundaries file
      call waq_wri_bnd()

      ! Pointer file (linkages)
      call waq_wri_poi(defaultFilename('poi'))

      ! Length file (dispersion lengths)
      call waq_wri_len(lnx, dx, acl, defaultFilename('len'))

      ! Surface file (horizontal surfaces)
      call waq_wri_srf(ndxi, ndx, ba, defaultFilename('srf'))

      ! Attributes file
      call waq_wri_atr(defaultFilename('atr'))

   end subroutine waq_wri_model_files
!
!------------------------------------------------------------------------------

!> Writes all necessary time-dependent couple files for DelWAQ.
!! (.are, .flo, .vol, .vel)
!!
!! Note: flow-related files (.are and .flo) are not written for first
!! time. Thereafter, accumulated flux is associated with previous
!! timestep. At the last time a dummy record is written in .are and .flo.
   subroutine waq_wri_couple_files(time)
      use m_laterals, only: numlatsg
      use m_flowtimes
      use m_flowgeom
      use m_flow
      use fm_external_forcings_data
      use m_waves
      use unstruc_files, only: defaultFilename
      use m_gettaus
      use m_gettauswave
      implicit none
      !
      !           Global variables
      !
      double precision, intent(in) :: time !< Current simulation time
      !
      !           Local variables
      !
      integer :: itim !< Time (seconds) since simulation start.
      integer, save :: itim_prev = -1
      !
   !! executable statements -------------------------------------------------------
      !
      write (msgbuf, *) 'In waq_wri_couple_files: #', it_waq, ', time: ', time
      it_waq = it_waq + 1
      call msg_flush()

      itim = nint(time)

      ! Volume file (volumes of computational cells)
      call waq_wri_vol(itim, defaultFilename('vol'), waqpar%lunvol)
      ! TODO: AvD: add a 'mode' 0/1/2 similar to Delft3D, so that we can write some quantities
      ! at *start* of *next* timestep instead of currently at the *end* of *current* timestep.

      ! Flow velocity file (Flow element center velocity magnitude)
      call waq_wri_vel(itim, defaultFilename('vel'), waqpar%lunvel)

      ! Salinty file (salinity of computational cells)
      if (jasal > 0) then
         call waq_wri_sal(itim, defaultFilename('sal'), waqpar%lunsal)
      end if

      ! Temperature file (salinity of computational cells)
      if (jatem > 0) then
         call waq_wri_tem(itim, defaultFilename('tem'), waqpar%luntem)
      end if

      ! Taus file (contains taus at the bottom of computational cells)
      if (jawave == 0 .or. flowWithoutWaves) then ! If jawave > 0, then taus is obtained from subroutine tauwave (taus = taucur + tauwave).
         call gettaus(1, 2)
      else
         call gettauswave(jawaveswartdelwaq)
      end if
      call waq_wri_tau(itim, defaultFilename('tau'), waqpar%luntau)

      !> Vertical diffusion file (contains vertical diffusion of computational cells)
      if (waqpar%kmxnxa > 1) then
         call waq_wri_vdf(itim, defaultFilename('vdf'), waqpar%lunvdf)
      end if

      ! For first step, do not write any flux-related quantities (note the return).
      if (it_waq > 1) then
         ! Area file (flow areas)
         call waq_wri_are(itim_prev, defaultFilename('are'), waqpar%lunare)
         ! AvD: NOTE: A bit strange, au is *not* accumulated, defined at time1, but still printed
         !            in file with itim_prev.

         ! Flow file (discharges)
         call waq_wri_flo(itim_prev, int(ti_waq), defaultFilename('flo'), waqpar%lunflo)

         ! Write a dummy last record in area and flow file to make them complete.
         if (time == ti_waqe) then
            au = 0d0
            q1waq = 0d0
            if (kmx > 0) then
               qwwaq = 0d0
            end if
            if (numsrc > 0) then
               qsrcwaq = 0d0 ! Reset accumulated discharges
            end if
            if (numlatsg > 0) then
               qlatwaq = 0d0 ! Reset accumulated discharges
            end if

            ! Dummy area record
            call waq_wri_are(itim, defaultFilename('are'), waqpar%lunare)

            ! Dummy flow record
            call waq_wri_flo(itim, int(ti_waq), defaultFilename('flo'), waqpar%lunflo)
         end if
      end if
      q1waq = 0d0 ! Reset accumulated discharges
      if (kmx > 0) then
         qwwaq = 0d0 ! Reset accumulated discharges
      end if
      if (numsrc > 0) then
         qsrcwaq = 0d0 ! Reset accumulated discharges
      end if
      if (numlatsg > 0) then
         qlatwaq = 0d0 ! Reset accumulated discharges
      end if
      itim_prev = itim
   end subroutine waq_wri_couple_files
!
!------------------------------------------------------------------------------

!> Prepare aggregation and mapping tables between flow grid and WAQ cells.
!! Currently, default: one-to-one.
   subroutine waq_prepare_aggr()
      use m_flowgeom
      use m_partitioninfo
      use unstruc_model
      use m_flow
      use fm_external_forcings_data
      use m_alloc
      implicit none

      integer :: i, kb, kt, ktx, vaglay
      integer, dimension(1) :: kmxnr
      integer :: lunvag, istat, ierr

      call realloc(waqpar%iapnt, ndx, keepExisting=.false., fill=0)
      call realloc(waqpar%isaggr, ndkx, keepExisting=.false., fill=0)
      call realloc(waqpar%iqaggr, lnkx, keepExisting=.false., fill=0)
      call realloc(waqpar%iqwaggr, ndkx, keepExisting=.false., fill=0)

      ! Is there a DIDO aggregation, and no MPI? Otherwise set default aggregation
      if (md_waqhoraggr /= ' ') then
         waqpar%flhoraggr = md_waqhoraggr
         inquire (file=waqpar%flhoraggr, exist=horaggr) ! no interface supported yet
         if (.not. horaggr) then
            call mess(LEVEL_ERROR, 'Horizontal aggregation of WAQ output was specified, but file was not found:', trim(md_waqhoraggr))
         end if
      else
         horaggr = .false.
      end if

      if (horaggr .and. jampi /= 0) then
         call mess(LEVEL_WARN, 'Horizontal aggregation of WAQ output was specified, but is not (yet) supported in MPI simulations! Aggregation is ignored.')
         horaggr = .false.
      end if

      if (horaggr) then
         call mess(LEVEL_INFO, 'Using horizontal aggregation of hydrodynamics for WAQ from: ', trim(md_waqhoraggr))
         ! read the horizontal aggregation
         waqpar%aggre = 1
         call waq_read_dwq(ndxi, ndx, waqpar%iapnt, waqpar%flhoraggr)
         waqpar%nosegl = maxval(waqpar%iapnt)
      else
         ! no aggregation, create default one to one aggregation
         waqpar%aggre = 0
         waqpar%flhoraggr = " "
         do i = 1, ndxi
            waqpar%iapnt(i) = i
         end do
         waqpar%nosegl = ndxi
         ! add pointer for boundary nodes
         do i = ndxi + 1, ndx
            waqpar%iapnt(i) = -(i - ndxi)
         end do
      end if

      ! Maximum nr of layers per node in flow
      ! MJ: this might not yet be the absolute total number of layers in a z-model, but we might not need it...
      if (kmx == 0) then
         waqpar%kmxnx = 1
      else
         waqpar%kmxnx = maxval(kmxn)
      end if

      ! Determine maximum number of layers in all domains (for z-layers)
      if (jampi == 1) then
         kmxnr(1) = waqpar%kmxnx
         call reduce_int_max(1, kmxnr)
         waqpar%kmxnx = kmxnr(1)
      end if

      call realloc(waqpar%ilaggrhyd, waqpar%kmxnx, keepExisting=.false., fill=1)
      call realloc(waqpar%ilaggr, waqpar%kmxnx, keepExisting=.false., fill=0)
      waqpar%aggrel = 0
      waqpar%kmxnxa = waqpar%kmxnx

      ! Is there a vertical aggregation specified?
      if (md_waqvertaggr /= ' ') then
         waqpar%flvertaggr = md_waqvertaggr
         inquire (file=waqpar%flvertaggr, exist=vertaggr) ! no interface supported yet
         if (.not. vertaggr) then
            call mess(LEVEL_ERROR, 'Vertical aggregation of WAQ output was specified, but file was not found:', trim(md_waqvertaggr))
         end if
         call mess(LEVEL_INFO, 'Using vertical aggregation of hydrodynamics for WAQ from: ', trim(md_waqvertaggr))
      else
         vertaggr = .false.
      end if

      if (vertaggr) then
         call oldfil(lunvag, waqpar%flvertaggr)
         read (lunvag, *, iostat=istat) vaglay
         if (vaglay == -1) then
            call mess(LEVEL_INFO, 'Found -1 as number of layers in waqtest.vag. Will aggregate DELWAQ output to 2D.')
            waqpar%kmxnxa = 1
            waqpar%ilaggrhyd(1) = waqpar%kmxnx
         elseif (vaglay /= waqpar%kmxnx) then
            call mess(LEVEL_ERROR, 'Mismatch in number of layers in DELWAQ vertical aggregation file: ', trim(md_waqvertaggr))
         else
            read (lunvag, *, iostat=istat) waqpar%ilaggr(1:vaglay)
            if (istat /= 0) then
               call mess(LEVEL_ERROR, 'Error reading the vertical aggregation pointer in: ', trim(md_waqvertaggr))
            else
               ! Check validity of the layer aggregation and create waqpar%ilaggrhyd
               ierr = 0
               if (waqpar%ilaggr(1) == 1) then
                  waqpar%kmxnxa = 1
                  do i = 2, vaglay
                     if (waqpar%ilaggr(i - 1) == waqpar%ilaggr(i)) then
                        waqpar%ilaggrhyd(waqpar%kmxnxa) = waqpar%ilaggrhyd(waqpar%kmxnxa) + 1
                     elseif (waqpar%ilaggr(i) == waqpar%ilaggr(i - 1) + 1) then
                        waqpar%kmxnxa = waqpar%kmxnxa + 1
                     else
                        ierr = 1
                        exit
                     end if
                  end do
               else
                  ierr = 1
               end if
               if (ierr == 1) then
                  ! No correct layer aggregation, default: no layer aggragation
                  call mess(LEVEL_ERROR, 'Incorrect vertical aggregation pointer in: ', trim(md_waqvertaggr))
               else
                  waqpar%aggrel = 1
               end if
            end if
         end if
      else
         waqpar%aggrel = 0
         waqpar%kmxnxa = waqpar%kmxnx
      end if

      if (waqpar%aggrel == 1 .and. waqpar%kmxnxa == 1) then
         ! If layer aggregation results in one layer left, then waq output will be 2D from 2D FM data directly
         waqpar%aggrel = 0
      end if

      if (waqpar%aggrel == 0) then
         do i = 1, waqpar%kmxnxa
            waqpar%ilaggr(i) = i
         end do
      end if

      waqpar%num_cells = waqpar%nosegl * waqpar%kmxnxa
      call realloc(waqpar%nosega, waqpar%num_cells, keepExisting=.false., fill=0)
      call realloc(waqpar%vol, waqpar%num_cells, keepExisting=.false., fill=0d0)
      call realloc(waqpar%vel, waqpar%num_cells, keepExisting=.false., fill=0d0)
      call realloc(waqpar%sal, waqpar%num_cells, keepExisting=.false., fill=0d0)
      call realloc(waqpar%tem, waqpar%num_cells, keepExisting=.false., fill=0d0)
      call realloc(waqpar%tau, waqpar%num_cells, keepExisting=.false., fill=0d0)
      call realloc(waqpar%vdf, waqpar%num_cells, keepExisting=.false., fill=0d0)
      call realloc(waqpar%kmk1, waqpar%num_cells, keepExisting=.false., fill=0)
      call realloc(waqpar%kmk2, waqpar%num_cells, keepExisting=.false., fill=0)

      call getkbotktopmax(ndxi, kb, kt, ktx)
      waqpar%ndkxi = ktx ! Maximum internal 3D node

      call waq_make_aggr_seg()

      ! Prepare arrays for sinks and sources.
      call waq_prepare_src()

      ! Prepare arrays for sinks and sources.
      call waq_prepare_lat()

      ! allocate maximum possible number of exchanges before aggregation
      waqpar%noq12 = lnx * waqpar%kmxnxa
      if (waqpar%kmxnxa > 1) then
         waqpar%num_exchanges = waqpar%noq12 + waqpar%numsrcwaq + waqpar%numlatwaq + ndxi * waqpar%kmxnxa
      else
         waqpar%num_exchanges = waqpar%noq12 + numsrc + waqpar%numlatwaq
      end if
      call realloc(waqpar%ifrmto, (/4, waqpar%num_exchanges/), keepExisting=.false., fill=0)

      call waq_make_aggr_lnk()
      call realloc(waqpar%ifrmto, (/4, waqpar%num_exchanges/), keepExisting=.true., fill=0)
      call realloc(waqpar%qag, waqpar%num_exchanges, keepExisting=.false., fill=0d0)
      call realloc(waqpar%area, waqpar%num_exchanges, keepExisting=.false., fill=0d0)
      waqpar%noql = waqpar%noq12 / waqpar%kmxnxa
   end subroutine waq_prepare_aggr
!
!------------------------------------------------------------------------------

!> Creates aggregation and mapping tables to WAQ cells.
   subroutine waq_make_aggr_seg()
      use m_flowgeom
      use m_flow
      use wrwaq

      implicit none

      integer, parameter :: kmktopbot = 0 !< Segment is both the highest and lowest layer at this node
      integer, parameter :: kmktop = 1 !< Segment is at the highest layer at this node
      integer, parameter :: kmkmiddle = 2 !< Segment is an intermediate layer at this node
      integer, parameter :: kmkbot = 3 !< Segment is at the highest layer at this node

      integer :: k, kk, kb, kt, ktx, iseg

      !   clear segment aggregation pointer
      waqpar%isaggr = 0
      waqpar%kmk1 = 0
      waqpar%kmk2 = 0

      !   2D
      do k = 1, ndx
         iseg = waqpar%iapnt(k)
         waqpar%isaggr(k) = iseg
         if (iseg > 0) then
            waqpar%kmk1(iseg) = 1 !< segment is active (note MJ: not tested yet for dry cells...)
         end if
      end do

      !   3D
      if (waqpar%kmxnxa > 1) then
         do k = 1, ndxi
            call getkbotktopmax(k, kb, kt, ktx)
            do kk = ktx, kb, -1
               iseg = waqpar%iapnt(k) + (waqpar%ilaggr(ktx - kk + 1) - 1) * waqpar%nosegl
               waqpar%isaggr(kk) = iseg
               waqpar%nosega(iseg) = waqpar%nosega(iseg) + 1
               waqpar%kmk2(iseg) = kmkmiddle
               if (ktx == kb) then
                  waqpar%kmk2(iseg) = kmktopbot
               else if (kk == ktx) then
                  waqpar%kmk2(iseg) = kmktop
               else if (kk == kb) then
                  waqpar%kmk2(iseg) = kmkbot
               end if
               waqpar%kmk1(iseg) = 1 !< segment is active
            end do
         end do

         !       also aggregate boundary nodes in 3D!
         do k = ndxi + 1, ndx
            call getkbotktopmax(k, kb, kt, ktx)
            do kk = ktx, kb, -1
               waqpar%isaggr(kk) = -((k - ndxi) + (waqpar%ilaggr(ktx - kk + 1) - 1) * (ndx - ndxi))
            end do
         end do
      end if
   end subroutine waq_make_aggr_seg
!
!------------------------------------------------------------------------------

!> Creates aggregation and mapping tables to WAQ links.
   subroutine waq_make_aggr_lnk()
      use m_flowgeom
      use m_flow
      use wrwaq
      use m_get_Lbot_Ltop_max

      implicit none

      integer :: dseg, dbnd, isrc, ilatwaq
      integer :: L, LL, Lb, Lbb, Ltx, ip, ipa, ip1, ip2, iq
      integer :: k, kk, kb, kt, ktx

      ! first 2D
      waqpar%noq12 = 0
      do L = 1, lnx
         ip1 = waqpar%iapnt(ln(1, L))
         ip2 = waqpar%iapnt(ln(2, L))

         if ((ip1 > 0 .or. ip2 > 0) .and. ip1 /= ip2) then
            ip = 0
            if (waqpar%aggre == 1 .or. waqpar%aggrel == 1) then
               do iq = 1, waqpar%noq12
                  if (waqpar%ifrmto(1, iq) == ip1 .and. waqpar%ifrmto(2, iq) == ip2) then
                     ip = iq
                     exit
                  end if
                  if (waqpar%ifrmto(1, iq) == ip2 .and. waqpar%ifrmto(2, iq) == ip1) then
                     ip = -iq
                     exit
                  end if
               end do
            end if
            if (ip == 0) then
               waqpar%noq12 = waqpar%noq12 + 1
               waqpar%ifrmto(1, waqpar%noq12) = ip1
               waqpar%ifrmto(2, waqpar%noq12) = ip2
               waqpar%iqaggr(L) = waqpar%noq12
            else
               waqpar%iqaggr(L) = ip
            end if
         end if
      end do

      if (waqpar%kmxnxa == 1) then
         ! In 2D total number of equals number of vertical exchanges
         ! Add links from sink source here!!
         if (waqpar%numsrcwaq > 0) then
            do isrc = 1, waqpar%numsrcwaq
               waqpar%ifrmto(1, waqpar%noq12 + isrc) = waqpar%ifrmtosrc(1, isrc)
               waqpar%ifrmto(2, waqpar%noq12 + isrc) = waqpar%ifrmtosrc(2, isrc)
            end do
         end if
         waqpar%noq12s = waqpar%noq12 + waqpar%numsrcwaq
         if (waqpar%numlatwaq > 0) then
            do ilatwaq = 1, waqpar%numlatwaq
               waqpar%ifrmto(1, waqpar%noq12s + ilatwaq) = waqpar%ifrmtolat(1, ilatwaq)
               waqpar%ifrmto(2, waqpar%noq12s + ilatwaq) = waqpar%ifrmtolat(2, ilatwaq)
            end do
         end if
         waqpar%noq12sl = waqpar%noq12s + waqpar%numlatwaq
         waqpar%num_exchanges = waqpar%noq12sl
      else
         ! In 3D copy aggregated 2D exchanges to all layers
         do L = 1, lnx
            call getLbotLtopmax(L, Lb, Ltx)
            ip = waqpar%iqaggr(L)
            ipa = abs(ip)
            if (ip == 0) cycle
            do LL = Ltx, Lb, -1
               waqpar%iqaggr(LL) = ip + sign((waqpar%ilaggr(Ltx - LL + 1) - 1) * waqpar%noq12, ip)
               iq = abs(waqpar%iqaggr(LL))
               dseg = (waqpar%ilaggr(Ltx - LL + 1) - 1) * waqpar%nosegl
               dbnd = (waqpar%ilaggr(Ltx - LL + 1) - 1) * (ndx - ndxi + waqpar%numsrcbnd) ! current number of external links in FM, account for sinks sources here too!
               if (waqpar%ifrmto(1, iq) == 0) then
                  if (waqpar%ifrmto(1, ipa) > 0) waqpar%ifrmto(1, iq) = waqpar%ifrmto(1, ipa) + dseg
                  if (waqpar%ifrmto(1, ipa) < 0) waqpar%ifrmto(1, iq) = waqpar%ifrmto(1, ipa) - dbnd
                  if (waqpar%ifrmto(2, ipa) > 0) waqpar%ifrmto(2, iq) = waqpar%ifrmto(2, ipa) + dseg
                  if (waqpar%ifrmto(2, ipa) < 0) waqpar%ifrmto(2, iq) = waqpar%ifrmto(2, ipa) - dbnd
               end if
            end do
            Lbb = Ltx - waqpar%kmxnxa + 1
            if (Lbb < Lb) then
               !              add extra (dummy) exchanges for boundaries below the bed to trick delwaq
               do LL = Lb - 1, Lbb, -1
                  if (waqpar%ifrmto(1, ipa) < 0 .or. waqpar%ifrmto(2, ipa) < 0) then
                     iq = ip + sign((waqpar%ilaggr(Ltx - LL + 1) - 1) * waqpar%noq12, ip)
                     dbnd = (waqpar%ilaggr(Ltx - LL + 1) - 1) * (ndx - ndxi + waqpar%numsrcbnd) ! current number of external links in FM, account for sinks sources here too!
                     if (waqpar%ifrmto(1, iq) == 0) then
                        if (waqpar%ifrmto(1, ipa) < 0) waqpar%ifrmto(1, iq) = waqpar%ifrmto(1, ipa) - dbnd
                        if (waqpar%ifrmto(2, ipa) < 0) waqpar%ifrmto(2, iq) = waqpar%ifrmto(2, ipa) - dbnd
                     end if
                  end if
               end do
            end if
         end do
         waqpar%noq12 = waqpar%noq12 * waqpar%kmxnxa
         waqpar%num_exchanges = waqpar%noq12
         ! Add links from sink source
         if (waqpar%numsrcwaq > 0) then
            do isrc = 1, waqpar%numsrcwaq
               waqpar%ifrmto(1, waqpar%noq12 + isrc) = waqpar%ifrmtosrc(1, isrc)
               waqpar%ifrmto(2, waqpar%noq12 + isrc) = waqpar%ifrmtosrc(2, isrc)
            end do
         end if
         waqpar%noq12s = waqpar%noq12 + waqpar%numsrcwaq

         ! Add links from laterals
         if (waqpar%numlatwaq > 0) then
            do ilatwaq = 1, waqpar%numlatwaq
               waqpar%ifrmto(1, waqpar%noq12s + ilatwaq) = waqpar%ifrmtolat(1, ilatwaq)
               waqpar%ifrmto(2, waqpar%noq12s + ilatwaq) = waqpar%ifrmtolat(2, ilatwaq)
            end do
         end if
         waqpar%noq12sl = waqpar%noq12s + waqpar%numlatwaq
         waqpar%num_exchanges = waqpar%noq12sl

         ! And now the vertical exchanges and pointer (note: upward flow direction in FM is reversed for WAQ!)
         do k = 1, waqpar%nosegl
            do kk = 1, waqpar%kmxnxa - 1
               iq = waqpar%num_exchanges + k + (kk - 1) * waqpar%nosegl
               waqpar%ifrmto(1, iq) = k + (kk - 1) * waqpar%nosegl
               waqpar%ifrmto(2, iq) = k + kk * waqpar%nosegl
               waqpar%ifrmto(3, iq) = max(k + (kk - 2) * waqpar%nosegl, 0)
               waqpar%ifrmto(4, iq) = 0
               if (kk < waqpar%kmxnxa - 1) waqpar%ifrmto(4, iq) = k + (kk + 1) * waqpar%nosegl
            end do
         end do
         waqpar%num_exchanges = waqpar%num_exchanges + waqpar%nosegl * (waqpar%kmxnxa - 1)
         do k = 1, ndxi
            call getkbotktopmax(k, kb, kt, ktx)
            do kk = ktx - 1, kb, -1
               if (waqpar%ilaggr(ktx - kk + 1) /= waqpar%ilaggr(ktx - kk)) then
                  waqpar%iqwaggr(kk) = waqpar%noq12sl + waqpar%iapnt(k) + (waqpar%ilaggr(ktx - kk) - 1) * waqpar%nosegl
               end if
            end do
         end do
      end if
   end subroutine waq_make_aggr_lnk
!
!------------------------------------------------------------------------------

!> Prepare additional exchanges for waq to store sink/source discharges
!! Currently, we allocate all posible combinations in an inlet-outlet situation
!! since we doe not know which one are used
   subroutine waq_prepare_src()
      use m_flowgeom
      use m_flow
      use fm_external_forcings_data
      use m_alloc
      implicit none

      integer :: ibnd, nbnd, isrc, K, K1, K2, kk
      integer :: kk1
      integer :: kk2

      waqpar%numsrcbnd = 0
      waqpar%numsrcwaq = 0
      if (numsrc == 0) return ! skip is no resources
      call realloc(ksrcwaq, numsrc, keepexisting=.false., fill=-1)
      ! First determine the number of external sink/sources and the allocations needed
      do isrc = 1, numsrc
         kk1 = ksrc(1, isrc)
         kk2 = ksrc(4, isrc)
         if (kk1 == 0 .or. kk2 == 0) then
            ! If one of the nodes is external
            if (kk1 > 0 .or. kk2 > 0) then
               ! And the other is not a ghost cell, then this is a boundary within this domain
               ksrcwaq(isrc) = waqpar%numsrcwaq
               waqpar%numsrcbnd = waqpar%numsrcbnd + 1
               waqpar%numsrcwaq = waqpar%numsrcwaq + waqpar%kmxnxa
            end if
         else
            ! This is an internal sink/source combination
            if (kk1 > 0 .and. kk2 > 0) then
               ! And the first node is not a ghost cell
               ksrcwaq(isrc) = waqpar%numsrcwaq
               waqpar%numsrcwaq = waqpar%numsrcwaq + waqpar%kmxnxa * waqpar%kmxnxa
            else if (kk1 > 0 .or. kk2 > 0) then
               ! Since we do not know the (global) cell number when one of the nodes is not in the curren domain, we cannot add the link
               ! If both are in an other domain, we simply skip this.
               write (msgbuf, '(3a)') 'Sink/source cells of ', trim(srcname(numsrc)), ' are not in the same domain. This is not yet supported in DELWAQ output!'; call err_flush()
            end if
         end if
      end do
      call realloc(waqpar%ifrmtosrc, (/2, waqpar%numsrcwaq/), keepexisting=.true., fill=0)
      call realloc(qsrcwaq, waqpar%numsrcwaq, keepexisting=.true., fill=0.0d0)
      call realloc(qsrcwaq0, waqpar%numsrcwaq, keepexisting=.true., fill=0.0d0)
      nbnd = ndx - ndxi + waqpar%numsrcbnd ! total number of boudaries
      ibnd = ndx - ndxi ! starting number for sink source boundaries

      ! Create additional pointer for sink/sources
      do isrc = 1, numsrc
         kk1 = ksrc(1, isrc)
         kk2 = ksrc(4, isrc)
         if (kk1 == 0 .or. kk2 == 0) then
            ! This is a boundary. If kk1 or kk2 is positive, then it is in the active domain
            if (kk1 > 0) then
               ibnd = ibnd + 1
               do K = 1, waqpar%kmxnxa
                  waqpar%ifrmtosrc(1, ksrcwaq(isrc) + K) = waqpar%iapnt(kk1) + (K - 1) * waqpar%nosegl
                  waqpar%ifrmtosrc(2, ksrcwaq(isrc) + K) = -ibnd - nbnd * (K - 1)
               end do
            else if (kk2 > 0) then
               ibnd = ibnd + 1
               do K = 1, waqpar%kmxnxa
                  waqpar%ifrmtosrc(1, ksrcwaq(isrc) + K) = -ibnd - nbnd * (K - 1)
                  waqpar%ifrmtosrc(2, ksrcwaq(isrc) + K) = waqpar%iapnt(kk2) + (K - 1) * waqpar%nosegl
               end do
            end if
         else
            ! This is a sink/source combination
            if (kk1 > 0 .and. kk2 > 0) then
               ! The first location is not a ghost cell.
               ! The  internal sink source should only appear in one domain!
               if (waqpar%kmxnxa > 1) then
                  do K1 = 1, waqpar%kmxnxa
                     do K2 = 1, waqpar%kmxnxa
                        kk = ksrcwaq(isrc) + K1 + (K2 - 1) * waqpar%kmxnxa
                        waqpar%ifrmtosrc(1, kk) = waqpar%iapnt(kk1) + (K1 - 1) * waqpar%nosegl
                        waqpar%ifrmtosrc(2, kk) = waqpar%iapnt(kk2) + (K2 - 1) * waqpar%nosegl
                     end do
                  end do
               else
                  waqpar%ifrmtosrc(1, ksrcwaq(isrc) + 1) = waqpar%iapnt(kk1)
                  waqpar%ifrmtosrc(2, ksrcwaq(isrc) + 1) = waqpar%iapnt(kk2)
               end if
            end if
         end if
      end do
   end subroutine waq_prepare_src
!
!------------------------------------------------------------------------------

!> Prepare additional exchanges for waq to store lateral discharges
   subroutine waq_prepare_lat()
      use m_partitioninfo, only: is_ghost_node
      use m_flowgeom
      use m_flow
      use fm_external_forcings_data
      use m_laterals, only: numlatsg, nodeCountLat, n1latsg, n2latsg, nnlat
      use m_alloc
      implicit none

      integer :: ilat, ilatwaq, ibnd, k1, kk
      logical :: firstinsection

      waqpar%numlatwaq = 0
      waqpar%numlatsectwaq = 0
      call realloc(waqpar%numlatinsectwaq, numlatsg, fill=0, keepExisting=.false.)

      ! First determine the number of laterals actually used and the allocations needed
      do ilat = 1, numlatsg
         if (nodeCountLat(ilat) > 0) then
            firstinsection = .true.
            do k1 = n1latsg(ilat), n2latsg(ilat)
               kk = nnlat(k1)
               if (kk > 0) then
                  if (.not. is_ghost_node(kk)) then
                     ! This is a lateral within the current domain
                     waqpar%numlatwaq = waqpar%numlatwaq + 1
                     waqpar%numlatinsectwaq(ilat) = waqpar%numlatinsectwaq(ilat) + 1
                     if (firstinsection) then
                        waqpar%numlatsectwaq = waqpar%numlatsectwaq + 1
                        firstinsection = .false.
                     end if
                  end if
               end if
            end do
         end if
      end do
      ! Do not skip when numlatsg is zero - we need to have the arrays allocated, even to zero length
      call realloc(waqpar%ifrmtolat, (/2, waqpar%numlatwaq/), keepexisting=.true., fill=0)
      call realloc(qlatwaq, waqpar%numlatwaq, keepexisting=.true., fill=0.0d0)
      call realloc(qlatwaq0, waqpar%numlatwaq, keepexisting=.true., fill=0.0d0)

      ibnd = (ndx - ndxi + waqpar%numsrcbnd) * waqpar%kmxnxa
      ilatwaq = 0
      do ilat = 1, numlatsg
         if (nodeCountLat(ilat) > 0) then
            do k1 = n1latsg(ilat), n2latsg(ilat)
               kk = nnlat(k1)
               if (kk > 0) then
                  if (.not. is_ghost_node(kk)) then
                     ! This is a lateral within the current domain
                     ibnd = ibnd + 1
                     ilatwaq = ilatwaq + 1
                     waqpar%ifrmtolat(1, ilatwaq) = -ibnd
                     waqpar%ifrmtolat(2, ilatwaq) = kk
                  end if
               end if
            end do
         end if
      end do
   end subroutine waq_prepare_lat
!
!------------------------------------------------------------------------------

!> Write WAQ pointer file.
   subroutine waq_wri_poi(filename)
      use m_alloc
      use wrwaq
      implicit none
      !
      !           Global variables
      !
      character(len=*), intent(in) :: filename !< Output filename.
      !
   !! executable statements -------------------------------------------------------
      !
      ! Call the waq-poi file writer
      call wrwaqpoi(waqpar%ifrmto, waqpar%num_exchanges, filename, waq_format_ascii)
   end subroutine waq_wri_poi
   !
   !------------------------------------------------------------------------------

   !> Write WAQ len file.
   !! (contains dispersion lengths of computational cells)
   subroutine waq_wri_len(lnx, dx, acl, filename)
      use m_alloc
      use wrwaq
      implicit none
      !
      !           Global variables
      !
      integer, intent(in) :: lnx !< nr of flow links (internal + boundary)
      double precision, intent(in) :: dx(lnx) !< link length (m)
      double precision, intent(in) :: acl(lnx) !< left dx fraction, 0<=alfacl<=1
      character(len=*), intent(in) :: filename !< Output filename.
      !
      !           Local variables
      !
      integer :: L, ip, kk
      integer, allocatable :: noqa(:)
      double precision, allocatable :: lenex(:, :) !< Length table: 'half' dx length from cell center to interface.
   !! lenex(1,:) = dx for left/1st  cell to interface
   !! lenex(2,:) = dx for right/2nd cell to interface
      !
   !! executable statements -------------------------------------------------------
      !
      call realloc(lenex, (/2, waqpar%num_exchanges/), keepExisting=.false., fill=0d0)
      call realloc(noqa, waqpar%noql, keepExisting=.false., fill=0)

      do L = 1, lnx
         ip = waqpar%iqaggr(L)
         if (ip > 0) then
            ! MJ: TODO for now a simple average of the dispersion lengths, may be better to weight by wu (link initial width)
            lenex(1, ip) = lenex(1, ip) + dx(L) * acl(L)
            lenex(2, ip) = lenex(2, ip) + dx(L) * (1d0 - acl(L))
            noqa(ip) = noqa(ip) + 1
         else if (ip < 0) then
            lenex(1, -ip) = lenex(1, -ip) + dx(L) * (1d0 - acl(L))
            lenex(2, -ip) = lenex(2, -ip) + dx(L) * acl(L)
            noqa(-ip) = noqa(-ip) + 1
         end if
      end do
      do ip = 1, waqpar%noql
         if (waqpar%aggre == 1) then
            lenex(1, ip) = lenex(1, ip) / dble(noqa(ip))
            lenex(2, ip) = lenex(2, ip) / dble(noqa(ip))
         end if
         ! Copy lenghts to other layers
         do kk = 1, waqpar%kmxnxa - 1
            lenex(1, ip + kk * waqpar%noql) = lenex(1, ip)
            lenex(2, ip + kk * waqpar%noql) = lenex(2, ip)
         end do
      end do

      !   dummy lengthes for sinks/sources
      do ip = waqpar%noq12 + 1, waqpar%noq12s
         lenex(1, ip) = 1d5
         lenex(2, ip) = 1d5
      end do

      !   dummy lengthes for laterals
      do ip = waqpar%noq12s + 1, waqpar%noq12sl
         lenex(1, ip) = 1d5
         lenex(2, ip) = 1d5
      end do

      !   dummy lengthes in third direction for all layers (will be calculated by WAQ from volume and surface)
      do ip = waqpar%noq12sl + 1, waqpar%num_exchanges
         lenex(1, ip) = 1d0
         lenex(2, ip) = 1d0
      end do

      ! Call the waq-len file writer
      call wrwaqlen(waqpar%num_exchanges, lenex, filename, waq_format_ascii)
      deallocate (lenex)
   end subroutine waq_wri_len
!
!------------------------------------------------------------------------------

!> Write WAQ srf file.
!! (contains horizontal surface areas of computational cells)
   subroutine waq_wri_srf(ndxi, ndx, ba, filename)
      use m_alloc
      use wrwaq
      implicit none
      !
      !           Global variables
      !
      integer, intent(in) :: ndxi !< nr of internal flowcells (internal = 2D + 1D)
      integer, intent(in) :: ndx !< nr of flow nodes (internal + boundary)
      double precision, intent(in) :: ba(ndx) !< bottom area (m2), if < 0 use table in node type
      character(len=*), intent(in) :: filename !< Output filename.
      !
      !           Local variables
      !
      integer :: k, i
      !
   !! executable statements -------------------------------------------------------
      !
      call realloc(waqpar%horsurf, waqpar%num_cells, keepExisting=.false., fill=0d0)
      !
   !! executable statements -------------------------------------------------------
      !
      ! AvD: TODO: What if ba(..) < 0.
      do k = 1, ndxi
         waqpar%horsurf(waqpar%iapnt(k)) = waqpar%horsurf(waqpar%iapnt(k)) + max(ba(k), 0d0)
      end do

      ! Copy to all layers
      if (waqpar%kmxnxa > 1) then
         do k = 1, waqpar%kmxnxa - 1
            do i = 1, waqpar%nosegl
               waqpar%horsurf(k * waqpar%nosegl + i) = waqpar%horsurf(i)
            end do
         end do
      end if

      ! Call the waq-srf file writer
      call wrwaqsrf(waqpar%horsurf, waqpar%nosegl, waqpar%kmxnxa, filename, waq_format_ascii)

   end subroutine waq_wri_srf
!
!------------------------------------------------------------------------------

!> Write WAQ atr file.
!! (contains atributes of computational cells)
   subroutine waq_wri_atr(filename)
      use m_alloc
      use wrwaq
      implicit none
      !
      !           Global variables
      !
      character(len=*), intent(in) :: filename !< Output filename.
      !
   !! executable statements -------------------------------------------------------
      !
      ! Call the waq-atr file writer
      call wrwaqatr(waqpar%nosegl, waqpar%kmxnxa, waqpar%kmk1, waqpar%kmk2, filename)

   end subroutine waq_wri_atr
!
!------------------------------------------------------------------------------

!> Write WAQ vol file.
!! (contains volumes of computational cells)
   subroutine waq_wri_vol(itim, filenamevol, lunvol)
      use m_flowgeom
      use m_flow
      use wrwaq
      use m_get_Lbot_Ltop_max
      implicit none
      !
      !           Global variables
      !
      integer, intent(in) :: itim !< time (seconds) since simulation start
      character(len=*), intent(in) :: filenamevol !< Output filename for volumes (only used when lunvol < 0).
      integer, intent(inout) :: lunvol !< File pointer for output vol-file (opened upon first call).
      !
      !           Local variables
      !
      integer :: i, k, kb, kt, ktx, kk, k1, k2, LL, L, lb, Lt, num = 0, jacheck = 0

      double precision, save, allocatable :: dv(:), dv1(:)
      double precision :: errvol
      !
   !! executable statements -------------------------------------------------------
      !
      if (waqpar%aggre == 0 .and. waqpar%kmxnxa == 1) then
         do i = 1, ndxi
            waqpar%vol(i) = vol1(i)
         end do
      else if (waqpar%aggre == 0 .and. waqpar%aggrel == 0) then

         if (jacheck == 1) then
            if (.not. allocated(dv)) then
               allocate (dv(ndkx), dv1(ndkx))
            end if

            if (num > 0) then
               dv = 0d0
               do k = 1, ndxi
                  call getkbotktopmax(k, kb, kt, ktx)
                  do kk = kb, ktx
                     dv(kk) = vol1(kk) - waqpar%vol(waqpar%isaggr(kk))
                  end do
               end do

               dv1 = 0d0
               do L = 1, lnx
                  call getLbotLtopmax(L, Lb, Lt)
                  do LL = Lb, Lt
                     k1 = ln(1, LL); k2 = ln(2, LL)
                     dv1(k1) = dv1(k1) - q1waq(LL)
                     dv1(k2) = dv1(k2) + q1waq(LL)
                  end do
               end do

               do k = 1, ndxi
                  call getkbotktopmax(k, kb, kt, ktx)
                  do kk = kb, ktx - 1
                     !if (waqpar%iqwaggr(kk)>0) then
                     dv1(kk + 1) = dv1(kk + 1) + qwwaq(kk)
                     dv1(kk) = dv1(kk) - qwwaq(kk)
                     !end if
                  end do
               end do

               do k = 1, ndxi
                  call getkbotktopmax(k, kb, kt, ktx)
                  do kk = kb, ktx
                     errvol = dv(kk) - dv1(kk)
                     if (errvol > 1d-6) then
                        errvol = 0d0
                     end if
                  end do
               end do
            end if
            num = 1
         end if

         waqpar%vol = 0d0
         do k = 1, ndxi
            call getkbotktopmax(k, kb, kt, ktx)
            do kk = kb, ktx
               waqpar%vol(waqpar%isaggr(kk)) = vol1(kk)
            end do
         end do

      else
         waqpar%vol = 0d0
         do k = 1, ndxi
            call getkbotktopmax(k, kb, kt, ktx)
            do kk = kb, ktx
               waqpar%vol(waqpar%isaggr(kk)) = waqpar%vol(waqpar%isaggr(kk)) + vol1(kk)
            end do
         end do
      end if

      ! Call the waq-vol file writer
      call wrwaqbin(itim, waqpar%vol, waqpar%num_cells, filenamevol, waq_format_ascii, lunvol)
   end subroutine waq_wri_vol
!
!------------------------------------------------------------------------------

!> Write WAQ vel file.
!! (contains flow element center velocity magnitude)
   subroutine waq_wri_vel(itim, filenamevel, lunvel)
      use m_flowgeom
      use m_flow
      use wrwaq
      use m_get_ucx_ucy_eul_mag
      implicit none
      !
      !           Global variables
      !
      integer, intent(in) :: itim !< time (seconds) since simulation start
      character(len=*), intent(in) :: filenamevel !< Output filename for vel (only used when lunvel < 0).
      integer, intent(inout) :: lunvel !< File pointer for output vel-file (opened upon first call).
      !
      !           Local variables
      !
      integer :: i, k, kb, kt, ktx, kk, n
      !
   !! executable statements -------------------------------------------------------
      !
      waqpar%vel = 0d0

      if (.not. allocated(ucmag)) then
         call realloc(ucmag, ndkx, keepExisting=.false.)
      end if

      ! Update velocity magnitudes (only available on request)
      call getucxucyeulmag(ndkx, workx, worky, ucmag, 1, 1)

      if (waqpar%aggre == 0 .and. waqpar%kmxnxa == 1) then

         do i = 1, ndxi
            if (kmx == 0) then
               n = i
            else
               n = kbot(i)
            end if
            waqpar%vel(i) = ucmag(n)
         end do
      else if (waqpar%aggre == 0 .and. waqpar%aggrel == 0) then
         do k = 1, ndxi
            call getkbotktopmax(k, kb, kt, ktx)
            do kk = kb, ktx
               waqpar%vel(waqpar%isaggr(kk)) = ucmag(kk)
            end do
         end do
      else
         ! vels are aggregated horizontal surface weighted
         do k = 1, ndxi
            call getkbotktopmax(k, kb, kt, ktx)
            do kk = kb, ktx
               waqpar%vel(waqpar%isaggr(kk)) = waqpar%vel(waqpar%isaggr(kk)) + ucmag(kk) * max(ba(k), 0d0)
            end do
         end do
         do i = 1, waqpar%num_cells
            if (waqpar%horsurf(i) > 1d-25) then
               waqpar%vel(i) = waqpar%vel(i) / waqpar%horsurf(i)
            end if
         end do
      end if

      ! Call the waq-vol file writer for vel
      call wrwaqbin(itim, waqpar%vel, waqpar%num_cells, filenamevel, waq_format_ascii, lunvel)

   end subroutine waq_wri_vel
!
!------------------------------------------------------------------------------

!> Write WAQ sal file.
!! (contains salinity of computational cells)
   subroutine waq_wri_sal(itim, filenamesal, lunsal)
      use m_flowgeom
      use m_flow
      use m_transport
      use wrwaq
      implicit none
      !
      !           Global variables
      !
      integer, intent(in) :: itim !< time (seconds) since simulation start
      character(len=*), intent(in) :: filenamesal !< Output filename for salinity (only used when lunsal < 0 and do_sal==1).
      integer, intent(inout) :: lunsal !< File pointer for output sal-file (opened upon first call, if do_sal==1).
      !
      !           Local variables
      !
      integer :: i, k, kb, kt, ktx, kk
      !
   !! executable statements -------------------------------------------------------
      !
      waqpar%sal = 0d0
      if (waqpar%aggre == 0 .and. waqpar%kmxnxa == 1) then
         do i = 1, ndxi
            if (vol1(i) > 1d-25) then
               waqpar%sal(i) = constituents(isalt, i)
            end if
         end do
      else if (waqpar%aggre == 0 .and. waqpar%aggrel == 0) then
         do k = 1, ndxi
            call getkbotktopmax(k, kb, kt, ktx)
            do kk = kb, ktx
               if (vol1(kk) > 1d-25) then
                  waqpar%sal(waqpar%isaggr(kk)) = constituents(isalt, kk)
               end if
            end do
         end do
      else
         ! Salinity is aggregated volume weighted
         do k = 1, ndxi
            call getkbotktopmax(k, kb, kt, ktx)
            do kk = kb, ktx
               if (vol1(kk) > 1d-25) then
                  waqpar%sal(waqpar%isaggr(kk)) = waqpar%sal(waqpar%isaggr(kk)) + constituents(isalt, kk) * vol1(kk)
               end if
            end do
         end do
         do i = 1, waqpar%num_cells
            if (waqpar%vol(i) > 1d-25) then
               waqpar%sal(i) = waqpar%sal(i) / waqpar%vol(i)
            else
               waqpar%sal(i) = 0d0
            end if
         end do
      end if

      ! Call the waq-vol file writer for salinity
      call wrwaqbin(itim, waqpar%sal, waqpar%num_cells, filenamesal, waq_format_ascii, lunsal)
   end subroutine waq_wri_sal
!
!------------------------------------------------------------------------------

!> Write WAQ tem file.
!! (contains temperature of computational cells)
   subroutine waq_wri_tem(itim, filenametem, luntem)
      use m_flowgeom
      use m_flow
      use wrwaq
      use m_transport, only: constituents, itemp
      implicit none
      !
      !           Global variables
      !
      integer, intent(in) :: itim !< time (seconds) since simulation start
      character(len=*), intent(in) :: filenametem !< Output filename for temperature (only used when luntem < 0 and do_tem==1).
      integer, intent(inout) :: luntem !< File pointer for output tem-file (opened upon first call, if do_tem==1).
      !
      !           Local variables
      !
      integer :: i, k, kb, kt, ktx, kk
      !
   !! executable statements -------------------------------------------------------
      !
      waqpar%tem = 0d0
      if (waqpar%aggre == 0 .and. waqpar%kmxnxa == 1) then
         do i = 1, ndxi
            if (vol1(i) > 1d-25) then
               waqpar%tem(i) = constituents(itemp, i) !  tem1(i)
            end if
         end do
      else if (waqpar%aggre == 0 .and. waqpar%aggrel == 0) then
         do k = 1, ndxi
            call getkbotktopmax(k, kb, kt, ktx)
            do kk = kb, ktx
               if (vol1(kk) > 1d-25) then
                  waqpar%tem(waqpar%isaggr(kk)) = constituents(itemp, kk)
               end if
            end do
         end do
      else
         ! Temperature is aggregated volume weighted
         do k = 1, ndxi
            call getkbotktopmax(k, kb, kt, ktx)
            do kk = kb, ktx
               if (vol1(kk) > 1d-25) then
                  waqpar%tem(waqpar%isaggr(kk)) = waqpar%tem(waqpar%isaggr(kk)) + constituents(itemp, kk) * vol1(kk)
               end if
            end do
         end do
         do i = 1, waqpar%num_cells
            if (waqpar%vol(i) > 1d-25) then
               waqpar%tem(i) = waqpar%tem(i) / waqpar%vol(i)
            else
               waqpar%tem(i) = 0d0
            end if
         end do
      end if

      ! Call the waq-vol file writer for temperature
      call wrwaqbin(itim, waqpar%tem, waqpar%num_cells, filenametem, waq_format_ascii, luntem)
   end subroutine waq_wri_tem
!
!------------------------------------------------------------------------------

!> Write WAQ tau file.
!! (contains taus at the bottom of computational cells)
   subroutine waq_wri_tau(itim, filenametau, luntau)
      use m_flowgeom
      use m_flow
      use wrwaq
      implicit none
      !
      !           Global variables
      !
      integer, intent(in) :: itim !< time (seconds) since simulation start
      character(len=*), intent(in) :: filenametau !< Output filename for tau (only used when luntau < 0).
      integer, intent(inout) :: luntau !< File pointer for output tau-file (opened upon first call).
      !
      !           Local variables
      !
      integer :: i, k, kb, kt, ktx, kk
      !
   !! executable statements -------------------------------------------------------
      !
      waqpar%tau = 0d0

      if (waqpar%aggre == 0 .and. waqpar%kmxnxa == 1) then
         do i = 1, ndxi
            waqpar%tau(i) = taus(i)
         end do
      else if (waqpar%aggre == 0 .and. waqpar%aggrel == 0) then
         do k = 1, ndxi
            call getkbotktopmax(k, kb, kt, ktx)
            do kk = kb, ktx
               waqpar%tau(waqpar%isaggr(kk)) = taus(k)
            end do
         end do
      else
         ! Taus are aggregated horizontal surface weighted
         do k = 1, ndxi
            waqpar%tau(waqpar%isaggr(k)) = waqpar%tau(waqpar%isaggr(k)) + taus(k) * max(ba(k), 0d0)
         end do
         do i = 1, waqpar%nosegl
            if (waqpar%horsurf(i) > 1d-25) then
               waqpar%tau(i) = waqpar%tau(i) / waqpar%horsurf(i)
            end if
         end do
         do i = 1, waqpar%nosegl
            do k = 1, waqpar%kmxnxa - 1
               waqpar%tau(i + k * waqpar%nosegl) = waqpar%tau(i)
            end do
         end do
      end if

      ! Call the waq-vol file writer for tau
      call wrwaqbin(itim, waqpar%tau, waqpar%num_cells, filenametau, waq_format_ascii, luntau)

   end subroutine waq_wri_tau
!
!------------------------------------------------------------------------------

!> Write WAQ vdf file.
!! (contains vertical diffusion of computational cells)
   subroutine waq_wri_vdf(itim, filenamevdf, lunvdf)
      use m_flowgeom
      use m_flow
      use wrwaq
      implicit none
      !
      !           Global variables
      !
      integer, intent(in) :: itim !< time (seconds) since simulation start
      character(len=*), intent(in) :: filenamevdf !< Output filename for vertical diffusion (only used when there's more than one layer).
      integer, intent(inout) :: lunvdf !< File pointer for output tau-file (opened upon first call).
      !           Local variables
      !
      integer :: i, k, kb, kt, ktx, kk
      double precision :: vdfmin ! help variable for WAQ minimum vertical diffusion for aggregated layers in this column
      double precision :: volsum ! help variable for WAQ summed volume for aggregated layers in this column
      !
   !! executable statements -------------------------------------------------------
      !
      waqpar%vdf = 0d0
      if (waqpar%aggre == 0 .and. waqpar%aggrel == 0) then
         do k = 1, ndxi
            call getkbotktopmax(k, kb, kt, ktx)
            do kk = kb + 1, ktx
               if (vol1(kk) > 1d-25) then
                  waqpar%vdf(waqpar%isaggr(kk)) = vicwws(kk - 1)
               end if
            end do
         end do
      else
         do k = 1, ndxi
            call getkbotktopmax(k, kb, kt, ktx)
            vdfmin = 0.0
            volsum = vol1(kb)
            do kk = kb + 1, ktx
               if (waqpar%isaggr(kk - 1) == waqpar%isaggr(kk)) then
                  ! equal to the previous layer? find next minimum, and add volume
                  if (vol1(kk) > 1d-25) then
                     if (vicwws(kk - 1) < vdfmin .or. vdfmin == 0.0) vdfmin = vicwws(kk - 1)
                     volsum = volsum + vol1(kk)
                  end if
               else
                  ! not equal to previous layer? add the (minimum) dispersion * volume vor the horizontal averaging
                  waqpar%vdf(waqpar%isaggr(kk - 1)) = waqpar%vdf(waqpar%isaggr(kk - 1)) + vdfmin * volsum
                  if (vol1(kk) > 1d-25) then
                     vdfmin = dble(vicwws(kk - 1))
                     volsum = vol1(kk)
                  end if
               end if
            end do
            ! and add the last layer
            waqpar%vdf(waqpar%isaggr(ktx)) = waqpar%vdf(waqpar%isaggr(ktx)) + vdfmin * volsum
         end do
         do i = 1, waqpar%num_cells
            if (waqpar%vol(i) > 1d-25) then
               waqpar%vdf(i) = waqpar%vdf(i) / waqpar%vol(i)
            else
               waqpar%vdf(i) = 0d0
            end if
         end do
      end if
      ! Call the waq-vol file writer for vertical diffusion
      call wrwaqbin(itim, waqpar%vdf, waqpar%num_cells, filenamevdf, waq_format_ascii, lunvdf)
   end subroutine waq_wri_vdf
!
!------------------------------------------------------------------------------
!> Write WAQ are file.
!! (contains areas at flow interfaces)
   subroutine waq_wri_are(itim, filename, lun)
      use m_flowgeom
      use m_flow
      use wrwaq
      use m_get_Lbot_Ltop_max
      implicit none
      !
      !           Global variables
      !
      integer, intent(in) :: itim !< time (seconds) since simulation start
      character(len=*), intent(in) :: filename !< Output filename (only used when lun < 0).
      integer, intent(inout) :: lun !< File pointer for output are-file.
      !
      !           Local variables
      !
      integer :: i, L, Lb, Ltx, LL, ip
      !
   !! executable statements -------------------------------------------------------
      !
      waqpar%area = 0d0

      if (waqpar%aggre == 0 .and. waqpar%kmxnxa == 1) then
         do i = 1, lnx
            waqpar%area(i) = au(i)
         end do
      else if (waqpar%aggre == 0 .and. waqpar%aggrel == 0) then
         do L = 1, lnx
            call getLbotLtopmax(L, Lb, Ltx)
            do LL = Lb, Ltx
               waqpar%area(waqpar%iqaggr(LL)) = au(LL)
            end do
         end do
      else
         do L = 1, lnx
            call getLbotLtopmax(L, Lb, Ltx)
            if (abs(waqpar%iqaggr(Lb)) > 0) then
               do LL = Lb, Ltx
                  ip = abs(waqpar%iqaggr(LL))
                  waqpar%area(ip) = waqpar%area(ip) + au(LL)
               end do
            end if
         end do
      end if

      ! dummy areas for sink/sources
      do i = waqpar%noq12 + 1, waqpar%noq12s
         waqpar%area(i) = 0.1d0
      end do

      ! dummy areas for laterals
      do i = waqpar%noq12s + 1, waqpar%noq12sl
         waqpar%area(i) = 0.1d0
      end do

      ! Add area of the vertical exchanges
      if (waqpar%kmxnxa > 1) then
         do i = 1, waqpar%num_cells - waqpar%nosegl
            waqpar%area(waqpar%noq12sl + i) = waqpar%horsurf(i)
         end do
      end if

      ! Call the waq-flo file writer
      call wrwaqbin(itim, waqpar%area, waqpar%num_exchanges, filename, waq_format_ascii, lun)
   end subroutine waq_wri_are
!
!------------------------------------------------------------------------------

!> Write WAQ flo file.
!! (contains discharges at flow interfaces)
   subroutine waq_wri_flo(itim, ti_waq, filename, lun)
      use m_flowgeom
      use m_flow
      use wrwaq
      use m_get_Lbot_Ltop_max
      implicit none
      !
      !           Global variables
      !
      integer, intent(in) :: itim !< time (seconds) since simulation start
      integer, intent(in) :: ti_waq !< time step (seconds) between two waq outputs
      character(len=*), intent(in) :: filename !< Output filename (only used when lun < 0).
      integer, intent(inout) :: lun !< File pointer for output flo-file.
      !
      !           Local variables
      !
      integer :: isrc, ilatwaq
      integer :: i, L, LL, Lb, Ltx, ip
      integer :: k, kk, kb, kt, ktx
      !
   !! executable statements -------------------------------------------------------
      !
      waqpar%qag = 0d0

      ! Average the accumulated discharges.
      if (waqpar%aggre == 0 .and. waqpar%kmxnxa == 1) then
         do i = 1, lnx
            if (kmx == 0) then
               L = i
            else
               L = Lbot(i)
            end if
            waqpar%qag(i) = q1waq(L) / dble(ti_waq)
         end do
      else if (waqpar%aggre == 0 .and. waqpar%aggrel == 0) then
         do L = 1, lnx
            call getLbotLtopmax(L, Lb, Ltx)

            do LL = Lb, Ltx
               waqpar%qag(waqpar%iqaggr(LL)) = q1waq(LL) / dble(ti_waq)
            end do
         end do
      else
         do L = 1, lnx
            call getLbotLtopmax(L, Lb, Ltx)
            if (abs(waqpar%iqaggr(Lb)) > 0) then
               do LL = Lb, Ltx
                  ip = abs(waqpar%iqaggr(LL))
                  if (waqpar%iqaggr(LL) > 0) then
                     waqpar%qag(ip) = waqpar%qag(ip) + q1waq(LL) / dble(ti_waq)
                  else
                     waqpar%qag(ip) = waqpar%qag(ip) - q1waq(LL) / dble(ti_waq)
                  end if
               end do
            end if
         end do
      end if

      ! Add sink/source dicharges
   !! TODO: write out discharges to a separe (ascii) file for additional wasteloads?
      if (waqpar%numsrcwaq > 0) then
         do isrc = 1, waqpar%numsrcwaq
            waqpar%qag(waqpar%noq12 + isrc) = qsrcwaq(isrc) / dble(ti_waq)
         end do
      end if

      ! Add laterals
      if (waqpar%numlatwaq > 0) then
         do ilatwaq = 1, waqpar%numlatwaq
            waqpar%qag(waqpar%noq12s + ilatwaq) = qlatwaq(ilatwaq) / dble(ti_waq)
         end do
      end if

      ! Add discharge of the vertical exchanges (note: upward flow direction in FM is reversed for WAQ!)
      if (waqpar%kmxnxa > 1) then
         if (waqpar%aggre == 0 .and. waqpar%aggrel == 0) then
            do k = 1, ndxi
               call getkbotktopmax(k, kb, kt, ktx)
               do kk = kb, ktx - 1
                  if (waqpar%iqwaggr(kk) > 0) then
                     waqpar%qag(waqpar%iqwaggr(kk)) = -qwwaq(kk) / dble(ti_waq)
                  end if
               end do
            end do
         else
            do k = 1, ndxi
               call getkbotktopmax(k, kb, kt, ktx)
               do kk = kb, ktx
                  if (waqpar%iqwaggr(kk) > 0) then
                     waqpar%qag(waqpar%iqwaggr(kk)) = waqpar%qag(waqpar%iqwaggr(kk)) - qwwaq(kk) / dble(ti_waq)
                  end if
               end do
            end do
         end if
      end if

      ! Call the waq-flo file writer
      call wrwaqbin(itim, waqpar%qag, waqpar%num_exchanges, filename, waq_format_ascii, lun)
   end subroutine waq_wri_flo
!
!------------------------------------------------------------------------------

!> Read an aggregation file (.dwq) into the global aggregation table.
   subroutine waq_read_dwq(ndxi, ndx, iapnt, filename)
      use unstruc_files
      implicit none
      !
      !           Global variables
      !
      integer, intent(in) :: ndxi !< Nr. of (internal) flow cells expected
      integer, intent(in) :: ndx !< Nr. of total flow cells (incl boundaries)
      integer, intent(out) :: iapnt(ndx) !< Mapping table flow cell -> waq cell (size ndx)
      character(len=*), intent(in) :: filename !< Input filename
      !
      !           Local variables
      !
      logical :: aggregate
      integer :: lundwq, istat, i, l1
      integer :: headervals(5)
      !
   !! executable statements -------------------------------------------------------
      !
      aggregate = .true.

      l1 = len_trim(filename)
      if (l1 == 0) then
         aggregate = .false.
      else
         call oldfil(lundwq, trim(filename)) ! when the file does not exist the program stops(!)
         if (lundwq == 0) then
            call mess(LEVEL_WARN, 'Unable to open the horizontal aggregation file.')
            aggregate = .false.
         else
            read (lundwq, *, iostat=istat) headervals
            if (istat /= 0) then
               call mess(LEVEL_WARN, 'Unable to read dimensions in the horizontal aggregation file.')
               aggregate = .false.
            else
               if (headervals(3) /= ndxi) then
                  call mess(LEVEL_WARN, 'The dimension of the the horizontal aggregation file (1) do not match the number of cells (2): ', headervals(3), ndxi)
                  aggregate = .false.
               else
                  ! read the aggregation array
                  read (lundwq, *, iostat=istat) iapnt(1:ndxi)
                  if (istat /= 0) then
                     call mess(LEVEL_WARN, 'Error reading the horizontal aggregation pointer.')
                     aggregate = .false.
                  end if
               end if
            end if
            close (lundwq)
         end if
      end if

      if (.not. aggregate) then
         ! if no aggregation could be read correctly, create default one to one aggregation
         call mess(LEVEL_ERROR, 'There was a problem with the aggregation file: '''//trim(filename)//'''.')
      end if

      ! add pointer for boundary nodes
      do i = ndxi + 1, ndx
         iapnt(i) = -(i - ndxi)
      end do

   end subroutine waq_read_dwq
!
!------------------------------------------------------------------------------

   function makesectionname(prefix, id) result(sectionname)
      ! Make sure the Delwaq section id contains letters and is not just a number disguised as a string
      implicit none

      character(len=*), intent(in) :: prefix !< Optional prefix
      character(len=*), intent(in) :: id !< Input D-FM id
      character(len=20) :: sectionname !< Delwaq section name

      integer(8) :: int
      real(8) :: reel
      integer :: ierrint
      integer :: ierrreel

      read (id, '(i100)', iostat=ierrint) int
      read (id, '(g100.50)', iostat=ierrreel) reel

      if (ierrint == 0 .or. ierrreel == 0) then
         ! id could be read as an integer or real, add prefix
         sectionname = trim(prefix)//id
      else
         ! use original id
         sectionname = id
      end if
   end function makesectionname

end module waq
