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

module m_bedform_io
   !
   implicit none
   !
   public fm_rdbedformpar, fm_clrbedformpar
   !
contains
   subroutine fm_clrbedformpar(bfmpar)
      use precision
      use m_bedform_data

      implicit none

      integer :: istat
      type(bedformpar_type) :: bfmpar

      if (associated(bfmpar%duneheight)) deallocate (bfmpar%duneheight, STAT=istat)
      if (associated(bfmpar%duneheightequi)) deallocate (bfmpar%duneheightequi, STAT=istat)
      if (associated(bfmpar%dunelength)) deallocate (bfmpar%dunelength, STAT=istat)
      if (associated(bfmpar%qbedformx)) deallocate (bfmpar%qbedformx, STAT=istat)
      if (associated(bfmpar%qbedformy)) deallocate (bfmpar%qbedformy, STAT=istat)
      if (associated(bfmpar%ubedform)) deallocate (bfmpar%ubedform, STAT=istat)

      if (associated(bfmpar%rksr)) deallocate (bfmpar%rksr, STAT=istat)
      if (associated(bfmpar%rksmr)) deallocate (bfmpar%rksmr, STAT=istat)
      if (associated(bfmpar%rksd)) deallocate (bfmpar%rksd, STAT=istat)

      if (associated(bfmpar%hdpar)) deallocate (bfmpar%hdpar, STAT=istat)
      if (associated(bfmpar%ldpar)) deallocate (bfmpar%ldpar, STAT=istat)
      if (associated(bfmpar%kdpar)) deallocate (bfmpar%kdpar, STAT=istat)
      if (associated(bfmpar%cdpar)) deallocate (bfmpar%cdpar, STAT=istat)
      !
      if (associated(bfmpar%bedformD50)) deallocate (bfmpar%bedformD50, STAT=istat)
      if (associated(bfmpar%bedformD90)) deallocate (bfmpar%bedformD90, STAT=istat)
   end subroutine fm_clrbedformpar

   subroutine fm_initbedformpar(bfmpar, error)
      use precision
      use m_flowgeom
      use m_bedform_data
      use MessageHandling
      use message_module
      use unstruc_files

      implicit none

      !
      ! The following list of pointer parameters is used to point inside the data structure
      !
      character(256), pointer :: flbdfh
      !
      logical, pointer :: lfbedfrm
      logical, pointer :: lfbedfrmout
      logical, pointer :: lfbedfrmrou
      logical, pointer :: lfbedfrmCFL
      logical, pointer :: lfbedfrmADV
      logical, pointer :: lfbdfmor
      !
      integer, pointer :: bedformheighttype
      integer, pointer :: bedformlengthtype
      integer, pointer :: bdfrpt
      integer, pointer :: bdfrlxtype
      !
      real(fp), pointer :: bdfC_Hn
      real(fp), pointer :: bdfC_Hp
      real(fp), pointer :: bdfGmin
      real(fp), pointer :: bdfHmax
      real(fp), pointer :: bdfL_Hc
      real(fp), pointer :: bdfL_Hp
      real(fp), pointer :: bdfPmax
      real(fp), pointer :: bedformL_H
      real(fp), pointer :: bedformT_H
      real(fp), pointer :: bdfuni
      real(fp), pointer :: thetacdune
      !
      integer :: istat
      logical, intent(out) :: error
      !
      type(bedformpar_type), target, intent(inout) :: bfmpar
      !
  !! executable statements -------------------------------------------------------
      !

      flbdfh => bfmpar%flbdfh
      !
      lfbedfrm => bfmpar%lfbedfrm
      lfbedfrmout => bfmpar%lfbedfrmout
      lfbedfrmrou => bfmpar%lfbedfrmrou
      lfbedfrmCFL => bfmpar%lfbedfrmCFL
      lfbedfrmADV => bfmpar%lfbedfrmADV
      lfbdfmor => bfmpar%lfbdfmor
      !
      bedformheighttype => bfmpar%bedformheighttype
      bedformlengthtype => bfmpar%bedformlengthtype
      bdfrpt => bfmpar%bdfrpt
      bdfrlxtype => bfmpar%bdfrlxtype
      !
      bdfC_Hn => bfmpar%bdfC_Hn
      bdfC_Hp => bfmpar%bdfC_Hp
      bdfGmin => bfmpar%bdfGmin
      bdfHmax => bfmpar%bdfHmax
      bdfL_Hc => bfmpar%bdfL_Hc
      bdfL_Hp => bfmpar%bdfL_Hp
      bdfPmax => bfmpar%bdfPmax
      bedformL_H => bfmpar%bedformL_H
      bedformT_H => bfmpar%bedformT_H
      bdfuni => bfmpar%bdfuni
      thetacdune => bfmpar%thetacdune
      !
      ! Initialize all variables
      !
      nullify (bfmpar%duneheight)
      nullify (bfmpar%duneheightequi)
      nullify (bfmpar%dunelength)
      nullify (bfmpar%qbedformx)
      nullify (bfmpar%qbedformy)
      nullify (bfmpar%ubedform)
      nullify (bfmpar%hdpar)
      nullify (bfmpar%ldpar)
      nullify (bfmpar%kdpar)
      nullify (bfmpar%cdpar)
      nullify (bfmpar%rksr)
      nullify (bfmpar%rksmr)
      nullify (bfmpar%rksd)
      nullify (bfmpar%bedformD50)
      nullify (bfmpar%bedformD90)
      !
      flbdfh = ' '
      !
      lfbedfrm = .false.
      lfbedfrmout = .false.
      lfbedfrmrou = .false.
      lfbdfmor = .false.
      lfbedfrmADV = .true.
      lfbedfrmCFL = .false.
      !
      bedformheighttype = 0
      bedformlengthtype = 0
      bdfrpt = 0 ! default Van Rijn 2004
      bdfrlxtype = 0
      !
      bdfC_Hn = 5.5_fp
      bdfC_Hp = 3.5_fp
      bdfGmin = 0.2_fp
      bdfHmax = 5.0_fp
      bdfL_Hc = 1.0_fp
      bdfL_Hp = 2.5_fp
      bdfPmax = 3.0_fp
      bedformL_H = 0.0_fp
      bedformT_H = 1.0_fp
      bdfuni = 0.0_fp
      thetacdune = -999.0_fp
      !
      error = .false.
      !
      !-----------------------------------------------------
      ! Allocation of memory for bedform roughness arrays
      !
      istat = 0
      if (istat == 0) allocate (bfmpar%kdpar(6), stat=istat)
      if (istat == 0) allocate (bfmpar%rksr(1:ndx), stat=istat)
      if (istat == 0) allocate (bfmpar%rksmr(1:ndx), stat=istat)
      if (istat == 0) allocate (bfmpar%rksd(1:ndx), stat=istat)
      !
      if (istat /= 0) then
         write (errmsg, '(a)') 'Error in fm_initbedformpar: could not allocate memory.'
         call write_error(errmsg, unit=mdia)
         error = .true.
         return
      end if
      bfmpar%kdpar = 0.0_fp
      bfmpar%rksr = 0.01_fp
      bfmpar%rksmr = 0.0_fp
      bfmpar%rksd = 0.0_fp
      !
   end subroutine fm_initbedformpar

   subroutine fm_rdbedformpar(bfmpar, md_bedformfile, error)
      use precision
      use properties
      use table_handles
      use m_flowgeom, only: ndx, griddim, lnx
      use unstruc_files, only: mdia
      use m_sediment, only: stm_included
      use m_bedform_data
      use MessageHandling
      use message_module
      use tree_structures
      use m_depfil_stm
      !
      implicit none
      !
      ! The following list of pointer parameters is used to point inside the datastructures
      !
      character(256), pointer :: flbdfh
      character(256), pointer :: flnmD50
      character(256), pointer :: flnmD90
      logical, pointer :: lfbedfrm
      logical, pointer :: lfbedfrmout
      logical, pointer :: lfbedfrmrou
      logical, pointer :: lfbedfrmCFL
      logical, pointer :: lfbedfrmADV
      logical, pointer :: lfbdfmor
      logical, pointer :: spatial_bedform
      integer, pointer :: bedformheighttype
      integer, pointer :: bedformlengthtype
      integer, pointer :: bdfrpt
      integer, pointer :: bdfrlxtype
      real(fp), pointer :: bdfC_Hn
      real(fp), pointer :: bdfC_Hp
      real(fp), pointer :: bdfGmin
      real(fp), pointer :: bdfHmax
      real(fp), pointer :: bdfL_Hc
      real(fp), pointer :: bdfL_Hp
      real(fp), pointer :: bdfPmax
      real(fp), dimension(:), pointer :: bedformD50
      real(fp), dimension(:), pointer :: bedformD90
      real(fp), pointer :: bedformL_H
      real(fp), pointer :: bedformT_H
      real(fp), pointer :: bdfuni
      real(fp), pointer :: thetacdune
      real(fp), dimension(:), pointer :: hdpar
      real(fp), dimension(:), pointer :: ldpar
      real(fp), dimension(:), pointer :: cdpar
      real(fp), dimension(:), pointer :: kdpar
      real(fp), dimension(:), pointer :: duneheight
      real(fp), dimension(:), pointer :: duneheightequi
      real(fp), dimension(:), pointer :: dunelength
      real(fp), dimension(:), pointer :: qbedformx
      real(fp), dimension(:), pointer :: qbedformy
      real(fp), dimension(:), pointer :: ubedform
      real(fp), dimension(:), pointer :: rksr
      real(fp), dimension(:), pointer :: rksmr
      real(fp), dimension(:), pointer :: rksd
      type(tree_data), pointer :: md_bfmptr
      character(len=255), intent(in) :: md_bedformfile
      !real(fp)                        , pointer :: tunit

      logical, intent(out) :: error
      type(bedformpar_type), target, intent(inout) :: bfmpar
      !
      ! Local variables
      !
      character(20) :: ctemp
      character(60) :: txtput2
      character(40) :: txtput1
      real :: rtemp
      logical :: bdfhfile_exists
      logical :: hdread
      logical :: ldread
      logical :: success
      logical :: successD50
      logical :: successD90
      logical :: existD50
      logical :: existD90
      integer :: istat
      integer :: i
      integer :: readerr
      character(11) :: fmttmp ! Format file ('formatted  ')
      !
  !! executable statements -------------------------------------------------------
      !
      bdfC_Hn => bfmpar%bdfC_Hn
      bdfC_Hp => bfmpar%bdfC_Hp
      bdfGmin => bfmpar%bdfGmin
      bdfHmax => bfmpar%bdfHmax
      bdfL_Hc => bfmpar%bdfL_Hc
      bdfL_Hp => bfmpar%bdfL_Hp
      bdfPmax => bfmpar%bdfPmax
      bdfrpt => bfmpar%bdfrpt
      bdfrlxtype => bfmpar%bdfrlxtype
      bdfuni => bfmpar%bdfuni
      spatial_bedform => bfmpar%spatial_bedform
      flnmD50 => bfmpar%flnmD50
      flnmD90 => bfmpar%flnmD90
      bedformheighttype => bfmpar%bedformheighttype
      bedformlengthtype => bfmpar%bedformlengthtype
      bedformL_H => bfmpar%bedformL_H
      bedformT_H => bfmpar%bedformT_H
      flbdfh => bfmpar%flbdfh
      lfbedfrm => bfmpar%lfbedfrm
      lfbedfrmout => bfmpar%lfbedfrmout
      lfbedfrmrou => bfmpar%lfbedfrmrou
      lfbedfrmCFL => bfmpar%lfbedfrmCFL
      lfbedfrmADV => bfmpar%lfbedfrmADV
      lfbdfmor => bfmpar%lfbdfmor
      thetacdune => bfmpar%thetacdune
      !
      error = .false.
      !
      ! Open bedform file for parameter extraction
      !
      call tree_create(trim(md_bedformfile), md_bfmptr)
      !
      call prop_inifile(trim(md_bedformfile), md_bfmptr, readerr) ! without preprocessing the mdu-file
      !
      if (readerr /= 0) then
         istat = -1
         call mess(LEVEL_ERROR, 'bedform_io::fm_rdbedformpar - Error opening file ', trim(md_bedformfile), '.')
      end if
      !
      ! Read Bedform sediment diameter
      !
      if (.not. stm_included) then
         call prop_get(md_bfmptr, 'bedform', 'BdfD50', flnmD50, successD50)
         call prop_get(md_bfmptr, 'bedform', 'BdfD90', flnmD90, successD90)
         !
         ! check if D50 or D90 is spatial varying; if only one is given, both are treated so.
         ! JRE/AvD to do: depfil to FM version
         !
         if (.not. successD50 .and. .not. successD90) then
            spatial_bedform = .false.
            existD50 = .false.
            existD90 = .false.
         else
            if (successD50) then
               inquire (file=trim(flnmD50), exist=existD50)
            else
               existD50 = .false.
            end if
            if (successD90) then
               inquire (file=trim(flnmD90), exist=existD90)
            else
               existD90 = .false.
            end if
            spatial_bedform = (existD50 .or. existD90)
         end if
         if (spatial_bedform) then
            allocate (bfmpar%bedformD50(1:ndx), stat=istat)
            if (istat == 0) allocate (bfmpar%bedformD90(1:ndx), stat=istat)
         else
            allocate (bfmpar%bedformD50(1), stat=istat)
            if (istat == 0) allocate (bfmpar%bedformD90(1), stat=istat)
         end if
         if (istat /= 0) then
            call write_error('RDBEDFORMPAR: Could not allocate memory for D50/D90 arrays', unit=mdia)
            error = .true.
            return
         end if
         bedformD50 => bfmpar%bedformD50
         bedformD90 => bfmpar%bedformD90
         !
         ! default values:
         !
         bedformD50 = 0.0002_fp
         bedformD90 = 0.0003_fp

         if (successD50) then
            if (.not. existD50) then
               call prop_get(md_bfmptr, 'bedform', 'BdfD50', bedformD50(1), success)
               if (.not. success) then
                  write (errmsg, '(a,a,a)') 'Error in rdbedformpar: ', trim(flnmD50), ' is not a file and not a value.'
                  call write_error(errmsg, unit=mdia)
                  error = .true.
                  return
               end if
               if (spatial_bedform) then
                  bedformD50(:) = bedformD50(1)
               end if
            else
               !
               fmttmp = 'formatted'
               call depfil_stm_double(mdia, error, flnmD50, fmttmp, &
                         & bedformD50, 1, 1, griddim)
               if (error) then
                  write (errmsg, '(a,a,a)') 'Error while reading bedformD50 from file ', trim(flnmD50), '.'
                  call write_error(errmsg, unit=mdia)
                  error = .true.
                  return
               end if
            end if
         end if
         if (successD90) then
            if (.not. existD90) then
               call prop_get(md_bfmptr, 'bedform', 'BdfD90', bedformD90(1), success)
               if (.not. success) then
                  write (errmsg, '(a,a,a)') 'Error in rdbedformpar: ', trim(flnmD90), ' is not a file and not a value.'
                  call write_error(errmsg, unit=mdia)
                  error = .true.
                  return
               end if
               if (spatial_bedform) then
                  bedformD90(:) = bedformD90(1)
               end if
            else
               fmttmp = 'formatted'
               call depfil_stm_double(mdia, error, flnmD90, fmttmp, &
                         & bedformD90, 1, 1, griddim)
               if (error) then
                  write (errmsg, '(3a)') 'Error while reading bedformD90 from file ', trim(flnmD90), '.'
                  call write_error(errmsg, unit=mdia)
                  error = .true.
                  return
               end if
            end if
         else
            ! The do loop is needed (instead of an array operation), because otherwise
            ! the full array is copied to the stack. This goes wrong with big models
            !
            if (spatial_bedform) then
               do i = 1, ndx
                  bedformD90(i) = 1.5_fp * bedformD50(i)
               end do
            else
               bedformD90(1) = 1.5_fp * bedformD50(1)
            end if
         end if
      end if
      !
      call prop_get(md_bfmptr, 'bedform', 'Bdf', lfbedfrm)
      !
      call prop_get(md_bfmptr, 'bedform', 'BdfOut', lfbedfrmout)
      !
      kdpar => bfmpar%kdpar
      rksr => bfmpar%rksr
      rksmr => bfmpar%rksmr
      rksd => bfmpar%rksd
      !
      ! if Bdf keyword turned out to be NO,
      ! then try to read only Van Rijn 2004 bedform roughness height parameters.
      !
      if (.not. lfbedfrm) goto 8888
      !
      ! Allocation of memory for other arrays
      !
      istat = 0
      if (istat == 0) allocate (bfmpar%duneheight(1:ndx), stat=istat)
      if (istat == 0) allocate (bfmpar%duneheightequi(1:ndx), stat=istat)
      if (istat == 0) allocate (bfmpar%dunelength(1:ndx), stat=istat)
      if (istat == 0) allocate (bfmpar%qbedformx(1:lnx), stat=istat)
      if (istat == 0) allocate (bfmpar%qbedformy(1:lnx), stat=istat)
      if (istat == 0) allocate (bfmpar%ubedform(1:lnx), stat=istat)
      !
      if (istat == 0) allocate (bfmpar%hdpar(2), stat=istat)
      if (istat == 0) allocate (bfmpar%ldpar(2), stat=istat)
      if (istat == 0) allocate (bfmpar%cdpar(2), stat=istat)
      !
      if (istat /= 0) then
         write (errmsg, '(a)') 'Error in rdbedformpar: could not allocate memory.'
         call write_error(errmsg, unit=mdia)
         error = .true.
         return
      end if
      !
      duneheight => bfmpar%duneheight
      duneheightequi => bfmpar%duneheightequi
      dunelength => bfmpar%dunelength
      qbedformx => bfmpar%qbedformx
      qbedformy => bfmpar%qbedformy
      ubedform => bfmpar%ubedform
      !
      cdpar => bfmpar%cdpar
      hdpar => bfmpar%hdpar
      ldpar => bfmpar%ldpar
      !
      duneheight = 0.0_fp
      duneheightequi = 0.0_fp
      dunelength = 0.0_fp
      qbedformx = 0.0_fp
      qbedformy = 0.0_fp
      ubedform = 0.0_fp
      !
      hdpar = 0.0_fp
      ldpar = 0.0_fp
      cdpar = 0.0_fp
      !-----------------------------------------------------
      !
      write (mdia, '(a)') '*** Start of bedform input'
      !
      ! If BdfMor then the morphological time scale is used for bedform adaptation.
      ! By default the hydrodynamic time scale is used for bedform adaptation.
      !
      if (stm_included) then
         call prop_get(md_bfmptr, 'bedform', 'BdfMor', lfbdfmor)
         !
         txtput1 = 'Bedform adaptation based on'
         if (lfbdfmor) then
            write (mdia, '(a,a)') txtput1, ': morphologic time scale'
         else
            write (mdia, '(a,a)') txtput1, ': hydrodynamic time scale'
         end if
      else
         write (mdia, '(a)') 'Morphology module not active in present simulation.'
         txtput1 = 'Using characteristic sediment diameters'
         write (mdia, '(a,a)') txtput1, ':'
         txtput1 = '  D50 (m)'
         write (mdia, '(a,a,e20.4)') txtput1, ':', bedformD50
         txtput1 = '  D90 (m)'
         write (mdia, '(a,a,e20.4)') txtput1, ':', bedformD90
      end if
      !
      !---------------------------
      ! Reading choice for Bedform height
      !
      ctemp = ''
      call prop_get(md_bfmptr, 'bedform', 'BdfH', ctemp)
      call small(ctemp, 20)
      select case (ctemp)
      case ('vanrijn84')
         bedformheighttype = 1
         txtput2 = 'Van Rijn (1984)'
      case ('fredsoempm')
         bedformheighttype = 2
         txtput2 = 'Fredsoe (1982) for MPM (1948)'
      case ('fredsoeeh')
         bedformheighttype = 3
         txtput2 = 'Fredsoe (1982) for EH (1967)'
      case ('powerrelation')
         bedformheighttype = 4
         txtput2 = 'Power relation aH*(H^bH)'
      case default
         bedformheighttype = 1
         txtput2 = 'Van Rijn (1984)'
      end select
      txtput1 = 'Dune height predictor'
      write (mdia, '(a,a,a)') txtput1, ': ', txtput2
      !
      select case (bedformheighttype)
      case (1:3)
         hdpar(1) = 1.0_fp !Epsilon parameter in FredsoeMPM and FredsoeEH
         !Constant between 0.5 and 1.5 (APPENDIX A of report Q4190)
         call prop_get(md_bfmptr, 'bedform', 'BdfEps', hdpar(1))
         !
         txtput1 = '  eps'
         write (mdia, '(a,a,e20.4)') txtput1, ':', hdpar(1)
         !
         if (bedformheighttype <= 2) then
            call prop_get(md_bfmptr, 'bedform', 'BdfThC', thetacdune)
            if (thetacdune < 0.0) then
               write (mdia, '(a)') '  Critical shear stress for dunes calculated from shields curve'
            else
               txtput1 = '  Critical shear stress for dunes'
               write (mdia, '(a,a,e20.4)') txtput1, ':', thetacdune
            end if
         end if
      case (4)
         hdpar(1) = 0.0_fp
         hdpar(2) = 1.0_fp
         !
         call prop_get(md_bfmptr, 'bedform', 'BdfaH', hdpar(1))
         call prop_get(md_bfmptr, 'bedform', 'BdfbH', hdpar(2))
         if (hdpar(1) <= 0.0) then
            write (errmsg, '(a)') 'Error in rdbedformpar: bedform height BdfaH <= 0.0 m.'
            call write_error(errmsg, unit=mdia)
            error = .true.
            return
         end if
         !
         txtput1 = '  aH'
         write (mdia, '(a,a,e20.4)') txtput1, ':', hdpar(1)
         txtput1 = '  bH'
         write (mdia, '(a,a,e20.4)') txtput1, ':', hdpar(2)
      end select
      !
      !---------------------------
      ! Reading choice for Bedform relaxation
      !
      ctemp = ''
      call prop_get(md_bfmptr, 'bedform', 'BdfRlx', ctemp)
      call small(ctemp, 20)
      select case (ctemp)
      case ('thconst')
         bdfrlxtype = 1
         txtput2 = 'Constant T_H'
      case ('lhconst')
         bdfrlxtype = 2
         txtput2 = 'T_H = L_H / C_H'
      case ('lhfactor')
         bdfrlxtype = 3
         txtput2 = 'T_H = L_H / C_H'
      case ('lhchhmax')
         if (.not. stm_included) then
            write (errmsg, '(a)') 'Error in rdbedformpar: lhchhmax bedfrom relaxation specified without sediment transport.'
            call write_error(errmsg, unit=mdia)
            error = .true.
            return
         end if
         bdfrlxtype = 4
         txtput2 = 'T_H = L_H / C_H'
      case default
         bdfrlxtype = 0
         txtput2 = 'None (equilibrium)'
      end select
      !
      txtput1 = 'Dune height relaxation'
      write (mdia, '(a,a,a)') txtput1, ': ', txtput2
      !
      select case (bdfrlxtype)
      case (1)
         rtemp = 0.0_fp
         call prop_get(md_bfmptr, 'bedform', 'BdfT_H', rtemp) !Read adaptation time scale in seconds
         bedformT_H = rtemp
         !
         txtput1 = '  T_H (Tunit)'
         write (mdia, '(a,a,e20.4,a,a)') txtput1, ':', bedformT_H, ' ', 'Tunit'
      case (2)
         call prop_get(md_bfmptr, 'bedform', 'BdfL_H', bedformL_H) !Read adaptation length scale
         !
         txtput1 = '  L_H (m)'
         write (mdia, '(a,a,e20.4)') txtput1, ':', bedformL_H
      case (3)
         txtput1 = 'Length scale L_H'
         txtput2 = 'LHc*Ld'
         write (mdia, '(a,a,a)') txtput1, ': ', txtput2
         !
         call prop_get(md_bfmptr, 'bedform', 'BdfLHc', bdfL_Hc) !Read adaptation length scale factor
         !
         txtput1 = '  LHc (-)'
         write (mdia, '(a,a,e20.4)') txtput1, ':', bdfL_Hc
      case (4)
         txtput1 = 'Length scale L_H'
         txtput2 = 'min[(H_max/H)^LHp,phi_max]*Ld'
         write (mdia, '(a,a,a)') txtput1, ': ', txtput2
         !
         call prop_get(md_bfmptr, 'bedform', 'BdfHMx', bdfHmax) !Read maximum water depth
         call prop_get(md_bfmptr, 'bedform', 'BdfLHp', bdfL_Hp) !Read length scale phi power
         call prop_get(md_bfmptr, 'bedform', 'BdfPMx', bdfPmax) !Read maximum phi
         !
         txtput1 = '  H_max'
         write (mdia, '(a,a,e20.4)') txtput1, ':', bdfHmax
         txtput1 = '  LHp'
         write (mdia, '(a,a,e20.4)') txtput1, ':', bdfL_Hp
         txtput1 = '  phi_max'
         write (mdia, '(a,a,e20.4)') txtput1, ':', bdfPmax
      end select
      !
      !---------------------------
      ! Reading choice for Bedform advection
      !
      if (bdfRlxtype > 0) then
         call prop_get(md_bfmptr, 'bedform', 'BdfADV', lfbedfrmADV)
         !
         txtput1 = 'Dune height advection'
         txtput2 = '                  NO'
         if (lfbedfrmADV) txtput2 = '                 YES'
         write (mdia, '(a,a,a)') txtput1, ': ', txtput2
         !
         if (lfbedfrmADV) then
            call prop_get(md_bfmptr, 'bedform', 'BdfCFL', lfbedfrmCFL)
            !
            txtput1 = 'CFL check'
            txtput2 = '                  NO'
            if (lfbedfrmCFL) txtput2 = '                 YES'
            write (mdia, '(a,a,a)') txtput1, ': ', txtput2
         end if
         !
         if (lfbedfrmADV .or. bdfRlxtype > 1) then
            select case (bdfRlxtype)
            case (1:3)
               txtput1 = 'Celerity C_H'
               txtput2 = 'Power Relation aC*(U^bC)'
               write (mdia, '(a,a,a)') txtput1, ': ', txtput2
               !
               cdpar(1) = 0.0_fp
               cdpar(2) = 1.0_fp
               !
               call prop_get(md_bfmptr, 'bedform', 'BdfaC', cdpar(1))
               call prop_get(md_bfmptr, 'bedform', 'BdfbC', cdpar(2))
               !
               txtput1 = '  aC'
               write (mdia, '(a,a,e20.4)') txtput1, ':', cdpar(1)
               txtput1 = '  bC'
               write (mdia, '(a,a,e20.4)') txtput1, ':', cdpar(2)
            case (4)
               txtput1 = 'Celerity C_H'
               txtput2 = 'max[(H/H_max)^CHp,gamma_min] * CHn * S / [H * (1-Fr^2)]'
               write (mdia, '(a,a,a)') txtput1, ': ', txtput2
               !
               call prop_get(md_bfmptr, 'bedform', 'BdfCHp', bdfC_Hp) !Read bedform migration speed gamma power
               call prop_get(md_bfmptr, 'bedform', 'BdfCHn', bdfC_Hn) !Read bedform migration non-linearity parameter
               call prop_get(md_bfmptr, 'bedform', 'BdfGMn', bdfGmin) !Read bedform maximum gamma
               !
               txtput1 = '  CHp'
               write (mdia, '(a,a,e20.4)') txtput1, ':', bdfC_Hp
               txtput1 = '  CHn'
               write (mdia, '(a,a,e20.4)') txtput1, ':', bdfC_Hn
               txtput1 = '  gamma_min'
               write (mdia, '(a,a,e20.4)') txtput1, ':', bdfGmin
            end select
         end if
      end if
      !
      !---------------------------
      ! Reading choice for Bedform length
      !
      ctemp = ''
      call prop_get(md_bfmptr, 'bedform', 'BdfL', ctemp)
      call small(ctemp, 20)
      select case (ctemp)
      case ('vanrijn84')
         bedformlengthtype = 1
         txtput2 = 'Van Rijn (1984)'
      case ('powerrelation')
         bedformlengthtype = 2
         txtput2 = 'Power relation aL*(H^bL)'
      case default
         bedformlengthtype = 1
         txtput2 = 'Van Rijn (1984)'
      end select
      txtput1 = 'Dune length predictor'
      write (mdia, '(a,a,a)') txtput1, ': ', txtput2
      !
      if (bedformlengthtype == 2) then
         ldpar(1) = 0.0_fp
         ldpar(2) = 1.0_fp
         !
         call prop_get(md_bfmptr, 'bedform', 'BdfaL', ldpar(1))
         call prop_get(md_bfmptr, 'bedform', 'BdfbL', ldpar(2))
         if (ldpar(1) <= 0.0) then
            write (errmsg, '(a)') 'Error in rdbedformpar: Dune length coefficients in .mdu <= 0.0 m.'
            call write_error(errmsg, unit=mdia)
            error = .true.
            return
         end if
         !
         txtput1 = '  aL'
         write (mdia, '(a,a,e20.4)') txtput1, ':', ldpar(1)
         txtput1 = '  bL'
         write (mdia, '(a,a,e20.4)') txtput1, ':', ldpar(2)
      end if
      !
      !---------------------------
      ! Reading choice for Bedform roughness predictor
      !
      ctemp = ''
      call prop_get(md_bfmptr, 'bedform', 'BdfRou', ctemp)
      call small(ctemp, 20)
      select case (ctemp)
      case ('vanrijn07')
         bdfrpt = 0
         txtput2 = 'Van Rijn (2007)'
      case ('vanrijn84')
         bdfrpt = 1
         txtput2 = 'Van Rijn (1984)'
      case ('powerrelation')
         bdfrpt = 2
         txtput2 = 'Power Relation aR*(Hd^bR)'
      case default
         bdfrpt = 1
         txtput2 = 'Van Rijn (1984)'
      end select
      txtput1 = 'Dune roughness height predictor'
      write (mdia, '(a,a,a)') txtput1, ': ', txtput2
      !
8888  continue
      ! if Bdf keyword turned out to be NO, then bdfrpt will be 0 (Van Rijn 2004).
      ! read those parameters
      !
      select case (bdfrpt)
      case (0)
         kdpar(1) = 1.0_fp
         kdpar(2) = 0.0_fp
         kdpar(3) = 0.0_fp
         !
         kdpar(4) = 0.0_fp
         kdpar(5) = 0.0_fp
         kdpar(6) = 0.0_fp
         !
         call prop_get(md_bfmptr, 'bedform', 'BdfRpC', kdpar(1))
         call prop_get(md_bfmptr, 'bedform', 'BdfMrC', kdpar(2))
         call prop_get(md_bfmptr, 'bedform', 'BdfDnC', kdpar(3))
         !
         call prop_get(md_bfmptr, 'bedform', 'BdfRpR', kdpar(4))
         call prop_get(md_bfmptr, 'bedform', 'BdfMrR', kdpar(5))
         call prop_get(md_bfmptr, 'bedform', 'BdfDnR', kdpar(6))
         !
         if (lfbedfrm) then
            txtput1 = '  Ripple calibration (-)'
            write (mdia, '(a,a,e20.4)') txtput1, ':', kdpar(1)
            txtput1 = '  Ripple relaxation factor (-)'
            write (mdia, '(a,a,e20.4)') txtput1, ':', kdpar(4)
            !
            txtput1 = '  Mega-ripple calibration (-)'
            write (mdia, '(a,a,e20.4)') txtput1, ':', kdpar(2)
            txtput1 = '  Mega-ripple relaxation factor (-)'
            write (mdia, '(a,a,e20.4)') txtput1, ':', kdpar(5)
            !
            txtput1 = '  Dune calibration (-)'
            write (mdia, '(a,a,e20.4)') txtput1, ':', kdpar(3)
            txtput1 = '  Dune relaxation factor (-)'
            write (mdia, '(a,a,e20.4)') txtput1, ':', kdpar(6)
         end if
      case (2)
         kdpar(1) = 0.0_fp
         kdpar(2) = 1.0_fp
         !
         call prop_get(md_bfmptr, 'bedform', 'BdfaR', kdpar(1))
         call prop_get(md_bfmptr, 'bedform', 'BdfbR', kdpar(2))
         if (kdpar(1) <= 0.0) then
            write (errmsg, '(a)') 'Error in rdbedformpar: Dune roughness coefficients <= 0.0 m.'
            call write_error(errmsg, unit=mdia)
            error = .true.
            return
         end if
         !
         txtput1 = '  aR'
         write (mdia, '(a,a,e20.4)') txtput1, ':', kdpar(1)
         txtput1 = '  bR'
         write (mdia, '(a,a,e20.4)') txtput1, ':', kdpar(2)
      end select
      !
      ! if Bdf keyword turned out to be NO, skip remainder
      !
      if (.not. lfbedfrm) goto 9999
      !
      !---------------------------
      ! Reading initial dune height/dune length
      !
      ! JRE TO DO: add restart form netcdf file
      !
      !call restart_bdf_from_trim(lundia   ,nmaxus   ,mmax     ,duneheight, &
      !                         & hdread   ,dunelength,ldread  ,gdp      )
      !
      hdread = .false.
      ldread = .false.
      if (.not. hdread) then
         !
         ! First assume that 'BdfUni' contains a filename
         ! If the file does not exist, assume that 'BdfUni' contains a uniform value (real)
         !
         call prop_get(md_bfmptr, 'bedform', 'BdfUni', flbdfh)
         !
         ! Intel 7.0 crashes on an inquire statement when file = ' '
         !
         if (flbdfh == ' ') flbdfh = 'dummyname'
         inquire (file=flbdfh, exist=bdfhfile_exists)
         txtput1 = 'Initial dune height'
         if (.not. bdfhfile_exists) then
            flbdfh = ' '
            call prop_get(md_bfmptr, 'bedform', 'BdfUni', bdfuni)
            duneheight = bdfuni
            write (mdia, '(a,a,e20.4)') txtput1, ':', bdfuni
         else
            !
            !Now if the bedformheightfile exists fill duneheight with the specified information
            !
            fmttmp = 'formatted'
            call depfil_stm_double(mdia, error, flbdfh, fmttmp, &
                      & duneheight, 1, 1, griddim)
            !
            write (mdia, '(a,a,a)') txtput1, ': ', flbdfh
            if (error) then
               errmsg = 'bedform_io::fm_rdbedformpar: error reading initial bedform heights from BdfUni'
               call write_error(errmsg, unit=mdia)
               return
            end if
         end if
      end if
      !
      if (.not. ldread) then
         txtput1 = 'Initial dune length'
         dunelength = 0.0_fp
         write (mdia, '(a,a,e20.4)') txtput1, ':', 0.0_fp
      end if
      !
      write (mdia, '(a)') '*** End of bedform input'
      write (mdia, *)
      !
9999  continue

   end subroutine fm_rdbedformpar

end module m_bedform_io
