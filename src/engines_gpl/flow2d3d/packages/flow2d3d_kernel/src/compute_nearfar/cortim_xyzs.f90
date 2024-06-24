subroutine cortim_xyzs(lun    ,no_modules   ,nrow, modules, modstart, x_cor, y_cor, z_cor, s_cor,bv_cor,bh_cor,v_cor)
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
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
!  $Id: cortim_xyzs.f90 5888 2016-02-24 10:14:54Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/cortim_xyzs.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Read the data from the cortim output file
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
!
! Global variables
!
    integer                                                    , intent(in)    :: lun
    integer                                                    , intent(in)    :: no_modules
    integer                                                    , intent(in)    :: nrow
    integer      , dimension(no_modules)                       , intent(inout) :: modstart
    real(fp)     , dimension(nrow)                             , intent(inout) :: x_cor
    real(fp)     , dimension(nrow)                             , intent(inout) :: y_cor
    real(fp)     , dimension(nrow)                             , intent(inout) :: z_cor
    real(fp)     , dimension(nrow)                             , intent(inout) :: s_cor
    real(fp)     , dimension(nrow)                             , intent(inout) :: bv_cor
    real(fp)     , dimension(nrow)                             , intent(inout) :: bh_cor
    real(fp)     , dimension(nrow)                             , intent(inout) :: v_cor
    character*256, dimension(no_modules)                       , intent(inout) :: modules

!
! Local variables
!
    integer                                     :: ix
    integer                                     :: ix1
    integer                                     :: ix2
    integer                                     :: iocond
    integer                                     :: irow
    real(fp)                                    :: xx
    real(fp)                                    :: yy
    real(fp)                                    :: zz
    real(fp)                                    :: ss
    real(fp)                                    :: rdum
    real(fp)                                    :: bv
    real(fp)                                    :: bh
    character*256                               :: record
    character*256                               :: record_org
    logical                                     :: found
!
!! executable statements -------------------------------------------------------
!
    irow       = 0
    imodule    = 0
    iocond     = 0

    do while (iocond == 0)
       read (lun,'(a256)',iostat = iocond) record
       call small(record,len(record))

       ix1 = index (record,'begin')
       ix2 = index (record,'mod')

       !
       !  The begin of a module!
       !

       if (ix1 /= 0 .and. ix2 /=0) then
          record_org = record(ix2:ix2+5)
          found = .false.
          do while (.not. found)
             read (lun,'(a256)',iostat = iocond) record
             call small (record,len(record))
             ix1 = index (record,'x        y       z')
             ix2 = index (record,'end')
             ix3 = index (record,'mod')

             !
             ! The begin of a trajectory
             !

             if (ix1 /= 0) then
                found   = .true.
                imodule = imodule + 1
                modules (imodule) = trim(record_org)
                modstart(imodule) = irow + 1
                ix2    = 0
                ix3    = 0

                !
                ! Read the trajectory (and belonging information) until the end of the module is encountered
                !

                 do while (ix2 == 0 .and. ix3 == 0)
                   read (lun,'(a256)',iostat = iocond) record
                   call small(record,len(record))
                   ix2 = index (record,'end')
                   ix3 = index (record,'mod')
                   read (record,*, iostat=iocond) xx,yy,zz,ss,rdum,bv,bh
                   if (iocond == 0) then
                      irow = irow + 1
                      x_cor(irow)  = xx
                      y_cor(irow)  = yy
                      z_cor(irow)  = zz
                      s_cor(irow)  = ss
                      !
                      if (modules(imodule)(1:len(modules(imodule))) == 'mod110') then
                         !
                         ! bh contains jet centreline velocity in case of corjet
                         !             jet width (horizontal) in other cases
                         v_cor(irow)  = bh
                         bh_cor(irow) = bv
                         bv_cor(irow) = 0.0_fp
                         
                      else
                         bh_cor(irow) = bh
                         bv_cor(irow) = bv
                      endif
                   endif
                enddo
                iocond = 0
             elseif (ix2 /= 0 .and. ix3 /=0) then
                found = .true.
             endif
          enddo
       endif
    enddo
end subroutine cortim_xyzs
