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

!> read drypoints files and delete dry points from net geometry (netcells)
!! Grid enclosures are handled via the jinside=-1 option.
subroutine delete_drypoints_from_netgeom(dryptsfilelist, jaconfirm, jinside)
   use unstruc_messages
   use m_sferic, only: jsferic
   use string_module
   use m_polygon, only: NPL, ZPL, savepol, restorepol
   use m_tpoly
   use m_samples
   use m_wall_clock_time
   use m_delpol
   use m_reapol
   use m_delsam
   implicit none

   character(*), intent(inout) :: dryptsfilelist !< List of file names to process for deleting dry parts. (Supported formats: .xyz, .pol)
   integer, intent(in) :: jaconfirm !< Whether (1) or not (0) to interactively prompt for inclusion of each individual file from the list.
   integer, intent(in) :: jinside !< Override the inside check of polygon files. 0: use ZPL polygon (no override), 1: Always delete inside polygon, -1: always delete outside polygon.
   character(len=128) :: ext

   character(len=255) :: dryptsfile

   character(len=128) :: mesg

   character(len=255), dimension(:), allocatable :: fnames
   integer :: ifil

   double precision :: t0, t1

   integer :: minp, N1, N2
   integer :: ja
   integer :: ierror ! error (1) or not (0)
   logical :: jawel

   type(tpoly), dimension(:), allocatable :: pli !< tpoly-type polygons
   integer :: numpols

   type(tpoly), dimension(:), allocatable :: pli_save !< tpoly-type polygons
   integer :: numpols_save

!  store global polygon
   numpols = 0
   if (NPL > 0) then
      call pol_to_tpoly(numpols, pli, keepExisting=.false.)
   end if

!  store saved global polygon
   call restorepol()
   numpols_save = 0
   if (NPL > 0) then
      call pol_to_tpoly(numpols_save, pli_save, keepExisting=.false.)
   end if

   if (len_trim(dryptsfilelist) > 0) then
      call strsplit(dryptsfilelist, 1, fnames, 1)
   else
      goto 1234
   end if

   call mess(LEVEL_INFO, 'removing dry cells...')

   call wall_clock_time(t0)

   do ifil = 1, size(fnames)

      ierror = 1

      dryptsfile = fnames(ifil)

      if (len_trim(dryptsfile) > 0) then
         inquire (FILE=trim(dryptsfile), exist=jawel)
         if (jawel) then
            if (jaconfirm == 1) then
               ja = 0
               call confrm('Take drypointsfile '//trim(dryptsfile)//' into account?', ja)
               if (ja /= 1) then
                  ierror = 0
                  return
               end if
            end if

            ! Find file extention based on first full stop symbol '.' at the back of the string.
            N1 = index(trim(dryptsfile), '.', .true.)
            N2 = len_trim(dryptsfile)
            EXT = ' '
            if (N2 > N1) then
               EXT(1:N2 - N1 + 1) = dryptsfile(N1:N2)
            end if

            if (ext(1:4) == '.lst') then
               call cutcell_list(6, 0)
               ierror = 0
            else if (ext(1:4) == '.pol' .or. ext(1:4) == '.POL') then
               call oldfil(minp, dryptsfile)
               call savepol()
               call reapol(minp, 0)

               if (jinside /= 0) then
                  ZPL(1:NPL) = jinside
               end if

               if (jsferic == 1) then
                  call fix_global_polygons(1, 0)
               end if

               call pol_to_cellmask() ! third column in pol-file may be used to specify inside (1), or outside (0) mode, only 0 or 1 allowed.
               call delpol()
               call restorepol()

               ierror = 0
            else if (ext(1:4) == '.xyz' .or. ext(1:4) == '.XYZ') then
               call oldfil(minp, dryptsfile)
               call savesam()
               call reasam(minp, 0)
               call samples_to_cellmask2()
               call delsam(0)

               ierror = 0
            end if

            call remove_masked_netcells()

         end if
      else
         ierror = 0 ! nothing to do
      end if

      if (ierror /= 0) then
         call mess(LEVEL_ERROR, 'error reading dry-points file '//trim(dryptsfile))
      end if
   end do

   call wall_clock_time(t1)

   write (mesg, "('done in ', F12.5, ' sec.')") t1 - t0
   call mess(LEVEL_INFO, trim(mesg))

   if (allocated(fnames)) deallocate (fnames)

1234 continue

!  restore saved global polygon
   NPL = 0
   call tpoly_to_pol(pli_save)
   call dealloc_tpoly(pli_save)
   call savepol()

!  restore global polygon
   NPL = 0
   call tpoly_to_pol(pli)
   call dealloc_tpoly(pli)

   return
end subroutine delete_drypoints_from_netgeom
