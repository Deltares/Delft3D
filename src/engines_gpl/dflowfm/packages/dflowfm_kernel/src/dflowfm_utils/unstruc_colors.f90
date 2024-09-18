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

! m_WEARELT movet to gridgeom

module m_textlines
   double precision :: txsize
   double precision :: txxpos
   double precision :: txypos
   character(len=60) :: txlin(3)
end module m_textlines

module unstruc_colors

   use m_WEARELT
   use M_DEVICES
   use m_textlines
!! Centralizes color definitions for unstruc.
!! Color specifications are based on Interactor.

   implicit none

   integer :: klvec = 4, klaxs = 30, klscl = 221, kltex = 3, klfra = 31, klobs = 227, klsam = 33, klzm = 31, klank = 31, klprof = 222, KLSRC = 233

   ! Color numbers for standard colors.
   integer :: ncolgray = 255
   integer :: ncolred = 252
   integer :: ncolyellow = 251
   integer :: ncolgreen = 250
   integer :: ncolcyan = 249
   integer :: ncolblue = 248
   integer :: ncolmagenta = 247
   integer :: ncolmaroon = 246
   integer :: ncoldarkgreen = 245
   integer :: ncolteal = 244
   integer :: ncolpink = 243
   integer :: ncolorange = 242
   integer :: ncollavender = 241
   integer :: ncolbrown = 240

   integer :: ncoldn = 3 !< Design net
   integer :: ncolrn = 211 !< Previous state net
   integer :: ncolnn = 89 ! 203 !< Net node dots
   integer :: ncoldg = 31 !< Design grid
   integer :: ncolrg = 212 !< Previous state grid
   integer :: ncolln = 120 !< Land boundary
   integer :: ncolsp = 204 !< Splines
   integer :: ncoltx = 210 !< Some textlines
   integer :: ncolpl = 221 !< Polygons
   integer :: ncolcrs = 230 !< Cross sections
   integer :: ncolthd = 231 !< Thin dams
   integer :: ncolfxw = 232 !< Fixed weirs
   integer :: ncolmh = 191 !< Fixed weirs
   integer :: ncolwarn1 = 191 ! warning1
   integer :: ncolwarn2 = 31 ! warning2
   integer :: ncolwarn3 = 22 ! warning3
   integer :: ncolhl = 31 ! Highlight nodes/links
   integer :: ncolANA = 63 ! 180! ANALYTIC SOLOUTIONS

   integer :: ncolblack = 254
   integer :: ncolwhite = 253

   ! colors in text screens
   ! 0 : Black       4 : Cyan
   ! 1 : Red         5 : Blue
   ! 2 : Yellow      6 : Magenta
   ! 3 : Green       7 : White

   integer :: STDFOR = 0, STDBCK = 5, & !   std
              MNUFOR = 0, MNUBCK = 4, & !   choice menu's
              INPFOR = 0, INPBCK = 4, & !   input menu's
              ERRFOR = 1, ERRBCK = 7, & !   error messages
              LBLFOR = 7, LBLBCK = 5, & !   menu names
              LINFOR = 0, LINBCK = 4, & !   lines
              TOPFOR = 1, TOPBCK = 7, & !   top line
              HLPFOR = 7, HLPBCK = 5, & !   help window
              BOTFOR = 7, BOTBCK = 5, & !   page indication
              KEYFOR = 1, KEYBCK = 4, & !   key indication
              WNDFOR = 0, WNDBCK = 4, & !   menu indication, POPUP WINDOW HELP
              SHAFOR = 7, SHABCK = 0 !   menu indication, shadow behind input forms

   integer :: nbluep
   integer :: nblues
   integer :: ngreenp
   integer :: ngreens
   integer :: nredp
   integer :: nreds

   character(len=255) :: coltabfile = ' '
   character(len=255) :: coltabfile2 = ' '

end module unstruc_colors

