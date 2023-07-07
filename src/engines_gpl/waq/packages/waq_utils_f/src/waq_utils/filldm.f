!!  Copyright (C)  Stichting Deltares, 2012-2023.
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
      module m_filldm

      implicit none

      contains


      subroutine filldm(elmdms    ,ielem     ,dm1       ,dm2       ,
     *                  dm3       ,dm4       ,dm5       ,dm6       )
      implicit none
!-----------------------------------------------------------------------
!             Module: SUBROUTINE FILLDM
!           Function: Write element dimensions in array elmdms
!        Method used:
!               Date: 08-06-1995
!         Programmer: A. Hoekstra, H.H. Leepel
!         CVS header
!            $Author: Beek_j $
!              $Date: 17-03-03 15:38 $
!            $Source: /u/trisula/cvsroot/trisula/output/filldm.f,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Calling routines:            numerous
!-----------------------------------------------------------------------
!   Called  routines:              NONE
!-----------------------------------------------------------------------
!          Constants:
!
! Const.      Type
!
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! DM1     I   I*4                  Number of dimensions
! DM2     I   I*4                  Size of first dimension
! DM3     I   I*4                  Size of second dimension
! DM4     I   I*4                  Size of third dimension
! DM5     I   I*4                  Size of fourth dimension
! DM6     I   I*4                  Size of fifth dimension
! ELMDMS   O  I*4   6,*            Array containing info about the
!                                  element dimensions ELMDMS(1,*) is
!                                  the number of dimensions
!                                  ELMDMS(2-ELMDMS(1,*),*) is the size
!                                  of each dimension. The size of the
!                                  array is (6,NELEMS).
! IELEM   I   I*4                  Index number of element in group
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
!-----------------------------------------------------------------------
!
!  declaration and specification
!
      integer elmdms( 6, *), ielem,
     *        dm1   ,dm2   ,dm3   ,dm4   ,dm5   ,dm6
!-----------------------------------------------------------------------
!-----define element dimensions
!-----------------------------------------------------------------------
      elmdms(1,ielem) = dm1
      elmdms(2,ielem) = dm2
      elmdms(3,ielem) = dm3
      elmdms(4,ielem) = dm4
      elmdms(5,ielem) = dm5
      elmdms(6,ielem) = dm6
!
      end
      end module m_filldm
