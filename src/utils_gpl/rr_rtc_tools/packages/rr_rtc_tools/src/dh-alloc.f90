!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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

! DOC
!
!  dh-alloc.f90  Subroutines for allocating and initialisation of variables (arrays)
!
!  Copyright (C) 2003 Geert Prinsen  WL|Deltares
!
!  General information:
!  This module contains a number of auxilary subroutines that can be
!  used to handle allocation and initialisation
!  Public functions include:
!  - dh_allocate       allocate an array
!                      The allocatable array may be a logical, character, integer, real or double array
!                      it may be 1,2,3,4 dimensional
!                      It is declared by the user using pointer (real,                  pointer, save :: array1(:,:)
!                                                          (or   character(len=maxlen), pointer, save :: array2(:,:)
!  - dh_allocinit      allocate and initialise an allocatable array
!
!
!  Limitations:
!   - up to 4 dimensional arrays
!   - up to 3 arrays of the same type and rank in 1 call
!   - initialisation value should be of the same type as the arrays
!        (so a double precision value 0D0 for double precision arrays)
!
!   - General calling format
!     Assume your own declarations
!        real,                  pointer, save :: array1(:,:), .. arraym(:,:)
!     or character(len=maxlen), pointer, save :: array1(:,:,:), .. arraym(:,:,:)
!     Use
!       success = Dh_allocate  (Dim1, ..., Dimn, Array1,.. Arraym)
!       success = Dh_allocinit (Dim1, ..., Dimn, Array1,.. Arraym, value)
!     With
!       n=number of dimensions (at most 4)
!       m=number of arrays     (at most 3)
!
!
! ENDDOC
!
!  $Author$
!  $Date$
!  $Source$
!
! --------------------------------------------------------------------
!   Module:    DH_ALLOC
!   Author:    Geert Prinsen
!   Purpose:   Handle allocation/initialisation of allocatable arrays in Fortran 90
!   Context:   To be used by applications
! --------------------------------------------------------------------
!
    module dh_alloc

      implicit none

       interface dh_allocate
! 1d array
          module procedure dh_alloc_1darray_integer
          module procedure dh_alloc_1darray_real
          module procedure dh_alloc_1darray_double
          module procedure dh_alloc_1darray_character
          module procedure dh_alloc_1darray_logical
! 2d array
          module procedure dh_alloc_2darray_integer
          module procedure dh_alloc_2darray_real
          module procedure dh_alloc_2darray_double
          module procedure dh_alloc_2darray_character
          module procedure dh_alloc_2darray_logical
! 3d array
          module procedure dh_alloc_3darray_integer
          module procedure dh_alloc_3darray_real
          module procedure dh_alloc_3darray_double
          module procedure dh_alloc_3darray_character
          module procedure dh_alloc_3darray_logical
! 4d array
          module procedure dh_alloc_4darray_integer
          module procedure dh_alloc_4darray_real
          module procedure dh_alloc_4darray_double
          module procedure dh_alloc_4darray_character
          module procedure dh_alloc_4darray_logical
! set of 1d arrays
          module procedure dh_alloc_1d_integer_2arrays
          module procedure dh_alloc_1d_real_2arrays
          module procedure dh_alloc_1d_double_2arrays
          module procedure dh_alloc_1d_char_2arrays
          module procedure dh_alloc_1d_logical_2arrays
          module procedure dh_alloc_1d_integer_3arrays
          module procedure dh_alloc_1d_real_3arrays
          module procedure dh_alloc_1d_double_3arrays
          module procedure dh_alloc_1d_char_3arrays
          module procedure dh_alloc_1d_logical_3arrays
! set of 2d arrays
          module procedure dh_alloc_2d_integer_2arrays
          module procedure dh_alloc_2d_real_2arrays
          module procedure dh_alloc_2d_double_2arrays
          module procedure dh_alloc_2d_char_2arrays
          module procedure dh_alloc_2d_logical_2arrays
          module procedure dh_alloc_2d_integer_3arrays
          module procedure dh_alloc_2d_real_3arrays
          module procedure dh_alloc_2d_double_3arrays
          module procedure dh_alloc_2d_char_3arrays
          module procedure dh_alloc_2d_logical_3arrays
! set of 3d arrays
          module procedure dh_alloc_3d_integer_2arrays
          module procedure dh_alloc_3d_real_2arrays
          module procedure dh_alloc_3d_double_2arrays
          module procedure dh_alloc_3d_char_2arrays
          module procedure dh_alloc_3d_logical_2arrays
          module procedure dh_alloc_3d_integer_3arrays
          module procedure dh_alloc_3d_real_3arrays
          module procedure dh_alloc_3d_double_3arrays
          module procedure dh_alloc_3d_char_3arrays
          module procedure dh_alloc_3d_logical_3arrays
! set of 4d arrays
          module procedure dh_alloc_4d_integer_2arrays
          module procedure dh_alloc_4d_real_2arrays
          module procedure dh_alloc_4d_double_2arrays
          module procedure dh_alloc_4d_char_2arrays
          module procedure dh_alloc_4d_logical_2arrays
          module procedure dh_alloc_4d_integer_3arrays
          module procedure dh_alloc_4d_real_3arrays
          module procedure dh_alloc_4d_double_3arrays
          module procedure dh_alloc_4d_char_3arrays
          module procedure dh_alloc_4d_logical_3arrays
       end interface

       interface dh_allocinit
! 1d array
          module procedure dh_allocinit_1darray_integer
          module procedure dh_allocinit_1darray_real
          module procedure dh_allocinit_1darray_double
          module procedure dh_allocinit_1darray_character
          module procedure dh_allocinit_1darray_logical
! 2d array
          module procedure dh_allocinit_2darray_integer
          module procedure dh_allocinit_2darray_real
          module procedure dh_allocinit_2darray_double
          module procedure dh_allocinit_2darray_character
          module procedure dh_allocinit_2darray_logical
! 3d array
          module procedure dh_allocinit_3darray_integer
          module procedure dh_allocinit_3darray_real
          module procedure dh_allocinit_3darray_double
          module procedure dh_allocinit_3darray_character
          module procedure dh_allocinit_3darray_logical
! 4d array
          module procedure dh_allocinit_4darray_integer
          module procedure dh_allocinit_4darray_real
          module procedure dh_allocinit_4darray_double
          module procedure dh_allocinit_4darray_character
          module procedure dh_allocinit_4darray_logical
! set of 1d arrays
          module procedure dh_allocinit_1d_integer_2arrays
          module procedure dh_allocinit_1d_real_2arrays
          module procedure dh_allocinit_1d_double_2arrays
          module procedure dh_allocinit_1d_char_2arrays
          module procedure dh_allocinit_1d_logical_2arrays
          module procedure dh_allocinit_1d_integer_3arrays
          module procedure dh_allocinit_1d_real_3arrays
          module procedure dh_allocinit_1d_double_3arrays
          module procedure dh_allocinit_1d_char_3arrays
          module procedure dh_allocinit_1d_logical_3arrays
! set of 2d arrays
          module procedure dh_allocinit_2d_integer_2arrays
          module procedure dh_allocinit_2d_real_2arrays
          module procedure dh_allocinit_2d_double_2arrays
          module procedure dh_allocinit_2d_char_2arrays
          module procedure dh_allocinit_2d_logical_2arrays
          module procedure dh_allocinit_2d_integer_3arrays
          module procedure dh_allocinit_2d_real_3arrays
          module procedure dh_allocinit_2d_double_3arrays
          module procedure dh_allocinit_2d_char_3arrays
          module procedure dh_allocinit_2d_logical_3arrays
! set of 3d arrays
          module procedure dh_allocinit_3d_integer_2arrays
          module procedure dh_allocinit_3d_real_2arrays
          module procedure dh_allocinit_3d_double_2arrays
          module procedure dh_allocinit_3d_char_2arrays
          module procedure dh_allocinit_3d_logical_2arrays
          module procedure dh_allocinit_3d_integer_3arrays
          module procedure dh_allocinit_3d_real_3arrays
          module procedure dh_allocinit_3d_double_3arrays
          module procedure dh_allocinit_3d_char_3arrays
          module procedure dh_allocinit_3d_logical_3arrays
! set of 4d arrays
          module procedure dh_allocinit_4d_integer_2arrays
          module procedure dh_allocinit_4d_real_2arrays
          module procedure dh_allocinit_4d_double_2arrays
          module procedure dh_allocinit_4d_char_2arrays
          module procedure dh_allocinit_4d_logical_2arrays
          module procedure dh_allocinit_4d_integer_3arrays
          module procedure dh_allocinit_4d_real_3arrays
          module procedure dh_allocinit_4d_double_3arrays
          module procedure dh_allocinit_4d_char_3arrays
          module procedure dh_allocinit_4d_logical_3arrays
       end interface


      contains

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_integer_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 1D arrays
!   Arguments:  names of 1d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_1d_integer_2arrays (dim1, array1, array2) result(retVal)

      integer dim1
      integer, pointer, dimension(:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, array1)
      retVal = retVal .and. dh_allocate (dim1, array2)

      return
      end function dh_alloc_1d_integer_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_real_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 1D real arrays
!   Arguments:  names of 1d real array
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_1d_real_2arrays (dim1, array1, array2) result(retVal)

      integer dim1
      real, pointer, dimension(:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, array1)
      retVal = retVal .and. dh_allocate (dim1, array2)

      return
      end function dh_alloc_1d_real_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_double_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 1D double arrays
!   Arguments:  names of 1d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_1d_double_2arrays (dim1, array1, array2) result(retVal)

      integer dim1
      double precision, pointer, dimension(:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, array1)
      retVal = retVal .and. dh_allocate (dim1, array2)

      return
      end function dh_alloc_1d_double_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_char_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 1D arrays
!   Arguments:  names of 1d character arrays
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_1d_char_2arrays (dim1, array1, array2) result(retVal)

      integer dim1
      character(len=*), pointer, dimension(:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, array1)
      retVal = retVal .and. dh_allocate (dim1, array2)

      return
      end function dh_alloc_1d_char_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_logical_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 1D logical arrays
!   Arguments:  names of 1d logical arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_1d_logical_2arrays (dim1, array1, array2) result(retVal)

      integer dim1
      logical, pointer, dimension(:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, array1)
      retVal = retVal .and. dh_allocate (dim1, array2)

      return
      end function dh_alloc_1d_logical_2arrays


! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_integer_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 1D arrays
!   Arguments:  names of 1d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_1d_integer_3arrays (dim1, array1, array2, array3) result(retVal)

      integer dim1
      integer, pointer, dimension(:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, array1)
      retVal = retVal .and. dh_allocate (dim1, array2)
      retVal = retVal .and. dh_allocate (dim1, array3)

      return
      end function dh_alloc_1d_integer_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_real_arrays_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 1D real arrays
!   Arguments:  names of 1d real array
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_1d_real_3arrays (dim1, array1, array2, array3) result(retVal)

      integer dim1
      real, pointer, dimension(:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, array1)
      retVal = retVal .and. dh_allocate (dim1, array2)
      retVal = retVal .and. dh_allocate (dim1, array3)

      return
      end function dh_alloc_1d_real_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_double_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 1D double arrays
!   Arguments:  names of 1d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_1d_double_3arrays (dim1, array1, array2, array3) result(retVal)

      integer dim1
      double precision, pointer, dimension(:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, array1)
      retVal = retVal .and. dh_allocate (dim1, array2)
      retVal = retVal .and. dh_allocate (dim1, array3)

      return
      end function dh_alloc_1d_double_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_char_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 1D arrays
!   Arguments:  names of 1d character arrays
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_1d_char_3arrays (dim1, array1, array2, array3) result(retVal)

      integer dim1
      character(len=*), pointer, dimension(:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, array1)
      retVal = retVal .and. dh_allocate (dim1, array2)
      retVal = retVal .and. dh_allocate (dim1, array3)

      return
      end function dh_alloc_1d_char_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_logical_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 1D logical arrays
!   Arguments:  names of 1d logical arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_1d_logical_3arrays (dim1, array1, array2, array3) result(retVal)

      integer dim1
      logical, pointer, dimension(:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, array1)
      retVal = retVal .and. dh_allocate (dim1, array2)
      retVal = retVal .and. dh_allocate (dim1, array3)

      return
      end function dh_alloc_1d_logical_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_integer_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 2D arrays
!   Arguments:  names of 2d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_2d_integer_2arrays (dim1, dim2, array1, array2) result(retVal)

      integer dim1, dim2
      integer, pointer, dimension(:,:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, dim2, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, array2)

      return
      end function dh_alloc_2d_integer_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_real_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 2D real arrays
!   Arguments:  names of 2d real array
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_2d_real_2arrays (dim1, dim2, array1, array2) result(retVal)

      integer dim1, dim2
      real, pointer, dimension(:,:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, dim2, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, array2)

      return
      end function dh_alloc_2d_real_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_double_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 1D double arrays
!   Arguments:  names of 2d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_2d_double_2arrays (dim1, dim2, array1, array2) result(retVal)

      integer dim1, dim2
      double precision, pointer, dimension(:,:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, dim2, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, array2)

      return
      end function dh_alloc_2d_double_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_char_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D arrays
!   Arguments:  names of 2d character arrays
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_2d_char_2arrays (dim1, dim2, array1, array2) result(retVal)

      integer dim1, dim2
      character(len=*), pointer, dimension(:,:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, dim2, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, array2)

      return
      end function dh_alloc_2d_char_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_logical_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D logical arrays
!   Arguments:  names of 2d logical arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_2d_logical_2arrays (dim1, dim2, array1, array2) result(retVal)

      integer dim1, dim2
      logical, pointer, dimension(:,:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, dim2, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, array2)

      return
      end function dh_alloc_2d_logical_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_integer_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D arrays
!   Arguments:  names of 2d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_2d_integer_3arrays (dim1, dim2, array1, array2, array3) result(retVal)

      integer dim1, dim2
      integer, pointer, dimension(:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, dim2, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, array2)
      retVal = retVal .and. dh_allocate (dim1, dim2, array3)

      return
      end function dh_alloc_2d_integer_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_real_arrays_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D real arrays
!   Arguments:  names of 2d real array
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_2d_real_3arrays (dim1, dim2, array1, array2, array3) result(retVal)

      integer dim1, dim2
      real, pointer, dimension(:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, dim2, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, array2)
      retVal = retVal .and. dh_allocate (dim1, dim2, array3)

      return
      end function dh_alloc_2d_real_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_double_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D double arrays
!   Arguments:  names of 2d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_2d_double_3arrays (dim1, dim2, array1, array2, array3) result(retVal)

      integer dim1, dim2
      double precision, pointer, dimension(:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, dim2, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, array2)
      retVal = retVal .and. dh_allocate (dim1, dim2, array3)

      return
      end function dh_alloc_2d_double_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_char_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D arrays
!   Arguments:  names of 2d character arrays
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_2d_char_3arrays (dim1, dim2, array1, array2, array3) result(retVal)

      integer dim1, dim2
      character(len=*), pointer, dimension(:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, dim2, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, array2)
      retVal = retVal .and. dh_allocate (dim1, dim2, array3)

      return
      end function dh_alloc_2d_char_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_logical_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D logical arrays
!   Arguments:  names of 2d logical arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_2d_logical_3arrays (dim1, dim2, array1, array2, array3) result(retVal)

      integer dim1, dim2
      logical, pointer, dimension(:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, dim2, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, array2)
      retVal = retVal .and. dh_allocate (dim1, dim2, array3)

      return
      end function dh_alloc_2d_logical_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_3d_integer_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 3D arrays
!   Arguments:  names of 3d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_3d_integer_2arrays (dim1, dim2, dim3, array1, array2) result(retVal)

      integer dim1, dim2, dim3
      integer, pointer, dimension(:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, array2)

      return
      end function dh_alloc_3d_integer_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_3d_real_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 3D real arrays
!   Arguments:  names of 3d real array
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_3d_real_2arrays (dim1, dim2, dim3, array1, array2) result(retVal)

      integer dim1, dim2, dim3
      real, pointer, dimension(:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, array2)

      return
      end function dh_alloc_3d_real_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_3d_double_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 3D double arrays
!   Arguments:  names of 3d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_3d_double_2arrays (dim1, dim2, dim3, array1, array2) result(retVal)

      integer dim1, dim2, dim3
      double precision, pointer, dimension(:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, array2)

      return
      end function dh_alloc_3d_double_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_3d_char_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 3D arrays
!   Arguments:  names of 3d character arrays
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_3d_char_2arrays (dim1, dim2, dim3, array1, array2) result(retVal)

      integer dim1, dim2, dim3
      character(len=*), pointer, dimension(:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, array2)

      return
      end function dh_alloc_3d_char_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_3d_logical_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 3D logical arrays
!   Arguments:  names of 3d logical arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_3d_logical_2arrays (dim1, dim2, dim3, array1, array2) result(retVal)

      integer dim1, dim2, dim3
      logical, pointer, dimension(:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, array2)

      return
      end function dh_alloc_3d_logical_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_3d_integer_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D arrays
!   Arguments:  names of 3d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_3d_integer_3arrays (dim1, dim2, dim3, array1, array2, array3) result(retVal)

      integer dim1, dim2, dim3
      integer, pointer, dimension(:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, array2)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, array3)

      return
      end function dh_alloc_3d_integer_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_3d_real_arrays_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D real arrays
!   Arguments:  names of 3d real array
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_3d_real_3arrays (dim1, dim2, dim3, array1, array2, array3) result(retVal)

      integer dim1, dim2, dim3
      real, pointer, dimension(:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, array2)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, array3)

      return
      end function dh_alloc_3d_real_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_3d_double_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D double arrays
!   Arguments:  names of 3d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_3d_double_3arrays (dim1, dim2, dim3, array1, array2, array3) result(retVal)

      integer dim1, dim2, dim3
      double precision, pointer, dimension(:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, array2)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, array3)

      return
      end function dh_alloc_3d_double_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_3d_char_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D arrays
!   Arguments:  names of 3d character arrays
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_3d_char_3arrays (dim1, dim2, dim3, array1, array2, array3) result(retVal)

      integer dim1, dim2, dim3
      character(len=*), pointer, dimension(:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, array2)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, array3)

      return
      end function dh_alloc_3d_char_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_logical_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D logical arrays
!   Arguments:  names of 2d logical arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_3d_logical_3arrays (dim1, dim2, dim3, array1, array2, array3) result(retVal)

      integer dim1, dim2, dim3
      logical, pointer, dimension(:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, array2)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, array3)

      return
      end function dh_alloc_3d_logical_3arrays


! --------------------------------------------------------------------
!   Subroutine: dh_alloc_4d_integer_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 4D arrays
!   Arguments:  names of 4d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_4d_integer_2arrays (dim1, dim2, dim3, dim4, array1, array2) result(retVal)

      integer dim1, dim2, dim3, dim4
      integer, pointer, dimension(:,:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, dim4, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, dim4, array2)

      return
      end function dh_alloc_4d_integer_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_4d_real_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 4D real arrays
!   Arguments:  names of 4d real array
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_4d_real_2arrays (dim1, dim2, dim3, dim4, array1, array2) result(retVal)

      integer dim1, dim2, dim3, dim4
      real, pointer, dimension(:,:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, dim4, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, dim4, array2)

      return
      end function dh_alloc_4d_real_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_4d_double_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 4D double arrays
!   Arguments:  names of 4d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_4d_double_2arrays (dim1, dim2, dim3, dim4, array1, array2) result(retVal)

      integer dim1, dim2, dim3, dim4
      double precision, pointer, dimension(:,:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, dim4, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, dim4, array2)

      return
      end function dh_alloc_4d_double_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_4d_char_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 4D arrays
!   Arguments:  names of 4d character arrays
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_4d_char_2arrays (dim1, dim2, dim3, dim4, array1, array2) result(retVal)

      integer dim1, dim2, dim3, dim4
      character(len=*), pointer, dimension(:,:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, dim4, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, dim4, array2)

      return
      end function dh_alloc_4d_char_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_4d_logical_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 4D logical arrays
!   Arguments:  names of 4d logical arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_4d_logical_2arrays (dim1, dim2, dim3, dim4, array1, array2) result(retVal)

      integer dim1, dim2, dim3, dim4
      logical, pointer, dimension(:,:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, dim4, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, dim4, array2)

      return
      end function dh_alloc_4d_logical_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_4d_integer_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D arrays
!   Arguments:  names of 4d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_4d_integer_3arrays (dim1, dim2, dim3, dim4, array1, array2, array3) result(retVal)

      integer dim1, dim2, dim3, dim4
      integer, pointer, dimension(:,:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, dim4, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, dim4, array2)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, dim4, array3)

      return
      end function dh_alloc_4d_integer_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_4d_real_arrays_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D real arrays
!   Arguments:  names of 4d real array
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_4d_real_3arrays (dim1, dim2, dim3, dim4, array1, array2, array3) result(retVal)

      integer dim1, dim2, dim3, dim4
      real, pointer, dimension(:,:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, dim4, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, dim4, array2)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, dim4, array3)

      return
      end function dh_alloc_4d_real_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_4d_double_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D double arrays
!   Arguments:  names of 4d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_4d_double_3arrays (dim1, dim2, dim3, dim4, array1, array2, array3) result(retVal)

      integer dim1, dim2, dim3, dim4
      double precision, pointer, dimension(:,:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, dim4, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, dim4, array2)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, dim4, array3)

      return
      end function dh_alloc_4d_double_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_4d_char_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D arrays
!   Arguments:  names of 4d character arrays
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_4d_char_3arrays (dim1, dim2, dim3, dim4, array1, array2, array3) result(retVal)

      integer dim1, dim2, dim3, dim4
      character(len=*), pointer, dimension(:,:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, dim4, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, dim4, array2)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, dim4, array3)

      return
      end function dh_alloc_4d_char_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_4d_logical_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D logical arrays
!   Arguments:  names of 2d logical arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_4d_logical_3arrays (dim1, dim2, dim3, dim4, array1, array2, array3) result(retVal)

      integer dim1, dim2, dim3, dim4
      logical, pointer, dimension(:,:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocate (dim1, dim2, dim3, dim4, array1)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, dim4, array2)
      retVal = retVal .and. dh_allocate (dim1, dim2, dim3, dim4, array3)

      return
      end function dh_alloc_4d_logical_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_1d_integer_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 1D arrays
!   Arguments:  names of 1d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_1d_integer_2arrays (dim1, array1, array2, value) result(retVal)

      integer dim1, value
      integer, pointer, dimension(:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, array2, value)

      return
      end function dh_allocinit_1d_integer_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_real_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 1D real arrays
!   Arguments:  names of 1d real array
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_1d_real_2arrays (dim1, array1, array2, value) result(retVal)

      integer dim1
      real    value
      real, pointer, dimension(:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, array2, value)

      return
      end function dh_allocinit_1d_real_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_double_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 1D double arrays
!   Arguments:  names of 1d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_1d_double_2arrays (dim1, array1, array2, value) result(retVal)

      integer dim1
      double precision value
      double precision, pointer, dimension(:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, array2, value)

      return
      end function dh_allocinit_1d_double_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_char_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 1D character arrays
!   Arguments:  names of 1d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_1d_char_2arrays (dim1, array1, array2, value) result(retVal)

      integer dim1
      character(len=*) value
      character(len=*), pointer, dimension(:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, array2, value)

      return
      end function dh_allocinit_1d_char_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_logical_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 1D logical arrays
!   Arguments:  names of 1d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_1d_logical_2arrays (dim1, array1, array2, value) result(retVal)

      integer dim1
      logical value
      logical, pointer, dimension(:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, array2, value)

      return
      end function dh_allocinit_1d_logical_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_integer_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 1D arrays
!   Arguments:  names of 1d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_1d_integer_3arrays (dim1, array1, array2, array3, value) result(retVal)

      integer dim1, value
      integer, pointer, dimension(:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, array3, value)

      return
      end function dh_allocinit_1d_integer_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_real_arrays_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 1D real arrays
!   Arguments:  names of 1d real array
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_1d_real_3arrays (dim1, array1, array2, array3, value) result(retVal)

      integer dim1
      real    value
      real, pointer, dimension(:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, array3, value)

      return
      end function dh_allocinit_1d_real_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_double_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 1D double arrays
!   Arguments:  names of 1d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_1d_double_3arrays (dim1, array1, array2, array3, value) result(retVal)

      integer dim1
      double precision value
      double precision, pointer, dimension(:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, array3, value)

      return
      end function dh_allocinit_1d_double_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_char_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 1D character arrays
!   Arguments:  names of 1d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_1d_char_3arrays (dim1, array1, array2, array3, value) result(retVal)

      integer dim1
      character(len=*) value
      character(len=*), pointer, dimension(:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, array3, value)

      return
      end function dh_allocinit_1d_char_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1d_logical_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 1D logical arrays
!   Arguments:  names of 1d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_1d_logical_3arrays (dim1, array1, array2, array3, value) result(retVal)

      integer dim1
      logical value
      logical, pointer, dimension(:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, array3, value)

      return
      end function dh_allocinit_1d_logical_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_integer_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 2D arrays
!   Arguments:  names of 2d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_2d_integer_2arrays (dim1, dim2, array1, array2, value) result(retVal)

      integer dim1, dim2, value
      integer, pointer, dimension(:,:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, dim2, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, array2, value)

      return
      end function dh_allocinit_2d_integer_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_real_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 2D real arrays
!   Arguments:  names of 2d real array
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_2d_real_2arrays (dim1, dim2, array1, array2, value) result(retVal)

      integer dim1, dim2
      real    value
      real, pointer, dimension(:,:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, dim2, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, array2, value)

      return
      end function dh_allocinit_2d_real_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_double_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 1D double arrays
!   Arguments:  names of 2d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_2d_double_2arrays (dim1, dim2, array1, array2, value) result(retVal)

      integer dim1, dim2
      double precision value
      double precision, pointer, dimension(:,:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, dim2, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, array2, value)

      return
      end function dh_allocinit_2d_double_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_char_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 1D character arrays
!   Arguments:  names of 2d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_2d_char_2arrays (dim1, dim2, array1, array2, value) result(retVal)

      integer dim1, dim2
      character(len=*) value
      character(len=*), pointer, dimension(:,:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, dim2, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, array2, value)

      return
      end function dh_allocinit_2d_char_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_logical_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 1D logical arrays
!   Arguments:  names of 2d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_2d_logical_2arrays (dim1, dim2, array1, array2, value) result(retVal)

      integer dim1, dim2
      logical value
      logical, pointer, dimension(:,:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, dim2, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, array2, value)

      return
      end function dh_allocinit_2d_logical_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_integer_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D arrays
!   Arguments:  names of 2d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_2d_integer_3arrays (dim1, dim2, array1, array2, array3, value) result(retVal)

      integer dim1, dim2, value
      integer, pointer, dimension(:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, dim2, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, array3, value)

      return
      end function dh_allocinit_2d_integer_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_real_arrays_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D real arrays
!   Arguments:  names of 2d real array
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_2d_real_3arrays (dim1, dim2, array1, array2, array3, value) result(retVal)

      integer dim1, dim2
      real    value
      real, pointer, dimension(:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, dim2, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, array3, value)

      return
      end function dh_allocinit_2d_real_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_double_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D double arrays
!   Arguments:  names of 2d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_2d_double_3arrays (dim1, dim2, array1, array2, array3, value) result(retVal)

      integer dim1, dim2
      double precision value
      double precision, pointer, dimension(:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, dim2, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, array3, value)

      return
      end function dh_allocinit_2d_double_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_char_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D character arrays
!   Arguments:  names of 2d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_2d_char_3arrays (dim1, dim2, array1, array2, array3, value) result(retVal)

      integer dim1, dim2
      character(len=*) value
      character(len=*), pointer, dimension(:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, dim2, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, array3, value)

      return
      end function dh_allocinit_2d_char_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2d_logical_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D logical arrays
!   Arguments:  names of 2d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_2d_logical_3arrays (dim1, dim2, array1, array2, array3, value) result(retVal)

      integer dim1, dim2
      logical value
      logical, pointer, dimension(:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, dim2, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, array3, value)

      return
      end function dh_allocinit_2d_logical_3arrays


! --------------------------------------------------------------------
!   Subroutine: dh_alloc_3d_integer_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 3D arrays
!   Arguments:  names of 3d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_3d_integer_2arrays (dim1, dim2, dim3, array1, array2, value) result(retVal)

      integer dim1, dim2, dim3, value
      integer, pointer, dimension(:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, array2, value)

      return
      end function dh_allocinit_3d_integer_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_3d_real_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 3D real arrays
!   Arguments:  names of 3d real array
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_3d_real_2arrays (dim1, dim2, dim3, array1, array2, value) result(retVal)

      integer dim1, dim2, dim3
      real    value
      real, pointer, dimension(:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, array2, value)

      return
      end function dh_allocinit_3d_real_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_3d_double_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 3D double arrays
!   Arguments:  names of 3d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_3d_double_2arrays (dim1, dim2, dim3, array1, array2, value) result(retVal)

      integer dim1, dim2, dim3
      double precision value
      double precision, pointer, dimension(:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, array2, value)

      return
      end function dh_allocinit_3d_double_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_3d_char_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 3D arrays
!   Arguments:  names of 3d character arrays
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_3d_char_2arrays (dim1, dim2, dim3, array1, array2, value) result(retVal)

      integer dim1, dim2, dim3
      character(len=*) value
      character(len=*), pointer, dimension(:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, array2, value)

      return
      end function dh_allocinit_3d_char_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_3d_logical_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 3D logical arrays
!   Arguments:  names of 3d logical arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_3d_logical_2arrays (dim1, dim2, dim3, array1, array2, value) result(retVal)

      integer dim1, dim2, dim3
      logical value
      logical, pointer, dimension(:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, array2, value)

      return
      end function dh_allocinit_3d_logical_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_3d_integer_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D arrays
!   Arguments:  names of 3d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_3d_integer_3arrays (dim1, dim2, dim3, array1, array2, array3, value) result(retVal)

      integer dim1, dim2, dim3, value
      integer, pointer, dimension(:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, array3, value)

      return
      end function dh_allocinit_3d_integer_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_3d_real_arrays_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D real arrays
!   Arguments:  names of 3d real array
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_3d_real_3arrays (dim1, dim2, dim3, array1, array2, array3, value) result(retVal)

      integer dim1, dim2, dim3
      real    value
      real, pointer, dimension(:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, array3, value)

      return
      end function dh_allocinit_3d_real_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_3d_double_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D double arrays
!   Arguments:  names of 3d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_3d_double_3arrays (dim1, dim2, dim3, array1, array2, array3, value) result(retVal)

      integer dim1, dim2, dim3
      double precision value
      double precision, pointer, dimension(:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, array3, value)

      return
      end function dh_allocinit_3d_double_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_3d_char_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D arrays
!   Arguments:  names of 3d character arrays
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_3d_char_3arrays (dim1, dim2, dim3, array1, array2, array3, value) result(retVal)

      integer dim1, dim2, dim3
      character(len=*) value
      character(len=*), pointer, dimension(:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, array3, value)

      return
      end function dh_allocinit_3d_char_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_3d_logical_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D logical arrays
!   Arguments:  names of 2d logical arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_3d_logical_3arrays (dim1, dim2, dim3, array1, array2, array3, value) result(retVal)

      integer dim1, dim2, dim3
      logical value
      logical, pointer, dimension(:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, array3, value)

      return
      end function dh_allocinit_3d_logical_3arrays


! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_4d_integer_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 4D arrays
!   Arguments:  names of 4d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_4d_integer_2arrays (dim1, dim2, dim3, dim4, array1, array2, value) result(retVal)

      integer dim1, dim2, dim3, dim4, value
      integer, pointer, dimension(:,:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, dim4, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, dim4, array2, value)

      return
      end function dh_allocinit_4d_integer_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_4d_real_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 4D real arrays
!   Arguments:  names of 4d real array
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_4d_real_2arrays (dim1, dim2, dim3, dim4, array1, array2, value) result(retVal)

      integer dim1, dim2, dim3, dim4
      real    value
      real, pointer, dimension(:,:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, dim4, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, dim4, array2, value)

      return
      end function dh_allocinit_4d_real_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_4d_double_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 4D double arrays
!   Arguments:  names of 4d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_4d_double_2arrays (dim1, dim2, dim3, dim4, array1, array2, value) result(retVal)

      integer dim1, dim2, dim3, dim4
      double precision value
      double precision, pointer, dimension(:,:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, dim4, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, dim4, array2, value)

      return
      end function dh_allocinit_4d_double_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_4d_char_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 4D arrays
!   Arguments:  names of 4d character arrays
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_4d_char_2arrays (dim1, dim2, dim3, dim4, array1, array2, value) result(retVal)

      integer dim1, dim2, dim3, dim4
      character(len=*) value
      character(len=*), pointer, dimension(:,:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, dim4, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, dim4, array2, value)

      return
      end function dh_allocinit_4d_char_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_4d_logical_2arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 2 4D logical arrays
!   Arguments:  names of 4d logical arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_4d_logical_2arrays (dim1, dim2, dim3, dim4, array1, array2, value) result(retVal)

      integer dim1, dim2, dim3, dim4
      logical value
      logical, pointer, dimension(:,:,:,:) :: array1, array2
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, dim4, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, dim4, array2, value)

      return
      end function dh_allocinit_4d_logical_2arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_4d_integer_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D arrays
!   Arguments:  names of 4d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_4d_integer_3arrays (dim1, dim2, dim3, dim4, array1, array2, array3, value) result(retVal)

      integer dim1, dim2, dim3, dim4, value
      integer, pointer, dimension(:,:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, dim4, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, dim4, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, dim4, array3, value)

      return
      end function dh_allocinit_4d_integer_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_4d_real_arrays_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D real arrays
!   Arguments:  names of 4d real array
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_4d_real_3arrays (dim1, dim2, dim3, dim4, array1, array2, array3, value) result(retVal)

      integer dim1, dim2, dim3, dim4
      real    value
      real, pointer, dimension(:,:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, dim4, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, dim4, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, dim4, array3, value)

      return
      end function dh_allocinit_4d_real_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_4d_double_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D double arrays
!   Arguments:  names of 4d arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_4d_double_3arrays (dim1, dim2, dim3, dim4, array1, array2, array3, value) result(retVal)

      integer dim1, dim2, dim3, dim4
      double precision value
      double precision, pointer, dimension(:,:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, dim4, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, dim4, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, dim4, array3, value)

      return
      end function dh_allocinit_4d_double_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_4d_char_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D arrays
!   Arguments:  names of 4d character arrays
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_4d_char_3arrays (dim1, dim2, dim3, dim4, array1, array2, array3, value) result(retVal)

      integer dim1, dim2, dim3, dim4
      character(len=*) value
      character(len=*), pointer, dimension(:,:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, dim4, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, dim4, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, dim4, array3, value)

      return
      end function dh_allocinit_4d_char_3arrays

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_4d_logical_3arrays
!   Author:     Geert Prinsen
!   Purpose:    allocate 3 2D logical arrays
!   Arguments:  names of 2d logical arrays
!               dimension of arrays to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_4d_logical_3arrays (dim1, dim2, dim3, dim4, array1, array2, array3, value) result(retVal)
      logical value

      integer dim1, dim2, dim3, dim4
      logical, pointer, dimension(:,:,:,:) :: array1, array2, array3
      logical retVal

      retval = dh_allocinit (dim1, dim2, dim3, dim4, array1, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, dim4, array2, value)
      retVal = retVal .and. dh_allocinit (dim1, dim2, dim3, dim4, array3, value)

      return
      end function dh_allocinit_4d_logical_3arrays


! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1darray_integer
!   Author:     Geert Prinsen
!   Purpose:    allocate a 1D integer array
!   Arguments:  name of 1d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_1darray_integer (dim1, onedimarray) result(retVal)

      logical retVal
      integer  dim1
      integer, pointer, dimension(:) :: onedimarray

      integer allocation_error

      Allocate ( onedimarray(dim1), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_1darray_integer

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1darray_real
!   Author:     Geert Prinsen
!   Purpose:    allocate a 1D real array
!   Arguments:  name of 1d real array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_1darray_real (dim1, onedimarray) result(retVal)

      logical retVal
      integer  dim1
      real, pointer, dimension(:) :: onedimarray

      integer allocation_error

      Allocate ( onedimarray(dim1), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_1darray_real


! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1darray_double
!   Author:     Geert Prinsen
!   Purpose:    allocate a 1D double array
!   Arguments:  name of 1d double array
!               dimension double array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_1darray_double (dim1, onedimarray) result(retVal)

      logical retVal
      integer  dim1
      double precision, pointer, dimension(:) :: onedimarray

      integer allocation_error

      Allocate ( onedimarray(dim1), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_1darray_double


! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1darray_logical
!   Author:     Geert Prinsen
!   Purpose:    allocate a 1D logical array
!   Arguments:  name of 1d logical array
!               dimension logical array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_1darray_logical (dim1, onedimarray) result(retVal)

      logical retVal
      integer  dim1
      logical, pointer, dimension(:) :: onedimarray

      integer allocation_error

      Allocate ( onedimarray(dim1), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_1darray_logical

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1darray_character
!   Author:     Geert Prinsen
!   Purpose:    allocate a 1D character array
!   Arguments:  name of 1d character array
!               dimension character array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_1darray_character (dim1, onedimarray) result(retVal)

      logical retVal
      integer  dim1
      character(len=*), pointer, dimension(:) :: onedimarray

      integer allocation_error

      Allocate ( onedimarray(dim1), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_1darray_character

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_1darray_integer
!   Author:     Geert Prinsen
!   Purpose:    allocate a 1D integer array
!   Arguments:  name of 1d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_1darray_integer (dim1, onedimarray, value) result(retVal)

      logical retVal
      integer  dim1, value
      integer, pointer, dimension(:) :: onedimarray

      integer allocation_error

      Allocate ( onedimarray(dim1), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      if (retVal) onedimarray = value

      return
      end function dh_allocinit_1darray_integer

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_1darray_real
!   Author:     Geert Prinsen
!   Purpose:    allocate a 1D real array
!   Arguments:  name of 1d real array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_1darray_real (dim1, onedimarray, value)  result(retVal)

      logical retVal
      integer dim1
      real, pointer, dimension(:) :: onedimarray
      real    value

      integer allocation_error

      Allocate ( onedimarray(dim1), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      if (retVal) onedimarray = value

      return
      end function dh_allocinit_1darray_real


! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_1darray_double
!   Author:     Geert Prinsen
!   Purpose:    allocate a 1D double array
!   Arguments:  name of 1d double array
!               dimension double array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_1darray_double (dim1, onedimarray, value) result(retVal)

      logical retVal
      integer  dim1
      double precision, pointer, dimension(:) :: onedimarray
      double precision  value

      integer allocation_error

      Allocate ( onedimarray(dim1), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      if (retVal) onedimarray = value

      return
      end function dh_allocinit_1darray_double


! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_1darray_logical
!   Author:     Geert Prinsen
!   Purpose:    allocate a 1D logical array
!   Arguments:  name of 1d logical array
!               dimension logical array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_1darray_logical (dim1, onedimarray, value) result(retVal)

      logical retVal
      integer  dim1
      logical, pointer, dimension(:) :: onedimarray
      logical value

      integer allocation_error

      Allocate ( onedimarray(dim1), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)
      if (retVal) onedimarray = value

      return
      end function dh_allocinit_1darray_logical

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_1darray_character
!   Author:     Geert Prinsen
!   Purpose:    allocate a 1D character array
!   Arguments:  name of 1d character array
!               dimension character array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_1darray_character (dim1, onedimarray, value) result(retVal)

      logical retVal
      integer  dim1
      character(len=*), pointer, dimension(:) :: onedimarray
      character(len=*) value

      integer allocation_error

      Allocate ( onedimarray(dim1), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)
      if (retVal) onedimarray = value

      return
      end function dh_allocinit_1darray_character


! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2darray_integer
!   Author:     Geert Prinsen
!   Purpose:    allocate a 2D integer array
!   Arguments:  name of 2d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_2darray_integer (dim1, dim2, twodimarray) result(retVal)

      logical retVal
      integer  dim1, dim2
      integer, pointer, dimension(:,:) :: twodimarray

      integer allocation_error

      Allocate ( twodimarray(dim1,dim2), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_2darray_integer

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2darray_real
!   Author:     Geert Prinsen
!   Purpose:    allocate a 2D real array
!   Arguments:  name of 2d real array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_2darray_real (dim1, dim2, twodimarray) result(retVal)

      logical retVal
      integer  dim1, dim2
      real, pointer, dimension(:,:) :: twodimarray

      integer allocation_error

      Allocate ( twodimarray(dim1,dim2), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_2darray_real


! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2darray_double
!   Author:     Geert Prinsen
!   Purpose:    allocate a 2D double array
!   Arguments:  name of 2d double array
!               dimension double array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_2darray_double (dim1, dim2, twodimarray) result(retVal)

      logical retVal
      integer dim1, dim2
      double precision, pointer, dimension(:,:) :: twodimarray

      integer allocation_error

      Allocate ( twodimarray(dim1,dim2), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_2darray_double


! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2darray_logical
!   Author:     Geert Prinsen
!   Purpose:    allocate a 2D logical array
!   Arguments:  name of 2d logical array
!               dimension logical array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_2darray_logical (dim1, dim2, twodimarray) result(retVal)

      logical retVal
      integer  dim1, dim2
      logical, pointer, dimension(:,:) :: twodimarray

      integer allocation_error

      Allocate ( twodimarray(dim1,dim2), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_2darray_logical

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_2darray_character
!   Author:     Geert Prinsen
!   Purpose:    allocate a 2D character array
!   Arguments:  name of 2d character array
!               dimension character array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_2darray_character (dim1, dim2, twodimarray) result(retVal)

      logical retVal
      integer  dim1, dim2
      character(len=*), pointer, dimension(:,:) :: twodimarray

      integer allocation_error

      Allocate ( twodimarray(dim1,dim2), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_2darray_character

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_2darray_integer
!   Author:     Geert Prinsen
!   Purpose:    allocate a 2D integer array
!   Arguments:  name of 2d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_2darray_integer (dim1, dim2, twodimarray, value) result(retVal)

      logical retVal
      integer  dim1, dim2, value
      integer, pointer, dimension(:,:) :: twodimarray

      integer allocation_error

      Allocate ( twodimarray(dim1,dim2), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      if (retVal) twodimarray = value

      return
      end function dh_allocinit_2darray_integer

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_2darray_real
!   Author:     Geert Prinsen
!   Purpose:    allocate a 2D real array
!   Arguments:  name of 2d real array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_2darray_real (dim1, dim2, twodimarray, value)  result(retVal)

      logical retVal
      integer dim1, dim2
      real    value
      real, pointer, dimension(:,:) :: twodimarray

      integer allocation_error

      Allocate ( twodimarray(dim1,dim2), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      if (retVal) twodimarray = value

      return
      end function dh_allocinit_2darray_real


! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_2darray_double
!   Author:     Geert Prinsen
!   Purpose:    allocate a 2D double array
!   Arguments:  name of 2d double array
!               dimension double array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_2darray_double (dim1, dim2, twodimarray, value) result(retVal)

      logical retVal
      integer  dim1, dim2
      double precision, pointer, dimension(:,:) :: twodimarray
      double precision  value

      integer allocation_error

      Allocate ( twodimarray(dim1,dim2), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      if (retVal) twodimarray = value

      return
      end function dh_allocinit_2darray_double


! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_2darray_logical
!   Author:     Geert Prinsen
!   Purpose:    allocate a 2D logical array
!   Arguments:  name of 2d logical array
!               dimension logical array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_2darray_logical (dim1, dim2, twodimarray, value) result(retVal)

      logical retVal
      integer  dim1, dim2
      logical, pointer, dimension(:,:) :: twodimarray
      logical value

      integer allocation_error

      Allocate ( twodimarray(dim1,dim2), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)
      if (retVal) twodimarray = value

      return
      end function dh_allocinit_2darray_logical

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_2darray_character
!   Author:     Geert Prinsen
!   Purpose:    allocate a 2D character array
!   Arguments:  name of 2d character array
!               dimension character array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_2darray_character (dim1, dim2, twodimarray, value) result(retVal)

      logical retVal
      integer  dim1, dim2
      character(len=*), pointer, dimension(:,:) :: twodimarray
      character(len=*) value

      integer allocation_error

      Allocate ( twodimarray(dim1,dim2), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)
      if (retVal) twodimarray = value

      return
      end function dh_allocinit_2darray_character



! --------------------------------------------------------------------
!   Subroutine: dh_alloc_3darray_integer
!   Author:     Geert Prinsen
!   Purpose:    allocate a 3d integer array
!   Arguments:  name of 3d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_3darray_integer (dim1, dim2, dim3, threedimarray) result(retVal)

      logical retVal
      integer  dim1, dim2, dim3
      integer, pointer, dimension(:,:,:) :: threedimarray

      integer allocation_error

      Allocate ( threedimarray(dim1,dim2,dim3), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_3darray_integer

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_3darray_real
!   Author:     Geert Prinsen
!   Purpose:    allocate a 3d real array
!   Arguments:  name of 3d real array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_3darray_real (dim1, dim2, dim3, threedimarray) result(retVal)

      logical retVal
      integer  dim1, dim2, dim3
      real, pointer, dimension(:,:,:) :: threedimarray

      integer allocation_error

      Allocate ( threedimarray(dim1,dim2,dim3), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_3darray_real


! --------------------------------------------------------------------
!   Subroutine: dh_alloc_3darray_double
!   Author:     Geert Prinsen
!   Purpose:    allocate a 3d double array
!   Arguments:  name of 3d double array
!               dimension double array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_3darray_double (dim1, dim2, dim3, threedimarray) result(retVal)

      logical retVal
      integer dim1, dim2, dim3
      double precision, pointer, dimension(:,:,:) :: threedimarray

      integer allocation_error

      Allocate ( threedimarray(dim1,dim2,dim3), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_3darray_double


! --------------------------------------------------------------------
!   Subroutine: dh_alloc_3darray_logical
!   Author:     Geert Prinsen
!   Purpose:    allocate a 3d logical array
!   Arguments:  name of 3d logical array
!               dimension logical array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_3darray_logical (dim1, dim2, dim3, threedimarray) result(retVal)

      logical retVal
      integer  dim1, dim2, dim3
      logical, pointer, dimension(:,:,:) :: threedimarray

      integer allocation_error

      Allocate ( threedimarray(dim1,dim2,dim3), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_3darray_logical

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1darray_character
!   Author:     Geert Prinsen
!   Purpose:    allocate a 3d character array
!   Arguments:  name of 3d character array
!               dimension character array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_3darray_character (dim1, dim2, dim3, threedimarray) result(retVal)

      logical retVal
      integer  dim1, dim2, dim3
      character(len=*), pointer, dimension(:,:,:) :: threedimarray

      integer allocation_error

      Allocate ( threedimarray(dim1,dim2,dim3), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_3darray_character

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_3darray_integer
!   Author:     Geert Prinsen
!   Purpose:    allocate a 3d integer array
!   Arguments:  name of 3d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_3darray_integer (dim1, dim2, dim3, threedimarray, value) result(retVal)

      logical retVal
      integer  dim1, dim2, dim3, value
      integer, pointer, dimension(:,:,:) :: threedimarray

      integer allocation_error

      Allocate ( threedimarray(dim1,dim2,dim3), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      if (retVal) threedimarray = value

      return
      end function dh_allocinit_3darray_integer

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_3darray_real
!   Author:     Geert Prinsen
!   Purpose:    allocate a 3d real array
!   Arguments:  name of 3d real array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_3darray_real (dim1, dim2, dim3, threedimarray, value)  result(retVal)

      logical retVal
      integer dim1, dim2, dim3
      real    value
      real, pointer, dimension(:,:,:) :: threedimarray

      integer allocation_error

      Allocate ( threedimarray(dim1,dim2,dim3), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      if (retVal) threedimarray = value

      return
      end function dh_allocinit_3darray_real


! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_3darray_double
!   Author:     Geert Prinsen
!   Purpose:    allocate a 3d double array
!   Arguments:  name of 3d double array
!               dimension double array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_3darray_double (dim1, dim2, dim3, threedimarray, value) result(retVal)

      logical retVal
      integer  dim1, dim2, dim3
      double precision, pointer, dimension(:,:,:) :: threedimarray
      double precision  value

      integer allocation_error

      Allocate ( threedimarray(dim1,dim2,dim3), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      if (retVal) threedimarray = value

      return
      end function dh_allocinit_3darray_double


! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_3darray_logical
!   Author:     Geert Prinsen
!   Purpose:    allocate a 3d logical array
!   Arguments:  name of 3d logical array
!               dimension logical array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_3darray_logical (dim1, dim2, dim3, threedimarray, value) result(retVal)

      logical retVal
      integer  dim1, dim2, dim3
      logical, pointer, dimension(:,:,:) :: threedimarray
      logical value

      integer allocation_error

      Allocate ( threedimarray(dim1,dim2,dim3), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)
      if (retVal) threedimarray = value

      return
      end function dh_allocinit_3darray_logical

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_3darray_character
!   Author:     Geert Prinsen
!   Purpose:    allocate a 3d character array
!   Arguments:  name of 3d character array
!               dimension character array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_3darray_character (dim1, dim2, dim3, threedimarray, value) result(retVal)

      logical retVal
      integer  dim1, dim2, dim3
      character(len=*), pointer, dimension(:,:,:) :: threedimarray
      character(len=*) value

      integer allocation_error

      Allocate ( threedimarray(dim1,dim2,dim3), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)
      if (retVal) threedimarray = value

      return
      end function dh_allocinit_3darray_character



! --------------------------------------------------------------------
!   Subroutine: dh_alloc_4darray_integer
!   Author:     Geert Prinsen
!   Purpose:    allocate a 4d integer array
!   Arguments:  name of 4d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_4darray_integer (dim1, dim2, dim3, dim4, fourdimarray) result(retVal)

      logical retVal
      integer  dim1, dim2, dim3, dim4
      integer, pointer, dimension(:,:,:,:) :: fourdimarray

      integer allocation_error

      Allocate ( fourdimarray(dim1,dim2,dim3,dim4), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_4darray_integer

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_4darray_real
!   Author:     Geert Prinsen
!   Purpose:    allocate a 4d real array
!   Arguments:  name of 4d real array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_4darray_real (dim1, dim2, dim3, dim4, fourdimarray) result(retVal)

      logical retVal
      integer  dim1, dim2, dim3, dim4
      real, pointer, dimension(:,:,:,:) :: fourdimarray

      integer allocation_error

      Allocate ( fourdimarray(dim1,dim2,dim3,dim4), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_4darray_real


! --------------------------------------------------------------------
!   Subroutine: dh_alloc_4darray_double
!   Author:     Geert Prinsen
!   Purpose:    allocate a 4d double array
!   Arguments:  name of 4d double array
!               dimension double array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_4darray_double (dim1, dim2, dim3, dim4, fourdimarray) result(retVal)

      logical retVal
      integer dim1, dim2, dim3, dim4
      double precision, pointer, dimension(:,:,:,:) :: fourdimarray

      integer allocation_error

      Allocate ( fourdimarray(dim1,dim2,dim3,dim4), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_4darray_double


! --------------------------------------------------------------------
!   Subroutine: dh_alloc_4darray_logical
!   Author:     Geert Prinsen
!   Purpose:    allocate a 4d logical array
!   Arguments:  name of 4d logical array
!               dimension logical array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_4darray_logical (dim1, dim2, dim3, dim4, fourdimarray) result(retVal)

      logical retVal
      integer  dim1, dim2, dim3, dim4
      logical, pointer, dimension(:,:,:,:) :: fourdimarray

      integer allocation_error

      Allocate ( fourdimarray(dim1,dim2,dim3,dim4), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_4darray_logical

! --------------------------------------------------------------------
!   Subroutine: dh_alloc_1darray_character
!   Author:     Geert Prinsen
!   Purpose:    allocate a 4d character array
!   Arguments:  name of 4d character array
!               dimension character array to be allocated
! --------------------------------------------------------------------
!
      function dh_alloc_4darray_character (dim1, dim2, dim3, dim4, fourdimarray) result(retVal)

      logical retVal
      integer  dim1, dim2, dim3, dim4
      character(len=*), pointer, dimension(:,:,:,:) :: fourdimarray

      integer allocation_error

      Allocate ( fourdimarray(dim1,dim2,dim3,dim4), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      return
      end function dh_alloc_4darray_character

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_4darray_integer
!   Author:     Geert Prinsen
!   Purpose:    allocate a 4d integer array
!   Arguments:  name of 4d integer array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_4darray_integer (dim1, dim2, dim3, dim4, fourdimarray, value) result(retVal)

      logical retVal
      integer  dim1, dim2, dim3, dim4, value
      integer, pointer, dimension(:,:,:,:) :: fourdimarray

      integer allocation_error

      Allocate ( fourdimarray(dim1,dim2,dim3,dim4), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      if (retVal) fourdimarray = value

      return
      end function dh_allocinit_4darray_integer

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_4darray_real
!   Author:     Geert Prinsen
!   Purpose:    allocate a 4d real array
!   Arguments:  name of 4d real array
!               dimension of array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_4darray_real (dim1, dim2, dim3, dim4, fourdimarray, value)  result(retVal)

      logical retVal
      integer dim1, dim2, dim3, dim4
      real    value
      real, pointer, dimension(:,:,:,:) :: fourdimarray

      integer allocation_error

      Allocate ( fourdimarray(dim1,dim2,dim3,dim4), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      if (retVal) fourdimarray = value

      return
      end function dh_allocinit_4darray_real


! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_4darray_double
!   Author:     Geert Prinsen
!   Purpose:    allocate a 4d double array
!   Arguments:  name of 4d double array
!               dimension double array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_4darray_double (dim1, dim2, dim3, dim4, fourdimarray, value) result(retVal)

      logical retVal
      integer  dim1, dim2, dim3, dim4
      double precision, pointer, dimension(:,:,:,:) :: fourdimarray
      double precision  value

      integer allocation_error

      Allocate ( fourdimarray(dim1,dim2,dim3,dim4), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)

      if (retVal) fourdimarray = value

      return
      end function dh_allocinit_4darray_double


! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_4darray_logical
!   Author:     Geert Prinsen
!   Purpose:    allocate a 4d logical array
!   Arguments:  name of 4d logical array
!               dimension logical array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_4darray_logical (dim1, dim2, dim3, dim4, fourdimarray, value) result(retVal)

      logical retVal
      integer  dim1, dim2, dim3, dim4
      logical, pointer, dimension(:,:,:,:) :: fourdimarray
      logical value

      integer allocation_error

      Allocate ( fourdimarray(dim1,dim2,dim3,dim4), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)
      if (retVal) fourdimarray = value

      return
      end function dh_allocinit_4darray_logical

! --------------------------------------------------------------------
!   Subroutine: dh_allocinit_4darray_character
!   Author:     Geert Prinsen
!   Purpose:    allocate a 4d character array
!   Arguments:  name of 4d character array
!               dimension character array to be allocated
! --------------------------------------------------------------------
!
      function dh_allocinit_4darray_character (dim1, dim2, dim3, dim4, fourdimarray, value) result(retVal)

      logical retVal
      integer  dim1, dim2, dim3, dim4
      character(len=*), pointer, dimension(:,:,:,:) :: fourdimarray
      character(len=*) value

      integer allocation_error

      Allocate ( fourdimarray(dim1,dim2,dim3,dim4), Stat=Allocation_Error )
      retVal = (allocation_error .eq. 0)
      if (retVal) fourdimarray = value

      return
      end function dh_allocinit_4darray_character



    end module dh_alloc

