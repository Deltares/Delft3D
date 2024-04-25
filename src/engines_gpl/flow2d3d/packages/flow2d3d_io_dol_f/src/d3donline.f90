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
!  
!  
!-------------------------------------------------------------------------------
!   Delft3D - Fortran-95 Stubs for C++ D3D Online Routines
!
!   Irv.Elshoff@deltares.nl
!   Jules Overmars
!   Hans van Putten
!   26 feb 08
!
!-------------------------------------------------------------------------------


module D3DOnline
    !
    ! It is important to use exactly the same definition for double/single precision
    !
    use precision
    interface FLOWOL_Publish
        module procedure FLOWOL_Publish_scalar_integer
        module procedure FLOWOL_Publish_scalar_real
        module procedure FLOWOL_Publish_scalar_double
        module procedure FLOWOL_Publish_scalar_doublecomplex
        module procedure FLOWOL_Publish_scalar_complex
        module procedure FLOWOL_Publish_scalar_logical
        module procedure FLOWOL_Publish_scalar_character
        module procedure FLOWOL_Publish_array_integer
        module procedure FLOWOL_Publish_array_real
        module procedure FLOWOL_Publish_array_double
        module procedure FLOWOL_Publish_array_doublecomplex
        module procedure FLOWOL_Publish_array_complex
        module procedure FLOWOL_Publish_array_logical
        module procedure FLOWOL_Publish_array_character
    end interface

contains 

    !-------------------------------------------------------------------------------
    !   Polymorphic subroutines
    !   Below are 14 implementations of FLOWOL_Publish.  There are seven base
    !   types in Fortran, and for each of these a scalar and an array variant.
    !   Each implementation is identical, with the exception of one line: the
    !   declaration of the "address" parameter.  This would be a lot cleaner if
    !   Fortran had a macro (preprocessor) facility, but it doesn't, so there's
    !   lots of boring code.  Ho hum.

    !-------------------------------------------------------------------------------
    !   Scalar implementations

    subroutine FLOWOL_Publish_scalar_integer &
                (name, description, units, definedon, arrayshape, basetype, address, inout)

        character(*), intent (in)   ::  name, description, units, definedon, arrayshape
        integer, intent (in)        ::  basetype, inout
        integer, intent (in)        ::  address

        call FLOWOL_Publish_c &
                (name, description, units, definedon, arrayshape, basetype, address, inout)
    end subroutine

    subroutine FLOWOL_Publish_scalar_real &
                (name, description, units, definedon, arrayshape, basetype, address, inout)

        character(*), intent (in)   ::  name, description, units, definedon, arrayshape
        integer, intent (in)        ::  basetype, inout
        real(sp), intent (in)       ::  address

        call FLOWOL_Publish_c &
                (name, description, units, definedon, arrayshape, basetype, address, inout)
    end subroutine

    subroutine FLOWOL_Publish_scalar_double &
                (name, description, units, definedon, arrayshape, basetype, address, inout)

        character(*), intent (in)   ::  name, description, units, definedon, arrayshape
        integer, intent (in)        ::  basetype, inout
        real(hp), intent (in)       ::  address

        call FLOWOL_Publish_c &
                (name, description, units, definedon, arrayshape, basetype, address, inout)
    end subroutine

    subroutine FLOWOL_Publish_scalar_doublecomplex &
                (name, description, units, definedon, arrayshape, basetype, address, inout)

        character(*), intent (in)   ::  name, description, units, definedon, arrayshape
        integer, intent (in)        ::  basetype, inout
        complex(hp), intent (in)    ::  address

        call FLOWOL_Publish_c &
                (name, description, units, definedon, arrayshape, basetype, address, inout)
    end subroutine

    subroutine FLOWOL_Publish_scalar_complex &
                (name, description, units, definedon, arrayshape, basetype, address, inout)

        character(*), intent (in)   ::  name, description, units, definedon, arrayshape
        integer, intent (in)        ::  basetype, inout
        complex(sp), intent (in)    ::  address

        call FLOWOL_Publish_c &
                (name, description, units, definedon, arrayshape, basetype, address, inout)
    end subroutine

    subroutine FLOWOL_Publish_scalar_logical &
                (name, description, units, definedon, arrayshape, basetype, address, inout)

        character(*), intent (in)   ::  name, description, units, definedon, arrayshape
        integer, intent (in)        ::  basetype, inout
        logical, intent (in)        ::  address

        call FLOWOL_Publish_c &
                (name, description, units, definedon, arrayshape, basetype, address, inout)
    end subroutine

    subroutine FLOWOL_Publish_scalar_character &
                (name, description, units, definedon, arrayshape, basetype, address, inout)

        character(*), intent (in)   ::  name, description, units, definedon, arrayshape
        integer, intent (in)        ::  basetype, inout
        character(*), intent (in)      ::  address

        !
        ! When address is a character string, the length is automatically added to the parameter list
        ! Therefore, a separate c-routine must be called
        !
        call FLOWOL_Publish_string_c &
                (name, description, units, definedon, arrayshape, basetype, address, inout)
    end subroutine

    !-------------------------------------------------------------------------------
    !   Array implementations

    subroutine FLOWOL_Publish_array_integer &
                (name, description, units, definedon, arrayshape, basetype, address, inout)

        character(*), intent (in)   ::  name, description, units, definedon, arrayshape
        integer, intent (in)        ::  basetype, inout
        integer, dimension(*), intent (in) ::  address

        call FLOWOL_Publish_c &
                (name, description, units, definedon, arrayshape, basetype, address, inout)
    end subroutine

    subroutine FLOWOL_Publish_array_real &
                (name, description, units, definedon, arrayshape, basetype, address, inout)

        character(*), intent (in)   ::  name, description, units, definedon, arrayshape
        integer, intent (in)        ::  basetype, inout
        real(sp), dimension(*), intent (in) ::  address

        call FLOWOL_Publish_c &
                (name, description, units, definedon, arrayshape, basetype, address, inout)
    end subroutine

    subroutine FLOWOL_Publish_array_double &
                (name, description, units, definedon, arrayshape, basetype, address, inout)

        character(*), intent (in)   ::  name, description, units, definedon, arrayshape
        integer, intent (in)        ::  basetype, inout
        real(hp), dimension(*), intent (in) ::  address

        call FLOWOL_Publish_c &
                (name, description, units, definedon, arrayshape, basetype, address, inout)
    end subroutine

    subroutine FLOWOL_Publish_array_doublecomplex &
                (name, description, units, definedon, arrayshape, basetype, address, inout)

        character(*), intent (in)   ::  name, description, units, definedon, arrayshape
        integer, intent (in)        ::  basetype, inout
        complex(hp), dimension(*), intent (in) ::  address

        call FLOWOL_Publish_c &
                (name, description, units, definedon, arrayshape, basetype, address, inout)
    end subroutine

    subroutine FLOWOL_Publish_array_complex &
                (name, description, units, definedon, arrayshape, basetype, address, inout)

        character(*), intent (in)   ::  name, description, units, definedon, arrayshape
        integer, intent (in)        ::  basetype, inout
        complex(sp), dimension(*), intent (in) ::  address

        call FLOWOL_Publish_c &
                (name, description, units, definedon, arrayshape, basetype, address, inout)
    end subroutine

    subroutine FLOWOL_Publish_array_logical &
                (name, description, units, definedon, arrayshape, basetype, address, inout)

        character(*), intent (in)   ::  name, description, units, definedon, arrayshape
        integer, intent (in)        ::  basetype, inout
        logical, dimension(*), intent (in) ::  address

        call FLOWOL_Publish_c &
                (name, description, units, definedon, arrayshape, basetype, address, inout)
    end subroutine

    subroutine FLOWOL_Publish_array_character &
                (name, description, units, definedon, arrayshape, basetype, address, inout)

        character(*), intent (in)   ::  name, description, units, definedon, arrayshape
        integer, intent (in)        ::  basetype, inout
        character(*), dimension(*), intent (in) ::  address

        !
        ! When address is a character string, the length is added to the parameter list
        ! Therefore, a separate c-routine must be called
        !
        call FLOWOL_Publish_string_c &
               (name, description, units, definedon, arrayshape, basetype, address, inout)
    end subroutine

end module D3DOnline

