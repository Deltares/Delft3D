!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2015-2024.                                
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
module io_openfoam
    implicit none

contains

!> Write D-Flow FM info+version as an OpenFOAM header into an ASCII file.
subroutine foam_write_dflowfminfo(mout)
    use unstruc_version_module
    integer, intent(in) :: mout !< File unit nr for output.

    character(len=20) :: rundat

    call datum(rundat)

    write(mout, '(a)')       '/*---------------------------------------------------------------------------*\ '  
    write(mout, '(a,a,a,a)') '| Generated on ', trim(rundat), repeat(' ', 79-16-len_trim(rundat)), '|'
    write(mout, '(a,a,a,a)') '| ', trim(unstruc_version_full), repeat(' ', 79-3-len_trim(unstruc_version_full)), '|'
    write(mout, '(a)')       '\*---------------------------------------------------------------------------*/ ' 
end subroutine foam_write_dflowfminfo

!> Writes an OpenFOAM data file header.
!!
!! The type of data should be described by the headdict dictionary,
!! according to http://www.openfoam.org/docs/user/basic-file-format.php
subroutine foam_write_datafile_header(mout, headdict)
    use properties

    integer,          intent(in) :: mout     !< File unit nr for output.
    type(TREE_DATA), pointer     :: headdict !< Tree structure containing dictionary entries (should be a list/flat tree)

    call foam_write_dflowfminfo(mout)

    call foam_write_dictionary(mout, 'FoamFile', headdict)
    
    write (mout, '(a)') '// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //'
    write (mout, '(a)') ''
    write (mout, '(a)') ''

end subroutine foam_write_datafile_header

!> Generic writer for OpenFOAM dictionary (list of key-value pairs)
!! Used for printing the 'FoamFile' file header dictionary.
subroutine foam_write_dictionary(mout, name, dicttree)
    use properties
    integer, intent(in)          :: mout     !< File unit number for output.
    character(len=*), intent(in) :: name     !< name of the dictionary
    type(TREE_DATA), pointer     :: dicttree !< Tree structure containing dictionary entries (should be a list/flat tree)

    character(len=1), allocatable :: lenmaxdata(:)
    logical :: dummylog
    integer :: keymaxlen

    write(mout, '(a)') name
    write(mout, '(a)') '{'

    allocate(lenmaxdata(size(transfer(123, node_value)))) ! Fit a single integer into char array (generally 4 bytes)

    ! Determine maximum key stringlength (used for prettyprinting/alignment in print_foam_dict)
    call tree_fold(dicttree, max_keylength, leaf_keylength, lenmaxdata, dummylog)
    keymaxlen = transfer(lenmaxdata, 123)

    ! Print the tree by traversing it depth-first, pass mout and lenmax by transfer into data variable.
    call tree_traverse(dicttree, print_foam_dict, transfer((/ mout, keymaxlen /), node_value), dummylog)

    write(mout, '(a)') '}'

end subroutine foam_write_dictionary

!> Prints the root of a tree (as a whitespace-separated key-value pair)
!! to be used in call to tree_traverse
subroutine print_foam_dict( tree, data, stop )
    use properties

  type(TREE_DATA), pointer                   :: tree    !< Tree whose root should be printed.
  character(len=1), dimension(:), intent(in) :: data    !< Help data (max key length, used for alignment, and output file unit nr).
  logical,                        intent(inout) :: stop !< Whether to continue or stop.

  integer, dimension(2)                  :: inputdata
  integer                                :: mout
  integer                                :: maxkeylength 
  character(len=1), dimension(:),pointer :: data_ptr
  character(len=256)                     :: string
  character(len=40)                      :: type_string
  logical                                :: success
  integer                                :: level

  inputdata    = transfer(data, inputdata)
  mout         = inputdata(1) !< File pointer
  maxkeylength = inputdata(2)

  level = tree_traverse_level()
  if (level == 0) return

  call tree_get_data_ptr( tree, data_ptr, type_string )
  if (associated(data_ptr)) then
     string = tree_get_name(tree)
     write(mout, '(a,a,a)', advance='no') &
          '    ', trim(string), repeat(' ', max(0,maxkeylength-len_trim(string))+4)
  end if

  select case (type_string)
  case ('STRING')
     string = ''
     call tree_get_data_string( tree, string, success )
     write(mout,'(a,a)') trim(string), ';'
  case default
     string = '(unknown data type)'
     write(mout,'(a,a,a,a)') '# ', trim(string), ' -- ', trim(type_string)
  end select
end subroutine print_foam_dict


!> Writes the current network to a set of OpenFOAM polyMesh files.
subroutine foam_write_polymesh(filename)
    use m_flowgeom
    use unstruc_files
    use properties
    character(len=*), intent(in) :: filename !< TODO: Output file names

    integer :: mfil
    integer :: L
    type(tree_data), pointer :: headdict
    character(len=16) :: strtmp

    call tree_create('FoamFile', headdict)
    call prop_set(headdict, '', 'version',  '2.0')
    call prop_set(headdict, '', 'format',   'ascii')

    
    L = len_trim(filename)

    ! -- Step 1. points file ----------
    call newfil(mfil, filename)


    call prop_set(headdict, '', 'location', '"bla/points"')
    call prop_set(headdict, '', 'class',    'vectorField')
    call prop_set(headdict, '', 'object',   'points')

    call foam_write_datafile_header(mfil, headdict)
    call write_points_(mfil)
    call doclose(mfil)

    ! -- Step 2. faces file ----------
    

contains
    subroutine write_points_(mout)
        use network_data
        integer, intent(in) :: mout !< File unit nr for output.

        integer :: k
        write (strtmp, '(i10)') numk
        write (mout, '(a)') adjustl(strtmp)
        write (mout, '(a)') '('
        do k=1,numk
            write (mout, '(a,3(f25.16),a)') '(', xk(k), yk(k), zk(k), ')'
        end do
        write (mout, '(a)') ')'

    end subroutine write_points_

    subroutine write_faces_(mout)
        use network_data
        integer, intent(in) :: mout !< File unit nr for output.

        integer :: k
        write (strtmp, '(i10)') numk
        write (mout, '(a)') adjustl(strtmp)
        write (mout, '(a)') '('
        do k=1,numk
            write (mout, '(a,3(f25.16),a)') '(', xk(k), yk(k), zk(k), ')'
        end do
        write (mout, '(a)') ')'

    end subroutine write_faces_
end subroutine foam_write_polymesh

end module io_openfoam
