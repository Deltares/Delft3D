!!  Copyright (C)  Stichting Deltares, 2012-2024.
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

module test_hash_search
    use ftnunit
    use m_hash_search

    implicit none

contains
subroutine tests_hash_search
    call test( test_hash_init,          'Initialise a hash table' )
    call test( test_hash_fill,          'Initialise and fill a hash table' )
    call test( test_hash_dealloc,       'Deallocate a hash table' )
    call test( test_hash_search_or_add, 'Store and retrieve strings in a hash table' )
    call test( test_hash_realloc,       'Automatically extend the arrays in a hash table' )
end subroutine tests_hash_search

subroutine test_hash_init
    type(t_hashlist) :: hashlist
    integer          :: count

    !
    ! Check that the hash list is properly initialised - almost trivial
    !
    count = 100
    call hashfill_init( hashlist, count )

    call assert_equal( hashlist%size, count, "The initial size of the hash list is incorrect" )
end subroutine test_hash_init

subroutine test_hash_fill
    type(t_hashlist) :: hashlist
    integer          :: count

    !
    ! Check that the hash list is properly filled
    !
    count = 100
    call hashfill_init( hashlist, count )
    call hashfill( hashlist )

    call assert_true( allocated(hashlist%hashfirst), "The 'first' array in the hashlist should have been allocated" )

    if ( allocated(hashlist%hashfirst) ) then
        call assert_equal( size(hashlist%hashfirst), 1+hashlist%hashcon, "The size of the 'first' array in the hash list is incorrect" )
    endif

    call assert_true( allocated(hashlist%hashnext), "The 'next' array in the hashlist shoudl have been allocated" )

    if ( allocated(hashlist%hashfirst) ) then
        call assert_equal( size(hashlist%hashnext), hashlist%id_count, "The size of the 'next' array in the hash list is incorrect" )
    endif


    call assert_equal( hashlist%size, count, "The initial size of the hash list is incorrect" )
end subroutine test_hash_fill

subroutine test_hash_dealloc
    type(t_hashlist) :: hashlist
    integer          :: count

    !
    ! Check that the hash list is properly deallocated
    !
    count = 100
    call hashfill_init( hashlist, count )
    call hashfill( hashlist )

    call dealloc( hashlist )

    call assert_equal( hashlist%id_count, 0, "The number of IDs in the deallocated hash list is incorrect" )
    call assert_equal( hashlist%size, 0, "The size of the deallocated hash list is incorrect" )
    call assert_false( allocated(hashlist%hashfirst), "The 'first' array should have been deallocated" )
    call assert_false( allocated(hashlist%hashnext), "The 'next' array should have been deallocated" )
end subroutine test_hash_dealloc

subroutine test_hash_search_or_add
    type(t_hashlist) :: hashlist
    integer          :: idx1, idx2, idx3, idx4, idxn
    integer          :: count

    !
    ! Check that the hash list can be used to retrieve items
    !
    count = 100
    call hashfill_init( hashlist, count )

    idx1 = hashsearch_or_add( hashlist, 'foo' )
    idx2 = hashsearch_or_add( hashlist, 'bar' )
    idx3 = hashsearch_or_add( hashlist, 'baz' )
    idx4 = hashsearch_or_add( hashlist, 'qiz' )

    idxn = hashsearch( hashlist, 'foo' )
    call assert_equal( idx1, idxn, "The retrieved ID is different from the original ID" )

    idxn = hashsearch( hashlist, 'FOO' )
    call assert_equal( idx1, idxn, "The retrieval should be case-insensitive" )
end subroutine test_hash_search_or_add

subroutine test_hash_realloc
    type(t_hashlist) :: hashlist
    integer          :: idx1, idx2, idx3, idx4, idxn
    integer          :: count

    !
    ! Check that the hash list is correctly extended - by forcing a reallocation
    !
    count = 1
    call hashfill_init( hashlist, count )

    idx1 = hashsearch_or_add( hashlist, 'foo' )
    idx2 = hashsearch_or_add( hashlist, 'bar' )
    idx3 = hashsearch_or_add( hashlist, 'baz' )
    idx4 = hashsearch_or_add( hashlist, 'qiz' )

    idxn = hashsearch( hashlist, 'foo' )
    call assert_false( idxn == -1, "The stored key 'foo' should have been retrieved" )
    if ( idxn /= -1 ) then
        call assert_equal( idx1, idxn, "The retrieved ID for 'foo' is different from the original ID" )
    endif

    idxn = hashsearch( hashlist, 'qiz' )
    call assert_false( idxn == -1, "The stored key 'qiz' should have been retrieved" )
    if ( idxn /= -1 ) then
        call assert_equal( idx4, idxn, "The retrieved ID for 'qiz' is different from the original ID" )
    endif
end subroutine test_hash_realloc

end module test_hash_search

