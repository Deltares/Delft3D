! tables.f90 --
!      Module to work with named tables with named columns
!
module tables_module
    implicit none

    integer, parameter :: dp = kind(1.0d0)

    type table
        character(len=20), dimension(:), allocatable :: names
        real(kind=dp), dimension(:), allocatable     :: values
    endtype table

    type tables
        character(len=20), dimension(:), allocatable :: names
        type(table), dimension(:), allocatable       :: list
    endtype tables

    interface add
        module procedure add_to_tables
        module procedure add_data_to_table
    end interface add

    interface get_table
        module procedure get_table_by_name
        module procedure get_table_by_index
    end interface get_table

    interface get_column
        module procedure get_column_by_name
        module procedure get_column_by_index
    end interface get_column

    interface get_row
        module procedure get_row_by_index
    end interface get_row

contains
subroutine add_to_tables( list_of_tables, new_name, new_table )
    type(tables), intent(inout)  :: list_of_tables
    character(len=*), intent(in) :: new_name
    type(table), intent(in)      :: new_table

    type(table), dimension(:), allocatable       :: new_list
    character(len=20), dimension(:), allocatable :: new_names
    integer                                      :: ierror
    integer                                      :: newidx

    if ( allocated(list_of_tables%list) ) then
        newidx = size(list_of_tables%names)+1
    else
        newidx = 1
    endif
    allocate( new_list(newidx) , stat=ierror)
    allocate( new_names(newidx), stat=ierror)

    if ( allocated(list_of_tables%list) ) then
        new_list(1:newidx-1)  = list_of_tables%list
        new_names(1:newidx-1) = list_of_tables%names
    endif

    new_list(newidx)  = new_table
    new_names(newidx) = new_name

    call move_alloc( new_list, list_of_tables%list )
    call move_alloc( new_names, list_of_tables%names )
end subroutine add_to_tables

subroutine get_table_by_name( list_of_tables, name, copy_table, error )
    type(tables), intent(in)     :: list_of_tables
    character(len=*), intent(in) :: name
    type(table), intent(out)     :: copy_table
    logical, intent(out)         :: error

    integer                      :: i, idx

    error = .false.

    idx = -1
    do i = 1,size(list_of_tables%list)
        if ( list_of_tables%names(i) == name ) then
            idx = i
            exit
        endif
    enddo

    if ( idx == -1 ) then
        error = .true.
        return
    endif

    call get_table_by_index( list_of_tables, idx, copy_table, error )
end subroutine get_table_by_name

subroutine get_table_by_index( list_of_tables, idx, copy_table, error )
    type(tables), intent(in)     :: list_of_tables
    integer, intent(in)          :: idx
    type(table), intent(out)     :: copy_table
    logical, intent(out)         :: error
    
    integer :: ierror

    if ( idx < 1 .or. idx > size(list_of_tables%list) ) then
        error = .true.
        return
    endif

    allocate( copy_table%names(size(list_of_tables%list(idx)%names))  , stat=ierror)
    allocate( copy_table%values(size(list_of_tables%list(idx)%values)), stat=ierror)

    copy_table%names  = list_of_tables%list(idx)%names
    copy_table%values = list_of_tables%list(idx)%values
end subroutine get_table_by_index

subroutine define_table( new_table, names )
    type(table), intent(out)                   :: new_table
    character(len=*), dimension(:), intent(in) :: names
    
    integer :: ierror

    if ( allocated(new_table%names) ) then
        deallocate( new_table%names, new_table%values, stat=ierror)
    endif

    allocate( new_table%names(size(names)), stat=ierror)
    allocate( new_table%values(0)         , stat=ierror)
    new_table%names = names

end subroutine define_table

subroutine get_column_by_name( filled_table, name, column_data, error )
    type(table), intent(in)             :: filled_table
    character(len=*), intent(in)        :: name
    real(dp), dimension(:), allocatable :: column_data
    logical, intent(out)                :: error

    integer                             :: i, idx

    error = .false.

    idx = -1
    do i = 1,size(filled_table%names)
        if ( filled_table%names(i) == name ) then
            idx = i
            exit
        endif
    enddo

    if ( idx == -1 ) then
        error = .true.
        return
    endif

    call get_column_by_index( filled_table, idx, column_data, error )
end subroutine get_column_by_name

subroutine get_column_by_index( filled_table, idx, column_data, error )
    type(table), intent(in)                          :: filled_table
    integer, intent(in)                              :: idx
    real(dp), dimension(:), allocatable, intent(out) :: column_data
    logical, intent(out)                             :: error

    integer                                          :: rows, columns, ierror

    if ( allocated(column_data) ) then
        deallocate( column_data, stat=ierror)
    endif

    if ( idx < 1 .or. idx > size(filled_table%names) ) then
        allocate( column_data(0), stat=ierror)
        error = .true.
        return
    endif

    columns = size(filled_table%names)
    rows    = size(filled_table%values) / columns

    allocate( column_data(rows), stat=ierror)
    column_data = filled_table%values(idx::columns)
end subroutine get_column_by_index

subroutine get_row_by_index( filled_table, idx, row_data, error )
    type(table), intent(in)                          :: filled_table
    integer, intent(in)                              :: idx
    real(dp), dimension(:), allocatable, intent(out) :: row_data
    logical, intent(out)                             :: error

    integer                                          :: rows, columns, ierror

    if ( allocated(row_data) ) then
        deallocate( row_data, stat=ierror)
    endif

    error   = .false.
    columns = size(filled_table%names)
    rows    = size(filled_table%values) / columns

    if ( idx < 1 .or. idx > rows ) then
        allocate( row_data(0), stat=ierror)
        error = .true.
        return
    endif

    allocate( row_data(columns), stat=ierror)
    row_data = filled_table%values(1+(idx-1)*columns:idx*columns)
end subroutine get_row_by_index

subroutine add_data_to_table( filled_table, data, error )
    type(table), intent(inout)          :: filled_table
    real(dp), dimension(:), intent(in)  :: data
    logical, intent(out)                :: error

    integer                             :: rows, columns, ierror
    real(dp), dimension(:), allocatable :: new_data

    error   = .false.
    columns = size(filled_table%names)
    rows    = size(data) / columns

    if ( columns * rows /= size(data) ) then
        error = .true.
        return
    endif

    allocate( new_data(size(filled_table%values)+size(data)), stat=ierror)

    new_data(1:size(filled_table%values))  = filled_table%values
    new_data(size(filled_table%values)+1:) = data

    call move_alloc( new_data, filled_table%values )
end subroutine add_data_to_table

end module tables_module
