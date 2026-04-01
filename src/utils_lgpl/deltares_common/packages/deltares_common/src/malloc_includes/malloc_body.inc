      DTYPE, DATTR, intent(inout) :: arr(:)
      integer, intent(in)            :: uindex
      integer, intent(in), optional  :: lindex
      integer, intent(out), optional :: stat
      DTYPE, intent(in), optional   :: fill
      integer, intent(in), optional  :: shift
      logical, intent(in), optional :: keepExisting

#ifdef DTYPE_CHAR
      character(len=len(arr)), DATTR :: temp(:)
#else
      DTYPE, DATTR :: temp(:)
#endif
      integer :: old_l_index, old_u_index
      integer :: new_l_index, new_u_index
      integer :: data_l_index, data_u_index
      integer :: shift_
      logical :: keepExisting_
      logical :: allocated_old

      if (present(stat)) stat = 0

      new_l_index = 1
      if (present(lindex)) new_l_index = lindex
      new_u_index = uindex

      shift_ = 0
      if (present(shift)) shift_ = shift

      keepExisting_ = .true.
      if (present(keepExisting)) keepExisting_ = keepExisting

      allocated_old = IS_ALLOCATED(arr)

      if (allocated_old) then
         old_l_index = lbound(arr, 1)
         old_u_index = ubound(arr, 1)

         if (new_l_index == old_l_index .and. new_u_index == old_u_index .and. shift_ == 0) then
            if (.not. keepExisting_ .and. present(fill)) arr = fill
            return
         end if
      end if

      ! Reallocation required
      allocate(temp(new_l_index:new_u_index))

      if (present(fill)) temp = fill

      if (keepExisting_ .and. allocated_old) then
         data_l_index = max(old_l_index + shift_, new_l_index)
         data_u_index = min(old_u_index + shift_, new_u_index)
         ! arr access below is safe, because:
         ! data_l_index - shift_ >= (old_l_index + shift_) - shift_ = old_l_index
         ! data_u_index - shift_ <= (old_u_index + shift_) - shift_ = old_u_index
         if (data_l_index <= data_u_index) then
            temp(data_l_index:data_u_index) = arr(data_l_index - shift_:data_u_index - shift_)
         end if
      end if

      MOVE_ALLOC