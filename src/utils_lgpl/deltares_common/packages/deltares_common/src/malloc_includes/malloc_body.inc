#define REALLOC1(SUBNAME, DTYPE) \
   subroutine SUBNAME(arr, uindex, lindex, stat, fill, shift, keepExisting); \
   implicit none; \
   DTYPE, allocatable, intent(inout) :: arr(:); \
   integer, intent(in)            :: uindex; \
   integer, intent(in), optional  :: lindex; \
   integer, intent(out), optional :: stat; \
   DTYPE, intent(in), optional    :: fill; \
   integer, intent(in), optional  :: shift; \
   logical, intent(in), optional  :: keepExisting; \
   DTYPE, allocatable :: temp(:); \
   integer :: old_l_index, old_u_index; \
   integer :: new_l_index, new_u_index; \
   integer :: data_l_index, data_u_index; \
   integer :: shift_; \
   integer :: local_err; \
   logical :: keepExisting_; \
   logical :: allocated_old; \
   allocated_old = allocated(arr); \
   local_err = 0; \
   new_l_index = 1; \
   new_u_index = uindex; \
   shift_ = 0; \
   keepExisting_ = .true.; \
   if (present(lindex)) then; \
      new_l_index = lindex; \
   end if; \
   if (present(shift)) then; \
      shift_ = shift; \
   end if; \
   if (present(keepExisting)) then; \
      keepExisting_ = keepExisting; \
   end if; \
   if (allocated_old) then; \
      old_l_index = lbound(arr, 1); \
      old_u_index = ubound(arr, 1); \
      if (new_l_index == old_l_index .and. new_u_index == old_u_index .and. shift_ == 0) then; \
         if (.not. keepExisting_ .and. present(fill)) then; \
            arr = fill; \
         end if; \
         return; \
      end if; \
   end if; \
   allocate(temp(new_l_index:new_u_index), stat=local_err); \
   if (local_err /= 0) then; \
      goto 999; \
   end if; \
   if (present(fill)) then; \
      temp = fill; \
   end if; \
   if (keepExisting_ .and. allocated_old) then; \
      data_l_index = max(old_l_index + shift_, new_l_index); \
      data_u_index = min(old_u_index + shift_, new_u_index); \
      if (data_l_index <= data_u_index) then; \
         temp(data_l_index:data_u_index) = arr(data_l_index - shift_:data_u_index - shift_); \
      end if; \
   end if; \
   call move_alloc(temp, arr); \
999 continue; \
   if (present(stat)) then; \
      stat = local_err; \
   end if; \
   end subroutine SUBNAME

#define REALLOC1P(SUBNAME, DTYPE) \
   subroutine SUBNAME(arr, uindex, lindex, stat, fill, shift, keepExisting); \
   implicit none; \
   DTYPE, pointer, intent(inout) :: arr(:); \
   integer, intent(in)            :: uindex; \
   integer, intent(in), optional  :: lindex; \
   integer, intent(out), optional :: stat; \
   DTYPE, intent(in), optional    :: fill; \
   integer, intent(in), optional  :: shift; \
   logical, intent(in), optional  :: keepExisting; \
   DTYPE, pointer :: temp(:); \
   integer :: old_l_index, old_u_index; \
   integer :: new_l_index, new_u_index; \
   integer :: data_l_index, data_u_index; \
   integer :: shift_; \
   integer :: local_err; \
   logical :: keepExisting_; \
   logical :: allocated_old; \
   allocated_old = associated(arr); \
   local_err = 0; \
   new_l_index = 1; \
   new_u_index = uindex; \
   shift_ = 0; \
   keepExisting_ = .true.; \
   if (present(stat)) then; \
      stat = 0; \
   end if; \
   if (present(lindex)) then; \
      new_l_index = lindex; \
   end if; \
   if (present(shift)) then; \
      shift_ = shift; \
   end if; \
   if (present(keepExisting)) then; \
      keepExisting_ = keepExisting; \
   end if; \
   if (allocated_old) then; \
      old_l_index = lbound(arr, 1); \
      old_u_index = ubound(arr, 1); \
      if (new_l_index == old_l_index .and. new_u_index == old_u_index .and. shift_ == 0) then; \
         if (.not. keepExisting_ .and. present(fill)) then; \
            arr = fill; \
         end if; \
         return; \
      end if; \
   end if; \
   allocate(temp(new_l_index:new_u_index), stat=local_err); \
   if (local_err /= 0) then; \
      goto 999; \
   end if; \
   if (present(fill)) then; \
      temp = fill; \
   end if; \
   if (keepExisting_ .and. allocated_old) then; \
      data_l_index = max(old_l_index + shift_, new_l_index); \
      data_u_index = min(old_u_index + shift_, new_u_index); \
      if (data_l_index <= data_u_index) then; \
         temp(data_l_index:data_u_index) = arr(data_l_index - shift_:data_u_index - shift_); \
      end if; \
   end if; \
   if (associated(arr)) then; \
      deallocate(arr); \
   end if; \
   arr => temp; \
999 continue; \
   if (present(stat)) then; \
      stat = local_err; \
   end if; \
   end subroutine SUBNAME