module precice_adapter_facade
   use precice_adapter_interface, only: precice_adapter_interface_t
   use precice_adapter_builder, only: precice_adapter_builder_t
   
   implicit none(type, external)
   
   private
   public :: precice_adapter_enable
   public :: precice_adapter_is_enabled
   public :: precice_adapter_get_builder
   public :: precice_adapter_get_adapter
   public :: precice_adapter_interface_t
   public :: precice_adapter_builder_t
   
   logical :: is_enabled = .false.
   class(precice_adapter_builder_t), allocatable, target :: builder_instance
   class(precice_adapter_interface_t), allocatable, target :: adapter_instance
   
   
contains

   subroutine precice_adapter_enable()      
      is_enabled = .true.
   end subroutine precice_adapter_enable

   function precice_adapter_is_enabled() result(enabled)
      logical :: enabled
      enabled = is_enabled
   end function precice_adapter_is_enabled

   function precice_adapter_get_builder() result(builder)
      class(precice_adapter_builder_t), pointer :: builder

      if (.not. allocated(builder_instance)) then
         builder_instance = precice_adapter_builder_t()
      end if
      
      builder => builder_instance
   end function precice_adapter_get_builder

   function precice_adapter_get_adapter() result(adapter)
      class(precice_adapter_interface_t), pointer :: adapter

      class(precice_adapter_builder_t), pointer :: builder

      if (.not. allocated(adapter_instance)) then
         builder => precice_adapter_get_builder()
         adapter_instance = builder%build()
      end if

      adapter => adapter_instance
   end function precice_adapter_get_adapter
   
end module precice_adapter_facade