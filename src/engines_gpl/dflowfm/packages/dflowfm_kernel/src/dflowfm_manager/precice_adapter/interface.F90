module precice_adapter_interface

   implicit none(type, external)

   private
   public :: precice_adapter_interface_t

   type, abstract :: precice_adapter_interface_t
   contains
      procedure(initialize_function_interface), deferred :: initialize
      procedure(update_function_interface), deferred :: update
      procedure(finalize_function_interface), deferred :: finalize
   end type

   interface
      subroutine initialize_function_interface(self)
         import precice_adapter_interface_t
         implicit none(type, external)

         class(precice_adapter_interface_t), intent(inout) :: self
      end subroutine initialize_function_interface

      subroutine update_function_interface(self, timestep)
         use precision, only: dp
         import precice_adapter_interface_t
         implicit none(type, external)

         class(precice_adapter_interface_t), intent(inout) :: self
         real(kind=dp), intent(in) :: timestep
      end subroutine update_function_interface

      subroutine finalize_function_interface(self)
         import precice_adapter_interface_t
         implicit none(type, external)

         class(precice_adapter_interface_t), intent(inout) :: self
      end subroutine finalize_function_interface

   end interface

end module precice_adapter_interface
