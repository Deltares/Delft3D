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

module m_connection_data
    use m_waq_precision

    implicit none

    type :: connection_data
        logical :: incoming                 !< Identifies the direction: if true, data from outside
        integer(kind=int_wp) :: category    !< Which category of data: concentrations, process parameters ...
        integer(kind=int_wp) :: buffer_idx  !< Index into the RBUF array
        integer(kind=int_wp) :: system_idx  !< Index of the substance or quantity in general
        real(kind=dp), pointer :: p_value   !< Copy of the value received/sent by DELWAQ - must be a pointer!
        character(len=40) :: exchange_name  !< Name used by get_var to identify the item
    end type

end module m_connection_data
