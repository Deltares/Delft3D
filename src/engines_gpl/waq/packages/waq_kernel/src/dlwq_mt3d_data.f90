!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

module dlwq_mt3d_data

    ! configuration

    logical                   :: mt3d_init = .true. ! triggers initialisation
    logical                   :: mt3d_active        ! if coupling is active
    logical                   :: mt3d_online        ! if online coupling is active
    logical                   :: mt3d_to_delwaq     ! if communication is from mt3d to delwaq
    logical                   :: delwaq_to_mt3d     ! if communication is from delwaq to mt3d
    character(len=256)        :: mt3d_config        ! configuration file
    character(len=256)        :: dwqflagfile        ! dwq flag file for online coupling
    character(len=256)        :: mt3dflagfile       ! mt3d flag file for online coupling
    character(len=256)        :: dwqcommfile        ! dwq communication file
    character(len=256)        :: mt3dcommfile       ! mt3d communication file
    integer                   :: lunmt3dflag        ! unit number flag file
    integer                   :: lunmt3dcomm        ! unit number comm file
    integer                   :: lundwqflag         ! unit number flag file
    integer                   :: lundwqcomm         ! unit number comm file

    ! timer stuff

    logical                   :: first_step = .true.! first step indication
    integer                   :: idt_delwaq         ! original delwaq idt
    integer                   :: idt_hyd            ! hyd coupling timestep
    real                      :: rdt_mt3d           ! mt3d timestep in days
    integer                   :: idt_mt3d           ! mt3d timestep in seconds
    integer                   :: itstop_mt3d        ! stop time current mt3d step
    integer                   :: itstop_hyd         ! stop time current hyd step

    ! substance mapping

    integer                   :: no_sub_gsl         ! number of substances in gsl coupling
    integer, allocatable      :: isub_gsl(:)        ! boundary pointer gsl node

    ! boundary mapping

    integer                   :: no_gsl             ! number of ground water surface water linkages
    integer, allocatable      :: ib_gsl(:)          ! boundary pointer gsl node

    ! exchange mapping

    integer                   :: noq_gsl            ! number of exchanges in gsl nodes
    integer, allocatable      :: iq_iq_gsl(:)       ! exchange number
    integer, allocatable      :: igsl_iq_gsl(:)     ! gsl number

    ! concentration array's

    real, allocatable         :: gsl_conc_upw(:,:)  ! concentrations read from mt3d upward seepage
    real, allocatable         :: gsl_conc_inf(:,:)  ! concentrations read from mt3d infiltration
    real, allocatable         :: gsl_cum_upw(:,:)   ! cummulative substance flux
    real, allocatable         :: gsl_cum_inf(:,:)   ! cummulative substance flux
    real, allocatable         :: gsl_q(:)           ! cummulative water flux
    real, allocatable         :: gsl_cumq(:)        ! cummulative water flux
    real, allocatable         :: gsl_q_upw(:)       ! water flux upward seepage
    real, allocatable         :: gsl_q_inf(:)       ! water flux infiltration
    real, allocatable         :: gsl_cumq_upw(:)    ! water flux upward seepage
    real, allocatable         :: gsl_cumq_inf(:)    ! water flux infiltration
    real, allocatable         :: gsl_prev_upw(:,:)  ! previous value in balance array
    real, allocatable         :: gsl_prev_inf(:,:)  ! previous value in balance array

end module dlwq_mt3d_data
