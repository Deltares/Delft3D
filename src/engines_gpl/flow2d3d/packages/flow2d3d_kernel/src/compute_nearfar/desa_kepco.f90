subroutine desa_kepco(x_jet   ,y_jet    ,z_jet   ,s_jet   ,nrow    , &
              & kcs     ,xz       ,yz      ,dps     ,s0      , &
              & nmmax   ,thick    ,kmax    ,lstsci  ,lsal    , &
              & ltem    ,bv_jet   ,bh_jet   ,idis    , &
              & xstart  ,xend     ,ystart  ,yend    ,r0      , &
              & linkinf ,gdp      )
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
!  $Id: desa_kepco.f90 6274 2016-07-08 18:15:23Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/desa_kepco.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Converts Jet3D/Corjet/Cortime/Cormix output to delft3d sources
!              following the DESA methodology of Joseph Lee
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    integer ,dimension(:)          , pointer :: m_intake
    integer ,dimension(:)          , pointer :: n_intake
    integer ,dimension(:)          , pointer :: k_intake
    real(fp),dimension(:)          , pointer :: q_diff
    real(fp),dimension(:,:)        , pointer :: const_diff
    real(fp),dimension(:,:,:)      , pointer :: disnf
    real(fp),dimension(:,:,:,:)    , pointer :: sournf
    integer                        , pointer :: lunscr
    integer                        , pointer :: lundia
    logical , dimension(:)         , pointer :: flbcktemp
!
! Global variables
!
    integer                                                    , intent(in)    :: idis     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)    :: kmax     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)    :: lstsci   !  Description and declaration in tricom.igs
    integer                                                    , intent(in)    :: lsal     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)    :: ltem     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)    :: nmmax    !  Description and declaration in tricom.igs
    integer                                                    , intent(in)    :: nrow     !  Description and declaration in
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)    :: kcs      !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)                                                   , intent(out)   :: xstart
    real(fp)                                                   , intent(out)   :: xend
    real(fp)                                                   , intent(out)   :: ystart
    real(fp)                                                   , intent(out)   :: yend
    real(fp)   , dimension(8)                                  , intent(inout) :: linkinf
    real(fp)   , dimension(nrow)                               , intent(in)    :: x_jet    !  Description and declaration in
    real(fp)   , dimension(nrow)                               , intent(in)    :: y_jet    !  Description and declaration in
    real(fp)   , dimension(nrow)                               , intent(in)    :: z_jet    !  Description and declaration in
    real(fp)   , dimension(nrow)                               , intent(in)    :: s_jet    !  Description and declaration in
    real(fp)   , dimension(nrow)                               , intent(in)    :: bv_jet   !  Description and declaration in
    real(fp)   , dimension(nrow)                               , intent(in)    :: bh_jet   !  Description and declaration in
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)    :: s0       !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub,kmax,lstsci)  , intent(in)    :: r0       !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)    :: xz       !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)    :: yz       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(kmax)                               , intent(in)    :: thick    !  Description and declaration in esm_alloc_real.f90 gs
    real(prec) , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)    :: dps      !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                              :: ierror
    integer                              :: irow
    integer                              :: idum
    integer                              :: iidis
    integer                              :: k
    integer                              :: k_end_top
    integer                              :: k_end_down
    integer                              :: k_irow
    integer                              :: k_last
    integer                              :: k_start
    integer                              :: lcon
    integer                              :: nm
    integer                              :: nm_end
    integer                              :: nm_irow
    integer                              :: nm_last
    integer                              :: nm_start
    integer                              :: nm_tmp
    integer                              :: no_dis
    integer                              :: n_tmp
    integer                              :: m_tmp
    real(fp)                             :: add
    real(fp)                             :: dis_dil
    real(fp)                             :: dis_tot
    real(fp)                             :: q1
    real(fp)                             :: q2
    real(fp)                             :: pi
    real(fp)                             :: thick_tot
    real(fp)                             :: ang_end
    real(fp)                             :: dx
    real(fp)                             :: dy
    real(fp),dimension(:), allocatable   :: weight
    integer, dimension(:), allocatable   :: nm_dis
    
    ! Temporary fix to ensure discharging of mass in case of S = 0 or T = 0
    
    real(fp)                             :: eps_conc
    logical                              :: inside
!
!! executable statements -------------------------------------------------------
!
    lunscr         => gdp%gdinout%lunscr
    lundia         => gdp%gdinout%lundia
    m_intake       => gdp%gdnfl%m_intake
    n_intake       => gdp%gdnfl%n_intake
    k_intake       => gdp%gdnfl%k_intake
    q_diff         => gdp%gdnfl%q_diff
    const_diff     => gdp%gdnfl%const_diff
    disnf          => gdp%gdnfl%disnf
    sournf         => gdp%gdnfl%sournf
    flbcktemp      => gdp%gdheat%flbcktemp

    dis_dil   = 0.0_fp
    dis_tot   = 0.0_fp
    thick_tot = 0.0_fp
    pi        = acos(-1.0_fp)
    
    eps_conc  = 1.0e-12_fp

    !
    ! Only if cormix simulation was succesfull
    !
    if (nrow > 0) then

       disnf   (1:nmmax, 1:kmax,idis)           = 0.0_fp
       sournf  (1:nmmax, 1:kmax, 1:lstsci,idis) = 0.0_fp

       !
       ! Get characteristics starting point
       !

       call findnmk(xz      , yz      , dps     , s0       , kcs      ,nmmax   , &
                  & thick   , kmax    , x_jet(1), y_jet(1) , z_jet(1) ,nm_start, &
                  & k_start , inside  , gdp    )

       nm_last = nm_start
       k_last  = k_start

       !
       ! Get characteristics end      point
       !

       call findnmk(xz       , yz      , dps         , s0          , kcs         ,nmmax   , &
                    thick    , kmax    , x_jet(nrow) , y_jet(nrow) , z_jet(nrow) ,nm_end  , &
                    k_end_top, inside  , gdp    )

       !
       ! For postproc essing stor begin and end coordinates of the plume trajectory
       !

       linkinf(7) = nm_start
       linkinf(8) = nm_end

       !
       ! Cycle over points in Cormix output file
       !

       do irow = 2, nrow
          !
          ! Get position of point
          !
          call findnmk (xz      , yz      , dps         , s0          , kcs         ,nmmax   , &
                        thick   , kmax    , x_jet(irow) , y_jet(irow) , z_jet(irow) ,nm_irow , &
                        k_irow  , inside  , gdp    )

          if (nm_irow == 0 .or. k_irow == 0) then
             nm_irow = nm_last
             k_irow  = k_last
          endif
          nm_last  = nm_irow
          k_last   = k_irow

          !
          ! Fill disch_nf array: Desa Method, subtract the amount of water corresponding with the dilution
          !                      Keep track of total amounts of water, salt in order to discharge the correct
          !                      amounts at the end of the near field
          !
          if (nm_last /= nm_end .or. k_last /= k_end_top) then
             dis_dil                  = 1.0_fp*(s_jet(irow) - s_jet(irow-1))*q_diff(idis)
             dis_tot                  = dis_tot + dis_dil
             disnf   (nm_last,k_last,idis) = disnf   (nm_last,k_last,idis) - dis_dil
          endif
       enddo

       !
       ! Determine the relative thickness over which to distribute the diluted discharge
       !
       call findnmk (xz        , yz      , dps         , s0          , kcs         ,nmmax   , &
                     thick     , kmax    , x_jet(nrow) , y_jet(nrow) , z_jet(nrow) - bv_jet(nrow), nm_end , &
                     k_end_top , inside  , gdp    )

       !call findnmk (xz        , yz      , dps         , s0          , kcs         ,nmmax        , &
       !              thick     , kmax    , x_jet(nrow) , y_jet(nrow) , z_jet(nrow) + bv_jet(nrow), nm_end , &
       !              k_end_down, inside  , gdp    )


       ! Tijdelijk, verdelen over halve waterdiepte (Geeft voor Kepco beste resulaten)

       call findnmk (xz       , yz      , dps         , s0          , kcs         ,nmmax        , &
                    thick     , kmax    , x_jet(nrow) , y_jet(nrow) , z_jet(nrow)               , nm_end , &
                    k_end_down, inside  , gdp    )

!      k_end_top  = k_last
!      k_end_down = k_last

       !
       ! Determine grid cells over which to distribute the diluted discharge, begin and and of horizontal distribution area
       !

       ang_end = atan2((y_jet(nrow) - y_jet(nrow - 1)),(x_jet(nrow) - x_jet(nrow - 1)))
       dx      = -1.0_fp*bh_jet(nrow)*cos(pi/2.0_fp - ang_end)
       dy      =  1.0_fp*bh_jet(nrow)*sin(pi/2.0_fp - ang_end)

!      dx = 0.0_fp
!      dy = 0.0_fp

       xstart   = x_jet(nrow) + dx
       ystart   = y_jet(nrow) + dy
       xend     = x_jet(nrow) - dx
       yend     = y_jet(nrow) - dy

       !
       ! Determine grid cell numbers over which to distribute the diluted discharge
       !

       allocate (nm_dis(1000), stat=ierror)
       allocate (weight(1000), stat=ierror)

       nm_dis   = 0
       weight   = 0.0_fp

       no_dis   = 1
       call findnmk (xz        , yz      , dps         , s0          , kcs         ,nmmax         , &
                     thick     , kmax    , xstart      , ystart      , 0.0_fp      ,nm_tmp        , &
                     idum      , inside  , gdp    )

       nm_dis(1) = nm_tmp
       weight(1) = 0.001_fp


        dx = (xend - xstart)/999.0_fp
        dy = (yend - ystart)/999.0_fp

        do iidis = 1, 999
           call findnmk (xz        , yz      , dps              , s0               , kcs         ,nmmax         , &
                         thick     , kmax    , xstart + iidis*dx, ystart + iidis*dy, 0.0_fp      ,nm_tmp        , &
                         idum      , inside  , gdp    )

            if (nm_tmp /= nm_dis(no_dis)) then
               no_dis = no_dis + 1
               nm_dis(no_dis) = nm_tmp
            endif
            weight(no_dis) = weight(no_dis) + 0.001_fp
        enddo

       !
       ! Distribute sources discharges horizontal and vertical
       !

       add = 0.0_fp


       do iidis = 1, no_dis
          do k = k_end_top, k_end_down
             if (disnf(nm_dis(iidis),k,idis) == 0.0_fp) then
                thick_tot = thick_tot + weight(iidis)*thick(k)
             endif
          enddo
       enddo

       do iidis = 1, no_dis
          do k = k_end_top, k_end_down
             if (disnf(nm_dis(iidis),k,idis) == 0.0_fp) then
                disnf    (nm_dis(iidis),k,idis)         = disnf(nm_dis(iidis),k,idis) + (q_diff(idis) + dis_tot)/(thick_tot/(weight(iidis)*thick(k)))

                if (lsal /= 0) then
                   call coupled (add,r0,kmax,lstsci,lsal,thick,m_intake(idis),n_intake(idis),k_intake(idis),gdp)
                   sournf (nm_dis(iidis),k,lsal,idis)   = q_diff(idis) * (max(const_diff(idis,2),eps_conc) + add)/(thick_tot/(weight(iidis)*thick(k)))
                endif
                if (ltem /= 0) then
                   call coupled (add,r0,kmax,lstsci,ltem,thick,m_intake(idis),n_intake(idis),k_intake(idis),gdp)
                   sournf (nm_dis(iidis),k,ltem,idis)   = q_diff(idis) * (max(const_diff(idis,1),eps_conc) + add)/(thick_tot/(weight(iidis)*thick(k)))
                endif
                do lcon = ltem + 1, lstsci
                   if ( flbcktemp(lcon) ) then
                      ! Backgroundtemerature: discharge with the temeprature last time step in discharge point
                      sournf (nm_dis(iidis), k, lcon,idis) = q_diff(idis) * max(r0(nm_dis(iidis),k,lcon),eps_conc)/(thick_tot/(weight(iidis)*thick(k)))
                   else
                      sournf (nm_dis(iidis), k, lcon,idis) = 1.0_fp*q_diff(idis)/(thick_tot/(weight(iidis)*thick(k)))
                   endif
                enddo
             endif
          enddo
       enddo

       deallocate(nm_dis, stat=ierror)
       deallocate(weight, stat=ierror)

    endif
end subroutine desa_kepco
