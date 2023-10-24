module morstatistics
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
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
!  
!  
!-------------------------------------------------------------------------------
private

!
! public routines
!
public morstats

interface morstats
    module procedure morstats_simple
    module procedure morstats_full
end interface morstats

contains

subroutine morstats_simple(gderosed, gdmorpar, dbodsd, nmlb, nmub, lsedtot)
    use globaldata
    !
    implicit none
!
! Global variables
!
    integer                                , intent(in)  :: nmlb
    integer                                , intent(in)  :: nmub
    integer                                , intent(in)  :: lsedtot
    real(fp), dimension(lsedtot, nmlb:nmub), intent(in)  :: dbodsd !  change in sediment composition, units : kg/m2
    type (sedtra_type)                     , target      :: gderosed
    type (morpar_type)                     , intent(in)  :: gdmorpar
!
! Local variables
!
    real(fp), dimension(:,:)            , pointer :: statqnt
    integer                                       :: lsed
    integer                                       :: nm
!
!! executable statements -------------------------------------------------------
!
    statqnt   => gderosed%statqnt
    !
    if (gdmorpar%moroutput%nstatqnt > 0) then
       do nm = nmlb, nmub
           do lsed = 1, lsedtot
               statqnt(nm,1+lsed) = statqnt(nm,1+lsed) + dbodsd(lsed, nm)
           enddo
       enddo
    endif
end subroutine morstats_simple


subroutine morstats_full(gdp, dbodsd, s1, dps, umean, vmean, sbuu, sbvv, ssuu, ssvv, nmlb, nmub, lsedtot, lsedsus)
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer                                , intent(in)  :: nmlb
    integer                                , intent(in)  :: nmub
    integer                                , intent(in)  :: lsedtot
    integer                                , intent(in)  :: lsedsus
    real(fp), dimension(lsedtot, nmlb:nmub), intent(in)  :: dbodsd !  change in sediment composition, units : kg/m2
    real(fp), dimension(nmlb:nmub)         , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(nmlb:nmub)       , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmlb:nmub)         , intent(in)  :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmlb:nmub)         , intent(in)  :: vmean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmlb:nmub, lsedtot), intent(in)  :: sbuu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmlb:nmub, lsedtot), intent(in)  :: sbvv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmlb:nmub, lsedsus), intent(in)  :: ssuu
    real(fp), dimension(nmlb:nmub, lsedsus), intent(in)  :: ssvv
!
! Local variables
!
    integer                              , pointer :: nstatqnt
    real(fp), dimension(:)               , pointer :: rhosol
    real(fp), dimension(:)               , pointer :: cdryb
    type (moroutputtype)                 , pointer :: moroutput  ! structure containing morphology output options
    real(fp), dimension(:,:)             , pointer :: statqnt
    integer                                        :: lsed
    integer                                        :: ndm
    integer                                        :: nm
    integer                                        :: nmaxddb
    integer                                        :: nmd
    real(fp)                                       :: q
    real(fp)                                       :: qu
    real(fp)                                       :: qv
    real(fp)                                       :: rhol
    real(fp)                                       :: wght
!
!! executable statements -------------------------------------------------------
!
    rhosol         => gdp%gdsedpar%rhosol
    cdryb          => gdp%gdsedpar%cdryb
    moroutput      => gdp%gdmorpar%moroutput
    nstatqnt       => gdp%gdmorpar%moroutput%nstatqnt
    statqnt        => gdp%gderosed%statqnt
    nmaxddb        =  gdp%d%nmax - 2*gdp%d%ddbound
    !
    if (nstatqnt == 0) return
    !
    do nm = nmlb, nmub
        nmd = nm - nmaxddb
        ndm = nm - 1
        !
        ! mean and std computed using deposited mass as weighting factor
        !
        wght = 0.0_fp
        do lsed = 1, lsedtot
            wght = wght + max(0.0_fp,dbodsd(lsed, nm)) ! only count sedimentation
        enddo
        statqnt(nm,1) = statqnt(nm,1) + wght
        !
        do lsed = 1, lsedtot
            statqnt(nm,1+lsed) = statqnt(nm,1+lsed) + dbodsd(lsed, nm)
        enddo
        !
        if (moroutput%statflg(1,1)>0) then
            q = s1(nm)+real(dps(nm),fp)
            call local_stats(moroutput%statflg(:,1), nm, q, wght)
        endif
        !
        if (moroutput%statflg(1,2)>0) then
            qu = 0.5_fp*(umean(nm)+umean(nmd))
            qv = 0.5_fp*(vmean(nm)+vmean(ndm))
            q = sqrt(qu**2 + qv**2)
            call local_stats(moroutput%statflg(:,2), nm, q, wght)
        endif
        !
        if (moroutput%statflg(1,3)>0) then
            qu = 0.0_fp
            qv = 0.0_fp
            do lsed = 1, lsedtot
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(lsed)
                case (2)
                   rhol = rhosol(lsed)
                end select
                qu = qu + 0.5_fp*(sbuu(nm, lsed)+sbuu(nmd, lsed))/rhol
                qv = qv + 0.5_fp*(sbvv(nm, lsed)+sbvv(ndm, lsed))/rhol
            enddo
            q = sqrt(qu**2 + qv**2)
            call local_stats(moroutput%statflg(:,3), nm, q, wght)
        endif
        !
        if (moroutput%statflg(1,4)>0) then
            qu = 0.0_fp
            qv = 0.0_fp
            do lsed = 1, lsedsus
                select case(moroutput%transptype)
                case (0)
                   rhol = 1.0_fp
                case (1)
                   rhol = cdryb(lsed)
                case (2)
                   rhol = rhosol(lsed)
                end select
                qu = qu + 0.5_fp*(ssuu(nm, lsed)+ssuu(nmd, lsed))/rhol
                qv = qv + 0.5_fp*(ssvv(nm, lsed)+ssuu(ndm, lsed))/rhol
            enddo
            q = sqrt(qu**2 + qv**2)
            call local_stats(moroutput%statflg(:,4), nm, q, wght)
        endif
    enddo
    
contains

    subroutine local_stats(idx, nm, q, wght)
    integer, dimension(5) :: idx
    integer               :: nm
    real(fp)              :: q
    real(fp)              :: wght
    !
    if (idx(2)>0 .and. wght>0.0_fp) then
        statqnt(nm,idx(2)) = min(statqnt(nm,idx(2)),q)
    endif
    if (idx(3)>0 .and. wght>0.0_fp) then
        statqnt(nm,idx(3)) = max(statqnt(nm,idx(3)),q)
    endif
    if (idx(4)>0) then
        statqnt(nm,idx(4)) = statqnt(nm,idx(4)) + wght*q
    endif
    if (idx(5)>0) then
        statqnt(nm,idx(5)) = statqnt(nm,idx(5)) + wght*q**2
    endif
    end subroutine local_stats
   
end subroutine morstats_full

end module morstatistics
