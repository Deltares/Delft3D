subroutine bedLOADperiod(sbuu,sbvv,lsedtot,hdt,nlb,nub,mlb,mub,gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
!  $Id: My_intersec.f90 
!  $HeadURL:
!!--description-----------------------------------------------------------------
!
!   Function:   Copy the total sediment transport from the water level (periodic) boundary
!               to the discharge (periodic) boundary
!
!  Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                 , pointer :: nrPER
    integer                 , pointer :: PERIODalongM
    integer, dimension(:)   , pointer :: mPQ_ext
    integer, dimension(:)   , pointer :: nPQ_ext
    integer, dimension(:)   , pointer :: mPH_int
    integer, dimension(:)   , pointer :: nPH_int
    integer                 , pointer :: shiftQM
    integer                 , pointer :: shiftQN
    real(fp), dimension(:,:), pointer :: qfilt_bdl
    real(fp)                , pointer :: reltim_qtq_bdl
!
! global variables
!
  real(fp)  , dimension(nlb:nub,mlb:mub,lsedtot)            ,intent(inout)   :: sbuu  
  real(fp)  , dimension(nlb:nub,mlb:mub,lsedtot)            ,intent(inout)   :: sbvv
  real(fp), intent(in)       :: hdt
  integer, intent(in)        :: nlb
  integer, intent(in)        :: nub
  integer, intent(in)        :: mlb
  integer, intent(in)        :: mub
  integer, intent(in)        :: lsedtot
!
! local variables
!
  integer                    :: k,l
  real(fp)                   :: a
  real(fp)                   :: qnew
  logical,SAVE               :: firstCALL = .TRUE.
!
! executable statements -------------------------------------------------------
!                            
    nrPER          => gdp%gdimbound%nrPER
    PERIODalongM   => gdp%gdimbound%PERIODalongM
    mPQ_ext        => gdp%gdimbound%mPQ_ext
    nPQ_ext        => gdp%gdimbound%nPQ_ext
    mPH_int        => gdp%gdimbound%mPH_int
    nPH_int        => gdp%gdimbound%nPH_int
    shiftQM        => gdp%gdimbound%shiftQM
    shiftQN        => gdp%gdimbound%shiftQN
    qfilt_bdl      => gdp%gdimbound%qfilt_bdl
    reltim_qtq_bdl => gdp%gdimbound%reltim_qtq_bdl
!    
    if (PERIODalongM==1) then
       do l = 1,lsedtot  
          do k=1,nrPER 
             qnew = sbuu(nPH_int(k)+shiftQN,mPH_int(k)+shiftQM,l)  
             ! write(12341234,*) k,sbuu(nPH_int(k)+shiftQN,mPH_int(k)+shiftQM,l)
             if (reltim_qtq_bdl>0) then
                if (.NOT.firstCALL) then
                   a = exp( - hdt/reltim_qtq_bdl/60.0_fp) ! hdt (or dt) in sec, reltim in minutes
                   qfilt_bdl(k,l) = a*qfilt_bdl(k,l) + (1._fp - a)*qnew
                else
                   qfilt_bdl(k,l) = qnew
                endif
             else
                qfilt_bdl(k,l) = qnew 
             endif
             sbuu(nPQ_ext(k)+shiftQN,mPQ_ext(k)+shiftQM,l) = qfilt_bdl(k,l)
          enddo
       enddo
    else
       do l = 1,lsedtot  
          do k=1,nrPER  
             qnew = sbvv(nPH_int(k)+shiftQN,mPH_int(k)+shiftQM,l)     
             if (reltim_qtq_bdl>0) then
                if (.NOT.firstCALL) then
                   a = exp( - hdt/reltim_qtq_bdl/60.0_fp) ! hdt (or dt) in sec, reltim in minutes
                   qfilt_bdl(k,l) = a*qfilt_bdl(k,l) + (1._fp - a)*qnew
                else
                   qfilt_bdl(k,l) = qnew
                endif
             else
                qfilt_bdl(k,l) = qnew 
             endif     
             sbvv(nPQ_ext(k)+shiftQN,mPQ_ext(k)+shiftQM,l) = qfilt_bdl(k,l)                           
          enddo
       enddo
    endif
    !
    firstCALL = .false.
    !
    return
end subroutine bedLOADperiod
