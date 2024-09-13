SUBROUTINE POROSupdate(kcs,dps,s1,nub,mub,nlb,mlb,nmaxus,mmax,dt,MF, gdp)
!
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
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
!  $Id: POROSupdate.f90 6272 2016-07-08 15:16:42Z platzek $
!  $HeadURL: 
!!--description-----------------------------------------------------------------`
!
!    Function: update porosity due to encroachment of vegetation
!
!    Author: Alberto Canestrelli
!
!---pseudo code and references--------------------------------------------------
! NONE
!---declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp), dimension(:,:), pointer :: dpL
    real(fp), dimension(:,:), pointer :: dpH
    real(fp), dimension(:,:), pointer :: poros
    integer                 , pointer :: typeVEGencr
    real(fp)                , pointer :: ELEVencr
    real(fp), dimension(:,:), pointer :: TIMElowDEPTH
    real(fp), dimension(:,:), pointer :: TIMEsubmerged
    real(fp)                , pointer :: timeFORdisrupt
    real(fp)                , pointer :: THRdepVEGET
    real(fp)                , pointer :: timeFORenchr
!
! Global variables
!
    real(prec), dimension(nlb:nub,mlb:mub)                              , intent(inout) :: dps
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: s1
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcs
    integer                                          , intent(in)    :: nlb
    integer                                          , intent(in)    :: nub
    integer                                          , intent(in)    :: mlb
    integer                                          , intent(in)    :: mub
    integer                                          , intent(in)    :: nmaxus
    integer                                          , intent(in)    :: mmax
    real(fp)                                         , intent(in)    :: dt
    real(fp)                                         , intent(in)    :: MF
! Local variables
!
    integer :: n 
    integer :: m
    integer :: kn
    integer :: km
    integer :: cont
    real(fp) :: depthL
    real(fp) :: depthH
!
! executable statements -------------------------------------------------------
!
    dpL            => gdp%gdimbound%dpL
    dpH            => gdp%gdimbound%dpH
    poros          => gdp%gdimbound%poros
    typeVEGencr    => gdp%gdimbound%typeVEGencr
    ELEVencr       => gdp%gdimbound%ELEVencr
    TIMElowDEPTH   => gdp%gdimbound%TIMElowDEPTH
    TIMEsubmerged  => gdp%gdimbound%TIMEsubmerged
    timeFORdisrupt => gdp%gdimbound%timeFORdisrupt
    THRdepVEGET    => gdp%gdimbound%THRdepVEGET
    timeFORenchr   => gdp%gdimbound%timeFORenchr

    SELECT CASE(typeVEGencr)
    CASE(0) ! no encroachment
    CASE(1)
       !
       ! fix isolated cut cells inside vegetated area
       !
       do m=2,mmax-1         !RESHAPE_CYCLE  1, nmax      and use kcs properly
          do n=2,nmaxus-1    !RESHAPE_CYCLE  1, mmaxus   
             cont =0
             if (comparereal(poros(n,m),0._fp)>0.and.comparereal(poros(n,m),1._fp)<0) then
                do km = m-1,m+1   !3x3 stencil
                   do kn = n-1,n+1
                      if (comparereal(poros(kn,km),0._fp)==0.and..not.(kn==n.and.km==m)) then
                         cont = cont + 1
                      endif
                   enddo
                enddo
                if (cont==8) then
                  dps(n,m) = dpH(n,m)*(1._fp-poros(n,m))+dpL(n,m)*poros(n,m)
                  dpL(n,m) = dps(n,m)
                  dpH(n,m) = dps(n,m)
                !  poros(n,m) = 0._fp this is determined below based on new elevation. I should consider making it vegetated.
                endif
             endif
          enddo
       enddo 
       !
       ! encroachment of vegetation
       ! 
       do m=2,mmax-1
          do n=2,nmaxus-1
             if (-dpL(n,m) .gt. ELEVencr) then !and clearly dpH(n,m) .gt. ELEVencr too
                if (comparereal(poros(n,m),0._fp)>0) then
                   dps(n,m) = dpH(n,m)*(1._fp-poros(n,m))+dpL(n,m)*poros(n,m)
                   dpL(n,m) = dps(n,m)
                   dpH(n,m) = dps(n,m)
                !elseif (comparereal(poros(n,m),1._fp)==0) continue poros becomes 0 below
                !elseif (comparereal(poros(n,m),0._fp)==0) vegetated cells stays vegetated
                endif
                poros(n,m) = 0._fp
             elseif (-dpH(n,m) .lt. ELEVencr) then !and clearly dpL(n,m) .lt. ELEVencr too
                if (comparereal(poros(n,m),0._fp)>0.and.comparereal(poros(n,m),1._fp)<0) then
                   dps(n,m) = dpH(n,m)*(1._fp-poros(n,m))+dpL(n,m)*poros(n,m) 
                   dpL(n,m) = dps(n,m)
                   dpH(n,m) = dps(n,m)
                !else   !poros is zero or 1,dpL=dpH and poros is set to 1 below
                  ! continue
                endif
                poros(n,m) = 1._fp
             endif
          enddo
       enddo 
    CASE(-999) ! CHANNEL FROM Q TO H BOUNDARY ONLY:encroachment for cells below the water surface slope fr (with a threshold)
    CASE(2) ! IF DEPTH LOWER TRESHOLD FOR MORE THAN N TIME IT BECOMES VEGETATE
       do m=2,mmax-1
          do n=2,nmaxus-1
             if (kcs(n,m) == 1) then 
                depthL = s1(n,m)+dpL(n,m)
                depthH = s1(n,m)+dpH(n,m)
                if (depthL<THRdepVEGET) then
                   TIMElowDEPTH(n,m) = TIMElowDEPTH(n,m) + dt
                   TIMEsubmerged(n,m) = 0._fp
                   if (TIMElowDEPTH(n,m)*MF>timeFORenchr) then 
                      dps(n,m) = dpH(n,m)*(1._fp-poros(n,m))+dpL(n,m)*poros(n,m)
                      dpL(n,m) = dps(n,m)
                      dpH(n,m) = dps(n,m)
                      poros(n,m) = 0._fp
                      TIMElowDEPTH(n,m) = 0._fp
                   endif
                elseif (depthH>1.5_fp*THRdepVEGET) then  !reset time
                   TIMElowDEPTH(n,m) = 0._fp
                   TIMEsubmerged(n,m) = TIMEsubmerged(n,m) + dt
                   if (TIMEsubmerged(n,m)*MF>timeFORdisrupt) then 
                      dps(n,m) = dpH(n,m)*(1._fp-poros(n,m))+dpL(n,m)*poros(n,m)
                      dpL(n,m) = dps(n,m)
                      dpH(n,m) = dps(n,m)
                      poros(n,m) = 1._fp
                      TIMEsubmerged(n,m) = 0._fp
                   endif
                endif 
             endif
          enddo
       enddo
    CASE DEFAULT
       write(*,*) 'wrong choice for typeVEGencr'
       call d3stop(1, gdp)
    END SELECT

  return
end subroutine POROSupdate
