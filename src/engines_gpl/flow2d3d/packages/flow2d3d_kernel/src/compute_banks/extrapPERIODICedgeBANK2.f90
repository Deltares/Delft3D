subroutine extrapPERIODICedgeBANK2(gdp)
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
!  Ndryact: delft3d.support@deltares.nl                                         
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
!   Function:  extrapolate EDGEtypeBANK to the respective external periodic boundary,
!              in the way that ghost points in findGhostPoints are correctly found.  
!               
!    Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                       , pointer :: nrPER
    integer, dimension(:,:,:)     , pointer :: EDGEtypeBANK
    integer, dimension(:,:,:)     , pointer :: EDGEtypeBANKerod
    real(fp), dimension(:,:,:)    , pointer :: EDGElenBANK
    real(fp), dimension(:,:,:)    , pointer :: EDGElenWET
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyBANK
    real(fp), dimension(:,:)      , pointer :: gvv_cc
    real(fp), dimension(:,:)      , pointer :: guu_cc
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyBANKerod
    integer, dimension(:)         , pointer :: mPH_ext
    integer, dimension(:)         , pointer :: nPH_ext
    integer, dimension(:)         , pointer :: mPQ_ext
    integer, dimension(:)         , pointer :: nPQ_ext
    integer, dimension(:)         , pointer :: mPH_int
    integer, dimension(:)         , pointer :: mPQ_int
    integer, dimension(:)         , pointer :: mPH_intint
    integer, dimension(:)         , pointer :: mPQ_intint
    integer, dimension(:)         , pointer :: nPH_int
    integer, dimension(:)         , pointer :: nPQ_int
    integer, dimension(:)         , pointer :: nPH_intint
    integer, dimension(:)         , pointer :: nPQ_intint
    integer, dimension(:)         , pointer :: mPH_extext
    integer, dimension(:)         , pointer :: mPQ_extext
    integer, dimension(:)         , pointer :: nPH_extext
    integer, dimension(:)         , pointer :: nPQ_extext
    logical                       , pointer :: twoCELLSperiod
    real(fp)                      , pointer :: LchanPERprojX
    real(fp)                      , pointer :: LchanPERprojY
!
! global variables
! 
!
! local variables
!
  integer                    :: k 
  REAL(FP)                   :: SHIFT(4,2,2)
!
! executable statements -------------------------------------------------------
!
    nrPER            => gdp%gdimbound%nrPER
    EDGEtypeBANK     => gdp%gdimbound%EDGEtypeBANK
    EDGEtypeBANKerod => gdp%gdimbound%EDGEtypeBANKerod
    EDGElenBANK      => gdp%gdimbound%EDGElenBANK
    EDGElenWET       => gdp%gdimbound%EDGElenWET
    EDGExyBANK       => gdp%gdimbound%EDGExyBANK
    gvv_cc           => gdp%gdimbound%gvv_cc
    guu_cc           => gdp%gdimbound%guu_cc
    EDGExyBANKerod   => gdp%gdimbound%EDGExyBANKerod
    mPH_ext          => gdp%gdimbound%mPH_ext
    nPH_ext          => gdp%gdimbound%nPH_ext
    mPQ_ext          => gdp%gdimbound%mPQ_ext
    nPQ_ext          => gdp%gdimbound%nPQ_ext
    mPH_int          => gdp%gdimbound%mPH_int
    mPQ_int          => gdp%gdimbound%mPQ_int
    mPH_intint       => gdp%gdimbound%mPH_intint
    mPQ_intint       => gdp%gdimbound%mPQ_intint
    nPH_int          => gdp%gdimbound%nPH_int
    nPQ_int          => gdp%gdimbound%nPQ_int
    nPH_intint       => gdp%gdimbound%nPH_intint
    nPQ_intint       => gdp%gdimbound%nPQ_intint
    mPH_extext       => gdp%gdimbound%mPH_extext
    mPH_ext          => gdp%gdimbound%mPH_ext
    mPQ_extext       => gdp%gdimbound%mPQ_extext
    nPH_extext       => gdp%gdimbound%nPH_extext
    nPQ_extext       => gdp%gdimbound%nPQ_extext
    twoCELLSperiod   => gdp%gdimbound%twoCELLSperiod
    LchanPERprojX    => gdp%gdimbound%LchanPERprojX
    LchanPERprojY    => gdp%gdimbound%LchanPERprojY
      do k=1,nrPER+1 !+1 cause  I prescribe the last value on the lower edge of the cell 
!
         EDGEtypeBANKerod(:,nPQ_ext(k),mPQ_ext(k)) = EDGEtypeBANKerod(:,nPH_int(k),mPH_int(k)) 
         EDGEtypeBANKerod(:,nPH_ext(k),mPH_ext(k)) = EDGEtypeBANKerod(:,nPQ_int(k),mPQ_int(k))
!
!         EDGElenBANK(:,nPQ_ext(k),mPQ_ext(k)) = EDGElenBANK(:,nPH_int(k),mPH_int(k))  !removed,not modificated between extrapPERIODICedgeBANK1 and extrapPERIODICedgeBANK2
!         EDGElenBANK(:,nPH_ext(k),mPH_ext(k)) = EDGElenBANK(:,nPQ_int(k),mPQ_int(k))  !removed,not modificated between extrapPERIODICedgeBANK1 and extrapPERIODICedgeBANK2
!
!         EDGElenWET(nPQ_ext(k),mPQ_ext(k),:) = EDGElenWET(nPH_int(k),mPH_int(k),:)  !removed,not modificated between extrapPERIODICedgeBANK1 and extrapPERIODICedgeBANK2
!         EDGElenWET(nPH_ext(k),mPH_ext(k),:) = EDGElenWET(nPQ_int(k),mPQ_int(k),:)  !removed,not modificated between extrapPERIODICedgeBANK1 and extrapPERIODICedgeBANK2
!
!         EDGExyBANK(nPQ_ext(k),mPQ_ext(k),:,:,:) = EDGExyBANK(nPH_int(k),mPH_int(k),:,:,:)  !removed,not modificated between extrapPERIODICedgeBANK1 and extrapPERIODICedgeBANK2
!         EDGExyBANK(nPH_ext(k),mPH_ext(k),:,:,:) = EDGExyBANK(nPQ_int(k),mPQ_int(k),:,:,:) !removed,not modificated between extrapPERIODICedgeBANK1 and extrapPERIODICedgeBANK2
!                  
         SHIFT = RESHAPE([LchanPERprojX,LchanPERprojX,LchanPERprojX,LchanPERprojX,LchanPERprojX,LchanPERprojX,LchanPERprojX,LchanPERprojX,&
                          LchanPERprojY,LchanPERprojY,LchanPERprojY,LchanPERprojY,LchanPERprojY,LchanPERprojY,LchanPERprojY,LchanPERprojY],[4,2,2]) 

         EDGExyBANKerod(nPQ_ext(k),mPQ_ext(k),:,:,:) = EDGExyBANKerod(nPH_int(k),mPH_int(k),:,:,:) - SHIFT 
         EDGExyBANKerod(nPH_ext(k),mPH_ext(k),:,:,:) = EDGExyBANKerod(nPQ_int(k),mPQ_int(k),:,:,:) + SHIFT
!
         !they are computed now,and they dont have to be the periodic one but they have to be zero in order to have no flux in sud 
       !  guu_cc(nPQ_ext(k),mPQ_ext(k)) = guu_cc(nPH_int(k),mPH_int(k))      
      !   gvv_cc(nPQ_ext(k),mPQ_ext(k)) = gvv_cc(nPH_int(k),mPH_int(k))   
       !  guu_cc(nPH_ext(k),mPH_ext(k)) = guu_cc(nPQ_int(k),mPQ_int(k))      
      !   gvv_cc(nPH_ext(k),mPH_ext(k)) = gvv_cc(nPQ_int(k),mPQ_int(k))   
         if (twoCELLSperiod) then !ONLY for circular, so no shift by LchanPERprojX  needed
            EDGEtypeBANKerod(:,nPQ_extext(k),mPQ_extext(k)) = EDGEtypeBANKerod(:,nPH_int(k),mPH_intint(k)) 
            EDGEtypeBANKerod(:,nPH_extext(k),mPH_extext(k)) = EDGEtypeBANKerod(:,nPQ_int(k),mPQ_intint(k))
!
            EDGExyBANKerod(nPQ_extext(k),mPQ_extext(k),:,:,:) = EDGExyBANKerod(nPH_intint(k),mPH_intint(k),:,:,:) 
            EDGExyBANKerod(nPH_extext(k),mPH_extext(k),:,:,:) = EDGExyBANKerod(nPQ_intint(k),mPQ_intint(k),:,:,:)
         endif
      enddo
!
RETURN
end subroutine extrapPERIODICedgeBANK2
