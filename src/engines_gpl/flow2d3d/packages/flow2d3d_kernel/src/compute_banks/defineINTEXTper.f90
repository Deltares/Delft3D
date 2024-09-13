subroutine defineINTEXTper(kcs,xcor,ycor,nlb,nub,mlb,mub,gdp)
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
!   Function:   compute internal and external location of periodic BC. Check if periodic
!               boundary is correctly located at kcs==2
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
    integer              , pointer :: nrPER
    integer              , pointer :: PERIODalongM
    logical              , pointer :: twoCELLSperiod
    integer, dimension(:), pointer :: mPH_ext
    integer, dimension(:), pointer :: nPH_ext
    integer, dimension(:), pointer :: mPQ_ext
    integer, dimension(:), pointer :: nPQ_ext
    integer, dimension(:), pointer :: mPH_int
    integer, dimension(:), pointer :: mPQ_int
    integer, dimension(:), pointer :: mPH_intint
    integer, dimension(:), pointer :: mPQ_intint
    integer, dimension(:), pointer :: nPH_int
    integer, dimension(:), pointer :: nPQ_int
    integer, dimension(:), pointer :: nPH_intint
    integer, dimension(:), pointer :: nPQ_intint
    integer, dimension(:), pointer :: mPH_extext
    integer, dimension(:), pointer :: mPQ_extext
    integer, dimension(:), pointer :: nPH_extext
    integer, dimension(:), pointer :: nPQ_extext
    integer              , pointer :: shiftHM
    integer              , pointer :: shiftHN
    integer              , pointer :: shiftQM
    integer              , pointer :: shiftQN
    logical              , pointer :: perHwaterRIGHT
    integer              , pointer :: distQHm
    integer              , pointer :: distQHn
    logical              , pointer :: perCIRC
    real(fp)             , pointer :: LchanPERprojX
    real(fp)             , pointer :: LchanPERprojY
    logical              , pointer :: curvMESH
    real(fp)             , pointer :: distanceBOUNDper
!
! global variables
!
  integer, dimension(nlb:nub,mlb:mub)            ,intent(in)  :: kcs
  real(fp), dimension(nlb:nub,mlb:mub)           ,intent(in)  :: xcor
  real(fp), dimension(nlb:nub,mlb:mub)           ,intent(in)  :: ycor
  integer, intent(in)        :: nlb
  integer, intent(in)        :: nub
  integer, intent(in)        :: mlb
  integer, intent(in)        :: mub
!
! local variables
!
  integer                    :: k 
 
 
!
! executable statements -------------------------------------------------------
!
    nrPER            => gdp%gdimbound%nrPER
    PERIODalongM     => gdp%gdimbound%PERIODalongM
    twoCELLSperiod   => gdp%gdimbound%twoCELLSperiod
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
    shiftHM          => gdp%gdimbound%shiftHM
    shiftHN          => gdp%gdimbound%shiftHN
    shiftQM          => gdp%gdimbound%shiftQM
    shiftQN          => gdp%gdimbound%shiftQN
    perHwaterRIGHT   => gdp%gdimbound%perHwaterRIGHT
    distQHm          => gdp%gdimbound%distQHm
    distQHn          => gdp%gdimbound%distQHn
    perCIRC          => gdp%gdimbound%perCIRC
    LchanPERprojX    => gdp%gdimbound%LchanPERprojX
    LchanPERprojY    => gdp%gdimbound%LchanPERprojY
    curvMESH         => gdp%gdimbound%curvMESH
    distanceBOUNDper => gdp%gdimbound%distanceBOUNDper
    if (PERIODalongM.eq.1) then   
       if (kcs(nPH_ext(1),mPH_ext(1)+1) == 1) then
          !
          ! It is true when the periodic H boundary has water on the right of the boundary (i.e. for larger m )
          !
          perHwaterRIGHT =.true.
          mPH_int   (0:nrPER+1) = mPH_ext(0:nrPER+1)+1
          mPQ_int   (0:nrPER+1) = mPQ_ext(0:nrPER+1)-1
          mPH_intint(0:nrPER+1) = mPH_ext(0:nrPER+1)+2
          mPQ_intint(0:nrPER+1) = mPQ_ext(0:nrPER+1)-2
          nPH_int   (0:nrPER+1) = nPH_ext(0:nrPER+1)
          nPQ_int   (0:nrPER+1) = nPQ_ext(0:nrPER+1) 
          nPH_intint(0:nrPER+1) = nPH_ext(0:nrPER+1)
          nPQ_intint(0:nrPER+1) = nPQ_ext(0:nrPER+1) 
          mPH_extext(0:nrPER+1) = mPH_ext(0:nrPER+1)-1
          mPQ_extext(0:nrPER+1) = mPQ_ext(0:nrPER+1)+1
          nPH_extext(0:nrPER+1) = nPH_ext(0:nrPER+1)
          nPQ_extext(0:nrPER+1) = nPQ_ext(0:nrPER+1) 
       else
          perHwaterRIGHT =.false.
          mPH_int(0:nrPER+1)    = mPH_ext(0:nrPER+1)-1
          mPQ_int(0:nrPER+1)    = mPQ_ext(0:nrPER+1)+1
          mPH_intint(0:nrPER+1) = mPH_ext(0:nrPER+1)-2
          mPQ_intint(0:nrPER+1) = mPQ_ext(0:nrPER+1)+2
          nPH_int(0:nrPER+1)    = nPH_ext(0:nrPER+1)
          nPQ_int(0:nrPER+1)    = nPQ_ext(0:nrPER+1) 
          nPH_intint(0:nrPER+1) = nPH_ext(0:nrPER+1)
          nPQ_intint(0:nrPER+1) = nPQ_ext(0:nrPER+1) 
          mPH_extext(0:nrPER+1) = mPH_ext(0:nrPER+1)+1
          mPQ_extext(0:nrPER+1) = mPQ_ext(0:nrPER+1)-1
          nPH_extext(0:nrPER+1) = nPH_ext(0:nrPER+1)
          nPQ_extext(0:nrPER+1) = nPQ_ext(0:nrPER+1)
       endif
    else 
       if (kcs(nPH_ext(1)+1,mPH_ext(1)) == 1) then
          perHwaterRIGHT =.true.
          mPH_int(0:nrPER+1)    = mPH_ext(0:nrPER+1)
          mPQ_int(0:nrPER+1)    = mPQ_ext(0:nrPER+1)
          mPH_intint(0:nrPER+1) = mPH_ext(0:nrPER+1)
          mPQ_intint(0:nrPER+1) = mPQ_ext(0:nrPER+1)
          nPH_int(0:nrPER+1)    = nPH_ext(0:nrPER+1)+1
          nPQ_int(0:nrPER+1)    = nPQ_ext(0:nrPER+1)-1
          nPH_intint(0:nrPER+1) = nPH_ext(0:nrPER+1)+2
          nPQ_intint(0:nrPER+1) = nPQ_ext(0:nrPER+1)-2
          mPH_extext(0:nrPER+1) = mPH_ext(0:nrPER+1) 
          mPQ_extext(0:nrPER+1) = mPQ_ext(0:nrPER+1) 
          nPH_extext(0:nrPER+1) = nPH_ext(0:nrPER+1)-1
          nPQ_extext(0:nrPER+1) = nPQ_ext(0:nrPER+1)+1
       else
          perHwaterRIGHT =.false.
          mPH_int(0:nrPER+1)    = mPH_ext(0:nrPER+1)
          mPQ_int(0:nrPER+1)    = mPQ_ext(0:nrPER+1)
          mPH_intint(0:nrPER+1) = mPH_ext(0:nrPER+1)
          mPQ_intint(0:nrPER+1) = mPQ_ext(0:nrPER+1)
          nPH_int(0:nrPER+1)    = nPH_ext(0:nrPER+1)-1
          nPQ_int(0:nrPER+1)    = nPQ_ext(0:nrPER+1)+1
          nPH_intint(0:nrPER+1) = nPH_ext(0:nrPER+1)-2
          nPQ_intint(0:nrPER+1) = nPQ_ext(0:nrPER+1)+2
          mPH_extext(0:nrPER+1) = mPH_ext(0:nrPER+1) 
          mPQ_extext(0:nrPER+1) = mPQ_ext(0:nrPER+1) 
          nPH_extext(0:nrPER+1) = nPH_ext(0:nrPER+1)+1
          nPQ_extext(0:nrPER+1) = nPQ_ext(0:nrPER+1)-1
       endif
    endif
    !
    ! Define shifts
    !
    if (PERIODalongM==1) then
       !
       ! Note: the same shift (shiftQM,shiftQN) is used for both right and left hand sides when prescribing something at Q bounadry. 
       ! The same for H boundary.
       !
       if (perHwaterRIGHT) then
          shiftHM = 0 !shift in m that I have to prescribe when I copy othogonal velocity (vel that is orth to bound) at H boundary
          shiftHN = 0
          shiftQM =-1 !shift in m that I have to prescribe when I copy othogonal velocity (vel that is orth to bound) at Q boundary
          shiftQN = 0
       else
          shiftHM =-1
          shiftHN = 0
          shiftQM = 0
          shiftQN = 0
       endif
    else
       if (perHwaterRIGHT) then
          shiftHM = 0
          shiftHN = 0
          shiftQM = 0
          shiftQN =-1
       else
          shiftHM = 0
          shiftHN =-1
          shiftQM = 0
          shiftQN = 0
       endif
    endif
    !
    ! Find shift needed for peridic ditribution of discharge
    !
    distQHm = mPQ_int(1) - mPH_int(1)
    distQHn = nPQ_int(1) - nPH_int(1)
    !
    ! Determine perCIRC, i.e. if periodice circular channel or periodic straight channel
    !
    if (.not.curvMESH) then ! if mesh is not curvilinear
       if (PERIODalongM==1) then
          if (mPH_ext(1).gt.mPQ_ext(1)) then
             ! perHright = .true. !H is on the right of Q
             if (perHwaterRIGHT) then
                perCIRC = .true. !it is a circular periodicity
                distanceBOUNDper = 0._fp ! the boundaries concides (not used)
                !LchanPERprojX = 0._fp
                !LchanPERprojY = 0._fp
             else
                perCIRC = .false.
                !distanceBOUNDper = sqrt( (xcor(nPH_ext(1),mPH_ext(1)-1)-xcor(nPQ_ext(1),mPQ_ext(1)))**2 + (ycor(nPH_ext(1),mPH_ext(1)-1)-ycor(nPQ_ext(1),mPQ_ext(1)))**2)  !only distQHm not distQHn, I stay in the m line I want distance between boundaries
                LchanPERprojX = xcor(nPH_ext(1),mPH_ext(1)-1)-xcor(nPQ_ext(1),mPQ_ext(1)) !it has a sign so BC works also if m increasing toward decreasing x
                LchanPERprojY = ycor(nPH_ext(1),mPH_ext(1)-1)-ycor(nPQ_ext(1),mPQ_ext(1)) !it has a sign so BC works also if m increasing toward decreasing x
                distanceBOUNDper = sqrt(LchanPERprojX**2+LchanPERprojY**2)
             endif
          else
             ! perHright = .false. !H is on the left of Q
             if (perHwaterRIGHT) then
                perCIRC = .false.
                LchanPERprojX = xcor(nPH_ext(1),mPH_ext(1))-xcor(nPQ_ext(1),mPQ_ext(1)-1)
                LchanPERprojY = ycor(nPH_ext(1),mPH_ext(1))-ycor(nPQ_ext(1),mPQ_ext(1)-1)
                !  distanceBOUNDper =  sqrt( (xcor(nPH_ext(1),mPH_ext(1))-xcor(nPQ_ext(1),mPQ_ext(1)-1))**2 ++ (ycor(nPH_int(1),mPH_int(1))-ycor(nPH_int(1),mPQ_ext(1)+distQHm))**2)  !only distQHm not distQHn, I stay in the m line I want distance between boundaries
                distanceBOUNDper = sqrt(LchanPERprojX**2+LchanPERprojY**2)
             else
                perCIRC = .true.
                !LchanPERprojX = 0._fp
                !LchanPERprojY = 0._fp
                distanceBOUNDper = 0._fp ! the boundaries concides (not used)
             endif
          endif
       else
          if (nPH_ext(1).gt.nPQ_ext(1)) then
             !  perHright = .true.
             if (perHwaterRIGHT) then
                perCIRC = .true.
                distanceBOUNDper = 0._fp
                ! LchanPERprojX = 0._fp
                ! LchanPERprojY = 0._fp
             else
                perCIRC = .false.
                !  distanceBOUNDper =  sqrt( (xcor(nPH_int(1),mPH_int(1))-xcor(nPH_int(1)+distQHn,mPQ_ext(1)))**2 + (ycor(nPH_int(1),mPH_int(1))-ycor(nPH_int(1)+distQHn,mPQ_ext(1)))**2)
                LchanPERprojX = xcor(nPH_ext(1)-1,mPH_ext(1))-xcor(nPQ_ext(1),mPQ_ext(1))
                LchanPERprojY = ycor(nPH_ext(1)-1,mPH_ext(1))-ycor(nPQ_ext(1),mPQ_ext(1))
                distanceBOUNDper = sqrt(LchanPERprojX**2+LchanPERprojY**2)
             endif
          else
             ! perHright = .false.
             if (perHwaterRIGHT) then
                perCIRC = .false.
                ! distanceBOUNDper =  sqrt( (xcor(nPH_int(1),mPH_int(1))-xcor(nPH_int(1)+distQHn,mPQ_ext(1)))**2 + (ycor(nPH_int(1),mPH_int(1))-ycor(nPH_int(1)+distQHn,mPQ_ext(1)))**2)  !only distQHn not distQHm, I stay in the n line I want distance between boundaries
                LchanPERprojX = xcor(nPH_ext(1),mPH_ext(1))-xcor(nPQ_ext(1)-1,mPQ_ext(1))
                LchanPERprojY = ycor(nPH_ext(1),mPH_ext(1))-ycor(nPQ_ext(1)-1,mPQ_ext(1))
                distanceBOUNDper = sqrt(LchanPERprojX**2+LchanPERprojY**2)
             else
                perCIRC = .true.
                distanceBOUNDper = 0._fp
                !  LchanPERprojX = 0._fp
                ! LchanPERprojY = 0._fp
             endif
          endif
       endif     
    else !if mesh curvilinear only circular periodic channels are admitted!
       write(*,*) 'Warning: since mesh is not regular (curvilinear?) only circular periodic channels are admitted'         
       perCIRC = .true.
       LchanPERprojX = 0._fp
       LchanPERprojY = 0._fp
    endif
    !
    ! Check location 
    !
    do k=1,nrPER 
       if ( (kcs(nPH_ext(k),mPH_ext(k)) /= 2) .or. (kcs(nPQ_ext(k),mPQ_ext(k)) /= 2) )then
          write(*,*)'Periodic BC have to be on a boundary (kcs=2)!!'
          call d3stop(1, gdp)
       endif
    enddo
    !
    ! Check twoCELLSperiod and straight (if I wanna do it I have to define also LchanPERprojX2 and LchanPERprojY2 
    ! for the second line of periodic cells, and use it by SHIFT2 in extrapPERIODICedgeBANK1 and extrapPERIODICedgeBANK2
    ! 
    if (.not.perCIRC.and.twoCELLSperiod) then
       write(*,*)'Periodic BC for straight channel needs twoCELLSperiod=.false.!!' 
       call d3stop(1, gdp)
    endif    
    !
    ! Cheak feasibility of twoCELLSperiod
    !
    if (twoCELLSperiod) then
       do k=1,nrPER 
          if ( (kcs(nPH_extext(k),mPH_extext(k)) /= 0) .or. (kcs(nPQ_extext(k),mPQ_extext(k)) /= 0) )then
             write(*,*)'Periodic BC with twoCELLSperiod=true needs to have 2 extra lines of cells available! '
             call d3stop(1, gdp)
          endif
       enddo
    endif
    !
    return
end subroutine defineINTEXTper
