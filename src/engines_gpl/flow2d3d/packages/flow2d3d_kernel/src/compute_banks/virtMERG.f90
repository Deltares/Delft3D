subroutine virtMERG(VAR,gsqs,s1,dps,cdryb,icx,icy,nmmax,nmlb,nmub,nst,lsedtotINI,lsedtotFIN,INIcycle,FINcycle,lundia,bedupd,&
                       bedchangemesscount,bedchangemessmax,ntstep,IcheckBED,nmaxddb,ddb,&
                       NMlistMERGED,Nmerged,dim_nmlist) 

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
!   Function:   it averages out VAR in the small cut and adjacent,in a way that 
!               they have the same value 
!
!  Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
  use precision
!
  implicit none
!
! global variables
!
    integer                                              , intent(in)    :: dim_nmlist
    integer                                              , intent(in)    :: IcheckBED
    integer                                              , intent(in)    :: bedchangemessmax
    integer                                              , intent(inout) :: bedchangemesscount
    integer                                              , intent(in)    :: ntstep
    integer                                              , intent(in)    :: lundia         ! Unit number of diagnosis file
    integer                                              , intent(in)    :: nst
    integer                                              , intent(in)    :: INIcycle
    integer                                              , intent(in)    :: FINcycle
    integer                                              , intent(in)    :: lsedtotINI
    integer                                              , intent(in)    :: lsedtotFIN
    integer                                              , intent(in)    :: nmlb   
    integer                                              , intent(in)    :: nmub
    integer                                              , intent(in)    :: icx   
    integer                                              , intent(in)    :: icy
    integer                                              , intent(in)    :: nmmax
    integer                                              , intent(in)    :: ddb
    integer                                              , intent(in)    :: nmaxddb
    integer, dimension(nmlb:nmub)                        , intent(in)    :: Nmerged
    integer, dimension(dim_NMlist,nmlb:nmub)             , intent(in)    :: NMlistMERGED
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: gsqs
    real(prec), dimension(nmlb:nmub)                     , intent(in)    :: dps
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: s1
    real(fp), dimension(lsedtotINI:lsedtotFIN,nmlb:nmub) , intent(inout) :: VAR
    real(fp), dimension(FINcycle)                        , intent(in)    :: cdryb !FINcycle concides with sedtot. Unless cdryb is dummy and so FINcycle =1
    logical                                              , intent(in)    :: bedupd  
!
! local variables
!
    integer  :: nm,nmu,num,mm,nn
    integer  :: nmN
    integer  :: j 
    integer  :: L
    integer  :: m 
    integer  :: n
    integer  :: idir
    integer  :: nm4(1:4)
    integer  :: nm4c(1:4)
    integer  :: nmj
    integer  :: nmOK
    integer  :: nmEX
    real(fp) :: VARaver
    real(fp) :: dhmax
    real(fp) :: H1
    real(fp) :: H2
    real(fp) :: Atot
    real(fp) :: numer
    logical                                            :: positTEST
    logical                                            :: exceeded
    character(300) :: message
!
!
! executable statements -------------------------------------------------------
!
   do nm=1,nmmax
!   do idir = 1,2
!    
!         if (idir == 1) then
!            !
!            ! AT U POINT
!            !
!            positTEST = isMERGEDu(nm)==1
!            nmOK = nm + icx
!         else
!            !
!            ! AT V POINT
!            !
!            positTEST = isMERGEDv(nm)==1
!            nmOK = nm + icy
!         endif
!         if (positTEST) then
      if (Nmerged(nm)>1) then
         Atot = 0._fp
         DO N = 1,Nmerged(nm)  
            nmN = NMlistMERGED(N,nm)  
            Atot = Atot + gsqs(nmN)     !NOTE: HERE gsqs ALREADY INCLUDES gsqs
         ENDDO
         do L = INIcycle, FINcycle !IF bed elevation =lsedtot, if vert vel =kmax-1. Otherwise =1.
            !VAR is averaged out in ALL the Nmerged(nm) cells and in the cell nm itself
            numer = 0._fp
            DO N = 1,Nmerged(nm) ! 
               nmN = NMlistMERGED(N,nm)
               numer = numer + VAR(L,nmN)*gsqs(nmN)
            ENDDO
            VARaver = numer/Atot
            !CORRECT BIG CELL (N=1) and SMALL ADJACENT cut cells
            DO N = 1,Nmerged(nm) 
               nmN = NMlistMERGED(N,nm)
               VAR(L, nmN)   = VARaver
            ENDDO
            !
            if (IcheckBED) THEN
               !
               ! Warn if bottom changes are very large,
               ! depth change NOT LIMITED. Note: this is only for small cut cells (agsqs(nmj).lt.thresMERGE)
               ! note: testing here small or big merged cell is the same since they have the same dbodsdAV .
               !       The big is already tested before merging therefore with a different value of dbodsd,
               !       so the testing here is not a duplicate but I think its unlikely it can be satisfied if it was not
               !       before
               !
               dhmax = 0.05_fp
               exceeded = .FALSE.              
               DO N = 1,Nmerged(nm) 
                  nmN = NMlistMERGED(N,nm)
                  h2 = max(0.01_fp, s1(nmN) + real(dps(nmN),fp))
                  if (abs(VARaver) > dhmax*h2*cdryb(l) .and. bedupd) then
                     exceeded = .TRUE.
                     nmEX = nmN
                     IF (exceeded) then
                        bedchangemesscount = bedchangemesscount + 1
                        if (bedchangemesscount <= bedchangemessmax) then
                           call nm_to_n_and_m_noGDP(nmEX, nn, mm, nmaxddb,ddb)
                           write (lundia, '(a,f5.1,a,i0,a,i0,a,i0,a)') &
                              & '*** WARNING Bed change exceeds ' , dhmax*100, ' % of waterdepth after ', ntstep,  &
                                 & ' timesteps, location (m,n) = (', mm,',',nn,')'
                        endif
                     endif
                  endif
               ENDDO
            ENDIF
         ENDDO
      endif
   enddo
!
RETURN
end subroutine virtMERG
