subroutine COMPUTEmergingCARATT(kcs,kfs,agsqs,aguu,agvv,icx,icy,nmmax,nmlb,nmub,nst,lundia,&
                                virtualMERGEupd,typeVIRTmergeUPD,thresMERGE,NMlistMERGED,Nmerged,&
                                isMERGEDu,isMERGEDv,MERGEDwith,facMERGElink,dim_nmlist,gdp)
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
!   Function:   computes MERGING quantities if virtualMERGEupd=true. Method 3 is made in a way that a cell is either a donor or a receptor
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
    logical, dimension(:), pointer :: found
    logical              , pointer :: virtualLINK
    
!
! global variables
!
    integer                                              , intent(in)    :: dim_nmlist
    integer                                              , intent(in)    :: typeVIRTmergeUPD
    integer                                              , intent(in)    :: lundia         ! Unit number of diagnosis file
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: aguu
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: agvv
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: agsqs
    integer, dimension(nmlb:nmub)                        , intent(in)    :: kcs
    integer, dimension(nmlb:nmub)                        , intent(in)    :: kfs
    integer, dimension(nmlb:nmub)                        , intent(out)   :: isMERGEDu
    integer, dimension(nmlb:nmub)                        , intent(out)   :: isMERGEDv
    integer, dimension(nmlb:nmub)                        , intent(out)   :: Nmerged
    integer, dimension(nmlb:nmub)                        , intent(out)   :: MERGEDwith
    integer, dimension(dim_NMlist,nmlb:nmub)             , intent(out)   :: NMlistMERGED
    integer                                              , intent(in)    :: nst
    integer                                              , intent(in)    :: nmlb   
    integer                                              , intent(in)    :: nmub
    integer                                              , intent(in)    :: icx   
    integer                                              , intent(in)    :: icy
    integer                                              , intent(in)    :: nmmax
    real(fp)                                             , intent(in)    :: thresMERGE
    real(fp)                                             , intent(in)    :: facMERGElink
    logical                                              , intent(in)    :: virtualMERGEupd
!
! local variables
!
    integer                    :: nm,nmu,num
    integer                    :: j 
    integer                    :: N
    integer                    :: Nmn
    integer                    :: NmergedOLD
    integer  :: nm4(1:4)
    integer  :: nm8(1:8)
    integer  :: nm4c(1:4)
    integer  :: nmj
    integer  :: nmBIG
    integer  :: nmOK
    integer  :: cont
    integer  :: jOK
    integer  :: NtoGOprec
    integer  :: NtoGO
    integer  :: NMlist
    real(fp) :: aguv(1:4)
    real(fp) :: MAXedge
    real(fp) :: agsqs_tot
    real(fp) :: Atot
    real(fp) :: AtotOK
    logical  :: notDONE
    logical  :: closed
    !logical , dimension(nmlb:nmub)                     :: found 
    character(300) :: message
!
!
! executable statements -------------------------------------------------------
!
    found                     => gdp%gdimbound%Lwrka1
    virtualLINK               => gdp%gdimbound%virtualLINK
     ! if (virtualLINK) THEN
     !    facMERGElink_loc = facMERGElink
     ! else
     !    facMERGElink_loc = 1
     ! endif
      !banks can move, reinitialize isMERGED
      isMERGEDu(1:nmmax) = 0 
      isMERGEDv(1:nmmax) = 0
      Nmerged(:) = 1
      NMlistMERGED(1,1:nmmax) = [1:nmmax] !the first element is the cell itself
      select case(typeVIRTmergeUPD)
      CASE(-1)!search on 8 cells sharing a vertex !OBSOLETE
         nm8(1) = -icx-icy !lower left
         nm8(2) = -icx     !left
         nm8(3) = -icx+icy !upper left
         nm8(4) =     -icy !lower 
         nm8(5) =     +icy !upper 
         nm8(6) = +icx-icy !lower right
         nm8(7) = +icx     !right
         nm8(8) = +icx+icy !upper right
         found(nmlb:nmub) =.false.
         do nm=1,nmmax
            nm8(1:8) = nm8(1:8) + 1
            if (kcs(nm)*kfs(nm) == 1.and. agsqs(nm).lt.thresMERGE) then
               nmOK = -999
               do j=1,8 !search on 3*3 stencil
	              nmj=nm8(j)
                  if (kcs(nmj)*kfs(nmj) == 1.and. comparereal(agsqs(nmj),0._fp).gt.0 .and. .not.found(nmj)) then !agsqs(nmj).gt.thresMERGE
                     found(nmj) =.true.
                     nmOK = nmj
                     exit
                  endif
               enddo
               if (nmOK.eq.-999) then
                  write(*,*) 'virtualMERGEupd=true but no adjacent is found for cell nm=',nm
                  call d3stop(1, gdp)
               endif
            endif
         enddo
      CASE(4,5)!4: search on 4 cells sharing a EDGE  5:search on 4 cells sharing a EDGE,if not found search on corner cells
         nm4(1)=     -icy !lower 
         nm4(2)= +icx     !right
         nm4(3)=     +icy !upper
         nm4(4)= -icx     !left
         found(nmlb:nmub) =.false.
         Nmerged(:) = 1
         do nm=1,nmmax
            nm4(1:4) = nm4(1:4) + 1
            if (kcs(nm)*kfs(nm) == 1.and. agsqs(nm).lt.thresMERGE) then
               nmOK = -999
               do j=1,4 !search on 4 cells sharing a edge
                  nmj=nm4(j)
                  if (kcs(nmj)*kfs(nmj) == 1.and. agsqs(nmj).gt.thresMERGE .and. .not.found(nmj)) then
                     found(nmj) =.true.
                     nmOK = nmj
                     if (j.eq.1) then
                        isMERGEDv(nm-icy) = 1
                     elseif (j.eq.2) then
                        isMERGEDu(nm) = 1
                     elseif (j.eq.3) then
                        isMERGEDv(nm) = 1
                     elseif (j.eq.4) then
                        isMERGEDu(nm-icx) = 1
                     endif
                     Nmerged(nmOK) = Nmerged(nmOK)+1  
                     IF (Nmerged(nmOK)>5) THEN !maximum 4 small adjacent sharing an edge to merge
                        write(*,*) 'There is a bug: Nmerged cannot be larger than 4!'
                        call d3stop(1, gdp)
                     ENDIF
                     NMlistMERGED(Nmerged(nmOK),nmOK) = nm
                     exit
                  endif
               enddo

               if (nmOK.eq.-999.and.typeVIRTmergeUPD==3) then ! none is found serch on the 4 corner cells sharing vertex but no edge
                  nm4c(1) = nm-icx-icy !lower left
                  nm4c(2) = nm-icx+icy !upper left
                  nm4c(3) = nm+icx-icy !lower right
                  nm4c(4) = nm+icx+icy !upper right
                  do j=1,4 !search on 4 cells sharing a vertex
                     nmj=nm4c(j)
                     if (kcs(nmj)*kfs(nmj) == 1.and. agsqs(nmj).gt.thresMERGE.and. .not.found(nmj)) then
                        found(nmj) =.true.
                        nmOK = nmj
                        write(*,*)  'warning virtualMERGE =3 but only corner adjacent is found for cell nm=',nm
                        write(message,'(a,i20)')  'warning virtualMERGE =3 but only corner adjacent is found for cell nm=',nm
                        call prterr(lundia, 'U190', trim(message))
                        !se questo accade, provare a vedere se e` possible avere merging di piu` celle adiacenti
                        !col lato,senza usare quelle nel corner.se cosi` e`, il baricentro del merged poligono
                        !deve essere ricompiuto e anche la media in virtMERGbed
                        !pause
                        !in this case isMERGEDv and isMERGEDu are not changed so the slope is never zero on the edge
                        exit
                     endif
                  enddo
               endif
               if (nmOK.eq.-999) then ! none is found in the 3*3 stencil
                  write(*,*) 'virtualMERGEupd=true but no adjacent is found for cell nm=',nm
                  call d3stop(1, gdp)
               endif
            endif
         enddo   
      CASE(2)! like case(2), but found is removed so multiple merging of a cell is allowed ( one big cell with agsqs>0.5 can be averaged with multiple cell with agsqs<0.5
         nm4(1)=     -icy !lower 
         nm4(2)= +icx     !right
         nm4(3)=     +icy !upper
         nm4(4)= -icx     !left
         Nmerged(:) = 1
         do nm=1,nmmax
            nm4(1:4) = nm4(1:4) + 1
            if (kcs(nm)*kfs(nm) == 1 .and. comparereal(agsqs(nm),0._fp).gt.0  .and. agsqs(nm).lt.thresMERGE) then
               nmOK = -999
               do j=1,4 !search on 4 cells sharing a edge
	              nmj=nm4(j)
                  if (kcs(nmj)*kfs(nmj) == 1.and. comparereal(agsqs(nmj),0._fp).gt.0 ) then !agsqs(nmj).gt.thresMERGE. It has to be  agsqs(nmj).gt.0 otherwise for some sharp bends it does not find a receptor cell. But note that in this case a donor can be also a receiver and merging is not purely geometrical, is more of a smoothing.
                     nmOK = nmj
                     if (j.eq.1) then
                        isMERGEDv(nm-icy) = 1
                     elseif (j.eq.2) then
                        isMERGEDu(nm) = 1
                     elseif (j.eq.3) then
                        isMERGEDv(nm) = 1
                     elseif (j.eq.4) then
                        isMERGEDu(nm-icx) = 1
                     endif
                     Nmerged(nmOK) = Nmerged(nmOK)+1  
                     IF (Nmerged(nmOK)>5) THEN !maximum 4 small adjacent sharing an edge to merge (so the cell itself + the 4 adjecent=5
                        write(*,*) 'There is a bug: Nmerged cannot be larger than 4!'
                        call d3stop(1, gdp)
                     ENDIF
                     NMlistMERGED(Nmerged(nmOK),nmOK) = nm
                     exit
                  endif
               enddo
               if (nmOK.eq.-999) then 
                  write(*,*) 'virtualMERGEupd=true but no adjacent is found for cell nm=',nm
                  call d3stop(1, gdp)
               endif                
            endif
         enddo         
      CASE(33)! like case(2), but found is removed so multiple merging of a cell is allowed ( one big cell with agsqs>0.5 can be averaged with multiple cell with agsqs<0.5
         !added merging with intermediate cells having (comparereal(agsqs(nm),thresMERGE).ge.0 .and. comparereal(agsqs(nm),thresMERGE*facMERGElink).lt.0 )
         if (nmmax.gt.999999990) then
            write(*,*) 'too many cells. Define MERGEDwith as integer*8 and increase -999999990/-999999999 to -999999999999990/999999999999999 ,since -999999990 is used together with negative cell numbers'
            call d3stop(1, gdp)
         endif
         if (thresMERGE*facMERGElink>1._fp) then
            write(*,*) 'Error: thresMERGE*facMERGElink>1'
            call d3stop(1, gdp)
         endif
         nm4(1)=     -icy !lower 
         nm4(2)= +icx     !right
         nm4(3)=     +icy !upper
         nm4(4)= -icx     !left
         MERGEDwith(nmlb:nmub) = -999999999  
         Nmerged(:) = 1
         do nm=1,nmmax
            nm4(1:4) = nm4(1:4) + 1
            if (kcs(nm)*kfs(nm) == 1 .and. comparereal(agsqs(nm),0._fp).gt.0  .and. comparereal(agsqs(nm),thresMERGE*facMERGElink).lt.0 ) then
               nmOK = -999
               aguv(1) = agvv(nm - icy)
               aguv(2) = aguu(nm)
               aguv(3) = agvv(nm)
               aguv(4) = aguu(nm - icx)
               MAXedge = -999._fp
               closed = .true.
               do j=1,4 !search on 4 cells sharing a edge
	              nmj=nm4(j)
         !        if (comparereal(aguv(j),0._fp).gt.0) then !I need to check if the edge is open otherwise I could merge it with the other side of a narrow land dividing 2 channels
         !  !   however, this gives problem at the boundary in case of a small cut cell open only on the boundary edge. So in the way its implemented now the small cell at the boundary will be merged even if aguv=0 since thats the maximum i have and its not closed (its open on kcs=2)
         !           closed = .false. !it is open at least on one side
         !        endif
                  if (kcs(nmj)*kfs(nmj) >= 1) then
                     if (comparereal(aguv(j),0._fp).gt.0)  closed = .false. !for both  kcs==1 and 2 if the adjacent is active (kcs*kfs>=1 and active edge) the cell is not close!  i dont think I need to check for kcs==1 or 2, or can it be 3?
                     if (comparereal(agsqs(nmj),thresMERGE*facMERGElink).ge.0) then ! I exclude merging with boundary. It might have long merged stripe of small cut for banks almost parallel to boundary
                        if (kcs(nmj)==1) then   !I dont wanna merge it with kcs==2
                           if (aguv(j)>MAXedge) then
                              jOK = j
                              MAXedge = aguv(j)
                           endif
                        endif   
                     endif                  
                  endif
             !     endif
               enddo
               if (MAXedge>-1.) then !otherwise it is a isolated non-bank small cell surrounded by bank edges. nothing happens
                  nmOK = nm4(jOK)
                  Nmerged(nmOK) = Nmerged(nmOK)+1  
                  MERGEDwith(nm) = nmOK
                  if (comparereal(agsqs(nm),thresMERGE).ge.0 .and. comparereal(agsqs(nm),thresMERGE*facMERGElink).lt.0 ) then
                     MERGEDwith(nm) = - nmOK 
                  endif
                  NMlistMERGED(Nmerged(nmOK),nmOK) = nm
                  j=jOK
                  if (j.eq.1) then
                     isMERGEDv(nm-icy) = 1
                  elseif (j.eq.2) then
                     isMERGEDu(nm) = 1
                  elseif (j.eq.3) then
                     isMERGEDv(nm) = 1
                  elseif (j.eq.4) then
                     isMERGEDu(nm-icx) = 1
                  endif
               elseif(closed) then !  it is a isolated non-bank small cell surrounded by bank edges. nothing happens
                  MERGEDwith(nm) = -999999990   
               endif
               if (nmOK.eq.-999) then 
                  !write(*,*) 'virtualMERGEupd=true but no adjacent is found for cell nm=',nm
                  !call d3stop(1, gdp)
                  !found(nm)=.false.
               endif  
            else 
               MERGEDwith(nm) = -999999990       
            endif
         enddo
         !here:!-999999990: not needed to be merged  -999999999: not merged but needeed to be merged    
         !
         ! for the cells that have not a "big" adjacent cell, merge them with the same big cell that is used for the small adjacent.
         ! And do this iteratively, until all donor cells have a receptor cell
         !
         notDONE=.TRUE.
         cont = 0 
         NtoGO = 0
         do while(notDONE)
            notDONE = .false.
            cont = cont + 1
            nm4(1)=     -icy !lower 
            nm4(2)= +icx     !right
            nm4(3)=     +icy !upper
            nm4(4)= -icx     !left
            NtoGOprec = NtoGO
            NtoGO = 0
            do nm=1,nmmax
               nm4(1:4) = nm4(1:4) + 1
               if (kcs(nm)*kfs(nm) == 1 .and. MERGEDwith(nm)==-999999999) then
                  NtoGO = NtoGO + 1
                  nmOK = -999
                  aguv(1) = agvv(nm - icy)
                  aguv(2) = aguu(nm)
                  aguv(3) = agvv(nm)
                  aguv(4) = aguu(nm - icx)
                  MAXedge = -999._fp
                  do j=1,4 !search on 4 cells sharing a edge for a cell that has been merged.
	                 nmj=nm4(j)
                     if (kcs(nmj)*kfs(nmj) == 1.and. MERGEDwith(nmj)>-999999990) then !if -999999990 I dont use it since it is not merged. the edge could be closed and in that case I dont wanna merge it with a cell having -999999999
                        if (aguv(j)>MAXedge) then
                           jOK = j
                           MAXedge = aguv(j)
                        endif
                     endif
                  enddo
                  if (MAXedge>-1.) then
                     MERGEDwith(jOK) = abs(MERGEDwith(jOK))
                     NMbig = MERGEDwith(jOK)
                     MERGEDwith(nm) = NMbig
                     Nmerged(NMbig) = Nmerged(NMbig)+1  
                     IF (Nmerged(NMbig)>20) THEN !maximum 1+4+15 small adjacent sharing an edge to merge (so the cell itself + the 4 adjecent=5+15 for multiple merging of narrow stripes
                        write(*,*) 'Error: Nmerged cannot be larger than 20!'
                        call d3stop(1, gdp)
                     ENDIF
                     NMlistMERGED(Nmerged(NMbig),NMbig) = nm
                  else 
                     write(*,*) 'virtualMERGEupd=true but no intermediate adjacent is found for cell nm=',nm
                     call d3stop(1, gdp)
                  endif
                  if (nmOK == -999) then
                     notDONE = .true.
                  endif
               endif
            enddo
           ! if(NtoGOprec = NtoGO) then
           !    write(515151,'(a,I0,I0,a)') 'Deadlock in COMPUTEmergingCARATT,NM,NST =',NM,NST,', continue anyway (it might be an isolated cutcell surrounded by dry cells'
           !    notDONE = .false.
           ! endif
            if (cont>1000000) then
               write(*,*) 'Deadlock in COMPUTEmergingCARATT,NM,NST =',NM,NST
               write(*,*) 'Note: s1 might be NaN and cause kfs=0'
               call d3stop(1, gdp)
            endif
         end do
!
!       if facMERGElink>1 I have to exclude cells if agsqs(nm) between thresMERGE and thresMERGE*facMERGElink that are not intermediate cells
!
        do nm=1,nmmax
           if (kcs(nm)*kfs(nm) == 1) THEN
              if (MERGEDwith(nm)>-999999990.and. MERGEDwith(nm)<0) then
                 !MERGEDwith(nm) stays negative
                 nmOK = abs(MERGEDwith(nm))  
                 MERGEDwith(nm) = -999999990 !nmOK
                 Nmerged(nmOK) = Nmerged(nmOK)-1  !REMOVE IT FROM RECEIVING CELL SINCE ITS NOT MERGED 
                 isMERGEDv(nm-icy) = 0
                 isMERGEDu(nm) = 0
                 isMERGEDv(nm) = 0
                 isMERGEDu(nm-icx) = 0
              endif
           endif
        enddo
!
!        check for number of cell merged (if virtuallink with min it has to have enough area to provide the minimum value to small cells)
!
         if (facMERGElink>1._fp) then
            do nm=1,nmmax
               if (Nmerged(nm)>1) then
                  agsqs_tot = 0._fp
                  do N=1,Nmerged(nm)
                     nmN = NMlistMERGED(N,nm)  
                     agsqs_tot = agsqs_tot + agsqs(nmN)       
                  enddo           
                  if (real(Nmerged(nm),fp)*thresMERGE>agsqs_tot) then
                     write(*,*) 'Decrease thresMERGE since more then Nmerged=' ,int(Nmerged(nm)),'  cells are merged and there is not enough donor area',Nmerged(nm),thresMERGE,facMERGElink,agsqs(nm),nm
                     call d3stop(1, gdp)
                  endif
               endif
            enddo
         endif
!
      CASE(345) ! without facMERGElink, I merge until Nmerged*thresMERGE*Area_i/Atot>1, basically the sum of areas Atot is larger of the sum of Nmerged*thresMERGE*Area_i, so I have enough area
                ! to set all the small cells to thresMERGE ........NON VA BENE
                !
                ! DA RIFARE COSI': OGNI CELLA PICCOLA DEVE MERGERSI CON LA ADIACENTE PIU'GRANDE FINCHE' L'AREA TOTALE E 'MAGGIORE DI QUELLA MINIMA
                ! PER LO SPREADING PER virtual link=true, o mi fermo nel caso virtMERGE. Usare subroutine recoursive per
                ! cercare adiacente nel caso del virtual link.
                ! dopo fare un ciclo che parta da celle con Nmerge>1 (sono receptors per almeno una cella) e mergedwith<1 (cioe'che non sono donate a altre celle)
                ! e successivamente raggruppare tutte le celle. nel ciclo mettere un check che cella non sia gia'stata inserita nel grupps (vedi caso dello
                ! spigolo)
                
!
         MERGEDwith(nmlb:nmub) = -99999  
         NMlistMERGED(2:10,:)= 0 ! not needed only for debugging purpose
        !here:!-99999: not initialized  -99999999: needeed to be merged   0:no need to be merged   
         Nmerged(:) = 1
         notDONE=.TRUE.
         cont = 0 
         do while(notDONE)  !should be done at most in 2 cycles (in case small cell has not an adjacent cell big enough

            notDONE = .false.
            cont = cont + 1     
            nm4(1)= nmmax+1    -icy !lower
            nm4(2)= nmmax+1+icx     !right
            nm4(3)= nmmax+1    +icy !upper
            nm4(4)= nmmax+1-icx     !left           
            do nm=nmmax,1,-1
               nm4(1:4) = nm4(1:4) - 1
               if (kcs(nm)*kfs(nm) == 1 .and. ((comparereal(agsqs(nm),0._fp).gt.0  .and. comparereal(agsqs(nm),thresMERGE).lt.0.and.MERGEDwith(nm)==-99999) .or. MERGEDwith(nm)==-99999999)  ) then !i enter here if small cell needed to be merged (MERGEDwith not defiend yet) or only virtual link) a normal cell needed to be merged cause the previous cycle it was merged with a small cell but it didnt have the area to provide
                  nmOK = -999
                  aguv(1) = agvv(nm - icy)
                  aguv(2) = aguu(nm)
                  aguv(3) = agvv(nm)
                  aguv(4) = aguu(nm - icx)
                  MAXedge = -999._fp
                  closed = .true.                  
                  do j=1,4 !search on 4 cells sharing a edge
	                  nmj=nm4(j)
                     Atot = agsqs(nmj) !at each edge i recompute Atotx
                     do N=1,Nmerged(nm)
                        Atot = Atot + agsqs(nm) !i suppose almost uniform mesh, I use agsqs(nm) instead of agsqs(nmj)*gsqs(nm)
                     enddo                       
            !        if (comparereal(aguv(j),0._fp).gt.0) then !I need to check if the edge is open otherwise I could merge it with the other side of a narrow land dividing 2 channels
            !  !   however, this gives problem at the boundary in case of a small cut cell open only on the boundary edge. So in the way its implemented now the small cell at the boundary will be merged even if aguv=0 since thats the maximum i have and its not closed (its open on kcs=2)
            !           closed = .false. !it is open at least on one side
            !        endif
                     if (kcs(nmj)*kfs(nmj) >= 1) then
                        if (comparereal(aguv(j),0._fp).gt.0)  closed = .false. !for both  kcs==1 and 2 if the adjacent is active (kcs*kfs>=1 and active edge) the cell is not close!  i dont think I need to check for kcs==1 or 2, or can it be 3?
                        if (kcs(nmj)==1) then   !I dont wanna merge it with kcs==2 ! I exclude merging with boundary. It might have long merged stripe of small cut for banks almost parallel to boundary
                           if (agsqs(nmj)>MAXedge) then
                              jOK = j
                              MAXedge = agsqs(nmj)       
                              AtotOK = Atot
                           endif   
                        endif       
                     endif
                !     endif
                  enddo
                  if (MAXedge>-1.) then !otherwise it is a isolated non-bank small cell surrounded by bank edges. nothing happens
                     nmOK = nm4(jOK)
                     NmergedOLD = Nmerged(nmOK)
                     Nmerged(nmOK) = Nmerged(nmOK)+Nmerged(nm) !Nmerged(nm)  is generally 1, unless the cell was already merged
                     MERGEDwith(nm) = nmOK      
                  !   if (comparereal(agsqs(nm),thresMERGE).ge.0) then !add it to list only if is not a small cell (otherwise it is already included)
                  !      NMlistMERGED(Nmerged(nmOK),nmOK) = nm      
                  !   endif
                     DO N=NmergedOLD+1,Nmerged(nmOK)
                        NMlistMERGED(N,nmOK) = NMlistMERGED(N-NmergedOLD,nm)  !copy from the previous intermediate size celll                              
                     ENDDO                     
                     if ( (comparereal(Nmerged(nmOK)*thresMERGE,AtotOK).ge.0.and.VIRTUALlink).or.comparereal(agsqs(nmOK),thresMERGE).lt.0) then
                           MERGEDwith(nmOK) = -99999999 !needed to be merged, it does not have enough area to provide
                           notDONE = .true.
                     else
                        if (Nmerged(nm)>1) then ! reset Nmerged(nm) and set MERGEDwith for all  the small cell in the tree to nmOK
                          ! MERGEDwith(nmOK) = -99999 !no need to be merged
                           DO N=2,Nmerged(nm)                          
                              NMlist = NMlistMERGED(N,nm)
                              MERGEDwith(NMlist) = nmOK !they are all merged with nmOK
                              NMlistMERGED(N,nm) = -999 !non needed by nicer. reset  NMlistMERGED for the previous intermediate-size cell
                           ENDDO
                           Nmerged(nm) = 1  !reset  Nmerged for the previous intermediate-size cell
                        endif
                     endif           
                     j=jOK
                     if (j.eq.1) then
                        isMERGEDv(nm-icy) = 1
                     elseif (j.eq.2) then
                        isMERGEDu(nm) = 1
                     elseif (j.eq.3) then
                        isMERGEDv(nm) = 1
                     elseif (j.eq.4) then
                        isMERGEDu(nm-icx) = 1
                     endif
                  elseif(closed) then !  it is a isolated non-bank small cell surrounded by bank edges. nothing happens
                     MERGEDwith(nm) = 0
                  endif
                  if (nmOK.eq.-999) then 
                   !  write(*,*) 'virtualMERGEupd=true but no adjacent is found for cell nm=',nm
                   !  pause
                   !  stop
                   !  found(nm)=.false.
                  endif  
            !   else 
            !       MERGEDwith(nm) = 0 
               endif
            enddo
            if (cont>1000000) then
               write(*,*) 'Deadlock in COMPUTEmergingCARATT,NM,NST =',NM,NST
               write(*,*) 'Note: s1 might be NaN and cause kfs=0'
               !pause
               stop
            endif            
         enddo
!
          

      CASE(3) !old case before allowing multiple merge with factor
!
         if (thresMERGE*facMERGElink>1._fp) then
            write(*,*) 'Error: thresMERGE*facMERGElink>1'
            call d3stop(1, gdp)
         endif
!
         nm4(1)=     -icy !lower 
         nm4(2)= +icx     !right
         nm4(3)=     +icy !upper
         nm4(4)= -icx     !left
         MERGEDwith(nmlb:nmub) = -99999999  
         Nmerged(:) = 1
         do nm=1,nmmax
            nm4(1:4) = nm4(1:4) + 1
            if (kcs(nm)*kfs(nm) == 1 .and. comparereal(agsqs(nm),0._fp).gt.0  .and. comparereal(agsqs(nm),thresMERGE).lt.0 ) then
               nmOK = -999
               aguv(1) = agvv(nm - icy)
               aguv(2) = aguu(nm)
               aguv(3) = agvv(nm)
               aguv(4) = aguu(nm - icx)
               MAXedge = -999._fp
               closed = .true.
               do j=1,4 !search on 4 cells sharing a edge
	              nmj=nm4(j)
         !        if (comparereal(aguv(j),0._fp).gt.0) then !I need to check if the edge is open otherwise I could merge it with the other side of a narrow land dividing 2 channels
         !  !   however, this gives problem at the boundary in case of a small cut cell open only on the boundary edge. So in the way its implemented now the small cell at the boundary will be merged even if aguv=0 since thats the maximum i have and its not closed (its open on kcs=2)
         !           closed = .false. !it is open at least on one side
         !        endif
                  if (kcs(nmj)*kfs(nmj) >= 1) then
                     if (comparereal(aguv(j),0._fp).gt.0)  closed = .false. !for both  kcs==1 and 2 if the adjacent is active (kcs*kfs>=1 and active edge) the cell is not close!  i dont think I need to check for kcs==1 or 2, or can it be 3?
                     if (comparereal(agsqs(nmj),thresMERGE*facMERGElink).ge.0) then ! I exclude merging with boundary. It might have long merged stripe of small cut for banks almost parallel to boundary
                        if (kcs(nmj)==1) then   !I dont wanna merge it with kcs==2
                           if (aguv(j)>MAXedge) then
                              jOK = j
                              MAXedge = aguv(j)
                           endif
                        endif   
                     endif                  
                  endif
             !     endif
               enddo
               if (MAXedge>-1.) then !otherwise it is a isolated non-bank small cell surrounded by bank edges. nothing happens
                  nmOK = nm4(jOK)
                  Nmerged(nmOK) = Nmerged(nmOK)+1  
                  MERGEDwith(nm) = nmOK
                  NMlistMERGED(Nmerged(nmOK),nmOK) = nm
                  j=jOK
                  if (j.eq.1) then
                     isMERGEDv(nm-icy) = 1
                  elseif (j.eq.2) then
                     isMERGEDu(nm) = 1
                  elseif (j.eq.3) then
                     isMERGEDv(nm) = 1
                  elseif (j.eq.4) then
                     isMERGEDu(nm-icx) = 1
                  endif
               elseif(closed) then !  it is a isolated non-bank small cell surrounded by bank edges. nothing happens
                  MERGEDwith(nm) = -99999   
               endif
               if (nmOK.eq.-999) then 
                  !write(*,*) 'virtualMERGEupd=true but no adjacent is found for cell nm=',nm
                  !call d3stop(1, gdp)
                  !found(nm)=.false.
               endif  
            else 
               MERGEDwith(nm) = -99999       
            endif
         enddo
         !here:!-99999: not needed to be merged  -99999999: not merged but needeed to be merged    
         !
         ! for the cells that have not a "big" adjacent cell, merge them with the same big cell that is used for the small adjacent.
         ! And do this iteratively, until all donor cells have a receptor cell
         !
         notDONE=.TRUE.
         cont = 0 
         NtoGO = 0
         do while(notDONE)
            notDONE = .false.
            cont = cont + 1
            nm4(1)=     -icy !lower 
            nm4(2)= +icx     !right
            nm4(3)=     +icy !upper
            nm4(4)= -icx     !left
            NtoGOprec = NtoGO
            NtoGO = 0
            do nm=1,nmmax
               nm4(1:4) = nm4(1:4) + 1
               if (kcs(nm)*kfs(nm) == 1 .and. MERGEDwith(nm)==-99999999) then
                  NtoGO = NtoGO + 1
                  nmOK = -999
                  do j=1,4 !search on 4 cells sharing a edge for a cell that has been merged.
	                 nmj=nm4(j)
                     if (kcs(nmj)*kfs(nmj) == 1.and. MERGEDwith(nmj)>-99999) then !if -99999 I dont use it since it is not merged. the edge could be closed and in that case I dont wanna merge it with a cell having -99999999
                        NMbig = MERGEDwith(nmj)
                        MERGEDwith(nm) = NMbig
                        Nmerged(NMbig) = Nmerged(NMbig)+1  
                        IF (Nmerged(NMbig)>20) THEN !maximum 1+4+15 small adjacent sharing an edge to merge (so the cell itself + the 4 adjecent=5+15 for multiple merging of narrow stripes
                           write(*,*) 'Error: Nmerged cannot be larger than 20!'
                           call d3stop(1, gdp)
                        ENDIF
                        NMlistMERGED(Nmerged(NMbig),NMbig) = nm
                        exit
                     endif
                  enddo
                  if (nmOK == -999) then
                     notDONE = .true.
                  endif
               endif
            enddo
           ! if(NtoGOprec = NtoGO) then
           !    write(515151,'(a,I0,I0,a)') 'Deadlock in COMPUTEmergingCARATT,NM,NST =',NM,NST,', continue anyway (it might be an isolated cutcell surrounded by dry cells'
           !    notDONE = .false.
           ! endif
            if (cont>1000000) then
               write(*,*) 'Deadlock in COMPUTEmergingCARATT,NM,NST =',NM,NST
               write(*,*) 'Note: s1 might be NaN and cause kfs=0'
               call d3stop(1, gdp)
            endif
         end do
!
!        check for number of cell merged (if virtuallink with min it has to have enough area to provide the minimum value to small cells)
!
      !  if (facMERGElink>1._fp) then
      !     do nm=1,nmmax
      !        if (Nmerged(nm)>1) then
      !           if (real(Nmerged(nm),fp)*thresMERGE>1._fp) then 
      !              write(*,*) 'Decrease thresMERGE since Nmerged*thresMERGE>1'
      !              call d3stop(1, gdp)
      !           elseif (real(Nmerged(nm),fp)*thresMERGE>agsqs(nm)) then
      !              write(*,*) 'Decrease thresMERGE since more then Nmerged=' ,int(Nmerged(nm)),'  cells are merged and there is not enough donor area',Nmerged(nm),thresMERGE,facMERGElink,agsqs(nm),nm
      !              call d3stop(1, gdp)
      !           endif
      !        endif
      !     enddo
      !  endif
         if (facMERGElink>1._fp) then
            do nm=1,nmmax
               if (Nmerged(nm)>1) then
                  agsqs_tot = 0._fp
                  do N=1,Nmerged(nm)
                     nmN = NMlistMERGED(N,nm)  
                     agsqs_tot = agsqs_tot + agsqs(nmN)       
                  enddo           
                  if (real(Nmerged(nm),fp)*thresMERGE>agsqs_tot) then
                     write(*,*) 'Decrease thresMERGE since more then Nmerged=' ,int(Nmerged(nm)),'  cells are merged and there is not enough donor area',Nmerged(nm),thresMERGE,facMERGElink,agsqs(nm),nm
                     call d3stop(1, gdp)
                  endif
               endif
            enddo
         endif
      CASE(-2)! like case(3), but EXIT IS COMMENTED SO multiple merging of a cell is allowed ( one small cell with agsqs<0.5 is averaged with multiple cells, all the one sharing a edge and with agsqs>0.5
         !OBSOLATE: DOES NOT REALLY MERGE IN A BIGGER POLYGON IT KEEPS AVERAGING, IT IS UNSTABLE
         nm4(1)=     -icy !lower 
         nm4(2)= +icx     !right
         nm4(3)=     +icy !upper
         nm4(4)= -icx     !left
         found(nmlb:nmub) =.false.
         Nmerged(:) = 1
         do nm=1,nmmax
            nm4(1:4) = nm4(1:4) + 1
            if (kcs(nm)*kfs(nm) == 1.and. agsqs(nm).lt.thresMERGE) then
               nmOK = -999
               do j=1,4 !search on 4 cells sharing a edge
	              nmj=nm4(j)
                  if (kcs(nmj)*kfs(nmj) == 1.and. agsqs(nmj).gt.thresMERGE ) then
                     nmOK = nmj
                     if (j.eq.1) then
                        isMERGEDv(nm-icy) = 1
                     elseif (j.eq.2) then
                        isMERGEDu(nm) = 1
                     elseif (j.eq.3) then
                        isMERGEDv(nm) = 1
                     elseif (j.eq.4) then
                        isMERGEDu(nm-icx) = 1
                     endif
                     !exit is commented!!
                  endif
               enddo
               if (nmOK.eq.-999) then 
                  write(*,*) 'virtualMERGEupd=true but no adjacent is found for cell nm=',nm
                  call d3stop(1, gdp)
               endif

            endif
         enddo
      CASE DEFAULT 
         write(*,*) 'typeVIRTmergeUPD not implemented'
         call d3stop(1, gdp)
      END SELECT
!
RETURN
end subroutine COMPUTEmergingCARATT
