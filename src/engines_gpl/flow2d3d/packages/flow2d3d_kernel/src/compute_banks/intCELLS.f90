subroutine intCELLS(gsqs,kfs,kcs,s1,u1,v1,dps,lunscr,Irov,mmax,nmax,nmaxus,kmax,nlb,nub,mlb,mub,nmlb,nmub, gdp)
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
!  $Id: intCELLS.f90 7024 2017-02-20 13:43:29Z canestrelli.x $
!  $HeadURL: 
!!--description-----------------------------------------------------------------`
!
!    Function: Read polygons from file and determine the porosity function "poros" for each cell.
!
!    Author: Alberto Canestrelli          
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
    use globaldata
    use dfparall
    use Cplusplus
    use mathconsts, only: pi
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp)                  , pointer :: R1_anal
    real(fp)                  , pointer :: R2_anal
    integer                   , pointer :: analyticalPOLY
    integer                   , pointer :: NvertTOT
    integer                   , pointer :: Npoly
    integer                   , pointer :: idebugCUT
    integer, dimension(:)     , pointer :: nvert
    integer, dimension(:)     , pointer :: progVERT
    real(fp), dimension(:,:)  , pointer :: dpH
    real(fp), dimension(:,:)  , pointer :: dpL
    real(fp), dimension(:,:)  , pointer :: POROS
    real(fp), dimension(:,:)  , pointer :: PSIx
    real(fp), dimension(:,:)  , pointer :: PSIy
    real(fp), dimension(:,:)  , pointer :: xcor0
    real(fp), dimension(:,:)  , pointer :: ycor0
    real(fp), dimension(:,:)  , pointer :: xG
    real(fp), dimension(:,:)  , pointer :: yG
    real(fp), dimension(:)    , pointer :: vertx
    real(fp), dimension(:)    , pointer :: verty
    real(fp), dimension(:,:,:), pointer :: xcell
    real(fp), dimension(:,:,:), pointer :: ycell
    real(fp), dimension(:,:)  , pointer :: Npsi
    real(fp), dimension(:,:)  , pointer :: Neta
    real(fp), dimension(:,:)  , pointer :: Nx
    real(fp), dimension(:,:)  , pointer :: Ny
    logical, dimension(:,:)   , pointer :: CELLtoRECON
    logical                   , pointer :: EXACTpolygons
    logical                   , pointer :: EXACTpolygonsONLYfirst
    logical                   , pointer :: IGNOREwrongDEPTH
!
! Global variables
!
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: s1 
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: u1
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: v1
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: gsqs
    real(prec), dimension(nlb:nub,mlb:mub)                              , intent(in)    :: dps
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kfs 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcs 
    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: lunscr   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: irov   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
!    
! parameter
!
    real(fp), parameter            :: hugeNUM=HUGE(1.0_fp) 
    integer, parameter             :: maxNODdry     = 1000
    integer, parameter             :: maxINTERSedge = 10
!
! Local variables
!
    integer                        :: i
    integer                        :: form
    integer                        :: j
    integer                        :: k
    integer                        :: kk
    integer                        :: N
    integer                        :: M
    integer                        :: Nk
    integer                        :: Mk
    integer                        :: Nref
    integer                        :: Mref
    integer                        :: Nprec
    integer                        :: Mprec
    integer                        :: Np
    integer                        :: Np2
    integer                        :: Nv
    integer                        :: Nv2
    integer                        :: cont
    integer                        :: L
    integer                        :: mADJ
    integer                        :: nADJ
    integer                        :: Lad    
    integer                        :: typeINTER
    integer                        :: nextVERT
    integer                        :: NnodDRY
    integer                        :: LwithVERTEX
    integer                        :: nPROV
    integer                        :: MPROV
    integer                        :: deadlock
    integer                        :: Vini,Vfin,vertNini,vertMini
    integer                        :: mEDGEad(4)
    integer                        :: nEDGEad(4)
    integer                        :: mCORNad(4)
    integer                        :: nCORNad(4)
    integer                        :: Nok
    integer                        :: KKok
    integer                        :: Ninterf
    integer                        :: NdryVERT  
    integer                        :: NdryVERT_in 
    integer                        :: NdryVERT_out
!
    logical, allocatable           :: VERTinCELL(:)
    logical, allocatable           :: NODinPOLY(:,:,:)
!
    integer                        :: inivert 
    integer                        :: finVERT
    integer, allocatable           :: shift(:)
    integer, allocatable           :: NODinPOLYnp(:,:)  
    integer, allocatable           :: CELLinPOLY(:,:,:),TOTinterL(:,:,:)
    integer, allocatable           :: vertINcellN(:)
    integer, allocatable           :: vertINcellM(:)
!
    real(fp)                       :: AREA
    real(fp)                       :: AreaDRY
    real(fp)                       :: dummyR
    real(fp)                       :: PiPj 
    real(fp)                       :: xjxi 
    real(fp)                       :: xjxi2 
    real(fp)                       :: yjyi 
    real(fp)                       :: yjyi2 
    real(fp)                       :: xjxiyjyi 
    real(fp)                       :: xjxiPiPj 
    real(fp)                       :: yjyiPiPj 
    real(fp)                       :: INVxjxi2 
    real(fp)                       :: xjxiyjyi_X_INVxjxi2
    real(fp)                       :: aa
    real(fp)                       :: bb
    real(fp)                       :: angle
    real(fp)                       :: dx
    real(fp)                       :: dy
    real(fp)                       :: xint(2,3)
    real(fp)                       :: yint(2,3)
    real(fp)                       :: xint_prov(4)
    real(fp)                       :: yint_prov(4)
    real(fp)                       :: modN
    real(fp)                       :: R12(2)
    real(fp)                       :: Nx_pr(4)
    real(fp)                       :: Ny_pr(4)
    real(fp)                       :: radius(2) 
    real(fp)                       :: midRADIUS
    real(fp)                       :: Dteta    
    real(fp)                       :: teta     
    real(fp),allocatable           :: Xcirc(:,:),Ycirc(:,:)                  
!
    real(fp),allocatable           :: XI(:,:),YI(:,:)
    real(fp),allocatable           :: interL(:,:,:,:,:) 
!
    real(fp),allocatable           :: NODpolyDRY(:,:)
    real(fp),allocatable           :: POROSpart(:,:,:) ! porosity given by a single input polygon. poros = sum(POROSpart) over all polygons
!
    logical                        :: inp,found,atBOUND
    logical                        :: good(4)
!
    logical,allocatable            :: foundINT(:,:)
    logical                        :: INpoly
    logical                        :: changeSIGN
!
    logical, allocatable           :: vect4log(:,:,:,:)
! 
    character*100 charBUTTA2,charBUTTA,FILEtypeCELL 
!
! executable statements -------------------------------------------------------
!
    R1_anal                => gdp%gdimbound%R1_anal
    R2_anal                => gdp%gdimbound%R2_anal
    analyticalPOLY         => gdp%gdimbound%analyticalPOLY
    NvertTOT               => gdp%gdimbound%NvertTOT
    Npoly                  => gdp%gdimbound%Npoly
    idebugCUT              => gdp%gdimbound%idebugCUT
    nvert                  => gdp%gdimbound%nvert
    progVERT               => gdp%gdimbound%progVERT
    dpH                    => gdp%gdimbound%dpH
    dpL                    => gdp%gdimbound%dpL
    POROS                  => gdp%gdimbound%POROS
    PSIx                   => gdp%gdimbound%PSIx
    PSIy                   => gdp%gdimbound%PSIy
    xcor0                  => gdp%gdimbound%xcor0
    ycor0                  => gdp%gdimbound%ycor0
    xG                     => gdp%gdimbound%xG
    yG                     => gdp%gdimbound%yG
    vertx                  => gdp%gdimbound%vertx
    verty                  => gdp%gdimbound%verty
    xcell                  => gdp%gdimbound%xcell
    ycell                  => gdp%gdimbound%ycell
    Npsi                   => gdp%gdimbound%Npsi
    Neta                   => gdp%gdimbound%Neta
    Nx                     => gdp%gdimbound%Nx
    Ny                     => gdp%gdimbound%Ny
    CELLtoRECON            => gdp%gdimbound%CELLtoRECON
    EXACTpolygons          => gdp%gdimbound%EXACTpolygons
    EXACTpolygonsONLYfirst => gdp%gdimbound%EXACTpolygonsONLYfirst
    IGNOREwrongDEPTH       => gdp%gdimbound%IGNOREwrongDEPTH
    !
    allocate(VERTinCELL(NvertTOT))    
    allocate(CELLinPOLY(Npoly,Nmax,Mmax))  
    allocate(NODinPOLY(Npoly,0:Nmax,0:Mmax))
    allocate(vertINcellN(NvertTOT))
    allocate(vertINcellM(NvertTOT))
    allocate(xi(2,NvertTOT))
    allocate(yi(2,NvertTOT))
    allocate(vect4log(4,Npoly,nmax,mmax))
    allocate(TOTinterL(4,Nmax,Mmax))
    allocate(interL(2,maxINTERSedge,4,nmax,mmax))
    allocate(foundINT(nmax,mmax))
    allocate(POROSpart(Npoly,nmax,mmax))
    allocate(NODpolyDRY(maxNODdry,2))
    allocate(shift(Npoly))
!
!     CHECK THAT THE FIRST NODE of polygon CONCIDES WITH LAST (this assumption is used below in the intersection search)
!     In future maybe this check can be removed and the cycle where the intersections are found can be done as Nv = 1,Nvert(Np) (check if it works for both closed and open polygons)
!    
    if (analyticalPOLY==0) then
!
       iniVERT = 1
       finVERT = 0
       do Np=1,Npoly
          finVERT = finVERT + Nvert(Np) 

          if (.not.(comparereal(vertx(iniVERT),vertx(finVERT)).eq.0 .and. comparereal(verty(iniVERT),verty(finVERT)).eq.0)) then
             write(*,*) 'First and last points of all bank polygons must coincide!!' 
             call d3stop(1, gdp)
          endif
          iniVERT = INIvert + Nvert(Np) 

       enddo
!
    endif
    
!
!    CHECK if polygons intersect by checking if any node of a polygon is inside of any other polygon.
!    If this is the case then exit.  
!
    if (analyticalPOLY==0) then
       do Np=1,Npoly
           do Np2=1,Npoly
              if (Np2.ne.Np) then ! check if point Np2 is inside polygon Np
                 do Nv2 = 1,Nvert(Np2)  
                    cont = progVERT(Np2)+Nv2
                    call inpolygon(vertx(cont)            , verty(cont)            , &
                                   vertx(progVERT(Np)+1)  , verty(progVERT(Np)+1)  , nvert(Np) ,INpoly)  
                    if (INpoly) then
                       write(*,*) 'Input polygon for cut cells cannot intersect!!!'
                       call d3stop(1, gdp)
                    endif
                 enddo
              endif
           enddo
       enddo     
    endif
!
!   Determine if cell nodes are inside the polygons or not
!
!   All singular cases can be handled thinking that a cell is cut by one (OR MORE) polygon(s) if and only if (either a grid
!   node is inside a polygon or a polygon vertex is inside the cell or both)
!
    if (analyticalPOLY==0) then
       do m=0,mmax
          do n=0,nmaxus
         ! note NO MASKING HERE SINCE IT IS A CYCLE ON THE NODES (HARD TO MASK  USING CELL MASKING I RATHER DO ALL OF THEM)
                do Np=1,Npoly
                   call inpolygon(xcor0(n,m) , ycor0(n,m) , vertx(progVERT(Np)+1)  , &
                                                          verty(progVERT(Np)+1)  , nvert(Np) ,NODinPOLY(Np,n,m))                                               
                enddo
          enddo
       enddo   
    else
       if (analyticalPOLY==1) then !circle
          do m=0,mmax
             do n=0,nmaxus
                ! first cyrcle (internal)
                if (comparereal(xcor0(n,m)**2,R1_anal**2-ycor0(n,m)**2).lt.0) then 
                    NODinPOLY(1,n,m) =.true.!inside the circle (i.e. in the bank)
                else
                    NODinPOLY(1,n,m) =.false.!outside the circle (water)
                endif
                ! first cyrcle (internal)
                if (comparereal(xcor0(n,m)**2,R2_anal**2-ycor0(n,m)**2).gt.0) then 
                    NODinPOLY(2,n,m) =.true.!outside the circle (i.e. in the bank)
                else
                    NODinPOLY(2,n,m) =.false.!inside the circle (water)
                endif
             enddo
          enddo   
       endif  
    endif
    if (idebugCUT.eq.1) THEN
       do Np=1,Npoly
          write(charBUTTA,'(i7)') nP
          FILEtypeCELL = 'NODinPOLY_'//TRIM(ADJUSTL(charBUTTA))//'.txt'
          OPEN(40,file = FILEtypeCELL)
          form = mmax-1
          do n=1,nmaxus-1
             write(40,'(<form>i10)' ) (NODinPOLY(Np,n,m),m=1,mmax-1)
          enddo
          close(40)
       enddo
    endif    
!
!  determine if a cell is a vegetated bank, non-vegetated or an interfacial cell
!             
    do m=1,mmax
       do n=1,nmaxus
          if (kcs(n,m)==1.or.kcs(n,m)==2) then
             do Np=1,Npoly
                vect4log(1,Np,n,m) =  NODinPOLY(Np,n-1,m-1) ! store nodes in anticlockwise way
                vect4log(2,Np,n,m) =  NODinPOLY(Np,n-1,m)    
                vect4log(3,Np,n,m) =  NODinPOLY(Np,n,m)                       
                vect4log(4,Np,n,m) =  NODinPOLY(Np,n,m-1)
                !
                if (ALL(vect4log(:,Np,n,m))) then
                   CELLinPOLY(Np,n,m) = 1
                ELSEif (ANY(vect4log(:,Np,n,m))) then
                   CELLinPOLY(Np,n,m) = 2
                ELSE
                   CELLinPOLY(Np,n,m) = 0
                ENDIF
             enddo
          ELSE
             CELLinPOLY(1:NPOLY,n,m) = 1 !kcs cells are vegetated
          endif
       enddo
    enddo   
!
!   DETERMINE INTERSECTION BETWEEN POLYGONS AND CELLS !when RESHAPE_CYCLE remove this block
!
    do m=1,mmax 
       do n=1,nmaxus
          ! x- and y-coordinates of the 4 corners of cell (n,m) in counter-clockwise direction
          xcell(:,n,m) = (/ xcor0(n-1,m-1), xcor0(n-1,m), xcor0(n,m), xcor0(n,m-1), xcor0(n-1,m-1)/)
          ycell(:,n,m) = (/ ycor0(n-1,m-1), ycor0(n-1,m), ycor0(n,m), ycor0(n,m-1), ycor0(n-1,m-1)/)
       enddo
    enddo
    !END BLOCK TO BE REMOVED when RESHAPE_CYCLE 
!
!   My first idea:check that first node of polygon is inside a cell. If not rotate the numbering of the polygon in a way
!   the first node is inside the domain. if a polygon has no node inside the domain, delete the polygon 
!   and issue a warning
!   Final idea: you can have all nodes of the polygon outside of the domain but still have cut cells. So
!   I first compute the intersection with the edges starting from the nodes of polygons that are 
!   contained in some cells. The remaining intersection are the ones departing from the boundary. 
!   and I comnpute them separately. Note that I ignore the presence of cells that are excluded from the
!   computation, it is easier to find cut cells just starting from the boundary.
!      
!   DETERMINE what cell do the vertices belong to.
!
    xi(:,:) = hugeNUM
    yi(:,:) = hugeNUM
    TOTinterL(:,:,:) =  0
    !
    if (analyticalPOLY==0) then 
       !
       vertINcellN(:) = -999
       vertINcellM(:) = -999
       do Np=1,Npoly
          found=.false.
         ! shift(Np) = 0
          do Nv = 1,Nvert(Np) -1 ! because the last coincides with the first, I deal with the last separately below 
             cont = progVERT(Np)+Nv
             DOexitthis : do m=1,mmax 
                do n=1,nmaxus 
                   if (kcs(n,m)==1.OR.kcs(n,m)==2) then               
                     call inpolygon(vertx(cont),verty(cont), xcell(1:5,n,m) , ycell(1:5,n,m) , 5 ,VERTinCELL(cont)) !,n,m))  
                     !if (VERTinCELL(cont,n,m).eq..true.) then
                     if (VERTinCELL(cont).eq..true.) then
                         vertINcellN(cont) = n
                         vertINcellM(cont) = m
                         CELLinPOLY(Np,n,m) = 2  ! i put it in the list of cut cell! if this case the nodal test would have failed (all nodes are outside)
                           !       _______
                           !      |       | 
                           !      |   /\  |     CASE 1
                           !      |_ /__\_|
                           !        /    \                    
                         exit DOexitthis
                     endif         
                   endif                                                                   
                enddo                                                      
             enddo DOexitthis
          enddo 
       enddo 
!      
!      
       !write debug file (to be commented)
       if (idebugCUT.eq.1) THEN
          do Np=1,Npoly
             write(charBUTTA,'(i7)') nP
             FILEtypeCELL = 'TYPEcell_'//TRIM(ADJUSTL(charBUTTA))//'.txt'
             OPEN(20,file = FILEtypeCELL)
             do n=2,nmaxus-1            
                write(20,'(<mmax>i9)') (CELLinPOLY(Np,n,m),m=2,mmax-1 )
             enddo  
             CLOSE(20)  
          enddo
       endif
!      
!      copy the attribute on the last vertexes (= with the first)
       do Np=1,Npoly
          Vini = progVERT(Np) + 1
          Vfin = progVERT(Np) + Nvert(Np)   
          vertNini = vertINcellN(Vini)
          vertMini = vertINcellM(Vini)
          vertINcellN(Vfin)     = vertNini 
          vertINcellM(Vfin)     = vertMini
          VERTinCELL(Vfin)  = VERTinCELL(Vini)
       enddo
!      
       if (idebugcut.eq.1) THEN
          do Np=1,Npoly
             write(charBUTTA,'(i7)') Np
             FILEtypeCELL = 'vertINcell_'//TRIM(ADJUSTL(charBUTTA))//'.txt'
             OPEN(20,file = FILEtypeCELL) 
             do Nv = 1,Nvert(Np)
                cont = progVERT(Np)+Nv
                if (VERTinCELL(cont).eq..true.) then
                  write(20,'(<mmax>f25.15)') (xG(vertINcellN(cont),vertINcellM(cont)),yG(vertINcellN(cont),vertINcellM(cont)))
                endif
             enddo
          enddo
          close(20)  
       endif
!
!    this was the first version that was faster but it was overly complicated and too many exceptions had to be handled
!       
!       
!!!!    interL(:,:,:,:,:) =   -99999999999._fp
!    do Np=1,Npoly
!       foundINT(:,:)=.false.
!       do Nv = 1,Nvert(Np)-1 ! because the last coincides with the first
!          cont = progVERT(Np)+Nv
!          if (VERTinCELL(cont).eq..TRUE.) then
!             n = vertINcellN(cont)
!             m = vertINcellM(cont) 
!             nextVERT = cont + 1
!             deadlock = 0
!             DO_WHILE : DO WHILE (vertINcellN(nextVERT).NE.n.OR.vertINcellM(nextVERT).NE.m) !i.e. do until the vertex of hte polygon is in the cell itself
!               WRITE(11111,*) Np,Nv,N,M
!                found = .false.
!                do L=1,4  !cycle on the four edges
!                   IF (TOTinterL(L,n,m).eq.0) then ! I NEED IT TO DEAL WITH SINGULAR CASE 2 WITH THE VERTEX BEING ASSIGNED TO THE UPPER POLYGON (REMEMBER VERTEX ASSIGNMENT IS SUBSTATIALLY RANDOM, VERTEX IT IS ASSIGNED TO FIRST CELL THAT HIT INPOLYGON SUBROUTINE ABOVE. 
!                                                     !NON VA BENE SE VOGLIO USARE MULTIPLE INTERSEC
!                      call My_intersec( vertx(cont)    ,verty(cont)    ,vertx(nextVERT) ,verty(nextVERT)   , & 
!                                        xcell(L,n,m)   ,ycell(L,n,m)   ,xcell(L+1,n,m)  ,ycell(L+1,n,m)    , &
!                                        xi(:,cont)     ,yi(:,cont)     ,typeINTER       )
!                      if ((typeINTER.eq.1).or.(typeINTER.eq.2)) THEN ! I FOUND IT, I STORE AND I EXIT!! I USE IDEA THAT A SEGMENT OF POLYGON WITH STARTING POINT INSIDE THE CELL AND ENDING POINT OUTSIDE THE CELL HAS ONE AND ONLY ONE INTERSECTION WITH AN EDGE ONLY IF THE CELL IS CONVEX!!  
!                        ! typeINTER.eq.2 handles the case in which the segments are coincident but then the polygon changes direction within the cell
!                        CALL ADJACENT(L,m,n,mADJ,nADJ,Lad) ! gives the cell adjacent to cell (n,m) on the edge L                       
!
!                            !store the informations 
!                            TOTinterL(L,n,m) = TOTinterL(L,n,m) + 1 
!                            if (TOTinterL(L,n,m).gt.maxINTERSedge) then
!                            !   WRITE(*,*) l,N,M
!                               write(lunscr,*) ' more than maxINTERSedge intersections with polygon', Np ,' in one edge of a cell. Please refine the grid or derefine the polygon'
!                               call d3stop(1, gdp)
!                            endif
!                            interL(1,TOTinterL(L,n,m),L,n,m) =  xi(1,cont)   
!                            interL(2,TOTinterL(L,n,m),L,n,m) =  yi(1,cont)                          
!                            TOTinterL(Lad,nADJ,mADJ) = TOTinterL(Lad,nADJ,mADJ) + 1   
!                            interL(1,TOTinterL(Lad,nADJ,mADJ),Lad,nADJ,mADJ) =  xi(1,cont) ! salvo solo il primo
!                            interL(2,TOTinterL(Lad,nADJ,mADJ),Lad,nADJ,mADJ) =  yi(1,cont) ! salvo solo il primo
!                            if (typeINTER.eq.2) THEN !coincident line I save 2 points     
!                               DO K=1,2
!                                  IF (xi(k,cont).lt.HUGEnum-1) then  !It is actually not necessary, if the grid has been generated properly they have always two intersections, worst case they are coincident
!                                     TOTinterL(L,n,m) = TOTinterL(L,n,m) + 1 
!                                     interL(1,TOTinterL(L,n,m),L,n,m) =  xi(2,cont)   
!                                     interL(2,TOTinterL(L,n,m),L,n,m) =  yi(2,cont)   
!                              TOTinterL(Lad,nADJ,mADJ) = TOTinterL(Lad,nADJ,mADJ) + 1   
!                              interL(1,TOTinterL(Lad,nADJ,mADJ),Lad,nADJ,mADJ) =  xi(2,cont) ! I store the second in the adj.
!                              interL(2,TOTinterL(Lad,nADJ,mADJ),Lad,nADJ,mADJ) =  yi(2,cont) ! I store the second in the adj.
!                                  ENDIF
!                               ENDDO
!                            endif
!                            write(8888,*) Np,nV,L,N,M
!                            write(200000,'(<mmax>f25.15)') interL(1,TOTinterL(L,n,m),L,n,m),interL(2,TOTinterL(L,n,m),L,n,m)
!                            write(200000,'(<mmax>f25.15)') interL(1,TOTinterL(Lad,nADJ,mADJ),Lad,nADJ,mADJ),interL(2,TOTinterL(Lad,nADJ,mADJ),Lad,nADJ,mADJ)
!!
!                            IF (CELLinPOLY(Np,nADJ,mADJ)== 2)  THEN ! IF THE ADJACENT CELL IS NOT AN INTERFACE CELL I STORE IT FOR CELL (n,m) but not for the adjacent cell.
!                               foundINT(n,m) = .TRUE.
!                               found =  foundINT(n,m) 
!                               ! update CELLinPOLY. Some cells could have been cut by the polygon but still with all the cell nodes outside the polygon and the polygon vertex outside the cell, e.g::
!                               !       ____/\____
!                               !      |   /  \   | 
!                               !      |  /    \  |  CASE 3
!                               !      |_/______\_|
!                               !       /        \
!                               CELLinPOLY(Np,n,m) = 2
!                               CELLinPOLY(Np,nADJ,mADJ) = 2 !note I dont use any special treatment for boundary cell since CELLinPOLY has first and last raw/column for bb cells, so I never go out of bounds in the array.
!                               mPREC = m
!                               nPREC = n
!                               n = nADJ
!                               m = mADJ
!                               CONTINUE
!                               exit  ! (could be different for unstructured grid) NOTE: I can exit because  A SEGMENT OF POLYGON WITH STARTING POINT INSIDE THE CELL AND ENDING POINT OUTSIDE THE CELL HAS ONE AND ONLY ONE INTERSECTION WITH AN EDGE ONLY IF THE CELL IS CONVEX!!  (could be different for unstructured grid)
!                                     !note: if CELLinPOLY(Np,nADJ,mADJ)/= 2 the adjacent is not dry and I cannot exit, I have to keep searching there has to be another intersection unless I am at the domain boundary
!                            ENDIF
!!
!                      endif !typeintersect
!                   ELSE 
!                       !CALL ADJACENT(L,m,n,mADJ,nADJ,Lad)
!                      LwithVERTEX= L !this used only if I do not find any edges with intersection beside that (that means that I am in the singular case 4)
!                   ENDIF
!                enddo
!                if (.NOT.found) then !AND (N,M) IS NOT BOUNDARY (i.e. on the cell next to the enclosure polygon
!                  !    WE HAVE TO HANDLE THE CASE IN WHICH THE POLYGON EXACTLY TOUCHES AN EDGE 
!                  !   (I ALREADY COMPUTED THE INTERSECTIONS FOR BOTH THE CELLS WHEN I WAS IN THE CELL UNDERNEATH IT, AND THE PROGRAM STOPS IN THE WET CELL CAUSE IT DOES NOT FIND OTHER INTERSECTION
!                  !       __________
!                  !      |          | 
!                  !      |          |   CASE 2
!                  !      |__________|
!                  !           /\   
!                  !          /  \  
!                  !other similar  special cases: the node shared by the 4 cells is inside cell (m,n). When I am in that cell I have to go back the the adjacent in order not to deadlock the search
!                  !           _____________ _____________
!                  !          |  (mADJ,nADJ)|    (m,n)    |
!                  !          |CELLinPOLY/=2| CELLinPOLY=2|            CASE 4a
!                  !polig_____|_____________|_____________|
!                  !          |             |\            |
!                  !          |CELLinPOLY=2 | \ CELLinPOLY=2           
!                  !          |_____________|__\__________| 
!                  !                            \
!                  !                             \polig(scanned from here)
!                  !even worst
!                  !           _____________ _____________
!                  !          |  (mADJ,nADJ)|    (m,n)    |
!                  !          |CELLinPOLY/=2| CELLinPOLY=2|            CASE 4b
!                  !polig_____|_____________|_____________|
!                  !          |             |             |
!                  !          |CELLinPOLY=2 |CELLinPOLY/=2|
!                  !          |_____________|_____________| 
!                  !                        |
!                  !                        | polig(scanned from here)
! 
!                 !  CALL ADJACENT(LwithVERTEX,m,n,mADJ,nADJ,Lad) ! gives the cell adjacent to cell (n,m) on the edge L
!                 !  write(200000,'(<mmax>f25.15)') interL(1,TOTinterL(LwithVERTEX,n,m),LwithVERTEX,n,m),interL(2,TOTinterL(LwithVERTEX,n,m),LwithVERTEX,n,m)  
!                 !  write(8888,*) Np,nV,LwithVERTEX,N,M,'NOT FOUND'
!                 !  n = nADJ
!                 !  m = mADJ
!                     !first time I try to go back to an adjacent on edge with  CELLinPOLY(Np,KK,K)== 2) (i.e.interface cell). If It fails, I check for an adjacent on corners with  CELLinPOLY(Np,KK,K)== 2)
!                      mEDGEad(1) = m-1;  nEDGEad(1) = n
!                      mEDGEad(2) = m  ;  nEDGEad(2) = n-1
!                      mEDGEad(3) = m+1;  nEDGEad(3) = n
!                      mEDGEad(4) = m  ;  nEDGEad(4) = n+1
!                      mCORNad(1) = m-1;  nCORNad(1) = n-1
!                      mCORNad(2) = m+1;  nCORNad(2) = n-1
!                      mCORNad(3) = m+1;  nCORNad(3) = n+1
!                      mCORNad(4) = m-1;  nEDGEad(4) = n+1
!                   !   ATbound = .true.
!
!                      do k=1,4 ! IF NOT SQUARE DOMAIN JUST GIVE CELLinPOLY= 0  TO GHOST CELLS
!                         IF ((CELLinPOLY(Np,nEDGEad(k),mEDGEad(k))== 2)) then  
!                            m = mEDGEad(k)
!                            n = nEDGEad(k)
!                      !      ATbound = .FALSE.
!                            deadlock=deadlock+1
!                            IF (vertINcellN(cont).NE.n.OR.vertINcellM(cont).NE.m) THEN 
!                               cycle DO_WHILE 
!                            ELSE                        ! TO AVOID CASE 4
!                               exit DO_WHILE            ! TO AVOID CASE 4
!                            ENDIF
!                         ENDIF
!                      enddo
!                      do k=1,4
!                         IF ((CELLinPOLY(Np,nCORNad(k),mCORNad(k))== 2)) then  
!                            m = mCORNad(k)
!                            n = nCORNad(k)
!                            deadlock=deadlock+1
!                 !           ATbound = .FALSE.
!                            IF (vertINcellN(cont).NE.n.OR.vertINcellM(cont).NE.m) THEN 
!                               cycle DO_WHILE 
!                            ELSE                        ! TO AVOID CASE 4
!                               exit DO_WHILE            ! TO AVOID CASE 4
!                            ENDIF 
!                         ENDIF
!                      enddo
!                      do k=1,4 ! I SEARCH FOR AN ADJACENT ON THE EDGE WITH  FoundINT=FALSE! IF NOT SQUARE DOMAIN JUST GIVE CELLinPOLY= 0  TO GHOST CELLS
!                         IF (.not.FoundINT ( nEDGEad(k),mEDGEad(k) ) ) then  
!                            m = mEDGEad(k)
!                            n = nEDGEad(k)
!                      !      ATbound = .FALSE.
!                            deadlock=deadlock+1
!                            IF (vertINcellN(cont).NE.n.OR.vertINcellM(cont).NE.m) THEN 
!                               cycle DO_WHILE 
!                            ELSE                        ! TO AVOID CASE 4
!                               exit DO_WHILE            ! TO AVOID CASE 4
!                            ENDIF
!                         ENDIF
!                      enddo
!                      do k=1,4 !  I SEARCH FOR AN ADJACENT ON THE corner WITH  FoundINT=FALSE (TO ANDLE CASE 4) IF NOT SQUARE DOMAIN JUST GIVE CELLinPOLY= 0  TO GHOST CELLS
!                         IF (.not.FoundINT(nCORNad(k),mCORNad(k) )) then  
!                            m = mCORNad(k)
!                            n = nCORNad(k)
!                            deadlock=deadlock+1
!                 !           ATbound = .FALSE.
!                            IF (vertINcellN(cont).NE.n.OR.vertINcellM(cont).NE.m) THEN 
!                               cycle DO_WHILE 
!                            ELSE                        ! TO AVOID CASE 4
!                               exit DO_WHILE            ! TO AVOID CASE 4
!                            ENDIF 
!                         ENDIF
!                      enddo
!                      !if both fails I am here:
!                      !it is a boundary cell?
!                      !IF (ATbound) THEN NON SERVE PIU`ATbound ORA HO  exit DO_WHILE 
!                    !  if (deadlock.gt.3) EXIT DO_WHILE 
!                      !ENDIF
!                      !
!                      !
!                      !
!!                      if(kkk.eq.1) then
!!                         DO K=m-1,m+1
!!                            do KK=n-1,n+1
!!                               IF ((CELLinPOLY(Np,KK,K)== 2).and.(k.ne.m).and.(kk.ne.n))  THEN
!!                                 m = K
!!                                 n = KK
!!                                 exit DO_exit_here
!!                               ENDIF
!!                            ENDDO
!!                         ENDDO
!!                      endif
! 
!
!                   !  EXIT !WHILE CYCLE
!                   !NOTE I DONT STORE THE ADJACENT SINCE IT WAS ALREADY STORED
!                    !     write(lunscr,*) 'cannot find  intersection between polygon line and any edge of the cell.'
!                    !     call d3stop(1, gdp)
! 
!                endif 
!             ENDDO DO_WHILE
!!
!          endif   
!       enddo    
!    enddo
!
       do Np=1,Npoly
          do Nv = 1,Nvert(Np)-1 ! because the last coincides with the first
             cont = progVERT(Np)+Nv
             nextVERT = cont + 1
             do m=1,mmax
                do n=1,nmaxus
                   do L=1,4
                      IF (kcs(n,m)==1.or.kcs(n,m)==2) then !TAKE OUT THIS IF AND THE NEXT FROM THE L CYCLE
                         IF (CELLinPOLY(Np,n,m)== 2) then !TAKE OUT THIS IF AND THE PREVIOUS FROM THE L CYCLE
                           call My_intersec( vertx(cont)    ,verty(cont)    ,vertx(nextVERT) ,verty(nextVERT)   , & 
                                             xcell(L,n,m)   ,ycell(L,n,m)   ,xcell(L+1,n,m)  ,ycell(L+1,n,m)    , &
                                             xi(:,cont)     ,yi(:,cont)     ,typeINTER       )
                           if ((typeINTER.eq.1).or.(typeINTER.eq.2)) THEN ! I FOUND IT, I STORE AND I EXIT!! I USE IDEA THAT A SEGMENT OF POLYGON WITH STARTING POINT INSIDE THE CELL AND ENDING POINT OUTSIDE THE CELL HAS ONE AND ONLY ONE INTERSECTION WITH AN EDGE ONLY IF THE CELL IS CONVEX!!  
                           ! typeINTER.eq.2 handles the case in which the segments are coincident but then the polygon changes direction within the cell
       
                              !store the informations 
                              TOTinterL(L,n,m) = TOTinterL(L,n,m) + 1 
                              if (TOTinterL(L,n,m).gt.maxINTERSedge) then
                              !   WRITE(*,*) l,N,M
                                 write(lunscr,*) ' more than maxINTERSedge intersections with polygon', Np ,' in one edge of a cell. Please refine the grid or derefine the polygon'
                                 call d3stop(1, gdp)
                              endif
                              interL(1,TOTinterL(L,n,m),L,n,m) =  xi(1,cont)   
                              interL(2,TOTinterL(L,n,m),L,n,m) =  yi(1,cont)   
                              if (typeINTER.eq.2) THEN !coincident line I save 2 points     
                                 IF (xi(2,cont).lt.HUGEnum-1) then  !It is actually not necessary, if the grid has been generated properly they have always two intersections, worst case they are coincident
                                    TOTinterL(L,n,m) = TOTinterL(L,n,m) + 1 
                                    interL(1,TOTinterL(L,n,m),L,n,m) =  xi(2,cont)             ! I store the second   
                                    interL(2,TOTinterL(L,n,m),L,n,m) =  yi(2,cont)             ! I store the second 
                                 ENDIF
                              endif                      
                            endif
                         endif 
                      endif
                   enddo
                enddo
             enddo
          enddo
       enddo
!
    elseif (analyticalPOLY==1) then !circle
!        
       R12(1) = R1_anal
       R12(2) = R2_anal
       cont = 1 ! I do not need to store xi and yi, so I will always use cont=1 without increasing cont
       do m=1,mmax
          do n=1,nmaxus
             IF (kcs(n,m)==1.or.kcs(n,m)==2) then  
                do Np=1,Npoly
                   IF (CELLinPOLY(Np,n,m)== 2) then  
                      do L=1,4                         
                         call My_intersec_circle( xcell(L,n,m)  ,ycell(L,n,m)  ,xcell(L+1,n,m) ,ycell(L+1,n,m)  , &
                                                  0._fp         ,0._fp         ,R12(Np)        ,xi(:,cont)      ,yi(:,cont)     ,typeINTER       ) !center of circle is (0,0)
                         if ((typeINTER.eq.1).or.(typeINTER.eq.2)) THEN ! I FOUND IT, I STORE it
                         
                            !store the informations 
                            TOTinterL(L,n,m) = TOTinterL(L,n,m) + 1 
                            if (TOTinterL(L,n,m).gt.maxINTERSedge) then
                               write(lunscr,*) ' more than maxINTERSedge intersections with polygon', Np ,' in one edge of a cell. Please refine the grid or derefine the polygon'
                               call d3stop(1, gdp)
                            endif
                            interL(1,TOTinterL(L,n,m),L,n,m) =  xi(1,cont)   
                            interL(2,TOTinterL(L,n,m),L,n,m) =  yi(1,cont)   
                           ! if (typeINTER.eq.2) THEN !coincident line I save 2 points     
                           !    IF (xi(2,cont).lt.HUGEnum-1) then  !It is actually not necessary, if the grid has been generated properly they have always two intersections, worst case they are coincident
                           !       TOTinterL(L,n,m) = TOTinterL(L,n,m) + 1 
                           !       interL(1,TOTinterL(L,n,m),L,n,m) =  xi(2,cont)             ! I store the second   
                           !       interL(2,TOTinterL(L,n,m),L,n,m) =  yi(2,cont)             ! I store the second 
                           !    ENDIF
                           ! endif                      
                         endif
                      enddo
                   endif 
                enddo
             endif
          enddo
       enddo
    endif
!
             

    if (idebugCUT.eq.1) THEN   
       FILEtypeCELL = 'intersectionPOINTS.txt'
       OPEN(20,file = FILEtypeCELL) 
       do m=1,mmax
          do n=1,nmaxus
             do L=1,4
               IF (TOTinterL(L,n,m).GT.0) THEN
                  write(20,'(<mmax>f25.15)') interL(1,TOTinterL(L,n,m),L,n,m),interL(2,TOTinterL(L,n,m),L,n,m)
               ENDIF
             enddo
          enddo
       enddo
       close(20)
    endif
!        
!   compute poros variable as sum of POROSpart
!    
    POROSpart(:,:,:) = 0._fp
!
    do m=1,mmax
       do n=1,nmaxus
          if (kcs(n,m)==1.or.kcs(n,m)==2) then         
             do Np=1,Npoly           
                IF (CELLinPOLY(Np,n,m).eq.2) then !Interfacial cell
                   NnodDRY = 0
              !    if (ANY( TOTinterL(:,n,m).gt.1) then !I SET TO ONE CELL WITH MORE THAN ONE !
              !      poros = 1._fp
              !      cycle
              !    endif
                   DO L=1,4
                     IF (vect4log(L,Np,n,m)) then !ADDING CELL NODE IF DRY
                        NnodDRY = NnodDRY + 1   
                        NODpolyDRY(NnodDRY,1) = xcell(L,n,m)
                        NODpolyDRY(NnodDRY,2) = ycell(L,n,m)       
                        IF (NnodDRY.GT.1) THEN !CHECK FOR DOUBLE NODES (VERY UNCOMMON, but it gives problems when exactpolygon=.true.)
                           IF (comparereal(NODpolyDRY(NnodDRY,1),NODpolyDRY(NnodDRY-1,1)).eq.0.AND.comparereal(NODpolyDRY(NnodDRY,2),NODpolyDRY(NnodDRY-1,2)).eq.0) then
                             NnodDRY = NnodDRY - 1 ! I TAKE IT OUT
                           endif                  
                        ENDIF         
                     ENDIF   
                 !    DO K=1,TOTinterL(L,n,m) !ADDING ALL EDGE INTERCEPTIONS IF PRESENT (in order to do this I have first to order them and replace 1 with k below, i.e. interL(1,K,L,n,m))
                     if (TOTinterL(L,n,m).gt.0) then ! i take only the first
                        NnodDRY = NnodDRY + 1   
                        NODpolyDRY(NnodDRY,1) = interL(1,1,L,n,m)
                        NODpolyDRY(NnodDRY,2) = interL(2,1,L,n,m)   
                        IF (NnodDRY.GT.1) THEN !CHECK FOR DOUBLE NODES (VERY UNCOMMON, but it gives problems when exactpolygon=.true.)
                          IF (comparereal(NODpolyDRY(NnodDRY,1),NODpolyDRY(NnodDRY-1,1)).eq.0.AND.comparereal(NODpolyDRY(NnodDRY,2),NODpolyDRY(NnodDRY-1,2)).eq.0) then
                             NnodDRY = NnodDRY - 1 ! I TAKE IT OUT
                          endif                  
                        ENDIF                                      
                     ENDif     
                   ENDDO   
                   IF (NnodDRY.GT.1) THEN !CHECK FOR DOUBLE NODES BEGINNING-END (I.E. LAST NODES WITH FIRST) !THIS IF MIGHT BE REMOVED AND LEFT ONLY THE INTERNAL ONE, SINCE Nnoddry should always >1 since CELLinPOLY=2 but double check
                     IF (comparereal(NODpolyDRY(NnodDRY,1),NODpolyDRY(1,1)).eq.0.AND.comparereal(NODpolyDRY(NnodDRY,2),NODpolyDRY(1,2)).eq.0) then
                        NnodDRY = NnodDRY - 1 ! I TAKE THE LAST OUT
                     endif                  
                   ENDIF   
                   NnodDRY = NnodDRY + 1                    !  ADD ENDING POIN COINCIDENT WITH THE FIRST, NEEDED FOR COMPUTING AREA.
                   if (NnodDRY.gt.maxNODdry) then
                      write(lunscr,*) ' more than maxNODdry NODES forming dry area in a cell. Please refine the grid or derefine the polygon'
                      call d3stop(1, gdp)
                   endif
                   NODpolyDRY(NnodDRY,:) = NODpolyDRY(1,:) !  ADD ENDING POINT COINCIDENT WITH THE FIRST, NEEDED FOR COMPUTING AREA.
                   ! COMPUTE AREA AND BARYCENTER
                   CALL A_G_Poly(NODpolyDRY(:,1),NODpolyDRY(:,2),NnodDRY,AREAdry,dummyR,dummyR,1,lunscr,gdp)                   
                   POROSpart(Np,n,m) =  (AREAdry/gsqs(n,m))                   
                ENDIF
             enddo
             IF (ANY(CELLinPOLY(:,n,m).eq.2)) then !Interfacial cell
                POROS(n,m) = 1._fp - SUM(POROSpart(:,n,m))
                 if (comparereal(POROS(n,m),0.9999999999_fp).ge.0)  then !CHANNEL cell (!it could be 1.0000000000000003124 i set it to 1._fp)
                    POROS(n,m) = 1._fp  
                 elseif(comparereal(POROS(n,m),0.0000000001_fp).le.0) then !BANK cell (!it could be -0.0000000000000003124 i set it to 0._fp)
                    POROS(n,m) = 0._fp  
                 else !interfacial cell
                     IF (EXACTpolygons) then
                        if (NnodDRY.gt.6) then
                           write(*,*) 'Option EXACTpolygons only valid if polygons intersect maximum 2 edges of a cell'
                           !pause
                           call d3stop(1,gdp)
                        endif
                        !
                        cont = 0
                        do L=1,4
                           if (TOTinterL(L,n,m).gt.0) then ! i take only the first
                              cont = cont + 1   
                              xINT_prov(cont) = interL(1,1,L,n,m)
                              yINT_prov(cont) = interL(2,1,L,n,m) 
                              IF (cont.GT.1) THEN !CHECK FOR DOUBLE NODES (VERY UNCOMMON, but it happens when a polygon has a node ocincident with a grid vertex)
                                IF (comparereal(xINT_prov(cont),xINT_prov(cont-1)).eq.0.AND.comparereal(yINT_prov(cont),yINT_prov(cont-1)).eq.0) then
                                   cont = cont - 1 ! I TAKE IT OUT
                                endif                  
                              ENDIF                                                                      
                           ENDif   
                        enddo
                        !cont is usually 2, but in rare cases I can intersect a first edge, then a second and change direction on the second adn go
                        ! collinear with the second (see at the boundary of the annular test case). In this case I want to ignore the intersection on
                        !  the vertexes, but this is not true in general, think of a bank cutting exactly diagonally through the 2 vertices
                        ! :Also, it can enter on one edge, go tangent to another (with 2nd point on edge) and then intersecting a third. Or the 2nd can exit and reenter the edge. So
                        !  in this case I remove the edge having a multiplicity. But note that in general it could happen that a ploygon has a point in each of 3 edges. In this rare case there its hard to determine which one is the interface,
                        ! the best way I found is to see if all the dry vertices lay on the same side and choose that as an interface.
                        if (cont==2) then
                           Nok=2
                           ! CONTINUE (i just use the first two, if I intersect 3 edges i always do an error neglecting one so its the same
                           xINT(1:2,1) = xINT_prov(1:2)
                           yINT(1:2,1) = yINT_prov(1:2)
                           nINTERF = 1
                        elseif (cont>=3) then
                           Nok=3
                           ! CONTINUE (i just use the first two, if I intersect 3 edges i always do an error neglecting one so its the same
                           xINT(1:2,1) = xINT_prov(1:2)
                           yINT(1:2,1) = yINT_prov(1:2)
                           xINT(1:2,2) = (/xINT_prov(1),xINT_prov(3)/)
                           yINT(1:2,2) = (/yINT_prov(1),yINT_prov(3)/)
                           xINT(1:2,3) = (/xINT_prov(2),xINT_prov(3)/)
                           yINT(1:2,3) = (/yINT_prov(2),yINT_prov(3)/)
                           nINTERF = 3
                           if (cont>=4) then ! (>4 should be impossible). =4: rare case in which polygon goes through 4 edges. In this case I simplify I use only first 3
                              cont = 3
                           endif                           
                        endif
                        if (cont.lt.2.or.Nok.lt.2) then
                           write(*,*) 'Error in intCELLS',N,M,CONT,nOK ! THERE HAS ALWAYS TO BE 2 INTERSECTION POINTS. Coincident when poros=0
                           !pause
                           call d3stop(1,gdp)
                        endif
                        ! iterate in all the possible combination of interfaces and choose the one that  contains all the dry vertices (there has always to be one)
                        KKok = -999
                        do kk = 1,nINTERF
                            Nx_pr(kk) = yINT(1,kk)-yINT(2,kk)    !direction but not sense (sense+orientation = direction of the vector  <=> in italian: direzione + verso + modulo = vettore)
                            Ny_pr(kk) = -(xINT(1,kk)-xINT(2,kk))
                            modN = sqrt(Nx_pr(kk)**2+Ny_pr(kk)**2) 
                            if (comparereal(modN,0._fp).eq.0) then
                               write(*,*) 'THERE are 2 Coincident INTERSECTION POINTS (poros=0) in intCELLS' ! , caso non trattato
                               !pause
                               call d3stop(1,gdp)
                            endif
                            Nx_pr(kk) = Nx_pr(kk)/modN
                            Ny_pr(kk) = Ny_pr(kk)/modN
                            ! check if I have to change sign to the normal
                            NdryVERT = 0
                            NdryVERT_in = 0
                            NdryVERT_out = 0
                            DO L=1,4
                               IF (ANY(vect4log(L,:,n,m))) then
                                  IF (comparereal(xcell(L,n,m),xINT(1,kk)).eq.0.AND.comparereal(ycell(L,n,m),yINT(1,kk)).eq.0.or.comparereal(xcell(L,n,m),xINT(2,kk)).eq.0.AND.comparereal(ycell(L,n,m),yINT(2,kk)).eq.0) then
                                     continue !I SKIP it if they are coincident with any of the 2 interface points (it happens when polygon exactly passes through one vertex). If it passes through 2 diagonals vertixes there is always a dry point. If it passes through 2 vertices sharing a edge, then porosity is zero or 1.
                                  else      
                                     NdryVERT  =  NdryVERT + 1
                                     dx = xcell(L,n,m) -  xINT(1,kk)        ! I take one of the 2 interface points and I see if the first found (it would work with any ) dry point stays  on the same side of the normal. if not, change sign.
                                     dy = ycell(L,n,m) -  yINT(1,kk) 
                                     angle = atan2(dx*Ny_pr(kk)-dy*Nx_pr(kk),dx*Nx_pr(kk)+dy*Ny_pr(kk))
                                     if (comparereal(angle,-0.5_fp*pi).ge.0.and.comparereal(angle,0.5_fp*pi).le.0) then
                                        NdryVERT_in = NdryVERT_in + 1
                                     else
                                        NdryVERT_out = NdryVERT_out + 1
                                      !  changeSIGN = .true.
                                     endif                                     
                                  endif
                               ENDIF   
                            enddo
                            if (NdryVERT_in==NdryVERT) then
                               changeSIGN  =.false.
                               KKok = kk
                               exit
                            elseif(NdryVERT_out==NdryVERT) then
                               changeSIGN = .true.
                               KKok = kk
                               exit
                            else
                               continue  ! interface containing all the dry vertices not found yet
                            endif
                        enddo
                        if (KKok==-999) then 
                           write(*,*) 'Error in intCELLS: NO interface contains all the dry points'
                           call d3stop(1, gdp)
                        endif
                        if (changeSIGN) THEN
                           Nx(n,m) = -Nx_pr(KKok)
                           Ny(n,m) = -Ny_pr(KKok)
                        else
                           Nx(n,m) = Nx_pr(KKok)
                           Ny(n,m) = Ny_pr(KKok)
                        endif
                         !compute Npsi Neta to be used in reconVOF.  recontruction of normal Npsi,Neta is skipped in reconVOF and (Npsi,Neta) are provided here (Nx, Ny) are recomputed in reconVOF
                        CALL ROTATEback(Nx(n,m),ny(n,m),PSIx(n,m),PSIy(n,m),1,Npsi(n,m),Neta(n,m))
                     endif !END IF (EXACTpolygons.or.EXACTpolygonsONLYfirst) then
                 endif
             ELSEIF (ANY(CELLinPOLY(:,n,m).eq.1)) then !DRY cell
                POROS(n,m) = 0._fp  
             ELSE ! WET CELL
                POROS(n,m) = 1._fp  
             endif
          ELSE !IF KCS=0 
             POROS(n,m)=0._fp !to avoid initialized porosity
             dpL(n,m) = dpH(n,m)
          endif
       enddo
    enddo
!
!   print verify
!
    if (idebugCUT.eq.1) THEN
       do Np=1,Npoly 
         write(charBUTTA,'(i7)') nP
         FILEtypeCELL = 'porosity_'//TRIM(ADJUSTL(charBUTTA))//'.txt'
         OPEN(20,file = FILEtypeCELL)   
         do n=2,nmaxus-1           
            write(20,'(<mmax>f25.15)') (POROS(n,m),m=2,mmax-1)
         enddo
         close(20)
       enddo
    endif
!
! CHECK THAT INPUT DEPTHS ARE CORRECT 
!    
    do m=2,mmax-1     !RESHAPE_CYCLE  1, nmax     and use kcs properly
       do n=2,nmax-1  !RESHAPE_CYCLE  1, mmaxus      
          IF ((comparereal(poros(n,m),1._fp).eq.0.or.comparereal(poros(n,m),0._fp).eq.0).and.kcs(n,m).ne.2) then
             if (comparereal(dpH(n,m),dpL(n,m)).ne.0) then
                write(*,*) 'Error in input depths: dpH and dpL not coincident.(m,n)=',m,n 
                write(*,*) '(Xcor,Ycor) upper right vertex: ',xcor0(n,m),ycor0(n,m)
                if (.NOT.IGNOREwrongDEPTH) THEN
                   call d3stop(1, gdp)
                else
                   if (comparereal(poros(n,m),1._fp).eq.0) then
                      dpH(n,m)= dpL(n,m)
                   elseif(comparereal(poros(n,m),0._fp).eq.0) then
                      dpL(n,m)= dpH(n,m)
                   endif
                endif
             endif
          else
             if (comparereal(dpH(n,m),dpL(n,m)).ge.0.and.kcs(n,m).ne.2) then ! note dph and dpl are minus the depth in the classical sense
                write(*,*) 'Error in input depths: dpH lower than or coincident with dpL in cut cell (m,n)' ,m,n
                write(*,*) '(Xcor,Ycor) upper right vertex: ',xcor0(n,m),ycor0(n,m)
                if (.NOT.IGNOREwrongDEPTH) THEN
                   call d3stop(1, gdp)
                endif
             endif
          endif
       enddo
    enddo

!
!   note: i just considered fully wet all the cell that has one side cut twice by a polygon. That
!   means that the grid resolution is too low to capture that polygon and it should be refined. 
!   A more complicate approach that takes in account multiple nodes on the same edge would be the following:
!   Store multiple points on the edges in anti-clockwise way. For each polygon 
!    do do cycle on the edges (L=1,4) and  store the polygon that 
!   indicates dry land on that cell in this way: 
!    do Np=1,Npoly
!     do m=2,mmax-1
!      do n=2,nmaxus-1
!       if (ANY(CELLinPOLY(Np,n,m).eq.2) then
!        DO L=1:4 
!           1)check the node of the cell, if its dry (i.e. contained in THAT polygon) ADD IT otherwise dont
!           2)in either case, the next point can only be a edge (xi,yi) point or a vertex point: add the edge point it if present, other wise check if the next vertex is dry and add it if it is. Otherwise keep the search.
!           3)If I just stored an edge point the next point is an edge point if the polygon segment that gives that intersection has its next vertex outside the cell. if it is outside the next point is the vertex of the polygon.
!              MAYBE EASIER: 
!           4)if I stored an edge point i do like at point 3). If I stored a polygon point i check if next polygon point is inside, if its inside I add it, otherwise its intersection with 
!             the edge
!           
!        
!        ENDDO
!       ENDIF
!      ENDDO
!     ENDDO
!    ENDDO
!
       deallocate(CELLinPOLY,NODinPOLY,vertINcellN,vertINcellM,xi,yi,TOTinterL,vect4log,interL,VERTinCELL)
       deallocate(foundINT,POROSpart,NODpolyDRY)
       deallocate(shift)
return 
end subroutine intCELLS
