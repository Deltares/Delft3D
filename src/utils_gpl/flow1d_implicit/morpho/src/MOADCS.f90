subroutine moadcs ( igp,     isec,   ibrtyp,&
&deltaa,  time,   moropt,&
&nboun,   ngrid,  nnode,&
&branch,  ibr,    node,   mbdpar,&
&maxtab,  ntabm,  ntab,   table,&
&maxlev,  nlev,   hlev,&
&wft,    ws,&
&flwdir&
&)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOADCS (MORPHology Adapt Cross Sections)
!
! Module description: Adapt cross section dimensions for a cross secti-
!                     on. The following cross section types are possi-
!                     ble:
!
!                     o     sedredge cross sections;
!                     o     tabulated cross sections.
!
!                     Cross sections of type circle can not be proces-
!                     sed.
!
!                     For tabulated cross sections this routine will
!                     call a cross section adaption routine depending on
!                     the users choice. Two choices are possible for
!                     tabulated cross sections:
!
!                     o   erosion/sedimentation spread equally over
!                         transport width;
!                     o   option = erosion/sedimentation proportional to
!                         local depth.
!
!                     If a grid point is a boundary and the boundary
!                     condition is a bed level and sediment is moving in
!                     no delta z is calculated. In that case the bounda-
!                     ry condition is used to adapt the cross section.
!                     In all other cases the calculated delta A is used
!                     to calculate a delta z. For sedredge cross secti-
!                     ons this routine will calculate a delta z and
!                     update the bottom of the section involved. In case
!                     the grid point is a boundary and the boundary
!                     conditions is a bed level and sediment is moving
!                     on no delta z is calculated. The new bottom is
!                     interpolated from the boundary conditions table
!                     and will be assigned to the section involved.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 11 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  4 deltaa            I  Calculated change in area
! 20 hlev(ngrid,       IO (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  3 ibrtyp            I  Type of branch
!                           ccrtab (1) : tabulated branch
!                           ccrcir (2) : circle branch
!                           ccrsed (3) : sedredge branch
!  1 igp               I  Gridpoint number
!  2 isec              I  Section number (1 or 2)
! 18 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 14 maxtab            I  Maximum number of defined tables.
! 13 mbdpar(5,nboun)   I  Morphodynamic boundary conditions:
!                         (1,i) = Type of boundary condition:
!                                 cmbsft (1) : Condition S=f(t).
!                                 cmbsfq (2) : Condition S=f(Q).
!                                 cmbzft (3) : Condition z=f(t).
!                         (2,i) = Location (node number).
!                         (3,i) = Branch number that is connected.
!                         (4,i) = Table pointer for boundary table. In
!                                 case of a connected sedredge branch
!                                 the pointer will be assigned to the
!                                 left channel.
!                         (5,i) = Table pointer for boundary table. In
!                                 case of a connected sedredge branch
!                                 the pointer will be assigned to the
!                                 right channel. In other cases undefi-
!                                 ned.
!  6 moropt            I  Method of adapting cross sections
!                         ceqows (1) : Equally over the actual sediment
!                                      transp. width of cross section
!                         cprodp (2) : Proportional to the local water
!                                      depth across the cross section
!  8 nboun             I  Number of boundary nodes.
!  9 ngrid             I  Number of grid points in network.
! 19 nlev              P  -
! 10 nnode             I  Number of nodes.
! 12 node(4,nnode)     I  Definition of nodes:
!                         (1,i) = Type of node i:
!                                 cintnd (1) : Internal node
!                                 chbou  (2) : H-boundary
!                                 cqbou  (3) : Q-boundary
!                                 cqhbou (4) : QH-boundary
!                                 chqbou (5) : HQ-boundary
!                         (2,i) = Gridpoint in case of boundary, else
!                                 undefined.
!                         (3,i) = Station number for boundary, undefined
!                                 for internal nodes:
!                                 HQ, H-boundary: station nr H-station.
!                                 QH, Q-boundary: station nr Q-station.
!                         (4,i) = Boundary number in case of boundary.
! 16 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
!                         to maxtab. For a specific table number k and
!                         function Y = f (X) the following definitions
!                         exist:
!                         (1,k) = Length of table k.
!                         (2,k) = Start address X in table.
!                         (3,k) = Start address Y in table.
!                         (4,k) = Access method and period control: xy
!                                 x = ctbnpf (0) : No period defined
!                                 x = ctbpfu (1) : Period defined
!                                 y = ctbico (0) : Continue interpltn
!                                 y = ctbidi (1) : Discrete interpltn
! 15 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 24 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
!               1|2       (At first transports per unit width, finaly
!                         total transports)
!                         - Normal branches:
!                         (i,1) = Sediment transport in gridpoint i of
!                                 main section.
!                         - Sedredge branches:
!                         (i,1) = Sediment transport in gridpoint i,
!                                 left channel.
!                         (i,2) = Sediment transport in gridpoint i,
!                                 right channel.
!                         (i,3) = Sediment exchange in gridpoint i.
! 17 table             P  -
!  5 time              P  -
! 22 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
!                                 point i.
!                         - For a circle cross section:
!                         (i,1) = Radius of the circle.
!                         - For a sedredge cross section:
!                         (i,1) = Width of main section (i.e. left chan-
!                                 nel).
!                         (i,2) = Width of sub section 1 (i.e. right
!                                 channel).
! 23 ws                P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! inttab  INTerpolate in TABle
! moeqdz  MORPHology EQually over Transport Width Delta Z calculation
! moeqtw  MORPHology erosion/sedimentation spread EQually over Transport Width
! momlev  MORPHology Morphodynamic LEVel
! moprdz  MORPHology PRoportional Delta Z calculation
! moprld  MORPHology erosion/sedimentation PRoportional to Local Depth
! moseci  MOrphology SECtion I
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: moadcs.pf,v $
! Revision 1.7  1999/03/15  15:52:44  kuipe_j
! tabs removed
!
! Revision 1.6  1998/06/11  11:47:05  kuipe_j
! Estuary special integrated
!
! Revision 1.5  1997/06/17  11:18:27  kuipe_j
! Remove undefined vars
!
! Revision 1.4  1996/09/03  14:48:46  kuipe_j
! frequency time hist,Improved sed distribution at nodes
!
! Revision 1.3  1995/05/30  09:55:45  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:04:31  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:01  hoeks_a
! Initial check-in
!
! Revision 1.4  1994/12/02  13:27:18  kuipe_j
! At a boundary always 'equal' distribution.
!
! Revision 1.3  1994/11/28  08:52:18  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:32:24  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:05  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer   igp, ibr ,&
   &ibrtyp,&
   &isec,&
   &maxlev,&
   &maxtab,&
   &moropt,&
   &ntabm,&
   &nboun,&
   &ngrid,&
   &nnode
! Aanpassing Kees Sloff 8-8-1997: branch dimensies
   integer   branch (4,ibr),&
   &mbdpar (5,nboun),&
   &nlev   (ngrid),&
   &ntab   (4,maxtab),&
   &node   (4,nnode),&
   &flwdir (ngrid)

   real      ws(ngrid)

   real      table  (ntabm),&
   &wft    (ngrid,maxlev)

   double precision    time, hlev   (ngrid,maxlev),&
   &deltaa
!
!     Local variables
!
   integer   i1,  i2,  iboun,  itab,  k,   n1,  n2
!
   real      z, wsact
!
   double precision hws, deltaz
!
   logical   entbou
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Read parameters from branch
!
   n1 = branch(1,ibr)
   n2 = branch(2,ibr)
   i1 = branch(3,ibr)
   i2 = branch(4,ibr)
!
!     Check if grid point is entering boundary:
!
!     This is the case if sediment is moving in at a node and the
!     boundary condition is given as a delta Z.
!
   if     (igp .eq. i1) then
!
!        Begin of branch (S > 0: entering)
!
      if (flwdir(igp) .gt. 0 ) then
!
!           Fetch boundary number if not internal node
!
         if (node (1,n1) .ne. cintnd) then
            iboun = node (4,n1)
!
!              Read morphodynamic boundary condition table number itab
!
            if (mbdpar(1,iboun) .eq. cmbzft) then
!
!                 z = f(t)
!
               entbou = .true.
!
!                 Boundary table number (index 4 or 5)
!
               itab = mbdpar(isec+3,iboun)
            else
               entbou = .false.
            endif
         else
            entbou = .false.
         endif
      else
         entbou = .false.
      endif

   elseif (igp .eq. i2) then
!
!        End of branch (S < 0: entering boundary)
!
      if (flwdir(igp) .lt. 0 ) then
!
!           Fetch boundary number if not internal node
!
         if (node(1,n2) .ne. cintnd) then
            iboun = node (4,n2)
!
!              Read morphodynamic boundary condition table number itab
!
            if (mbdpar(1,iboun) .eq. cmbzft) then
!
!                 z = f(t)
!
               entbou = .true.
!
!                 Boundary table number (index 4 or 5)
!
               itab = mbdpar(isec+3,iboun)
            else
               entbou = .false.
            endif
         else
            entbou = .false.
         endif
      else
         entbou = .false.
      endif

   else
!
!        Not a boundary gridpoint
!
      entbou = .false.
   endif


   if (ibrtyp .eq. ccrsed) then
!
!        Sedredge branch is processed differently
!
      if (.not. entbou) then
!
!           Calculate delta z for current section (left or right)
!
         deltaz = deltaa / wft(igp,isec)
!
!           Update bottom of section
!
         hlev(igp,isec) = hlev(igp,isec) - deltaz

      else
!
!           Determine Z from boundary condition z = f(t)
!
         call inttab ( int (ntab(1,itab)),&
         &int (ntab(4,itab)),&
         &table (int (ntab(2,itab))),&
         &table (int (ntab(3,itab))),&
         &time,&
         &z&
         &)
!
!           Store new bottom level
!
         hlev(igp,isec) = z

      endif
   else
!
!        Code for tabulated cross sections
!

!
!        Determine maximum level k in cross sectional table
!
      call momlev ( igp,   ngrid,  maxlev,&
      &nlev,  wft,    ws,&
      &k,     wsact&
      &)
!
!        Check for morphology option (equally or proportional)
!
      if (moropt .eq. ceqows) then
!
!           Erosion/sedimentation spread equally over transport width
!
         if (.not. entbou) then
!
!              Determine delta Z by calculation
!
            call moeqdz ( igp,    k,    ngrid,   maxlev,&
            &nlev,   wft,  deltaa,  deltaz&
            &)

         else
!
!              Determine delta Z from boundary condition z = f(t)
!
            call inttab ( int (ntab(1,itab)),&
            &int (ntab(4,itab)),&
            &table (int (ntab(2,itab))),&
            &table (int (ntab(3,itab))),&
            &time,&
            &z&
            &)
            deltaz = hlev(igp,1) - z
         endif
!
!           Adapt cross sectional table
!
         call moeqtw ( igp,   k,      ngrid,    maxlev,&
         &hlev,  deltaz&
         &)

      elseif (moropt .eq. cprodp) then
!
!           Erosion/sedimentation proportional to local depth
!
!           Calculate level of sediment transporting width
!
         call moseci ( ngrid  ,igp    ,maxlev ,nlev   ,&
         &wft    ,hlev   ,wsact  ,hws    )
!
         if (.not. entbou) then
!
!              Determine delta Z by calculation
!
            call moprdz ( igp,     hws,    k,     ngrid,&
            &maxlev,  nlev,   hlev,  wft,&
            &deltaa,  deltaz&
            &)

!
!              Adapt cross sectional table
!
            call moprld ( igp,     hws,    k,      ngrid,&
            &maxlev,  hlev,   deltaz&
            &)
         else
!
!              Determine delta Z from boundary condition z = f(t)
!
            call inttab ( int (ntab(1,itab)),&
            &int (ntab(4,itab)),&
            &table (int (ntab(2,itab))),&
            &table (int (ntab(3,itab))),&
            &time,&
            &z&
            &)
            deltaz = hlev(igp,1) - z
!
!              Adapt cross sectional table
!
            call moeqtw ( igp,   k,      ngrid,    maxlev,&
            &hlev,  deltaz&
            &)
         endif
      endif
   endif
   return
end
