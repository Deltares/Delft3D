!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2022.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id$
! $HeadURL$


module m_f1dimp_data
   !
   ! Contains variables and parameters related to flow 1d implicit module
   !
   use precision
   !use m_flowgeom, only: tnode !you cannot because <flow1d_implicit> project does not depend on <dflowfm_kernel>
   
   implicit none
   
   !public f1dimppar_type, tnode
   public f1dimppar_type
   
   !!FM1DIMP2DO: this is a copy of the <type> in <m_flowgeom>. It cannot be called the same, as both are available in <initialize_flow1d_implicit>
   !type tnode_sre                                          !< node administration
   !  integer                         :: lnx            !< max nr of links attached to this node
   !  integer, allocatable            :: ln (:)         !< linknrs attached to this node, >0: to this flownode, <0: from this flownode
   !
   !  integer, allocatable            :: nod(:)         !< Mapping to net nodes
   !  double precision, allocatable   :: x  (:)         !< for now, this is only for quick/aligned plotting, the corners of a cell
   !  double precision, allocatable   :: y  (:)         !< for now, this is only for quick/aligned plotting, the corners of a cell
   !  integer                         :: nwx            !< nr of walls attached
   !  integer, allocatable            :: nw (:)         !< wallnrs attached to this node
   !end type tnode_sre
 
   type f1dimppar_type

      ! 
      !variables in <flwpar>
      !
      
      logical                          :: lconv                  !< Flag if simulation should continue without conververgence if max.number of iterations is reached. 
      
      integer                          :: flitmx                 !< maximum number of iterationsteps
      integer                          :: iterbc                 !< Maximum iterations to be performed in BICGST
      integer                          :: juer                   !< file identifier SRE
      
      !don't use <real(fp)>. In <FLOWIT> it is defined as <real>
      real                             :: g                      !< Value for gravity acceleration
      real                             :: psi                    !< Spatial weigth factor in Preissmann scheme
      real                             :: theta                  !< Temporal weigth factor in Preissmann scheme
      real                             :: epsh                   !< Convergence criterium for water levels
      real                             :: epsq                   !< Convergence criterium for discharges(absolute)
      real                             :: rhow                   !< Density of water
      real                             :: omega                  !< Under relaxation coefficient
      real                             :: epsqrl                 !< Convergence criterium for discharges(relative)
      real                             :: lambda                 !< Extra resistance in general structure
      real                             :: relstr                 !< Under relaxation factor for structures
      real                             :: dhstru                 !< dh used for numerical differentation
      real                             :: cflpse                 !< (initial) pseudo Courant number
      real                             :: overlp                 !< sumerdike transition height
      real                             :: omcfl                  !< parameter for computing next value of dx/dt_pseudo
      real                             :: dhtyp                  !< parameter for computing next value of dx/dt_pseudo
      real                             :: exrstp                 !< Extra resistance in momentum equation
      
      double precision                 :: resid                  !< Allowable convergence measure for BICGST
      
      !*******
      !input variables to <SOFLOW>
      !*******
      
      logical                          :: steady                 !<  steady state flag
      
      integer                          :: istep                  !<  Current time step number (at t n+1 ) 
      !integer, dimension(2)            :: itim                   !<   Actual time level (at t n+1 ) expressed in date and time. Format:
      !                                                               !– itim(1) = YYYYMMDD (year,month,day)
      !                                                               !– itim(2) = HHMMSSHH (hour,minute,second, hundredth of a second)
      
      double precision                :: time                   !<  Actual time level (at t n+1 ) in seconds.
      double precision                :: dtf                    !<  Flow time step in seconds.     

      !*******
      !dimensions
      !*******
      
      integer                          :: ngrid                 !< Number of cells in network.
      integer                          :: ngridm                !< Maximum number of cells in a branch.
      integer                          :: nbran                 !< Maximum number of connected branches to one node.
      integer                          :: maxlev                !< Maximum+1 number of nlev(1:ngrid).
      integer                          :: nnode                 !< Number of nodes.
      integer                          :: nhstat                !< Number of h-boundary stations.
      integer                          :: nqstat                !< Number of q-boundary stations.
      integer                          :: ntabm                 !< Maximum size of table. 
      integer                          :: maxtab                !< Maximum number of defined tables.
      integer                          :: nbrnod                !< Maximum number of connected branches to one node.
      integer                          :: table_length          !< Number of items in each table.
      
      !*******
      !dependent on branch
      !*******
      
      integer, allocatable, dimension(:,:) :: branch              !< branch : P, double(4, nbran ). Branch information.     
                                                                  !- branch(1,i) : integer. Node number n1 at begin of branch i.
                                                                  !- branch(2,i) : integer. Node number n2 at end of branch i.
                                                                  !- branch(3,i) : integer. Grid point i1 at begin of branch i.
                                                                  !- branch(4,i) : integer. Grid point i2 at end of branch i.
      integer, allocatable, dimension(:,:) :: bfrict              !  <bfrict(3,nbran)>: P, double(3,<nbranch>): Bed friction in sections of branch:
                                                                  !	  <bfrict(1,i)>: Friction type in main section in branch i:
                                                                  !		  <bfrict(1,i)=cfrchc> (1) : Ch\'ezy constant
                                                                  !		  <bfrict(1,i)=cfrchq> (2) : Ch\'ezy function of discharge
                                                                  !		  <bfrict(1,i)=cfrchh> (3) : Ch\'ezy function of water level
                                                                  !		  <bfrict(1,i)=cfrman> (4) : Manning constant
                                                                  !		  <bfrict(1,i)=cfrskn> (5) : Strickler 1 constant ($k_n$)
                                                                  !		  <bfrict(1,i)=cfrsks> (6) : Strickler 2 constant ($k_s$)
                                                                  !		  <bfrict(1,i)=cfrnik> (7) : Nikuradze constant
                                                                  !		  <bfrict(1,i)=cfreng> (8) : Engelund predictor
                                                                  !	  <bfrict(2,i)> : Friction type in sub section 1 in branch i
                                                                  !		  <bfrict(2,i)=cfrchc>(1) : Ch\'ezy constant
                                                                  !		  <bfrict(2,i)=cfrman>(4) : Manning constant
                                                                  !		  <bfrict(2,i)=cfrskn>(5) : Strickler 1 constant (k s )
                                                                  !		  <bfrict(2,i)=cfrsks>(6) : Strickler 2 constant (k s )
                                                                  !		  <bfrict(2,i)=cfrnik>(7) : Nikuradze constant
                                                                  !	  <bfrict(3,i)> Friction type in sub section 2 in branch i.
                                                                  !		  <bfrict(3,i)=cfrchc >(1): Ch\'ezy constant.
                                                                  !		  <bfrict(3,i)=cfrinan>(4): Manning constant.
                                                                  !		  <bfrict(3,i)=cfrskn >(5): Strickler 1 constant ($k_n$)
                                                                  !		  <bfrict(3,i)=cfrsks >(6): Strickler 2 constant ($k_s$)
                                                                  !		  <bfrict(3,i)=cfrnik >(7): Nikuradze constant
      
      !*******
      !dependent on gridpoints 
      !*******
      integer, allocatable, dimension(:)               :: nlev   !< Number of h-levels for every cross-section.
      integer, allocatable, dimension(:)               :: grd_sre_fm             !< Map from gridpoint  number in SRE to flownode      number in FM.  For <grd_sre_fm(i)>  one obtains the FM  flownode     associated to SRE gridpoint <i>.
      integer, allocatable, dimension(:)               :: grd_fm_sre             !< Map from flownode   number in FM  to gridpoint     number in SRE. For <grd_fm_sre(i)>  one obtains the SRE gridpoint    associated to FM  flownode  <i>. It includes additional nodes due to multivaluedness starting from ndx.
      integer, allocatable, dimension(:)               :: grd_fm_sre2            !< Map from flownode 2 number in FM  to gridpoint     number in SRE. For <grd_fm_sre2(i)> one obtains the SRE gridpoint    associated to FM  flownode  <i>. It includes only the SRE gridpoints of junctions with only 2 branches. 
      integer, allocatable, dimension(:)               :: grd_sre_cs             !< Map from gridpoint  number in SRE to cross-section number in FM.  For <grd_sre_cs(i)>  one obtains the FM cross-section associated to SRE gridpoint <i>.
      integer, allocatable, dimension(:)               :: grd_ghost_link_closest !< Map from a link in FM to the closest link in FM in the same branch. 
      integer, allocatable, dimension(:)               :: grd_fmmv_fmsv          !< Map from a multivalued-flownode in FM to its associated single-valued flownode in FM. 
      !integer, allocatable, dimension(:)               :: kcs_sre      !< <kcs> mask of grid for SRE.
      
      integer, allocatable, dimension(:,:)             :: grd_fmL_sre  !< Conversion between internal link in FM and SRE global flownodes. For <grd_fmL_sre(i)> one obtains the two flownodes in SRE associated to <i>
      integer, allocatable, dimension(:,:)             :: grd_fmLb_sre !< Conversion between boundary link <L> in FM and SRE global flownode (in position (k,1)) and the FM cell-centre (in position (k,2)), where  <k=[lnxi+1:lnx1Db]-lnxi+1>
      
      real   , allocatable, dimension(:)               :: x      !  x-coordinate for each grid point.
      
      
      real   , allocatable, dimension(:,:)             :: bfricp ! <bfricp>: P, double(6,<ngrid>): Bed friction parameters.
                                                                 !	 <bfricp(1,i)>: Parameter for positive flow direction in main section (depending on friction type)
                                                                 !		 = Ch\'ezy constant value.
                                                                 !		 = Table pointer (Q or h table).
                                                                 !		 = Nikuradse parameter $k_n$ for Nikuradse formula.
                                                                 !		 = Manning parameter $n_m$ for Manning formula.
                                                                 !		 = Strickler coefficient $k_s$ for Strickler formula.
                                                                 !	 <bfricp(2,i)> Parameter for negative flow direction in main section (depending on friction type). Same definitions as <bfricp(l,i)>.
                                                                 !	 <bfricp(3,i)> Parameter for positive flow direction in subsec 1 (depending on friction type).
                                                                 !		 = Ch\'ezy constant value.
                                                                 !		 = Nikuradse parameter $k_n$ for Nikuradse formula.
                                                                 !		 = Manning parameter $n_m$ for Manning formula.
                                                                 !		 = Strickler coefficient $k_s$ for Strickler formula.
                                                                 !	 <bfricp(4,i)> Parameter for negative flow direction in sub sec 1 (depending on friction type). Same definition as <bfricp (3,1)>.
                                                                 !	 <bfricp(5,i)> Parameter for positive flow direction in sub sec 2 (depending on friction type). Same definition as <bfricp (3,i)>.
                                                                 !	 <bfricp(6,i)> Parameter for negative flow direction in sub sec 2 (depending on friction type). Same definition as <bfricp (3,i)>.
      
      real   , allocatable, dimension(:,:)             :: waoft ! <waoft>: P, double(<ngrid>,14): cross-sectional variables
      
      double precision, allocatable, dimension(:)      :: bedlevel  !lowest level of the cross-section
      
      double precision, allocatable, dimension(:,:)    :: hpack  !<hpack>: I, double(<ngrid,3>). Water level:            
                                                                 ! <hpack(i,1)>: at time $n$.
                                                                 ! <hpack(i,2)>: at time $*$.
                                                                 ! <hpack(i,3)>: at time $n+1$ (the one to be filled for initial condition).
      double precision, allocatable, dimension(:,:)    :: qpack  !<qpack>: I, double(<ngrid,3>). Water discharge:
                                                                 ! <qpack(i,1)>: at time $n$.
                                                                 ! <qpack(i,2)>: at time $*$.
                                                                 ! <qpack(i,3)>: at time $n+1$ (the one to be filled for initial condition).
      

      
      
      !*******
      !cross-sectional information
      !*******      
      real            , allocatable, dimension(:,:)                :: wft   ! Flow width at h = hlev (i,j) for grid-point i.
      real            , allocatable, dimension(:,:)                :: aft   ! Flow area at h = hlev (i,j) for grid-point i.
      real            , allocatable, dimension(:,:)                :: wtt   ! Total width at h = hlev (i,j) for grid-point i.
      real            , allocatable, dimension(:,:)                :: att   ! Total area at h = hlev (i,j) for grid-point i.
      real            , allocatable, dimension(:,:)                :: of    ! Actual wetted perimeter at every cross section.
      
      double precision, allocatable, dimension(:,:)                :: hlev  ! j-th water level in table for grid point i.

      !*******
      !boundary conditions
      !*******
      integer, allocatable, dimension(:,:) :: hbdpar             !hbdpar(3,nhstat)  Hydrodynamic conditions for H-stations:
                                                                 !    (1,i) = Location [grid point] for H-station.
                                                                 !    (2,i) = Type of condition
                                                                 !            cbftim (1) : h = f(t)
                                                                 !            cbfqoh (2) : h = h(Q)
                                                                 !            cbfour (3) : h = fourier
                                                                 !            cbtidl (4) : h = tidal components
                                                                 !    (3,i) = Table number for f(t), h(Q), fourier
                                                                 !            or tidal components table.
      
      integer, allocatable, dimension(:,:) :: qbdpar             !qbdpar(3,nqstat)  I  Hydrodynamic conditions for Q-stations:
                                                                 !    (1,i) = Location [grid point] for Q-station.
                                                                 !    (2,i) = Type of condition
                                                                 !            cbftim (1) : Q = f(t)
                                                                 !            cbfqoh (2) : Q = Q(h)
                                                                 !            cbfour (3) : Q = fourier
                                                                 !            cbtidl (4) : Q = tidal components
                                                                 !    (3,i) = Table number for f(t), Q(h), fourier
                                                                 !            or tidal components table.
      !*******
      !tables
      !*******
      real   , allocatable, dimension(:)      :: table           ! Contains all table values.
      
      integer, allocatable, dimension (:,:)   :: ntab            ! Table descriptor:
	                                                             ! <ntab(1,k)>: length of table $k$.
	                                                             ! <ntab(2,k)>: Start address $X$ in table table $k$.
	                                                             ! <ntab(3,k)>: Start address $Y$ in table table $k$.
	                                                             ! <ntab(4,k)>: Access method and period control: $xy$
	                                                             ! 	x = ctbnpf> (0) : No period defined
	                                                             ! 	x = ctbpfu> (1) : Period defined
	                                                             ! 	y = ctbico> (0) : Continue interpolation
	                                                             ! 	y = ctbidi> (1) : Discrete
      !*******
      !dependent on nodes
      !*******
      integer, allocatable, dimension(:)     :: numnod           ! Indicates the maximum number of nodes connected with node i 
      
      integer, allocatable, dimension(:,:)   :: node             ! Definition of nodes.
                                                                 !<node(1,i)>: I, integer. Type of node.
                                                                 !	<cintnd>(1) : Internal node.
                                                                 !	<chbou >(2) : H-boundary.
                                                                 !	<cqbou >(3) : Q-boundary.
                                                                 !	<cqhbou>(4) : QH-boundary.
                                                                 !	<chqbou>(5) : HQ-boundary.
                                                                 !<node(2,i)>: I, integer. Gridpoint in case of boundary, else undefined
                                                                 !<node(3,i)>: I, integer. Station number for boundary, undefined for internal nodes.
                                                                 !<node(4,i)>: I, integer. Boundary number in case of boundary.
      
      integer, allocatable, dimension(:,:)   :: nodnod           ! Administration of nodal adminstration matrix. All elements (i,j) with index i contain the numbers of the nodes connected to node i, node i included.
      
      !*******
      !debug variables
      !*******
      integer :: fm1dimp_debug_k1 
      integer :: debug_wr 

      !*******
      !stucture variable
      !*******
      !type(tnode_sre), allocatable :: nd_sre(:)
      

      
   end type f1dimppar_type
   
   !type nd_type
   !    integer, allocatable, dimension(:) :: ln
   !end type nd_type
      
end module m_f1dimp_data