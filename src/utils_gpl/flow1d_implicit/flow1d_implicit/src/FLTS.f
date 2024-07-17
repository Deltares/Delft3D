      subroutine FLTS (il     ,ir     ,iter   ,ngrid  ,istru  ,
     +                 nstru  ,relstr ,strpar ,stdbq  ,nstdb1 ,
     +                 h      ,h1     ,q      ,q1     ,strhis ,
     +                 asde   ,bsde   ,csde   ,dsde   ,esde   ,
     +                 juer   ,ker    )
c      
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Kuipers/H.Petit
c
c Module:             FLTS (FLow Tabulated Structure)
c
c Module description: In this subroutine the ABCDE coefficients are 
c                     computed for a structure for which the 
c                     Q-h relation is provided in the form of a table.
c
c                     In this subroutine the coefficients will be 
c                     determined for the stage-discharge equation for
c                     this specific structure. In the subroutine FLQTS
c                     the Q-h relation and derivatives to h 
c                     (left and right) are determined 
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME            IO DESCRIPTION
c asde                O a-coefficient in the stage-discharge equation
c                       for the structure
c bsde                O b-coefficient in the stage-discharge equation
c                       for the structure
c csde                O c-coefficient in the stage-discharge equation
c                       for the structure
c dsde                O d-coefficient in the stage-discharge equation
c                       for the structure
c esde                O e-coefficient in the stage-discharge equation
c                       for the structure
c h                   I array containing water level at all grid points 
c                       at latest iteration
c h1                  I array containing water level at all grid points
c                       at time t(n)
c il                  I number of gridpoint at the left  of the structure
c ir                  I number of gridpoint at the right of the structure
c istru               I structure number
c iter                I iteration step number
c ngrid               I number of grid points in the network
c nstdb1              I number of elements of data base for this
c                       structure
c nstru               I number of structures
c q                   I array containing discharge at all grid points
c                       at latest iteration
c q1                  I array containing discharge at all grid points
c                       at time t(n)
c relstr              I Under relaxation factor for structures.
c stdbq              IO Table containing discrete Q-h relation, hl values
c                       and hr (or hl-hr) values at initial input.
c                       s(i,-1)=hl(i)                         for i=0(1)m
c                       s(-1,j)=(hl-hr)(j)  or
c                       s(-1,j)=hr(j)                         for j=0(1)n
c                       At output and following calls:
c                       (1)      dummy / gate value
c                       (2..)    hl(i) for i=0(1)m
c                       (2+m..)  hr(j) or (hl-hr)(j) for i=0(1)m
c                       (2+m+n..)Q(i,j for i=0(1)m and i=0(1)m
c strhis(??,nstru)   IO For each structure the discharge and the
c                       parameters to be controlled must be saved to
c                       be able to write to the output file. This will
c                       be done in array strhis(??,nstru).
c                       (4,i) = Discharge through structure
c strpar(21,nstru)   IO Parameters for data base structure:
c                       (1,i) interpolation:
c                             1 = linear
c                             2 = spline
c                       (2,i) sequence number of data base structure 
c                             (1,2,…), i.e. data base structure number
c                       (3,i) number of dimensions of data base (2 or 3)
c                       (4,i) number of H1-values nh1
c                       (5,i) number of H2-values nh2
c                       (6,i) number of values of gate height, crest 
c                             level or width
c                             (extension in future; 1 in Sobek 2.51)
c                       (7,i) dummy
c                       (8,i) dummy
c                       (9,i) index of first element in array with 
c                             discharges stdbq for this structure
c                       (10,i) type of values of first row in stdbq for 
c                             this structure:
c                             1 : H2-values
c                             2 : dH-values (dH=H2-H1)
c                       (11,i) number of structure where the data base 
c                             of this structure is defined. This is
c                             the structure with the first occurrence 
c                             of a link to this data base table.
c                       (12,i) indicator that identifies if this data 
c                             base table is initialised:
c                             0 : no
c                             1 : yes
c                       (13,i) level of sill of structure or reference
c                             level
c                       (14,i) indicator that defines if discharge is zero
c                             if sill is dry (0), otherwise 1.
c                       (15,i) minimum H1 (wrt sill) in case of underflow 
c                             (otherwise 1E20)  
c                       (16,i) minimum H2/Dh in case of underflow 
c                             (otherwise 1E20)  
c                       (17,i) maximum H1 (wrt sill) in case of overflow 
c                             (otherwise -1E20)  
c                       (18,i) maximum H2/DH in case of overflow
c                             (otherwise -1E20)
c                       (19,i) 4-digit indicater for under or overflow in
c                             an iteration step.
c                             From right to left a digit corresponds for:
c                             H1-underflow, H2/DH-underflow,
c                             H1-overflow, H2/DH-overflow.
c                             A digit can be 0 (inside) or 1 (outside 
c                             data base).
c-----------------------------------------------------------------------
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Declaration of parameters:
c
      integer il, ir, istru, ngrid, iter, nstru, nstdb1,juer  ,ker
      real    strpar(dmstrpar,nstru), strhis(dmstrh,nstru)
      real    stdbq(nstdb1) 
      real    asde, bsde, csde, dsde, esde, relstr 
      double precision h(ngrid), h1(ngrid), q(ngrid),q1(ngrid)
c
c     Declaration of local variables:
c      
      integer n     ,m    ,xnod  ,ynod ,s    ,inttyp
      real    qa    ,qdh1 ,qdh2  ,hl   ,hr   ,hunp1  ,hun   ,
     &        hdnp1 ,hdn  ,qunp1 ,qun  ,zs
      logical deltah,zero
c
      integer     qol
      parameter  (qol=4)
c      
      call fltspa(strpar ,istru  ,nstru  ,stdbq ,nstdb1 ,s     ,
     &            xnod   ,ynod   ,n      ,m     ,deltah ,inttyp,
     &            zs     ,zero   ,juer   ,ker   )
c      
      hl = sngl( h(il) )
      hr = sngl( h(ir) )
c      
      call flqht(hl   ,hr   ,qa   ,qdh1 ,qdh2  ,stdbq(s) ,stdbq(xnod) ,
     +           stdbq(ynod),n    ,m    ,deltah,inttyp   ,zs    ,zero ,
     +           strpar(19,istru) )
c    
c       WRITE (*,*) iter,hl,hr,qa

c      
      asde = qdh1
      bsde = 0.0
      csde = qdh2
      dsde = 0.0
c
c     Underrelaxation
c
c     WRITE (*,*) qa,asde,csde
      if(iter.gt.1)then
        qa = relstr*qa+(1.0-relstr)*strhis(qol,istru)
      endif
c
      strhis(qol,istru) = qa
c
      hunp1 = sngl ( h (il) )
      hun   = sngl ( h1(il) )
      hdnp1 = sngl ( h (ir) )
      hdn   = sngl ( h1(ir) )
      qunp1 = sngl ( q (il) )
      qun   = sngl ( q1(il) )
      esde  = -qa+asde*(hunp1-hun)+csde*(hdnp1-hdn)+
     &        (qunp1-qun)*(bsde+dsde)
c
      return
      end
