subroutine FLTS (il     ,ir     ,iter   ,ngrid  ,istru  ,&
&nstru  ,relstr ,strpar ,stdbq  ,nstdb1 ,&
&h      ,h1     ,q      ,q1     ,strhis ,&
&asde   ,bsde   ,csde   ,dsde   ,esde   ,&
&juer   ,ker    )
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Kuipers/H.Petit
!
! Module:             FLTS (FLow Tabulated Structure)
!
! Module description: In this subroutine the ABCDE coefficients are
!                     computed for a structure for which the
!                     Q-h relation is provided in the form of a table.
!
!                     In this subroutine the coefficients will be
!                     determined for the stage-discharge equation for
!                     this specific structure. In the subroutine FLQTS
!                     the Q-h relation and derivatives to h
!                     (left and right) are determined
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME            IO DESCRIPTION
! asde                O a-coefficient in the stage-discharge equation
!                       for the structure
! bsde                O b-coefficient in the stage-discharge equation
!                       for the structure
! csde                O c-coefficient in the stage-discharge equation
!                       for the structure
! dsde                O d-coefficient in the stage-discharge equation
!                       for the structure
! esde                O e-coefficient in the stage-discharge equation
!                       for the structure
! h                   I array containing water level at all grid points
!                       at latest iteration
! h1                  I array containing water level at all grid points
!                       at time t(n)
! il                  I number of gridpoint at the left  of the structure
! ir                  I number of gridpoint at the right of the structure
! istru               I structure number
! iter                I iteration step number
! ngrid               I number of grid points in the network
! nstdb1              I number of elements of data base for this
!                       structure
! nstru               I number of structures
! q                   I array containing discharge at all grid points
!                       at latest iteration
! q1                  I array containing discharge at all grid points
!                       at time t(n)
! relstr              I Under relaxation factor for structures.
! stdbq              IO Table containing discrete Q-h relation, hl values
!                       and hr (or hl-hr) values at initial input.
!                       s(i,-1)=hl(i)                         for i=0(1)m
!                       s(-1,j)=(hl-hr)(j)  or
!                       s(-1,j)=hr(j)                         for j=0(1)n
!                       At output and following calls:
!                       (1)      dummy / gate value
!                       (2..)    hl(i) for i=0(1)m
!                       (2+m..)  hr(j) or (hl-hr)(j) for i=0(1)m
!                       (2+m+n..)Q(i,j for i=0(1)m and i=0(1)m
! strhis(??,nstru)   IO For each structure the discharge and the
!                       parameters to be controlled must be saved to
!                       be able to write to the output file. This will
!                       be done in array strhis(??,nstru).
!                       (4,i) = Discharge through structure
! strpar(21,nstru)   IO Parameters for data base structure:
!                       (1,i) interpolation:
!                             1 = linear
!                             2 = spline
!                       (2,i) sequence number of data base structure
!                             (1,2,…), i.e. data base structure number
!                       (3,i) number of dimensions of data base (2 or 3)
!                       (4,i) number of H1-values nh1
!                       (5,i) number of H2-values nh2
!                       (6,i) number of values of gate height, crest
!                             level or width
!                             (extension in future; 1 in Sobek 2.51)
!                       (7,i) dummy
!                       (8,i) dummy
!                       (9,i) index of first element in array with
!                             discharges stdbq for this structure
!                       (10,i) type of values of first row in stdbq for
!                             this structure:
!                             1 : H2-values
!                             2 : dH-values (dH=H2-H1)
!                       (11,i) number of structure where the data base
!                             of this structure is defined. This is
!                             the structure with the first occurrence
!                             of a link to this data base table.
!                       (12,i) indicator that identifies if this data
!                             base table is initialised:
!                             0 : no
!                             1 : yes
!                       (13,i) level of sill of structure or reference
!                             level
!                       (14,i) indicator that defines if discharge is zero
!                             if sill is dry (0), otherwise 1.
!                       (15,i) minimum H1 (wrt sill) in case of underflow
!                             (otherwise 1E20)
!                       (16,i) minimum H2/Dh in case of underflow
!                             (otherwise 1E20)
!                       (17,i) maximum H1 (wrt sill) in case of overflow
!                             (otherwise -1E20)
!                       (18,i) maximum H2/DH in case of overflow
!                             (otherwise -1E20)
!                       (19,i) 4-digit indicater for under or overflow in
!                             an iteration step.
!                             From right to left a digit corresponds for:
!                             H1-underflow, H2/DH-underflow,
!                             H1-overflow, H2/DH-overflow.
!                             A digit can be 0 (inside) or 1 (outside
!                             data base).
!-----------------------------------------------------------------------
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters:
!
   integer il, ir, istru, ngrid, iter, nstru, nstdb1,juer  ,ker
   real    strpar(dmstrpar,nstru), strhis(dmstrh,nstru)
   real    stdbq(nstdb1)
   real    asde, bsde, csde, dsde, esde, relstr
   double precision h(ngrid), h1(ngrid), q(ngrid),q1(ngrid)
!
!     Declaration of local variables:
!
   integer n     ,m    ,xnod  ,ynod ,s    ,inttyp
   real    qa    ,qdh1 ,qdh2  ,hl   ,hr   ,hunp1  ,hun   ,&
   &hdnp1 ,hdn  ,qunp1 ,qun  ,zs
   logical deltah,zero
!
   integer     qol
   parameter  (qol=4)
!
   call fltspa(strpar ,istru  ,nstru  ,stdbq ,nstdb1 ,s     ,&
   &xnod   ,ynod   ,n      ,m     ,deltah ,inttyp,&
   &zs     ,zero   ,juer   ,ker   )
!
   hl = sngl( h(il) )
   hr = sngl( h(ir) )
!
   call flqht(hl   ,hr   ,qa   ,qdh1 ,qdh2  ,stdbq(s) ,stdbq(xnod) ,&
   &stdbq(ynod),n    ,m    ,deltah,inttyp   ,zs    ,zero ,&
   &strpar(19,istru) )
!
!       WRITE (*,*) iter,hl,hr,qa

!
   asde = qdh1
   bsde = 0.0
   csde = qdh2
   dsde = 0.0
!
!     Underrelaxation
!
!     WRITE (*,*) qa,asde,csde
   if(iter.gt.1)then
      qa = relstr*qa+(1.0-relstr)*strhis(qol,istru)
   endif
!
   strhis(qol,istru) = qa
!
   hunp1 = sngl ( h (il) )
   hun   = sngl ( h1(il) )
   hdnp1 = sngl ( h (ir) )
   hdn   = sngl ( h1(ir) )
   qunp1 = sngl ( q (il) )
   qun   = sngl ( q1(il) )
   esde  = -qa+asde*(hunp1-hun)+csde*(hdnp1-hdn)+&
   &(qunp1-qun)*(bsde+dsde)
!
   return
end
