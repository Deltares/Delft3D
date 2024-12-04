subroutine FLHTRG(time   ,itrig  ,strtyp ,triger ,maxtab ,ntabm  ,&
&ntab   ,table  ,ngrid  ,h      ,q      ,strhis ,&
&hydtrg ,nqlat  ,qltpar ,strpar ,g      ,rho    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLHTRG (FLow Hydraulic TRiGger)
!
! Module description: Determine if hydraulic trigger is active.
!
!                     To determine if a hydraulic trigger is active
!                     first the hydraulic parameter will be calculated
!                     or the structure parameter will be retrieved.
!                     After this a check is made if the computed value
!                     exceeds a defined value.
!                     Various hydraulic triggers are possible:
!                     - water level at a certain location;
!                     - head difference over a structure;
!                     - discharge at a certain location;
!                     - value of a controlled parameter of a structure;
!                     - opening or closing of structure in progress.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 10 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
! 13 hydtrg            O  Logical indicator,
!                         = TRUE, exceeding/underexceeding of the
!                         selected hydraulic parameter at a certain
!                         location.
!  2 itrig             I  Trigger number.
!  5 maxtab            I  Maximum number of defined tables.
!  9 ngrid             I  Number of grid points in network.
!  7 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
!  6 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 11 q(ngrid)          I  Discharge in every grid point at the latest
!                         iteration.
! 12 strhis(10,nstru)  I  For each structure the discharge and the
!                         parameters to be controlled must be saved to
!                         be able to write to the output file. This will
!                         be done in array strhis(8,nstru). This array
!                         will also be used to check the values of the
!                         controlled parameters or to determine if
!                         increase(open) or decrease(close) of these
!                         parameters occurs. This array will also be
!                         part of the restart file.
!                         (1,i) = Gate height
!                         (2,i) = Crest height
!                         (3,i) = Crest width
!                         (4,i) = Discharge through structure
!                         (5,i) = Gate height at previous time step
!                         (6,i) = Crest height at previous time step
!                         (7,i) = Crest width at previous time step
!                         (8,i) = Flow condition of general structure:
!                                 formno = 0, closed or other structure
!                                 formno = 1, free weir
!                                 formno = 2, drowned weir
!                                 formno = 3, free gate
!                                 formno = 4, drowned gate
!                         (9,i) = coefficient Q-H-realtion asde
!                         (10,i)= coefficient Q-H-realtion bsde
!                         (11,i)= coefficient Q-H-realtion csde
!                         (12,i)= coefficient Q-H-realtion dsde
!  3 strtyp(10,nstru)  I  Structure definitions:
!                         (1,i) = Type of structure:
!                                 csweir (1) : Simple weir
!                                 caweir (2) : Advanced weir
!                                 csgate (3) : - not used, reserved for
!                                                simple gate -
!                                 cpump  (4) : Pump
!                                 cgenst (5) : General Structure
!                         (2,i) = Position of structure:
!                                 cstbra (1) : In branch
!                                 cstlat (2) : Lateral structure
!                         (3,i) = Left gridpoint.
!                         (4,i) = Right gridpoint.
!                         (5,i) = dummy
!                         (6,i) = dummy
!                         (7,i) = dummy
!                         (8,i) = dummy
!                         (9,i) = dummy
!                         (10,i)= dummy
!  8 table             P  -
!  1 time              P  -
!  4 triger(10,ntrigr) I  Array containing parameters for each trigger
!                         or trigger combination.
!                         (1,i) = Output of trigger (combination):
!                                 1 : True
!                                 0 : False
!                         (2,i) = dummy
!                         (3,i) = dummy
!                         (4,i) = dummy
!                         (5,i) = dummy
!                         (6,i) = trigger parameter
!                                 cnotrg (0) : No trigger defined
!                                 ctitrg (1) : Time trigger used
!                                 chytrg (2) : Hydraulic trigger used
!                                 candor (3) : Time & hydraulic trigger
!                                              used (and/or mode)
!                         (7,i) = table number of time trigger
!                         (8,i) = table number 1 for hydraulic trigger
!                         (9,i) = table number 2 for hydraulic trigger
!                         (10,i)= table number of and/or period
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! inttab  INTerpolate in TABle
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flhtrg.pf,v $
! Revision 1.7  1999/03/15  15:49:57  kuipe_j
! tabs removed
!
! Revision 1.6  1996/01/17  14:38:27  kuipe_j
! header update
!
! Revision 1.5  1995/09/22  10:01:29  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/08/30  12:36:36  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:55:02  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:00  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:45  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:37:27  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:30:53  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:49  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters:
!
   logical hydtrg
   integer itrig, maxtab, ntabm, ngrid, nqlat
   integer strtyp(10,*), triger(10,*), ntab(4,maxtab)
   real    g
   real    table(ntabm), strhis(dmstrh,*),&
   &qltpar(9,nqlat), rho(ngrid), strpar(dmstrpar,*)
   double  precision  time, h(ngrid), q(ngrid)
!
!     Declaration of local variables:
!
   integer    get   ,icpnum     ,juerd
   parameter (get=2 ,icpnum = 1 ,juerd = -1)
   integer    il, ir, itab, trgtyp, locat, istru, indx, istat,&
   &kerd, isttyp
   real       hdiff, value, x, conpar, direct, zs
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     For an hydraulic trigger the controller will be switched ON in
!     case of exceedance c.q. underexceedance of a selected hydraulic
!     parameter at a certain location. Otherwise the trigger will be OFF
!     This type of trigger will be stored in two time tables.
!
!     Table 1 contains: type of trigger, location and type of check.
!     itab   : table number
!     trgtyp : ctrgh   - water level
!              ctrghd  - head difference
!              ctrgdi  - discharge
!              ctrggh  - gate height
!              ctrgch  - crest height
!              ctrgcw  - crest width
!     locat  : gridpoint (type ctrgh)
!              structure number (type ctrghd)
!     locat <0 check for <x ; locat >0 check for >x ,
!              or indicator for opening/closing structure
!
!     x      : check value for the hydraulic trigger, or
!              open/close indicator
!
   itab = triger(8,itrig)
   call INTTAB (ntab(1,itab), ntab(4,itab),&
   &table(ntab(2,itab)),&
   &table(ntab(3,itab)),&
   &time, value        )
   trgtyp = int(abs(mod(value,10.)))
   locat  = int(value/10.)
!
!     Table 2 contains check values for the hydraulic trigger
!
   itab = triger(9,itrig)
   call INTTAB (ntab(1,itab), ntab(4,itab),&
   &table(ntab(2,itab)),&
   &table(ntab(3,itab)),&
   &time, x            )
!
   indx   = 0
   hydtrg = .false.
!
   if ( trgtyp .eq. ctrgh ) then
!
!        Controlled by water level
!
      if ( locat .lt. 0 ) then
         hydtrg = h(abs(locat)) .lt. x
      else
         hydtrg = h(locat) .gt. x
      endif
!
   else if ( trgtyp .eq. ctrghd ) then
!
!        Controlled by head difference
!
      istru = abs(locat)
      il    = strtyp(3,istru)
      ir    = strtyp(4,istru)
      hdiff = h(il) - h(ir)
      if ( locat .lt. 0 ) then
         hydtrg = hdiff .lt. x
      else
         hydtrg = hdiff .gt. x
      endif

   else if ( trgtyp .eq. ctrgpd ) then
!
!        Controlled by pressure difference
!
!        - Pressure difference from structure name -
!
      istru  = abs(locat)
      il     = strtyp(3,istru)
      ir     = strtyp(4,istru)
      isttyp = strtyp(1,istru)
      call FLCNPA(get    ,isttyp ,istru ,icpnum, zs ,&
      &strpar ,juerd  ,kerd  )
!
      hdiff = max((h(il) - zs),0.)**2 * rho(il)*g/2.0 -&
      &max((h(ir) - zs),0.)**2 * rho(ir)*g/2.0
!
      if ( locat .lt. 0 ) then
         hydtrg = hdiff .lt. x
      else
         hydtrg = hdiff .gt. x
      endif
!
   else if ( trgtyp .eq. ctrgdi ) then
!
!        Controlled by discharge
!
      if ( locat .lt. 0 ) then
         hydtrg = q(abs(locat)) .lt. x
      else
         hydtrg = q(locat) .gt. x
      endif
!
   else if ( trgtyp .eq. ctrggh ) then
      indx = 1
!
   else if ( trgtyp .eq. ctrgch ) then
      indx = 2
!
   else if ( trgtyp .eq. ctrgcw ) then
      indx = 3
   else if ( trgtyp .eq. ctrghlat ) then
      istat = abs(locat)
      if (INT(qltpar(2,istat)).eq.cqlret) then
         istru = MOD(INT(qltpar(9,istat)), 1000)
         if ( locat .lt. 0 ) then
            hydtrg = strhis(13,istru) .lt. x
         else
            hydtrg = strhis(13,istru) .gt. x
         endif
      endif
   endif
!
!     Trigger of controlled structure parameter?
!
   if ( indx .gt. 0 ) then
      istru = abs(locat)
!jk 6-5-2000 test improved ARS 5233
      if ( nint(x) .gt. -999 ) then
!           - check value of controlled parameter
         conpar = strhis(indx,istru)
         if ( locat .gt. 0 ) then
            hydtrg = conpar .gt. x
         else
            hydtrg = conpar .lt. x
         endif
      else
!           - check if structure is opening or closing
         direct = strhis(indx,istru) - strhis(indx+4,istru)
         if ( locat .gt. 0 ) then
!              - opening in progress ?
            hydtrg = direct .gt. 0.
         else
!              - closing in progress ?
            hydtrg = direct .lt. 0.
         endif
      endif
   endif
!
end
