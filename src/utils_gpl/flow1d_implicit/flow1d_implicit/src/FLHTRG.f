      subroutine FLHTRG(time   ,itrig  ,strtyp ,triger ,maxtab ,ntabm  ,
     +                  ntab   ,table  ,ngrid  ,h      ,q      ,strhis ,
     +                  hydtrg ,nqlat  ,qltpar ,strpar ,g      ,rho    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLHTRG (FLow Hydraulic TRiGger)
c
c Module description: Determine if hydraulic trigger is active.
c
c                     To determine if a hydraulic trigger is active
c                     first the hydraulic parameter will be calculated
c                     or the structure parameter will be retrieved.
c                     After this a check is made if the computed value
c                     exceeds a defined value.
c                     Various hydraulic triggers are possible:
c                     - water level at a certain location;
c                     - head difference over a structure;
c                     - discharge at a certain location;
c                     - value of a controlled parameter of a structure;
c                     - opening or closing of structure in progress.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 10 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c 13 hydtrg            O  Logical indicator,
c                         = TRUE, exceeding/underexceeding of the
c                         selected hydraulic parameter at a certain
c                         location.
c  2 itrig             I  Trigger number.
c  5 maxtab            I  Maximum number of defined tables.
c  9 ngrid             I  Number of grid points in network.
c  7 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
c                         to maxtab. For a specific table number k and
c                         function Y = f (X) the following definitions
c                         exist:
c                         (1,k) = Length of table k.
c                         (2,k) = Start address X in table.
c                         (3,k) = Start address Y in table.
c                         (4,k) = Access method and period control: xy
c                                 x = ctbnpf (0) : No period defined
c                                 x = ctbpfu (1) : Period defined
c                                 y = ctbico (0) : Continue interpltn
c                                 y = ctbidi (1) : Discrete interpltn
c  6 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 11 q(ngrid)          I  Discharge in every grid point at the latest
c                         iteration.
c 12 strhis(10,nstru)  I  For each structure the discharge and the
c                         parameters to be controlled must be saved to
c                         be able to write to the output file. This will
c                         be done in array strhis(8,nstru). This array
c                         will also be used to check the values of the
c                         controlled parameters or to determine if
c                         increase(open) or decrease(close) of these
c                         parameters occurs. This array will also be
c                         part of the restart file.
c                         (1,i) = Gate height
c                         (2,i) = Crest height
c                         (3,i) = Crest width
c                         (4,i) = Discharge through structure
c                         (5,i) = Gate height at previous time step
c                         (6,i) = Crest height at previous time step
c                         (7,i) = Crest width at previous time step
c                         (8,i) = Flow condition of general structure:
c                                 formno = 0, closed or other structure
c                                 formno = 1, free weir
c                                 formno = 2, drowned weir
c                                 formno = 3, free gate
c                                 formno = 4, drowned gate
c                         (9,i) = coefficient Q-H-realtion asde
c                         (10,i)= coefficient Q-H-realtion bsde
c                         (11,i)= coefficient Q-H-realtion csde
c                         (12,i)= coefficient Q-H-realtion dsde
c  3 strtyp(10,nstru)  I  Structure definitions:
c                         (1,i) = Type of structure:
c                                 csweir (1) : Simple weir
c                                 caweir (2) : Advanced weir
c                                 csgate (3) : - not used, reserved for
c                                                simple gate -
c                                 cpump  (4) : Pump
c                                 cgenst (5) : General Structure
c                         (2,i) = Position of structure:
c                                 cstbra (1) : In branch
c                                 cstlat (2) : Lateral structure
c                         (3,i) = Left gridpoint.
c                         (4,i) = Right gridpoint.
c                         (5,i) = dummy
c                         (6,i) = dummy
c                         (7,i) = dummy
c                         (8,i) = dummy
c                         (9,i) = dummy
c                         (10,i)= dummy
c  8 table             P  -
c  1 time              P  -
c  4 triger(10,ntrigr) I  Array containing parameters for each trigger
c                         or trigger combination.
c                         (1,i) = Output of trigger (combination):
c                                 1 : True
c                                 0 : False
c                         (2,i) = dummy
c                         (3,i) = dummy
c                         (4,i) = dummy
c                         (5,i) = dummy
c                         (6,i) = trigger parameter
c                                 cnotrg (0) : No trigger defined
c                                 ctitrg (1) : Time trigger used
c                                 chytrg (2) : Hydraulic trigger used
c                                 candor (3) : Time & hydraulic trigger
c                                              used (and/or mode)
c                         (7,i) = table number of time trigger
c                         (8,i) = table number 1 for hydraulic trigger
c                         (9,i) = table number 2 for hydraulic trigger
c                         (10,i)= table number of and/or period
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c inttab  INTerpolate in TABle
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flhtrg.pf,v $
c Revision 1.7  1999/03/15  15:49:57  kuipe_j
c tabs removed
c
c Revision 1.6  1996/01/17  14:38:27  kuipe_j
c header update
c
c Revision 1.5  1995/09/22  10:01:29  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/08/30  12:36:36  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:55:02  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:00  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:45  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:37:27  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:30:53  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:49  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Declaration of parameters:
c
      logical hydtrg
      integer itrig, maxtab, ntabm, ngrid, nqlat
      integer strtyp(10,*), triger(10,*), ntab(4,maxtab)
      real    g
      real    table(ntabm), strhis(dmstrh,*),
     +        qltpar(9,nqlat), rho(ngrid), strpar(dmstrpar,*) 
      double  precision  time, h(ngrid), q(ngrid)
c
c     Declaration of local variables:
c
      integer    get   ,icpnum     ,juerd
      parameter (get=2 ,icpnum = 1 ,juerd = -1)
      integer    il, ir, itab, trgtyp, locat, istru, indx, istat, 
     +           kerd, isttyp
      real       hdiff, value, x, conpar, direct, zs
c
c     Include sobek constants
c
      include '../include/sobcon.i'
c
c     For an hydraulic trigger the controller will be switched ON in
c     case of exceedance c.q. underexceedance of a selected hydraulic
c     parameter at a certain location. Otherwise the trigger will be OFF
c     This type of trigger will be stored in two time tables.
c
c     Table 1 contains: type of trigger, location and type of check.
c     itab   : table number
c     trgtyp : ctrgh   - water level
c              ctrghd  - head difference
c              ctrgdi  - discharge
c              ctrggh  - gate height
c              ctrgch  - crest height
c              ctrgcw  - crest width
c     locat  : gridpoint (type ctrgh)
c              structure number (type ctrghd)
c     locat <0 check for <x ; locat >0 check for >x ,
c              or indicator for opening/closing structure
c
c     x      : check value for the hydraulic trigger, or
c              open/close indicator
c
      itab = triger(8,itrig)
      call INTTAB (ntab(1,itab), ntab(4,itab),
     +             table(ntab(2,itab)),
     +             table(ntab(3,itab)),
     +             time, value        )
      trgtyp = int(abs(mod(value,10.)))
      locat  = int(value/10.)
c
c     Table 2 contains check values for the hydraulic trigger
c
      itab = triger(9,itrig)
      call INTTAB (ntab(1,itab), ntab(4,itab),
     +             table(ntab(2,itab)),
     +             table(ntab(3,itab)),
     +             time, x            )
c
      indx   = 0
      hydtrg = .false.
c
      if ( trgtyp .eq. ctrgh ) then
c
c        Controlled by water level
c
         if ( locat .lt. 0 ) then
            hydtrg = h(abs(locat)) .lt. x
         else
            hydtrg = h(locat) .gt. x
         endif
c
      else if ( trgtyp .eq. ctrghd ) then
c
c        Controlled by head difference
c
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
c
c        Controlled by pressure difference
c
c        - Pressure difference from structure name -
c
         istru  = abs(locat)
         il     = strtyp(3,istru)
         ir     = strtyp(4,istru)
         isttyp = strtyp(1,istru)
         call FLCNPA(get    ,isttyp ,istru ,icpnum, zs ,
     &               strpar ,juerd  ,kerd  )
c     
         hdiff = max((h(il) - zs),0.0d0)**2 * rho(il)*g/2.0 - 
     &           max((h(ir) - zs),0.0d0)**2 * rho(ir)*g/2.0
c
         if ( locat .lt. 0 ) then
            hydtrg = hdiff .lt. x
         else
            hydtrg = hdiff .gt. x
         endif     
c
      else if ( trgtyp .eq. ctrgdi ) then
c
c        Controlled by discharge
c
         if ( locat .lt. 0 ) then
            hydtrg = q(abs(locat)) .lt. x
         else
            hydtrg = q(locat) .gt. x
         endif
c
      else if ( trgtyp .eq. ctrggh ) then
         indx = 1
c
      else if ( trgtyp .eq. ctrgch ) then
         indx = 2
c
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
c
c     Trigger of controlled structure parameter?
c
      if ( indx .gt. 0 ) then
         istru = abs(locat)
cjk 6-5-2000 test improved ARS 5233         
         if ( nint(x) .gt. -999 ) then
c           - check value of controlled parameter
            conpar = strhis(indx,istru)
            if ( locat .gt. 0 ) then
               hydtrg = conpar .gt. x
            else
               hydtrg = conpar .lt. x
            endif
         else
c           - check if structure is opening or closing
            direct = strhis(indx,istru) - strhis(indx+4,istru)
            if ( locat .gt. 0 ) then
c              - opening in progress ?
               hydtrg = direct .gt. 0.
            else
c              - closing in progress ?
               hydtrg = direct .lt. 0.
            endif
         endif
      endif
c
      end
