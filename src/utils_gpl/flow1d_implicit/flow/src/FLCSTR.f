      subroutine FLCSTR(time   ,dt1    ,istep  ,g      ,rho    ,ncontr ,
     +                  contrl ,strtyp ,strpar ,ngrid  ,h      ,q      ,
     +                  af     ,maxtab ,ntabm  ,ntab   ,table  ,ncsrel ,
     +                  cnstrl ,ntrigr ,triger ,ntcrel ,trcnrl ,nstru  ,
     +                  strhis ,conhis ,cnpflg ,lagstm ,nlags  ,buflag ,
     +                  nqlat  ,qltpar ,juer   ,lrest  ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLCSTR (FLow Controlled STRuctures)
c
c Module description: This routine determines for each controlled
c                     structure the new value of the controlled
c                     variables.
c
c                     At the start of a new time step each controlled
c                     structure will be processed by this routine. This
c                     will be done in the following steps:
c                     - First, routine FLTRIG is called to check each
c                       trigger. A trigger must return the value true
c                       or false.
c                     - Secondly, routine FLTRCO is called to determine
c                       the status of each controller (active or
c                       not-active). The status depends on the value of
c                       the related triggers.
c
c                     A trigger is related to one or more controllers.
c                     A controller is related to zero, one or more
c                     triggers.
c
c                     - Copy "current control values" to "previous
c                       values". (c(tn-1) = c(tn))
c                     - Thirdly, routine FLCONT is called to calculate
c                       the values of the structure parameters
c                       controlled by all active controllers. These
c                       values are stored in array conhis.
c                     - Finally, routine FLCOST copies the actual values
c                       of the controlled parameters to the arrays of
c                       the related structures (strpar and strhis).
c
c                     A controller is related to one or more structures.
c                     A structure is related to zero, one or more
c                     controllers.
c
c
c
c                     In routine FLTRIG all triggers will be evaluated.
c                     The values are ON=true or OFF=false.
c                     Dependend of the value of a trigger (ON or OFF)
c                     related controllers will be triggered.
c
c                     In routine FLTRIG the actual trigger (ON/OFF) is
c                     computed. Three options are possible:
c
c                     - time trigger;
c                     - hydraulic trigger;
c                     - combination of time and hydraulic trigger.
c
c                     It is possible to perform a logical AND or
c                     logical OR function on the time- and hydraulic
c                     trigger.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 13 af                P  -
c 19 cnstrl            P  -
c 26 conhis            P  -
c  7 contrl            P  -
c  2 dt1               P  -
c  4 g                 P  -
c 11 h                 P  -
c  3 istep             P  -
c 14 maxtab            I  Maximum number of defined tables.
c  6 ncontr            I  Number of controlled structures.
c 18 ncsrel            P  -
c 10 ngrid             I  Number of grid points in network.
c 24 nstru             I  Number of structures.
c 16 ntab              P  -
c 15 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 22 ntcrel            P  -
c 20 ntrigr            P  -
c 12 q                 P  -
c  5 rho               P  -
c 25 strhis(10,nstru)  IO For each structure the discharge and the
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
c  9 strpar            P  -
c  8 strtyp            P  -
c 17 table             P  -
c  1 time              P  -
c 23 trcnrl            P  -
c 21 triger            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flcont  FLow CONTrollers
c flcost  FLow COntroller STructure relations
c fltrco  FLow TRigger COntroller relations
c fltrig  FLow TRIGger
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flcstr.pf,v $
c Revision 1.11  1999/03/15  15:49:44  kuipe_j
c tabs removed
c
c Revision 1.10  1998/12/11  13:05:05  kuipe_j
c avoid test dupl contr in autostart
c
c Revision 1.9  1998/06/08  12:29:42  kuipe_j
c time lag hydr controller
c
c Revision 1.8  1996/09/03  14:51:53  kuipe_j
c frequency time hist,Messages controllers added
c
c Revision 1.7  1996/05/30  09:56:34  kuipe_j
c general structure dlim, controllers
c
c Revision 1.6  1996/01/17  14:38:17  kuipe_j
c header update
c
c Revision 1.5  1995/09/22  10:01:12  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/08/30  12:36:29  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:54:55  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:52  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:37  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:37:23  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:30:46  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:48  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters:
c
      integer istep , ncontr, maxtab, ntabm, ngrid, ncsrel, ntrigr
      integer ntcrel, nstru , juer  , ker, lagstm ,nlags, nqlat
      integer strtyp(10,nstru), ntab(4,maxtab), cnstrl(2,*)
      integer triger(10,*), trcnrl(5,*) , cnpflg(dmcopr,*)
      real    g, table(ntabm), strpar(dmstrpar,nstru)
      real    af(ngrid), contrl(17,*) ,rho(ngrid)
      real    conhis(5,nstru), strhis(dmstrh,*), buflag(lagstm,nlags)
      real    qltpar(9,nqlat)  
      logical lrest
      double  precision  time ,dt1, h(ngrid), q(ngrid)
c
c     Declaration of local variables:
c
      integer i, j
      logical lauto
c
      lauto = istep .eq. 0
      if (.not. lauto) then
      call FLTRIG(time   ,strtyp ,ntrigr ,triger ,maxtab ,ntabm  ,
     +            ntab   ,table  ,ngrid  ,h      ,q      ,strhis ,
     +            nqlat  ,qltpar ,strpar ,g      ,rho    )
c
      call FLTRCO(ntcrel ,trcnrl ,triger, contrl )
c
c     Copy "current control values" to "previous values"
c
      do 10 i = 1, nstru
         do 10 j = 1, 3
            strhis(j+4,i) = strhis(j,i)
   10 continue
c
c     Collect discharges on locations with time lag.
c
      call FLTLAG(3   ,ncontr ,contrl ,lagstm ,nlags  ,buflag ,
     +            dt1 ,ngrid  ,q      )
      endif
c
      call FLCONT(lauto  ,time   ,dt1    ,istep  ,g      ,rho    ,
     +            strtyp ,ngrid  ,h      ,q      ,af     ,maxtab ,
     +            ntabm  ,ntab   ,table  ,ncontr ,contrl ,ncsrel ,
     +            cnstrl ,strhis ,conhis ,strpar ,nstru  ,lrest  ) 
c
      call FLCOST(ncsrel ,nstru  ,strtyp ,strpar ,strhis ,cnstrl ,
     +            conhis ,contrl ,cnpflg ,juer   ,ker    )
c
      if (lauto) then
         do 20 i=1,ncontr
            contrl(2,i) = 1.
  20     continue
      endif
      end
