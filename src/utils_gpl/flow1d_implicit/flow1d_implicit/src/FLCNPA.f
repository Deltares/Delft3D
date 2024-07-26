      subroutine FLCNPA(putget, isttyp, istnum, icpnumsgn, value ,
     &                  strpar, juer  , ker   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLCNPA (FLow put or get CoNtrolled PArameter)
c
c Module description: Write the value of a controlled parameter to array
c                     strpar or read the value of a controlled parameter
c                     from array strpar.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 icpnum            I  Control parameter number.
c  3 istnum            I  Structure number.
c  2 isttyp            I  Structure type.
c  1 putget            I  Put(=1) or get(=2) value of controlled
c                         parameter to or from array strpar.
c                         0=initial put after restart
c  6 strpar(21,nstru)  IO Each structure is characterized by a number of
c                         specific parameters. strpar (i,j) = parameter
c                         i of structure j:
c                         - Simple weir:
c                         (1,j) = Crest height Zs.
c                         (2,j) = Crest width Ws.
c                              Positive flow:
c                         (3,j) = Correction coefficient cw.
c                         (4,j) = Submergence limit Slim.
c                         (5,j) = Table pointer for drowned reduction
c                                 curve f(h2/h1).
c                              Negative flow:
c                         (6,j) = Correction coefficient cw.
c                         (7,j) = Submergence limit Slim.
c                         (8,j) = Table pointer for drowned reduction
c                                 curve f(h2/h1).
c                         - Advanced weir:
c                         (1,j) = Crest height Zs.
c                         (2,j) = Total net width Wn.
c                         (3,j) = Number of piers N.
c                              Positive flow:
c                         (4,j) = Heigth of upstream face P.
c                         (5,j) = Design head H0 of the weir.
c                         (6,j) = Pier contraction coefficient Kp.
c                         (7,j) = Abutment contraction coefficient Ka.
c                              Negative flow:
c                         (8,j) = Heigth of upstream face P.
c                         (9,j) = Design head H0 of the weir.
c                         (10,j)= Pier contraction coefficient Kp.
c                         (11,j)= Abutment contraction coefficient Ka.
c                         - Pump:
c                         (1,j) = Control direction:
c                                 cpmpup (-1) : upward control
c                                 cpmpdw (+1) : downward control
c                         (2,j) = Table pointer for pump capacitity re-
c                                 duction factor.
c                         (3,j) = Capacity.
c                         (4,j) = Water level which starts pump.
c                         (5,j) = Water level which stops pump.
c                         - General structure:
c                         (1,j) = Width left side of structure W1.
c                         (2,j) = Bed level left side of structure Zb1.
c                         (3,j) = Width structure left side Wsdl.
c                         (4,j) = Bed left side of structure Zbsl.
c                         (5,j) = Width structure centre Ws.
c                         (6,j) = Bed level centre Zs.
c                         (7,j) = Width structure right side Wsdr.
c                         (8,j) = Bed right side of structure Zbsr.
c                         (9,j) = Width right side of structure W2.
c                         (10,j)= Bed level right side of structure Zb2.
c                         (11,j)= Gate opening heigth dg.
c                              Positive flow:
c                         (12,j)= Correction coefficient for free gate
c                                 flow cgf.
c                         (13,j)= Correction coefficient for drowned
c                                 gate flow cgd.
c                         (14,j)= Correction coefficient for free weir
c                                 flow cwf.
c                         (15,j)= Correction coefficient for drowned
c                                 weir flow cwd.
c                         (16,j)= Contraction coefficient for free gate
c                                 flow MU-gf.
c                              Negative flow:
c                         (17,j)= Correction coefficient for free gate
c                                 flow cgf.
c                         (18,j)= Correction coefficient for drowned
c                                 gate flow cgd.
c                         (19,j)= Correction coefficient for free weir
c                                 flow cwf.
c                         (20,j)= Correction coefficient for drowned
c                                 weir flow cwd.
c                         (21,j)= Contraction coefficient for free gate
c                                 flow MU-gf.
c  5 value             IO Value which has to be put to or get from
c                         strpar
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flcnpa.pf,v $
c Revision 1.8  1999/06/01  13:42:14  kuipe_j
c names in messages substituted + message template
c
c Revision 1.7  1999/03/15  15:49:41  kuipe_j
c tabs removed
c
c Revision 1.6  1998/05/25  19:11:44  kuipe_j
c Wendy structures
c
c Revision 1.5  1996/11/01  15:04:08  kuipe_j
c Improve contoller messages
c
c Revision 1.4  1996/09/03  14:51:51  kuipe_j
c frequency time hist,Messages controllers added
c
c Revision 1.3  1996/04/12  13:03:41  kuipe_j
c headers, minor changes
c
c
c***********************************************************************
c
c
c     Include constants for array dimensions
c
      include '../include/mempool.i'
      include '../include/sobdim.i'
c
c     Declaration of parameters
c
      integer putget, isttyp, istnum, icpnumsgn, juer  , ker
      real    value , strpar(dmstrpar,*)
c
c     Declaration of local variables
c
      integer   indx,lstnam,icpnum,ker1
      real      value0
      character strnam*40
      logical   EQUAL
      external  EQUAL
      
c
c     Include sobek constants and error codes constants for 
c     array dimensions
c
      include '../include/errcod.i'
      include '../include/sobcon.i'

      icpnum = abs(icpnumsgn)  
      if ( putget.eq.1 ) then
        if ( value .lt. 0. ) then
          if ( icpnum .eq. 2 ) then
            call getstr(istnum,strnam,lstnam)
            ker = fatal
            call sre_error (juer,'FLCNPA Negative crest width in structure @
     &'//strnam(:lstnam)//'@', eflncw, ker)
          else if ( icpnum .eq. 3 ) then
            call getstr(istnum,strnam,lstnam)
            ker = fatal
            call sre_error (juer,'FLCNPA Negative gate opening height in str
     &ucture @'//strnam(:lstnam)//'@', eflngo, ker)
          endif
        endif
      endif
c
c     Compute index in array -strpar-
c     Simple- or advanced weir?
      indx = -1
      if ( isttyp .eq. csweir .or.
     &     isttyp .eq. caweir )  then
         if ( icpnum .eq. 1 ) then
            indx = 1
         else if ( icpnum .eq. 2 ) then
            indx = 2
         endif
c
c     General structure?
      else if ( isttyp .eq. cgenst ) then
         if ( icpnum .eq. 1 ) then
            indx = 6
         else if ( icpnum .eq. 2 ) then
            indx = 5
         else if ( icpnum .eq. 3 ) then
            indx = 11
         endif
c     Data base structure?
      else if ( isttyp .eq. cdtbst ) then
         if ( icpnum .eq. 1 ) then
            indx = 13
         endif
c
c     Sluice with underflow gate?
      else if ( isttyp .eq. cslund ) then
         if ( icpnum .eq. 3 ) then
            indx = 6
         endif
c
c     Gated culvert with pressure flow?
      else if ( isttyp .eq. cculpr ) then
         if ( icpnum .eq. 3 ) then
            indx = 7
         endif
c
c     Sluice with bottom hinged gate?
      else if ( isttyp .eq. cslubo ) then
         if ( icpnum .eq. 1 ) then
            indx = 4
         endif
c
c     Sluice with overflow/underflow gate?
      else if ( isttyp .eq. csovun ) then
         if ( icpnum .eq. 1 ) then
            indx = 7
         else if ( icpnum .eq. 3 ) then
            indx = 6
         endif
      endif
c
      if ( putget .eq. 1 .and. indx .gt. 0 ) then
         value0 = strpar(indx,istnum)
         strpar(indx,istnum) = value
         if (icpnumsgn.lt.0) then   
c           First put after restart
c           Check uncontrolled structure
c           for change of controllable parameter
            if (.not.EQUAL(value,value0)) then
               call getstr(istnum,strnam,lstnam)
               ker1 = warnng
               call sre_error (juer,'FLCNPA controllable par. of @'//
     &         strnam(:lstnam)//'@ changed after restart',
     &         eflcha, ker1)   
               ker = max(ker,ker1)  
            endif   
         endif
      else if ( putget .eq. 2 ) then 
         if ( indx .eq. -1 ) then
            value = 0.
         else
            value = strpar(indx,istnum)
         endif
      endif
      
c
      end
