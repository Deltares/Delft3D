subroutine FLCNPA(putget, isttyp, istnum, icpnumsgn, value ,&
&strpar, juer  , ker   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLCNPA (FLow put or get CoNtrolled PArameter)
!
! Module description: Write the value of a controlled parameter to array
!                     strpar or read the value of a controlled parameter
!                     from array strpar.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 icpnum            I  Control parameter number.
!  3 istnum            I  Structure number.
!  2 isttyp            I  Structure type.
!  1 putget            I  Put(=1) or get(=2) value of controlled
!                         parameter to or from array strpar.
!                         0=initial put after restart
!  6 strpar(21,nstru)  IO Each structure is characterized by a number of
!                         specific parameters. strpar (i,j) = parameter
!                         i of structure j:
!                         - Simple weir:
!                         (1,j) = Crest height Zs.
!                         (2,j) = Crest width Ws.
!                              Positive flow:
!                         (3,j) = Correction coefficient cw.
!                         (4,j) = Submergence limit Slim.
!                         (5,j) = Table pointer for drowned reduction
!                                 curve f(h2/h1).
!                              Negative flow:
!                         (6,j) = Correction coefficient cw.
!                         (7,j) = Submergence limit Slim.
!                         (8,j) = Table pointer for drowned reduction
!                                 curve f(h2/h1).
!                         - Advanced weir:
!                         (1,j) = Crest height Zs.
!                         (2,j) = Total net width Wn.
!                         (3,j) = Number of piers N.
!                              Positive flow:
!                         (4,j) = Heigth of upstream face P.
!                         (5,j) = Design head H0 of the weir.
!                         (6,j) = Pier contraction coefficient Kp.
!                         (7,j) = Abutment contraction coefficient Ka.
!                              Negative flow:
!                         (8,j) = Heigth of upstream face P.
!                         (9,j) = Design head H0 of the weir.
!                         (10,j)= Pier contraction coefficient Kp.
!                         (11,j)= Abutment contraction coefficient Ka.
!                         - Pump:
!                         (1,j) = Control direction:
!                                 cpmpup (-1) : upward control
!                                 cpmpdw (+1) : downward control
!                         (2,j) = Table pointer for pump capacitity re-
!                                 duction factor.
!                         (3,j) = Capacity.
!                         (4,j) = Water level which starts pump.
!                         (5,j) = Water level which stops pump.
!                         - General structure:
!                         (1,j) = Width left side of structure W1.
!                         (2,j) = Bed level left side of structure Zb1.
!                         (3,j) = Width structure left side Wsdl.
!                         (4,j) = Bed left side of structure Zbsl.
!                         (5,j) = Width structure centre Ws.
!                         (6,j) = Bed level centre Zs.
!                         (7,j) = Width structure right side Wsdr.
!                         (8,j) = Bed right side of structure Zbsr.
!                         (9,j) = Width right side of structure W2.
!                         (10,j)= Bed level right side of structure Zb2.
!                         (11,j)= Gate opening heigth dg.
!                              Positive flow:
!                         (12,j)= Correction coefficient for free gate
!                                 flow cgf.
!                         (13,j)= Correction coefficient for drowned
!                                 gate flow cgd.
!                         (14,j)= Correction coefficient for free weir
!                                 flow cwf.
!                         (15,j)= Correction coefficient for drowned
!                                 weir flow cwd.
!                         (16,j)= Contraction coefficient for free gate
!                                 flow MU-gf.
!                              Negative flow:
!                         (17,j)= Correction coefficient for free gate
!                                 flow cgf.
!                         (18,j)= Correction coefficient for drowned
!                                 gate flow cgd.
!                         (19,j)= Correction coefficient for free weir
!                                 flow cwf.
!                         (20,j)= Correction coefficient for drowned
!                                 weir flow cwd.
!                         (21,j)= Contraction coefficient for free gate
!                                 flow MU-gf.
!  5 value             IO Value which has to be put to or get from
!                         strpar
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flcnpa.pf,v $
! Revision 1.8  1999/06/01  13:42:14  kuipe_j
! names in messages substituted + message template
!
! Revision 1.7  1999/03/15  15:49:41  kuipe_j
! tabs removed
!
! Revision 1.6  1998/05/25  19:11:44  kuipe_j
! Wendy structures
!
! Revision 1.5  1996/11/01  15:04:08  kuipe_j
! Improve contoller messages
!
! Revision 1.4  1996/09/03  14:51:51  kuipe_j
! frequency time hist,Messages controllers added
!
! Revision 1.3  1996/04/12  13:03:41  kuipe_j
! headers, minor changes
!
!
!***********************************************************************
!
!
!     Include constants for array dimensions
!
   include '../include/mempool.i'
   include '../include/sobdim.i'
!
!     Declaration of parameters
!
   integer putget, isttyp, istnum, icpnumsgn, juer  , ker
   real    value , strpar(dmstrpar,*)
!
!     Declaration of local variables
!
   integer   indx,lstnam,icpnum,ker1
   real      value0
   character strnam*40
   logical   EQUAL
   external  EQUAL

!
!     Include sobek constants and error codes constants for
!     array dimensions
!
   include '../include/errcod.i'
   include '../include/sobcon.i'

   icpnum = abs(icpnumsgn)
   if ( putget.eq.1 ) then
      if ( value .lt. 0. ) then
         if ( icpnum .eq. 2 ) then
            call getstr(istnum,strnam,lstnam)
            ker = fatal
            call sre_error (juer,'FLCNPA Negative crest width in structure @&
            &'//strnam(:lstnam)//'@', eflncw, ker)
         else if ( icpnum .eq. 3 ) then
            call getstr(istnum,strnam,lstnam)
            ker = fatal
            call sre_error (juer,'FLCNPA Negative gate opening height in str&
            &ucture @'//strnam(:lstnam)//'@', eflngo, ker)
         endif
      endif
   endif
!
!     Compute index in array -strpar-
!     Simple- or advanced weir?
   indx = -1
   if ( isttyp .eq. csweir .or.&
   &isttyp .eq. caweir )  then
      if ( icpnum .eq. 1 ) then
         indx = 1
      else if ( icpnum .eq. 2 ) then
         indx = 2
      endif
!
!     General structure?
   else if ( isttyp .eq. cgenst ) then
      if ( icpnum .eq. 1 ) then
         indx = 6
      else if ( icpnum .eq. 2 ) then
         indx = 5
      else if ( icpnum .eq. 3 ) then
         indx = 11
      endif
!     Data base structure?
   else if ( isttyp .eq. cdtbst ) then
      if ( icpnum .eq. 1 ) then
         indx = 13
      endif
!
!     Sluice with underflow gate?
   else if ( isttyp .eq. cslund ) then
      if ( icpnum .eq. 3 ) then
         indx = 6
      endif
!
!     Gated culvert with pressure flow?
   else if ( isttyp .eq. cculpr ) then
      if ( icpnum .eq. 3 ) then
         indx = 7
      endif
!
!     Sluice with bottom hinged gate?
   else if ( isttyp .eq. cslubo ) then
      if ( icpnum .eq. 1 ) then
         indx = 4
      endif
!
!     Sluice with overflow/underflow gate?
   else if ( isttyp .eq. csovun ) then
      if ( icpnum .eq. 1 ) then
         indx = 7
      else if ( icpnum .eq. 3 ) then
         indx = 6
      endif
   endif
!
   if ( putget .eq. 1 .and. indx .gt. 0 ) then
      value0 = strpar(indx,istnum)
      strpar(indx,istnum) = value
      if (icpnumsgn.lt.0) then
!           First put after restart
!           Check uncontrolled structure
!           for change of controllable parameter
         if (.not.EQUAL(value,value0)) then
            call getstr(istnum,strnam,lstnam)
            ker1 = warnng
            call sre_error (juer,'FLCNPA controllable par. of @'//&
            &strnam(:lstnam)//'@ changed after restart',&
            &eflcha, ker1)
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

!
end
