      subroutine sazfvu (im     ,nnode ,nhstat ,maxtab ,ntabm ,time   ,
     &                   dt     ,tp    ,lslack ,mouth  ,node  ,hbdpar ,
     &                   ntab   ,table ,emppar ,mouqpu )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SAZFVU (SAlt Zwendl Flood Vol. and max. vel.(U))
c
c Module description: Calculate flood volume and maximum flood velocity
c                     for next tide.
c
c                     At the start of a tidal period a new flood volume
c                     and maximum velocity have to be calculated. This
c                     is done by reading the hydrodynamic boundary con-
c                     ditions and determination of the highest and lo-
c                     west water level over the next tidal period. Note
c                     that for the Zwendl formulation the boundary con-
c                     dition for the mouth must be a water level as a
c                     function of time.
c
c                     The obtained flood volume and the maximum flood
c                     velocity are stored for the last tide and the
c                     values of the last tide are stored for the one but
c                     last tide.
c
c Precondition:       It is already determined if it is the start of a
c                     new tidal period.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  7 dt                I  Computational time step dt [sec].
c 15 emppar(4,nmouth)  I  Constants for each sea mouth for the empirical
c                         formulation:
c                         (1,i) = Constant u0 at sea mouth i.
c                         (2,i) = Constant u1 at sea mouth i.
c                         (3,i) = Constant P0 at sea mouth i.
c                         (4,i) = Constant P1 at sea mouth i.
c 12 hbdpar(3,nhstat)  I  Hydrodynamic conditions for H-stations:
c                         (1,i) = Location [grid point] for H-station.
c                         (2,i) = Type of condition
c                                 cbftim (1) : h = f(t)
c                                 cbfqoh (2) : h = h(Q)
c                                 cbfour (3) : h = fourier
c                                 cbtidl (4) : h = tidal components
c                         (3,i) = Table number for f(t), h(Q), fourier
c                                 or tidal components table.
c  1 im                I  Mouth number.
c  9 lslack            I  = .true.   :  Slack water before flood encoun-
c                                       tered
c                         = .false.  :  Otherwise.
c  4 maxtab            I  Maximum number of defined tables.
c 16 mouqpu(3,0:2,     IO Contains auxilliary data for the Thatcher
c        nmouth)          Harleman or ZWENDL dispersion formulation.
c                         - First index:
c                         (1,,) = Fresh water discharge.
c                         (2,,) = Flood volume.
c                         (3,,) = Maximum flood velocity.
c                         - Second index:
c                         (,0,) = For current tide. Mouqpu(i,0,j) con-
c                                 tains the actual sum or maximum on the
c                                 current time.
c                         (,1,) = For the last tide.
c                         (,2,) = For the tide before the last tide.
c                         - Third index:
c                         (,,i) = Mouth number.
c 10 mouth(2,nmouth)   I  Node numbers which are mouths:
c                         (1,i) = Node number j which is a mouth.
c                         (2,i) = Number of the branch that contains the
c                                 mouth.
c  3 nhstat            I  Number of H-boundary stations.
c  2 nnode             I  Number of nodes.
c 11 node(4,nnode)     I  Definition of nodes:
c                         (1,i) = Type of node i:
c                                 cintnd (1) : Internal node
c                                 chbou  (2) : H-boundary
c                                 cqbou  (3) : Q-boundary
c                                 cqhbou (4) : QH-boundary
c                                 chqbou (5) : HQ-boundary
c                         (2,i) = Gridpoint in case of boundary, else
c                                 undefined.
c                         (3,i) = Station number for boundary, undefined
c                                 for internal nodes:
c                                 HQ, H-boundary: station nr H-station.
c                                 QH, Q-boundary: station nr Q-station.
c                         (4,i) = Boundary number in case of boundary.
c 13 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
c  5 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 14 table             P  -
c  6 time              I  Actual time level tn+1. in sec.
c  8 tp                I  Tidal period   (salt module).
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c inttab  INTerpolate in TABle
c series  SERIES of fourier or tidal component
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sazfvu.pf,v $
c Revision 1.4  1995/10/18  09:00:34  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/05/30  09:56:21  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:06:26  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:07  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  09:17:31  kuipe_j
c Time , timestep and period in double precision.
c
c Revision 1.2  1993/11/26  15:34:17  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:17  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer   im    ,nnode ,nhstat ,maxtab  ,ntabm
      integer   mouth (2,*)      ,node(4,nnode) ,hbdpar(3,nhstat),
     &          ntab  (4,maxtab)
      real      table (ntabm) ,emppar(4,*)      ,mouqpu(3,0:2,*)
      double precision time   ,dt    ,tp
      logical   lslack
c
c     Declaration of local parameters
c
      integer   inod ,ihstat ,iopt ,itab  ,nt  ,it
      real      h    ,hmax   ,hmin
      double    precision     tcur
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
      if (lslack) then
c
c        Change from outflow to inflow (slack water before flood).
c        At first, the minimum and maximum water levels at the
c        mouth are calculated in the next tidal period.
c
         inod   = mouth(1,im)
         ihstat = node (3,inod)
         iopt   = int(hbdpar(2,ihstat))
c
c        itab       : TABLE number h=f(t)
c
         itab   = int(hbdpar(3,ihstat))
c
         hmax   = -1.e+10
         hmin   =  1.e+10
         nt     = int(tp / dt)
         do 10 it = 1,nt
            tcur = dble(it-1)*dt + time
            if (iopt .eq. cbftim) then

               call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                      table(ntab(2,itab)),
     &                      table(ntab(3,itab)),
     &                      tcur               ,h           )
            else if (iopt .eq. cbfour .or. iopt .eq. cbtidl) then
               call series (tcur  ,iopt  ,table(ntab(2,itab)),
     &                      ntab(1,itab) ,h                  )
            endif
c
            hmax = max(hmax,h)
            hmin = min(hmin,h)
   10    continue
c
c        Secondly the flood volume and maximum velocity are calculated.
c        [ Doc. S-FO-001.5KV / Eq 19-10(b,c) ]
c
         mouqpu(2,0,im) = emppar(3,im) + emppar(4,im)*(hmax-hmin)
         mouqpu(3,0,im) = emppar(1,im) + emppar(2,im)*(hmax-hmin)
c
c        Shift flood volume.
         mouqpu(2,2,im) = mouqpu(2,1,im)
         mouqpu(2,1,im) = mouqpu(2,0,im)
c
c        Shift maximum velocity.
         mouqpu(3,2,im) = mouqpu(3,1,im)
         mouqpu(3,1,im) = mouqpu(3,0,im)
c
      endif
c
      end
