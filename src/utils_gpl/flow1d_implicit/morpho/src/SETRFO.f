      subroutine setrfo (initra ,g      ,pacfac ,relden ,kinvis ,grn   ,
     &                   chezy  ,u      ,depth  ,hrad   ,uscofb ,trforb,
     &                   forcng ,sedtra )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SETRFO (SEdiment TRansport FOrmula)
c
c Module description: Select the appropriate transport formula and cal-
c                     culate the sediment transport in a grid point in a
c                     section.
c
c                     The appropriate module is called depending on the
c                     transport formula definition. After this the sedi-
c                     ment transport is multiplied with a calibration
c                     factor. The formula and calibration factor may be
c                     different for each branch.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  7 chezy             P  -
c  9 depth             P  -
c 13 forcng            P  -
c  2 g                 P  -
c  6 grn               P  -
c 10 hrad              P  -
c  1 initra            P  -
c  5 kinvis            P  -
c  3 pacfac            P  -
c  4 relden            P  -
c 14 sedtra            IO calculated sediment transport
c 12 trforb(3)         I  Defines for a branch a transport formula:
c                         (1) =   Transport formula number:
c                                 ctrfeh (1) : Engelund & Hansen
c                                 ctrfmm (2) : Meyer-Peter & Muller
c                                 ctrfaw (3) : Ackers & White
c                                 ctrfvr (4) : Van Rijn
c                                 ctrfpk (5) : Parker & Klingeman
c                                 ctrfud (6) : User Defined Formula
c                         (2) =   Starting index of the user coeffi-
c                                 cients for this branch.
c                         (3) =   Calibration factor for transport for-
c                                 mula.
c  8 u                 I  velocity
c 11 uscofb            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c setfaw  SEdiment Transport Formula Ackers & White
c setfeh  SEdiment Transp. Form. Engelund & Hansen
c setfmm  SEdiment Transp. Form. Meyer-Peter & Muller
c setfpk  SEdiment Transp. Form. Parker & Klingeman
c setfud  SEdiment Transport Formula User Defined
c setfvr  SEdiment Transport Formula Van Rijn
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: setrfo.pf,v $
c Revision 1.3  1996/12/03  08:24:17  kuipe_j
c calibration factor added in power distribution
c
c Revision 1.2  1995/05/30  07:07:41  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:36  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:35:11  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:24  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      real       g      ,pacfac   ,relden   ,kinvis  ,chezy  ,hrad  ,
     &           u      ,depth    ,sedtra
      real       grn(4) ,uscofb(*),forcng(4),trforb(3)
      logical    initra
c
c     Declaration of local parameters
c
      real               velo
c
c     Constants
c
      integer     d35   , d50   , d90   , dmed
      parameter  (d35=1 , d50=2 , d90=3 , dmed=4)
c
      velo  = abs(u)
c
      goto (10,20,30,40,50,60,70) int(trforb(1))
c
c        Engelund and Hansen
c
   10    call setfeh
     &       (initra   ,g         ,pacfac  ,relden   ,grn(d50),chezy   ,
     &        velo     ,forcng(1) ,sedtra  )
      goto 100
c
c        Meyer-Peter and Muller
c
   20    call setfmm
     &       (initra   ,g      ,pacfac  ,relden   ,grn(dmed),grn(d90) ,
     &        chezy    ,velo   ,hrad    ,forcng(1),sedtra   )
      goto 100
c
c        Ackers and White
c
   30    call setfaw
     &       (initra   ,g        ,pacfac  ,relden ,kinvis   ,grn(d35) ,
     &        chezy    ,velo    ,depth  ,forcng(1),forcng(2),
     &        forcng(3),forcng(4),sedtra  )
      goto 100
c
c        Van Rijn
c
   40    call setfvr
     &       (initra   ,g        ,pacfac ,relden  ,kinvis   ,grn(d50) ,
     &        grn(d90) ,velo     ,depth  ,hrad    ,forcng(1),forcng(2),
     &        forcng(3),forcng(4),sedtra )
      goto 100
c
c        Parker and Klingeman
c
   50    call setfpk
     &       (initra   ,g        ,pacfac   ,relden ,grn(d50) ,chezy ,
     &        velo     ,forcng(1),forcng(2),sedtra )
      goto 100
c
c        User defined formula
c
   60    call setfud
     &       (initra  ,g      ,pacfac ,relden ,grn(d50) ,grn(d90) ,
     &        chezy   ,uscofb ,velo   ,hrad   ,forcng(1),forcng(2),
     &        sedtra  )
      goto 100
c
c        No formula known, exit with transport = 0
c
   70    sedtra = 0.
  100 continue
c
      if (.not.initra) sedtra = sign(sedtra,u) * trforb(3)
c
      end
