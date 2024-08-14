      subroutine seini (nbran  ,ngrid  ,nsedrd ,g      ,sedpar ,branch,
     &                  sedinf ,nucoef ,uscoef ,trform ,grsize ,forcon,
     &                  sedtr  ,celer  ,dissed ,sedini ,juer   ,ker   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SEINI (SEdiment INItialisation)
c
c Module description: Calculate all coefficients in the sediment trans-
c                     port formulas that are constant in time. The coef-
c                     ficients are calculated in every grid point in the
c                     relevant sections.
c
c                     Initialize the sediment transport result arrays
c                     and the celerity array.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 13 celer(ngrid,*)    O  Sediment celerity for each gridpoint.
c               1|2       - Normal branches:
c                         (i,1) = Celerity in gridpoint i in main secti-
c                                 on.
c                         - Sedredge branches:
c                         (i,1) = Celerity in gridpoint i,
c                                 left channel.
c                         (i,2) = Celerity in gridpoint i,
c                                 right channel.
c 14 dissed(4,nbran)   O  Redistributed sediment transport at begin and
c                         end of branches. At the outflow side of the
c                         branch the calculated transports are stored.
c                         At the inflow side the redistributed trans-
c                         ports are stored.
c                         (1,i)   Transport at section 1 (main or left
c                                 channel)  at begin of branch.
c                         (2,i)   Transport at section 2 (right channel)
c                                 at begin of branch.
c                         (3,i)   Transport at section 1 (main or left
c                                 channel)  at end of branch.
c                         (4,i)   Transport at section 2 (right channel)
c                                 at end of branch.
c 11 forcon(4,ngrid,*) O  Constants in transport formulas that do not
c               1|3       vary in time.
c                         Index 1:      A transport formula may have 1
c                                       till 4 constants.
c                         Index 2:      Grid point number.
c                         Index 3:      Section number:
c                         - Normal branches:
c                         (i,j,1) =     Constant in main section.
c                         - Sedredge branches:
c                         (i,j,1) =     Constant in left channel.
c                         (i,j,2) =     Constant in right channel.
c                         (i,j,3) =     Constant in exchange region.
c  4 g                 P  -
c 10 grsize(4,ngrid,*) I  Grain sizes for each gridpoint and section.
c                  1|2    Depending on the transport formula chosen one
c                         or more of the grain sizes will be defined.
c                         When a branch has been marked as a sedredge
c                         branch the D50 value should always be defined,
c                         no matter what transport formula has been
c                         chosen. For normal branches only section 1
c                         will be used being the D size in the main and
c                         sub sections. For sedredge branches section 1
c                         will be the D size in the left channel and
c                         section 2 will be the D size in the right
c                         channel.
c                         (1,i,j) =     D35 value for section j, grid-
c                                       point i.
c                         (2,i,j) =     D50 value for section j, grid-
c                                       point i.
c                         (3,i,j) =     D90 value for section j, grid-
c                                       point i.
c                         (4,i,j) =     Dmedium value for section j,
c                                       gridpoint i.
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  3 nsedrd            I  Number of defined sedredge branches.
c  7 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
c                         sedredge branch or not.
c                         (1,j) = Sedredge branch number (1..nsedrd)
c                                 else 0.
c                         (2,j) = Starting index in Rc array (River bend
c                                 curvature).
c  5 sedpar(10)        I  (1) = kinematic viscosity
c                         (2) = packing factor (porosity)
c                         (3) = relative density
c                         (4) = bed form height ratio
c                         (5) = reduction factor for sed. transp. width
c 12 sedtr(ngrid,*)    O  Sediment transport results for each gridpoint.
c               1|2       (At first transports per unit width, finaly
c                         total transports)
c                         - Normal branches:
c                         (i,1) = Sediment transport in gridpoint i of
c                                 main section.
c                         - Sedredge branches:
c                         (i,1) = Sediment transport in gridpoint i,
c                                 left channel.
c                         (i,2) = Sediment transport in gridpoint i,
c                                 right channel.
c                         (i,3) = Sediment exchange in gridpoint i.
c  9 trform(3,nbran)   I  Defines for each branch a transport formula:
c                         (1,j) = Transport formula number:
c                                 ctrfeh (1) : Engelund & Hansen
c                                 ctrfmm (2) : Meyer-Peter & Muller
c                                 ctrfaw (3) : Ackers & White
c                                 ctrfvr (4) : Van Rijn
c                                 ctrfpk (5) : Parker & Klingeman
c                                 ctrfud (6) : User Defined Formula
c                         (2,j) = Starting index of the user coeffi-
c                                 cients for this branch.
c                         (3,j) = Calibration factor for transport for-
c                                 mula.
c  8 uscoef            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c setrfo  SEdiment TRansport FOrmula
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: seini.pf,v $
c Revision 1.7  1999/06/01  13:42:42  kuipe_j
c names in messages substituted + message template
c
c Revision 1.6  1999/03/15  15:53:37  kuipe_j
c tabs removed
c
c Revision 1.5  1998/02/25  12:48:56  kuipe_j
c Check on grain size added
c
c Revision 1.4  1998/02/13  12:09:57  kuipe_j
c Avoid pointer toempty array
c
c Revision 1.3  1997/06/17  11:27:15  kuipe_j
c output in history format
c
c Revision 1.2  1995/05/30  07:07:24  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:25  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:53  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:21  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
      include '..\include\errcod.i'
      include '..\include\sobdim.i'
c
c     Declaration of parameters
c
      integer    nbran ,ngrid   ,nsedrd, juer, ker, nucoef
      integer    branch(4,nbran), sedinf(2,nbran)
      integer    sedini(*)
      real       g
      real       uscoef(nrcoefs,nucoef) ,trform(3,nbran)  ,
     &           dissed(4,nbran)        ,sedpar(*)        ,
     &           grsize(4,ngrid,*)      ,forcon(4,ngrid,*),
     &           sedtr (ngrid,*)        ,celer (ngrid,*) 
c
c     Declaration of local parameters
c
      integer    ibr   ,igr   ,ind   ,isec ,nsec  ,ised  ,i  ,j  ,k  ,
     &           n1    ,n2    ,ifil, nmess ,lbrnam,ibrn
      real       velo  ,chezy ,depth ,hrad ,sedtra
      real       pacfac,relden,kinvis, xc
      real       grn(4)
      logical    incall
      character*40    branam
      character*11    xtxt
c
      kinvis = sedpar(1)
      relden = sedpar(2)
      pacfac = sedpar(3)
c
c     Set initial flags for HIS files
c
      do 5 ifil=1,2
         sedini(ifil)=0
    5 continue
c
c     Set some array's initialy to zero. This avoids undefined values
c     when some of the branches are sedredge branches.
c
      if (nsedrd .eq. 0) then
         n1 = 1
         n2 = 1
      else
         n1 = 3
         n2 = 2
      endif
c
      do 40 j=1,ngrid
         do 20 k=1,n1
            do 10 i=1,4
               forcon(i,j,k) = 0.
   10       continue
            sedtr(j,k) = 0.
   20    continue
         do 30 k=1,n2
            celer(j,k) = 0.
   30    continue
   40 continue
c
      do 60 j=1,nbran
         do 50 i=1,4
           dissed(i,j) = 0.
   50    continue
   60 continue
c
c     Initialize parameters that are not used when the transport
c     formulas are called in initial mode.
c
      velo   = 0.
      chezy  = 0.
      depth  = 0.
      hrad   = 0.
      sedtra = 0.
c
      nmess = 0
      do 100 ibr=1,nbran
c
         ind  = max(int(trform(2,ibr)),1)
         ised = sedinf(1,ibr)
c
c        IND  = Index of coefficients for user defined formula of
c               current branch.
c        ISED = Sedredge branch number.
c
         if (ised.eq.0) then
            nsec = 1
         else
            nsec = 2
         endif
c
         incall = .true.
c
         do 90 igr = branch(3,ibr),branch(4,ibr)
c
c           Calculate constants through channels 1 (and 2).
c
            do 70 isec = 1,nsec
c
               call setrfo
     &             (incall  ,g        ,pacfac       ,relden,kinvis     ,
     &              grsize(1,igr,isec),chezy ,velo  ,depth ,hrad       ,
     &              uscoef(1,ind)     ,trform(1,ibr),forcon(1,igr,isec),
     &              sedtra)
c
c
c              Check if grainsize is specified
c              Forcon(1,,) is set to -1001 if grain size was not
c              specified
c
               if (nmess .lt. 11) then
                   if (forcon(1,igr,isec) .lt. -1000.) then
                      nmess = nmess + 1
                      ker   = fatal
                      call getloc (igr,ibrn,xc)
                      write (xtxt,'(f10.2)') xc
                      call getbrn (ibrn,branam,lbrnam)
                      call error (juer ,
     +               'SEINI grain sizes too small'//
     +               ' at branch @'//branam(:lbrnam)//'@ X= @' //
     +                xtxt//'@', esegrn , ker )
                   endif
               endif
   70       continue

            if (nsec .eq. 2) then
c
c              Calculate constants in exchange region (Sedredge)
c              At first calculate averaged grain sizes.
c
               do 80 i = 1, 4
                  grn(i) = (grsize(i,igr,1) + grsize(i,igr,2)) *.5
   80          continue
c
               call setrfo
     &             (incall  ,g    ,pacfac,relden  ,kinvis,grn     ,
     &              chezy         ,velo  ,depth   ,hrad  ,
     &              uscoef(1,ind) ,trform(1,ibr)  ,forcon(1,igr,3),
     &              sedtra        )
c
            endif
c
  90     continue
 100  continue
c
      end
