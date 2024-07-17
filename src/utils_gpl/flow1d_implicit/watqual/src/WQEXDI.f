      subroutine wqexdi (ngrid , nbran , nnode , nbrnod,
     j                   nsegmt, npntr , nsegtb, nexdef,
     j                   branch, brnode, segmnt, segtab,
     j                   segcon, juer  , ker   )
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         J.Kuipers
c
c Module:             WQEXDI (Water Quality EXchanges DImension)
c
c Module description: This module calculates the dimensions for the
c                     administration of exchanges for use in de water
c                     quality model DELWAQ. This module uses the same
c                     algorithm as WQEXAD.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 ngrid             I  nr of gridpoints
c  2 nbran             I  nr of branches
c  3 nnode             I  nr of nodes
c  4 nbrnod            I  max nr of branches in a node
c  5 nsegmt            I  nr of entries in segmnt(segments + boundaries)
c  6 npntr             O  nr of pointers
c  7 nsegtb            O  nr of entries in segtab
c  8 nexdef            I  nr of entries in exdef
c  9 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 10 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
c        ,nnode)          contains the number of connected branches
c                         (index 1) for each node. The second index
c                         contains the first connected branch number
c                         etc.
c 11 segmnt(3,nsegmt)  I  Segment definition:
c                         (1,i) = segment number (if > 0)
c                         (2,i) = first entry in segtab for this segment
c                         (3,i) = nr.of entries in segtab for this segment
c                         (1,i) = negative boundary number (if < 0)
c                         (2,i) = grdpoint where boundary is situated
c                         (3,i) = should have value 0!
c 12 segtab(5,nsegtb)  I  This table contains for each segment the en-
c                         closed grid-cells together with the length
c                         factors and section indication.
c                         (1,j) = Gridpoint 1
c                         (2,j) = Gridpoint 2
c                         (3,j) = Length factor Lb
c                         (4,j) = Length factor Le
c                         (5,j) = Section
c                                 cnopar (0) : No parallel sections
c                                 cmainc (1) : Main channel
c                                 csub1  (2) : Sub section 1
c                                 csub2  (3) : Sub section 2
c 13 segcon            O  Auxilliary array with all segments at a node.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c wqtnod  get Node number for a gridpoint
c wqtcns  get Connected Segments for a grid point
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
c $Log: wqexdi.pf,v $
c Revision 1.2  1999/03/12  12:34:57  kuipe_j
c parallel segments added
c
c Revision 1.1  1996/10/31  09:51:44  kuipe_j
c Calculation of exchanges added
c
c
c***********************************************************************
c
c       Parameters
c

      integer      ngrid ,
     j             nbran ,
     j             nnode ,
     j             nbrnod,
     j             nsegmt,
     j             npntr ,
     j             nsegtb,
     j             nexdef, juer ,ker
      integer      branch(4,nbran),
     j             segmnt(3,nsegmt),
     j             brnode(nbrnod+1,nnode)
      real         segtab(5,nsegtb)

c     Locals

      integer      igrid ,
     j             inode ,
     j             isegm ,
     j             isgtb ,
     j             jsegm ,
     j             jsgtb ,
     j             nsgtb ,
     j             msgtb ,
     j             isgtb0,
     j             jsgtb0,
     j             nsegto,isegto,
     j             nsegfr,isegfr,nsegno,
     j             igrdfr,
     j             igrdto,
     j             iconn ,
     j             icon2 ,
     j             nconn ,
     j             ibran ,ksec
      real         dista
      integer      segcon(nbrnod)
      character*8  txt
c
c     Include sobek error code file
c
      include '..\include\errcod.i'

c**********************************************************************
c     Exchanges of type 1: exchanges in gridpoints (NOT KNODES)
c**********************************************************************

      nexdef = 0
      npntr  = 0

      do 100 igrid = 1,ngrid

c        Only if this is not a knode!

         call wqtnod ( igrid , inode , branch , nbran )
         if ( inode .le. 0 ) then

c           find segments connected to igrid:
c             nsegfr is segment which includes WU ending in igrid
c             nsegto is segment which includes WU starting in igrid

            call wqtcns ( igrid , nsegmt, isegto, isegfr,
     j                    nsegtb, segmnt, segtab)

            nsegfr = segmnt(1,isegfr)
            nsegto = segmnt(1,isegto)

c           two different connected segments found??

            if ( nsegfr .ne. 0 .and. nsegto .ne. 0 .and.
     j           nsegfr .ne. nsegto ) then

c              yes! create exchange of type 1

               ksec   = max(nint(segtab(5,segmnt(2,isegfr)))*3,
     j                      nint(segtab(5,segmnt(2,isegto)))*3,1)
               npntr  = npntr + ksec
               nexdef = nexdef + ksec
            endif

         endif
  100 continue

c**********************************************************************
c     Exchanges of type 2: exchanges between gridpoints
c**********************************************************************

c     Loop over segments

      do 200 isegm = 1,nsegmt
        isgtb0 = segmnt(2,isegm)
        nsegto = segmnt(1,isegm)
c
c       Search only for the main segment, as sub segments
c       do have adjacent numbers
c
        if (nsegto .gt. 0 .and.nint(segtab(5,isgtb0)) .le. 1) then
          nsgtb  = segmnt(3,isegm)

c         Loop over grid cells (working units) within the segment

          do 150 isgtb = isgtb0,isgtb0+nsgtb-1
            dista = segtab(3,isgtb)
            igrdfr = nint(segtab(1,isgtb))
            igrdto = nint(segtab(2,isgtb))

c           Is this a grid cell cut in two pieces?

            if ( dista .gt. 0.0 ) then

c              Yes! We will look for the other piece!
c              Second loop over segments

               do 130 jsegm = 1,nsegmt
                 jsgtb0 = segmnt(2,jsegm)
                 nsegfr = segmnt(1,jsegm)
c
c                Search only for the main segment, as sub segments
c                do have adjacent numbers
c
                 if (nsegfr .gt. 0 .and.
     +              nint(segtab(5,jsgtb0)) .le. 1) then
                   msgtb  = segmnt(3,jsegm)

c                  Second loop over grid cells within the segment

                   do 120 jsgtb = jsgtb0,jsgtb0+msgtb-1
                     if ( abs(segtab(4,jsgtb)-dista) .lt. 1e-10 .and.
     j               nint(segtab(1,jsgtb)).eq. igrdfr .and.
     j               nint(segtab(2,jsgtb)).eq. igrdto ) then

c                       This is the other piece! create exchange of type 2

                        ksec = max(nint(segtab(5,segmnt(2,jsegm)))*3,
     j                             nint(segtab(5,segmnt(2,isegm)))*3,1)
                        npntr  = npntr + ksec
                        nexdef = nexdef + ksec

                        goto 150
                     endif
  120              continue
                 endif
  130          continue
            endif
  150     continue
        endif
  200 continue

c**********************************************************************
c     Exchanges of type 3: exchanges in knodes
c     Includes exchanges in boundaries, which will be defined as type 1
c**********************************************************************

c     we will determine for every node which segments and gridcells
c     are connected to this knode
c     For this purpose we use the local array segcon.

c     loop over nodes

      do 300 inode = 1,nnode

c        zero local arrays

         do 240 iconn = 1,nbrnod
            segcon(iconn) = 0
  240    continue

c        loop over branches connected to this node

         nconn = brnode(1,inode)
         do 250 iconn = 1,nconn
            ibran = brnode(1+iconn,inode)

c           find grid point on this branch in the node
c           and set dircon: = 1: branch starts, = -1: branch stops

            if ( branch(1,ibran) .eq. inode ) then
               igrid = branch(3,ibran)
            else
               igrid = branch(4,ibran)
            endif

c           find connected segment

            call wqtcns ( igrid , nsegmt, isegto, isegfr,
     j                    nsegtb, segmnt, segtab)
            if ( isegfr .ne. 0 ) then
                segcon(iconn) = isegfr
            elseif ( isegto .ne. 0 ) then
                segcon(iconn) = isegto
            else
               ker = fatal
               write (txt,'(i8)') igrid
               call error (juer ,'WQEXDI Segment error in grid point @'
     j                     //txt//'@' ,ewqseg ,ker)
            endif
  250    continue

         if ( nconn .eq. 1 ) then

c           create boundary: exchange of type 1

            isegm  = segcon(1)
            ksec   = max(nint(segtab(5,segmnt(2,isegm)))*3,1)
            npntr  = npntr + ksec
            nexdef = nexdef + ksec

         else

c           loop over every combination of branches

            do 270 iconn = 1,nconn
               do 260 icon2 = iconn+1,nconn

                  isegfr = segcon(iconn)
                  isegto = segcon(icon2)
                  nsegfr = segmnt(1,isegfr)
                  nsegto = segmnt(1,isegto)

c                 are they in the same segment?

                  if ( nsegfr .ne. nsegto ) then

c                    No! Create exchange of type 3

                     ksec = max(nint(segtab(5,segmnt(2,isegfr)))*3,1) *
     j                      max(nint(segtab(5,segmnt(2,isegto)))*3,1)

                     npntr  = npntr + ksec
                     nexdef = nexdef + ksec

                  endif
  260          continue
  270       continue

         endif

  300 continue

c**********************************************************************
c     Exchanges of type 5: exchanges between parallel segments
c**********************************************************************

      do 400 isegm = 1,nsegmt
c
c        Search for parallel segments
c
         nsegno = segmnt(1,isegm)
         if (nsegno .gt. 0) then
           if (nint(segtab(5,segmnt(2,isegm))) .gt. 0) then
              npntr  = npntr + 1
              nexdef = nexdef + 1
           endif
         endif
  400 continue
c     WRITE (11,*) 'nsegmt,npntr,nexdef'
c     WRITE (11,*) nsegmt,npntr,nexdef

      end
