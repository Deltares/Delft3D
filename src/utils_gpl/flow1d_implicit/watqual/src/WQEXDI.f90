subroutine wqexdi (ngrid , nbran , nnode , nbrnod,&
&nsegmt, npntr , nsegtb, nexdef,&
&branch, brnode, segmnt, segtab,&
&segcon, juer  , ker   )
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         J.Kuipers
!
! Module:             WQEXDI (Water Quality EXchanges DImension)
!
! Module description: This module calculates the dimensions for the
!                     administration of exchanges for use in de water
!                     quality model DELWAQ. This module uses the same
!                     algorithm as WQEXAD.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 ngrid             I  nr of gridpoints
!  2 nbran             I  nr of branches
!  3 nnode             I  nr of nodes
!  4 nbrnod            I  max nr of branches in a node
!  5 nsegmt            I  nr of entries in segmnt(segments + boundaries)
!  6 npntr             O  nr of pointers
!  7 nsegtb            O  nr of entries in segtab
!  8 nexdef            I  nr of entries in exdef
!  9 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 10 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
!        ,nnode)          contains the number of connected branches
!                         (index 1) for each node. The second index
!                         contains the first connected branch number
!                         etc.
! 11 segmnt(3,nsegmt)  I  Segment definition:
!                         (1,i) = segment number (if > 0)
!                         (2,i) = first entry in segtab for this segment
!                         (3,i) = nr.of entries in segtab for this segment
!                         (1,i) = negative boundary number (if < 0)
!                         (2,i) = grdpoint where boundary is situated
!                         (3,i) = should have value 0!
! 12 segtab(5,nsegtb)  I  This table contains for each segment the en-
!                         closed grid-cells together with the length
!                         factors and section indication.
!                         (1,j) = Gridpoint 1
!                         (2,j) = Gridpoint 2
!                         (3,j) = Length factor Lb
!                         (4,j) = Length factor Le
!                         (5,j) = Section
!                                 cnopar (0) : No parallel sections
!                                 cmainc (1) : Main channel
!                                 csub1  (2) : Sub section 1
!                                 csub2  (3) : Sub section 2
! 13 segcon            O  Auxilliary array with all segments at a node.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! wqtnod  get Node number for a gridpoint
! wqtcns  get Connected Segments for a grid point
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: wqexdi.pf,v $
! Revision 1.2  1999/03/12  12:34:57  kuipe_j
! parallel segments added
!
! Revision 1.1  1996/10/31  09:51:44  kuipe_j
! Calculation of exchanges added
!
!
!***********************************************************************
!
!       Parameters
!

   integer      ngrid ,&
   &nbran ,&
   &nnode ,&
   &nbrnod,&
   &nsegmt,&
   &npntr ,&
   &nsegtb,&
   &nexdef, juer ,ker
   integer      branch(4,nbran),&
   &segmnt(3,nsegmt),&
   &brnode(nbrnod+1,nnode)
   real         segtab(5,nsegtb)

!     Locals

   integer      igrid ,&
   &inode ,&
   &isegm ,&
   &isgtb ,&
   &jsegm ,&
   &jsgtb ,&
   &nsgtb ,&
   &msgtb ,&
   &isgtb0,&
   &jsgtb0,&
   &nsegto,isegto,&
   &nsegfr,isegfr,nsegno,&
   &igrdfr,&
   &igrdto,&
   &iconn ,&
   &icon2 ,&
   &nconn ,&
   &ibran ,ksec
   real         dista
   integer      segcon(nbrnod)
   character*8  txt
!
!     Include sobek error code file
!
   include '..\include\errcod.i'

!**********************************************************************
!     Exchanges of type 1: exchanges in gridpoints (NOT KNODES)
!**********************************************************************

   nexdef = 0
   npntr  = 0

   do 100 igrid = 1,ngrid

!        Only if this is not a knode!

      call wqtnod ( igrid , inode , branch , nbran )
      if ( inode .le. 0 ) then

!           find segments connected to igrid:
!             nsegfr is segment which includes WU ending in igrid
!             nsegto is segment which includes WU starting in igrid

         call wqtcns ( igrid , nsegmt, isegto, isegfr,&
         &nsegtb, segmnt, segtab)

         nsegfr = segmnt(1,isegfr)
         nsegto = segmnt(1,isegto)

!           two different connected segments found??

         if ( nsegfr .ne. 0 .and. nsegto .ne. 0 .and.&
         &nsegfr .ne. nsegto ) then

!              yes! create exchange of type 1

            ksec   = max(nint(segtab(5,segmnt(2,isegfr)))*3,&
            &nint(segtab(5,segmnt(2,isegto)))*3,1)
            npntr  = npntr + ksec
            nexdef = nexdef + ksec
         endif

      endif
100 continue

!**********************************************************************
!     Exchanges of type 2: exchanges between gridpoints
!**********************************************************************

!     Loop over segments

   do 200 isegm = 1,nsegmt
      isgtb0 = segmnt(2,isegm)
      nsegto = segmnt(1,isegm)
!
!       Search only for the main segment, as sub segments
!       do have adjacent numbers
!
      if (nsegto .gt. 0 .and.nint(segtab(5,isgtb0)) .le. 1) then
         nsgtb  = segmnt(3,isegm)

!         Loop over grid cells (working units) within the segment

         do 150 isgtb = isgtb0,isgtb0+nsgtb-1
            dista = segtab(3,isgtb)
            igrdfr = nint(segtab(1,isgtb))
            igrdto = nint(segtab(2,isgtb))

!           Is this a grid cell cut in two pieces?

            if ( dista .gt. 0.0 ) then

!              Yes! We will look for the other piece!
!              Second loop over segments

               do 130 jsegm = 1,nsegmt
                  jsgtb0 = segmnt(2,jsegm)
                  nsegfr = segmnt(1,jsegm)
!
!                Search only for the main segment, as sub segments
!                do have adjacent numbers
!
                  if (nsegfr .gt. 0 .and.&
                  &nint(segtab(5,jsgtb0)) .le. 1) then
                     msgtb  = segmnt(3,jsegm)

!                  Second loop over grid cells within the segment

                     do 120 jsgtb = jsgtb0,jsgtb0+msgtb-1
                        if ( abs(segtab(4,jsgtb)-dista) .lt. 1e-10 .and.&
                        &nint(segtab(1,jsgtb)).eq. igrdfr .and.&
                        &nint(segtab(2,jsgtb)).eq. igrdto ) then

!                       This is the other piece! create exchange of type 2

                           ksec = max(nint(segtab(5,segmnt(2,jsegm)))*3,&
                           &nint(segtab(5,segmnt(2,isegm)))*3,1)
                           npntr  = npntr + ksec
                           nexdef = nexdef + ksec

                           goto 150
                        endif
120                  continue
                  endif
130            continue
            endif
150      continue
      endif
200 continue

!**********************************************************************
!     Exchanges of type 3: exchanges in knodes
!     Includes exchanges in boundaries, which will be defined as type 1
!**********************************************************************

!     we will determine for every node which segments and gridcells
!     are connected to this knode
!     For this purpose we use the local array segcon.

!     loop over nodes

   do 300 inode = 1,nnode

!        zero local arrays

      do 240 iconn = 1,nbrnod
         segcon(iconn) = 0
240   continue

!        loop over branches connected to this node

      nconn = brnode(1,inode)
      do 250 iconn = 1,nconn
         ibran = brnode(1+iconn,inode)

!           find grid point on this branch in the node
!           and set dircon: = 1: branch starts, = -1: branch stops

         if ( branch(1,ibran) .eq. inode ) then
            igrid = branch(3,ibran)
         else
            igrid = branch(4,ibran)
         endif

!           find connected segment

         call wqtcns ( igrid , nsegmt, isegto, isegfr,&
         &nsegtb, segmnt, segtab)
         if ( isegfr .ne. 0 ) then
            segcon(iconn) = isegfr
         elseif ( isegto .ne. 0 ) then
            segcon(iconn) = isegto
         else
            ker = fatal
            write (txt,'(i8)') igrid
            call error (juer ,'WQEXDI Segment error in grid point @'&
            &//txt//'@' ,ewqseg ,ker)
         endif
250   continue

      if ( nconn .eq. 1 ) then

!           create boundary: exchange of type 1

         isegm  = segcon(1)
         ksec   = max(nint(segtab(5,segmnt(2,isegm)))*3,1)
         npntr  = npntr + ksec
         nexdef = nexdef + ksec

      else

!           loop over every combination of branches

         do 270 iconn = 1,nconn
            do 260 icon2 = iconn+1,nconn

               isegfr = segcon(iconn)
               isegto = segcon(icon2)
               nsegfr = segmnt(1,isegfr)
               nsegto = segmnt(1,isegto)

!                 are they in the same segment?

               if ( nsegfr .ne. nsegto ) then

!                    No! Create exchange of type 3

                  ksec = max(nint(segtab(5,segmnt(2,isegfr)))*3,1) *&
                  &max(nint(segtab(5,segmnt(2,isegto)))*3,1)

                  npntr  = npntr + ksec
                  nexdef = nexdef + ksec

               endif
260         continue
270      continue

      endif

300 continue

!**********************************************************************
!     Exchanges of type 5: exchanges between parallel segments
!**********************************************************************

   do 400 isegm = 1,nsegmt
!
!        Search for parallel segments
!
      nsegno = segmnt(1,isegm)
      if (nsegno .gt. 0) then
         if (nint(segtab(5,segmnt(2,isegm))) .gt. 0) then
            npntr  = npntr + 1
            nexdef = nexdef + 1
         endif
      endif
400 continue
!     WRITE (11,*) 'nsegmt,npntr,nexdef'
!     WRITE (11,*) nsegmt,npntr,nexdef

end
