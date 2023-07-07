!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      function umagi   (xp    , yp    , vz     , &
                        np    , mp    , kp     , &
                        nmax  , mmax  , layt   , &
                        flow  , depth , lgrid  , &
                        vol   , xcor  , ycor   , &
                        lgrid2, mnmaxk, acomp  , &
                        tcktot)
!
!
!                   Deltares
!
!
!     function              : higher order interpolation on horizontal
!                             flows and jacobian for accuarte displacement
!
!
! umagp = magnitude of particle velocity at particle location obtained
!         by (up,vp,wp) determined by advection procedure
! umagi = magnitude of particle velocity at particle location obtained
!         by (higher order) linear interpolation
!
! umagp = sqrt( up*up + vp*vp + wp*wp )
!
! umagi = (1/j)* sqrt( (h*dy*u)**2 + (h*dx*v)**2 + (w/h)**2 )
!
!***********************************************************************
!
!     logical unit numbers  : none.
!
!     subroutines called    : none.
!
!     functions   called    : none.
!
!     parameters            :
!
!     name    kind     length     funct.  description
!     ====    ====     ======     ======  ===========
!     acomp   real        1       input   use an analytical function
!     c2      real        1       input   depth curve of the wind component
!     c3      real        1       input   depth curve vertical velocities
!     depth   real      mnmax2    input   depth of segment  (in m)
!     dx      real      mnmax2    input   delta x for the segments
!     dy      real      mnmax2    input   delta y for the segments
!     flow    real    2*mnmaxk    input   flows in two directions
!     mp      integer     1       in/out  1th grid index particles
!     np      integer     1       in/out  2th grid index particles
!     kp      integer     1       in/out  3th grid index particles
!     layt    integer             input   number of layers of hydr. database
!     lgrid   integer nmax*mmax   input   grid numbering active table
!     lgrid2  integer nmax*mmax   input   grid numbering all cells (total)
!     mmax    integer     1       input   nr. grid cells in m dir.
!     nmax    integer     1       input   nr. grid cells in n dir.
!     tcktot  real      layt      input   rel. thickness hydro. layers
!     umagi   real        1       input   accurate magn. part.velo at pred.pos
!     umagp   real        1       input   mass-conserv. magn.velo at pred.pos
!     vol     real        1       input   volume (jacobian of this cel)
!     xcor    real        1       input   x-coordinates bottom points
!     ycor    real        1       input   y-coordinates bottom points
!     xp      real        1       input   predicted x-coordinate
!     yp      real        1       input   predicted y-coordinate
!     zp      real        1       input   predicted z-coordinate
!     xpold   real        1       input   initial   x-coordinate
!     ypold   real        1       input   initial   y-coordinate
!     zpold   real        1       input   initial   z-coordinate
!
      use precision_part    ! single/double precision
      use timers
!
      implicit none    ! force explicit typing
!
      integer(ip), dimension(:,: ) :: lgrid , lgrid2
      real   (sp), dimension(:)    :: flow  , depth
      real   (sp), dimension(:)    :: xcor  , ycor , tcktot

      logical  :: acomp, ldepth
!
!     local scalars
!
      integer (ip) :: im     , in    , inr    , mmid  , nmid
      integer (ip) :: kdep   , kp    , layt   , mdn
      integer (ip) :: mmax   , mn    , mn2    , mnd2
      integer (ip) :: mnd    , mnu   , mp     , mun   , mund   , np
      integer (ip) :: mnmaxk , imr
      integer (ip) :: mdn2   , mdnu  , mdnd2  , mdnd  , nmax
!
!     local scalars
!
      real    (sp) :: ac11   , ac12  , ac21   , ac22  , corj
      real    (sp) :: ddx    , ddy   , ddz    , dpx   , dpy
      real    (sp) :: ha     , hb    , hc     , hd    , he
      real    (sp) :: qxf    , qyf   , umagi
      real    (sp) :: qxl    , qyl   , rxf    , rxl   , ryf    , ryl
      real    (sp) :: radius , sqrt  , tperio , twopi , up1    , vd
      real    (sp) :: ua     , ub    , uc     , ud
      real    (sp) :: umdndk , umdnk , umdnuk , umndk , umnk
      real    (sp) :: va     , vb    , vc
      real    (sp) :: velo   , vol   , vp1    , vz    , wp1
      real    (sp) :: vmdndk , vmdnk , vmndk  , vmnk  , vmundk , vmunk
      real    (sp) :: xp     , xq    , yp     , yq
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "umagi", ithndl )
!
      if(.not.acomp) then
!
!
!..compute an accurate velocity from an accurate jacobian and flow
!
!         linear interpolation
!         the method will be restricted to 2dh application
!
!                 ---+-------+---
!                    |       |
!                  umdnu    umnu
!                    |       |
!         ---+-------d-------c-------+---
!            |       |       |       |
!            |     umdn     umn      |
!            |       |       |       |
!         ---+-------a-------b-------+---
!                    |       |
!                  umdnd    umnd
!                    |       |
!                 ---+-------+---
!
!.. m // u // x-intern // y-national grid
!.. n // v // y-intern // x-national grid
!.. (internal coordinates are natural coordinates between 0 and 1,
!.. for curvilinear grids these are curvilinear coordinates epsilon and nu)
!
         if(np==1.or.mp==mmax) then
            write (*,*) ' Particle outside domain in umagi '
            call stop_exit(1)
         endif
         mn    = lgrid ( np, mp )
         if(mn <= 0) then
            write (*,*) ' MN at forbidden position in umagi '
            call stop_exit(1)
         endif
         mn2   = lgrid2( np, mp )
         kdep = (kp-1)*nmax*mmax
!
         mdn   = lgrid ( np  , mp-1 )
         mdn2  = lgrid2( np  , mp-1 )
!
         mnd   = lgrid ( np-1, mp )
         mnd2  = lgrid2( np-1, mp )
!
         mdnd   = lgrid ( np-1, mp-1 )
         mdnd2  = lgrid2( np-1, mp-1 )
!
         mun    = lgrid( np, mp+1 )
!
         mund   = lgrid( np-1, mp+1 )
!
         mnu    = lgrid( np+1, mp )
!
         mdnu   = lgrid( np+1, mp-1 )
!
!  determine accurate water depth, and accurate flows at corner points
!
!  start with water depth he
!
         ldepth = .true.
         if(ldepth) then
!
!.. x//m; y//n (x,y are the internal or natural coordinates)
!.. 5 points interpolation
!..
!..
!..     d------c
!.. ^   |      |
!.. |   |      |
!.. y   a------b
!.. n,
!..   m, x =>
!..
!
            if(xp <= (0.5)) then
               xq = 2*xp
               if(mdn > 0) then
                  hd = 0.5*(depth(mdn)+depth(mn))
                  hc = depth(mn)
               else
                  hd = depth(mn)
                  hc = depth(mn)
               endif
            else
               xq = 2*xp-1.0
               if(mun > 0) then
                  hd = depth(mn)
                  hc = 0.5*(depth(mun)+depth(mn))
               else
                  hd = depth(mn)
                  hc = depth(mn)
               endif
            endif
!
            if(yp <= (0.5)) then
               yq = 2*yp
               if(mnd > 0) then
                  ha = 0.5*(depth(mnd)+depth(mn))
                  hb = depth(mn)
               else
                  ha = depth(mn)
                  hb = depth(mn)
               endif
            else
               yq = 2*yp - 1.0
               ha = hc
               hb = hd
               if(mnu > 0) then
                  hc = depth(mn)
                  hd = 0.5*(depth(mnu)+depth(mn))
               else
                  hc = depth(mn)
                  hd = depth(mn)
               endif
            endif
            he = (1.0-xq)*(1.0-yq)*ha
            he = he + xq*(1.0-yq)*hb
            he = he + xq*yq*hc
            he = he + (1.0-xq)*yq*hd
            if(kp <= 0.or.kp > layt) then
               write (*,*) ' KP out of range in umagi '
               call stop_exit(1)
            endif
            he = he*tcktot(kp)
!
            if(he <= (1.0e-15)) then
               write (*,*) ' HP to small or negative in umagi'
               call stop_exit(1)
            endif   
         else
            he = depth(mn)
         endif
!
!.. accurate ground surface (determinant)
!
         qxf  = (1.-yp)*xcor(mdnd2) + yp*xcor(mdn2)
         qyf  = (1.-yp)*ycor(mdnd2) + yp*ycor(mdn2)
         qxl  = (1.-yp)*xcor(mnd2 ) + yp*xcor(mn2 )
         qyl  = (1.-yp)*ycor(mnd2 ) + yp*ycor(mn2 )
!
         rxf  = (1.-xp)*xcor(mdnd2) + xp*xcor(mnd2)
         ryf  = (1.-xp)*ycor(mdnd2) + xp*ycor(mnd2)
         rxl  = (1.-xp)*xcor(mdn2 ) + xp*xcor(mn2 )
         ryl  = (1.-xp)*ycor(mdn2 ) + xp*ycor(mn2 )
!
         ac11 = qxl-qxf
         ac21 = qyl-qyf
!
         ac12 = rxl-rxf
         ac22 = ryl-ryf
!
!  the jacobian of the transformation: h*sqrt(g)
!
         corj  = (ac11*ac22 - ac12*ac21)*he
!
!  determine flows at corner points (a, b, c, d)
!
!  first  flow direction:  v // n // y
!  second flow direction:  u // m // x
!
!.. correct for lateral variations , i.e. for u in n direction:
!.. note that delpar is based on the trisula regular ordering
!.. note: interpolation on flows is done, not on velocities
!.. zero flows in cell itself are accounted for, of neighbouring cells
!.. these zero's are neglected.
!
!===flows current cell
!
         umnk  = flow (mn + kdep + mnmaxk)
         vmnk  = flow (mn + kdep)
!
!=== a ===
!
         if(mdn > 0) then
            umdnk = flow (mdn + kdep + mnmaxk)
!.. for d:
            vmdnk = flow (mdn + kdep)
         else
            umdnk = 0.0
            vmdnk = 0.0
         endif
         if(mnd > 0) then
            vmndk = flow (mnd + kdep)
         else
            vmndk = 0.0
         endif
         if(mdnd > 0) then
            umdndk = flow (mdnd + kdep + mnmaxk)
            vmdndk = flow (mdnd + kdep)
         else
            umdndk = umdnk
            vmdndk = vmndk
         endif
         ua = 0.5*(umdnk + umdndk)
         va = 0.5*(vmndk + vmdndk)
!
!=== b ===
!
         if(mnd > 0) then
            umndk = flow (mnd + kdep + mnmaxk)
            vmndk = flow (mnd + kdep)
         else
            umndk = umnk
            vmndk = 0.0
         endif
         if(mund > 0) then
            vmundk = flow(mund+kdep)
         else
            vmundk = vmndk
         endif
         ub = 0.5*(umnk  + umndk)
         vb = 0.5*(vmundk+ vmndk)
!
!=== c ===
!
         if(mnd > 0) then
            umndk = flow (mnu + kdep + mnmaxk)
         else
            umndk = umnk
         endif
         if(mun > 0) then
            vmunk = flow (mun + kdep)
         else
            vmunk = vmnk
         endif
         uc = 0.5*(umnk + umndk)
         vc = 0.5*(vmnk + vmunk)
!
!=== d ===
!
        if(mdnu > 0) then
           umdnuk = flow (mdnu + kdep + mnmaxk)
        else
           umdnuk = umdnk
        endif
        ud = 0.5*(umdnk + umdnuk)
        vd = 0.5*(vmdnk + vmnk  )
!
!
!.. bilinear intepolation in curvilinear space
!
         up1 = (1.-xp)*( (1.-yp)*ua + yp*ud )  &
                  +xp *( (1.-yp)*ub + yp*uc )
!
         vp1 = (1.-xp)*( (1.-yp)*va + yp*vd )  &
                  +xp *( (1.-yp)*vb + yp*vc )
!
!   better approximations for wp1 should be considered in future
!
         wp1 = vol*vz
         umagi = sqrt( up1**2 + vp1**2 + wp1**2   )
         umagi = umagi/corj
!
!  determine umagi analytically
!
      else
         ddx   = 100.0*int(20/(nmax+1))
         ddy   = 100.0*int(20/(mmax+1))
         ddz   = 1.0
         vol   = ddx*ddy*ddz
         twopi = 8.0*atan(1.0)
         tperio= 7200.0
         mmid  = int(mmax/2) + 1
         nmid  = int(nmax/2) + 1
         im  = mp
         in  = np
         inr = in-nmid
         dpy = inr*ddy + (yp-0.5)*ddy
         imr = im-mmid
         dpx = imr*ddx + (xp-0.5)*ddx
         radius = sqrt(dpx*dpx + dpy*dpy)
         velo   = twopi*radius/tperio
!.. ok since ddx = ddy
!        umagi  = ddz*ddx*velo/vol
         umagi  = velo/ddy
      endif
!
      if ( timon ) call timstop ( ithndl )
      return
      end function

