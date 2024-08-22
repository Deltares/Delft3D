!!  Copyright (C)  Stichting Deltares, 2012-2024.
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

module m_real_array_indices
    !     Pointers in real array workspace

    integer :: IVOL     !    pointer to VOLUME, volumes
    integer :: IAREA    !    pointer to AREA  , exchange area's
    integer :: IFLOW    !    pointer to FLOW  , flows
    integer :: ILENG    !    pointer to LENG  , dispersion lengths
    integer :: IDISP    !    pointer to DISP  , 3 uniform dispersions
    integer :: ICONC    !    pointer to CONC  , concentrations
    integer :: IMASS    !    pointer to MASS  , masses
    integer :: IDERV    !    pointer to DERIV , derivatives aop
    integer :: IBOUN    !    pointer to BOUND , used boundary conditions
    integer :: IBSET    !    pointer to BSET  , set boundary conditions
    integer :: IBSAV    !    pointer to BSAVE , saved boundary conditions
    integer :: IWSTE    !    pointer to WASTE , waste loads
    integer :: ICONS    !    pointer to CONS  , constants
    integer :: IPARM    !    pointer to PARAM , parameters
    integer :: IFUNC    !    pointer to FUNC  , functions
    integer :: ISFUN    !    pointer to SFUNC , segment functions
    integer :: IDNEW    !    pointer to DISPNW, new dispersion array
    integer :: IDIFF    !    pointer to DISPER, additional dispersions
    integer :: IVNEW    !    pointer to VELONW, new velocity array
    integer :: IVELO    !    pointer to VELO  , additional velocities
    integer :: IHARM    !    pointer to HARMAT, harmonic constants
    integer :: IFARR    !    pointer to FARR  , breakpoint functions
    integer :: IMAS2    !    pointer to MASS2 , mass balance total system
    integer :: ITIMR    !    pointer to TIMER , restime/matrix st.state
    integer :: IVOL2    !    pointer to VOL2  , second volume array
    integer :: ITRAC    !    pointer to TRACE , diagonal of fast matrix            !! obsolete !!
    integer :: IGWRK    !    pointer to GMRWRK, workspace GMRES                    !! obsolete !!
    integer :: IGHES    !    pointer to GMRHES, Hessenberg matrix (GMRES)          !! obsolete !!
    integer :: IGSOL    !    pointer to GMSOL , solution vector for GMRES          !! obsolete !!
    integer :: IGDIA    !    pointer to DIAGCC, copy of diagonal of matrix A       !! obsolete !!
    integer :: IGTRI    !    pointer to GMTRI , workspace for tridiagonal solver   !! obsolete !!
    integer :: ISMAS    !    pointer to ASMAS , segment mass balance
    integer :: IPLOC    !    pointer to PLOCAL, Parameters local to proces
    integer :: IDEFA    !    pointer to DEFAUL, Process defaults array
    integer :: IFLUX    !    pointer to FLUX  , proces fluxes
    integer :: ISTOC    !    pointer to STOCHI, proces stochiometry
    integer :: IFLXD    !    pointer to FLXDMP, stored fluxes
    integer :: IFLXI    !    pointer to FLXINT, integrated stored fluxes
    integer :: IRIOB    !    pointer to RIOBUF, output buffer
    integer :: IDSPX    !    pointer to DISPX , extra dispersion array's
    integer :: IVELX    !    pointer to VELX  , extra velocity array's
    integer :: ILOCX    !    pointer to VLOCX , vars local on exch. level
    integer :: IDSTO    !    pointer to DSTO  , stochio extra dispersions
    integer :: IVSTO    !    pointer to VSTO  , stochio extra velocity
    integer :: IDMPQ    !    pointer to DMPQ  , dumped exchange fluxes
    integer :: IDMPS    !    pointer to DMPS  , dumped segment fluxes
    integer :: ITRRA    !    pointer to TRRAAI, cum tranport over transect
    integer :: INRSP    !    pointer to INWRSP, time series real space
    integer :: IVOLL    !    pointer to VOLUML, last volume before rewind solver
    integer :: IVOL3    !    pointer to VOLUME, volumes                            !! obsolete !!
    integer :: IR1      !    pointer to R1    , concentrations for meth. 12
    integer :: IQXK     !    pointer to QXK   , adv. flux in x-direction
    integer :: IQYK     !    pointer to QYK   , adv. flux in y-direction
    integer :: IQZK     !    pointer to QZK   , adv. flux in z-direction
    integer :: IDIFX    !    pointer to DIFX  , difx. flux in x-direction
    integer :: IDIFY    !    pointer to DIFX  , difx. flux in y-direction
    integer :: IDIFZ    !    pointer to DIFX  , difx. flux in z-direction
    integer :: IVOLA    !    pointer to VOL0  , volume at old time level (tris.)
    integer :: IVOLB    !    pointer to VOL1  , volume at new time level (tris.)
    integer :: IGUV     !    pointer to GUV   , grid distance in x-direction
    integer :: IGVU     !    pointer to GVU   , grid distance in y-direction
    integer :: IGZZ     !    pointer to GZZ   , grid distance in z-direction
    integer :: IAAK     !    pointer to AAK   , work array
    integer :: IBBK     !    pointer to BBK   , work array
    integer :: ICCK     !    pointer to CCK   , work array
    integer :: IBD3X    !    pointer to BD3X  , work array
    integer :: IBDDX    !    pointer to BDDX  , work array
    integer :: IBDX     !    pointer to BDX   , work array
    integer :: IBU3X    !    pointer to BU3X  , work array
    integer :: IBUUX    !    pointer to BUUX  , work array
    integer :: IBUX     !    pointer to BUX   , work array
    integer :: IWRK1    !    pointer to WRK1  , work array
    integer :: IWRK2    !    pointer to WRK2  , work array
    integer :: IAAKL    !    pointer to AAKL  , work array
    integer :: IBBKL    !    pointer to BBKL  , work array
    integer :: ICCKL    !    pointer to CCKL  , work array
    integer :: IDDKL    !    pointer to DDKL  , work array
    integer :: IWDMP    !    pointer to WSTDMP, accumulated wasteloads

    integer, parameter :: IASIZE = 78

    common / sysa / ivol, iarea, iflow, ileng, idisp, &
            iconc, imass, iderv, iboun, ibset, &
            ibsav, iwste, icons, iparm, ifunc, &
            isfun, idnew, idiff, ivnew, ivelo, &
            iharm, ifarr, imas2, itimr, ivol2, &
            itrac, igwrk, ighes, igsol, igdia, &
            igtri, ismas, iploc, idefa, iflux, &
            istoc, iflxd, iflxi, iriob, idspx, &
            ivelx, ilocx, idsto, ivsto, idmpq, &
            idmps, itrra, inrsp, ivoll, ivol3, &
            ir1, iqxk, iqyk, iqzk, idifx, &
            idify, idifz, ivola, ivolb, iguv, &
            igvu, igzz, iaak, ibbk, icck, &
            ibd3x, ibddx, ibdx, ibu3x, ibuux, &
            ibux, iwrk1, iwrk2, iaakl, ibbkl, &
            icckl, iddkl, iwdmp

    integer         ip_rar(iasize)                    ! help array to fill the common block / SYSA /
    equivalence   (ivol, ip_rar(1))              ! first entry equivalences with first entry common block

end module m_real_array_indices
