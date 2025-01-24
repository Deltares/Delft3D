!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!
module unstruc_opengl
   use precision
#ifdef HAVE_OPENGL
   use IFWINA, only: HANDLE
   use IFOPNGL
#endif
   implicit none
   private

#ifdef HAVE_OPENGL
   integer(HANDLE) :: HWND = 0
   integer(HANDLE) :: MEMDC = 0
   integer(HANDLE) :: HBITMAP = 0
   integer :: OldBitmap = 0
   integer(HANDLE) :: HDC = 0
   integer(HANDLE) :: HRC = 0
   integer, public :: currentWidth, currentHeight
   integer :: lastNCol = -1
   integer :: lastWidth, lastHeight
   integer, public :: jaOpenGL = 1 !< use OpenGL (1) or not (0)
! curvigrid is not being displayed with OpenGl
#else
   integer, public :: jaOpenGL = 0 !< use OpenGL (1) or not (0)
#endif
   logical, public :: InOpenGLRendering = .false.
   integer, parameter, public :: FontSize = 28
   real(kind=sp), public :: xlast, ylast

   public :: BeginRender, EndRender, RenderText, SetLineWidth, LineTo, MoveTo, FillPolygon, DrawPoint, SetColorFromColorNR, SetTextHeight

contains

   !todo: optimize by checking for triangle (n=3)/quads (n=4) first, don't switch mode while in those
   subroutine FillPolygon(xs, ys, n)
      implicit none
      integer, intent(in) :: n !< num vertices
      real(kind=sp), intent(in) :: xs(n)
      real(kind=sp), intent(in) :: ys(n)
      integer :: i

#ifdef HAVE_OPENGL
      !draw solid polygon
      call fglPolygonMode(GL_FRONT, GL_FILL) !todo: do we have to call this every time?
      call fglBegin(GL_POLYGON)
      do i = 1, n
         call fglVertex2f(xs(i), ys(i))
      end do
      call fglEnd()
#endif

   end subroutine

   subroutine SetLineWidth(w) ! pixels
      implicit none
      integer, intent(in) :: w
#ifdef HAVE_OPENGL
      call fglLineWidth(real(w))
#endif
   end subroutine

   subroutine SetPointSize(r) ! pixels
      implicit none
      real(kind=sp), intent(in) :: r
#ifdef HAVE_OPENGL
      call fglPointSize(r)
#endif
   end subroutine

   !todo: optimize all these Begin/End calls, only call when switching modes
   subroutine DrawLine(x1, y1, x2, y2) ! world coordinates
      implicit none
      real(kind=sp), intent(in) :: x1
      real(kind=sp), intent(in) :: x2
      real(kind=sp), intent(in) :: y1
      real(kind=sp), intent(in) :: y2

#ifdef HAVE_OPENGL
      call fglBegin(GL_LINES)
      call fglVertex2f(x1, y1)
      call fglVertex2f(x2, y2)
      call fglEnd()
#endif

   end subroutine

   subroutine DrawPoint(x, y) ! world coordinates
      implicit none
      real(kind=sp), intent(in) :: x
      real(kind=sp), intent(in) :: y

#ifdef HAVE_OPENGL
      call fglBegin(GL_POINTS)
      call fglVertex2f(x, y)
      call fglEnd()
#endif
      xlast = x
      ylast = y
   end subroutine

   subroutine RenderText(x, y, txt) ! world coordinates
      implicit none
      real(kind=sp), intent(in) :: x
      real(kind=sp), intent(in) :: y
      character(len=*), intent(in) :: txt

#ifdef HAVE_OPENGL
      call fglRasterPos2f(x, y)
      call fglCallLists(len(txt), GL_UNSIGNED_BYTE, LOC(txt)) ! now draw the characters in a string
#endif

   end subroutine

   subroutine SetColorFromColorNr(ncol) !interacter color nr.
      implicit none
      integer, intent(in) :: ncol
      integer :: rgb, r, g, b
      real(kind=sp) :: rr, gg, bb
      integer, external :: InfoGrPalette !access interacter palette info

#ifdef HAVE_OPENGL
      if (lastNCol == ncol) then
         return ! no change..
      end if

      lastNCol = ncol

      ! grab the rgb value of the color nr
      rgb = InfoGrPalette(NCOL)

      ! split into separate r, g, b channel values (0.0 - 1.0)
      r = iand(rgb, z'ff')
      g = iand(ishft(rgb, -8), z'ff')
      b = iand(ishft(rgb, -16), z'ff')
      rr = real(r) / 255.0
      gg = real(g) / 255.0
      bb = real(b) / 255.0

      ! set OpenGl color
      call fglColor3f(rr, gg, bb) ! values from 0 - 255
#endif

   end subroutine

   subroutine MoveTo(x, y)
      implicit none
      real(kind=hp), intent(in) :: x
      real(kind=hp), intent(in) :: y

#ifdef HAVE_OPENGL
      xlast = x
      ylast = y
#endif
   end subroutine

   subroutine LineTo(xd, yd)
      implicit none
      real(kind=hp), intent(in) :: xd
      real(kind=hp), intent(in) :: yd

      real(kind=sp) :: x, y
      x = xd
      y = yd

#ifdef HAVE_OPENGL
      call DrawLine(xlast, ylast, x, y)
      xlast = x ! also set xlast & ylast: a LineTo may follow directly without MoveTo in between
      ylast = y
#endif
   end subroutine

   subroutine BeginRender
#ifdef HAVE_OPENGL
      use M_DEVICES
      use m_WEARELT
      use unstruc_colors
      use m_drawthis

      implicit none
      integer :: infoscreen
      real(kind=sp) :: r, g, b

      if (jaOpenGL == 0) then
         InOpenGLRendering = .false.
         return
      end if

      currentWidth = INFOSCREEN(4) ! npx & npy are not always up-to-date it seems..
      currentHeight = INFOSCREEN(5)

      if (HWND <= 0) then ! first time only
         call InitializeOpenGL()
      end if

      if (lastWidth /= currentWidth .or. lastHeight /= currentHeight) then
         call ReInitializeBackBuffer()
      end if

      ! screen coordinates extend
      call fglViewPort(0, 0, currentWidth, currentHeight)
      call fglDisable(GL_DEPTH_TEST) ! no depth

      ! switch to 2d projection (world coordinates)
      call fglMatrixMode(GL_PROJECTION)
      call fglLoadIdentity()

      call fglOrtho(real(X1, dp), real(X2, dp), real(Y1, dp), real(Y2, dp), real(0, dp), real(1, dp)) ! world coordinates extent
      call fglMatrixMode(GL_MODELVIEW)

      ! clear the screen
      ! CALL fglClearColor(.4, .4, .4, 0) ! gray background

      if (ndraw(10) == 0) then
         r = nreds / 255d0
         g = ngreens / 255d0
         b = nblues / 255d0
      else
         r = nredp / 255d0
         g = ngreenp / 255d0
         b = nbluep / 255d0
      end if

      call fglClearColor(real(r, sp), real(g, sp), real(b, sp), real(0, sp)) ! screen background

      call fglClear(GL_COLOR_BUFFER_BIT)

      InOpenGLRendering = .true.
#endif
   end subroutine

   subroutine EndRender
#ifdef HAVE_OPENGL
      use IFWINA
      use M_DEVICES
      use user32
      implicit none
      integer(1) :: res

      if (jaOpenGL == 0) then
         return
      end if

      ! Ignored if single-buffered
      !res = SwapBuffers(hDC) !note: non-blocking
      call fglFinish() ! force completion before continuing with non-opengl stuff

      res = BitBlt(HDC, 0, 0, currentWidth, currentHeight, MEMDC, 0, 0, SRCCOPY)

      InOpenGLRendering = .false.
#endif
   end subroutine

   subroutine ReInitializeBackBuffer
#ifdef HAVE_OPENGL
      use IFWINA ! renamed symbols to avoid conflicts
      use M_DEVICES
      implicit none
      integer(HANDLE) :: ptr_bytes
      integer(1) :: res

      type(T_BITMAPINFO) bmi

      ! We render into a bitmap because not all video cards support mixing gdi / opengl directly. This may mean
      ! we loose hardware acceleration, but even without this it's still significantly faster than rendering
      ! using gdi (interacter).

      if (hbitmap /= 0) then
         res = DeleteObject(hbitmap)
      end if

      lastWidth = currentWidth
      lastHeight = currentHeight

      ! create bitmap
      call ZeroMemory(loc(bmi%bmiHeader), sizeof(bmi%bmiHeader))
      bmi%bmiHeader%biSize = sizeof(bmi)
      bmi%bmiHeader%biWidth = currentWidth
      bmi%bmiHeader%biHeight = currentHeight
      bmi%bmiHeader%biPlanes = 1
      bmi%bmiHeader%biBitCount = 32
      bmi%bmiHeader%biCompression = BI_RGB
      bmi%bmiHeader%biSizeImage = 0
      bmi%bmiHeader%biXPelsPerMeter = 0
      bmi%bmiHeader%biYPelsPerMeter = 0
      bmi%bmiHeader%biClrUsed = 0
      bmi%bmiHeader%biClrImportant = 0

      ! bmi%bmicolors(1) = 0

      ptr_bytes = 0

      hBitmap = CreateDIBSection(memDC, bmi, DIB_RGB_COLORS, ptr_bytes, NULL, 0) ! kleur green is -15?

      oldBitmap = SelectObject(memDC, hBitmap)

#endif
   end subroutine

   subroutine InitializeOpenGl
#ifdef HAVE_OPENGL
      use IFWINA ! renamed symbols to avoid conflicts
      implicit none
      integer(1) :: res
      integer :: pixelFormat, error_code
      type(T_PixelFormatDescriptor) pfd

      HWND = GetActiveWindow()
      HDC = GetDC(HWND) ! get device context of entire screen
      memDC = CreateCompatibleDC(HDC)

      call ReInitializeBackBuffer()

      ! set pixel format
      call ZeroMemory(loc(pfd), sizeof(pfd))
      pfd%nSize = sizeof(pfd)
      pfd%nVersion = 1
      pfd%dwFlags = ior(PFD_DRAW_TO_BITMAP, PFD_SUPPORT_OPENGL)
      pfd%dwFlags = ior(pfd%dwFlags, PFD_SUPPORT_GDI)
      pfd%iPixelType = PFD_TYPE_RGBA
      pfd%cColorBits = 32
      pixelFormat = ChoosePixelFormat(memDC, pfd)
      res = SetPixelFormat(memDC, pixelFormat, pfd)

      ! create render context
      HRC = fwglCreateContext(memDC)
      if (HRC == 0) then
         error_code = GetLastError()
         if (error_code > 0) then
            write (*, *) 'error initialiseopengl:', error_code
         end if
      end if
      res = fwglMakeCurrent(memDC, HRC)

      call SetTextHeight(FontSize)

#endif
   end subroutine

   subroutine SetTextHeight(height)
#ifdef HAVE_OPENGL
      use IFWINA ! renamed symbols to avoid conflicts
#endif
      implicit none

      integer, intent(in) :: height

#ifdef HAVE_OPENGL
      integer(HANDLE) :: font
      integer(1) :: res

      ! prepare the font to render text in
      font = CreateFont(height, 0, 0, 0, & ! font size
                        FW_NORMAL, & ! bold
                        0, & ! italic
                        0, & ! underline
                        0, & ! strikout
                        ANSI_CHARSET, &
                        OUT_TT_PRECIS, &
                        CLIP_DEFAULT_PRECIS, &
                        ANTIALIASED_QUALITY, &
                        ior(FF_DONTCARE, DEFAULT_PITCH), &
                        'Arial') !font name

!    prevFont = SelectObject (hdc, font)
      res = fwglUseFontBitmaps(hdc, 0, 255, 0) ! create the bitmap display lists, we're making images of glyphs 0 thru 254
!    res = SelectObject(hdc, prevFont) ! select old font again
      res = DeleteObject(font) ! delete temporary font

#endif
   end subroutine

end module unstruc_opengl

