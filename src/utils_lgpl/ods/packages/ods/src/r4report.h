/* r4report.h   (c)Copyright Sequiter Software Inc., 1991-2024.  All rights reserved. */

#define text4displayed 1
#define text4disp_once 2
#define text4display_always 3

#define obj4expr 0
#define obj4label 1
#define obj4field 2
#define obj4total 3

#define text4number 0
#define text4exponent 1
#define text4dollar 2
#define text4percent 3

#define text4left 0
#define text4center 1
#define text4right 2

#define total4lowest 11
#define total4highest 12
#define total4count 13
#define total4average 14
#define total4sum 15


#ifdef S4WINDOWS
typedef struct tagRECT R4RECT;
typedef HDC R4HDC;
typedef HWND R4HWND;
typedef struct tagLOGFONT R4LOGFONT;
typedef R4LOGFONT* PR4LOGFONT;
typedef UINT R4UINT;
typedef BYTE R4BYTE;
#endif

#ifndef S4WINDOWS

   #define LF_FACESIZE         32

   typedef struct r4RECT
   {
      short left;
      short top;
      short right;
      short bottom;
   } R4RECT ;

   typedef unsigned short HFONT;

   typedef unsigned short R4HDC ;
   typedef unsigned short R4HWND;
   typedef unsigned char  R4BYTE;
   typedef unsigned int   R4UINT;

   typedef unsigned short      WORD;
   typedef unsigned long       DWORD;

   typedef struct tagLOGFONT
   {
      short int lfHeight;
      short int lfWidth;
      short int lfEscapement;
      short int lfOrientation;
      short int lfWeight;
      R4BYTE      lfItalic;
      R4BYTE    lfUnderline;
      R4BYTE    lfStrikeOut;
      R4BYTE    lfCharSet;
      R4BYTE    lfOutPrecision;
      R4BYTE    lfClipPrecision;
      R4BYTE    lfQuality;
      R4BYTE    lfPitchAndFamily;
      R4BYTE    lfFaceName[LF_FACESIZE];
   } R4LOGFONT, *PR4LOGFONT ;

   typedef long COLORREF;
#ifndef __IBMC__
#ifndef __WATCOMC__
   typedef unsigned HANDLE;
#endif
#endif

   typedef struct tagPOINT {
      int x;
      int y;
   } POINT ;

#endif

typedef unsigned MBITMAP;



struct REPORT4st ;
struct TOTAL4st ;
struct GROUP4st ;
struct OBJECTS4st ;

typedef struct
{
   LINK4 link ;
   int object_type ;  /* obj4expr, obj4label, */
                      /* obj4total, obj4field */
   int x,y ; /* Do not change order of x,y;  Used as a Windows point */
   int w ;  /* In inches */
   R4RECT r ;  /* In device coordinates */
             /* Calculated just before report */
   int display_status ;/* text4display_once, text4displayed, */
                    /* text4display_always */
   struct OBJECTS4st S4PTR *list ; /* Pointer to the corresponding list */
   R4HWND  hWnd ;        /* Handle to corresponding design window. */
   int   is_selected ;
} OBJ4 ;

typedef struct
{
   LINK4 link ;
   OBJ4  S4PTR *ptr ;
} OBJ4PTR ;

typedef struct
{
   LINK4  link ;
   char name[20] ;
   HFONT   hFont ;
   R4LOGFONT  lf ;
   COLORREF  color ;
   int iptsize ;
   int codes_before_len, codes_after_len ;
   char S4PTR *codes_before, S4PTR *codes_after ;
} STYLE4 ;

typedef struct
{
   OBJ4 obj ;
   STYLE4 S4PTR *style ;
   int  len_max ;    /* The maximum number of characters to display */
   int  len ;
   int  dec ;
   int  alignment ;    /* text4left, text4center, text4right */
   int  numeric_type ; /* text4exponent, text4dollar, */
                       /* text4percent, text4number */
   char use_brackets ; /* TRUE/FALSE - Use brackets for negative numbers ? */
   char display_zero ;
   char S4PTR *date_format ; /* If null, use default */

   EXPR4 S4PTR *expr ;             /* For expression */
   FIELD4 S4PTR *field ;           /* For field */
   char S4PTR *ptr ;               /* For label */
   struct TOTAL4st S4PTR *total ;  /* For total */
   struct GROUP4st S4PTR *reset_group ;
} TEXT4 ;

typedef struct TOTAL4st
{
   LINK4 link ;
   EXPR4CALC S4PTR *calc_ptr ; /* A total is a calculation for easy removal/name changes */

   short  total_type ;  /* total4lowest, total4highest, total4sum, */
                      /* total4count, total4average */
   struct GROUP4st S4PTR *reset_level ;
   struct REPORT4st S4PTR *report ;
   double total ;  /* Running Total */
   double low ;    /* Lowest Value */
   double high ;   /* Hightest Value */
   long count ;    /* Running Count */
} TOTAL4 ;

typedef struct OBJECTS4st
{
   LIST4 list ; /* A list of OBJ4 */
   struct GROUP4st S4PTR *group ; /* The corresponding Group; if any */
   int height ; /* In 1/1000 inch */
   int height_dev ; /* In device units */
   struct REPORT4st S4PTR *report ;
   R4HWND  hWnd ;  /* Handle to corresponding design window. */
   R4HWND  hWndInfo ;  /* Status information of object list */
} OBJECTS4 ;

typedef struct GROUP4st
{
   LINK4 link ;
   struct REPORT4st S4PTR *report ;

   EXPR4   S4PTR *expr ;
   char    S4PTR *value ;
   int     value_len ;

   int position ;  /* Linked list position; used to figure total resets */

   OBJECTS4  header ;
   int swap_header ;
   int repeat_header ;

   OBJECTS4  footer ;  /* List of DisplayObject */
   int swap_footer ;

   int reset_page ;

   char label[12] ;
} GROUP4 ;

typedef struct REPORT4st
{
   CODE4   S4PTR *cb ;
   RELATE4 S4PTR *relate ;  /* Structure R4RELATION is not part of the interface */

   MEM4 S4PTR *text_memory ;
   MEM4 S4PTR *style_memory ;

   LIST4  groups ;       /* A list of GROUP4 */
   LIST4  styles ;       /* A list of STYLE4, Selected is Default. */

   STYLE4 S4PTR *def_style;

   OBJECTS4  page_header ;
   OBJECTS4  page_footer ;

   OBJECTS4  title ;      
   OBJECTS4  summary ;    

   long  page_no ;
   long  rec_no ;
   int   y ;         /* 0 - Top of page. */
   int   in_new_page ;  /* Insure that 'new_page()' is not called recursively */
   int   first ;

   char S4PTR *printer_name ;
   char S4PTR *report_name ;
   R4HDC   hDC ; /* Display Context Handle for report output. */

   int   margin_top ;
   int   margin_bottom ;
   int   margin_left ;
   int   report_width ;
   int   margin_right ;
   char  decimal_point ;
   char  thousand_separator ;
   char  currency_sym ;
   char  leading_zero ;

   MBITMAP hBitMap ;
   R4HWND    hWnd ;
   R4HDC     hDCdisplay ;
   int     report_command ;
   R4HWND    hWndParent ;

   int     to_screen ;  /* In Windows True if the output is to go to the screen. False if to printer. */
   int     is_registered ;

   /* Information for designer. */
   int hide_info ; /* True/False - Hide list information windows ? */
   int   units ;  /* units4inches; units4cent; units4points */

   int sensitivity_x ; /* In 1/1000 of an inch.  Modulus on object pos. */
   int sensitivity_y ;
   int sensitivity_adjust ; /* Adjust object positions when sensitivity changed */
   int sheight, swidth, cline, cx ;
   int output_handle,use_styles ;
} REPORT4 ;


#ifdef __cplusplus
   extern "C" {
#endif


#ifdef S4WINDOWS
   long FAR PASCAL _export report4output_proc( R4HWND, unsigned, unsigned, LONG ) ;
   long FAR PASCAL _export report4cancel_proc( R4HWND, unsigned, unsigned, LONG ) ;
#endif

int S4FUNCTION e4string( EXPR4 S4PTR *, char S4PTR * S4PTR *, int, int ) ;
int S4FUNCTION dc4mapping( R4HDC hDC ) ; /* Sets coordinates to 1/1000 Inch */

int  S4FUNCTION obj4display( OBJ4 S4PTR *, REPORT4 S4PTR *, int ) ;
int  obj4width( OBJ4 S4PTR *, int ) ;  /* Sets the maximuS4PTR m width */
void S4FUNCTION obj4free( OBJ4 S4PTR * ) ;

OBJ4 S4PTR * S4FUNCTION obj4first( REPORT4 S4PTR * ) ;
OBJ4 S4PTR * S4FUNCTION obj4next( OBJ4 S4PTR * ) ;

OBJECTS4 S4PTR * S4FUNCTION objects4first( REPORT4 S4PTR * ) ;
OBJECTS4 S4PTR * S4FUNCTION objects4next( OBJECTS4 S4PTR * ) ;

int  S4FUNCTION objects4display( OBJECTS4 S4PTR * ) ;
int  S4FUNCTION objects4height( OBJECTS4 S4PTR *, int ) ; /* Returns the height of the list */
void S4FUNCTION objectsort4init( OBJECTS4 S4PTR *, REPORT4 S4PTR *, GROUP4 S4PTR * ) ;
void S4FUNCTION objects4purge( OBJECTS4 S4PTR * ) ;


int S4FUNCTION report4display_repeat_headers(  REPORT4 S4PTR *, GROUP4 S4PTR * ) ;
int S4FUNCTION report4new_page( REPORT4 S4PTR *, int ) ;  /* Ejects, increments page# */

int S4FUNCTION report4update_expressions( REPORT4 S4PTR * ) ;

/* User Interface */
void   S4FUNCTION text4alignment( TEXT4 S4PTR *, int ) ;
void   S4FUNCTION text4numeric( TEXT4 S4PTR *, int ) ;
void   S4FUNCTION text4dec( TEXT4 S4PTR *, int ) ;
void   S4FUNCTION text4display_zero( TEXT4 S4PTR *, int ) ;
void   S4FUNCTION text4len_max( TEXT4 S4PTR *, int ) ;
void   S4FUNCTION text4len_set( TEXT4 S4PTR * ) ;
void   S4FUNCTION text4use_brackets( TEXT4 S4PTR *, int ) ;
void   S4FUNCTION text4display_once( TEXT4 S4PTR *, GROUP4 S4PTR * );
int    S4FUNCTION text4convert_double( OBJ4 S4PTR *, double, char S4PTR * ) ; /* Returns # of character's used, trim's on left. */
int    S4FUNCTION text4conv_double( TEXT4 S4PTR *, double , char * );
int    S4FUNCTION text4width( TEXT4 S4PTR *, int );
int    S4FUNCTION text4date_format( TEXT4 S4PTR *, char S4PTR * ) ;
char   *text4ptr( TEXT4 S4PTR * ) ;

TEXT4  S4PTR * S4FUNCTION text4create( OBJECTS4 S4PTR *, int, int ) ; /* Does initial create work */
TEXT4  S4PTR * S4FUNCTION text4expr(  OBJECTS4 S4PTR *, int, int, char S4PTR * ) ;
TEXT4  S4PTR * S4FUNCTION text4field( OBJECTS4 S4PTR *, int, int, FIELD4 S4PTR * ) ;
TEXT4  S4PTR * S4FUNCTION text4label( OBJECTS4 S4PTR *, int, int, char S4PTR * ) ;
TEXT4  S4PTR * S4FUNCTION text4total( OBJECTS4 S4PTR *, int, int, TOTAL4 S4PTR * ) ;
STYLE4 S4PTR * S4FUNCTION text4style( TEXT4 S4PTR *, STYLE4 S4PTR * ) ;

#ifdef S4WINDOWS
   int S4FUNCTION text4width_estimate( TEXT4 S4PTR *, TEXTMETRIC * ) ; /* Estimate the width (if max. use it) */
#endif

unsigned S4FUNCTION u4ncat( char S4PTR *, char S4PTR *, unsigned ) ;

int      S4FUNCTION report4program( REPORT4 S4PTR *, char S4PTR *, char S4PTR *, int );
int      S4FUNCTION report4display( REPORT4 S4PTR *, int ) ; /* Display report to screen ? */
int      S4FUNCTION report4do( REPORT4 S4PTR * ) ;
int      S4FUNCTION report4dc_free( REPORT4 S4PTR * ) ;
int      S4FUNCTION report4dc_page_height( REPORT4 S4PTR * ) ; /* In 1/1000 of an Inch */
int      S4FUNCTION report4layout( REPORT4 S4PTR * ) ; /* Create an instant layout for the report */
int      S4FUNCTION report4printer( REPORT4 S4PTR *, char S4PTR * ) ;
int      S4FUNCTION report4save( REPORT4 S4PTR *, char S4PTR * ) ;
int      S4FUNCTION report4currency(REPORT4 S4PTR *, char );
int      S4FUNCTION report4symbols_numeric(REPORT4 S4PTR *, int, int ) ;
int      S4FUNCTION group4expr( GROUP4 S4PTR *, char S4PTR * ) ; /* Set condition for new group */
int      S4FUNCTION group4name_set( GROUP4 S4PTR *, char S4PTR * );
void     S4FUNCTION report4leading_zero( REPORT4 S4PTR *, int ) ;
void     S4FUNCTION report4message_loop( REPORT4 S4PTR * ) ;
void     S4FUNCTION report4parent( REPORT4 S4PTR *, R4HWND ) ;
void     S4FUNCTION report4register_classes( REPORT4 S4PTR *) ;
void     S4FUNCTION report4free( REPORT4 S4PTR *, int, int ) ;

R4HDC      S4FUNCTION report4dc_get( REPORT4 S4PTR * ) ;

GROUP4   S4PTR * S4FUNCTION report4calc_first_change_group( REPORT4 S4PTR * ) ;
GROUP4   S4PTR * S4FUNCTION group4lookup( REPORT4 S4PTR *, char S4PTR * ) ;
GROUP4   S4PTR * S4FUNCTION group4create( REPORT4 S4PTR * ) ;

REPORT4  S4PTR * S4FUNCTION report4retrieve( CODE4 S4PTR *, char S4PTR *, int ) ;
REPORT4  S4PTR * S4FUNCTION report4init( RELATE4 S4PTR * ) ;

void     S4FUNCTION report4driver_init_undo(void);
void     S4FUNCTION report4output( REPORT4 S4PTR *, int, char S4PTR *, int );
int      S4FUNCTION report4driver_init( REPORT4 S4PTR *, int, char S4PTR * );
int      S4FUNCTION report4driver_new_page(void);
int      S4FUNCTION report4driver_write( int, int, char S4PTR *, int, 
                   char S4PTR *, int, char S4PTR *, int );



TOTAL4   S4PTR * S4FUNCTION total4create( REPORT4 S4PTR *, char S4PTR *, char S4PTR *, int ) ;
TOTAL4   S4PTR * S4FUNCTION total4create_total( REPORT4 S4PTR *, char S4PTR *, EXPR4 *, int ) ;
TOTAL4   S4PTR * S4FUNCTION total4lookup( REPORT4 S4PTR *, char S4PTR * ) ; /* Must call from expr4parse */
double   S4FUNCTION total4value( struct TOTAL4st S4PTR * ) ; /* Must call from e4new_total */
void     S4FUNCTION total4reset_level( TOTAL4 S4PTR *, GROUP4 S4PTR * ) ;
void     S4FUNCTION total4value_reset(TOTAL4 S4PTR *);
void     S4FUNCTION total4value_update( TOTAL4 S4PTR *, GROUP4 S4PTR * );

void     S4FUNCTION style4color( STYLE4 S4PTR *, COLORREF ) ;
int      S4FUNCTION style4default_create( REPORT4 S4PTR * ) ;

#ifdef   S4WINDOWS
STYLE4   S4PTR * S4FUNCTION style4create( REPORT4 S4PTR *, char S4PTR *, LOGFONT S4PTR * ) ;
#endif

#ifndef  S4WINDOWS
STYLE4   S4PTR * S4FUNCTION style4create( REPORT4 S4PTR *, char S4PTR *,
                       char S4PTR *, int, char S4PTR *, int );
#endif

STYLE4   S4PTR * S4FUNCTION style4default_set( REPORT4 S4PTR *, STYLE4 S4PTR * ) ;
STYLE4   S4PTR * S4FUNCTION style4lookup( REPORT4 S4PTR *, char S4PTR * ) ;

void     S4FUNCTION report4page_size( REPORT4 S4PTR *, int, int ) ;
int      S4FUNCTION report4position( int, int ) ;

int S4FUNCTION  report4parse_sstring( char S4PTR *, char S4PTR *, int);
void S4FUNCTION report4to_hex(char S4PTR *, char S4PTR *);
void S4FUNCTION report4unparse_sstring( char S4PTR *, char S4PTR *, int );

#ifdef __cplusplus
   }
#endif

