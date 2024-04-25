//---- GPL ---------------------------------------------------------------------
//                                                                              
// Copyright (C)  Stichting Deltares, 2011-2024.                                
//                                                                              
// This program is free software: you can redistribute it and/or modify         
// it under the terms of the GNU General Public License as published by         
// the Free Software Foundation version 3.                                      
//                                                                              
// This program is distributed in the hope that it will be useful,              
// but WITHOUT ANY WARRANTY; without even the implied warranty of               
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
// GNU General Public License for more details.                                 
//                                                                              
// You should have received a copy of the GNU General Public License            
// along with this program.  If not, see <http://www.gnu.org/licenses/>.        
//                                                                              
// contact: delft3d.support@deltares.nl                                         
// Stichting Deltares                                                           
// P.O. Box 177                                                                 
// 2600 MH Delft, The Netherlands                                               
//                                                                              
// All indications and logos of, and references to, "Delft3D" and "Deltares"    
// are registered trademarks of Stichting Deltares, and remain the property of  
// Stichting Deltares. All rights reserved.                                     
//                                                                              
//------------------------------------------------------------------------------
// $Id$
// $HeadURL$
/* macro for function to calculate r = a oprtr b */
#define FUNC( type_a, type_b ) \
    static BInt2 gen ## type_a ## with ## type_b ( \
	BInt2 oprtr, BInt4 nvar, BData r, BData a, BData b ) \
    { \
	BInt4 ind; \
\
	switch ( oprtr ) { \
	    case '/': \
		for ( ind = 0 ; ind < nvar ; ind++ ) { \
		    if ( ((type_b *)b)[ind] == 0.0 ) { \
			((float *)r)[ind] = (float)(UNDEFVALUE); \
		    } \
		    else { \
			((float *)r)[ind] = (float)(((type_a *)a)[ind] \
				/ ((type_b *)b)[ind]); \
		    } \
		} \
		break; \
\
	    case '+': \
		for ( ind = 0 ; ind < nvar ; ind++ ) { \
		    ((float *)r)[ind] = (float)(((type_a *)a)[ind] \
			    + ((type_b *)b)[ind]); \
		} \
		break; \
\
	    case '-': \
		for ( ind = 0 ; ind < nvar ; ind++ ) { \
		    ((float *)r)[ind] = (float)(((type_a *)a)[ind] \
			    - ((type_b *)b)[ind]); \
		} \
		break; \
\
	    case '*': \
		for ( ind = 0 ; ind < nvar ; ind++ ) { \
		    ((float *)r)[ind] = (float)(((type_a *)a)[ind] \
			    * ((type_b *)b)[ind]); \
		} \
		break; \
\
	    default: \
		return -1; \
	} \
\
	return 0; \
    }

#define GENFUNCX( type_a ) \
    FUNC ( type_a, BRea8 ) \
    FUNC ( type_a, BRea4 ) \
    FUNC ( type_a, BInt4 ) \
    FUNC ( type_a, BInt2 )

#define GENFUNC \
    GENFUNCX ( BRea8 ) \
    GENFUNCX ( BRea4 ) \
    GENFUNCX ( BInt4 ) \
    GENFUNCX ( BInt2 ) 
    
/* The following statement generates functions for operations
   on arrays of different storage types */
GENFUNC


/* This function is called fro all data types not suitable
   for the desired operations */
static BInt2 gen_error ( 
    BInt2 oprtr, BInt4 ind, BData a, BData b, BData c ) 
{
    return -1;
}

#define GENP( type1, type2 ) \
    gen ## type1 ## with ## type2 ,

#define GENPNTRS \
    GEN_DUM			/* no DBLCPL */ \
    GEN_DUM			/* no COMPLEX */ \
    GEN( BRea8 ) \
    GEN( BRea4 ) \
    GEN( BInt4 ) \
    GEN( BInt2 ) \
    GEN_DUM			/* no BOOL */ \
    GEN_DUM			/* no BOOLSHRT */ \
    GEN_DUM			/* no CHAR */ \
    GEN_DUM			/* no UNDEF */

#define GEN_DUM \
    gen_error, \
    gen_error, \
    gen_error, \
    gen_error, \
    gen_error, \
    gen_error, \
    gen_error, \
    gen_error, \
    gen_error, \
    gen_error, 

#define GEN( type_a ) \
    gen_error,			/* no DBLCPL */ \
    gen_error,			/* no COMPLEX */ \
    GENP( type_a, BRea8 ) \
    GENP( type_a, BRea4 ) \
    GENP( type_a, BInt4 ) \
    GENP( type_a, BInt2 ) \
    gen_error, \
    gen_error, \
    gen_error, \
    gen_error, 

/*  @@
 */
void FU_basics (
    BText name_a,
    BText name_b,
    BText name_c,
    BText arith_oper /* is either "+" or "-" or "*" or "/" */
    )
{
    VsVarData   pa;
    VsVarData   pb;
    VsVarData   pn;
    BRea4     * varpnt;
    BInt4       nvar;
    BInt4       retcod;
    BInt4       type_a;
    BInt4       type_b;
    static BInt2 ( * genfunc[10][10] ) (
	BInt2, BInt4, BData, BData, BData ) =
	{
	    GENPNTRS
	};

    /* first check if variables a and b exist */
    if ( ( pa = VR_get_pointer_to_variable ( name_a )) == NULL ) {
	GEN_message_to_errorfile ( 101, name_a );
	return;
    } 

    if ( ( pb = VR_get_pointer_to_variable ( name_b )) == NULL ) {
	GEN_message_to_errorfile ( 101, name_b );
	return;
    } 

    /* check if variable c not alredy exists */
    if ( VR_get_pointer_to_variable ( name_c ) != NULL ) {
	GEN_message_to_errorfile ( 9 );
	return;
    }

    /* check if a and b have the same structure */
    if ( VR_same_structure ( pa, pb ) != 0 ) {
	GEN_message_to_errorfile ( 108 ,name_a, name_b );
	return;
    }	

    nvar = PP_calc_number_of_variables ( pa );

    /* the operations always return float */
    varpnt = (float *)GEN_malloc ( nvar * sizeof ( float ) );

    type_a = VR_get_data_type ( pa );
    type_b = VR_get_data_type ( pb );

    retcod = ( *genfunc[type_a][type_b] ) (
	    arith_oper[0], nvar, varpnt, pa->varpnt, pb->varpnt );

    if ( retcod == 0 ) {
	if ( ( pn = VR__alloc_memory_for_variable () ) != NULL ) {

	    BInt4 i;

	    (void)strcpy ( pn->varnam, name_c );
	    (void)strcpy ( pn->elmtyp, "REAL" );
	    pn->nbytsg = 4;
	    (void)strcpy ( pn->grpnam, "--Derived--" );
	    (void)strcpy ( pn->elmnam, arith_oper  );
	    pn->grpndm = pa->grpndm;
	    pn->elmndm = pa->elmndm;

	    for ( i = 0 ; i < pa->grpndm; i++ ) {
		pn->grpdms[i] = pa->grpdms[i];
	    }
	    for ( i = 0 ; i < pa->elmndm; i++ ) {
		pn->elmdms[i] = pa->elmdms[i];
	    }
	    pn->varpnt = varpnt;
	    VR_add_variable_to_memo ( pn );
	}
	else {
	    GEN_message_to_errorfile ( 1 );
	    GEN_free ( varpnt ) ;
	}
    }
    else {
    	GEN_message_to_errorfile ( 109 ,name_c );
	GEN_free ( varpnt );
    }
}
