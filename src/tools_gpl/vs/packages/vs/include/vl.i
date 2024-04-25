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
/* macro for function to calculate r = a oprtr constant */
#define FUNC( type_a ) \
    static BInt2 gen ## type_a  ( \
	BInt2 oprtr, BInt4 nvar, BData r, BData a, BRea8 * value ) \
    { \
	BInt4 ind; \
\
	switch ( oprtr ) { \
	    case '/': \
		for ( ind = 0 ; ind < nvar ; ind++ ) { \
		    if ( *value == 0.0 ) { \
			((float *)r)[ind] = (float)(UNDEFVALUE); \
		    } \
		    else { \
			((float *)r)[ind] = (float)(((type_a *)a)[ind] \
				/ *value); \
		    } \
		} \
		break; \
\
	    case '+': \
		for ( ind = 0 ; ind < nvar ; ind++ ) { \
		    ((float *)r)[ind] = (float)(((type_a *)a)[ind] \
			    + *value); \
		} \
		break; \
\
	    case '-': \
		for ( ind = 0 ; ind < nvar ; ind++ ) { \
		    ((float *)r)[ind] = (float)(((type_a *)a)[ind] \
			    - *value); \
		} \
		break; \
\
	    case '*': \
		for ( ind = 0 ; ind < nvar ; ind++ ) { \
		    ((float *)r)[ind] = (float)(((type_a *)a)[ind] \
			    * *value); \
		} \
		break; \
\
	    default: \
		return -1; \
	} \
\
	return 0; \
    }

#define GENFUNC \
    FUNC ( BRea8 ) \
    FUNC ( BRea4 ) \
    FUNC ( BInt4 ) \
    FUNC ( BInt2 )

/* The following statement generates functions for operations
   on arrays of different storage types */
GENFUNC


/* This function is called fro all data types not suitable
   for the desired operations */
static BInt2 gen_error ( 
    BInt2 oprtr, BInt4 ind, BData a, BData b, BRea8 * value ) 
{
    return -1;
}

#define GEN( type ) \
    gen ## type ,

#define GENPNTRS \
    gen_error,		/* no DBLCPL */ \
    gen_error,		/* no COMPLEX */ \
    GEN( BRea8 ) \
    GEN( BRea4 ) \
    GEN( BInt4 ) \
    GEN( BInt2 ) \
    gen_error,		/* no BOOL */ \
    gen_error,		/* no BOOLSHRT */ \
    gen_error,		/* no CHAR */ \
    gen_error,		/* no UNDEF */


/*  @@
 */
void FU_simple (
    BText   name_a,    /* b = a oprtr value */
    BText   name_b,
    BRea8 * value,
    BText   arith_oper /* is either "+" or "-" or "*" or "/" */
    )
{
    VsVarData   pa;
    VsVarData   pn;
    BRea4     * varpnt;
    BInt4       nvar;
    BInt2       retcod;
    NfDtp       type_a;
    static BInt2 ( * genfunc[10] ) (
	BInt2, BInt4, BData, BData, BRea8 * ) =
	{
	    GENPNTRS
	};

    /* first check if variables a exists */
    if ( ( pa = VR_get_pointer_to_variable ( name_a )) == NULL ) {
	GEN_message_to_errorfile ( 101, name_a );
	return;
    } 

    /* check if variable b not alredy exists */
    if ( VR_get_pointer_to_variable ( name_b ) != NULL ) {
	GEN_message_to_errorfile ( 9 );
	return;
    }

    nvar = PP_calc_number_of_variables ( pa );

    /* the operations always return float */
    varpnt = (float *)GEN_malloc ( nvar * sizeof ( float ) );

    type_a = VR_get_data_type ( pa );

    retcod = ( *genfunc[type_a]) (
	    arith_oper[0], nvar, varpnt, pa->varpnt, value );

    if ( retcod == 0 ) {
	if ( ( pn = VR__alloc_memory_for_variable () ) != NULL ) {

	    int i;

	    (void)strcpy ( pn->varnam, name_b );
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
    	GEN_message_to_errorfile ( 109 ,name_b );
	GEN_free ( varpnt );
    }
}
