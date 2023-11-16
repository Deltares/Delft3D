// errno for fun and profit

#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>

#define PTR8

#include "btps.h"
#include "nef-def.h"
#include "nef-tag.h"

typedef struct RETRIEVE {
  BInt4  hash_key            ;
  BChar  grp_name[MAX_NAME+1];
  BChar  elm_name[MAX_NAME+1];
  BUInt4 grp_num_dim         ;
  BUInt4 grp_dimens[MAX_DIM] ;
  BUInt4 grp_order [MAX_DIM] ;
  BUInt8 grp_pointer        ;
  BInt4  elm_dimens[MAX_DIM] ;
  BUInt4 elm_offset         ;
  BInt4  elm_num_dim         ;
  BInt4  elm_num_dimens      ;
  BChar  elm_type[MAX_TYPE+1];
  BInt4  elm_single_bytes    ;
  BUInt8 cel_num_bytes       ;
  BUInt8 write_bytes         ;
  struct RETRIEVE * left    ;
  struct RETRIEVE * right   ;
} retrieve;

static int first_create   = TRUE;
retrieve * free_retrieve_ptrs( retrieve * p );
static BInt4   first_retrieve = TRUE;
BInt4   clean_up;

BInt4   incr_nefis_files;
BInt4   nr_nefis_files;
BInt4   cap_nefis_files;
BInt4   set;

retrieve * array_retrieve_ptrs[MAX_NEFIS_FILES];
BUInt8 *** retrieve_var;
BUInt8 *** new_capacity_retrieve_var( BUInt8 *** retrieve, BInt4 length, BInt4 new_length);


int main( int argc, char **argv )
{
int i, j;
int icnt;

retrieve_var = NULL;
first_retrieve = TRUE;
incr_nefis_files = 1;
nr_nefis_files  = 0;
cap_nefis_files = 0;

for ( icnt=0; icnt < 100; icnt++)
{
fprintf(stdout,"Counting: %d\n", icnt);

for (set= 0; set<50; set++)
{
fprintf(stdout,"\tNEFIS-set: %d\n", set);
	/*
	 * Create the retrieve_var array
	 */
    if ( cap_nefis_files == 0 )
    {
      cap_nefis_files = 1;
      retrieve_var = (BUInt8 ***) malloc ( cap_nefis_files * sizeof(BUInt8) );
      for ( i=0; i<cap_nefis_files; i++ )
      {
        retrieve_var[i] = (BUInt8 **) malloc ( MAX_VAR_GROUPS * sizeof(BUInt8) );
      }
      for ( i=0; i<cap_nefis_files; i++ )
      {
        for ( j=0; j<MAX_VAR_GROUPS ; j++ )
        {
          retrieve_var[i][j]    = (BUInt8 *) malloc ( 1 * sizeof(BUInt8) );
          retrieve_var[i][j][0] = (BUInt8) ULONG_MAX;
        }
      }
    }
    if (set >= cap_nefis_files)
    {
        cap_nefis_files += incr_nefis_files;
        retrieve_var = new_capacity_retrieve_var( retrieve_var, cap_nefis_files-incr_nefis_files, cap_nefis_files);
    }
} // end for-loop set

fprintf(stdout,"\tCleaning up\n");

/*
 * Free the retrieve_var array if all nefis file set are closed
 * and set first_retrieve TRUE
 */
clean_up = TRUE;
for ( i=0; i<cap_nefis_files; i++ )
{
    nefis[i].exist   = -1;
	if ( nefis[i].exist != -1 )
	{
		clean_up = FALSE;
		break;
	}
	else
	{
		/* invalidate the pointers for this file set */
		for ( j=0; j<MAX_VAR_GROUPS ; j++ )
		{
			retrieve_var[i][j][0] = (BUInt8) ULONG_MAX;
		}
	}
}

	if ( clean_up == TRUE )
	{
		for ( i=0; i<cap_nefis_files; i++ )
		{
fprintf(stdout,"\t\tNEFIS-set: %d\n", i);
			for ( j=0; j<MAX_VAR_GROUPS ; j++ )
			{
				free ((BData) retrieve_var[i][j] );
				retrieve_var[i][j] = NULL;
			}
			free ( (BData) retrieve_var[i] );
			retrieve_var[i] = NULL;
			/*
			* Free the link-list retrieve pointers
			*/
			if ( array_retrieve_ptrs[i] != NULL )
			{
				array_retrieve_ptrs[i] = free_retrieve_ptrs( array_retrieve_ptrs[i] );
			}
		}
		free ( (BData) retrieve_var);
		retrieve_var = NULL;
		first_retrieve = TRUE;
		nr_nefis_files  = 0;
		cap_nefis_files = 0;
	}
} // end for-loop icnt
} // end main function

/*==========================================================================*/
retrieve * free_retrieve_ptrs( retrieve * p )
{
    if (p->left  != NULL)
    {
        p->left  = free_retrieve_ptrs( p->left  );
    }
    if (p->right != NULL)
    {
        p->right = free_retrieve_ptrs( p->right );
    }
    free((BData)p);
    p = NULL;

    return p;
}
/*==========================================================================*/
BUInt8 *** new_capacity_retrieve_var( BUInt8 *** retrieve, BInt4 length, BInt4 new_length)
{
    long i, j;

    retrieve = (BUInt8***) realloc(retrieve, (new_length)*sizeof(BUInt8));
    for (i=length; i<new_length; i++)  {
        retrieve[i] = (BUInt8 **) malloc(MAX_VAR_GROUPS * sizeof(BUInt8) );
    }
    for (j=0; j<MAX_VAR_GROUPS; j++)  {
        for (i=length; i<new_length; i++)  {
            retrieve[i][j] = (BUInt8 *) malloc(1 * sizeof(BUInt8) );
            retrieve[i][j][0] = (BUInt8) ULONG_MAX;
        }
    }

    return retrieve;
}
