#include "globals-fsm.h"

#if ( defined(__cplusplus) || defined(salford32) )
extern "C" {
#endif

#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL 
#   define ESM_INIT_F   FC_FUNC(esm_init_f,ESM_INIT_F)
#   define ESM_CREATE_F FC_FUNC(esm_create_f,ESM_CREATE_F)
#else
#   define STDCALL
#endif

int STDCALL
ESM_INIT_F (   int * flags
    ) 
{
int res;
res=ESM_Init(*flags);
return res;
}

int STDCALL
ESM_CREATE_F (       int *    shared, int *    pagesize
    ) 
{
int res;
res=ESM_Create(*shared, *pagesize);
return res;
}


#if ( defined(__cplusplus) || defined(salford32) )
}
#endif

