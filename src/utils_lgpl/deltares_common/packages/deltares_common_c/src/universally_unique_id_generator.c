/* getuuid.c --
       Get a UUID
*/

#include <stdio.h>

#ifdef WIN32
#include <windows.h>
#else
#include <uuid/uuid.h>
#endif

/* getuuid --
       Get a universally unique ID

   Arguments:
       guid_string   Character string of which the first 40 characters are used
*/
void generate_uuid( unsigned char *guid_string ) {
    int i, j;

    unsigned char guid[16];

#ifdef WIN32
    CoCreateGuid( (GUID *)guid );
#else
    uuid_generate( guid );
#endif

    for ( i = 0; i < 40; i ++ ) {
        guid_string[i] = '\0';
    }

    j = 0;
    for ( i = 0; i < 16; i ++ ) {
        sprintf( &guid_string[j], "%2.2x", guid[i]);
        if ( i == 3 || i == 5 || i == 7 || i == 9 ) {
            j += 1;
            sprintf( &guid_string[j+1], "-" );
        }
        j += 2;
    }
    guid_string[j] = '\0';
}
