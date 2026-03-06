#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "general.h"

// Test
void main( int argc, char *argv[] )
{
    char *ModelID   = "RobTest";
    char *TestSetID = "TestSet ID";
    char *Quantity[]    = {"q1","q2","q2_1", "q2_2","q2_3"};
    char *Description[] = {"q1_description","q2_description","q2_1_description", "q2_2_description", "q2_3_description"};
    char *Unit[]        = {"m3/s","kg/m3","m"};
    char *ElementID[]   = {"Element1","Element2","Element3"};
    FILE *fp=fopen("test.txt","w");
    int  n,j,k,m;
    char *s;
    double D0,D1;
    double *pD0, *pD1;

    n = RobInitialize("RobTest");

    n = RobModelAdd (ModelID, ModelID);

    // Add som Units
    for (j=0;j<3;j++)
    {
        n = RobUnitAdd(Unit[j], "Unit description", (double) 1., (double)2.);
    }

    // Quantities (with properties)
    fprintf(fp,"\nAdd properties to first two quantities");
    for (j=0;j<2;j++)
    {
        n=  RobQuantityAdd ((const char*)Quantity[j] , (const char*) Description[j], (const char*)Unit[0]);
        n = RobQuantityAddProperty(NULL,Quantity[j],"Property 1", "Property Value 1");
        n = RobQuantityAddProperty(NULL,Quantity[j],"Property 2", "Property Value 2");
    }
    RobDumpQuantities(fp);

    // Get the value for some properties
    s = RobQuantityPropertyGetValue(NULL,"q1","Property 1");
    s = RobQuantityPropertyGetValue(NULL,"q1","Property 2");
    
    fprintf(fp,"\nCreate Quantityset and add properties to first two quantities in set");
    // Create a Quantityset by adding Quantities to an existing Quantity
    n=  RobQuantityAddQuantity ((const char*)Quantity[1], (const char*)Quantity[2] ,(const char*) Description[2], (const char*)Unit[1]);
    n=  RobQuantityAddQuantity ((const char*)Quantity[1], (const char*)Quantity[3] ,(const char*) Description[3], (const char*)Unit[2]);
    n=  RobQuantityAddQuantity ((const char*)Quantity[1], (const char*)Quantity[4] ,(const char*) Description[4], (const char*)Unit[2]);

    // Add properties to Quantities in set
    for (j=2;j<4;j++)
    {
        n = RobQuantityAddProperty(Quantity[1],Quantity[j],"Property 1", "Property Value 1");
        n = RobQuantityAddProperty(Quantity[1],Quantity[j],"Property 2", "Property Value 2");
    }    	
    RobDumpQuantities(fp);

    // Set a new value for property
    fprintf(fp,"\nChange property value for the second quantities in the set");
	  n = RobQuantityPropertySetValue("q2",Quantity[3],"Property 2","New Property Value");
    s = RobQuantityPropertyGetValue("q2",Quantity[3],"Property 2");
    fprintf(fp,"\nValue now \"%s\"",s);

    // Remove second, first & last Quantity
    fprintf(fp,"\nRemove : %s\n",Quantity[3]);
    n = RobQuantitysetRemoveQuantity(Quantity[1],Quantity[3]);
    RobDumpQuantities(fp);

    fprintf(fp,"\nRemove : %s\n",Quantity[2]);
    n = RobQuantitysetRemoveQuantity(Quantity[1],Quantity[2]);
    RobDumpQuantities(fp);

    fprintf(fp,"\nRemove : %s\n",Quantity[4]);
    n = RobQuantitysetRemoveQuantity(Quantity[1],Quantity[4]);
    RobDumpQuantities(fp);

    // (Re)Create the Quantityset
    n=  RobQuantityAddQuantity ((const char*)Quantity[1], (const char*)Quantity[2] ,(const char*) Description[2], (const char*)Unit[1]);
    n=  RobQuantityAddQuantity ((const char*)Quantity[1], (const char*)Quantity[3] ,(const char*) Description[3], (const char*)Unit[2]);
    n=  RobQuantityAddQuantity ((const char*)Quantity[1], (const char*)Quantity[4] ,(const char*) Description[4], (const char*)Unit[2]);

    fprintf(fp,"\n\nRecreated Set :\n");
    RobDumpQuantities(fp);

    // Create an ElementSet
    n = RobElementsetAdd(ModelID, TestSetID, "TestSet Description", IDBASED);
    for (j=0;j<3;j++)
    {
        n = RobElementsetAddElementIDBased(ModelID, TestSetID, ElementID[j]);
    }
    fprintf(fp,"\nElementSets :\n");
    RobDumpElementsets(fp);

    // Create the ExchangeItems for the Quantity and Quantityset
    for(k=0;k<2;k++)
    {
        n = RobExchangeitemAdd (ModelID, Quantity[k], TestSetID, PROVIDING);
        n = RobExchangeitemAdd (ModelID, Quantity[k], TestSetID, ACCEPTING);
    }

    fprintf(fp,"\nExchangeItems (empty) :\n");
    RobDumpExchangeitems(fp);

    // Check RobPutDouble (single Quantity)
    D0 = 10.;
    D1 = 20.;

    fprintf(fp,"\nRobPutDouble :\n");
    n = RobPutDouble(ModelID, NULL, Quantity[0],TestSetID, PROVIDING, 1, &D0 );
    fprintf(fp,"RobPutDouble (all elements same value) : Quantity %s  ElementID %s PROVIDING %f\n",Quantity[0],TestSetID,(float) D0);
    n = RobPutDouble(ModelID, NULL, Quantity[0],TestSetID, ACCEPTING, 1, &D1 );
    fprintf(fp,"RobPutDouble (all elements same value) : Quantity %s  ElementID %s ACCEPTING %f\n",Quantity[0],TestSetID,(float) D1);
    fflush(fp);

    // Check RobPutDouble (QuantitySet)
    n = RobElementsetElementCount(ModelID, TestSetID);
    pD0 = (double*) malloc(sizeof(double) * n);
    pD1 = (double*) malloc(sizeof(double) * n);

    for (j=2;j<4;j++)
    {
        for ( m=0;m<n ;m++ )
        {
            pD0[m] = m*10 + m+1 * j+1;
            pD1[m] = 2*pD0[m];
        }

        m = RobPutDouble(ModelID, Quantity[1], Quantity[j], TestSetID, PROVIDING, n, pD0 );
        fprintf(fp,"RobPutDouble : Quantityset %s Quantity %s  ElementID %s PROVIDING ",Quantity[1],Quantity[j], TestSetID);
        for(m=0;m<n;m++) { fprintf(fp,"%6.1f  ", (float)pD0[m]);} fprintf(fp,"\n");

        m = RobPutDouble(ModelID, Quantity[1], Quantity[j], TestSetID, ACCEPTING, n, pD1 );
        fprintf(fp,"RobPutDouble : Quantityset %s Quantity %s  ElementID %s ACCEPTING ",Quantity[1],Quantity[j], TestSetID);
        for(m=0;m<n;m++) { fprintf(fp,"%6.1f  ", (float)pD1[m]);} fprintf(fp,"\n");
    }
    fflush(fp);

    fprintf(fp,"\nExchangeItems :\n");
    RobDumpExchangeitems(fp);

    // Check RobGetDouble (single Quantity)
    fprintf(fp,"\nRobGetDouble :\n");
    m = RobGetDouble(ModelID, NULL, Quantity[0],TestSetID, PROVIDING,n, pD0 );
    fprintf(fp,"RobGetDouble Quantity %s  ElementID %s PROVIDING ",Quantity[0],TestSetID);
    for(m=0;m<n;m++) { fprintf(fp,"%6.1f  ", (float)pD0[m]);} fprintf(fp,"\n");

    m = RobGetDouble(ModelID, NULL, Quantity[0],TestSetID, ACCEPTING, n, pD1 );
    fprintf(fp,"RobGetDouble Quantity %s  ElementID %s ACCEPTING ",Quantity[0],TestSetID);
    for(m=0;m<n;m++) { fprintf(fp,"%6.1f  ", (float)pD1[m]);} fprintf(fp,"\n");
    fflush(fp);

    // Check RobGetDouble (QuantitySet)
    for (j=2;j<4;j++)
    {
         m = RobGetDouble(ModelID, Quantity[1], Quantity[j], TestSetID, PROVIDING, n, pD0 );
         fprintf(fp,"RobGetDouble Quantityset %s Quantity %s  ElementID %s PROVIDING ",Quantity[1], Quantity[j],TestSetID);
         for(m=0;m<n;m++) { fprintf(fp,"%6.1f  ", (float)pD0[m]);} fprintf(fp,"\n");

         m = RobGetDouble(ModelID, Quantity[1], Quantity[j], TestSetID, ACCEPTING, n, pD1 );
         fprintf(fp,"RobGetDouble : Quantityset %s Quantity %s  ElementID %s ACCEPTING ",Quantity[1],Quantity[j],TestSetID);
         for(m=0;m<n;m++) { fprintf(fp,"%6.1f  ", (float)pD1[m]);} fprintf(fp,"\n");
    }

    // Remove a Quantity from the Quantityset and adjust ExchangeItems buffer
    fprintf(fp,"\nRemove & trim : %s\n",Quantity[3]);
    n = RobQuantitysetRemoveQuantity(Quantity[1],Quantity[3]);
    RobExchangeitemsTrim();
    RobDumpExchangeitems(fp);

    free(pD0); free(pD1);
    RobFinalize();
    fflush(fp);
    fclose(fp);
}
