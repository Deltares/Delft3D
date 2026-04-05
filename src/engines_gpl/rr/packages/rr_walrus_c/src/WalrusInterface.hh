//Walrus dll
#ifndef WALRUS_INTERFACE_H
#define WALRUS_INTERFACE_H

#include <stddef.h>
#include <vector>
#include "WALRUS.hh"

static std::vector<WALRUS> walrusInstances;

#ifdef WIN32
#else
#   include "config.h"
#   define ADDWALRUSINSTANCE  FC_FUNC(ADDWALRUSINSTANCE,ADDWALRUSINSTANCE)
#   define ADDWALRUSINSTANCES  FC_FUNC(addwalrusinstances,ADDWALRUSINSTANCES)
#   define WALRUSSETSEQC  FC_FUNC(walrussetseqc,WALRUSSETSEQC)
#   define WALRUSSET  FC_FUNC(walrusset,WALRUSSET)
#   define WALRUSGET  FC_FUNC(walrusget,WALRUSGET)
#   define WALRUSSETST  FC_FUNC(walrussetst,WALRUSSETST)
#   define WALRUSINIT  FC_FUNC(walrusinit,WALRUSINIT)
#   define WALRUSDOSTEP  FC_FUNC(walrusdostep,WALRUSDOSTEP)
#   define WALRUSSETWDVBYTABLE  FC_FUNC(walrussetwdvbytable,WALRUSSETWDVBYTABLE)
#   define WALRUSSETDVEQDGBYTABLE  FC_FUNC(walrussetdveqdgbytable,WALRUSSETDVEQDGBYTABLE)
#   define WALRUSSETBETADVBYTABLE  FC_FUNC(walrussetbetadvbytable,WALRUSSETBETADVBYTABLE)
#   define WALRUSSETQHSBYTABLE  FC_FUNC(walrussetqhsbytable,WALRUSSETQHSBYTABLE)
#endif

#ifdef __cplusplus
extern "C" {
#endif

   /*!
   * @brief Appends a single instance of Walrus to walrusInstances static vector
   */
   int ADDWALRUSINSTANCE();

   /*!
   * @brief Appends nWalrusInstances to walrusInstances static vector
   */
   int ADDWALRUSINSTANCES(int * nWalrusInstances);

   /*!
   * @brief sets times and fc_vec to walrusInstanceIndex
   */
   int WALRUSSETSEQC(int * walrusInstanceIndex, int * varEnum,
               void * times, int* timesLenght, void * fc_vec, int *  fc_vecLenght, double * timestepsize, int * startIndex);

   /*!
   * @brief sets val to walrusInstanceIndex for the variable varEnum
   */
   int WALRUSSET(int* walrusInstanceIndex, int* varEnum, double * val, int * startIndex);

   /*!
   * @brief gets the val of the walrusInstanceIndex for the variable varEnum
   */
   int WALRUSGET(int* walrusInstanceIndex, int* varEnum, void * val, int * startIndex);

   /*!
   * @brief sets soil to walrusInstanceIndex
   */
   int WALRUSSETST(int* walrusInstanceIndex, int* soil, int * startIndex);

   /*!
   * @brief Initialize the walrusInstanceIndex
   */
   int WALRUSINIT(int* walrusInstanceIndex, double* StartTime, double* Q0, double * hS0,
            double* dG0, double * dV0, double* hQ0, double* Gfrac, int * startIndex);

   /*!
   * @brief do one time step for the walrusInstanceIndex
   */
   int WALRUSDOSTEP(int* walrusInstanceIndex, double* deltime, int * startIndex);



   /*  void set_W_dV_bytable(const vector<double> &dV, const vector<double> &W) */
   int WALRUSSETWDVBYTABLE(int * walrusInstanceIndex,
            void * times, int* timesLenght, void * fc_vec, int *  fc_vecLenght, int * startIndex);

   /*  void set_dVeq_dG_bytable(const vector<double> &dG */
   int WALRUSSETDVEQDGBYTABLE(int * walrusInstanceIndex,
            void * times, int* timesLenght, void * fc_vec, int *  fc_vecLenght, int * startIndex);

   /*  void set_beta_dV_bytable(const vector<double> &dV */
   int WALRUSSETBETADVBYTABLE(int * walrusInstanceIndex,
            void * times, int* timesLenght, void * fc_vec, int *  fc_vecLenght, int * startIndex);

   /*  void set_Q_hS_bytable(const vector<double> &hS, const vector<double> &Q) */
   int WALRUSSETQHSBYTABLE(int * walrusInstanceIndex,
            void * times, int* timesLenght, void * fc_vec, int *  fc_vecLenght, int * startIndex);


#ifdef __cplusplus
}
#endif

#endif
