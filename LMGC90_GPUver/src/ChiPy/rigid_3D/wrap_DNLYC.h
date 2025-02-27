/*==========================================================================
 *
 * Copyright 2000-2024 CNRS-UM.
 *
 * This file is part of a software (LMGC90) which is a computer program 
 * which purpose is to modelize interaction problems (contact, multi-Physics,etc).
 *
 * This software is governed by the CeCILL license under French law and
 * abiding by the rules of distribution of free software.  You can  use, 
 * modify and/ or redistribute the software under the terms of the CeCILL
 * license as circulated by CEA, CNRS and INRIA at the following URL
 * "http://www.cecill.info". 
 *
 * As a counterpart to the access to the source code and  rights to copy,
 * modify and redistribute granted by the license, users are provided only
 * with a limited warranty  and the software's author,  the holder of the
 * economic rights,  and the successive licensors  have only  limited
 * liability. 
 *
 * In this respect, the user's attention is drawn to the risks associated
 * with loading,  using,  modifying and/or developing or reproducing the
 * software by the user in light of its specific status of free software,
 * that may mean  that it is complicated to manipulate,  and  that  also
 * therefore means  that it is reserved for developers  and  experienced
 * professionals having in-depth computer knowledge. Users are therefore
 * encouraged to load and test the software's suitability as regards their
 * requirements in conditions enabling the security of their systems and/or 
 * data to be ensured and,  more generally, to use and operate it in the 
 * same conditions as regards security. 
 *
 * The fact that you are presently reading this means that you have had
 * knowledge of the CeCILL license and that you accept its terms.
 *
 * To report bugs, suggest enhancements, etc. to the Authors, contact
 * Frederic Dubois.
 *
 * frederic.dubois@umontpellier.fr
 *
 *=========================================================================*/

#ifndef wrap_DNLYC_h
#define wrap_DNLYC_h

 /**
  * @fn void DNLYC_LoadTactors(void)
  * @brief load DNLYC from RBDY3 and initialize existing_entites
  *
  * @cond PYDOC
  * python usage : DNLYC_LoadTactors()
  * @endcond
  */
  extern "C" void DNLYC_LoadTactors(void);

 /**
  * @fn int DNLYC_IsVisible(int itacty)
  * @brief return if a given contactor is attached to a visible body 
  *
  * @cond PYDOC
  * python usage : visible = DNLYC_IsVisible(itacty)
  * @param[in] itacty (integer) : id of the contactor we want visibility
  * @return visible (integer) : 1 if body is visible, 0 else
  * @endcond
  *
  * @cond CDOC
  * @param[in] itacty (int) : id of the contactor we want visibility
  * @return (int) 1 if body is visible, 0 else
  * @endcond
  */
  extern "C" int DNLYC_IsVisible(int itacty);

 // external vtk visu

 /**
  * @fn int DNLYC_GetNbDNLYC(void)
  * @brief Get the number of DNLYC
  *
  * @cond PYDOC
  * python usage : nb_DNLYC = DNLYC_GetNbDNLYC()
  * 
  * @return nb_DNLYC (integer) : the number of DNLYC
  * @endcond
  *
  * @cond CDOC
  * @return (int) the number of DNLYC
  * @endcond
  */
  extern "C" int DNLYC_GetNbDNLYC(void);

 /**
  * @fn void DNLYC_GetPtrDNLYC2BDYTY(int ** pointer_out, int * dim, int * dim2)
  * @brief return a pointer onto the map dnlyc2bdyty
  *
  * @cond PYDOC
  * python usage : dnlyc2bdyty = DNLYC_GetPtrDNLYC2BDYTY()
  *
  * @return dnlyc2bdyty (integer array) : reference on map between dnlyc rank and body rank
  * @endcond
  *
  * @cond CDOC
  * @param[out] pointer_out (int**) : a pointer on the array dnlyc2bdyty
  * @param[out] dim1 (int *)        : first dim of pointer_out
  * @param[out] dim2 (int *)        : second dim of pointer_out
  * @endcond
  */
  extern "C" void DNLYC_GetPtrDNLYC2BDYTY(int** pointer_out, int* dim1, int* dim2);

 /**
  * @fn void DNLYC_InitOutlines(double ** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the outlines of all DNLYC
  *
  * @cond PYDOC
  * usage : outlines = DNLYC_InitOutlines()
  * @return outlines (double array) : a reference on outlines_DNLYC
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : reference on outlines_DNLYC array
  * @param[in,out] dim1 (int *)            : first dimension of pointer_out
  * @param[in,out] dim2 (int *)            : second dimension of pointer_out
  * @endcond
  *
  */
  extern "C" void DNLYC_InitOutlines(double ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn void DNLYC_InitScalarFields(double ** pointer_out, int * dim1, int * dim2)
  * @brief Get a reference on the scalar fields of all DNLYC
  *
  * @cond PYDOC
  * usage : scalarfields = DNLYC_InitScalarfields()
  * @return scalarfields (double array) : reference on scalarfields_DNLYC array
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (double **) : reference on scalarfields array
  * @param[in,out] dim1 (int *)            : first dimension of pointer_out
  * @param[in,out] dim2 (int *)            : second dimension of pointer_out
  * @endcond
  *
  */
  extern "C" void DNLYC_InitScalarFields(double ** pointer_out, int * dim1, int * dim2);

 /**
  * @fn void DNLYC_UpdatePostdata(void)
  * @brief Update values of outlines_DNLYC and scalarfields_DNLYC pointers
  *
  * @cond PYDOC
  * usage : DNLYC_UpdatePostdata
  * @endcond
  *
  */
  extern "C" void DNLYC_UpdatePostdata(void);

 /**
  * @fn void DNLYC_GetNbPointOutlines(int ** pointer_out, int * length)
  *
  * @brief Get the list of cumulated outline points number
  *
  * @cond PYDOC
  * python usage : nb_pointOutlines = DNLYC_GetNbPointOutlines()
  * @return nb_pointOutlines (integer array) : the cumulated number of outline points of the DNLYC
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (int **) : reference on nb_point_outlines array
  * @param[in,out] length (int *)       : first dimension of pointer_out
  * @endcond
  */
  extern "C" void DNLYC_GetNbPointOutlines(int** pointer_out, int* length);
  
 /**
  * @fn int DNLYC_GetNbScalarFields(void)
  *
  * @brief Get the number of scalar fields of a DNLYC
  *
  * @cond PYDOC
  * python usage : nb_scalarfields = DNLYC_GetNbScalarFields()
  * @return nb_scalarfields (integer) : the number of scalar fields of a DNLYC
  * @endcond
  *
  * @cond CDOC
  * @return nb_scalarfields (int) : the number of scalar fields of a DNLYC
  * @endcond
  */
  extern "C" int DNLYC_GetNbScalarFields(void);
  
 /**
  * @fn void DNLYC_GetPtrAllConnectivities(int ** i4_vector, int * i4_size)
  * @brief Get a reference on the connectivities of all DNLYC
  *
  * @cond PYDOC
  * usage : connec = DNLYC_GetPtrAllConnectivities()
  * @return connec (integer array) : a reference on all_connectivities
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] pointer_out (int **) : reference on all_connectivities array
  * @param[in,out] length (int *)       : length of pointer_out
  * @endcond
  *
  */
  extern "C" void DNLYC_GetPtrAllConnectivities(int ** pointer_out, int * length);

 /**
   * @fn void DNLYC_CleanMemory(void)
   * @brief Free all memory allocated within DNLYC module
   *
   * @cond PYDOC
   * python usage : DNLYC_CleanMemory()
   * @endcond
   */
   extern "C" void DNLYC_CleanMemory(void);
  
#endif /* wrap_DNLYC */
