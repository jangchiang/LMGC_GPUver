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

#ifndef wrap_ASpxx_h
#define wrap_ASpxx_h
    
 /**
  * @fn void ASpxx_LoadTactors(void)
  * @brief Load ASpxx from MAILx and Initialize existing_entities
  *
  * @cond PYDOC
  * python usage : ASpxx_LoadTactors()
  * @endcond
  */
  extern "C" void ASpxx_LoadTactors(void);

 /**
  * @fn void ASpxx_PushPreconNodes(void)
  * @brief set ASpxx supporting nodes as precon
  *
  * @cond PYDOC
  * python usage : ASpxx_PushPreconNodes()
  * @endcond
  */
  extern "C" void ASpxx_PushPreconNodes(void);

 /**
  * @fn void ASpxx_GetAllConnec(int** i4_vector, int * i4_size)
  * @brief return connectivity of all AS in a single vector using gloab node numbering of mecaMAILx
  *
  * @cond PYDOC
  * python usage : connec = ASxxx_getAllConnec()
  * @return    connec (integer 1D-array) : connectiviy of ASxxx elements
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] i4_vector (int **) : the connectivity
  * @param[in]     i4_size (int *)    : vector dimension
  * @endcond
  */
  extern "C" void ASpxx_GetAllConnec(int ** i4_vector, int * i4_size);

 /**
  * @fn void ASpxx_GetAllData(int** i4_matrix, int * i4_dim1, int * i4_dim2, double ** matrix_out, int * dim1, int * dim2 )
  * @brief return integer (ibdyty, itacty, i_as) and real data (normal) of all ASxxx
  *
  * @cond PYDOC
  * python usage : idata, rdata = ASxxx_getAllData()
  * @return    idata (integer 2D-array) : integer data array
  * @return    rdata (real 2D-array)    : real data array
  * @endcond
  *
  * @cond CDOC
  * @param[in,out] i4_matrix  (int **) : the integer data (i_bdyty, i_tacty, i_sci)
  * @param[in]     i4_dim1    (int *)  : 1st dimension of i4_matrix
  * @param[in]     i4_dim2    (int *)  : 2dn dimension of i4_matrix
  * @param[in,out] matrix_out (int *)  : the real data (normal)
  * @param[in]        dim1    (int *)  : 1st dimension of matrix_out
  * @param[in]        dim2    (int *)  : 2nd dimension of matrix_out
  * @endcond
  */
  extern "C" void ASpxx_GetAllData(int ** i4_matrix, int * i4_dim1, int * i4_dim2, double ** matrix_out, int * dim1, int * dim2 );

/**
  * @fn void ASpxx_CleanMemory(void)
  * @brief Free all memory allocated within ASpxx module
  *
  * @cond PYDOC
  * python usage : ASpxx_CleanMemory()
  * @endcond
  */
  extern "C" void ASpxx_CleanMemory(void);

/**
  * @fn void ASpxx_ExplodePatch(void)
  * @brief Explode ASpxx patch in singleton
  *
  * @cond PYDOC
  * python usage : ASpxx_ExplodePatch()
  * @endcond
  */
  extern "C" void ASpxx_ExplodePatch(void);

#endif /* wrap_ASpxx_h */
