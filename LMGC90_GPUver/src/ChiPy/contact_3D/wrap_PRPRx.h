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

#ifndef wrap_PRPRx_h
#define wrap_PRPRx_h

 /**
  * @fn void PRPRx_SelectProxTactors(int reset=0)
  * @brief contact detection between PRxxx and PRxxx tactors
  *
  * First recup coordinate prediction, then proceed to a box selection to found rough
  * contact list and finally compute the final contact list.
  *
  * @cond PYDOC
  * python usage : PRPRx_SelectProxTactors(reset=0)
  * @param[in] reset (integer) : if not 0, detection is skipped but the boxes will be computed anew at next call
  * @endcond
  */
  extern "C" void PRPRx_SelectProxTactors(int reset=0);

 /**
  * @fn void PRPRx_UseCpCundallDetection(int nb_iter, double cd_shrink=0., double an_shrink=0., double delta=0.)
  * @brief chooses the Cundall iterative detection method
  *
  * If shrink parameters are provided they may be conflicting with
  * a call to PRPRx_ShrinkPolyrFaces function. Remind that
  * that the shrink parameters provided here are lengths.
  *
  * @cond PYDOC
  * python usage : PRPRx_UseCpCundallDetection(nb_iter, cd_shrink=0., an_shrink=0., delta=0.)
  * @param[in] nb_iter   (integer) : max number of iterations
  * @param[in] cd_shrink (real)    : shrink parameter (length) in clipper for candidate
  * @param[in] an_shrink (real)    : shrink parameter (length) in clipper for antagonist
  * @param[in] delta     (real)    : intersection simplification parameter in clipper
  * @endcond
  *
  * @cond CDOC
  * @param[in] nb_iter   (int)    : max number of iterations
  * @param[in] cd_shrink (double) : shrink parameter (length) in clipper for candidate
  * @param[in] an_shrink (double) : shrink parameter (length) in clipper for antagonist
  * @param[in] delta     (double) : intersection simplification parameter in clipper
  * @endcond
  */
  extern "C" void PRPRx_UseCpCundallDetection(int iter, double cd_shrink=0., double an_shrink=0., double delta=0.);

 /**
  * @fn void PRPRx_UseCpF2fExplicitDetection(double tol, double cd_shrink=0., double an_shrink=0., double delta=0.)
  * @brief chooses the face 2 face combinatory detection method
  *
  * If shrink parameters are provided they may be conflicting with
  * a call to PRPRx_ShrinkPolyrFaces function. Remind that
  * that the shrink parameters provided here are lengths.
  *
  * @cond PYDOC
  * python usage : PRPRx_UseCpF2fExplicitDetection(tol, cd_shrink=0., an_shrink=0., delta=0.)
  * @param[in] tol       (real) : tolerance on normal orientations
  * @param[in] cd_shrink (real) : shrink parameter (length) in clipper for candidate
  * @param[in] an_shrink (real) : shrink parameter (length) in clipper for antagonist
  * @param[in] delta     (real) : intersection simplification parameter in clipper
  * @endcond
  *
  * @cond CDOC
  * @param[in] tol       (double) : tolerance on normal orientations
  * @param[in] cd_shrink (double) : shrink parameter (length) in clipper for candidate
  * @param[in] an_shrink (double) : shrink parameter (length) in clipper for antagonist
  * @param[in] delta     (double) : intersection simplification parameter in clipper
  * @endcond
  */
  extern "C" void PRPRx_UseCpF2fExplicitDetection(double tol, double cd_shrink=0., double an_shrink=0., double delta=0.);

 /**
  * @fn void PRPRx_UseCpF2fDetection(double tol,int iter, double cd_shrink=0., double an_shrink=0., double delta=0.)
  * @brief chooses a mixe of the face 2 face and Cundall detection method
  *
  * If shrink parameters are provided they may be conflicting with
  * a call to PRPRx_ShrinkPolyrFaces function. Remind that
  * that the shrink parameters provided here are lengths.
  *
  * @cond PYDOC
  * python usage : PRPRx_UseCpF2fExplicitDetection(tol,iter, cd_shrink=0., an_shrink=0., delta=0.)
  * @param[in] tol       (real)    : tolerance on normal orientations
  * @param[in] iter      (integer) : max number of iterations
  * @param[in] cd_shrink (real)    : shrink parameter (length) in clipper for candidate
  * @param[in] an_shrink (real)    : shrink parameter (length) in clipper for antagonist
  * @param[in] delta     (real)    : intersection simplification parameter in clipper
  * @endcond
  *
  * @cond CDOC
  * @param[in] tol       (double) : tolerance on normal orientations
  * @param[in] iter      (int)    : max number of iterations
  * @param[in] cd_shrink (double) : shrink parameter (length) in clipper for candidate
  * @param[in] an_shrink (double) : shrink parameter (length) in clipper for antagonist
  * @param[in] delta     (double) : intersection simplification parameter in clipper
  * @endcond
  */
  extern "C" void PRPRx_UseCpF2fDetection(double tol, int iter, double cd_shrink=0., double an_shrink=0., double delta=0.);

 /**
  * @fn void PRPRx_UseNcDetection(double gdist)
  * @brief chooses contact detection methode between non-convex shapes
  *
  * @cond PYDOC
  * python usage : PRPRx_UseNcDetection(gdist)
  * @param[in] gdist (real) : global distance
  * @endcond
  *
  *
  * @cond CDOC
  * @param[in] gdist (double) : global distance
  * @endcond
  */
  extern "C" void PRPRx_UseNcDetection(double gdist);

 /**
  * @fn void PRPRx_UseNcF2fDetection(double gdist, double tol)
  * @brief chooses contact detection between between non-convex shapes using f2f strategy
  *
  * @cond PYDOC
  * python usage : PRPRx_UseNcF2fDetection(gdist,tol)
  * @param[in] gdist (real) : global distance
  * @param[in] tol (real) : tolerance on normal orientations
  * @endcond
  *
  * @cond CDOC
  * @param[in] gdist (double) : global distance
  * @param[in] tol (double) : tolerance on normal orientations
  * @endcond
  */
  extern "C" void PRPRx_UseNcF2fDetection(double gdist,double tol);

 /**
  * @fn void PRPRx_UseNcF2fExplicitDetection(double gdist, double tol)
  * @brief chooses contact detection between between non-convex shapes using f2f strategy
  *
  * @cond PYDOC
  * python usage : PRPRx_UseNcF2fExplicitDetection(gdist,tol)
  * @param[in] gdist (real) : global distance
  * @param[in] tol (real) : tolerance on normal orientations
  * @endcond
  *
  * @cond CDOC
  * @param[in] gdist (double) : global distance
  * @param[in] tol (double) : tolerance on normal orientations
  * @endcond
  */
  extern "C" void PRPRx_UseNcF2fExplicitDetection(double gdist,double tol);

 /**
  * @fn void PRPRx_UseTrianglesIntersectionDetection(int nb_max_pt=16)
  * @brief chooses contact detection using shadow overlap detection
  *
  * @cond PYDOC
  * python usage : PRPRx_UseTrianglesIntersectionDetection(nb_max_pt=16)
  * @param[in] nb_max_pt(integer) : maximum contact points to store/check during detection
  * @endcond
  *
  * @cond CDOC
  * @param[in] nb_max_pt(int) : maximum contact points to store/check during detection
  * @endcond
  */
  extern "C" void PRPRx_UseTrianglesIntersectionDetection(int nb_max_pt=16);

 /**
  * @fn void PRPRx_SetF2fMinimalSurfaceSize(double tol)
  * @brief set the minimum contact surface size with f2f algo otherwize contact is not computed
  *
  * @cond PYDOC
  * python usage : PRPRx_SetF2fMinimalSurfaceSize(tol)
  * @param[in] tol (real) : minimum surface size
  * @endcond
  *
  * @cond CDOC
  * @param[in] tol (double) : minimum surface size
  * @endcond
  */
  extern "C" void PRPRx_SetF2fMinimalSurfaceSize(double tol);


 /**
  * @fn void PRPRx_UseExternalDetection(void)
  * @brief chooses external contact detection (bindings)
  *
  * @cond PYDOC
  * python usage : PRPRx_UseExternalDetection()
  * @endcond
  *
  * @cond CDOC
  * @endcond
  */
  extern "C" void PRPRx_UseExternalDetection(void);

 /**
  * @fn void PRPRx_WriteLastVlocRloc(void)
  * @brief write last local values of all PRPRx contacts
  *
  * The values written are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : PRPRx_WriteLastVlocRloc()
  * @endcond
  */
  extern "C" void PRPRx_WriteLastVlocRloc(void);

 /**
  * @fn void PRPRx_WriteOutVlocRloc(void)
  * @brief write local values of all PRPRx contacts
  *
  * The values written are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : PRPRx_WriteOutVlocRloc()
  * @endcond
  */
  extern "C" void PRPRx_WriteOutVlocRloc(void);

 /**
  * @fn void PRPRx_DisplayOutVlocRloc(void)
  * @brief display local values of all PRPRx contacts
  *
  * The values displayed are relative velocity, forces and local frame
  *
  * @cond PYDOC
  * python usage : PRPRx_DisplayOutVlocRloc()
  * @endcond
  */
  extern "C" void PRPRx_DisplayOutVlocRloc(void);

 /**
  * @fn void PRPRx_DisplayProxTactors(void)
  * @brief display contacts
  *
  * @cond PYDOC
  * python usage : PRPRx_DisplayProxTactors()
  * @endcond
  */
  extern "C" void PRPRx_DisplayProxTactors(void);

 /**
  * @fn void PRPRx_ReadIniVlocRloc(int num=0)
  * @brief Read VlocRloc file
  *
  * - If num <= 0 : DATBOX/VlocRloc.INI file is read
  * - Else : OUTBOX/VlocRloc.OUT.num is read, num being
  *   + the parameter used in TimeEvolution_ReadIniVlocRloc last call
  *
  * @cond PYDOC
  * python usage : PRPRx_ReadIniVlocRloc(num=0)
  * @param[in] num (integer) : which VlocRloc file to read
  * @endcond
  *
  * @cond CDOC
  * @param[in] num (int) : which VlocRloc file to read
  * @endcond
  *
  */
  extern "C" void PRPRx_ReadIniVlocRloc(int num=0);

 /**
  * @fn void PRPRx_ShrinkPolyrFaces(double shrink)
  * @brief Shrink the face of the candidate polyhedron for the detection
  *
  * May be conflicting with the shrink parameters of the detections functions
  * used by clipper library. The difference is that clipper use a single
  * length for all sample, whereas this function use a scale factor to
  * retract the vertices of the candidate polyhedron inside the the surface.
  *
  * @cond PYDOC
  * python usage : PRPRx_ShrinkPolyrFaces(shrink)
  * @param[in] shrink (real) : scale factor allowing to shrink candidate surface
                               0. no shrink, 1. no surface
  * @endcond
  *
  * @cond CDOC
  * @param[in] shrink (double) : scale factor allowing to shrink candidate surface
                                 0. no shrink, 1. no surface
  * @endcond
  */
  extern "C" void PRPRx_ShrinkPolyrFaces(double shrink);

 /**
  * @fn void PRPRx_LowSizeArrayPolyr(int sfactor)
  * @brief abscons parameter to manage memory allocation
  *
  * @cond PYDOC
  * python usage : PRPRx_LowSizeArrayPolyr(sfactor)
  * @param[in] sfactor (integer) :
  * @endcond
  *
  * @cond CDOC
  * @param[in] sfactor (int) :
  * @endcond
  */
  extern "C" void PRPRx_LowSizeArrayPolyr(int sfactor);

 /**
  * @fn void PRPRx_SaveProxTactorsToFile(void)
  * @brief write selected contacts to file
  *
  * @cond PYDOC
  * python usage : PRPRx_SaveProxTactorsToFile()
  * @endcond
  */
  extern "C" void PRPRx_SaveProxTactorsToFile(void);

 /**
  * @fn void PRPRx_LoadProxTactorsFromFile(void)
  * @brief load selected contact from files
  *
  * @cond PYDOC
  * python usage : PRPRx_LoadProxTactorsFromFile()
  * @endcond
  */
  extern "C" void PRPRx_LoadProxTactorsFromFile(void);

 /**
  * @fn void PRPRx_SetXPeriodicCondition(double xperiod)
  * @brief initialise data for simulation using periodic condition
  *
  * @cond PYDOC
  * python usage : PRPRx_SetXPeriodicCondition(xperiod)
  * @param[in] xperiod (real) : periode on x axis
  * @endcond
  *
  * @cond CDOC
  * @param[in] xperiod (double) : periode on x axis
  * @endcond
  */
  extern "C" void PRPRx_SetXPeriodicCondition(double xperiod);

 /**
  * @fn void PRPRx_SetYPeriodicCondition(double yperiod)
  * @brief initialise data for simulation using periodic condition
  *
  * @cond PYDOC
  * python usage : PRPRx_SetYPeriodicCondition(yperiod)
  * @param[in] yperiod (real) : period on y axis
  * @endcond
  *
  * @cond PYDOC
  * @param[in] yperiod (double) : period on y axis
  * @endcond
  */
  extern "C" void PRPRx_SetYPeriodicCondition(double yperiod);

 /**
  * @fn void PRPRx_VerboseF2F(int cd, int an)
  * @brief ask for verbose comment concerning contact detection between cd and an
  *
  * @cond PYDOC
  * python usage : PRPRx_VerboseF2F(cd,an)
  * @param[in] cd (integer) : candidate
  * @param[in] an (integer) : antagoniste
  * @endcond
  *
  * @cond CDOC
  * @param[in] cd (int) : candidate
  * @param[in] an (int) : antagoniste
  * @endcond
  */
extern "C" void PRPRx_VerboseF2F(int cd, int an);

 /**
  * @fn void PRPRx_GetF2f2Inters(int** i4_vector, int* i4_size)
  * @brief Get the list of interactions for each face-to-face structure
  * Array of integer with number of f2f, then for each f2f, the
  * number of interactions then the list of interaction id.
  *
  * @cond PYDOC
  * python usage : f2f_inters = PRPRx_GetF2f2Inters()
  * @return f2f_inters (integer array)  : the integer array
  * @endcond

  * @cond CDOC
  * @param[out] i4_vector (int **) : the integer array
  * @param[out] i4_size (int *)    : size of i4_vector
  * @endcond
  */
extern "C" void PRPRx_GetF2f2Inters(int** i4_vector, int* i4_size);

 /**
  * @fn void PRPRx_SetCundallNeighbor(double neighbor)
  * @brief set a neighbor distance around common plane to select projected nodes
  *
  * @cond PYDOC
  * python usage : PRPRx_SetCundallNeighbor(neighbor)
  * @param[in] neighbor (real) : ratio of a reference size 
  * @endcond
  *
  * @cond CDOC
  * @param[in] neighbor (double) : ratio of a reference size
  * @endcond
  */
  extern "C" void PRPRx_SetCundallNeighbor(double neighbor);

 /**
  * @fn void PRPRx_CpUseOldCcpm()
  * @brief use the old method for computing contact point position
  *
  * @cond PYDOC
  * python usage : PRPRx_CpUseOldCcpm()
  * @endcond
  *
  * @cond CDOC
  * @endcond
  */
  extern "C" void PRPRx_CpUseOldCcpm();

 /**
  * @fn void PRPRx_SetReactionTrackingLength(double length)
  * @brief function which makes possible to set the length of
  *        the hexaedra glyph representing the visavis reaction
  *
  * @cond PYDOC
  * python usage : PRPRx_SetReactionTrackingLength(length)
  * @param[in] length (real) : length the hexaedra glyph
  * @endcond
  *
  * @cond CDOC
  * @param[in] length (double) : length of the hexaedra glyph
  * @endcond
  */
  extern "C" void PRPRx_SetReactionTrackingLength(double length);

 /**
  * @fn void PRPRx_SetTolRecupRloc(double tol)
  * @brief set the distance tolerance used in PRPRx_RecupRloc
  *
  * @cond PYDOC
  * python usage : PRPRx_SetTolRecupRloc(tol)
  * @param[in] tol (double) : tolerance
  * @endcond
  *
  * @cond CDOC
  * @param[in] tol (double) : tolerance
  * @endcond
  */
  extern "C" void PRPRx_SetTolRecupRloc(double tol);

 /**
  * @fn void PRPRx_GetInteractionVector(char * cvalue1, int ivalue1, double** r8_vector, int* r8_size)
  * @brief Get a copy of a vector of a PRPRx
  *
  * possible values for datatype field are "Coor_", "N____"
  *
  * @cond PYDOC
  * python usage : vector = PRPRx_GetInteractionVector(datatype, icdan)
  * @param[in] datatype (string [5]) : the vector to get
  * @param[in] icdan  (integer)      : rank of the PRPRx
  * @return    vector (double array) : output vector
  * @endcond
  *
  * @cond CDOC
  * @param[in]  cvalue1 (char[5])    : the vector to get
  * @param[in]  ivalue1 (int)        : rank of the PRPRx
  * @param[out] r8_vector (double**) : the out vector
  * @param[out] r8_size (int*)       : the length of r8_vector
  * @endcond
  */
  extern "C" void PRPRx_GetInteractionVector(char * cvalue1, int ivalue1, double** r8_vector, int* r8_size);

 /**
  * @fn void PRPRx_SetInteractionInternal(int ivalue1, int ivalue2, double rvalue)
  * @brief Set a value of the internal vector of a PRPRx
  *
  * @cond PYDOC
  * python usage : PRPRx_SetInteractionInternal(i, icdan, value)
  * @param[in] i        (integer)      : rank of internal
  * @param[in] icdan    (integer)      : rank of the PRPRx
  * @param[in] value    (double)       : value to set
  * @endcond
  *
  * @cond CDOC
  * @param[in]  ivalue1 (int)     : rank of internal
  * @param[in]  ivalue2 (int)     : rank of the PRPRx
  * @param[in] value (double)    : value to set
  * @endcond
  */
  extern "C" void PRPRx_SetInteractionInternal(int value1, int ivalue2, double rvalue);

 /**
  * @fn double PRPRx_GetInteractionInternal(int ivalue1, int ivalue2)
  * @brief Get a value from the internal vector of a PRPRx
  *
  * @cond PYDOC
  * python usage : value = PRPRx_GetInteractionInternal(i, icdan)
  * @param[in] i        (integer)      : rank of internal
  * @param[in] icdan    (integer)      : rank of the PRPRx
  * @param[out] value    (double)      : value to get
  * @endcond
  *
  * @cond CDOC
  * @param[in]  ivalue1 (int)     : rank of internal
  * @param[in]  ivalue2 (int)     : rank of the PRPRx
  * @param[out] value (double)    : value to get
  * @endcond
  */
  extern "C" double PRPRx_GetInteractionInternal(int value1, int ivalue2);

 /**
  * @fn void PRPRx_GetInteractionInternalComment(int ivalue, char** string_out, int* string_size, int* real_size)
  * @brief Get internal comment of a given interaction
  *
  * @cond PYDOC
  * python usage : comment=PRPRx_GetInteractionInternalComment(icdan)
  * @param[in] icdan (integer)   : rank of the PRPRx
  * @return comment  (char[100]) : the string to get
  * @endcond
  *
  * @cond CDOC
  * @param[in]  ivalue (int) : rank of the PRPRx
  * @param[out] string_out (char **) : internal comment of the interaciton
  * @param[out] string_size (int * ) : size of string_out
  * @param[out] real_size   (int * ) : max size of string pointed by string_out on Fortran side
  * @endcond
  */
  extern "C" void PRPRx_GetInteractionInternalComment(int ivalue, char** string_out, int* string_size, int* real_size);

 /**
  * @fn void PRPRx_WithNodalContact(void)
  * @brief use cd contact points at nodes instead at faces with NcDetection
  *
  * @cond PYDOC
  * python usage : PRPRx_WithNodalContact()
  * @endcond
  */
  extern "C" void PRPRx_WithNodalContact(void);

 /**
  * @fn void PRPRx_SetInternalSurface(int ivalue, double rvalue)
  * @brief Set the value of a surface type (point, line or surf) for wti detection
  * 
  * For surface, if the value is left to 0., then the surface of the triangle is computed
  * To select the type of surface : 1->point, 2->line, 3->surface
  *
  * @cond PYDOC
  * python usage : PRPRx_SetInternalSurface(itype, value)
  * @param[in] itype (integer) : the type of surface to set
  * @param[in] value (double)  : value to set
  * @endcond
  *
  * @cond CDOC
  * @param[in] ivalue (int)   : the type of surface to set
  * @param[in] value (double) : value to set
  * @endcond
  */
  extern "C" void PRPRx_SetInternalSurface(int value, double rvalue);

 /**
  * @fn void PRPRx_CleanMemory(void)
  * @brief Free all memory allocated within PRPRx module
  *
  * @cond PYDOC
  * python usage : PRPRx_CleanMemory()
  * @endcond
  */
  extern "C" void PRPRx_CleanMemory(void);

#endif /* wrap_PRPRx_h */
