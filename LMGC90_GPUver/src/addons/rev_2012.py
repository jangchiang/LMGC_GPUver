
up_commands={'overall_SetTimeStep':'TimeEvolution_SetTimeStep',
             'overall_SetInitialTime': 'TimeEvolution_SetInitialTime',
             'overall_SetInitialStep': 'TimeEvolution_SetInitialStep',
             'overall_IncrementStep': 'TimeEvolution_IncrementStep',
             'overall_UpdateDof': 'TimeEvolution_UpdateStep',
             'overall_GetTimeStep': 'TimeEvolution_GetTimeStep',
             'overall_GetTime': 'TimeEvolution_GetTime',
             'overall_DisplayTimes': 'TimeEvolution_DisplayStep',
             'overall_ComputeTimeStep':'NewtonRaphson_ComputeTimeStep',
             'overall_SetMinTimeStep':'NewtonRaphson_SetMinTimeStep',
             'overall_SetMaxTimeStep':'NewtonRaphson_SetMaxTimeStep',
             'overall_SetFinalTime':'NewtonRaphson_SetFinalTime',
             'overall_CheckNewtonConvergence':'NewtonRaphson_CheckConvergence',
             'overall_CheckNlConvergence':'NewtonRaphson_CheckConvergence',
             'overall_SetNewtonMaxIter':'NewtonRaphson_SetMaxIter',
             'overall_SetNewtonGoodIter':'NewtonRaphson_SetGoodIter',
             'overall_SetNewtonBadIter':'NewtonRaphson_SetBadIter',
             'overall_SetNewtonIncPatience': 'NewtonRaphson_SetIncPatience',
             'overall_InitThetaIntegrator':'Integrator_InitTheta',
             'overall_InitCrankNickolsonIntegrator':'Integrator_InitCrankNickolson',
             'overall_InitGearIntegrator':'Integrator_InitGear',
             'overall_InitVerletIntegrator':'Integrator_InitVerlet',
             'post2D_SetRadiusPT2Dx':'PT2Dx_SetDisplayRadius',
             'overall_ReadIniDof':'TimeEvolution_ReadIniDof',
             'overall_BinaryReadIniDof':'TimeEvolution_BinaryReadIniDof',
	     'overall_WriteLastDof':'TimeEvolution_WriteLastDof',
	     'overall_WriteOutDof':'TimeEvolution_WriteOutDof',
             'overall_DisplayOutDof':'TimeEvolution_DisplayOutDof',
	     'overall_BinaryWriteLastDof':'TimeEvolution_BinaryWriteLastDof',
             'overall_BinaryWriteOutDof':'TimeEvolution_BinaryWriteOutDof',
             'overall_WriteLastRnod':'TimeEvolution_WriteLastRnod',
             'overall_WriteOutRnod':'TimeEvolution_WriteOutRnod',
	     'overall_DisplayOutRnod':'TimeEvolution_DisplayOutRnod',
             'overall_ReadIniVlocRloc':'TimeEvolution_ReadIniVlocRloc',
             'overall_BinaryReadIniVlocRloc':'TimeEvolution_BinaryReadIniVlocRloc',
	     'overall_WriteLastVlocRloc':'TimeEvolution_WriteLastVlocRloc',
   	     'overall_WriteOutVlocRloc':'TimeEvolution_WriteOutVlocRloc',
      	     'overall_DisplayOutVlocRloc':'TimeEvolution_DisplayOutVlocRloc',
	     'overall_BinaryWriteLastVlocRloc':'TimeEvolution_BinaryWriteLastVlocRloc',
   	     'overall_BinaryWriteOutVlocRloc':'TimeEvolution_BinaryWriteOutVlocRloc',
             'overall_ReadIniGPV':'TimeEvolution_ReadIniGPV',
	     'overall_WriteLastGPV':'TimeEvolution_WriteLastGPV',
     	     'overall_WriteOutGPV':'TimeEvolution_WriteOutGPV',
	     'RBDY2_SetVisibility':'RBDY2_SetVisible',
	     'RBDY2_IsBodyVisible':'RBDY2_IsVisible',
	     'DISKx_GetNbScalarfields':'DISKx_GetNbScalarFields',
	     'DISPx_GetNbScalarfields':'DISPx_GetNbScalarFields',
	     'JONCx_GetNbScalarfields':'JONCx_GetNbScalarFields',
	     'POLYG_GetNbScalarfields':'POLYG_GetNbScalarFields',
	     'PT2Dx_GetNbScalarfields':'PT2Dx_GetNbScalarFields',
	     'xKSID_GetNbScalarfields':'xKSID_GetNbScalarFields',
	     'xPSID_GetNbScalarfields':'xPSID_GetNbScalarFields',
	     'DISKx_InitScalarfields':'DISKx_InitScalarFields',
	     'DISPx_InitScalarfields':'DISPx_InitScalarFields',
	     'JONCx_InitScalarfields':'JONCx_InitScalarFields',
	     'POLYG_InitScalarfields':'POLYG_InitScalarFields',
	     'PT2Dx_InitScalarfields':'PT2Dx_InitScalarFields',
	     'xKSID_InitScalarfields':'xKSID_InitScalarFields',
	     'xPSID_InitScalarfields':'xPSID_InitScalarFields',
	     'Stress2D':'Stress',
	     'Stress3D':'Stress',
	     'Strain2D':'Strain',
	     'Strain3D':'Strain',
            }

ob_commands=['ComputeBox', 'POLYR_IncrementStep', 'CLALp_SetHalo', 'CheckOutOfBounds']

ch_arguments ={'bulk_behav_GetGravity'                    :0,
               'RBDY2_GetBodyInertia'                     :1,
               'RBDY2_GetBodyVector'                      :2,
               'RBDY3_GetBodyInertia'                     :1,
               'RBDY3_GetBodyVector'                      :2,
               'mecaMAILx_GetRigidFrameTT'                :1,
               'mecaMAILx_GetRigidCoorTT'                 :1,
               'mecaMAILx_GetRigidCooref'                 :1,
               'mecaMAILx_GetBodyVector'                  :2,
               'mecaMAILx_GetStress2D'                    :1,
               'mecaMAILx_GetStress3D'                    :1,
               'mecaMAILx_GetStrain2D'                    :1,
               'mecaMAILx_GetStrain3D'                    :1,
               'mecaMAILx_GetNodeCoorTT'                  :2,
               'mecaMAILx_GetNodeCooref'                  :2,
               'mecaMAILx_ComputeInfoPrincipalStressField':1,
               'therMAILx_GetBodyVector'                  :2,
               'therMAILx_GetGrad'                        :1,
               'therMAILx_GetFlux'                        :1,
               'poroMAILx_GetBodyVector'                  :2,
               'poroMAILx_GetStress2D'                    :1,
               'poroMAILx_GetStress3D'                    :1,
               'poroMAILx_GetStrain2D'                    :1,
               'poroMAILx_GetStrain3D'                    :1,
               'DKDKx_GetInteractions'                    :0,
               'DKDKx_GetGlobal2Local'                    :1,
               'PRPRx_GetInteractionVector'               :2,
               'DISKx_GetDISKx2RBDY2'                     :0,
               'DISKx_GetContactorCoor'                   :1,
               'DISPx_GetDISPx2RBDY2'                     :0,
               'JONCx_GetShape'                           :1,
               'JONCx_GetCoor'                            :1,
               'POLYG_GetPOLYG2RBDY2'                     :0,
               'POLYG_GetContactorOutline'                :1,
               'POLYG_GetVertices'                        :1,
               'post3D_GetBodyIni'                        :0,
               'post3D_GetBody'                           :0,
               'post3D_GetContact'                        :0,
              }

