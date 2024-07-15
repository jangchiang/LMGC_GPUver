# trace generated using paraview version 5.11.0
#import paraview
#paraview.compatibility.major = 5
#paraview.compatibility.minor = 11

#### import the simple module from the paraview
from paraview.simple import *

major = servermanager.vtkSMProxyManager.GetVersionMajor()
minor = servermanager.vtkSMProxyManager.GetVersionMinor()
version = paraview._version(major, minor)

#### disable automatic camera reset on 'Show'
paraview.simple._DisableFirstRenderCameraReset()

# get active view
renderView1 = GetActiveViewOrCreate('RenderView')

if version >= (5,11,):
    file_list = 'ck_1.vtp*'
else :
    file_list = 'ck_*'

# find source files
ckpvd = FindSource('ck.pvd') 
ckpvd = FindSource(file_list) if ckpvd is None else ckpvd

# hide data in view
if ckpvd:
    Hide(ckpvd, renderView1)

# create a new 'PressurePoint' threshold
if ckpvd and not FindSource('PressurePoint'):
    pressurePoint = Threshold(registrationName='PressurePoint', Input=ckpvd)
    pressurePoint.Scalars = ['CELLS', 'Type']
    pressurePoint.LowerThreshold = 0.0
    pressurePoint.UpperThreshold = 0.0
    
    # show pressurePoint data in view
    ppDisplay = Show(pressurePoint, renderView1, 'UnstructuredGridRepresentation')
    
    # trace defaults for the display properties.
    # ... skipped
    # init the 'PiecewiseFunction' selected for 'ScaleTransferFunction'
    # init the 'PiecewiseFunction' selected for 'OpacityTransferFunction'
    
    # update the view to ensure updated data information
    renderView1.Update()


# create a new 'PP_Reac' Line glyph
pressurePoint = FindSource('PressurePoint')
if ckpvd and pressurePoint and not FindSource('PP_Reac'):
    pp_reac = Glyph(registrationName='PP_Reac', Input=pressurePoint, GlyphType='Line')
    pp_reac.OrientationArray = ['POINTS', 'Reac']
    pp_reac.GlyphMode = 'All Points'
    pp_reac.ScaleArray = ['POINTS', 'No scale array']
    pp_reac.ScaleFactor = 0.1
    pp_reac.GlyphTransform = 'Transform2'
    
    # toggle interactive widget visibility (only when running from the GUI)
    # only paraview 11
    #ShowInteractiveWidgets(proxy=pp_reac.GlyphType)
    
    # show data in view
    pp_reacDisplay = Show(pp_reac, renderView1, 'GeometryRepresentation')
    
    # trace defaults for the display properties.
    # ... skipped

    # update the view to ensure updated data information
    renderView1.Update()

pp_reac = FindSource('PP_Reac')
# set active source
if pp_reac and not FindSource('ReacTube'):
    SetActiveSource(pp_reac)

    # create a new 'Tube'
    reac_tube = Tube(registrationName='ReacTube', Input=pp_reac)
    #reac_tube.Scalars = ['POINTS', 'rln']
    reac_tube.Vectors = ['POINTS', 'Reac']

    # Properties modified on tube_inter
    reac_tube.VaryRadius = 'By Vector Norm'
    reac_tube.Radius = 0.01
    reac_tube.RadiusFactor = 5.0
    reac_tube.NumberofSides = 12

    # show data in view
    reac_tubeDisplay = Show(reac_tube, renderView1, 'GeometryRepresentation')

    # hide data in view
    Hide(pp_reac, renderView1)
    
    # update the view to ensure updated data information
    renderView1.Update()

    ## set scalar coloring
    #ColorBy(reac_tubeDisplay, ('POINTS', 'Reac', 'Magnitude'))

# set active source
SetActiveSource(ckpvd)

# create a new 'Face2Face' threshold
if ckpvd and not FindSource('Face2Face'):
    f2f = Threshold(registrationName='Face2Face', Input=ckpvd)
    f2f.Scalars = ['CELLS', 'Type']
    f2f.LowerThreshold = 1.0
    f2f.UpperThreshold = 1.0
    
    # show data in view
    f2f_Display = Show(f2f, renderView1, 'UnstructuredGridRepresentation')
    
    # trace defaults for the display properties.
    f2f_Display.Representation = 'Wireframe'
    # ... skipped.

    # update the view to ensure updated data information
    renderView1.Update()
    
    # set scalar coloring
    ColorBy(f2f_Display, ('CELLS', 'Sigma_n'))
    

# set active source
SetActiveSource(ckpvd)

# create a new 'CentralKernel' threshold
if ckpvd and not FindSource('CentralKernel'):
    centralKernel= Threshold(registrationName='CentralKernel', Input=ckpvd)
    centralKernel.Scalars = ['CELLS', 'Type']
    centralKernel.LowerThreshold = 2.0
    centralKernel.UpperThreshold = 2.0
    
    # show data in view
    ck_Display = Show(centralKernel, renderView1, 'UnstructuredGridRepresentation')
    
    # trace defaults for the display properties.
    ck_Display.Representation = 'Surface'
    # ... skipped.

    # hide data in view
    #Hide(ckpvd, renderView1)
    
    # update the view to ensure updated data information
    renderView1.Update()
    
    # set scalar coloring
    ColorBy(ck_Display, ('CELLS', 'Status'))
    

#--------------------------------------------
# uncomment the following to render all views
# RenderAllViews()
# alternatively, if you want to write images, you can use SaveScreenshot(...).
