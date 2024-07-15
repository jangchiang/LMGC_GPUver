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
    file_list = 'ptc_1.vtp*'
else :
    file_list = 'ptc_*'

# find source files
ptcpvd = FindSource('ptc.pvd') 
ptcpvd = FindSource(file_list) if ptcpvd is None else ptcpvd

# hide data in view
if ptcpvd:
    Hide(ptcpvd, renderView1)

# create a new 'line_inter' Line glyph
if ptcpvd and not FindSource('interLine'):
    line_inter = Glyph(registrationName='interLine', Input=ptcpvd, GlyphType='Line')
    line_inter.OrientationArray = ['CELLS', 'R']
    line_inter.GlyphMode = 'All Points'
    # must not add line to get the correct 'no scale array' behaviour
    #line_inter.ScaleArray = ['CELLS', 'No scale array']
    line_inter.ScaleFactor = 0.1
    line_inter.GlyphTransform = 'Transform2'
    
    # toggle interactive widget visibility (only when running from the GUI)
    # paraview 11 only...
    #ShowInteractiveWidgets(proxy=line_inter.GlyphType)
    
    # show data in view
    line_interDisplay = Show(line_inter, renderView1, 'GeometryRepresentation')
    
    # trace defaults for the display properties.
    # ... skipped

    # update the view to ensure updated data information
    renderView1.Update()


line_inter = FindSource('interLine')
# set active source
if line_inter and not FindSource('interTube'):
    SetActiveSource(line_inter)

    # create a new 'Tube'
    tube_inter = Tube(registrationName='interTube', Input=line_inter)
    tube_inter.Scalars = ['POINTS', 'rln']
    tube_inter.Vectors = ['POINTS', 'N']

    # Properties modified on tube_inter
    tube_inter.VaryRadius = 'By Scalar'
    tube_inter.Radius = 0.01
    tube_inter.RadiusFactor = 5.0
    tube_inter.NumberofSides = 12

    # show data in view
    tube_interDisplay = Show(tube_inter, renderView1, 'GeometryRepresentation')

    # hide data in view
    Hide(line_inter, renderView1)
    
    # update the view to ensure updated data information
    renderView1.Update()

# create a new 'Ctc' threshold to exclude all 'noctc'
tube_inter = FindSource('interTube')
if tube_inter and not FindSource('WithCtc'):
    SetActiveSource(tube_inter)

    thresh_ctc = Threshold(registrationName='WithCtc', Input=tube_inter)
    thresh_ctc.Scalars = ['POINTS', 'status']
    thresh_ctc.LowerThreshold = 3.0
    thresh_ctc.UpperThreshold = 3.0
    thresh_ctc.Invert = 1
    
    # show data in view
    thresh_ctcDisplay = Show(thresh_ctc, renderView1, 'UnstructuredGridRepresentation')
    
    # hide data in view
    Hide(tube_inter, renderView1)
    
    # update the view to ensure updated data information
    renderView1.Update()

    ColorBy(thresh_ctcDisplay, ('POINTS', 'rln'))
