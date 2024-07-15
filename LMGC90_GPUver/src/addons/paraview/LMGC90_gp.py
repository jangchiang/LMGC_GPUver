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

# list of field to try to work on
field_list = [ f+"_ev"+str(i) for f in ['S','E'] for i in [1,2,3] ]

# get active view
renderView1 = GetActiveViewOrCreate('RenderView')

if version >= (5,11,):
    file_list = 'mecagp_1.vtp*'
else :
    file_list = 'mecagp_*'

# find source files
mgppvd = FindSource('mecagp.pvd') 
mgppvd = FindSource(file_list) if mgppvd is None else mgppvd
mgppvd = GetActiveSource()     if mgppvd is None else mgppvd

# hide data in view
if mgppvd:
    Hide(mgppvd, renderView1)
    field_list = [ f for f in field_list if f in mgppvd.PointData.keys() ]
else:
    field_list = [ ]

line_glyph = []

# create new Line glyphs for each fields and each direction
if mgppvd :
  for field in field_list :
    line = 'l'+field
    lg = FindSource(line)
    if not lg:

      lg = Glyph(registrationName=line, Input=mgppvd, GlyphType='Line')
      lg.OrientationArray = ['POINTS', field]
      lg.GlyphMode = 'All Points'
      lg.ScaleArray = ['POINTS', 'No scale array']
      lg.GlyphTransform = 'Transform2'
    
    line_glyph.append(lg)

    # toggle interactive widget visibility (only when running from the GUI)
    # paraview 11 only...
    #ShowInteractiveWidgets(proxy=line_inter.GlyphType)
    
    # show data in view
    lgDisplay = Show(lg, renderView1, 'GeometryRepresentation')
    
    # trace defaults for the display properties.
    # ... skipped

# update the view to ensure updated data information
renderView1.Update()


for line, field in zip(line_glyph, field_list):

  tube = 't'+field

  if not FindSource(tube):

    SetActiveSource(line)

    sfield = field[0]+field[-1]
    # create a new 'Tube'
    tf = Tube(registrationName=tube, Input=line)
    tf.Scalars = ['POINTS', sfield ]
    tf.Vectors = ['POINTS',  field ]

    # Properties modified on tube_inter
    tf.VaryRadius = 'By Scalar'
    tf.Radius = 0.01
    tf.RadiusFactor = 5.0
    tf.NumberofSides = 12

    # show data in view
    tfDisplay = Show(tf, renderView1, 'GeometryRepresentation')
    ColorBy(tfDisplay, ('POINTS', sfield))
    tfDisplay.RescaleTransferFunctionToDataRange(True, False)
    # show color bar/color legend
    tfDisplay.SetScalarBarVisibility(renderView1, True)

    # hide data in view
    Hide(line, renderView1)
    
# update the view to ensure updated data information
renderView1.Update()



