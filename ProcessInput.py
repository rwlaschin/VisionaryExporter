
import Renderer
import Visionary

# this won't work because camera must be initialized first
# also if the camera is turned player would not turn into
# the forward direction, instead, it would shift in to the 
# direction the camera is facing

oMouse = Visionary.GetMouse()
oKeys = Visionary.GetKeys()

print "Retrieved Mouse and Keyboard"

oCamera = Renderer.GetCamera()	
Renderer.OrientCamera( oMouse, oKeys, oCamera )

Visionary.MovePlayer( oMouse, oKeys, oCamera )
