#!BPY

"""
Name: 'Visonary (.ve)...'
Blender: 232
Group: 'Export'
Tooltip: 'Export selected meshes to Visionary File Format (.ve)'
"""

__author__ = "Robert Wlaschin"
__url__ = ("N/A")
__version__ = "Visonary 1.0.0.1"

__bpydoc__ = """\
This script exports meshes to Visionary file format.

Visionary is a full-featured private game engine.  VE file format
composed of objects.  Each object has a specifice data format.  
Please see the Visionary Documentation for the description for
each of these objects.  Each object is connected together by a
linked list.  This data comes from the outliner in blender

Usage:<br>
	Select meshes to be exported and run this script from "File->Export" menu.

Supported:<br>
	Lights, Camera, Multiple models, Meshes, Materials, Specular Highlights, 
	and Vertex Colors

Missing:<br>
	Animations, tracking of attached Scripts, Creation of Non-rendering
	game objects (triggers), textures, and shared object sets

Enhancements:<br>
	Implement an index'd array mech
	
Known issues:<br>
	None, yet ;)

Notes:<br>
	This file will change continously until Visionary is completed
"""

VERSION = 1.6

# $Id: visionary_export.py,v 1.6 2005/06/13 02:30:00 rwlaschin Exp $
#
# +---------------------------------------------------------+
# | Copyright (c) 2005 Robert Wlaschin                      |
# | http://www.geocities.com/raw77_m                        |
# | raw77_m@yahoo.com                                       |
# | June 23, 2005                                           |
# | Released under the Blender Artistic Licence (BAL)       |
# | Import Export Suite v0.5                                |
# +---------------------------------------------------------+
# | Write Visionary Object File Format (*.ve)               |
# +---------------------------------------------------------+

import Blender, meshtools
from Blender import Types, Object, NMesh, Camera, Lamp, Scene, Mathutils
from Blender.Scene import Render
from Blender.Scene.Render import *
import warnings
import struct, cStringIO, time, operator, copy, math
import string, chunk, os, types

# Filter warnings for crc data structure
warnings.filterwarnings( "ignore", "hex/oct constants > sys.maxint will return positive values in Python 2.4 and up" )

# ===============================================
# === Visionary_export this is the interface 
# === funtion, it is processes all of the selected
# === objects and dumps them into the VE format
# ===============================================
def visionary_export(filename = "start.ve"):
	global hFile
	
	print "-------------------------------------------------------"
	print "                      start\n"
	
	if filename.find('.ve', -3) <= 0: filename += '.ve'

	start = time.clock()
	
	# Try getting the names of all of the selected items in the scene
	objects	= Blender.Object.GetSelected()
	if not objects:
		meshtools.print_boxed("No objects are selected.")
		return
	
	objects.sort(lambda a,b: cmp(a.name,b.name))	
	
	# Call iterator and Parse Function
	it = iter(objects)
	parentInstance = ParseObjects( it.next(), it )
	
	header = []
	
	def addtoheader( name, dmp, size ):
		if size > 0: header.append( (name,GenerateHashValue(name),dmp,size) ) 
	
	# Build the shared information 
	(dmp, dsize) = PackSharedObjects()
	addtoheader("shared", dmp, dsize)
	
	# Build the object information	
	(dmp, dsize) = PackObject( parentInstance, 0 )
	addtoheader("render", dmp, dsize)
	
	if len(header) > 0:
		# Build the header and offsets
		(dump, size) = BuildFileHeader( header )
	
	if len(dump) > 0:
		hFile = open( filename, 'wb' )
		if hFile is not None:

			# Write to file
			hFile.write( dump )
	
			# Do Clean up
			hFile.close()
		# End If
	# End If
			
	end = time.clock()
	
	# Print status message
	seconds = " in %.2f %s" % (end-start, "seconds")
	message = "Successfully exported " + os.path.basename(filename) + seconds
	meshtools.print_boxed( message )

# ===============================================
# === ParseObject uses the iterator to walk
# === through the object list
# ===============================================
def ParseObjects( object, it ):
	
	# print repr(it), object.name

	if object is None:
		return
	
	# Get the object data
	data = object.getData()
	
	instancetype = 0x0b
	
	# Pick the correct routine to handle the object type
	if type(data) == Types.LampType:
		GenerateType = GenerateLight
	elif type(data) == Types.CameraType:
		instancetype = 0x1a # special camera instance
		GenerateType = GenerateCamera
	elif type(data) == Types.NMeshType:
		GenerateType = GenerateModel
	else:
		print "This object <%s> is unsupported!  Oops!" % (type(data))
		GenerateType = None
	# End If
	
	if GenerateType is not None:
		# Create new instance header
		instanceObject = InitializeHeader( instancetype, object.name )
		# print "Creating Instance Object", object.name, "0x%02x 0x%08x" % (instanceObject.data.id.type, instanceObject.data.id.number)
					
		instanceObject.sibling = GeneratePointer( None )
		instanceObject.child = GeneratePointer( None )
		instanceObject.pMatricies = []
		instanceObject.nMatricies = 0
		instanceObject.flags = -1

		# Retrieve the matrix based on type
		if type(data) == Types.CameraType:
			matrix = object.getInverseMatrix()			
		else:
			matrix = object.getMatrix()
			
		# Skip matrix calcs for light (uses alternate method)
		if type(data) != Types.LampType:
			# At this level only a single matrix is defined
			instanceObject.pMatricies.append( GeneratePointer( GenerateMatrix( object, data, matrix) ) )
			instanceObject.nMatricies = 1				
		# End If
		
			
		def NumUsers( o ):
			try: return o.users
			except: return 0
		
		# Is shared data?
		# print data.name, NumUsers(data)
		
		# WARNING: this keeps objects that are not shared off the shared list!
		# if NumUsers(data) > 1:
			# Add to the list of shared objects!
		#	SharedObjects( data )
		
		# create the object data
		instanceObject.object = GeneratePointer( GenerateType( object, data, matrix ) )

		try:
			# recurse and attach
			instanceObject.sibling = GeneratePointer( ParseObjects( it.next(), it ) )
		except StopIteration:
			instanceObject.sibling = None
		
		return instanceObject
	
	# print "Skipped", object.name, "not supported"
	
	# skip object for now if not supported!
	try:
		return ParseObjects( it.next(), it )
	except StopIteration:
		return None

# ===============================================
# === Creates Binary Data for Header object
# ===============================================	
def PackObject( object, size = 0 ):
	
	if object is None:
		return ("", 0)

	PackCbf = { 0x14: PackMatrix,
				0x16: PackVertexArray, 
				0x11: PackMaterial,
				0x12: PackTexture,
				0x15: PackVertexGroup,
				0x0f: PackLight,
				0x0e: PackCamera,
				0x10: PackModel,
				0x0b: PackInstance,
				0x1a: PackInstance }
					
	# print type(object), dir(object)
		
	# Is the object a pointer type?  If so no packing needs to be done!
							
	# Call Object specific Pack function
	if object.data.id.type in PackCbf:
		
		# print "Packing Object", object.name, "0x%02x (%d) 0x%x (%d)" % (object.data.id.type, object.id.type, object.data.id.number, object.id.number)
		# print PackCbf[object.data.id.type], type( PackCbf[object.data.id.type] )
	
		(dump, size) = PackCbf[object.data.id.type]( object, size )
				
	else:
		print "Skipped Object", object.name, "0x%02x (%d) 0x%x (%d)" % (object.data.id.type, object.id.type, object.data.id.number, object.id.number)
		dump = ""
	# End If
	
	result = PackHeader( object, size )
	
	# Call Header specific Pack function (add header to front!)
	return ( result[0] + dump, result[1] )

# End PackObject

# ===============================================
# === Creates binary data for Instance object
# ===============================================
def PackInstance( object, size = 0):
		
	fmtptr = "Pl"
	dump = ""
	objectsize = 0
		
	# general format
	fmt = fmtptr * 2 + "Pii" + fmtptr
	instancesize = struct.calcsize(fmt)
		
	result = PackHeader(object, instancesize)
	instancesize = result[1] # header does the addition for me ...
		
	# calculate size for matrix ptrs
	size += object.nMatricies * struct.calcsize(fmtptr)
	
	matrixdump = ""
	matrixsize = [0]
	result = (0,0) # reset back to zero
	
	# Get pack data for matricies
	for ptr in object.pMatricies:
		
		dump += struct.pack( fmtptr, instancesize+size+matrixsize[0], VerifyPointer(ptr) )	 # vit_offset and value
					
		# Get pack data for matrix
		result = PackPointer( ptr, result[1] )
		matrixdump += result[0]
				
		# matrix size is for the ptr array
		matrixsize = [result[1]] + matrixsize
	# End For ptr
		
	# matrix data
	size += matrixsize[0]
	dump += matrixdump
		
	# Get Pack data for object
	result = PackPointer( object.object, 0 )
	dump += result[0]
	size += result[1]
	objectsize = result[1]
	
	# print "Object Size, %x" % size, result[1]
		
	# Get pack data for children
	result = PackPointer( object.child, 0 )
	dump += result[0]
	size += result[1]
	childsize = result[1]
		
	# print "Child Size, %x" % size, result[1] # should be zero!
	
	if matrixdump is not "":
		offsetmatrix = instancesize
	else:
		offsetmatrix = 0
	
	if object.object is not None and object.object.type != 0:
		offsetobject = instancesize + object.nMatricies * struct.calcsize(fmtptr) + \
						matrixsize[0]
		# print "Object offset calc", offsetobject
	else:
		offsetobject = 0
		
	if object.child is not None and object.child.type != 0:
		# print "Child offset calc"
		offsetchild	= instancesize + object.nMatricies * struct.calcsize(fmtptr) + \
						 matrixsize[0] + objectsize
	else:
		offsetchild = 0

	if object.sibling is not None and object.sibling.type != 0:
		# print "Sibling offset calc"
		offsetsibling = instancesize + object.nMatricies * struct.calcsize(fmtptr) + \
						 matrixsize[0] + objectsize + childsize
	else:
		offsetsibling = 0
				
	# inline lambda function
	# (lambda x: x*2)(3)
		
	result = PackPointer( object.sibling, 0 )
		
	print object.name, "<%x> Object offset 0x%x" % ( object.id.number, objectsize ), "child offset 0x%x" % offsetchild , "sibling offset 0x%x" % offsetsibling
	
	return ( struct.pack( fmt, offsetchild, VerifyPointer(object.child),
							 offsetsibling, VerifyPointer(object.sibling),
							 offsetmatrix, object.nMatricies, 
							 object.flags, 
							 offsetobject, VerifyPointer(object.object) ) + \
				dump + result[0], result[1] + size + struct.calcsize(fmt) ) # change 1
				# instancesize + object.nMatricies * struct.calcsize(fmtptr) + matrixsize[0] + objectsize - 20 )
	
# End PackInstance

# ===============================================
# === Creates a hash id from string
# ===============================================
def GenerateHashValue( string ):
	crctt = ~0	
	
	for char in string:
		c = int(struct.unpack("b", char )[0]) # convert ascii char to int (unpack returns tuple, even on single items)
		crctt = ( crctab[ (crctt ^ c) & 0xff ] ^ ( (crctt>>8) & 0x00FFFFFFL ) )
	return crctt	

# End GenerateHashValue

# ===============================================
# === Creates Generic header data, does not modify
# === size attribute
# ===============================================
def InitializeHeader( type, name ):
	
	header = CCommonObjectHeader()	
	
	header.data	= CCommonObjectData(type, GenerateHashValue(name))		# duplicate data to maintain integrity after pointer resolution
	header.id	= CCommonObjectIdentifier(type, header.data.id.number)

	header.name = name # debug only!  Please remove before finish!
	
	# print "Creating Header %s 0x%02x 0x%8x" % (name, header.data.id.type, header.data.id.number)
	
	return header	

# End InitializeHeader

# ===============================================
# === Creates Binary Data for Header object
# ===============================================
def PackHeader( object, size = 0 ):
	
	fmt = "Ilili"
	size += struct.calcsize(fmt)
	dump = struct.pack( fmt, 
			size, 
			object.data.id.number, 
			object.data.id.type, 
			object.id.number, 
			object.id.type )

	return (dump, size)

# End PackHeader

# ===============================================
# === Creates Generic Pointer Object
# ===============================================
def GeneratePointer( header ):
	
	# print "Type:", type(header)
	# print "Is Instance:", isinstance( header, CCommonObjectPointer )
	
	if not isinstance( header, CCommonObjectPointer ):
		pointerObj = CCommonObjectPointer()
		
		if header is not None:	
			pointerObj.type = header.data.id.type
			pointerObj.ptr = header
	
		return pointerObj
		
	return header	
# ===============================================
# === This function just verifies pointer and 
#    passes correct object through
# ===============================================
def PackPointer( object, size = 0 ):
	
	if object is None:
		return ("", 0)	
	
	# print object, type(object), dir(object)
	# print "name", repr(object.ptr)
	# print object.ptr, type(object.ptr), dir(object.ptr)
	
	if type(object.ptr) == types.LongType:
		return ("",0)
		
	try:
		return PackObject( object.ptr, size )	
	except AttributeError:
		return PackObject(object, size)
	
# End PackPointer

# ===============================================
# === This function just verifies pointer and 
#    passes correct object through
# ===============================================
def VerifyPointer( object ):
	if object is None:
		return 0x00
		
	# print " Verify:", object.type, object.ptr,isinstance( object.ptr, CCommonObjectHeader )
	
	if object.ptr == None:
		return 0x00

	if isinstance( object.ptr, CCommonObjectHeader ):
		return 0x08
	
	return 0x07
	
# End VerifyPointer

# ===============================================
# === This function just verifies pointer and 
#    passes correct value through
# ===============================================
def VerifyOffset( object, offset ):
	if object == None:
		return 0

	print " Verify:", object.type, object.ptr,isinstance( object.ptr, CCommonObjectHeader )
			
	if isinstance( object.ptr, CCommonObjectHeader ):
		return offset
		
	if object.ptr == None:
		return 0

	return object.ptr
		
# End VerifyOffset

# ===============================================
# === Creates a Matrix object
#	: Notes :
#		Function only creates GL_MODELVIEW 
#		until discovery of matrix type variable
# ===============================================
def GenerateMatrix( object, data, matrix ):
		
	# print "Creating Matrix", object.name
	
	# Initialize a new matrix object
	matrixObj = InitializeHeader( 0x14, data.name + ".Matrix" )
	
	# Matrix
		# header
		# matrixMode - modelview ... etc
		# matrix[16] - c style
		
	matrixObj.matrixMode = 0x1700 # GL_MODELVIEW
	matrixObj.matrix = []
	
	for r in matrix:
		for c in r:
			matrixObj.matrix.append( c )
	# End For i
		
	return matrixObj	
	
# End GenerateMatrix

# ===============================================
# === Creates Binary data for Matrix object
# ===============================================
def PackMatrix( object, size = 0 ):

	dump = struct.pack( "i", object.matrixMode )
	size += struct.calcsize( "i" )
	for num in object.matrix:
		dump += struct.pack( "f", num )
		size += struct.calcsize( "f" )
	# End For num
	
	return (dump, size)

# End PackMatrix

# ===============================================
# === Creates a VertexArray object
# ===============================================
def GenerateVertexArray( object, data, type, size, verticies, state = ["", 0] ):

	# make sure names don't collide
	if state[0] != data.name:
		state[0] = data.name
		state[1] = 0
	else:
		state[1]+=1
		
	vertexObj = InitializeHeader( 0x16, object.name + "." + data.name + ".VertexArray." + str(state[1]) )
	vertexObj.vertexType = type
	vertexObj.vertexsize = size
	vertexObj.nVerticies = 0
	vertexObj.verticies = []
	
	# print verticies
	
	for c in verticies:
		# print c
		for r in c:
			# print r
			vertexObj.verticies.append( r )
		# End For r
	# End For c
			
	vertexObj.nVerticies = len( verticies )
	# print "nVerticies:", vertexObj.nVerticies
	
	return vertexObj
	
# End GenerateVertexArray

# ===============================================
# = Creates binary data for VertexArray
# ===============================================
def PackVertexArray( object, size = 0 ):
	
	fmt = "iii"
	dump = struct.pack( fmt, object.vertexType,object.vertexsize, object.nVerticies )
	size += struct.calcsize( fmt )

	for num in object.verticies:
		dump += struct.pack( "f", num )
		size += struct.calcsize( "f" )
	# End For num
	
	return (dump, size)
# End PackVertexArray

# This object is made of tuples
# (offset, object data)
# initially offset is 0 until binary is built
gObjectDict = {}

# ===============================================
# === Finds an object in the Global Look Up Table
# ===============================================
def GetObjectData( name, objDict = gObjectDict ):

	# print "Shared Object:", name, type(name), (type(name) == types.StringType)
	
	if name != None:	
		if type(name) == types.StringType:
			name = GenerateHashValue(name)
		
		string = "%xh" % name
		
		if string in objDict:
			return objDict[string]
		# End if
	# End if
	return None
# End LookUpName

def SetObjectData( name, object, objDict = gObjectDict ):

	if name != None:
		if type(name) == types.StringType:
			name = GenerateHashValue(name)
		
		string = "%xh" % name
		
		if string not in objDict:
			tup = (0,object)
			objDict[string] = tup
			return tup
		# End if
	# End if
	return None
# End SetObjectData

# ===============================================
# === Creates a Material object
# ===============================================
def FindCreateTexture( o, data, material, texture):
	
	name = None
	shobject = None
	
	# print dir(texture)
	# print texture.tex, "\n",dir(texture.tex)
	# for o in dir(texture.tex):
	#	print o, 
	#	try:
	#		try: print eval("texture.tex.%s()" % o)
	#		except: print eval("texture.tex.%s" % o)
	#	except: print
	
	image = texture.tex.getImage()
	# print image, "\n",dir(image)
	
	if image == None:
		# print "Object", object, "Does not use a supported texture type, texture must be loaded from a file"
		return None
	# End if
	
	# get the file name ... see if it exists in the 'table'
	name = image.name
	shobject = GetObjectData( name )

	# print "Shared Object:", shobject
	# print "Name:", name

	if shobject != None:
		# print "object data found!", shobject, type(shobject)
		shobject = shobject[1]
	else:
		# If no data, create texture data and attach to the global list

		oDepthToSize = {32:4,24:3}
		oDepthToGlDepth = {32:0x1908,24:0x1907}
	
		shobject = InitializeHeader( 0x12, name )
		
		# Build texture object!
		# get width, height, wrap (x,y), alpha setting
		# walk bits and build array!
		# print object
		
		shobject.iTexture = 0 # unassigned for internal use only
		shobject.width, shobject.height = image.getSize()
		shobject.xrepeat = 0x2900 # (gl_clamp) # image.xrep
		shobject.yrepeat = 0x2900 # (gl_clamp)# image.yrep
		
		depth = image.getDepth()
		shobject.size = oDepthToSize[depth]
		shobject.depth = oDepthToGlDepth[depth]
		
		xrange = range(shobject.width)
		yrange = range(shobject.height)
		
		yrange.reverse()
		
		shobject.bytes = [] # this is the texture data
		
		print yrange

		for x in xrange:
			shobject.bytes += [ map(lambda a: a*0xFF, image.getPixelF(x,y)) for y in yrange ] 
		# End For
		
		# print object, object.width, object.height, object.xrepeat, object.yrepeat, object.size, object.depth
		
		# add the new object to the 'list'
		SetObjectData(name, shobject)
		
	# End if
	
	pointerObj = CCommonObjectPointer()
	pointerObj.type = shobject.id.type
	pointerObj.ptr = shobject.id.number
	
	# print "Shared Object:", shobject.name, shobject.id.number
	
	# print pointerObj, pointerObj.type, pointerObj.ptr
	
	return pointerObj
	
# End FindCreateTexture

def PackTexture( object, size = 0 ):

	fmt = "iIiiiiii"
	
	# Get size of header
	headersize = PackHeader( object, 0 )[1]
	
	size += headersize + struct.calcsize( fmt )
	dump = struct.pack( fmt, object.iTexture,
							object.width, 
							object.height, 
							object.xrepeat, 
							object.yrepeat,
							object.size,
							object.depth,
							size)

	# walk bytes and attach to end of dump
	# keep track of size
	
	texfmt = "B" * object.size
	size += struct.calcsize(texfmt) * len(object.bytes)
	
	# object.bytes.reverse()
	
	# ... optmization ...
	if object.size == 3:
		for b in object.bytes: dump += struct.pack(texfmt,b[0],b[1],b[2])
	elif object.size == 4:
		for b in object.bytes: dump += struct.pack(texfmt,b[0],b[1],b[2],b[3])
	else:
		print "While creating texture, unknown depth count!"
	
	return dump, size - headersize

# End PackTexture

# ===============================================
# === Creates a Material object
# ===============================================
def GenerateMaterial( object, data, material ):

	textures =  material.getTextures()
	
	materialObj = InitializeHeader( 0x11, data.name )
	materialObj.face = 0x0404 # GL_Front
	materialObj.mode = 0x1602 # Gl_Ambient_and_Diffuse
	materialObj.vertexArray = [0,0,0,0,0]
	
	# print "Texture: ", len(textures), textures
	
	materialObj.textureArray = []
	
	# Create texture array ( blender has max of 8 but I don't need to allocate unused)
	for t in textures: 
		if t: materialObj.textureArray += [ GeneratePointer( FindCreateTexture( object, data, material, t) ) ]
	# End For
	
	materialObj.nTextures = len( materialObj.textureArray )
	
	materialObj.nTextureObjects = len( materialObj.textureArray )
	# print "Texture:",materialObj.nTextureObjects,materialObj.textureArray
	
	# generate array of pointers
	
	# print "Rgb", material.rgbCol
	# print "Amb", material.amb
	
	# ambient
	materialObj.vertexArray[0] = GeneratePointer( GenerateVertexArray( object, data, 0x1200, 4, [ [material.rgbCol[i]*material.amb for i in range(0,3)] + [material.alpha]] ))
	
	# diffuse
	materialObj.vertexArray[1] = GeneratePointer( GenerateVertexArray( object, data, 0x1201, 4, [ material.rgbCol + [material.alpha]] ))
	
	# specular
	materialObj.vertexArray[2] = GeneratePointer( GenerateVertexArray( object, data, 0x1202, 4, [ material.specCol + [material.alpha]] ))
	
	# emissive
	materialObj.vertexArray[3] = GeneratePointer( GenerateVertexArray( object, data, 0x1600, 4, [ [material.rgbCol[i] * material.emit for i in range(0,3)] + [material.alpha]] ))
	
	# shiny (hard)
	materialObj.vertexArray[4] = GeneratePointer( GenerateVertexArray( object, data, 0x1601, 1, [[material.hard/127.0]] )) # 127 becuase OpenGl rates shiny from 0->2.0, hard is 0->255
	# GenerateVertexArray( object, data, 0x1601, 4, [[material.hard/127.0]] )) # 127 becuase OpenGl rates shiny from 0->2.0, hard is 0->255
	
	return materialObj
	
# End GenerateMaterial

# ===============================================
# === Creates binary data for Material
# ===============================================
def PackMaterial( object, size = 0 ):
	
	# print "initial object size", size
	
	fmt = "ii", "iP"
	fmtptr = "Pi"
	
	headersize = PackHeader( object, 0 )[1]
	
	(dump, objectsize) = ( struct.pack( fmt[0], object.face, object.mode ),
										struct.calcsize( fmt[0] ) )
	
	# calculate the total object size for offsets
	objectsize += headersize + \
				struct.calcsize( fmtptr ) * len( object.vertexArray ) + \
				struct.calcsize( fmt[1] )

	# print "Object Size", objectsize
	# print "Offset Size %x" % (objectsize + size)
	
	offset = [size + objectsize]
	
	vertexarraydump = ""
	
	for ptr in object.vertexArray:
		
		dump += struct.pack( fmtptr, offset[0], VerifyPointer( ptr ) )
		
		# Create data for
		result = PackPointer( ptr, 0 )
		
		# capture buffer data (to combine with dump)
		vertexarraydump += result[0]
		
		# Prep offset for next iteration
		offset = [offset[0] + result[1]] + offset
		
	# End For
	
	texturearraydump = ""
	
	dump += struct.pack( fmt[1], object.nTextures, offset[0] ) + vertexarraydump
	
	if object.nTextures > 0:

		# recalculate the next offset for the texture array
		ptrsize = struct.calcsize( fmtptr * object.nTextures )
		offset = [offset[0] + ptrsize] + offset
		
		# create pointer array for textures
		for ptr in object.textureArray:

			# build pointer array			
			dump += struct.pack( fmtptr, VerifyOffset(ptr,offset[0]), VerifyPointer(ptr) )

			result = PackPointer( ptr, 0 )

			# capture buffer data (to combine with dump)
			texturearraydump += result[0]

			# Prep offset for next iteration
			offset = [offset[0] + result[1]] + offset

		# End For
	else:
		dump += struct.pack( fmt[1], object.nTextures, 0 )
	# End If

	# take off the header or it will be calculated twice
	objectsize = (offset[0] - headersize)

	# print "Material <%x, %d>" % (objectsize, objectsize)

	# Patch the rest of the data
	dump += texturearraydump

	return ( dump, objectsize )

# End PackMaterial

# ===============================================
# === Creates a VertexGroup object
# ===============================================
def GenerateVertexGroup( object, data, faces, state = ["", 0] ):
	
	# make sure names don't collide
	if state[0] != object.name:
		state[0] = object.name
		state[1] = 0
	else:
		state[1]+=1
		
	# print "GenerateVertexGroup", type(data), "::", type(face)
	# VertexGroup
		# header
		# material - single instance
		# primType
		# nArrays
		# vertexArray
	
	vertexGroupObj = InitializeHeader( 0x15, "%s.VertexGroup.%i" % (data.name,state[1]) )
	
	if data.materials == []:
		print "Error: object <%s> does not have a material!" % object.name
				
	# create material
	vertexGroupObj.material = GeneratePointer( GenerateMaterial( object, data, data.materials[faces[0].materialIndex] ) )
	
	# use uv coord if texture
	# print face.uv # not used yet
		
	#define GL_TRIANGLES                      0x0004
	#define GL_TRIANGLE_STRIP                 0x0005
	#define GL_TRIANGLE_FAN                   0x0006
	#define GL_QUADS                          0x0007
	#define GL_POLYGONS						  0x0009
			
	vertexGroupObj.nArrays = 0
	vertexGroupObj.vertexArray = []
		
	# ------------------------
	# Please note Order Matters for the following statements!
	# ------------------------
	
	( lvertex,lnormal, luvs, luv, lcolor) = ( len( faces[0].v[0].co ), len( faces[0].no ), len( faces[0].uv ),0, 0 ) # number of points
	( vertex, normal, color, uv ) = ( [], [], [], [] )
	
	length = len(faces[0].v) # number of verticies
	walkary = range(0,length)
	
	# New partial hardcode
	if length == 3: vertexGroupObj.primType = 0x04
	elif length == 4: vertexGroupObj.primType = 0x07
	else: vertexGroupObj.primType = 0x09
		
	for face in faces:
		
		if len(face.v[0].co) != lvertex: 
			print object.name, object.data, "Object was %i but now has alternate vertex count of %i, please fix!" % ( lvertex, len(face.v[0].co))
			continue
	
		# create vertex, normal, color, and uv arrays from the face data
		if lvertex > 0: vertex += [ v.co for v in face.v ]
		if luvs > 0: uv += [ v for v in face.uv ]
		
		if lnormal > 0: normal += [ face.no for i in walkary ]
		
		if face.col == []: color += [ vertexGroupObj.material.ptr.vertexArray[1].ptr.verticies for i in walkary ]
		else: color += [ [col.r/255.0, col.g/255.0, col.b/255.0, col.a/255.0 ] for col in face.col ]
		
	lcolor = len(color[0]) # only work first time
	if uv != []: luv = len(uv[0])
	
	# print "Vertex", lvertex, len(vertex), walkary
	# print "Normal", lnormal, len(normal)
	# print "Color", lcolor, len(color[0]), color[0][:4]
	# print "UV", luv, len(uv)# , uv 
	# for coord in uv: print "\t(%0.2f,%0.2f) " % (coord[0:])
	
	# print "Vertex %i" % len(vertex[i])," ","Normal %i" % len(normal[i])," ","Color %i" % len(color[i])
	# for i in range(len(vertex)):
	# 	for j in range(len(color[i])):
	# 		try: print "%+0.3f\t" % vertex[i][j],
	# 		except: print "      \t",
	# 		try: print "%+0.3f\t" % normal[i][j],
	# 		except: print "      \t",
	# 		print "%+0.3f\t" % color[i][j]
	# print
	
	if vertex != []: vertexGroupObj.vertexArray.append( GeneratePointer( GenerateVertexArray( object, data, 0x0009, lvertex, vertex ) ) )
	if normal != []: vertexGroupObj.vertexArray.append( GeneratePointer( GenerateVertexArray( object, data, 0x0009, lnormal, normal ) ) )
	if color != []: vertexGroupObj.vertexArray.append( GeneratePointer( GenerateVertexArray( object, data, 0x0009, lcolor, color ) ) )
	if uv != []: vertexGroupObj.vertexArray.append( GeneratePointer( GenerateVertexArray( object, data, 0x0009, luv, uv ) ) )
		
	# print "vertlist: %d, normals: %d, color: %d" % (len(verts),len(no),len(col))
		
	vertexGroupObj.nArrays = len( vertexGroupObj.vertexArray )

	return vertexGroupObj
	
# End GenerateVertexGroup

# ===============================================
# === Creates binary data for vertex group
# ===============================================	
def PackVertexGroup( object, size = 0 ):
	
	fmt = "ii"
	fmtptr = "Pi"
	
	dump = ""
	
	headersize = PackHeader( object, 0 )[1]
	
	# print "Pack Vertex Group"
	
	# Base object size
	objectsize = struct.calcsize( fmtptr + fmt )
	
	ptrsize = struct.calcsize( fmtptr ) * object.nArrays
	
	# Process structure		
	dump += struct.pack( "ii", object.primType, object.nArrays )
	
	offset = [headersize + objectsize + ptrsize]
	
	vertexarraydump = ""
	
	for ptr in object.vertexArray:
		
		# print "Offset - 0x%x" % offset[0]
		
		dump += struct.pack( fmtptr, offset[0], VerifyPointer( ptr ) )
				
		result = PackPointer( ptr, 0 ) # this is returning the wrong value!
		
		vertexarraydump += result[0]
		offset = [offset[0] + result[1]] + offset
		
	# End For

	result = PackPointer( object.material, offset[0] ) # will do math for me!
		
	dump = struct.pack( fmtptr, offset[0], VerifyPointer( object.material ) ) + dump + vertexarraydump + result[0]
	objectsize = result[1] - headersize
	 # result[1] 

	# print "VertexGroup:", object.data.id.number, objectsize	
	# print "VertexGroup <%x, %d>" % (objectsize, objectsize)
	
	return (dump, objectsize)
# End PackPackVertexGroup

# ===============================================
# === Creates a light object
# ===============================================
def GenerateLight( object, data, matrix ):
	
	# print "Creating Light", object.name
		
	# Create the header data (size is not initalized)
	lightObj = InitializeHeader( 0x0f, data.name )
		
	lightObj.lightid = 0		# tbd !!!
	lightObj.pVerticies = []
	
	# ambient
	lightObj.pVerticies.append( GeneratePointer( GenerateVertexArray( object, data, 0x1200, 4, [data.col + [1.0]] ) ) )
	
	if (data.Modes['NoDiffuse'] & data.mode) == 0:
		# diffuse
		lightObj.pVerticies.append( GeneratePointer( GenerateVertexArray( object, data, 0x1201, 4, [data.col + [1.0]] ) ) )
	# End If
		
	if (data.Modes['NoSpecular'] & data.mode) == 0:
		# specular
		lightObj.pVerticies.append( GeneratePointer( GenerateVertexArray( object, data, 0x1202, 4, [data.col + [1.0]] ) ) )
	# End If
	
	# prep the location as list for passing to Generate function
	location = []; map( lambda a: location.append(a), object.getLocation() )

	# convert radians to degrees for use with open gl
	lookat = map( lambda a:a, object.getEuler() )

	if (data.type == data.Types['Sun'] 
		or data.type == data.Types['Area'] 
		or data.type == data.Types['Hemi']): # directional (set w to 0)
		lightObj.pVerticies.append( GeneratePointer( GenerateVertexArray( object, data, 0x1203, 4, [location + [1.0]] ) ) )

	else:					
		if data.type == data.Types['Lamp']: # positional
			lightObj.pVerticies.append( GeneratePointer( GenerateVertexArray( object, data, 0x1203, 4, [location + [1.0]] ) ) )
			lightObj.pVerticies.append( GeneratePointer( GenerateVertexArray( object, data, 0x1204, 3, [lookat] ) ) )
						
		elif data.type == data.Types['Spot']:
			print "Processing Spot"

			lightObj.pVerticies.append( GeneratePointer( GenerateVertexArray( object, data, 0x1203, 4, [location + [1.0]] ) ) )
			lightObj.pVerticies.append( GeneratePointer( GenerateVertexArray( object, data, 0x1204, 3, [lookat] ) ) )
			lightObj.pVerticies.append( GeneratePointer( GenerateVertexArray( object, data, 0x1206, 1, [[data.getSpotSize()]] ) ) )
					
		elif data.type == data.Types['Photon']:
			print "Not Supported"
		# End If
		
	lightObj.nVerticies = len( lightObj.pVerticies )
	
	# sort list by type before exit (list must be sorted)
		
	return lightObj # return this, to connect next sibling object

# End GenerateLight

# ===============================================
# === Makes binary string for light object
# ===============================================
def PackLight( object, size = 0 ):

	fmtptr = "Pi"
	ptrsize = struct.calcsize( fmtptr )
	
	fmt =  "iPi"
	fmtsize = struct.calcsize( fmt )
				
	# get the header size!
	(dmp, headersize) = PackHeader( object, fmtsize )
					
	# calculate offset for first array pointer	
	offset = [ headersize+ptrsize * object.nVerticies ]
	
	dump = struct.pack( fmt, object.lightid, headersize, object.nVerticies )
	
	vertexarraydump = ""
	
	for ptr in object.pVerticies:
				
		# create array data
		result = PackPointer( ptr, 0 )
						
		# create pointer array (patch w/offsets)
		dump += struct.pack( fmtptr, offset[0], VerifyPointer(ptr) )
		
		# print "Light", offset[0]
		
		# build the dump string and the object size
		vertexarraydump += result[0]
		offset = [offset[0] + result[1]] + offset
		
	# End For ptr
		
	return (dump + vertexarraydump, fmtsize + (offset[0] - headersize) )
	
# End PackLight

# ===============================================
# === Creates a Camera object
# 	: Notes :
#		Ortho code not implemented
# ===============================================
def GenerateCamera( object, data, matrix ):
	
	# print "Creating Camera", object.name
	
	# Camera Object
		# header
		# iType - ortho/persp
		# fAspect
		# fNear
		# fFar
		# fCustom - range(otho)/fov(per)
		
	# print map( (lambda a: eval( 'Blender.Scene' + a ), dir(Blender.Scene))
		
	scene = Scene.GetCurrent()
	if scene is not None:
		context = scene.getRenderingContext()
		if context is not None:	
			factor = context.imageSizeX()/context.imageSizeY()
		# End If
	# End If
			
	# dump camera variables
	# print "--Camera", string.join( dir(object), "\n" )
	# print string.join( dir(data), "\n" )
	
	cameraObj = InitializeHeader( 0x0e, data.name )
	cameraObj.iType 	= data.getType() # persp = 0
	cameraObj.fAspect	= 0 # can be factor
	cameraObj.fNear		= data.getClipStart()
	cameraObj.fFar		= data.getClipEnd()
	
	if cameraObj.iType == 0:
		cameraObj.fCustom = 2 * 180/math.pi * math.atan( 16 * factor / data.getLens() )
	else:
		cameraObj.fCustom = data.getScale()
			
	return cameraObj # return this, to connect next sibling object

# End GenerateCamera

# ===============================================
# === Makes binary string for camera object
# ===============================================
def PackCamera( object, size = 0 ):

	fmt =  "iffff"
	dump = struct.pack( fmt, object.iType, 
				object.fAspect, 
				object.fNear, 
				object.fFar, 
				object.fCustom )
	size += struct.calcsize( fmt )
	
	return (dump, size)
	
# End PackCamera
	
# ===============================================
# === Creates a Model Object
# ===============================================
def GenerateModel( object, data, matrix ):
	
	# Model Object
		# getBoundingBox - todo
		# getMaterials - todo
		
	# print "- Creating Model "

	modelObj = InitializeHeader( 0x10, data.name )
	modelObj.subtype = 0
	
	modelObj.vertexGroup = []
	
	facesByMaterial ={'Triangle': [ [] for i in range(len(data.materials)) ],
					      'Quad': [ [] for i in range(len(data.materials)) ],
					   'Polygon': [ [] for i in range(len(data.materials)) ] }
	
	# Create a list of faces by material 
	for face in data.faces:
		length = len(face.v)
		if length == 3: key = 'Triangle'
		elif length == 4: key = 'Quad'
		else: key = 'Polygon'
		# sort by number of faces as well!
		facesByMaterial[key][face.mat].append(face)
		
	# material, vertex color array, vertex array, normal array
	for (key,value) in facesByMaterial.items():
		for faces in value:
			if faces != []:
				# print "Vertex data", key
				# print "Creating Vertex Group Data", object.name
				modelObj.vertexGroup.append( GeneratePointer( GenerateVertexGroup( object, data, faces ) ) )
		# End For faces
	# End For arrays
	
	modelObj.nGroups = len( modelObj.vertexGroup )
		
	# if len(data.verts):
	#	print "its verticies are obviously of type:", type(data.verts[0])
	# print "and its faces:", Types.NMFaceType
				
	return modelObj # return this, to connect next sibling object

# End GenerateModel

# ===============================================
# === Creates binary data for Model Object
# ===============================================
def PackModel( object, size = 0 ):

	fmt = "iPi"
	fmtptr = "Pi"
	
	# initial calcs
	objectsize = struct.calcsize( fmt )
	
	# use to determine struct size
	result = PackHeader( object, 0 )
	headersize = result[1]
	
	dump = struct.pack( fmt, object.subtype, objectsize+headersize, object.nGroups )
	
	# pre calculate base pointer size for vertex group offset
	ptrsize = struct.calcsize( fmtptr * object.nGroups )

	# prep initial offset
	offset = [headersize + objectsize + ptrsize]
	
	vertexgroupdump = ""
	
	for ptr in object.vertexGroup:
		
		# build pointer array
		dump += struct.pack( fmtptr, offset[0], VerifyPointer( ptr ) )
		
		# build vertex group
		result = PackPointer( ptr, 0 )
		
		# build vertex group buffer
		vertexgroupdump += result[0]
		
		# record data for next iter
		offset = [offset[0] + result[1]] + offset
				
	# End for
	
	dump += vertexgroupdump
	objectsize = offset[0] - headersize
	# result[1]
	
	# print "Model <%x, %d>" % (objectsize, objectsize)
		
	return (dump, size + objectsize)
# End PackModel

def BuildFileHeader( objectList ):
	size = 0
	header = ""
	dump = ""
	
	print "Creating File Header"
	
	# number of entries
	size += struct.calcsize( "fii" )

	# store the list count and offset for array
	header = struct.pack( "fii", VERSION, len(objectList), size )
	
	# print "Object List Length", len(objectList)
			
	# calculate initial size for pointer array
	size += struct.calcsize( "iii"*len(objectList) )
	
	# each list item is made of three fields,
	# number, data, size
	for o in objectList:
		
		print "Adding header Object %s id %#x size (%i)" % (o[0],o[1],o[3])
		
		# add the sction data, size of section, hash, and
		header += struct.pack("iii",o[1],o[3],size)
		
		# increment the size
		size += o[3]
		
		# add to the end of the header the pointers and finally the dump
		dump += o[2]
		
	# End for
	
	return header + dump, size
# End BuildFileHeader


def PackSharedObjects():

	if len(gObjectDict) <= 0:
		return "",0
	
	size = 0
	dump, pointers, objects = ("", "", "")
	nobjects = len(gObjectDict)
	
	# sort the objects in to the order for a balanced tree -- not done!
	
	(head, lhead) = "iPiP", struct.calcsize("iPiP")
	(leaf, lleaf) = "iiii", struct.calcsize("iiii")
	(pointer,lpointer) = "Pl",struct.calcsize("Pl")
	
	# create an array to include all of the 
	# items in the list!
	
	basebtree = lhead + lleaf # offset of first (NULL) leaf
	basepointer = basebtree + ( lleaf * nobjects )
	baseobject = basepointer + ( lpointer * nobjects )
	
	# Create an empty node at the beginning
	dump = struct.pack(leaf, 0, 0, 0, 0)
		
	for k, v in gObjectDict.iteritems():
		# add the leaf
		basebtree += lleaf
		dump += struct.pack(leaf, lhead, lhead, 0, basepointer)
		
		# add the pointer
		basepointer += lpointer
		pointers += struct.pack(pointer, baseobject, 0x08)
		
		# pack the object
		result = PackObject( v[1] )

		baseobject += result[1]
		objects += result[0]
		
	header = struct.pack(head,0,lhead+lleaf,len(gObjectDict),basebtree)
	
	return (header + dump + pointers + objects), baseobject
	
# End PackSharedObjects

# ===============================================
# Global Variables
# ===============================================

# Crc value table (produces crazy warning which can be ignored ... )
crctab = [0x00000000,0x77073096,0xee0e612c,0x990951ba,0x076dc419,0x706af48f,0xe963a535,0x9e6495a3,0x0edb8832,0x79dcb8a4,0xe0d5e91e,0x97d2d988,0x09b64c2b,0x7eb17cbd,0xe7b82d07,0x90bf1d91,0x1db71064,0x6ab020f2,0xf3b97148,0x84be41de,0x1adad47d,0x6ddde4eb,0xf4d4b551,0x83d385c7,0x136c9856,0x646ba8c0,0xfd62f97a,0x8a65c9ec,0x14015c4f,0x63066cd9,0xfa0f3d63,0x8d080df5,0x3b6e20c8,0x4c69105e,0xd56041e4,0xa2677172,0x3c03e4d1,0x4b04d447,0xd20d85fd,0xa50ab56b,0x35b5a8fa,0x42b2986c,0xdbbbc9d6,0xacbcf940,0x32d86ce3,0x45df5c75,0xdcd60dcf,0xabd13d59,0x26d930ac,0x51de003a,0xc8d75180,0xbfd06116,0x21b4f4b5,0x56b3c423,0xcfba9599,0xb8bda50f,0x2802b89e,0x5f058808,0xc60cd9b2,0xb10be924,0x2f6f7c87,0x58684c11,0xc1611dab,0xb6662d3d,0x76dc4190,0x01db7106,0x98d220bc,0xefd5102a,0x71b18589,0x06b6b51f,0x9fbfe4a5,0xe8b8d433,0x7807c9a2,0x0f00f934,0x9609a88e,0xe10e9818,0x7f6a0dbb,0x086d3d2d,0x91646c97,0xe6635c01,0x6b6b51f4,0x1c6c6162,0x856530d8,0xf262004e,0x6c0695ed,0x1b01a57b,0x8208f4c1,0xf50fc457,0x65b0d9c6,0x12b7e950,0x8bbeb8ea,0xfcb9887c,0x62dd1ddf,0x15da2d49,0x8cd37cf3,0xfbd44c65,0x4db26158,0x3ab551ce,0xa3bc0074,0xd4bb30e2,0x4adfa541,0x3dd895d7,0xa4d1c46d,0xd3d6f4fb,0x4369e96a,0x346ed9fc,0xad678846,0xda60b8d0,0x44042d73,0x33031de5,0xaa0a4c5f,0xdd0d7cc9,0x5005713c,0x270241aa,0xbe0b1010,0xc90c2086,0x5768b525,0x206f85b3,0xb966d409,0xce61e49f,0x5edef90e,0x29d9c998,0xb0d09822,0xc7d7a8b4,0x59b33d17,0x2eb40d81,0xb7bd5c3b,0xc0ba6cad,0xedb88320,0x9abfb3b6,0x03b6e20c,0x74b1d29a,0xead54739,0x9dd277af,0x04db2615,0x73dc1683,0xe3630b12,0x94643b84,0x0d6d6a3e,0x7a6a5aa8,0xe40ecf0b,0x9309ff9d,0x0a00ae27,0x7d079eb1,0xf00f9344,0x8708a3d2,0x1e01f268,0x6906c2fe,0xf762575d,0x806567cb,0x196c3671,0x6e6b06e7,0xfed41b76,0x89d32be0,0x10da7a5a,0x67dd4acc,0xf9b9df6f,0x8ebeeff9,0x17b7be43,0x60b08ed5,0xd6d6a3e8,0xa1d1937e,0x38d8c2c4,0x4fdff252,0xd1bb67f1,0xa6bc5767,0x3fb506dd,0x48b2364b,0xd80d2bda,0xaf0a1b4c,0x36034af6,0x41047a60,0xdf60efc3,0xa867df55,0x316e8eef,0x4669be79,0xcb61b38c,0xbc66831a,0x256fd2a0,0x5268e236,0xcc0c7795,0xbb0b4703,0x220216b9,0x5505262f,0xc5ba3bbe,0xb2bd0b28,0x2bb45a92,0x5cb36a04,0xc2d7ffa7,0xb5d0cf31,0x2cd99e8b,0x5bdeae1d,0x9b64c2b0,0xec63f226,0x756aa39c,0x026d930a,0x9c0906a9,0xeb0e363f,0x72076785,0x05005713,0x95bf4a82,0xe2b87a14,0x7bb12bae,0x0cb61b38,0x92d28e9b,0xe5d5be0d,0x7cdcefb7,0x0bdbdf21,0x86d3d2d4,0xf1d4e242,0x68ddb3f8,0x1fda836e,0x81be16cd,0xf6b9265b,0x6fb077e1,0x18b74777,0x88085ae6,0xff0f6a70,0x66063bca,0x11010b5c,0x8f659eff,0xf862ae69,0x616bffd3,0x166ccf45,0xa00ae278,0xd70dd2ee,0x4e048354,0x3903b3c2,0xa7672661,0xd06016f7,0x4969474d,0x3e6e77db,0xaed16a4a,0xd9d65adc,0x40df0b66,0x37d83bf0,0xa9bcae53,0xdebb9ec5,0x47b2cf7f,0x30b5ffe9,0xbdbdf21c,0xcabac28a,0x53b39330,0x24b4a3a6,0xbad03605,0xcdd70693,0x54de5729,0x23d967bf,0xb3667a2e,0xc4614ab8,0x5d681b02,0x2a6f2b94,0xb40bbe37,0xc30c8ea1,0x5a05df1b,0x2d02ef8d ]
hFile = None

# ===============================================
# Class Definitions
# ===============================================

class CCommonObjectPointer:
	"Defines Common Object Pointer: Ptr, Type"
	def __init__(self):
		self.ptr = None
		self.type = 0x0

class CCommonObjectIdentifier:
	"Defines Common Object Identifier: ID, Number"
	def __init__(self, type, number):
		self.number	= number
		self.type	= type
	
class CCommonObjectData:
	"Defines Common Object Data: Size, Id"
	def __init__(self, type, number):
		self.size	= -1
		self.id		= CCommonObjectIdentifier(type, number)
	
class CCommonObjectHeader:
	"Defines Common Object Header: Data, Id"
	pass
	
# ===============================================
# Executable Code

Blender.Window.FileSelector(visionary_export, "Export VE");