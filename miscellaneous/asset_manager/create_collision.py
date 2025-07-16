import sys
import bpy
from mathutils import Vector

def create_mesh(name, location, dimensions):
    bpy.ops.mesh.primitive_cube_add(location=location)
    collision = bpy.context.object
    collision.name = name
    collision.dimensions = dimensions
    collision.display_type = 'BOUNDS'
    collision.display_bounds_type = 'BOX' # BOX, CYLINDER
    
def delete_mesh(collection, name):
    if name in collection.objects.keys():
        bpy.context.collection.objects[name].select_set(True)
        bpy.ops.object.delete()    
    
def create_collision(collection):
    delete_mesh(collection, 'COLLISION')    
    m = sys.float_info.min
    M = sys.float_info.max
    pos_min = Vector((M,M,M))
    pos_max = Vector((m,m,m))
    meshes = [obj for obj in collection.objects if obj.type == 'MESH']
    for mesh in meshes:
        for v in mesh.bound_box:
            pos = mesh.matrix_world @ Vector(v)
            pos_min = Vector([min(z) for z in zip(pos, pos_min)])
            pos_max = Vector([max(z) for z in zip(pos, pos_max)])
    
    location = (pos_min + pos_max) * 0.5
    dimensions = pos_max - pos_min
    create_mesh('COLLISION', location, dimensions)
        
if bpy.context.collection:
    create_collision(bpy.context.collection)