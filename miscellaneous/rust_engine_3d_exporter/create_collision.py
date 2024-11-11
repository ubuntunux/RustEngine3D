import bpy
from mathutils import Vector

def create_collision(obj, bounds_type):
    center = Vector((0,0,0))
    for v in obj.bound_box:
        center += obj.matrix_world @ Vector(v)
    center /= 8
    scale = obj.dimensions / 2
    bpy.ops.mesh.primitive_cube_add(location=center)
    collision = bpy.context.object
    collision.name = 'COLLISION'
    collision.dimensions = obj.dimensions 
    collision.display_type = 'BOUNDS'
    collision.display_bounds_type = bounds_type
            
if bpy.context.object:
    create_collision(bpy.context.object, 'BOX')