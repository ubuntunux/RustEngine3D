import bpy

def create_animation_blend_mask(armature):
    if armature.type != 'ARMATURE':
        return
    
    bpy.ops.object.constraint_add(type='ARMATURE')
    for bone in armature.data.bones:
        target = armature.constraints["Armature"].targets.new()
        target.target = armature.override_library.reference
        target.subtarget = bone.name
        target.weight = 1.0
        
        


create_animation_blend_mask(bpy.context.object)