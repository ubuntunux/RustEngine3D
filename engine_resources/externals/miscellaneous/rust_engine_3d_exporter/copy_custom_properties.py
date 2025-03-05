import bpy

selected_objects = bpy.context.selected_objects
if 2 == len(selected_objects):
    src = selected_objects[0] # selected first
    dst = selected_objects[1] # selected second

    for key in dst.keys():
        if key not in dst.bl_rna.properties and key in src:
            dst[key] = src[key]