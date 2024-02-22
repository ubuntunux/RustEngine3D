import json

import bpy
import bpy_extras


def save(operator, context, filepath='', **keywords):
    if filepath == '':
        return {'FINISHED'}

    context.window.cursor_set('WAIT')

    scene = context.scene
    objects = scene.objects
    mesh_objects = [ob for ob in objects if ob.type == 'MESH']
    empty_objects = [ob for ob in objects if ob.type == 'EMPTY']
    light_objects = [ob for ob in objects if ob.type == 'LIGHT']
    camera_objects = [ob for ob in objects if ob.type == 'CAMERA']

    context.window.cursor_set('DEFAULT')

    game_scene_data = {
        "_scene_data_name": "intro_stage",
        "_player": {
            "player": {"_character_data_name": "jack", "_position": [0.0, 1.0, 0.0], "_rotation": [0.0, 0.0, 0.0],
                       "_scale": [0.25, 0.25, 0.25]}
        },
        "_characters": {
            "enemy00": {"_character_data_name": "pickle", "_position": [2.0, 1.0, 0.0], "_rotation": [0.0, 0.0, 0.0],
                        "_scale": [1.0, 1.0, 1.0]},
            "enemy01": {"_character_data_name": "pickle", "_position": [-2.0, 1.0, 0.0], "_rotation": [0.0, 0.0, 0.0],
                        "_scale": [1.0, 1.0, 1.0]},
            "enemy02": {"_character_data_name": "pickle", "_position": [0.0, 1.0, 0.0], "_rotation": [0.0, 0.0, 0.0],
                        "_scale": [1.0, 1.0, 1.0]}
        },
        "_blocks": {
            "rock00": {"_block_data_name": "cliff_grass", "_position": [2.0, 10.0, 0.0], "_rotation": [0.0, 0.0, 0.0],
                       "_scale": [1.0, 1.0, 1.0]},
            "rock01": {"_block_data_name": "cliff_grass", "_position": [4.0, 12.0, 0.0], "_rotation": [0.0, 0.0, 0.0],
                       "_scale": [1.0, 1.0, 1.0]}
        },
        "_start_point": [0.0, 1.0, 0.0]
    }

    with open(filepath, 'w') as f:
        f.write(json.dumps(game_scene_data, sort_keys=True, indent=4))

    return {'FINISHED'}
