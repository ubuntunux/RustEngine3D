from enum import Enum
import datetime
import os
import time
import json
import math

import importlib
import logging
importlib.reload(logging)
from logging.handlers import RotatingFileHandler

import bpy
import bpy_extras


def create_logger(logger_name, log_dirname, level):
    # prepare log directory
    if not os.path.exists(log_dirname):
        os.makedirs(log_dirname)
    log_file_basename = datetime.datetime.fromtimestamp(time.time()).strftime(f'{logger_name}_%Y%m%d_%H%M%S.log')
    log_filename = os.path.join(log_dirname, log_file_basename)

    # create logger
    logger = logging.getLogger(log_dirname)
    logger.setLevel(level=level)

    # add handler
    stream_handler = logging.StreamHandler()
    file_max_byte = 1024 * 1024 * 100 #100MB
    backup_count = 10
    file_handler = logging.handlers.RotatingFileHandler(log_filename, maxBytes=file_max_byte, backupCount=backup_count)

    logger.addHandler(stream_handler)
    logger.addHandler(file_handler)

    # set formatter
    formatter = logging.Formatter(fmt='%(asctime)s,%(msecs)03d [%(levelname)s|%(filename)s:%(lineno)d] %(message)s', datefmt='%Y-%m-%d:%H:%M:%S')
    stream_handler.setFormatter(formatter)
    file_handler.setFormatter(formatter)
    return logger


class ResourceTypeInfo:
    def __init__(self, resource_dirname, resource_ext, external_ext):
        self.resource_dirname = resource_dirname
        self.resource_ext = resource_ext
        self.external_ext = external_ext


class ResourceType(Enum):
    MATERIAL_INSTANCE = ResourceTypeInfo('material_instances', '.matinst', None)
    MODEL = ResourceTypeInfo('models', '.model', None)
    MESH = ResourceTypeInfo('meshes', '.mesh', '.gltf')


class AssetInfo:
    def __init__(self, asset):
        self.asset_name = asset.name

        tokens = asset.asset_data.catalog_simple_name.split('-')
        self.catalog_simple_name = asset.asset_data.catalog_simple_name
        self.asset_library_path = '/'.join(tokens)
        self.asset_library_name = tokens[0]
        self.asset_type_name = tokens[1]
        self.asset_relative_path = '/'.join(tokens[1:])
        self.asset_namepath = os.path.join('/'.join(tokens[2:]), self.asset_name)
        self.asset_fullpath = os.path.join(self.asset_relative_path, self.asset_name)

    def __str__(self):
        return str(self.__dict__)

    def get_asset_filepath(self, resource_path, ext):
        return os.path.join(resource_path, self.asset_relative_path, self.asset_name) + ext


class RustEngine3DExporter:
    def __init__(self, library_name):
        self.library_name = library_name
        self.asset_library = bpy.context.preferences.filepaths.asset_libraries.get(library_name)
        self.external_path = self.asset_library.path
        self.resource_path = os.path.split(self.external_path)[0]
        log_dirname = '.'
        if self.asset_library:
            log_dirname = os.path.join(self.asset_library.path, '.log')
        self.logger = create_logger(logger_name=library_name, log_dirname=log_dirname, level=logging.DEBUG)
        self.logger.info(f'>>> Begin Export Library: {library_name}')
    
    def convert_asset_location(self, asset):
        location = list(asset.location)
        return [location[0], location[2], location[1]]
    
    def convert_asset_rotation(self, asset, rx=0.0, ry=0.0, rz=0.0):
        rotation_euler = list(asset.rotation_euler)
        return [
            math.radians(rx) - rotation_euler[0], 
            math.radians(rz) - rotation_euler[2], 
            math.radians(ry) - rotation_euler[1]
        ]
    
    def convert_asset_scale(self, asset):
        scale = list(asset.scale)
        return [scale[0], scale[2], scale[1]]
    
    def convert_sun_color(self, asset):
        return [asset.data.energy * x for x in list(asset.data.color)]

    def export_material_instance(self, asset_info, mesh_collection, mesh_data):
        material_instance_namepaths = []
        material_slots_list = []
        for child_object in mesh_data.objects:
            if 'MESH' == child_object.type:
                print(f'child_object.material_slots: {len(child_object.material_slots)}')
                material_slots_list.append(child_object.material_slots)

        for child_object in mesh_collection.objects:
            if 'MESH' == child_object.type:
                texture_dirpath = os.path.join(self.resource_path, 'textures')
                material_slots = material_slots_list.pop(0)
                material_instance_slots = child_object.material_slots
                for material_index in range(len(material_instance_slots)):
                    material = material_slots[material_index].material
                    material_instance = material_instance_slots[material_index].material
                    material_asset_info = AssetInfo(material)
                    material_instance_asset_info = AssetInfo(material_instance)
                    material_instance_namepaths.append(material_instance_asset_info.asset_namepath)
                    
                    # export material instance
                    material_parameters = {}
                    material_instance_info = {
                        'material_name': material_asset_info.asset_namepath,
                        'material_parameters': material_parameters
                    }
                    
                    for node in material_instance.node_tree.nodes:
                        if node.label:
                            if 'TEX_IMAGE' == node.type:
                                image_relative_filepath = node.image.filepath.replace('//', '')
                                image_filepath = os.path.abspath(os.path.join(self.resource_path, asset_info.asset_relative_path, image_relative_filepath))
                                image_namepath = os.path.splitext(os.path.relpath(image_filepath, texture_dirpath))[0]
                                material_parameters[node.label] = image_namepath
                            elif 'RGB' == node.type:
                                material_parameters[node.label] = list(node.outputs[0].default_value)
                     
                    # export material instance
                    export_filepath = material_instance_asset_info.get_asset_filepath(self.resource_path, ".matinst")
                    self.logger.info(f'export_material_instance: {export_filepath}')           
                    with open(export_filepath, 'w') as f:
                        f.write(json.dumps(material_instance_info, sort_keys=True, indent=4))
        return material_instance_namepaths

    def export_selected_meshes(self, asset_info):
        export_filepath = asset_info.get_asset_filepath(self.external_path, '.gltf')
        bpy.ops.export_scene.gltf(
            filepath=export_filepath,
            export_format='GLTF_SEPARATE',
            use_selection=True,
            export_yup=True,
            export_texcoords=True,
            export_normals=True,
            export_tangents=True,
            export_colors=True,
            export_materials='NONE',
            export_skins=True,
            export_animations=True,
            export_force_sampling=True,
            export_bake_animation=True,
            export_optimize_animation_size=False,
            export_optimize_animation_keep_anim_armature=True,
            export_anim_single_armature=False,
            export_reset_pose_bones=True
        )
        self.logger.info(f'export_selected_meshes {asset_info.asset_namepath}: {export_filepath}')

    def export_models(self, asset, asset_info):
        self.logger.info(f'export_models: {asset_info.asset_namepath}')
        
        if 0 < len(asset.children):            
            material_instances = []      
            model_info = {
                "material_instances": material_instances, 
                "mesh": ""
            }
        
            for mesh_collection in asset.children:
                mesh_data = mesh_collection.override_library.reference
                mesh_asset_info = AssetInfo(mesh_data)
                model_info['mesh'] = mesh_asset_info.asset_namepath
                
                # material instance
                material_instance_namepaths = self.export_material_instance(asset_info, mesh_collection, mesh_data)
                material_instances += material_instance_namepaths

            # export model
            export_model_filepath = asset_info.get_asset_filepath(self.resource_path, '.model')
            with open(export_model_filepath, 'w') as f:
                f.write(json.dumps(model_info, sort_keys=True, indent=4))
            self.logger.info(f'export_models: {export_model_filepath}')
    
    def export_scenes(self, asset, asset_info):
        self.logger.info(f'export_scenes: {asset_info.asset_namepath}')
        
        cameras = {}
        directional_lights = {}
        effects = {}
        static_objects = {}
        skeletal_objects = {}
        
        scene_data = {
            "_cameras": cameras,
            "_directional_lights": directional_lights,
            "_effects": effects,
            "_static_objects": static_objects,
            "_skeletal_objects": skeletal_objects
        }
        
        for child in asset.objects:
            if 'LIGHT' == child.type:
                light_rotation = self.convert_asset_rotation(child, rx=90.0)
                directional_lights[child.name] = {
                    "_rotation": light_rotation,
                    "_light_constants": {                    
                        "_light_direction": light_rotation,
                        "_light_color": self.convert_sun_color(child)
                    }
                }
            elif 'CAMERA' == child.type:
                cameras[child.name] = {
                    #'fov': math.degrees(child.data.angle),
                    'position': self.convert_asset_location(child),
                    'rotation': self.convert_asset_rotation(child, rx=90.0)
                }                
            elif 'EMPTY' == child.type and 'COLLECTION' == child.instance_type:
                child_asset_info = AssetInfo(child.instance_collection)
                if 'models' == child_asset_info.asset_type_name:
                    static_objects[child.name] = {
                        "_model_data_name": child_asset_info.asset_namepath,
                        "_position": self.convert_asset_location(child),
                        "_rotation": self.convert_asset_rotation(child),
                        "_scale": self.convert_asset_scale(child)
                    }
                    # TODO - Skeletal Mesh
                else:
                    self.logger.error(f'not implemented asset type {(child.name, child_asset_info.asset_type_name)}')
            else:
                self.logger.error(f'not implemented object type {(child.name, child.type)}')
        
        export_filepath = asset_info.get_asset_filepath(self.resource_path, ".scene")
        self.logger.info(f'export_scenes: {export_filepath}')           
        with open(export_filepath, 'w') as f:
            f.write(json.dumps(scene_data, sort_keys=True, indent=4))
    
    # game data
    def set_game_data_asset_property(self, property_asset, key, game_data):
        if property_asset and key in property_asset:
            child_asset = property_asset.get(key, None)
            if child_asset:
                child_asset_info = AssetInfo(child_asset)
                game_data[key] = child_asset_info.asset_namepath
                
    def get_game_data_block(self, asset, asset_info):
        game_data = {}
        property_asset = asset.objects[0] if asset.objects else None
        if property_asset:
            self.set_game_data_asset_property(property_asset, '_model_data_name', game_data)
            game_data["_block_type"] = property_asset.get("_block_type", "Ground")
            game_data["_max_hp"] = property_asset.get("_max_hp", "100")
        return game_data
    
    def get_game_data_character(self, asset, asset_info):
        game_data = {}
        property_asset = asset.objects[0] if asset.objects else None
        if property_asset:
            self.set_game_data_asset_property(property_asset, '_model_data_name', game_data)
            self.set_game_data_asset_property(property_asset, '_idle_animation_mesh', game_data)
            self.set_game_data_asset_property(property_asset, '_walk_animation_mesh', game_data)
            self.set_game_data_asset_property(property_asset, '_jump_animation_mesh', game_data)
            self.set_game_data_asset_property(property_asset, '_attack_animation_mesh', game_data)
            game_data["_character_type"] = property_asset.get("_block_type", "UrsusArctos")
            game_data["_max_hp"] = property_asset.get("_max_hp", "100")
        return game_data
    
    def get_game_data_scenes(self, asset, asset_info):
        player = {}
        characters = {}
        blocks = {}        
        game_data = {
            "_scene_data_name": "",
            "_player": player,
            "_characters": characters,
            "_blocks": blocks,
            "_start_point": [0, 0, 0]
        }        
        for child_asset in asset.children:
            if '_scene' == child_asset.name:
                for child_object in child_asset.objects:
                    child_object_info = AssetInfo(child_object.instance_collection)
                    game_data['_scene_data_name'] = child_object_info.asset_namepath
            elif '_blocks' == child_asset.name:
                for child_object in child_asset.objects:
                    child_object_info = AssetInfo(child_object.instance_collection)
                    blocks[child_object.name] = {
                        "_block_data_name": '/'.join(child_object_info.asset_namepath.split('/')[1:]),
                        "_position": self.convert_asset_location(child_object),
                        "_rotation": self.convert_asset_rotation(child_object),
                        "_scale": self.convert_asset_scale(child_object)
                    }
            elif '_characters' == child_asset.name:
                for child_object in child_asset.objects:
                    child_object_info = AssetInfo(child_object.instance_collection)
                    characters[child_object.name] = {
                        "_character_data_name": '/'.join(child_object_info.asset_namepath.split('/')[1:]),
                        "_position": self.convert_asset_location(child_object),
                        "_rotation": self.convert_asset_rotation(child_object),
                        "_scale": self.convert_asset_scale(child_object)
                    }
            elif '_player' == child_asset.name:
                for child_object in child_asset.objects:
                    child_object_info = AssetInfo(child_object.instance_collection)
                    player[child_object.name] = {
                        "_character_data_name": '/'.join(child_object_info.asset_namepath.split('/')[1:]),
                        "_position": self.convert_asset_location(child_object),
                        "_rotation": self.convert_asset_rotation(child_object),
                        "_scale": self.convert_asset_scale(child_object)
                    }
            elif '_start_point' == child_asset.name:
                for child_object in child_asset.objects:
                    game_data['_start_point'] = self.convert_asset_location(child_object)
            else:
                self.logger.error(f'not implemented object type {child_asset.name}')
        return game_data
        
        
    def export_game_data(self, asset, asset_info):
        self.logger.info(f'export_game_data: {asset_info.asset_namepath}')
        tokens = asset_info.asset_library_path.split('/')
        if 2 < len(tokens):
            game_data = {}
            game_data_ext = '.data'
            game_data_type = tokens[2]
            if 'blocks' == game_data_type:
                game_data = self.get_game_data_block(asset, asset_info)
            elif 'characters' == game_data_type:
                game_data = self.get_game_data_character(asset, asset_info)
            elif 'game_scenes':
                game_data = self.get_game_data_scenes(asset, asset_info)
            else:
                self.logger.error(f'not implemented game data: {asset_info.asset_fullpath}')
                
            if game_data:
                export_filepath = asset_info.get_asset_filepath(self.resource_path, game_data_ext)
                self.logger.info(f'export_game_data: {export_filepath}')
                with open(export_filepath, 'w') as f:
                    f.write(json.dumps(game_data, sort_keys=True, indent=4))
                return
        self.logger.error(f'error export_game_data: {asset_info.asset_fullpath}')
            

    # export asset
    def export_asset(self, asset):
        asset_info = AssetInfo(asset)
        self.logger.info(f'export_object: {asset_info.asset_fullpath}')

        if 'meshes' == asset_info.asset_type_name:
            self.export_selected_meshes(asset_info)
        elif 'models' == asset_info.asset_type_name:
            self.export_models(asset, asset_info)
        elif 'scenes' == asset_info.asset_type_name:
            self.export_scenes(asset, asset_info)
        elif 'game_data' == asset_info.asset_type_name:
            self.export_game_data(asset, asset_info)
        else:
            self.logger.error(f'error export_asset: {asset_info.asset_type_name}')

    def export_selected_objects(self):
        bpy.ops.outliner.orphans_purge(do_local_ids=True, do_linked_ids=True, do_recursive=True)
        self.logger.info(f"export_selected_objects: {bpy.context.selected_objects}")
        for asset in bpy.context.selected_objects:
            if 'EMPTY' == asset.type and 'COLLECTION' == asset.instance_type:
                self.export_asset(asset.instance_collection)
            else:
                self.logger.error(f'error export_selected_objects: {asset.type}')

    def load_blend_file(self, blend_file):
        if not os.path.exists(blend_file):
            return None
        
        with bpy.data.libraries.load(blend_file, assets_only=True, link=True) as (data_from, data_to):
            data_to.materials = data_from.materials
            data_to.meshes = data_from.meshes
            data_to.collections = data_from.collections
            data_to.actions = data_from.actions
            data_to.armatures = data_from.armatures
            data_to.objects = data_from.objects
            self.logger.info(f'collections: {len(data_to.collections)}')
            self.logger.info(f'meshes: {len(data_to.meshes)}')
            self.logger.info(f'objects: {len(data_to.objects)}')
            return data_to
    
    def export_library_asset(self, asset, asset_data):        
        bpy.context.scene.collection.objects.link(asset)
                
        # select collection
        bpy.ops.object.select_all(action='DESELECT')
        asset.select_set(True)

        # export asset data
        self.export_asset(asset_data)

        # remove collection
        bpy.context.scene.collection.objects.unlink(asset)
        bpy.data.objects.remove(asset)

        # clean-up recursive unused data-blocks
        bpy.ops.outliner.orphans_purge(do_local_ids=True, do_linked_ids=True, do_recursive=True)

    def export_blend(self, blend_file):
        self.logger.info(f"export_blend: {blend_file}")
        data = self.load_blend_file(blend_file)
        if data:
            for (i, collection) in enumerate(data.collections):
                # create collection
                empty = bpy.data.objects.new(collection.name, None)
                empty.instance_type = 'COLLECTION'
                empty.instance_collection = collection
                # export
                self.export_library_asset(empty, collection)
                

    def export_resources(self):
        self.logger.info(f'>>> export_resource: {self.asset_library.path}')
        for dirpath, dirnames, filenames in os.walk(self.asset_library.path):
            for filename in filenames:
                if '.blend' == os.path.splitext(filename)[1].lower():
                    self.export_blend(os.path.join(dirpath, filename))

    def done(self):
        self.logger.info('>>> Done.\n')


def run_export_resources():
    bpy.context.window.cursor_set('WAIT')

    exporter = RustEngine3DExporter('StoneAge')

    exporter.export_selected_objects()
    #exporter.export_blend('/home/ubuntunux/WorkSpace/StoneAge/resources/externals/models/environments/cactus.blend')
    #exporter.export_blend('/home/ubuntunux/WorkSpace/StoneAge/resources/externals/meshes/environments/cliff_grass.blend')
    #exporter.export_blend('/home/ubuntunux/WorkSpace/StoneAge/resources/externals/meshes/characters/jack/jack.blend')
    #exporter.export_blend('/home/ubuntunux/WorkSpace/StoneAge/resources/externals/models/characters/jack.blend')
    #exporter.export_resources()
    exporter.done()

    # scene = context.scene
    # objects = scene.objects
    # mesh_objects = [ob for ob in objects if ob.type == 'MESH']
    #
    # for mesh in mesh_objects:
    #     for material in mesh.data.materials:
    #         catalog = material.asset_data.catalog_simple_name.replace('-', '/')
    #         relative_filepath = os.path.join(catalog, material.name)
    #         print(relative_filepath)
    #
    #
    # material_instance_data = {
    #     "material_name": "common/render_static_object",
    #     "material_parameters": {
    #         "textureBase": "environments/desert_ground",
    #         "textureMaterial": "common/default_m",
    #         "textureNormal": "common/default_n"
    #     }
    # }
    #
    # model_data = {
    #     "material_instances": [
    #         "environments/cactus"
    #     ],
    #     "mesh": "environments/cactus"
    # }


    #    with open(resource_info.resource_filepath, 'w') as f:
    #        f.write(json.dumps(game_scene_data, sort_keys=True, indent=4))

    bpy.context.window.cursor_set('DEFAULT')
    return {'FINISHED'}


run_export_resources()

''' 
# model
>>> cactus = bpy.context.selected_objects[0]
>>> cactus.instance_collection.library.filepath
'//../models/environments/cactus.blend'

>>> cactus.instance_collection.id_data.asset_data.catalog_simple_name
'StoneAge-models-environments'


# mesh
>>> mesh = cactus.instance_collection.objects[0]
>>> mesh.data.library.filepath
'//../meshes/environments/cactus.blend'



# material
>>> material = mesh.data.materials[0]
>>> material.library.filepath
'//../materials/common/render_static_object.blend'

>>> material.asset_data.catalog_simple_name
'materials-common'


# material instance
>>> material_instance = mesh.material_slots[0].material
>>> material_instance.library.filepath
'//../models/environments/desert_ground.blend'

>>> material_instance.asset_data.catalog_simple_name
'StoneAge-material_instances-environments'

'''