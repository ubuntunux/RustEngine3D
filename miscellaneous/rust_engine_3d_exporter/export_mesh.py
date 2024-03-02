from enum import Enum
import datetime
import os
import time
import json

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

    def export_material_instance(self, asset_info, mesh_collection, mesh_data):
        material_instance_namepaths = []
        material_slots_list = []
        for child_object in mesh_data.objects:
            if 'MESH' == child_object.type:
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

    def export_meshes(self, collection, asset_info):
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
            export_optimize_animation_size=False,
            export_bake_animation=True,
            export_optimize_animation_keep_anim_armature=True
        )
        self.logger.info(f'export_meshes {collection.name}: {export_filepath}')

    def export_models(self, collection, asset_info):
        if 0 < len(collection.children):
            material_instances = []      
            model_info = {
                "material_instances": material_instances, 
                "mesh": ""
            }
            
            for mesh_collection in collection.children:
                mesh_data = mesh_collection.override_library.reference
                mesh_asset_info = AssetInfo(mesh_data)
                model_info['mesh'] = mesh_asset_info.asset_namepath
                
                # material instance
                material_instance_namepaths = self.export_material_instance(asset_info, mesh_collection, mesh_data)
                material_instances += material_instance_namepaths
            
            # export model
            export_model_filepath = asset_info.get_asset_filepath(self.resource_path, '.model')
            self.logger.info(f'export_models: {export_model_filepath}')           
            with open(export_model_filepath, 'w') as f:
                f.write(json.dumps(model_info, sort_keys=True, indent=4))

    def export_object(self, object):
        asset_info = AssetInfo(object)
        self.logger.info(f'export_object: {asset_info}')

        if 'meshes' == asset_info.asset_type_name:
            self.export_meshes(object, asset_info)
        elif 'models' == asset_info.asset_type_name:
            self.export_models(object, asset_info)
        else:
            self.logger.error(f'asset_info.asset_type_name')

    def export_selected_objects(self):
        self.logger.info(f"export_selected_objects: {bpy.context.selected_objects}")
        for asset in bpy.context.selected_objects:
            if 'EMPTY' == asset.type and 'COLLECTION' == asset.instance_type:
                self.export_object(asset.instance_collection)
            elif 'MESH' == asset.type:
                self.export_object(asset)

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

        # export collection
        self.export_object(asset_data)

        # remove collection
        bpy.context.scene.collection.objects.unlink(asset)
        bpy.data.objects.remove(asset)

        # clean-up recursive unused data-blocks
        bpy.ops.outliner.orphans_purge(do_local_ids=True, do_linked_ids=True, do_recursive=True)

    def export_blend(self, blend_file):
        self.logger.info(f"export_blend: {blend_file}")
        data = self.load_blend_file(blend_file)
        if data:
            # export collections
            for (i, collection) in enumerate(data.collections):
                # create collection
                empty = bpy.data.objects.new(collection.name, None)
                empty.instance_type = 'COLLECTION'
                empty.instance_collection = collection
                self.export_library_asset(empty, collection)
            
            # export objects
            for (i, object) in enumerate(data.objects):
                self.export_library_asset(object, object)
                

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
