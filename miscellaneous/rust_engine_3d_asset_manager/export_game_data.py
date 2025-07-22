import bpy
import bpy_extras
import os
import json
import math
import shutil
import sys
import traceback

from collections import OrderedDict
from enum import Enum
from mathutils import Vector

def get_bound(collection):    
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
    return (pos_min, pos_max)


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


class AssetExportManager:
    def __init__(self, __logger__, library_name):
        global logger
        logger = __logger__

        self.library_name = library_name
        self.asset_library = bpy.context.preferences.filepaths.asset_libraries.get(library_name)
        self.external_path = os.path.normpath(self.asset_library.path)
        self.resource_path = os.path.split(self.external_path)[0]

    def convert_axis(self, axis):
        return [axis[0], axis[2], axis[1]]

    def convert_asset_location(self, asset):
        return self.convert_axis(list(asset.location))

    def convert_asset_rotation(self, asset, rx=0.0, ry=0.0, rz=0.0):
        rotation_euler = list(asset.rotation_euler)
        rotation_euler[0] = math.radians(rx) - rotation_euler[0]
        rotation_euler[1] = math.radians(ry) - rotation_euler[1]
        rotation_euler[2] = math.radians(rz) - rotation_euler[2]
        return self.convert_axis(rotation_euler)

    def convert_asset_scale(self, asset):
        return self.convert_axis(list(asset.scale))

    def convert_asset_dimensions(self, asset):
        return self.convert_axis(list(asset.dimensions))

    def convert_light_color(self, asset):
        return [asset.data.energy * x for x in list(asset.data.color)]

    def get_object_center(self, obj):
        center = Vector((0,0,0))
        for v in obj.bound_box:
            center += obj.matrix_world @ Vector(v)
        return center / 8

    def copy_file(self, title, src_filepath, dst_filepath):
        logger.info(f'{title}: {dst_filepath}')
        try:
            dst_dirpath = os.path.split(dst_filepath)[0]
            if not os.path.exists(dst_dirpath):
                os.makedirs(dst_dirpath)
            shutil.copy(src_filepath, dst_filepath)
        except:
            logger.error(traceback.format_exc())
            raise

    def write_to_file(self, title, data, export_filepath):
        logger.info(f'{title}: {export_filepath}')
        export_path = os.path.split(export_filepath)[0]
        if not os.path.exists(export_path):
            os.makedirs(export_path)

        with open(export_filepath, 'w') as f:
            f.write(json.dumps(data, sort_keys=True, indent=4))

    def export_animation_layers(self, asset, asset_info):
        bone_blend_map = OrderedDict()

        for child in asset.children:
            for child_object in [x for x in child.objects if 'ARMATURE' == x.type]:
                for constraint in [x for x in child_object.constraints if 'ARMATURE' == x.type]:
                    for target in constraint.targets:
                        bone_blend_map[target.subtarget] = target.weight

        animation_layers = OrderedDict({
            "_bone_blend_map": bone_blend_map
        })

        # export animation_layers
        export_filepath = asset_info.get_asset_filepath(self.resource_path, '.layer')
        self.write_to_file('export animation_layers', animation_layers, export_filepath)

    def export_material_instance(self, asset_info, mesh_collection):
        texture_dirpath = os.path.join(self.resource_path, 'textures')
        material_instance_name_paths = []
        materials_list = [child.data.materials for child in mesh_collection.objects if 'MESH' == child.type]
        material_instance_slots_list = [child.material_slots for child in mesh_collection.objects if 'MESH' == child.type]
        for list_index in range(len(materials_list)):
            materials = materials_list[list_index]
            material_instance_slots = material_instance_slots_list[list_index]
            for material_index in range(len(materials)):
                material = materials[material_index]
                material_instance = material_instance_slots[material_index].material

                material_asset_info = AssetInfo(material)
                material_instance_asset_info = AssetInfo(material_instance)
                material_instance_name_paths.append(material_instance_asset_info.asset_namepath)

                # export material instance
                material_parameters = OrderedDict()
                material_instance_info = OrderedDict({
                    'material_name': material_asset_info.asset_namepath,
                    'material_parameters': material_parameters
                })

                for node in material_instance.node_tree.nodes:
                    if node.label:
                        if 'TEX_IMAGE' == node.type:
                            # gather texture parameter
                            image_filepath = node.image.filepath.replace('\\', '/')
                            tokens = image_filepath.split('/')
                            textures_index = tokens.index('textures')
                            image_relative_filepath = os.path.join(*tokens[textures_index:])
                            image_filepath = os.path.abspath(os.path.join(self.resource_path, image_relative_filepath))
                            image_namepath = os.path.splitext(os.path.relpath(image_filepath, texture_dirpath))[0]
                            material_parameters[node.label] = image_namepath

                            # export texture
                            EXPORT_TEXTURES = False
                            if EXPORT_TEXTURES:
                                image_external_filepath = os.path.abspath(os.path.join(self.external_path, ))
                                if os.path.exists(image_external_filepath):
                                    src_modified_time = os.path.getmtime(image_external_filepath)
                                    dst_modified_time = os.path.getmtime(image_filepath) if os.path.exists(image_filepath) else 0
                                    if dst_modified_time < src_modifed_time:
                                        self.copy_file('export texture', image_external_filepath, image_filepath)
                        elif 'RGB' == node.type:
                            material_parameters[node.label] = list(node.outputs[0].default_value)
                        elif 'VALUE' == node.type:
                            material_parameters[node.label] = node.outputs[0].default_value

                # export material instance
                export_filepath = material_instance_asset_info.get_asset_filepath(self.resource_path, ".matinst")
                self.write_to_file('export material_instance', material_instance_info, export_filepath)
        return material_instance_name_paths

    def export_selected_meshes(self, asset_info):
        export_filepath = asset_info.get_asset_filepath(self.resource_path, '.gltf')

        try:
            export_dirpath = os.path.split(export_filepath)[0]
            if not os.path.exists(export_dirpath):
                os.makedirs(export_dirpath)

            bpy.ops.export_scene.gltf(
                filepath=export_filepath,
                export_format='GLTF_SEPARATE',
                use_selection=True,
                export_yup=True,
                export_texcoords=True,
                export_normals=True,
                export_tangents=True,
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
            logger.info(f'export_selected_meshes {asset_info.asset_namepath}: {export_filepath}')
        except:
            logger.error(traceback.format_exc())
            raise

    def export_models(self, asset, asset_info):
        logger.info(f'export_models: {asset_info.asset_namepath}')

        if 0 < len(asset.children):
            mesh_collection = asset.children[0]

            # mesh
            mesh_data = mesh_collection.override_library.reference
            mesh_asset_info = AssetInfo(mesh_data)
            mesh_path = mesh_asset_info.asset_namepath
            is_render_camera = True
            is_render_shadow = True

            # material instance
            material_instances = self.export_material_instance(asset_info, mesh_collection)

            # local transform object
            position = [0.0, 0.0, 0.0]
            rotation = [0.0, 0.0, 0.0]
            scale = [1.0, 1.0, 1.0]

            # default bound
            (default_pos_min, default_pos_max) = get_bound(mesh_collection)

            # bounding box
            bounding_box = OrderedDict({
                "_min": self.convert_axis(default_pos_min),
                "_max": self.convert_axis(default_pos_max)
            })

            # collision
            collision = OrderedDict({
                "_collision_type": "NONE",
                "_location": [0, 0, 0],
                "_extents": [0.5, 0.5, 0.5]
            })

            # sockets
            sockets = OrderedDict()

            # extra objects
            for obj in asset.objects:
                if obj.name.startswith('COLLISION'):
                    pos_min = obj.location - obj.dimensions * 0.5
                    pos_max = obj.location + obj.dimensions * 0.5
                    location = (pos_max + pos_min) * 0.5
                    collision['_collision_type'] = obj.display_bounds_type if obj.display_bounds_type in ['CYLINDER', 'SPHERE'] else 'BOX'
                    collision['_location'] = self.convert_axis(location)
                    collision['_extents'] = self.convert_axis(obj.dimensions * 0.5)
                elif obj.name.startswith('BOUND_BOX'):
                    pos_min = obj.location - obj.dimensions * 0.5
                    pos_max = obj.location + obj.dimensions * 0.5
                    bounding_box['_min'] = self.convert_axis(pos_min)
                    bounding_box['_max'] = self.convert_axis(pos_max)
                elif obj.name.startswith('SOCKET_'):
                    if obj.parent and obj.parent_type == 'BONE':
                        sockets[obj.name] = OrderedDict({
                            '_parent_bone': obj.parent_bone,
                            '_position': self.convert_asset_location(obj),
                            '_rotation': self.convert_asset_rotation(obj),
                            '_scale': self.convert_asset_scale(obj)
                        })

            # model transform
            for (model_obj, mesh_obj) in zip(mesh_collection.objects, mesh_data.objects):
                if mesh_obj.parent is None:
                    mesh_location = self.convert_asset_location(mesh_obj)
                    mesh_rotation = self.convert_asset_rotation(mesh_obj)
                    mesh_scale = self.convert_asset_scale(mesh_obj)
                    model_location = self.convert_asset_location(model_obj)
                    model_rotation = self.convert_asset_rotation(model_obj)
                    model_scale = self.convert_asset_scale(model_obj)
                    position = [model_location[0] - mesh_location[0], model_location[1] - mesh_location[1], model_location[2] - mesh_location[2]]
                    rotation = [model_rotation[0] - mesh_rotation[0], model_rotation[1] - mesh_rotation[1], model_rotation[2] - mesh_rotation[2]]
                    scale = [model_scale[0] / mesh_scale[0], model_scale[1] / mesh_scale[1], model_scale[2] / mesh_scale[2]]
                    is_render_camera = model_obj.visible_camera
                    is_render_shadow = model_obj.visible_shadow
                    break

            # export model
            model_info = OrderedDict({
                "_mesh": mesh_path,
                "_position": position,
                "_rotation": rotation,
                "_scale": scale,
                "_is_render_camera": is_render_camera,
                "_is_render_shadow": is_render_shadow,
                "_material_instances": material_instances,
                "_collision": collision,
                "_sockets": sockets
            })
            export_model_filepath = asset_info.get_asset_filepath(self.resource_path, '.model')
            self.write_to_file('export model', model_info, export_model_filepath)

    def get_scene_data(self, asset):
        cameras = OrderedDict()
        directional_lights = OrderedDict()
        point_lights = OrderedDict()
        effects = OrderedDict()
        static_objects = OrderedDict()
        skeletal_objects = OrderedDict()

        scene_data = OrderedDict({
            "_cameras": cameras,
            "_directional_lights": directional_lights,
            "_point_lights": point_lights,
            "_effects": effects,
            "_static_objects": static_objects,
            "_skeletal_objects": skeletal_objects
        })

        for child in asset.objects:
            if 'LIGHT' == child.type:
                light_color = self.convert_light_color(child)
                light_rotation = self.convert_asset_rotation(child, rx=90.0)

                if 'SUN' == child.data.type:
                    directional_lights[child.name] = OrderedDict({
                        "_rotation": light_rotation,
                        "_light_data": OrderedDict({
                            "_light_direction": light_rotation,
                            "_light_color": light_color,
                        })
                    })
                elif 'POINT' == child.data.type:
                    point_lights[child.name] = OrderedDict({
                        "_light_position": self.convert_asset_location(child),
                        "_radius": child.data.shadow_soft_size,
                        "_light_color": light_color
                    })
            elif 'CAMERA' == child.type:
                cameras[child.name] = OrderedDict({
                    # 'fov': math.degrees(child.data.angle),
                    'position': self.convert_asset_location(child),
                    'rotation': self.convert_asset_rotation(child, rx=90.0)
                })
            elif 'EMPTY' == child.type and 'COLLECTION' == child.instance_type:
                logger.info(f'child: {child.name, child.instance_collection.asset_data is None}')
                child_asset_info = AssetInfo(child.instance_collection)
                if 'models' == child_asset_info.asset_type_name:
                    static_objects[child.name] = OrderedDict({
                        "_model_data_name": child_asset_info.asset_namepath,
                        "_position": self.convert_asset_location(child),
                        "_rotation": self.convert_asset_rotation(child),
                        "_scale": self.convert_asset_scale(child)
                    })
                    # TODO - Skeletal Mesh
                else:
                    logger.error(f'not implemented asset type {(child.name, child_asset_info.asset_type_name)}')
            else:
                logger.error(f'not implemented object type {(child.name, child.type)}')
        return scene_data

    def export_scenes(self, asset, asset_info):
        logger.info(f'export_scenes: {asset_info.asset_namepath}')
        scene_data = self.get_scene_data(asset)
        export_filepath = asset_info.get_asset_filepath(self.resource_path, ".scene")
        self.write_to_file('export scene', scene_data, export_filepath)

    def asset_property_to_game_data(self, property_asset, game_data):
        for key in property_asset.keys():
            if key not in property_asset.bl_rna.properties:
                property_value = property_asset[key]
                property_type = type(property_value)
                if property_type in [bool, int, float, str]:
                     game_data[key] = property_value
                elif property_type is bpy.types.Collection:
                    if property_value:
                        property_value_asset_info = AssetInfo(property_value)
                        game_data[key] = property_value_asset_info.asset_namepath
                else:
                    logger.error(f'get_game_data_character not implemented type: {key, property_type}')

        for child_property_asset in property_asset.children:
             child_game_data = OrderedDict()
             self.asset_property_to_game_data(child_property_asset, child_game_data)
             game_data["_" + child_property_asset.name] = child_game_data

    def get_custom_properties(self, asset, asset_info, property_asset_name):
        game_data = OrderedDict()
        for property_asset in asset.objects:
            if property_asset.name == property_asset_name:
                self.asset_property_to_game_data(property_asset, game_data)
        return game_data

    def export_game_data(self, asset, asset_info):
        logger.info(f'export_game_data: {asset_info.asset_namepath}')
        logger.info(f'library_name: {self.library_name}, external_path: {self.external_path}, resource_path: {self.resource_path}')
        
        tokens = asset_info.asset_library_path.split('/')
        if 'game_data' == asset_info.asset_type_name and 2 < len(tokens):
            game_data = OrderedDict()
            game_data_ext = '.data'
            game_data_type = tokens[2]
            if 'characters' == game_data_type:
                game_data = self.get_custom_properties(asset, asset_info, 'character_data')
                for child_object in asset.objects:
                    if 'WEAPON' == child_object.name and child_object.parent:
                        child_asset = child_object.instance_collection
                        weapon_asset_info = AssetInfo(child_asset)
                        game_data["_weapon_create_info"] = OrderedDict({
                            "_weapon_socket_name": child_object.parent.name,
                            "_weapon_data_name": weapon_asset_info.asset_namepath,
                            "_position": self.convert_asset_location(child_object),
                            "_rotation": self.convert_asset_rotation(child_object),
                            "_scale": self.convert_asset_scale(child_object)
                        })
            elif 'items' == game_data_type:
                game_data = self.get_custom_properties(asset, asset_info, 'item_data')
            elif 'props' == game_data_type:
                game_data = self.get_custom_properties(asset, asset_info, 'prop_data')
            elif 'weapons' == game_data_type:
                game_data = self.get_custom_properties(asset, asset_info, 'weapon_data')
            elif 'game_scenes':
                game_data = self.get_game_data_scenes(asset, asset_info)
            else:
                logger.error(f'not implemented game data: {asset_info.asset_fullpath}')
                
            if game_data:
                export_filepath = asset_info.get_asset_filepath(self.resource_path, game_data_ext)
                self.write_to_file('export game_data', game_data, export_filepath)
                return
        logger.error(f'error export_game_data: {asset_info.asset_fullpath}')

    # export game asset
    def get_game_asset_data(self, asset_container, child_asset, asset_data_name):
        for child_object in child_asset.objects:
            child_object_info = AssetInfo(child_object.instance_collection)
            asset_container[child_object.name] = OrderedDict({
                asset_data_name: child_object_info.asset_namepath,
                "_position": self.convert_asset_location(child_object),
                "_rotation": self.convert_asset_rotation(child_object),
                "_scale": self.convert_asset_scale(child_object)
            })

    # export game scene
    def get_game_data_scenes(self, asset, asset_info):
        blocks = OrderedDict()
        characters = OrderedDict()
        items = OrderedDict()
        player = OrderedDict()
        props = OrderedDict()
        terrain = OrderedDict()
        game_data = OrderedDict({
            "_characters": characters,
            "_items": items,
            "_player": player,
            "_props": props,
            "_scene": "",
            "_start_point": [0, 0, 0],
            "_terrain": terrain
        })

        for child_asset in asset.children:
            if '_characters' == child_asset.name:
                self.get_game_asset_data(characters, child_asset, "_character_data_name")
            elif '_items' == child_asset.name:
                self.get_game_asset_data(items, child_asset, "_item_data_name")
            elif '_props' == child_asset.name:
                self.get_game_asset_data(props, child_asset, "_prop_data_name")
            elif '_player' == child_asset.name:
                self.get_game_asset_data(player, child_asset, "_character_data_name")
            elif '_scene' == child_asset.name:
                game_data['_scene'] = self.get_scene_data(child_asset)
            elif '_terrain' == child_asset.name:
                self.get_game_asset_data(terrain, child_asset, "_model_data_name")
            elif '_start_point' == child_asset.name:
                for child_object in child_asset.objects:
                    game_data['_start_point'] = self.convert_asset_location(child_object)
            else:
                logger.error(f'not implemented object type {child_asset.name}')
        return game_data

    # export asset
    def export_asset(self, asset):
        asset_info = AssetInfo(asset)
        logger.info(f'export_asset: {asset_info.asset_fullpath}')

        if 'animation_layers' == asset_info.asset_type_name:
            self.export_animation_layers(asset, asset_info)
        elif 'meshes' == asset_info.asset_type_name:
            self.export_selected_meshes(asset_info)
        elif 'models' == asset_info.asset_type_name:
            self.export_models(asset, asset_info)
        elif 'scenes' == asset_info.asset_type_name:
            self.export_scenes(asset, asset_info)
        elif 'game_data' == asset_info.asset_type_name:
            self.export_game_data(asset, asset_info)
        else:
            logger.error(f'error export_asset: {asset_info.asset_type_name}')

    def export_assets(self):
        bpy.ops.outliner.orphans_purge(do_local_ids=True, do_linked_ids=True, do_recursive=True)
        bpy.ops.object.select_all(action='SELECT')
        selected_objects = bpy.context.selected_objects
        logger.info(f">>> export_assets: {selected_objects}")
        for asset in selected_objects:
            bpy.ops.object.select_all(action='DESELECT')
            asset.select_set(True)
            asset.location = [0, 0, 0]
            if 'EMPTY' == asset.type and 'COLLECTION' == asset.instance_type:
                self.export_asset(asset.instance_collection)
            else:
                logger.error(f'error export_selected_objects: {asset.type}')
        logger.info(f'>>> End: export_assets')

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
            logger.info(f'collections: {len(data_to.collections)}')
            logger.info(f'meshes: {len(data_to.meshes)}')
            logger.info(f'objects: {len(data_to.objects)}')
            return data_to
    
    def export_library_asset(self, asset, asset_data):        
        bpy.context.scene.collection.objects.link(asset)
                
        # select collection
        bpy.ops.object.select_all(action='DESELECT')
        asset.select_set(True)

        # export asset data
        self.export_asset(asset_data)

        # remove a collection
        bpy.context.scene.collection.objects.unlink(asset)
        bpy.data.objects.remove(asset)

        # clean-up recursive unused data-blocks
        bpy.ops.outliner.orphans_purge(do_local_ids=True, do_linked_ids=True, do_recursive=True)

    def export_blend(self, blend_file):
        logger.info(f"export_blend: {blend_file}")
        data = self.load_blend_file(blend_file)
        if data:
            for (i, collection) in enumerate(data.collections):
                # create a collection
                empty = bpy.data.objects.new(collection.name, None)
                empty.instance_type = 'COLLECTION'
                empty.instance_collection = collection
                # export
                self.export_library_asset(empty, collection)

    def export_resources(self):
        logger.info(f'>>> export_resource: {self.asset_library.path}')
        for dirpath, dirnames, filenames in os.walk(self.asset_library.path):
            for filename in filenames:
                if '.blend' == os.path.splitext(filename)[1].lower():
                    self.export_blend(os.path.join(dirpath, filename))

def run_export_resources():
    exporter = AssetExportManager('StoneAge')
    exporter.export_assets()
    #exporter.export_blend('/home/ubuntunux/WorkSpace/StoneAge/resources/externals/models/environments/cactus.blend')
    #exporter.export_blend('/home/ubuntunux/WorkSpace/StoneAge/resources/externals/meshes/environments/cliff_grass.blend')
    #exporter.export_blend('/home/ubuntunux/WorkSpace/StoneAge/resources/externals/meshes/characters/jack/jack.blend')
    #exporter.export_blend('/home/ubuntunux/WorkSpace/StoneAge/resources/externals/models/characters/jack.blend')
    #exporter.export_resources()