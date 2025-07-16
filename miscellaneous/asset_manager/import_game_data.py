import bpy
import datetime
import os
from pathlib import Path
import shutil
import sys
import time
import traceback
import uuid


class Util:
    @staticmethod
    def copy(src_filepath, dst_filepath):
        if src_filepath.exists():
            if not dst_filepath.parent.exists():
                os.makedirs(dst_filepath.parent.as_posix())
            shutil.copy(src_filepath, dst_filepath)
    
    @staticmethod
    def get_mtime(filepath):
        return filepath.stat().st_mtime if filepath.exists() else 0
    
    @staticmethod
    def clear_assets(bpy_data_type):
        assets = bpy_data_type.values()
        for asset in assets:
            asset.use_fake_user = False
            bpy_data_type.remove(asset)
    
    @staticmethod
    def clear_scene():
        bpy.ops.wm.read_homefile(app_template="")        
        bpy.ops.object.select_all(action='SELECT')
        bpy.ops.object.delete(confirm=False)
        
        Util.clear_assets(bpy.data.collections)
        Util.clear_assets(bpy.data.texts)
        
        # clean-up recursive unused data-blocks
        bpy.ops.outliner.orphans_purge(do_local_ids=True, do_linked_ids=True, do_recursive=True)
    
    @staticmethod
    def create_collection(name):
        c = bpy.data.collections.new(name)
        bpy.context.scene.collection.children.link(c)
        return c
    
    @staticmethod
    def move_to_collection(collection, obj):
        bpy.context.scene.collection.objects.unlink(obj)
        collection.objects.link(obj)
    
    @staticmethod
    def save_as(filepath):
        if not filepath.parent.exists():
            os.makedirs(filepath.parent.as_posix())
        bpy.ops.wm.save_as_mainfile(filepath=filepath.as_posix())
        
    
class AssetImportManager:
    def __init__(self, __logger__, asset_library_name, asset_descriptor_manager):
        global logger
        logger = __logger__

        asset_library = bpy.context.preferences.filepaths.asset_libraries[asset_library_name]
        self._asset_library = asset_library
        self._asset_importer_filepath = bpy.data.filepath
        self._asset_catalogs_filepath = Path(asset_library.path, 'blender_assets.cats.txt')
        self._asset_catalog_names = {}
        self._asset_catalog_ids = {}
        self._assets = {}
        self._asset_descriptor_manager = asset_descriptor_manager
    
    def load_asset_catalogs(self):  
        logger.info('>>> load_asset_catalogs')
        contents = self._asset_catalogs_filepath.read_text().split('\n')
        for content in contents:
            if content.startswith('#') or ':' not in content:
                continue
            uuid, catalog_name, catalog_simple_name = content.strip().split(':')
            self._asset_catalog_ids[catalog_name] = uuid
            self._asset_catalog_names[uuid] = catalog_name
            logger.debug(f'{uuid}: {catalog_name}')
    
    def load_assets(self):
        logger.info('>>> load_assets')
        asset_library_path = Path(self._asset_library.path)
        for filepath in asset_library_path.glob('**/*.blend'):
            Util.clear_scene()
            
            with bpy.data.libraries.load(filepath.as_posix(), link=True, assets_only=True) as (data_from, data_to):
                data_to.objects = data_from.objects
                data_to.collections = data_from.collections
                data_to.materials = data_from.materials
                data_to.scenes = data_from.scenes
            
            data_blocks = [bpy.data.collections, bpy.data.objects, bpy.data.materials]
            for data_block in data_blocks:
                for asset in data_block:
                    if asset.asset_data:
                        library_path = os.path.abspath(bpy.path.abspath(asset.library.filepath))                        
                        if library_path == filepath.as_posix():
                            asset_name = self.get_asset_catalog_name(asset.asset_data.catalog_id) + '/' + asset.name                            
                            self._assets[asset_name] = asset
                            logger.debug(f'{type(asset)} {asset_name}')
    
    def get_asset_catalog_id(self, catalog_simple_name):
        catalog_id = self._asset_catalog_ids.get(catalog_simple_name, '')
        if not catalog_id:
            catalog_id = self.register_asset_catalog_name(catalog_simple_name)
        return catalog_id 
    
    def get_asset_catalog_name(self, catalog_id):
        return self._asset_catalog_names.get(catalog_id, '')
    
    def register_asset_catalog_name(self, catalog_name):
        if catalog_name not in self._asset_catalog_names:
            catalog_id = str(uuid.uuid4())
            catlog_simple_name = catalog_name.replace('/', '-')
            self._asset_catalog_names[catalog_id] = catalog_name
            self._asset_catalog_ids[catalog_name] = catalog_id
            
            contents = self._asset_catalogs_filepath.read_text().strip()
            contents += f'\n{catalog_id}:{catalog_name}:{catlog_simple_name}'
            self._asset_catalogs_filepath.write_text(contents)
            return catalog_id
        return ''
    
    def load_default_material(self):
        default_material_filepath = Path(self._asset_library.path, 'materials/common/render_static_object.blend')
        with bpy.data.libraries.load(default_material_filepath.as_posix(), link=True, assets_only=True) as (data_from, data_to):
            data_to.materials = data_from.materials
        return bpy.data.materials['render_static_object']    

    def override_material(self, material, material_name, blend_filepath):
        descriptor_name = self._asset_descriptor_manager.get_descriptor_name()
        catalog_name = '/'.join([self._asset_library.name, 'material_instances', descriptor_name])
        
        material.name = material_name
        material.asset_mark()
        material.asset_data.catalog_id = self.get_asset_catalog_id(catalog_name)
        
        for node in material.node_tree.nodes:
            if node.label == 'textureBase':
                texture_filepath = Path(self._asset_library.path, 'textures/PolygonNatureBiomes/Terrain/Rock_Texture_01.png').as_posix()
                image_data = bpy.data.images.load(filepath=texture_filepath, check_existing=True)
                image_data.filepath = bpy.path.relpath(texture_filepath)
                node.image = image_data
                logger.debug(node.image.filepath)
            elif node.label == 'textureMaterial':
                pass
            elif node.label == 'textureNormal':
                pass
    
    # process import
    def import_textures(self):
        textures_path = Path(self._asset_library.path, 'textures')
        textures = self._asset_descriptor_manager.get_textures().values()
        logger.info(f'>>> import_textures: {len(textures)}')
        for texture in textures:
            ext = texture.get_filepath().suffix
            dst_texture_filepath = Path(textures_path, texture.get_asset_name()).with_suffix(ext)
            if Util.get_mtime(dst_texture_filepath) < texture.get_mtime():
                logger.info(f'copy {dst_texture_filepath} -> {texture.get_filepath()}')
                Util.copy(texture.get_filepath(), dst_texture_filepath)

    def import_meshes(self):
        mesh_path = Path(self._asset_library.path, 'meshes')
        meshes = self._asset_descriptor_manager.get_meshes().values()
        descriptor_name = self._asset_descriptor_manager.get_descriptor_name()

        logger.info(f'>>> import_meshes: {len(meshes)}')
        for mesh in meshes:
            Util.clear_scene()

            asset_name = mesh.get_asset_name()
            blend_filepath = Path(mesh_path, asset_name).with_suffix('.blend')
            if mesh.get_mtime() <= Util.get_mtime(blend_filepath):
                continue
            
            # save
            logger.info(f'save mesh: {blend_filepath}')
            Util.save_as(blend_filepath)
            
            # import fbx
            bpy.ops.import_scene.fbx(filepath=mesh.get_filepath().as_posix())
            
            # create collection
            catalog_name = '/'.join([self._asset_library.name, 'meshes', descriptor_name])
            collection = Util.create_collection(Path(asset_name).name)
            collection.asset_mark()
            collection.asset_data.catalog_id = self.get_asset_catalog_id(catalog_name)
            
            # default material
            default_material = self.load_default_material()
            
            # make mesh
            for obj in bpy.context.scene.objects:
                # select object
                bpy.ops.object.select_all(action='DESELECT')
                obj.select_set(True)
                bpy.context.view_layer.objects.active = obj
                
                # move to collection
                Util.move_to_collection(collection, obj)
                
                # set material
                for material_slot in obj.material_slots:
                    material_slot.link = 'DATA'
                    material_slot.material = default_material
                    material_slot.link = 'OBJECT'
                    material_slot.material = default_material.copy()
                    self.override_material(material_slot.material, obj.name, blend_filepath)
            
            # save final
            collection.asset_generate_preview()
            Util.save_as(blend_filepath)
            bpy.ops.wm.open_mainfile(filepath=self._asset_importer_filepath)
    
    def import_models(self):
        model_path = Path(self._asset_library.path, 'models')
        models = self._asset_descriptor_manager.get_models().values()
        descriptor_name = self._asset_descriptor_manager.get_descriptor_name()
        logger.info(f'>>> import_models: {len(models)}')
        
        for model in models:
            Util.clear_scene()

            asset_name = model.get_asset_name()
            blend_filepath = Path(model_path, asset_name).with_suffix('.blend')
            if model.get_mtime() <= Util.get_mtime(blend_filepath):
                continue
            
            # save
            logger.info(f'save model: {blend_filepath}')
            Util.save_as(blend_filepath)
            
            # create collection
            collection = Util.create_collection(Path(asset_name).name)
            collection.asset_mark()
            catalog_name = '/'.join([self._asset_library.name, 'models', descriptor_name])
            catalog_id = self.get_asset_catalog_id(catalog_name)
            if catalog_id:
                collection.asset_data.catalog_id = catalog_id
            else:
                catalog_id = self.register_asset_catalog_name(catalog_name)
                collection.asset_data.catalog_id = catalog_id
            
            # make model
            for obj in bpy.context.scene.objects:
                # select object
                bpy.ops.object.select_all(action='DESELECT')
                obj.select_set(True)
                bpy.context.view_layer.objects.active = obj
                
                # move to collection
                Util.move_to_collection(collection, obj)
                
                # set material
                for material_slot in obj.material_slots:
                    material_slot.link = 'OBJECT'
                    material_slot.material = default_material.copy()
                    self.override_material(material_slot.material, obj.name, blend_filepath)
            
            # save final
            collection.asset_generate_preview()
            Util.save_as(blend_filepath)
            bpy.ops.wm.open_mainfile(filepath=self._asset_importer_filepath)

            # break for test
            return
        
    def import_assets(self):
        logger.info(f'>>> Begin: import_assets')

        # load asset descriptor
        self._asset_descriptor_manager.process()
        
        # load asset library
        self.load_asset_catalogs()
        self.load_assets()
        
        # process import        
        self.import_textures()
        self.import_meshes()
        #self.import_models()

        logger.info(f'>>> End: import_assets')