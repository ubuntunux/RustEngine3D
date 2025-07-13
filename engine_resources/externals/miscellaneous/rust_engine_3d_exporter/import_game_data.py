import bpy
import os
from pathlib import Path
import sys
import shutil
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
        
    
class AssetImportManager(bpy.types.Operator):
    bl_idname = "object.asset_import_manager"
    bl_label = "import assets"
    bl_options = {'REGISTER', 'UNDO'}
    
    def info(self, msg):
        self.report({'INFO'}, str(msg))
    
    def initialize(self, library_name, asset_descriptor):
        self._asset_library = bpy.context.preferences.filepaths.asset_libraries[library_name]
        self._asset_importer_filepath = bpy.data.filepath
        self._asset_descriptor = asset_descriptor
        self._asset_catalogs_filepath = Path(self._asset_library.path, 'blender_assets.cats.txt')
        self._asset_catalog_names = {}
        self._asset_catalog_ids = {}
        
        self.load_asset_catalogs()
    
    def load_asset_catalogs(self):        
        contents = self._asset_catalogs_filepath.read_text().split('\n')
        for content in contents:
            if content.startswith('#') or ':' not in content:
                continue
            uuid, catalog_name, catalog_simple_name = content.strip().split(':')
            self._asset_catalog_ids[catalog_name] = uuid
            self._asset_catalog_names[uuid] = catalog_name
    
    def get_asset_catalog_id(self, catalog_simple_name):
        return self._asset_catalog_ids.get(catalog_simple_name, '')
    
    def get_asset_catalog_name(self, catalog_id):
        return self._asset_catalog_names.get(catalog_id, '')
    
    def register_asset_catalog_name(self, catalog_name):
        if catalog_name not in self._asset_catalog_names:
            catalog_id = str(uuid.uuid4())
            catlog_simple_name = catalog_name.replace('/', '-')
            self._asset_catalog_names[catalog_id] = catalog_name
            self._asset_catalog_ids[catalog_name] = catalog_id
            
            contents = self._asset_catalogs_filepath.read_text()
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
        material.name = material_name
        material.asset_mark()
        
        for node in material.node_tree.nodes:
            if node.label == 'textureBase':
                texture_filepath = Path(self._asset_library.path, 'textures/PolygonNatureBiomes/Terrain/Rock_Texture_01.png').as_posix()
                image_data = bpy.data.images.load(filepath=texture_filepath, check_existing=True)
                image_data.filepath = bpy.path.relpath(texture_filepath)
                node.image = image_data
                self.info(node.image.filepath)
            elif node.label == 'textureMaterial':
                pass
            elif node.label == 'textureNormal':
                pass
            
    def make_textures(self):
        textures_path = Path(self._asset_library.path, 'textures')
        textures = self._asset_descriptor.get_textures().values()
        for texture in textures:
            ext = texture.get_filepath().suffix
            dst_texture_filepath = Path(textures_path, texture.get_asset_name()).with_suffix(ext)
            if Util.get_mtime(dst_texture_filepath) < texture.get_mtime():
                self.info(f'copy {dst_texture_filepath} -> {texture.get_filepath()}')
                Util.copy(texture.get_filepath(), dst_texture_filepath)

    def make_meshes(self):        
        mesh_path = Path(self._asset_library.path, 'meshes')
        meshes = self._asset_descriptor.get_meshes().values()
        descriptor_name = self._asset_descriptor.get_descriptor_name()
        
        for mesh in meshes:
            Util.clear_scene()

            asset_name = mesh.get_asset_name()
            blend_filepath = Path(mesh_path, asset_name).with_suffix('.blend')
            if mesh.get_mtime() <= Util.get_mtime(blend_filepath):
                continue
            
            # save
            self.info(f'save: {blend_filepath}')
            Util.save_as(blend_filepath)
            
            # import fbx
            bpy.ops.import_scene.fbx(filepath=mesh.get_filepath().as_posix())
            
            # create collection
            collection = Util.create_collection(Path(asset_name).name)
            collection.asset_mark()
            catalog_name = '/'.join([self._asset_library.name, 'meshes', descriptor_name])
            catalog_id = self.get_asset_catalog_id(catalog_name)
            if catalog_id:
                collection.asset_data.catalog_id = catalog_id
            else:
                catalog_id = self.register_asset_catalog_name(catalog_name)
                collection.asset_data.catalog_id = catalog_id
            
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

    def execute(self, context):
        asset_root_path = '/mnt/Workspace/temp/PolygonNatureBiomes'
        sys.path.append(asset_root_path)
        
        import importlib
        import asset_descriptor
        importlib.reload(asset_descriptor)
        
        asset_descriptor_instance = asset_descriptor.AssetDescriptor(self, asset_root_path)
        self.initialize('StoneAge', asset_descriptor_instance)
        
        self.make_textures()
        self.make_meshes()
        return {'FINISHED'}


class TestOperator(bpy.types.Operator):
    bl_idname = "object.test_operator"
    bl_label = "Test"
    bl_options = {'REGISTER', 'UNDO'}
    
    def info(self, msg):
        self.report({'INFO'}, str(msg))
    
    def execute(self, context):
        self.info(os.path.abspath('.'))
        return {'FINISHED'}
    

class AssetImporterPanel(bpy.types.Panel):
    bl_label = "Asset Import Tools"
    bl_idname = "PT_SimplePanel"
    bl_space_type = 'VIEW_3D'
    bl_region_type = 'UI'
    bl_category = 'Tool'

    def draw(self, context):
        layout = self.layout

        row = layout.row()
        row.operator("object.asset_import_manager")
        row.operator("object.test_operator")

def register():
    bpy.utils.register_class(AssetImportManager)
    bpy.utils.register_class(TestOperator)
    bpy.utils.register_class(AssetImporterPanel)

def unregister():
    bpy.utils.unregister_class(AssetImporterPanel)
    bpy.utils.unregister_class(TestOperator)
    bpy.utils.unregister_class(AssetImportManager)

if __name__ == "__main__":
    register()