import bpy
import os
from pathlib import Path
import sys

class Util:
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
        bpy.ops.wm.save_as_mainfile(filepath=filepath)
        
    
class SimpleOperator(bpy.types.Operator):
    bl_idname = "object.simple_operator"
    bl_label = "Run My Code"
    bl_options = {'REGISTER', 'UNDO'}
    
    def info(self, msg):
        self.report({'INFO'}, str(msg))
    
    def initialize(self, library_name, project):
        # initialize project
        self._project = project
        
        # asset library
        self._asset_library = bpy.context.preferences.filepaths.asset_libraries[library_name]
        
        # initialize paths
        self._importer_filepath = bpy.data.filepath
        self._asset_paths = {}
        self._asset_paths['MESH'] = Path(self._asset_library.path) / 'meshes'
    
    def get_asset_path(self, asset_type):
        return self._asset_paths[asset_type]
    
    def get_asset_name(self, asset_type, filepath):
        project_asset_path = self._project.get_asset_path(asset_type)
        relative_filepath = filepath.relative_to(project_asset_path)
        return Path(relative_filepath.parent, relative_filepath.stem).as_posix()
    
    def get_blender_filepath(self, asset_type, asset_name):
        return Path(self.get_asset_path('MESH'), asset_name).with_suffix('.blend').as_posix()
    
    def get_default_material(self):
        default_material_filepath = Path(self._asset_library.path, 'materials/common/render_static_object.blend')
        with bpy.data.libraries.load(default_material_filepath.as_posix(), link=True, assets_only=True) as (data_from, data_to):
            data_to.materials = data_from.materials
        return bpy.data.materials['render_static_object']
    
    def override_material(self, material, material_name, blend_filepath):
        material.name = material_name
        material.asset_mark()
        
        for node in material.node_tree.nodes:
            if node.label == 'textureBase':
                texture_filepath = Path(self._asset_library.path, 'textures/PNB_Tropical_Jungle/Terrain/Rock_Texture_01.png').as_posix()
                image_data = bpy.data.images.load(filepath=texture_filepath, check_existing=True)
                image_data.filepath = bpy.path.relpath(texture_filepath)
                node.image = image_data
                self.info(node.image.filepath)
            elif node.label == 'textureMaterial':
                pass
            elif node.label == 'textureNormal':
                pass

    def make_meshes(self):
        mesh_path = self._project.get_asset_path('MESH')
        for filepath in mesh_path.glob('**/*.fbx'):
            #Util.clear_scene()

            asset_name = self.get_asset_name('MESH', filepath)
            blend_filepath = self.get_blender_filepath('MESH', asset_name)
            
            # save
            Util.save_as(blend_filepath)
            
            # import fbx
            bpy.ops.import_scene.fbx(filepath=filepath.as_posix())
            
            # create collection
            collection = Util.create_collection(asset_name)
            
            # default material
            default_material = self.get_default_material()
            
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
            Util.save_as(blend_filepath)
            #bpy.ops.wm.open_mainfile(filepath=self._importer_filepath)
            
            # for test break
            return

    def execute(self, context):
        project_path = '/mnt/Workspace/temp/PolygonNatureBiomes'
        sys.path.append(project_path)
        import importer
        project = importer.Project(project_path)
        self.initialize('StoneAge', project)
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
    

class SimplePanel(bpy.types.Panel):
    bl_label = "My Custom Tools"
    bl_idname = "PT_SimplePanel"
    bl_space_type = 'VIEW_3D'
    bl_region_type = 'UI'
    bl_category = 'Tool'

    def draw(self, context):
        layout = self.layout

        row = layout.row()
        row.operator("object.simple_operator")
        row.operator("object.test_operator")

def register():
    bpy.utils.register_class(SimpleOperator)
    bpy.utils.register_class(TestOperator)
    bpy.utils.register_class(SimplePanel)

def unregister():
    bpy.utils.unregister_class(SimplePanel)
    bpy.utils.unregister_class(TestOperator)
    bpy.utils.unregister_class(SimpleOperator)

if __name__ == "__main__":
    register()