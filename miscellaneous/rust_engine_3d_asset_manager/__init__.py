import os
from pathlib import Path
import traceback

import bpy
from bpy.props import StringProperty
from bpy.types import AddonPreferences, Panel, Operator

from . import asset_descriptor, export_game_data, import_game_data, utilities

bl_info = {
    "name": "RustEngine3D Asset Manager",
    "author": "ubuntunux",
    "version": (1, 0, 0),
    "blender": (4, 0, 0),
    "location": "File > Import/Export",
    "description": "RustEngine3D Asset import/export addon.",
    "warning": "",
    "doc_url": "https://github.com/ubuntunux/RustEngine3D",
    "category": "Import-Export",
}

logger = utilities.create_logger(
    logger_name='log',
    log_dirname=Path(os.path.split(__file__)[0], '.log').as_posix(),
    level='INFO'
)
config_filepath = Path(__file__).with_name('config.ini')

class AssetImportPanel(bpy.types.Operator):
    bl_idname = "object.asset_import_panel"
    bl_label = "import assets"
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        try:
            # AssetDescriptorManager
            asset_descriptor_path = bpy.context.scene.asset_descriptor_path
            asset_descriptor_manager = asset_descriptor.AssetDescriptorManager(logger, asset_descriptor_path)
            if not asset_descriptor_manager.is_valid_asset_descriptor():
                asset_descriptor_filepath = asset_descriptor_manager.create_default_asset_descriptor_file()
                utilities.open_text_file_in_blender_editor(asset_descriptor_filepath, use_fake_user=False)
                return {'FINISHED'}

            # AssetImportManager
            asset_library_name = bpy.context.scene.asset_library_name
            asset_import_manager = import_game_data.AssetImportManager(logger, asset_library_name, asset_descriptor_manager)
            asset_import_manager.import_assets()
        except:
            logger.info(traceback.format_exc())
            raise

        logger.info('FINISHED')

        # open log file
        utilities.open_text_file_in_blender_editor(logger._filepath, use_fake_user=False)
        return {'FINISHED'}


class AssetExportPanel(bpy.types.Operator):
    bl_idname = "object.asset_export_panel"
    bl_label = "export assets"
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        bpy.context.window.cursor_set('WAIT')
        try:
            # AssetExportManager
            asset_library_name = bpy.context.scene.asset_library_name
            asset_export_manager = export_game_data.AssetExportManager(logger, asset_library_name)
            asset_export_manager.export_assets()
        except:
            logger.info(traceback.format_exc())
            raise

        bpy.context.window.cursor_set('DEFAULT')
        logger.info('FINISHED')

        # open log file
        utilities.open_text_file_in_blender_editor(logger._filepath)
        return {'FINISHED'}


class AssetManagerPanel(bpy.types.Panel):
    bl_label = "RustEngine3D Asset Manager"
    bl_idname = "object.asset_manager"
    bl_space_type = 'VIEW_3D'
    bl_region_type = 'UI'
    bl_category = 'Tool'
    def draw(self, context):
        column = self.layout.column()
        column.label(text='Asset Library Name')
        column.prop(context.scene, "asset_library_name", text='')
        column.label(text='Import Asset Descriptor Path')
        column.prop(context.scene, "asset_descriptor_path", text='')
        column.operator("object.asset_import_panel", text='import assets')
        column.operator("object.asset_export_panel", text='export assets')

def initialize():
    # default data
    default_asset_library_name = ''
    asset_libraries = bpy.context.preferences.filepaths.asset_libraries
    if 0 < len(asset_libraries):
        default_asset_library_name = asset_libraries[0].name

    # config file
    config = {
        'asset_library_name': default_asset_library_name,
        'asset_descriptor_path': ''
    }
    if config_filepath.exists():
        config = eval(config_filepath.read_text())
    else:
        config_filepath.write_text(str(config))

    # register ui
    bpy.types.Scene.asset_library_name = bpy.props.StringProperty(
        name='Asset Library Name',
        default=config['asset_library_name']
    )

    bpy.types.Scene.asset_descriptor_path = bpy.props.StringProperty(
        name='Import Asset Descriptor Path',
        description="Select a asset descriptor folder path.",
        default=config['asset_descriptor_path'],
        subtype='DIR_PATH'
    )

def close():
    config = {
        'asset_library_name': bpy.context.scene.asset_library_name,
        'asset_descriptor_path': bpy.context.scene.asset_descriptor_path
    }
    config_filepath.write_text(str(config))

    del bpy.types.Scene.asset_library_name
    del bpy.types.Scene.asset_descriptor_path

def register():
    bpy.utils.register_class(AssetImportPanel)
    bpy.utils.register_class(AssetExportPanel)
    bpy.utils.register_class(AssetManagerPanel)
    initialize()

def unregister():
    close()
    bpy.utils.unregister_class(AssetManagerPanel)
    bpy.utils.unregister_class(AssetExportPanel)
    bpy.utils.unregister_class(AssetImportPanel)

if __name__ == "__main__":
    register()