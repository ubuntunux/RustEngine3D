import bpy
import datetime
import importlib
import logging
from logging.handlers import RotatingFileHandler
import os
from pathlib import Path
import sys
import time
import traceback
from . import utilities

global logger
logger = utilities.create_logger(
    logger_name='log',
    log_dirname=Path(os.path.split(__file__)[0], '.log').as_posix(),
    level=logging.INFO
)

# --- Addon Information ---
bl_info = {
    "name": "RustEngine3D Asset Manager",
    "author": "ubuntunux",
    "version": (1, 0, 0),
    "blender": (4, 5, 0),
    "location": "File > Import/Export",
    "description": "RustEngine3D Asset import/export addon.",
    "warning": "",
    "doc_url": "https://github.com/ubuntunux/RustEngine3D",
    "category": "Import-Export",
}

class AssetImportPanel(bpy.types.Operator):
    bl_idname = "object.asset_import_panel"
    bl_label = "import assets"
    bl_options = {'REGISTER', 'UNDO'}
    
    def execute(self, context):
        try:
            # AssetDescriptorManager
            from . import asset_descriptor
            importlib.reload(asset_descriptor)
            
            asset_root_path = '/mnt/Workspace/temp/PolygonNatureBiomes'
            asset_descriptor_manager = asset_descriptor.AssetDescriptorManager(logger, asset_root_path)
            if not asset_descriptor_manager.is_valid_asset_descritor():
                asset_descritor_filepath = asset_descriptor_manager.create_default_asset_descritor_file()
                utilities.open_text_file_in_blender_editor(asset_descritor_filepath, use_fake_user=False)
                return {'FINISHED'}
            
            # AssetImportManager
            from . import import_game_data
            importlib.reload(import_game_data)
            
            asset_library_name = 'StoneAge'
            asset_import_manager = import_game_data.AssetImportManager(logger, asset_library_name, asset_descriptor_manager)
            asset_import_manager.import_assets()
        except:
            logger.info(traceback.format_exc())
                    
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
            from . import export_game_data
            importlib.reload(export_game_data)
            
            asset_library_name = 'StoneAge'
            asset_export_manager = export_game_data.AssetExportManager(logger, asset_library_name)
            asset_export_manager.export_assets()
        except:
            logger.info(traceback.format_exc())
        
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
        layout = self.layout
        row = layout.column()
        row.operator("object.asset_import_panel")
        row.operator("object.asset_export_panel")

def register():
    bpy.utils.register_class(AssetImportPanel)
    bpy.utils.register_class(AssetExportPanel)
    bpy.utils.register_class(AssetManagerPanel)

def unregister():
    bpy.utils.unregister_class(AssetManagerPanel)
    bpy.utils.register_class(AssetExportPanel)
    bpy.utils.unregister_class(AssetImportPanel)

if __name__ == "__main__":
    register()