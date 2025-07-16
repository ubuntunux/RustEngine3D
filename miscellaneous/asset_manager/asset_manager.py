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

sys.path.append(os.path.split(bpy.data.filepath)[0])

def open_text_file_in_blender_editor(filepath):
    text_data_block = bpy.data.texts.load(filepath)
    text_data_block.use_fake_user = False
    for window in bpy.context.window_manager.windows:
        screen = window.screen
        for area in screen.areas:
            if area.type == 'TEXT_EDITOR':
                if hasattr(area.spaces.active, 'text'):
                    area.spaces.active.text = text_data_block
                    bpy.context.window.screen = screen
                    bpy.context.area = area


def create_logger(logger_name, log_dirname, level):
    # prepare log directory
    if not os.path.exists(log_dirname):
        os.makedirs(log_dirname)
    log_file_basename = datetime.datetime.fromtimestamp(time.time()).strftime(f'{logger_name}_%Y%m%d_%H%M%S.log')
    log_filename = os.path.join(log_dirname, log_file_basename)

    # create logger
    logger = logging.getLogger(log_dirname)
    logger.setLevel(level=level)
    logger._filepath = log_filename

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
        

class AssetImportPanel(bpy.types.Operator):
    bl_idname = "object.asset_import_panel"
    bl_label = "import assets"
    bl_options = {'REGISTER', 'UNDO'}
    
    def execute(self, context):
        bpy.context.window.cursor_set('WAIT')
        try:
            # AssetDescriptorManager
            import asset_descriptor
            importlib.reload(asset_descriptor)
            asset_root_path = '/mnt/Workspace/temp/PolygonNatureBiomes'
            asset_descriptor_manager = asset_descriptor.AssetDescriptorManager(logger, asset_root_path)
            
            # AssetImportManager
            import import_game_data
            importlib.reload(import_game_data)
            asset_library_name = 'StoneAge'
            asset_import_manager = import_game_data.AssetImportManager(logger, asset_library_name, asset_descriptor_manager)
            asset_import_manager.import_assets()
        except:
            logger.info(traceback.format_exc())
        
        # open log file
        open_text_file_in_blender_editor(logger._filepath)
        bpy.context.window.cursor_set('DEFAULT')
        return {'FINISHED'}


class AssetExportPanel(bpy.types.Operator):
    bl_idname = "object.asset_export_panel"
    bl_label = "export assets"
    bl_options = {'REGISTER', 'UNDO'}
    
    def execute(self, context):
        bpy.context.window.cursor_set('WAIT')
        try:
            # AssetExportManager
            import export_game_data
            importlib.reload(export_game_data)
            asset_library_name = 'StoneAge'
            asset_export_manager = export_game_data.AssetExportManager(logger, asset_library_name)
            asset_export_manager.export_assets()
        except:
            logger.info(traceback.format_exc())
        
        # open log file
        open_text_file_in_blender_editor(logger._filepath)
        bpy.context.window.cursor_set('DEFAULT')
        return {'FINISHED'}


class AssetManagerPanel(bpy.types.Panel):
    bl_label = "Asset Manager"
    bl_idname = "object.asset_manager"
    bl_space_type = 'VIEW_3D'
    bl_region_type = 'UI'
    bl_category = 'Tool'

    def draw(self, context):
        layout = self.layout
        row = layout.row()
        row.operator("object.asset_import_panel")
        row.operator("object.asset_export_panel")

def register():
    bpy.types.Scene.shared_text_input = bpy.props.StringProperty(
        name="Shared Text",
        description="Text input shared between panels",
        default="Default Shared Text"
    )
    
    bpy.utils.register_class(AssetImportPanel)
    bpy.utils.register_class(AssetExportPanel)
    bpy.utils.register_class(AssetManagerPanel)

def unregister():
    bpy.utils.unregister_class(AssetManagerPanel)
    bpy.utils.register_class(AssetExportPanel)
    bpy.utils.unregister_class(AssetImportPanel)
    
    del bpy.types.Scene.shared_text_input

if __name__ == "__main__":
    global logger
    logger = create_logger(
        logger_name='log', 
        log_dirname=Path(os.path.split(bpy.data.filepath)[0], '.log').as_posix(), 
        level=logging.INFO
    )
    
    register()