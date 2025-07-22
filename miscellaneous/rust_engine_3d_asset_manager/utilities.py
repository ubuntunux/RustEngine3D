import bpy
import datetime
import logging
import os
import shutil
import time

from logging.handlers import RotatingFileHandler
from pathlib import Path

def create_logger(logger_name, log_dirname, level='INFO'):
    # prepare log directory
    if not os.path.exists(log_dirname):
        os.makedirs(log_dirname)
    log_file_basename = datetime.datetime.fromtimestamp(time.time()).strftime(f'{logger_name}_%Y%m%d_%H%M%S.log')
    log_filename = os.path.join(log_dirname, log_file_basename)

    # create logger
    logger = logging.getLogger(log_dirname)
    log_level = logging.INFO
    match level.lower():
        case 'debug': log_level = logging.DEBUG
        case 'info': log_level = logging.INFO
        case 'warn': log_level = logging.WARN
        case 'error': log_level = logging.ERROR
        case _: log_level = logging.INFO
    logger.setLevel(level=log_level)
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

def copy(src_filepath, dst_filepath):
        if src_filepath.exists():
            if not dst_filepath.parent.exists():
                os.makedirs(dst_filepath.parent.as_posix())
            shutil.copy(src_filepath, dst_filepath)

def get_mtime(filepath):
    return filepath.stat().st_mtime if filepath.exists() else 0

def clear_assets(bpy_data_type):
    assets = bpy_data_type.values()
    for asset in assets:
        asset.use_fake_user = False
        bpy_data_type.remove(asset)

def clear_scene():
    bpy.ops.wm.read_homefile(app_template="")
    bpy.ops.object.select_all(action='SELECT')
    bpy.ops.object.delete(confirm=False)

    clear_assets(bpy.data.collections)
    clear_assets(bpy.data.objects)
    clear_assets(bpy.data.texts)

    bpy.ops.outliner.orphans_purge(do_local_ids=True, do_linked_ids=True, do_recursive=True)

def create_collection_with_asset_mark(collection_name, catalog_id):
    c = bpy.data.collections.new(collection_name)
    bpy.context.scene.collection.children.link(c)
    c.asset_mark()
    c.asset_data.catalog_id = catalog_id
    return c

def move_to_collection(collection, obj):
    bpy.context.scene.collection.objects.unlink(obj)
    collection.objects.link(obj)

def save_as(filepath):
    if not filepath.parent.exists():
        os.makedirs(filepath.parent.as_posix())
    bpy.ops.wm.save_as_mainfile(filepath=filepath.as_posix())

def open_text_file_in_blender_editor(filepath):
    filepath = Path(filepath)
    if filepath.name in bpy.data.texts:
        text_data_block = bpy.data.texts[filepath.name]
    else:
        text_data_block = bpy.data.texts.load(filepath.as_posix())
    for window in bpy.context.window_manager.windows:
        for area in window.screen.areas:
            if area.type == 'TEXT_EDITOR':
                if hasattr(area.spaces.active, 'text'):
                    area.spaces.active.text = text_data_block
                    bpy.context.window.screen = window.screen
                    bpy.context.area = area
                    return

def switch_to_scripting_workspace(workspace_name):
    scripting_workspace = None
    for workspace in bpy.data.workspaces:
        # workspaces: 'Layout', 'Scripting'
        if workspace.name == workspace_name:
            scripting_workspace = workspace
            break

    if scripting_workspace:
        bpy.context.window.workspace = scripting_workspace

def open_text_file_in_blender_editor(filepath, use_fake_user=True):
    filepath = Path(filepath)
    if filepath.name in bpy.data.texts:
        text_data_block = bpy.data.texts[filepath.name]
    else:
        text_data_block = bpy.data.texts.load(filepath.as_posix())
    text_data_block.use_fake_user = use_fake_user
    for window in bpy.context.window_manager.windows:
        screen = window.screen
        for area in screen.areas:
            if area.type == 'TEXT_EDITOR':
                if hasattr(area.spaces.active, 'text'):
                    area.spaces.active.text = text_data_block
                    bpy.context.window.screen = screen
                    bpy.context.area = area
                    switch_to_scripting_workspace('Scripting')
                    return