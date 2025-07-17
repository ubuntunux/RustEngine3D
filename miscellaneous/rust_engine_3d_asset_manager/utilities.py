import bpy
import datetime
import logging
from logging.handlers import RotatingFileHandler
import os
from pathlib import Path
import sys
import time

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