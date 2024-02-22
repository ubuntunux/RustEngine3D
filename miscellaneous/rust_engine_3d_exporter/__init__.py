from bpy_extras.io_utils import (
    ExportHelper,
    orientation_helper,
)
from bpy.props import (
    StringProperty,
)
import bpy
bl_info = {
    "name": "RustEngine3D Game Scene Exporter",
    "author": "ubuntunux@gmail.com",
    "version": (1, 0, 0),
    "blender": (4, 0, 0),
    "location": "File > Import-Export",
    "description": "Game Scene Export objects",
    "warning": "Images must be in file folder, "
               "filenames are limited to DOS 8.3 format",
    "doc_url": "https://github.com/ubuntunux/StoneAge",
    "category": "Import-Export",
}

import importlib
from . import export_game_scene
importlib.reload(export_game_scene)


@orientation_helper()
class ExportGameScene(bpy.types.Operator, ExportHelper):
    bl_idname = "export_scene.rust_engine_3d_game_scene"
    bl_label = 'Export .game_scene'
    bl_options = {'PRESET', 'UNDO'}

    filename_ext = ".game_scene"
    filter_glob: StringProperty(
        default="*.game_scene",
        options={'HIDDEN'},
    )

    def execute(self, context):
        keywords = self.as_keywords(ignore=("axis_forward",
                                            "axis_up",
                                            "filter_glob",
                                            "check_existing",
                                            ))
        return export_game_scene.save(self, context, **keywords)

    def draw(self, context):
        pass


# Add to a menu
def menu_func_export(self, context):
    self.layout.operator(ExportGameScene.bl_idname, text="Game Scene (.game_scene)")


def register():
    bpy.utils.register_class(ExportGameScene)
    bpy.types.TOPBAR_MT_file_export.append(menu_func_export)


def unregister():
    bpy.utils.unregister_class(ExportGameScene)
    bpy.types.TOPBAR_MT_file_export.remove(menu_func_export)


if __name__ == "__main__":
    register()
