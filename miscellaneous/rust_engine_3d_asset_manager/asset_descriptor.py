import json
from pathlib import Path
import re

global logger
re_guid = re.compile('guid: ([a-fA-F0-9]+)')

class AssetTypeCatalogNames:
    asset_type_catalog_names = {
        'ANIMATION_LAYER': 'animation_layers',
        'GAME_CHARACTER': 'game_data/characters',
        'GAME_DATA': 'game_data/data',
        'GAME_SCENE': 'game_data/game_scenes',
        'GAME_ITEM': 'game_data/items',
        'GAME_PROP': 'game_data/props',
        'GAME_WEAPON': 'game_data/weapons',
        'MATERIAL_INSTANCE': 'material_instances',
        'MATERIAL': 'materials',
        'MESH': 'meshes',
        'MODEL': 'models',
        'SCENE': 'scenes',
        'TEXTURE': 'textures'
    }

    @classmethod
    def get_asset_type_names(cls):
        return list(cls.asset_type_catalog_names.keys())

    @classmethod
    def get_asset_type_catalog_name(cls, asset_type):
        return cls.asset_type_catalog_names.get(asset_type)


ASSET_DESCRIPTOR_TEMPLATE = '''
    "MATERIAL": {
        "_asset_path_infos": [
            {"_asset_path_name": "Materials", "_asset_base_name": "ProjectName"}
        ],
        "_exts": [".mat"]
    },
    "MESH": {
        "_asset_path_infos": [
            {"_asset_path_name": "Models", "_asset_base_name": "ProjectName"}
        ],
        "_exts": [".fbx"]
    },
    "MODEL": {
        "_asset_path_infos": [
            {"_asset_path_name": "Prefabs", "_asset_base_name": "ProjectName"}
        ],
        "_exts": [".prefab"]
    },
    "SCENE": {
        "_asset_path_infos": [
            {"_asset_path_name": "Scenes", "_asset_base_name": "ProjectName"}
        ],
        "_exts": [".unity"]
    },
    "TEXTURE": {
        "_asset_path_infos": [
            {"_asset_path_name": "Textures", "_asset_base_name": "ProjectName"}
        ],
        "_exts": [".png", ".tga", ".jpeg", ".jpg"]
    }
}'''

class AssetMetadata:
    def __init__(self, asset_type, asset_path, filepath):
        self._asset_type = asset_type
        self._asset_path = asset_path
        self._asset_name = Path(self._asset_path).name
        self._guid = self.extract_guid(filepath)
        self._filepath = filepath
        logger.debug(str(self))

    def __str__(self):
        return f'AssetMetadata(asset_type={self._asset_type}, asset_path={self._asset_path}, asset_name={self._asset_name}, guid={self._guid}, filepath={self._filepath})'

    def extract_guid(self, filepath):
        meta_filepath = filepath.with_suffix(f'{filepath.suffix}.meta')
        if meta_filepath.exists():
            return re_guid.findall(meta_filepath.read_text())[0]
        return ''

    def mesh_guid(self):
        find_mesh_filter = False
        for line in self._filepath.read_text().split('\n'):
            if line.strip().startswith('MeshFilter:'):
                find_mesh_filter = True
            elif find_mesh_filter and line.strip().startswith('m_Mesh:'):
                return re_guid.findall(line)[0]
        return  ''

    def get_guid(self):
        return self._guid

    def get_asset_name(self):
        return self._asset_name

    def get_asset_path(self):
        return self._asset_path

    def get_asset_type(self):
        return self._asset_type

    def get_filepath(self):
        return self._filepath

    def exists(self):
        return self._filepath.exists()

    def get_mtime(self):
        return self._filepath.stat().st_mtime if self._filepath.exists() else 0

class AssetDescriptor:
    def __init__(self, asset_descriptor, asset_type):
        self._asset_descriptor = asset_descriptor
        self._asset_type = asset_type
        self._assets = {}
        self._assets_by_guid = {}

    def register_asset_metadata(self, asset):
        self._assets[asset.get_asset_path()] = asset
        self._assets_by_guid[asset.get_guid()] = asset

    def get_assets(self):
        return self._assets

    def get_asset(self, asset_name='', guid=''):
        if asset_name:
            return self._assets.get(asset_name)
        elif guid:
            return self._assets_by_guid.get(guid)
        return None

    def process(self, asset_descriptor_data):
        logger.info(f'AssetDescriptor::process::{self._asset_type}')
        root_path = self._asset_descriptor.get_root_path()
        my_asset_descriptor_data = asset_descriptor_data.get(self._asset_type, {})
        self._assets = {}
        for asset_path_info in my_asset_descriptor_data.get('_asset_path_infos', []):
            asset_directory_path = root_path / asset_path_info.get('_asset_path_name', '')
            asset_base_name = asset_path_info.get('_asset_base_name', '')
            for ext in my_asset_descriptor_data.get('_exts', []):
                for filepath in asset_directory_path.rglob(f'*{ext}'):
                    relative_filepath = filepath.relative_to(asset_directory_path)
                    asset_path = asset_base_name / relative_filepath.with_suffix('')
                    asset_metadata = AssetMetadata(self._asset_type, asset_path.as_posix(), filepath)
                    self.register_asset_metadata(asset_metadata)
                    logger.debug(asset_metadata)

class AssetDescriptorManager:
    def __init__(self, __logger__, root_path):
        global logger
        logger = __logger__

        self._root_path = Path(root_path)
        self._descriptor_name = self._root_path.stem
        self._asset_descriptor_filepath = Path(self._root_path, 'asset_descriptor.json')
        self._material_descriptor = AssetDescriptor(self, asset_type='MATERIAL')
        self._mesh_descriptor = AssetDescriptor(self, asset_type='MESH')
        self._model_descriptor = AssetDescriptor(self, asset_type='MODEL')
        self._scene_descriptor = AssetDescriptor(self, asset_type='SCENE')
        self._texture_descriptor = AssetDescriptor(self, asset_type='TEXTURE')

    def get_asset_descriptor_filepath(self):
        return self._asset_descriptor_filepath.as_posix()

    def is_valid_asset_descriptor(self):
        return self._asset_descriptor_filepath.exists()

    def create_default_asset_descriptor_file(self):
        self._asset_descriptor_filepath.write_text(ASSET_DESCRIPTOR_TEMPLATE)
        return self.get_asset_descriptor_filepath()

    def process(self):
        asset_descriptor_data = json.loads(self._asset_descriptor_filepath.read_text())

        # Process each asset type
        self._texture_descriptor.process(asset_descriptor_data)
        self._material_descriptor.process(asset_descriptor_data)
        self._mesh_descriptor.process(asset_descriptor_data)
        self._model_descriptor.process(asset_descriptor_data)
        self._scene_descriptor.process(asset_descriptor_data)

    def get_descriptor_name(self):
        return self._descriptor_name

    def get_root_path(self):
        return self._root_path

    def get_materials(self):
        return self._material_descriptor.get_assets()

    def get_material(self, asset_name='', guid=''):
        return self._material_descriptor.get_asset(asset_name=asset_name, guid=guid)

    def get_meshes(self):
        return self._mesh_descriptor.get_assets()

    def get_mesh(self, asset_name='', guid=''):
        return self._mesh_descriptor.get_asset(asset_name=asset_name, guid=guid)

    def get_models(self):
        return self._model_descriptor.get_assets()

    def get_model(self, asset_name='', guid=''):
        return self._model_descriptor.get_asset(asset_name=asset_name, guid=guid)

    def get_scenes(self):
        return self._scene_descriptor.get_assets()

    def get_scene(self, asset_name='', guid=''):
        return self._scene_descriptor.get_asset(asset_name=asset_name, guid=guid)

    def get_textures(self):
        return self._texture_descriptor.get_assets()

    def get_texture(self, asset_name='', guid=''):
        return self._texture_descriptor.get_asset(asset_name=asset_name, guid=guid)