from pathlib import Path
import re

re_guid = re.compile('guid: (.+)')


class AssetInfo:
    def __init__(self, asset_name, asset_type, guid, filepath):
        self._asset_name = asset_name
        self._asset_type = asset_type
        self._guid = guid
        self._filepath = filepath
    
    def get_guid(self):
        return self._guid
    
    def get_asset_name(self):
        return self._asset_name
    
    def get_asset_type(self):
        return self._asset_type
    
    def get_filepath(self):
        return self._filepath
    
    def exists(self):
        return self._filepath.exists()

    def get_mtime(self):
        return self._filepath.stat().st_mtime if self._filepath.exists() else 0


class BaseImporter:
    def __init__(self, logger, root_path):
        self._logger = logger
        self._root_path = Path(root_path)
        self._assets = {}
        self._assets_by_guid = {}

    def register_asset(self, asset):
        self._assets[asset.get_asset_name()] = asset
        self._assets_by_guid[asset.get_asset_type()] = asset
    
    def get_assets(self):
        return self._assets
    
    def get_asset(self, asset_name='', guid=''):
        if asset_name:
            return self._assets.get(asset_name)
        elif guid:
            return self._assets_by_guid.get(guid)
        return None

    def process(self):
        pass


class MeshImporter(BaseImporter):
    pass


class TextureImporter(BaseImporter):
    def process(self):
        self._assets = {}
        asset_type = 'texture'
        asset_path_names = ['Terrain', 'Textures']
        exts = ['.png', '.tga', '.jpeg', '.jpg']
        for asset_path_name in asset_path_names:
            asset_path = self._root_path / asset_path_name
            for ext in exts:
                for filepath in asset_path.rglob(f'*{ext}'):
                    relative_filepath = filepath.relative_to(self._root_path)
                    asset_name = relative_filepath.parent / relative_filepath.stem
                    if asset_path_name == 'Textures':
                        asset_name = asset_name.relative_to(asset_path_name)
                    asset_name = asset_name.as_posix()
                    meta_filepath = filepath.with_suffix(f'{ext}.meta')
                    guid = re_guid.search(meta_filepath.read_text()).groups()[0]
                    self.register_asset(AssetInfo(asset_name, asset_type, guid, filepath))
        self._logger.info(f'textreus: {len(self._assets)}')


class AssetDescriptor:
    def __init__(self, logger, root_path):
        self._logger = logger
        self._root_path = Path(root_path)
        self._descriptor_name = self._root_path.stem
        self._texture_importer = TextureImporter(logger, root_path)
        self._mesh_importer = MeshImporter(logger, root_path)

        self.process()
    
    def process(self):
        self._texture_importer.process()
        self._mesh_importer.process()
        
    def get_descriptor_name(self):
        return self._descriptor_name
    
    def get_root_path(self):
        return self._root_path
    
    def get_textures(self):
        return self._texture_importer.get_assets()

    def get_texture(self, asset_name='', guid=''):
        return self._texture_importer.get_asset(asset_name=asset_name, guid=guid)