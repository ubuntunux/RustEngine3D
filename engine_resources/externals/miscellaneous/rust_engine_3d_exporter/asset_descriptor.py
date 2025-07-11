from pathlib import Path
import re

re_guid = re.compile('guid: (.+)')


# asset infos
class AssetInfo:
    _asset_type = 'ASSET'

    def __init__(self, asset_name, filepath):
        self._asset_name = asset_name
        self._guid = self.extract_guid(filepath)
        self._filepath = filepath

    def extract_guid(self, filepath):
        meta_filepath = filepath.with_suffix(f'{filepath.suffix}.meta')
        return re_guid.search(meta_filepath.read_text()).groups()[0]
    
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

class MaterialInfo(AssetInfo):
    _asset_type = 'MATERIAL'

class MeshInfo(AssetInfo):
    _asset_type = 'MESH'

class TextureInfo(AssetInfo):
    _asset_type = 'TEXTURE'


# descriptors
class BaseDescriptor:
    def __init__(self, asset_descriptor, logger):
        self._asset_descriptor = asset_descriptor
        self._logger = logger
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

class MeshDescriptor(BaseDescriptor):
    pass

class MarerialDescriptor(BaseDescriptor):
    def process(self):
        root_path = self._asset_descriptor.get_root_path()
        descriptor_name = self._asset_descriptor.get_descriptor_name()
        asset_path_name = 'Materials'
        ext = '.mat'
        asset_path = root_path / asset_path_name
        
        self._assets = {}        
        for filepath in asset_path.rglob(f'*{ext}'):
            relative_filepath = filepath.relative_to(root_path)
            asset_name = descriptor_name / relative_filepath.with_suffix('')
            self.register_asset(MaterialInfo(asset_name.as_posix(), filepath))
        self._logger.info(f'materials: {len(self._assets)}')

class TextureDescriptor(BaseDescriptor):
    def process(self):
        root_path = self._asset_descriptor.get_root_path()
        descriptor_name = self._asset_descriptor.get_descriptor_name()
        asset_path_names = ['Terrain', 'Textures']
        exts = ['.png', '.tga', '.jpeg', '.jpg']
        
        self._assets = {}
        for asset_path_name in asset_path_names:
            asset_path = root_path / asset_path_name
            for ext in exts:
                for filepath in asset_path.rglob(f'*{ext}'):
                    relative_filepath = filepath.relative_to(root_path)
                    asset_name = relative_filepath.with_suffix('')
                    if asset_path_name == 'Textures':
                        asset_name = asset_name.relative_to(asset_path_name)
                    asset_name = descriptor_name / asset_name                    
                    self.register_asset(TextureInfo(asset_name.as_posix(), filepath))
        self._logger.info(f'textreus: {len(self._assets)}')


# asset descriptor
class AssetDescriptor:
    def __init__(self, logger, root_path):
        self._logger = logger
        self._root_path = Path(root_path)
        self._descriptor_name = self._root_path.stem
        self._texture_descriptor = TextureDescriptor(self, logger)
        self._mesh_descriptor = MeshDescriptor(self, logger)

        self.process()
    
    def process(self):
        self._texture_descriptor.process()
        self._mesh_descriptor.process()
        
    def get_descriptor_name(self):
        return self._descriptor_name
    
    def get_root_path(self):
        return self._root_path
    
    def get_textures(self):
        return self._texture_descriptor.get_assets()

    def get_texture(self, asset_name='', guid=''):
        return self._texture_descriptor.get_asset(asset_name=asset_name, guid=guid)