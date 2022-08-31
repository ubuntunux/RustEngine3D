use std::fmt;
use std::collections::HashMap;

use sdl2::{ self, Sdl, AudioSubsystem };
use sdl2::mixer::{InitFlag, AUDIO_S16LSB, DEFAULT_CHANNELS, Sdl2MixerContext, Chunk, Channel};
use serde::{ Serialize, Deserialize };

use crate::constants::{ DEFAULT_AUDIO_VOLUME, MAX_AUDIO_CHANNEL_COUNT };
use crate::resource::resource::EngineResources;
use crate::resource::resource::ResourceData;
use crate::utilities::system::{ RcRefCell, newRcRefCell, ptr_as_mut, ptr_as_ref };


pub enum AudioLoop {
    ONCE,
    SOME(i32),
    LOOP,
}

pub struct AudioData {
    pub _audio_name: String,
    pub _sound_chunk: Chunk,
}

#[derive(Serialize, Deserialize, Default)]
#[serde(default)]
pub struct AudioBankCreateInfo {
    pub _audio_names: Vec<String>,
}

pub struct AudioBankData {
    pub _audio_bank_name: String,
    pub _audio_datas: Vec<RcRefCell<AudioData>>,
}

#[derive(Clone)]
pub struct AudioInstance {
    pub _audio_data: RcRefCell<AudioData>,
    pub _channel: Result<Channel, String>,
}

pub struct AudioManager {
    pub _engine_resources: *const EngineResources,
    pub _audio_instances: HashMap<i32, RcRefCell<AudioInstance>>,
    pub _bgm: Option<RcRefCell<AudioInstance>>,
    pub _audio_subsystem: AudioSubsystem,
    pub _mixer_context: Sdl2MixerContext,
    pub _volume: i32,
}


impl fmt::Debug for AudioData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}: {:?}", self._audio_name, self._sound_chunk.raw)
    }
}

impl Clone for AudioData {
    fn clone(&self) -> Self {
        AudioData {
            _audio_name: self._audio_name.clone(),
            _sound_chunk: Chunk {
                raw: self._sound_chunk.raw.clone(),
                owned: self._sound_chunk.owned
            }
        }
    }
}

impl fmt::Debug for AudioBankData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}: {:?}", self._audio_bank_name, self._audio_datas.len())
    }
}

impl AudioInstance {
    pub fn play_audio_instance(audio_data: &RcRefCell<AudioData>, audio_loop: AudioLoop) -> RcRefCell<AudioInstance> {
        let audio_loop = match audio_loop {
            AudioLoop::ONCE => 0,
            AudioLoop::SOME(x) => 0.max(x - 1),
            AudioLoop::LOOP => -1,
        };
        let chunk = &audio_data.borrow()._sound_chunk;

        newRcRefCell(AudioInstance {
            _audio_data: audio_data.clone(),
            _channel: sdl2::mixer::Channel::all().play(chunk, audio_loop),
        })
    }
}

impl AudioManager {
    pub fn create_audio_manager(sdl: &Sdl, engine_resources: *const EngineResources) -> AudioManager {
        log::info!("create_audio_manager");
        let audio_subsystem = sdl.audio().expect("failed to sdl.audio");
        let frequency = 44_100;
        let format = AUDIO_S16LSB; // signed 16 bit samples, in little-endian byte order
        let channels = DEFAULT_CHANNELS; // Stereo
        let chunk_size = 1_024;
        let result = sdl2::mixer::open_audio(frequency, format, channels, chunk_size);
        let mixer_context = sdl2::mixer::init(InitFlag::MP3 | InitFlag::FLAC | InitFlag::MOD | InitFlag::OGG).expect("sdl2::mixer::init");
        let channel_count = sdl2::mixer::allocate_channels(MAX_AUDIO_CHANNEL_COUNT);
        
        // audio debug info
        {
            log::info!("\tsdl2::mixer::open_audio: {:?}", result.unwrap());
            log::info!("\tsdl2::mixer::allocate_channels: {}", channel_count);
            log::info!("\tsdl2::mixer::linked version: {}", sdl2::mixer::get_linked_version());
            let n = sdl2::mixer::get_chunk_decoders_number();
            log::info!("\tavailable chunk(sample) decoders: {}", n);
            for i in 0..n {
                log::info!("\t\tdecoder {} => {}", i, sdl2::mixer::get_chunk_decoder(i));
            }
            let n = sdl2::mixer::get_music_decoders_number();
            log::info!("\tavailable music decoders: {}", n);
            for i in 0..n {
                log::info!("\t\tdecoder {} => {}", i, sdl2::mixer::get_music_decoder(i));
            }
            log::info!("\tquery spec => {:?}", sdl2::mixer::query_spec());
        }

        AudioManager {
            _engine_resources: engine_resources,
            _audio_instances: HashMap::new(),
            _bgm: None,
            _audio_subsystem: audio_subsystem,
            _mixer_context: mixer_context,
            _volume: DEFAULT_AUDIO_VOLUME,
        }
    }

    pub fn initialize_audio_manager(&mut self) {
        self._bgm = self.create_audio_instance("music-for-a-game-by-kris-klavenes", AudioLoop::LOOP);
    }

    pub fn destroy_audio_manager(&mut self) {
        sdl2::mixer::Music::halt();
        for (_key, audio) in self._audio_instances.iter() {
            let channel = &audio.borrow()._channel;
            if channel.is_ok() {
                channel.as_ref().unwrap().halt();
            }
        }
        self._audio_instances.clear();
    }

    pub fn get_engine_resources(&self) -> &EngineResources {
        ptr_as_ref(self._engine_resources)
    }
    pub fn get_engine_resources_mut(&self) -> &mut EngineResources {
        ptr_as_mut(self._engine_resources)
    }

    fn regist_audio_instance(&mut self, audio_instance: &RcRefCell<AudioInstance>) {
        match audio_instance.borrow()._channel {
            Ok(channel) => {
                channel.set_volume(self._volume);
                let Channel(channel_num) = channel;
                self._audio_instances.insert(channel_num, audio_instance.clone())
            },
            _ => None
        };
    }

    fn create_audio_instance_inner(&mut self, audio_data: &RcRefCell<AudioData>, audio_loop: AudioLoop) -> RcRefCell<AudioInstance> {
        let audio_instance = AudioInstance::play_audio_instance(audio_data, audio_loop);
        self.regist_audio_instance(&audio_instance);
        audio_instance
    }

    pub fn create_audio_instance(&mut self, audio_name: &str, audio_loop: AudioLoop) -> Option<RcRefCell<AudioInstance>> {
        let engine_resources = ptr_as_mut(self._engine_resources);
        if let ResourceData::Audio(audio_data) = engine_resources.get_audio_data(audio_name) {
            return Some(self.create_audio_instance_inner(&audio_data, audio_loop));
        }
        None
    }

    pub fn create_audio_instance_from_bank(&mut self, audio_name_bank: &str, audio_loop: AudioLoop) -> Option<RcRefCell<AudioInstance>> {
        if let ResourceData::AudioBank(audio_bank_data) = self.get_engine_resources_mut().get_audio_bank_data(audio_name_bank) {
            let audio_data_count = audio_bank_data.borrow()._audio_datas.len();
            if 0 < audio_data_count {
                let audio_data_index: usize = if 1 < audio_data_count { rand::random::<usize>() % audio_data_count } else { 0 };
                let audio_data = audio_bank_data.borrow()._audio_datas[audio_data_index].clone();
                return Some(self.create_audio_instance_inner(&audio_data, audio_loop))
            }
        }
        None
    }

    pub fn update_audio_manager(&mut self) {
        let mut remove_audio_instances: Vec<i32> = Vec::new();
        for (key, audio) in self._audio_instances.iter() {
            let channel = &audio.borrow()._channel;
            if channel.is_ok() {
                if false == channel.as_ref().unwrap().is_playing() {
                    remove_audio_instances.push(*key);
                }
            }
        }

        for key in remove_audio_instances.iter() {
            self._audio_instances.remove(key);
        }
    }
}
