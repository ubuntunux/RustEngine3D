use std::collections::HashMap;
use std::fmt;
use rand::Rng;
use sdl2::{self, AudioSubsystem, Sdl};
use sdl2::mixer::{AUDIO_S16LSB, Channel, Chunk, DEFAULT_CHANNELS, InitFlag, Sdl2MixerContext};
use serde::{Deserialize, Serialize};

use crate::constants::{DEFAULT_AUDIO_VOLUME, MAX_AUDIO_CHANNEL_COUNT};
use crate::resource::resource::EngineResources;
use crate::resource::resource::ResourceData;
use crate::utilities::system::{newRcRefCell, ptr_as_mut, ptr_as_ref, RcRefCell};

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
    pub _audio_data_list: Vec<RcRefCell<AudioData>>,
}

#[derive(Clone)]
pub struct AudioInstance {
    pub _audio_data: RcRefCell<AudioData>,
    pub _channel: Result<Channel, String>,
}

pub struct AudioManager<'a> {
    pub _engine_resources: *const EngineResources<'a>,
    pub _audio_instances: HashMap<i32, RcRefCell<AudioInstance>>,
    pub _bgm_audio_bank_data: Option<RcRefCell<AudioBankData>>,
    pub _bgm_audio_instance: Option<RcRefCell<AudioInstance>>,
    pub _bgm_volume: Option<f32>,
    pub _audio_subsystem: AudioSubsystem,
    pub _mixer_context: Sdl2MixerContext,
    pub _volume: i32,
}

// impl fmt::Debug for AudioData {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         write!(f, "{:?}: {:?}", self._audio_name, self._sound_chunk.raw)
//     }
// }
//
// impl Clone for AudioData {
//     fn clone(&self) -> Self {
//         AudioData {
//             _audio_name: self._audio_name.clone(),
//             _sound_chunk: Chunk {
//                 raw: self._sound_chunk.raw.clone(),
//                 owned: self._sound_chunk.owned,
//             },
//         }
//     }
// }

impl fmt::Debug for AudioBankData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:?}: {:?}",
            self._audio_bank_name,
            self._audio_data_list.len()
        )
    }
}

impl AudioInstance {
    pub fn play_audio_instance(
        audio_data: &RcRefCell<AudioData>,
        audio_loop: AudioLoop,
    ) -> RcRefCell<AudioInstance> {
        let audio_loop = match audio_loop {
            AudioLoop::ONCE => 0,
            AudioLoop::SOME(x) => 0.max(x - 1),
            AudioLoop::LOOP => -1,
        };
        let chunk = &audio_data.borrow()._sound_chunk;

        newRcRefCell(AudioInstance {
            _audio_data: audio_data.clone(),
            _channel: Channel::all().play(chunk, audio_loop),
        })
    }
}

impl<'a> AudioManager<'a> {
    pub fn create_audio_manager(
        sdl: &Sdl,
        engine_resources: *const EngineResources<'a>,
    ) -> Box<AudioManager<'a>> {
        log::info!("create_audio_manager");
        let audio_subsystem = sdl.audio().expect("failed to sdl.audio");
        let frequency = 44_100;
        let format = AUDIO_S16LSB; // signed 16 bit samples, in little-endian byte order
        let channels = DEFAULT_CHANNELS; // Stereo
        let chunk_size = 1_024;
        let result = sdl2::mixer::open_audio(frequency, format, channels, chunk_size);
        let mixer_context =
            sdl2::mixer::init(InitFlag::MP3 | InitFlag::FLAC | InitFlag::MOD | InitFlag::OGG)
                .expect("sdl2::mixer::init");
        let channel_count = sdl2::mixer::allocate_channels(MAX_AUDIO_CHANNEL_COUNT);

        // audio debug info
        {
            log::info!("\tsdl2::mixer::open_audio: {:?}", result.unwrap());
            log::info!("\tsdl2::mixer::allocate_channels: {}", channel_count);
            log::info!(
                "\tsdl2::mixer::linked version: {}",
                sdl2::mixer::get_linked_version()
            );
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

        Box::new(AudioManager {
            _engine_resources: engine_resources,
            _audio_instances: HashMap::new(),
            _bgm_audio_bank_data: None,
            _bgm_audio_instance: None,
            _bgm_volume: None,
            _audio_subsystem: audio_subsystem,
            _mixer_context: mixer_context,
            _volume: DEFAULT_AUDIO_VOLUME,
        })
    }

    pub fn initialize_audio_manager(&mut self) {
        // example) self._bgm = self.play_audio("default", AudioLoop::LOOP, None);
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

    pub fn get_engine_resources(&self) -> &EngineResources<'a> {
        ptr_as_ref(self._engine_resources)
    }
    pub fn get_engine_resources_mut(&self) -> &mut EngineResources<'a> {
        ptr_as_mut(self._engine_resources)
    }

    fn register_audio_instance(&mut self, audio_instance: &RcRefCell<AudioInstance>, volume: Option<f32>) {
        match audio_instance.borrow()._channel {
            Ok(channel) => {
                channel.set_volume(
                    if volume.is_some() {
                        (self._volume as f32 * volume.unwrap()) as i32
                    } else {
                        self._volume
                    }
                );
                let Channel(channel_num) = channel;
                self._audio_instances.insert(channel_num, audio_instance.clone())
            }
            _ => None,
        };
    }

    pub fn play_audio_data(
        &mut self,
        audio_data: &RcRefCell<AudioData>,
        audio_loop: AudioLoop,
        audio_volume: Option<f32>
    ) -> Option<RcRefCell<AudioInstance>> {
        let audio_instance = AudioInstance::play_audio_instance(audio_data, audio_loop);
        self.register_audio_instance(&audio_instance, audio_volume);
        Some(audio_instance)
    }

    pub fn play_audio(
        &mut self,
        audio_name: &str,
        audio_loop: AudioLoop,
        audio_volume: Option<f32>
    ) -> Option<RcRefCell<AudioInstance>> {
        let engine_resources = ptr_as_mut(self._engine_resources);
        if let ResourceData::Audio(audio_data) = engine_resources.get_audio_data(audio_name) {
            return self.play_audio_data(&audio_data, audio_loop, audio_volume);
        }
        None
    }

    pub fn play_audio_bank(
        &mut self,
        audio_name_bank: &str,
        audio_loop: AudioLoop,
        audio_volume: Option<f32>
    ) -> Option<RcRefCell<AudioInstance>> {
        let engine_resources = ptr_as_mut(self._engine_resources);
        if let ResourceData::AudioBank(audio_bank_data) = engine_resources.get_audio_bank_data(audio_name_bank) {
            return self.play_audio_bank_data(audio_bank_data, audio_loop, audio_volume);
        }
        None
    }

    pub fn play_audio_bank_data(
        &mut self,
        audio_bank_data: &RcRefCell<AudioBankData>,
        audio_loop: AudioLoop,
        audio_volume: Option<f32>
    ) -> Option<RcRefCell<AudioInstance>> {
        let audio_data_count = audio_bank_data.borrow()._audio_data_list.len() as u32;
        if 0 < audio_data_count {
            let mut rng = rand::rng();
            let n: u32 = rng.random();
            let audio_data_index: u32 = if 1 < audio_data_count {
                n % audio_data_count
            } else {
                0
            };
            let audio_data = audio_bank_data.borrow()._audio_data_list[audio_data_index as usize].clone();
            return self.play_audio_data(&audio_data, audio_loop, audio_volume);
        }
        None
    }

    pub fn play_audio_resource_data(
        &mut self,
        audio_resource_data: &ResourceData,
        audio_loop: AudioLoop,
        audio_volume: Option<f32>
    ) -> Option<RcRefCell<AudioInstance>> {
        match audio_resource_data {
            ResourceData::Audio(audio_data) => self.play_audio_data(audio_data, audio_loop, audio_volume),
            ResourceData::AudioBank(audio_bank_data) => self.play_audio_bank_data(audio_bank_data, audio_loop, audio_volume),
            _ => None
        }
    }

    pub fn play_bgm(
        &mut self,
        audio_data_name: &str,
        audio_volume: Option<f32>
    ) {
        let engine_resources = ptr_as_mut(self._engine_resources);
        let audio_resource_data = engine_resources.get_audio_bank_data(audio_data_name);
        if let ResourceData::AudioBank(audio_bank_data) = audio_resource_data {
            self.stop_bgm();

            self._bgm_audio_bank_data = Some(audio_bank_data.clone());
            self._bgm_audio_instance = self.play_audio_bank_data(audio_bank_data, AudioLoop::ONCE, audio_volume);
            self._bgm_volume = audio_volume;
        }
    }

    pub fn stop_bgm(&mut self) {
        if self._bgm_audio_instance.is_some() {
            if let Some(prev_audio_instance) = self._bgm_audio_instance.clone().as_ref() {
                self.stop_audio_instance(prev_audio_instance);
            }
        }
        self._bgm_audio_bank_data = None;
        self._bgm_audio_instance = None;
    }

    pub fn stop_audio_instance(&self, audio_instance: &RcRefCell<AudioInstance>) {
        let audio_instance_borrow = audio_instance.borrow();
        if let Ok(channel) = audio_instance_borrow._channel {
            channel.halt()
        }
    }

    pub fn is_playing_audio_instance(&self, audio_instance: &RcRefCell<AudioInstance>) -> bool {
        let audio_instance_borrow = audio_instance.borrow();
        if let Ok(channel) = audio_instance_borrow._channel.as_ref() {
            return channel.is_playing();
        }
        false
    }

    pub fn update_audio_manager(&mut self) {
        let mut is_playing_bgm: bool = false;
        if let Some(audio_instance_refcell) = self._bgm_audio_instance.as_ref() {
            if self.is_playing_audio_instance(audio_instance_refcell) {
                is_playing_bgm = true;
            }
        }

        if false == is_playing_bgm && self._bgm_audio_bank_data.is_some() {
            if let Some(bgm_audio_bank_data) = self._bgm_audio_bank_data.as_ref() {
                self._bgm_audio_instance = self.play_audio_bank_data(&bgm_audio_bank_data.clone(), AudioLoop::ONCE, self._bgm_volume);
            }
        }

        let mut remove_audio_instances: Vec<i32> = Vec::new();
        for (key, audio) in self._audio_instances.iter() {
            let channel = &audio.borrow()._channel;
            if let Ok(channel) = channel.as_ref() {
                if false == channel.is_playing() {
                    remove_audio_instances.push(*key);
                }
            }
        }

        for key in remove_audio_instances.iter() {
            self._audio_instances.remove(key);
        }
    }
}
