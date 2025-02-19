use std::path::Path;

use serde::{self, Deserialize, Deserializer, Serialize, Serializer};
use serde_with::{serde_as, NoneAsEmptyString};
use shlex::split;
use sqlx::{decode::Decode, encode::IsNull, sqlite::SqliteArgumentValue, Encode, Sqlite, Pool, Type};
use std::error::Error as StdError;
use tokio::io::AsyncReadExt;
use ts_rs::TS;

use crate::db::{handles, models::AdvancedConfiguration};
use crate::utils::ServiceError;
use derive_more::IsVariant;

#[derive(Debug, Clone, TS, PartialEq, Default, IsVariant)]
pub enum FilterValue {
    Some(String),
    Null,
    #[default]
    None,
}

impl<'de> Deserialize<'de> for FilterValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let opt = Option::<String>::deserialize(deserializer)?;
        Ok(match opt {
            Some(s) if s.trim().is_empty() => FilterValue::None,
            Some(s) if s.to_lowercase() == "null" => FilterValue::Null,
            Some(s) => FilterValue::Some(s),
            None => FilterValue::None,
        })
    }
}

impl Serialize for FilterValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            FilterValue::Some(s) => Some(s).serialize(serializer),
            FilterValue::Null => Some("null").serialize(serializer),
            FilterValue::None => None::<String>.serialize(serializer),
        }
    }
}

impl From<Option<String>> for FilterValue {
    fn from(opt: Option<String>) -> Self {
        match opt {
            Some(s) if s.trim().is_empty() => FilterValue::None,
            Some(s) if s.to_lowercase() == "null" => FilterValue::Null,
            Some(s) => FilterValue::Some(s),
            None => FilterValue::None,
        }
    }
}

impl From<&str> for FilterValue {
    fn from(s: &str) -> Self {
        if s.trim().is_empty() {
            FilterValue::None
        } else if s.to_lowercase() == "null" {
            FilterValue::Null
        } else {
            FilterValue::Some(s.to_string())
        }
    }
}

impl Type<Sqlite> for FilterValue {
    fn type_info() -> <Sqlite as sqlx::Database>::TypeInfo {
        <Option<String> as Type<Sqlite>>::type_info()
    }
}

impl<'q> Encode<'q, Sqlite> for FilterValue {
    fn encode_by_ref(&self, buf: &mut Vec<SqliteArgumentValue<'q>>) -> Result<IsNull, Box<dyn StdError + Send + Sync>> {
        match self {
            FilterValue::Some(s) => <Option<String> as Encode<Sqlite>>::encode_by_ref(&Some(s.clone()), buf),
            FilterValue::Null => <Option<String> as Encode<Sqlite>>::encode_by_ref(&Some("null".to_string()), buf),
            FilterValue::None => <Option<String> as Encode<Sqlite>>::encode_by_ref(&None, buf),
        }
    }
}

impl<'r> Decode<'r, Sqlite> for FilterValue {
    fn decode(value: <Sqlite as sqlx::database::Database>::ValueRef<'r>) -> Result<Self, Box<dyn StdError + Send + Sync>> {
        let value = <Option<String> as Decode<Sqlite>>::decode(value)?;
        Ok(match value {
            Some(s) if s.trim().is_empty() => FilterValue::None,
            Some(s) if s.to_lowercase() == "null" => FilterValue::Null,
            Some(s) => FilterValue::Some(s),
            None => FilterValue::None,
        })
    }
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, TS)]
#[ts(export, export_to = "advanced_config.d.ts")]
pub struct AdvancedConfig {
    pub id: i32,
    pub name: Option<String>,
    pub decoder: DecoderConfig,
    pub encoder: EncoderConfig,
    pub filter: FilterConfig,
    pub ingest: IngestConfig,
}

#[serde_as]
#[derive(Debug, Default, Serialize, Deserialize, Clone, TS)]
#[ts(export, export_to = "advanced_config.d.ts")]
pub struct DecoderConfig {
    #[ts(type = "string")]
    #[serde_as(as = "NoneAsEmptyString")]
    pub input_param: Option<String>,
    #[ts(type = "string")]
    #[serde_as(as = "NoneAsEmptyString")]
    pub output_param: Option<String>,
    #[ts(skip)]
    #[serde(skip_serializing, skip_deserializing)]
    pub input_cmd: Option<Vec<String>>,
    #[ts(skip)]
    #[serde(skip_serializing, skip_deserializing)]
    pub output_cmd: Option<Vec<String>>,
}

#[serde_as]
#[derive(Debug, Default, Serialize, Deserialize, Clone, TS)]
#[ts(export, export_to = "advanced_config.d.ts")]
pub struct EncoderConfig {
    #[ts(type = "string")]
    #[serde_as(as = "NoneAsEmptyString")]
    pub input_param: Option<String>,
    #[ts(skip)]
    #[serde(skip_serializing, skip_deserializing)]
    pub input_cmd: Option<Vec<String>>,
}

#[serde_as]
#[derive(Debug, Default, Serialize, Deserialize, Clone, TS)]
#[ts(export, export_to = "advanced_config.d.ts")]
pub struct IngestConfig {
    #[ts(type = "string")]
    #[serde_as(as = "NoneAsEmptyString")]
    pub input_param: Option<String>,
    #[ts(skip)]
    #[serde(skip_serializing, skip_deserializing)]
    pub input_cmd: Option<Vec<String>>,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, TS)]
#[ts(export, export_to = "advanced_config.d.ts")]
pub struct FilterConfig {
    #[ts(type = "string")]
    #[serde(default)]
    pub deinterlace: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub pad_video: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub fps: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub scale: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub set_dar: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub fade_in: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub fade_out: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub logo: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub overlay_logo_scale: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub overlay_logo_fade_in: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub overlay_logo_fade_out: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub overlay_logo: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub tpad: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub drawtext_from_file: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub drawtext_from_zmq: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub aevalsrc: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub afade_in: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub afade_out: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub apad: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub volume: FilterValue,
    #[ts(type = "string")]
    #[serde(default)]
    pub split: FilterValue,
}

impl AdvancedConfig {
    pub fn new(config: AdvancedConfiguration) -> Self {
        Self {
            id: config.id,
            name: config.name,
            decoder: DecoderConfig {
                input_param: config.decoder_input_param.clone(),
                output_param: config.decoder_output_param.clone(),
                input_cmd: match config.decoder_input_param {
                    Some(input_param) => split(&input_param),
                    None => None,
                },
                output_cmd: match config.decoder_output_param {
                    Some(output_param) => split(&output_param),
                    None => None,
                },
            },
            encoder: EncoderConfig {
                input_param: config.encoder_input_param.clone(),
                input_cmd: match config.encoder_input_param {
                    Some(input_param) => split(&input_param),
                    None => None,
                },
            },
            filter: FilterConfig {
                deinterlace: config.filter_deinterlace.into(),
                pad_video: config.filter_pad_video.into(),
                fps: config.filter_fps.into(),
                scale: config.filter_scale.into(),
                set_dar: config.filter_set_dar.into(),
                fade_in: config.filter_fade_in.into(),
                fade_out: config.filter_fade_out.into(),
                logo: config.filter_logo.into(),
                overlay_logo_scale: config.filter_overlay_logo_scale.into(),
                overlay_logo_fade_in: config.filter_overlay_logo_fade_in.into(),
                overlay_logo_fade_out: config.filter_overlay_logo_fade_out.into(),
                overlay_logo: config.filter_overlay_logo.into(),
                tpad: config.filter_tpad.into(),
                drawtext_from_file: config.filter_drawtext_from_file.into(),
                drawtext_from_zmq: config.filter_drawtext_from_zmq.into(),
                aevalsrc: config.filter_aevalsrc.into(),
                afade_in: config.filter_afade_in.into(),
                afade_out: config.filter_afade_out.into(),
                apad: config.filter_apad.into(),
                volume: config.filter_volume.into(),
                split: config.filter_split.into(),
            },
            ingest: IngestConfig {
                input_param: config.ingest_input_param.clone(),
                input_cmd: match config.ingest_input_param {
                    Some(input_param) => split(&input_param),
                    None => None,
                },
            },
        }
    }

    pub async fn dump(pool: &Pool<Sqlite>, id: i32) -> Result<(), ServiceError> {
        for conf in handles::select_related_advanced_configuration(pool, id).await? {
            let config = Self::new(conf);
            let f_keys = [
                "deinterlace",
                "pad_scale_w",
                "pad_scale_h",
                "pad_video",
                "fps",
                "scale",
                "set_dar",
                "fade_in",
                "fade_out",
                "overlay_logo_scale",
                "overlay_logo_fade_in",
                "overlay_logo_fade_out",
                "overlay_logo",
                "tpad",
                "drawtext_from_file",
                "drawtext_from_zmq",
                "aevalsrc",
                "afade_in",
                "afade_out",
                "apad",
                "volume",
                "split",
            ];

            let toml_string = toml_edit::ser::to_string_pretty(&config)?;
            let mut doc = toml_string.parse::<toml_edit::DocumentMut>()?;

            if let Some(decoder) = doc.get_mut("decoder").and_then(|o| o.as_table_mut()) {
                decoder
                    .decor_mut()
                    .set_prefix("# Changing these settings is for advanced users only!\n# There will be no support or guarantee that it will be stable after changing them.\n\n");
            }

            if let Some(output_param) = doc
                .get_mut("decoder")
                .and_then(|d| d.get_mut("output_param"))
                .and_then(|o| o.as_value_mut())
            {
                output_param
                    .decor_mut()
                    .set_suffix(" # get also applied to ingest instance.");
            }

            if let Some(filter) = doc.get_mut("filter") {
                for key in &f_keys {
                    if let Some(item) = filter.get_mut(*key).and_then(|o| o.as_value_mut()) {
                        match *key {
                            "deinterlace" => item.decor_mut().set_suffix(" # yadif=0:-1:0"),
                            "pad_video" => item
                                .decor_mut()
                                .set_suffix(" # pad='ih*{}/{}:ih:(ow-iw)/2:(oh-ih)/2'"),
                            "fps" => item.decor_mut().set_suffix(" # fps={}"),
                            "scale" => item.decor_mut().set_suffix(" # scale={}:{}"),
                            "set_dar" => item.decor_mut().set_suffix(" # setdar=dar={}"),
                            "fade_in" => item.decor_mut().set_suffix(" # fade=in:st=0:d=0.5"),
                            "fade_out" => item.decor_mut().set_suffix(" # fade=out:st={}:d=1.0"),
                            "overlay_logo_scale" => item.decor_mut().set_suffix(" # scale={}"),
                            "overlay_logo_fade_in" => {
                                item.decor_mut().set_suffix(" # fade=in:st=0:d=1.0:alpha=1");
                            }
                            "overlay_logo_fade_out" => item
                                .decor_mut()
                                .set_suffix(" # fade=out:st={}:d=1.0:alpha=1"),
                            "overlay_logo" => {
                                item.decor_mut().set_suffix(" # overlay={}:shortest=1");
                            }
                            "tpad" => item
                                .decor_mut()
                                .set_suffix(" # tpad=stop_mode=add:stop_duration={}"),
                            "drawtext_from_file" => {
                                item.decor_mut().set_suffix(" # drawtext=text='{}':{}{}");
                            }
                            "drawtext_from_zmq" => item
                                .decor_mut()
                                .set_suffix(" # zmq=b=tcp\\\\://'{}',drawtext@dyntext={}"),
                            "aevalsrc" => item.decor_mut().set_suffix(
                                " # aevalsrc=0:channel_layout=stereo:duration={}:sample_rate=48000",
                            ),
                            "afade_in" => item.decor_mut().set_suffix(" # afade=in:st=0:d=0.5"),
                            "afade_out" => item.decor_mut().set_suffix(" # afade=out:st={}:d=1.0"),
                            "apad" => item.decor_mut().set_suffix(" # apad=whole_dur={}"),
                            "volume" => item.decor_mut().set_suffix(" # volume={}"),
                            "split" => item.decor_mut().set_suffix(" # split={}{}"),
                            _ => (),
                        }
                    }
                }
            };

            tokio::fs::write(
                &format!("advanced_{id}_{}.toml", config.id),
                doc.to_string(),
            )
            .await?;

            println!("Dump advanced config to: advanced_{id}_{}.toml", config.id);
        }

        Ok(())
    }

    pub async fn import(pool: &Pool<Sqlite>, id: i32, path: &Path) -> Result<(), ServiceError> {
        if path.is_file() {
            let mut file = tokio::fs::File::open(path).await?;
            let mut contents = String::new();
            file.read_to_string(&mut contents).await?;

            let config: Self = toml_edit::de::from_str(&contents).unwrap();

            handles::update_advanced_configuration(pool, id, config).await?;
        } else {
            return Err(ServiceError::BadRequest("Path not exists!".to_string()));
        }

        Ok(())
    }

    pub fn is_empty_filter(&self) -> bool {
        self.filter.aevalsrc.is_none()
            && self.filter.afade_in.is_none()
            && self.filter.afade_out.is_none()
            && self.filter.apad.is_none()
            && self.filter.deinterlace.is_none()
            && self.filter.drawtext_from_file.is_none()
            && self.filter.drawtext_from_zmq.is_none()
            && self.filter.fade_in.is_none()
            && self.filter.fade_out.is_none()
            && self.filter.fps.is_none()
            && self.filter.logo.is_none()
            && self.filter.overlay_logo.is_none()
            && self.filter.overlay_logo_fade_in.is_none()
            && self.filter.overlay_logo_fade_out.is_none()
            && self.filter.overlay_logo_scale.is_none()
            && self.filter.pad_video.is_none()
            && self.filter.scale.is_none()
            && self.filter.set_dar.is_none()
            && self.filter.split.is_none()
            && self.filter.tpad.is_none()
            && self.filter.volume.is_none()
    }
}
