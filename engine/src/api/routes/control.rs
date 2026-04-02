use std::sync::atomic::Ordering;

use axum::{Extension, Json, extract::Path};
use sqlx::{Pool, Sqlite};
use tokio::sync::RwLock;

use crate::{
    db::models::Role,
    player::{controller::ChannelController, utils::get_data_map},
    utils::{
        TextFilter,
        control::{ControlParams, Process, ProcessCtl, control_state, send_message},
        errors::ServiceError,
    },
};

use super::AuthUser;

/// ### ffplayout controlling
///
/// here we communicate with the engine for:
/// - jump to last or next clip
/// - reset playlist state
/// - get infos about current, next, last clip
/// - send text to the engine, for overlaying it (as lower third etc.)
///
/// **Send Text to ffplayout**
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/control/1/text/ \
/// -H 'Content-Type: application/json' -H 'Authorization: Bearer <TOKEN>' \
/// -d '{"text": "Hello from ffplayout", "x": "(w-text_w)/2", "y": "(h-text_h)/2", fontsize": "24", "line_spacing": "4", "fontcolor": "#ffffff", "box": "1", "boxcolor": "#000000", "boxborderw": "4", "alpha": "1.0"}'
/// ```
pub async fn send_text_message(
    Path(id): Path<i32>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
    Json(data): Json<TextFilter>,
) -> Result<Json<serde_json::Map<String, serde_json::Value>>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    match send_message(manager, data).await {
        Ok(res) => Ok(Json(res)),
        Err(e) => Err(e),
    }
}

/// **Control Playout**
///
/// - next
/// - back
/// - reset
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/control/1/playout/ -H 'Content-Type: application/json'
/// -d '{ "command": "reset" }' -H 'Authorization: Bearer <TOKEN>'
/// ```
pub async fn control_playout(
    Extension(pool): Extension<Pool<Sqlite>>,
    Path(id): Path<i32>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
    Json(control): Json<ControlParams>,
) -> Result<Json<serde_json::Map<String, serde_json::Value>>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    if manager.is_processing.load(Ordering::SeqCst) {
        return Err(ServiceError::Conflict(
            "A command is already being processed, please wait".to_string(),
        ));
    }

    manager.is_processing.store(true, Ordering::SeqCst);

    let resp = match control_state(&pool, &manager, &control.control).await {
        Ok(res) => Ok(Json(res)),
        Err(e) => Err(e),
    };

    manager.is_processing.store(false, Ordering::SeqCst);

    resp
}

/// **Get current Clip**
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/control/1/media/current
/// -H 'Content-Type: application/json' -H 'Authorization: Bearer <TOKEN>'
/// ```
///
/// **Response:**
///
/// ```JSON
///     {
///       "media": {
///         "category": "",
///         "duration": 154.2,
///         "out": 154.2,
///         "in": 0.0,
///         "source": "/opt/tv-media/clip.mp4"
///       },
///       "index": 39,
///       "ingest": false,
///       "mode": "playlist",
///       "played": 67.808
///     }
/// ```
pub async fn media_current(
    Path(id): Path<i32>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
) -> Result<Json<serde_json::Map<String, serde_json::Value>>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let media_map = get_data_map(&manager).await;

    Ok(Json(media_map))
}

/// #### ffplayout Process Control
///
/// Control ffplayout process, like:
/// - start
/// - stop
/// - restart
/// - status
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/control/1/process/
/// -H 'Content-Type: application/json' -H 'Authorization: Bearer <TOKEN>'
/// -d '{"command": "start"}'
/// ```
pub async fn process_control(
    Path(id): Path<i32>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
    Json(proc): Json<Process>,
) -> Result<Json<&'static str>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    manager.list_init.store(true, Ordering::SeqCst);

    if manager.is_processing.load(Ordering::SeqCst) {
        return Err(ServiceError::Conflict(
            "A command is already being processed, please wait".to_string(),
        ));
    }

    manager.is_processing.store(true, Ordering::SeqCst);

    match proc.command {
        ProcessCtl::Status => {
            manager.is_processing.store(false, Ordering::SeqCst);

            if manager.is_alive.load(Ordering::SeqCst) {
                return Ok(Json("active"));
            }
            return Ok(Json("not running"));
        }
        ProcessCtl::Start => {
            if !manager.is_alive.load(Ordering::SeqCst) {
                manager.channel.lock().await.active = true;
                manager.start().await?;
            }
        }
        ProcessCtl::Stop => {
            manager.channel.lock().await.active = false;
            manager.stop_all(true).await;
        }
        ProcessCtl::Restart => {
            manager.channel.lock().await.active = false;
            manager.stop_all(false).await;

            tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;

            manager.channel.lock().await.active = true;
            manager.start().await?;
        }
    }

    manager.is_processing.store(false, Ordering::SeqCst);

    Ok(Json("Success"))
}
