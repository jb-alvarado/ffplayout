use std::sync::Arc;

use axum::{Extension, Json, extract::Path};
use sqlx::{Pool, Sqlite};
use tokio::sync::RwLock;

use crate::{
    db::{
        handles,
        models::{Channel, Role},
    },
    player::controller::ChannelController,
    utils::{
        channels::{create_channel, delete_channel},
        config::get_config,
        errors::ServiceError,
    },
};

use super::{AuthUser, MailQueues};

/// #### Settings
///
/// **Get Settings from Channel**
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/channel/1 -H "Authorization: Bearer <TOKEN>"
/// ```
///
/// **Response:**
///
/// ```JSON
/// {
///     "id": 1,
///     "name": "Channel 1",
///     "preview_url": "http://localhost/live/preview.m3u8",
///     "extra_extensions": "jpg,jpeg,png",
///     "utc_offset": "+120"
/// }
/// ```
pub async fn get_channel(
    Extension(pool): Extension<Pool<Sqlite>>,
    Path(id): Path<i32>,
    user: AuthUser,
) -> Result<Json<Channel>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    if let Ok(channel) = handles::select_channel(&pool, &id).await {
        return Ok(Json(channel));
    }

    Err(ServiceError::InternalServerError)
}

/// **Get settings from all Channels**
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/channels -H "Authorization: Bearer <TOKEN>"
/// ```
pub async fn get_all_channels(
    Extension(pool): Extension<Pool<Sqlite>>,
    user: AuthUser,
) -> Result<Json<Vec<Channel>>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;

    if let Ok(channel) = handles::select_related_channels(&pool, Some(user.id)).await {
        return Ok(Json(channel));
    }

    Err(ServiceError::InternalServerError)
}

/// **Update Channel**
///
/// ```BASH
/// curl -X PATCH http://127.0.0.1:8787/api/channel/1 -H "Content-Type: application/json" \
/// -d '{ "id": 1, "name": "Channel 1", "preview_url": "http://localhost/live/stream.m3u8", "extra_extensions": "jpg,jpeg,png"}' \
/// -H "Authorization: Bearer <TOKEN>"
/// ```
pub async fn patch_channel(
    Extension(pool): Extension<Pool<Sqlite>>,
    Path(id): Path<i32>,
    Extension(controllers): Extension<Arc<RwLock<ChannelController>>>,
    user: AuthUser,
    Json(data): Json<Channel>,
) -> Result<&'static str, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let mut data = data;

    if !user.is_global_admin() {
        let channel = handles::select_channel(&pool, &id).await?;

        data.public = channel.public;
        data.playlists = channel.playlists;
        data.storage = channel.storage;
    }

    handles::update_channel(&pool, id, data.clone()).await?;
    let new_config = get_config(&pool, id).await?;

    manager.update_config(new_config).await;
    manager.update_channel(&data).await;

    Ok("Update Success")
}

/// **Create new Channel**
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/channel/ -H "Content-Type: application/json" \
/// -d '{ "name": "Channel 2", "preview_url": "http://localhost/live/channel2.m3u8", "extra_extensions": "jpg,jpeg,png" }' \
/// -H "Authorization: Bearer <TOKEN>"
/// ```
pub async fn add_channel(
    Extension(pool): Extension<Pool<Sqlite>>,
    Extension(controllers): Extension<Arc<RwLock<ChannelController>>>,
    Extension(queue): Extension<MailQueues>,
    user: AuthUser,
    Json(data): Json<Channel>,
) -> Result<Json<Channel>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin])?;

    match create_channel(&pool, controllers, queue, data).await {
        Ok(c) => Ok(Json(c)),
        Err(e) => Err(e),
    }
}

/// **Delete Channel**
///
/// ```BASH
/// curl -X DELETE http://127.0.0.1:8787/api/channel/2 -H "Authorization: Bearer <TOKEN>"
/// ```
pub async fn remove_channel(
    Extension(pool): Extension<Pool<Sqlite>>,
    Path(id): Path<i32>,
    Extension(controllers): Extension<Arc<RwLock<ChannelController>>>,
    Extension(queue): Extension<MailQueues>,
    user: AuthUser,
) -> Result<Json<&'static str>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin])?;

    delete_channel(&pool, id, controllers, queue).await?;

    Ok(Json("Delete Channel Success"))
}
