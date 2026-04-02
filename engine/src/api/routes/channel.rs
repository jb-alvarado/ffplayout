use std::sync::Arc;

use actix_web::{Responder, delete, get, patch, post, web};
use actix_web_grants::{authorities::AuthDetails, proc_macro::protect};
use sqlx::{Pool, Sqlite};
use tokio::sync::{Mutex, RwLock};

use crate::{
    db::{
        handles,
        models::{Channel, Role, UserMeta},
    },
    player::controller::ChannelController,
    utils::{
        channels::{create_channel, delete_channel},
        config::get_config,
        errors::ServiceError,
        mail::MailQueue,
    },
};

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
#[get("/channel/{id}")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
async fn get_channel(
    pool: web::Data<Pool<Sqlite>>,
    id: web::Path<i32>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    if let Ok(channel) = handles::select_channel(&pool, &id).await {
        return Ok(web::Json(channel));
    }

    Err(ServiceError::InternalServerError)
}

/// **Get settings from all Channels**
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/channels -H "Authorization: Bearer <TOKEN>"
/// ```
#[get("/channels")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role"
)]
async fn get_all_channels(
    pool: web::Data<Pool<Sqlite>>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    if let Ok(channel) = handles::select_related_channels(&pool, Some(user.id)).await {
        return Ok(web::Json(channel));
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
#[patch("/channel/{id}")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
async fn patch_channel(
    pool: web::Data<Pool<Sqlite>>,
    id: web::Path<i32>,
    data: web::Json<Channel>,
    controllers: web::Data<RwLock<ChannelController>>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    let manager = {
        let guard = controllers.read().await;
        guard.get(*id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let mut data = data.into_inner();

    if !role.has_authority(&Role::GlobalAdmin) {
        let channel = handles::select_channel(&pool, &id).await?;

        data.public = channel.public;
        data.playlists = channel.playlists;
        data.storage = channel.storage;
    }

    handles::update_channel(&pool, *id, data.clone()).await?;
    let new_config = get_config(&pool, *id).await?;

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
#[post("/channel/")]
#[protect("Role::GlobalAdmin", ty = "Role")]
async fn add_channel(
    pool: web::Data<Pool<Sqlite>>,
    data: web::Json<Channel>,
    controllers: web::Data<RwLock<ChannelController>>,
    queue: web::Data<Mutex<Vec<Arc<Mutex<MailQueue>>>>>,
) -> Result<impl Responder, ServiceError> {
    match create_channel(
        &pool,
        controllers.into_inner(),
        queue.into_inner(),
        data.into_inner(),
    )
    .await
    {
        Ok(c) => Ok(web::Json(c)),
        Err(e) => Err(e),
    }
}

/// **Delete Channel**
///
/// ```BASH
/// curl -X DELETE http://127.0.0.1:8787/api/channel/2 -H "Authorization: Bearer <TOKEN>"
/// ```
#[delete("/channel/{id}")]
#[protect("Role::GlobalAdmin", ty = "Role")]
async fn remove_channel(
    pool: web::Data<Pool<Sqlite>>,
    id: web::Path<i32>,
    controllers: web::Data<RwLock<ChannelController>>,
    queue: web::Data<Mutex<Vec<Arc<Mutex<MailQueue>>>>>,
) -> Result<impl Responder, ServiceError> {
    delete_channel(&pool, *id, controllers.into_inner(), queue.into_inner()).await?;

    Ok(web::Json("Delete Channel Success"))
}
