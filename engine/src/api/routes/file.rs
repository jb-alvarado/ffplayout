use std::env;

use axum::{
    Extension, Json,
    body::Body,
    extract::{Multipart, Path, Query},
    http::{
        HeaderMap, StatusCode,
        header::{CONTENT_DISPOSITION, CONTENT_TYPE},
    },
    response::{IntoResponse, Response},
};
use tokio::{fs, sync::RwLock};

use crate::{
    api::routes::{FileObj, ImportObj},
    db::models::Role,
    file::{MoveObject, PathObject, norm_abs_path},
    player::{controller::ChannelController, utils::import::import_file},
    utils::errors::ServiceError,
};

use super::AuthUser;

/// ### File Operations
///
/// **Get File/Folder List**
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/file/1/browse/ -H 'Content-Type: application/json'
/// -d '{ "source": "/" }' -H 'Authorization: Bearer <TOKEN>'
/// ```
pub async fn file_browser(
    Path(id): Path<i32>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
    Json(data): Json<PathObject>,
) -> Result<Json<PathObject>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    match manager.storage.browser(&data).await {
        Ok(obj) => Ok(Json(obj)),
        Err(e) => Err(e),
    }
}

/// **Create Folder**
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/file/1/create-folder/ -H 'Content-Type: application/json'
/// -d '{"source": "<FOLDER PATH>"}' -H 'Authorization: Bearer <TOKEN>'
/// ```
pub async fn add_dir(
    Path(id): Path<i32>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
    Json(data): Json<PathObject>,
) -> Result<StatusCode, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    manager.storage.mkdir(&data).await?;

    Ok(StatusCode::OK)
}

/// **Rename File**
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/file/1/rename/ -H 'Content-Type: application/json'
/// -d '{"source": "<SOURCE>", "target": "<TARGET>"}' -H 'Authorization: Bearer <TOKEN>'
/// ```
pub async fn move_rename(
    Path(id): Path<i32>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
    Json(data): Json<MoveObject>,
) -> Result<Json<MoveObject>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    match manager.storage.rename(&data).await {
        Ok(obj) => Ok(Json(obj)),
        Err(e) => Err(e),
    }
}

/// **Remove File/Folder**
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/file/1/remove/ -H 'Content-Type: application/json'
/// -d '{"source": "<SOURCE>"}' -H 'Authorization: Bearer <TOKEN>'
/// ```
pub async fn remove(
    Path(id): Path<i32>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
    Json(data): Json<PathObject>,
) -> Result<StatusCode, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let recursive = data.recursive;

    match manager.storage.remove(&data.source, recursive).await {
        Ok(_) => Ok(StatusCode::OK),
        Err(e) => Err(e),
    }
}

/// **Upload File**
///
/// ```BASH
/// curl -X PUT http://127.0.0.1:8787/api/file/1/upload/ -H 'Authorization: Bearer <TOKEN>'
/// -F "file=@file.mp4"
/// ```
#[allow(clippy::too_many_arguments)]
pub async fn save_file(
    Path(id): Path<i32>,
    Query(obj): Query<FileObj>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
    payload: Multipart,
) -> Result<StatusCode, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    // let size: u64 = req
    //     .headers()
    //     .get("content-length")
    //     .and_then(|cl| cl.to_str().ok())
    //     .and_then(|cls| cls.parse().ok())
    //     .unwrap_or(0);

    manager.storage.upload(payload, &obj.path, false).await?;

    Ok(StatusCode::OK)
}

/// **Get File**
///
/// Can be used for preview video files
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/file/1/path/to/file.mp4
/// ```
pub async fn get_file(
    Path((id, filename)): Path<(i32, String)>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
) -> Result<Response, ServiceError> {
    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let config = manager.config.read().await;
    let storage = config.channel.storage.clone();
    let (path, _, _) = norm_abs_path(&storage, &filename)?;
    let bytes = fs::read(path).await?;

    let mut headers = HeaderMap::new();
    headers.insert(CONTENT_TYPE, "application/octet-stream".parse().unwrap());
    headers.insert(CONTENT_DISPOSITION, "attachment".parse().unwrap());

    Ok((StatusCode::OK, headers, Body::from(bytes)).into_response())
}

/// **Import playlist**
///
/// Import text/m3u file and convert it to a playlist
/// lines with leading "#" will be ignore
///
/// ```BASH
/// curl -X PUT http://127.0.0.1:8787/api/file/1/import/ -H 'Authorization: Bearer <TOKEN>'
/// -F "file=@list.m3u"
/// ```
pub async fn import_playlist(
    Path(id): Path<i32>,
    Query(obj): Query<ImportObj>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
    payload: Multipart,
) -> Result<(StatusCode, String), ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let channel_name = manager.channel.lock().await.name.clone();
    let playlists = manager.config.read().await.channel.playlists.clone();
    let file = obj.file.file_name().unwrap_or_default();
    let path = env::temp_dir().join(file);
    let path_clone = path.clone();

    manager.storage.upload(payload, &path, true).await?;

    let response = import_file(&playlists, &obj.date, Some(channel_name), &path_clone).await?;

    fs::remove_file(path).await?;

    Ok((StatusCode::OK, response))
}
