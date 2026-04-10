use axum::{
    Extension, Json,
    extract::{Path, Query},
};
use tokio::sync::RwLock;

use crate::{
    api::routes::{DateObj, PathsObj},
    db::models::Role,
    file::norm_abs_path,
    player::{controller::ChannelController, utils::JsonPlaylist},
    utils::{
        errors::ServiceError,
        playlist::{delete_playlist, generate_playlist, read_playlist, write_playlist},
    },
};

use super::AuthUser;

/// #### ffplayout Playlist Operations
///
/// **Get playlist**
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/playlist/1?date=2022-06-20
/// -H 'Content-Type: application/json' -H 'Authorization: Bearer <TOKEN>'
/// ```
pub async fn get_playlist(
    Path(id): Path<i32>,
    Query(obj): Query<DateObj>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
) -> Result<Json<JsonPlaylist>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let config = manager.config.read().await.clone();

    match read_playlist(&config, obj.date).await {
        Ok(playlist) => Ok(Json(playlist)),
        Err(e) => Err(e),
    }
}

/// **Save playlist**
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/playlist/1
/// -H 'Content-Type: application/json' -H 'Authorization: Bearer <TOKEN>'
/// --data "{<JSON playlist data>}"
/// ```
pub async fn save_playlist(
    Path(id): Path<i32>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
    Json(data): Json<JsonPlaylist>,
) -> Result<Json<String>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let config = manager.config.read().await.clone();

    match write_playlist(&config, data).await {
        Ok(res) => Ok(Json(res)),
        Err(e) => Err(e),
    }
}

/// **Generate Playlist**
///
/// A new playlist will be generated and response.
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/playlist/1/generate/2022-06-20
/// -H 'Content-Type: application/json' -H 'Authorization: Bearer <TOKEN>'
/// --data '{ "paths": [<list of paths>] }' # <- data is optional
/// ```
///
/// Or with template:
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/playlist/1/generate/2023-00-05
/// -H 'Content-Type: application/json' -H 'Authorization: Bearer <TOKEN>'
/// --data '{"template": {"sources": [\
///            {"start": "00:00:00", "duration": "10:00:00", "shuffle": true, "paths": ["path/1", "path/2"]}, \
///            {"start": "10:00:00", "duration": "14:00:00", "shuffle": false, "paths": ["path/3", "path/4"]}]}}'
/// ```
pub async fn gen_playlist(
    Path((id, date)): Path<(i32, String)>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
    Json(obj): Json<PathsObj>,
) -> Result<Json<JsonPlaylist>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    {
        let mut config = manager.config.write().await;

        config.general.generate = Some(vec![date.clone()]);

        if let Some(paths) = &obj.paths {
            let mut path_list = Vec::with_capacity(paths.len());
            let storage_root = config.channel.storage.clone();

            for path in paths {
                let (p, _, _) = norm_abs_path(&storage_root, path)?;
                path_list.push(p);
            }

            config.storage.paths = path_list;
        }

        config.general.template = obj.template.clone();
    }

    match generate_playlist(manager).await {
        Ok(playlist) => Ok(Json(playlist)),
        Err(e) => Err(e),
    }
}

/// **Delete Playlist**
///
/// ```BASH
/// curl -X DELETE http://127.0.0.1:8787/api/playlist/1/2022-06-20
/// -H 'Content-Type: application/json' -H 'Authorization: Bearer <TOKEN>'
/// ```
pub async fn del_playlist(
    Path((id, date)): Path<(i32, String)>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
) -> Result<Json<String>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let config = manager.config.read().await.clone();

    match delete_playlist(&config, &date).await {
        Ok(m) => Ok(Json(m)),
        Err(e) => Err(e),
    }
}
