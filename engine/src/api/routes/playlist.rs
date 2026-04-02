use actix_web::{Responder, delete, get, post, web};
use actix_web_grants::{authorities::AuthDetails, proc_macro::protect};
use tokio::sync::RwLock;

use crate::{
    api::routes::{DateObj, PathsObj},
    db::models::{Role, UserMeta},
    file::norm_abs_path,
    player::{controller::ChannelController, utils::JsonPlaylist},
    utils::{
        errors::ServiceError,
        playlist::{delete_playlist, generate_playlist, read_playlist, write_playlist},
    },
};

/// #### ffplayout Playlist Operations
///
/// **Get playlist**
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/playlist/1?date=2022-06-20
/// -H 'Content-Type: application/json' -H 'Authorization: Bearer <TOKEN>'
/// ```
#[get("/playlist/{id}")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
pub async fn get_playlist(
    id: web::Path<i32>,
    obj: web::Query<DateObj>,
    controllers: web::Data<RwLock<ChannelController>>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    let manager = {
        let guard = controllers.read().await;
        guard.get(*id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let config = manager.config.read().await.clone();

    match read_playlist(&config, obj.date.clone()).await {
        Ok(playlist) => Ok(web::Json(playlist)),
        Err(e) => Err(e),
    }
}

/// **Save playlist**
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/playlist/1/
/// -H 'Content-Type: application/json' -H 'Authorization: Bearer <TOKEN>'
/// --data "{<JSON playlist data>}"
/// ```
#[post("/playlist/{id}/")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
pub async fn save_playlist(
    id: web::Path<i32>,
    data: web::Json<JsonPlaylist>,
    controllers: web::Data<RwLock<ChannelController>>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    let manager = {
        let guard = controllers.read().await;
        guard.get(*id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let config = manager.config.read().await.clone();

    match write_playlist(&config, data.into_inner()).await {
        Ok(res) => Ok(web::Json(res)),
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
/// /// --data '{ "paths": [<list of paths>] }' # <- data is optional
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
#[post("/playlist/{id}/generate/{date}")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&params.0) || role.has_authority(&Role::GlobalAdmin)"
)]
pub async fn gen_playlist(
    params: web::Path<(i32, String)>,
    data: Option<web::Json<PathsObj>>,
    controllers: web::Data<RwLock<ChannelController>>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    let (id, date) = params.into_inner();

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    {
        let mut config = manager.config.write().await;

        config.general.generate = Some(vec![date.clone()]);

        if let Some(obj) = &data {
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
    }

    match generate_playlist(manager).await {
        Ok(playlist) => Ok(web::Json(playlist)),
        Err(e) => Err(e),
    }
}

/// **Delete Playlist**
///
/// ```BASH
/// curl -X DELETE http://127.0.0.1:8787/api/playlist/1/2022-06-20
/// -H 'Content-Type: application/json' -H 'Authorization: Bearer <TOKEN>'
/// ```
#[delete("/playlist/{id}/{date}")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&params.0) || role.has_authority(&Role::GlobalAdmin)"
)]
pub async fn del_playlist(
    params: web::Path<(i32, String)>,
    controllers: web::Data<RwLock<ChannelController>>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    let (id, date) = params.into_inner();
    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let config = manager.config.read().await.clone();

    match delete_playlist(&config, &date).await {
        Ok(m) => Ok(web::Json(m)),
        Err(e) => Err(e),
    }
}
