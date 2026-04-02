use std::env;

use actix_files;
use actix_multipart::Multipart;
use actix_web::{
    HttpRequest, HttpResponse, Responder, get,
    http::header::{ContentDisposition, DispositionType},
    post, put, web,
};
use actix_web_grants::{authorities::AuthDetails, proc_macro::protect};
use tokio::{fs, sync::RwLock};

use crate::{
    api::routes::{FileObj, ImportObj},
    db::models::{Role, UserMeta},
    file::{MoveObject, PathObject, norm_abs_path},
    player::{controller::ChannelController, utils::import::import_file},
    utils::errors::ServiceError,
};

/// ### File Operations
///
/// **Get File/Folder List**
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/file/1/browse/ -H 'Content-Type: application/json'
/// -d '{ "source": "/" }' -H 'Authorization: Bearer <TOKEN>'
/// ```
#[post("/file/{id}/browse/")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
pub async fn file_browser(
    id: web::Path<i32>,
    data: web::Json<PathObject>,
    controllers: web::Data<RwLock<ChannelController>>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    let manager = {
        let guard = controllers.read().await;
        guard.get(*id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    match manager.storage.browser(&data.into_inner()).await {
        Ok(obj) => Ok(web::Json(obj)),
        Err(e) => Err(e),
    }
}

/// **Create Folder**
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/file/1/create-folder/ -H 'Content-Type: application/json'
/// -d '{"source": "<FOLDER PATH>"}' -H 'Authorization: Bearer <TOKEN>'
/// ```
#[post("/file/{id}/create-folder/")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
pub async fn add_dir(
    id: web::Path<i32>,
    data: web::Json<PathObject>,
    controllers: web::Data<RwLock<ChannelController>>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<HttpResponse, ServiceError> {
    let manager = {
        let guard = controllers.read().await;
        guard.get(*id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    manager.storage.mkdir(&data.into_inner()).await?;

    Ok(HttpResponse::Ok().into())
}

/// **Rename File**
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/file/1/rename/ -H 'Content-Type: application/json'
/// -d '{"source": "<SOURCE>", "target": "<TARGET>"}' -H 'Authorization: Bearer <TOKEN>'
/// ```
#[post("/file/{id}/rename/")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
pub async fn move_rename(
    id: web::Path<i32>,
    data: web::Json<MoveObject>,
    controllers: web::Data<RwLock<ChannelController>>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    let manager = {
        let guard = controllers.read().await;
        guard.get(*id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    match manager.storage.rename(&data.into_inner()).await {
        Ok(obj) => Ok(web::Json(obj)),
        Err(e) => Err(e),
    }
}

/// **Remove File/Folder**
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/file/1/remove/ -H 'Content-Type: application/json'
/// -d '{"source": "<SOURCE>"}' -H 'Authorization: Bearer <TOKEN>'
/// ```
#[post("/file/{id}/remove/")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
pub async fn remove(
    id: web::Path<i32>,
    data: web::Json<PathObject>,
    controllers: web::Data<RwLock<ChannelController>>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    let manager = {
        let guard = controllers.read().await;
        guard.get(*id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let recursive = data.recursive;

    match manager
        .storage
        .remove(&data.into_inner().source, recursive)
        .await
    {
        Ok(obj) => Ok(web::Json(obj)),
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
#[put("/file/{id}/upload/")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
async fn save_file(
    id: web::Path<i32>,
    _req: HttpRequest,
    payload: Multipart,
    obj: web::Query<FileObj>,
    controllers: web::Data<RwLock<ChannelController>>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<HttpResponse, ServiceError> {
    let manager = {
        let guard = controllers.read().await;
        guard.get(*id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    // let size: u64 = req
    //     .headers()
    //     .get("content-length")
    //     .and_then(|cl| cl.to_str().ok())
    //     .and_then(|cls| cls.parse().ok())
    //     .unwrap_or(0);

    manager.storage.upload(payload, &obj.path, false).await?;

    Ok(HttpResponse::Ok().into())
}

/// **Get File**
///
/// Can be used for preview video files
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/file/1/path/to/file.mp4
/// ```
#[get("/file/{id}/{filename:.*}")]
async fn get_file(
    req: HttpRequest,
    controllers: web::Data<RwLock<ChannelController>>,
) -> Result<actix_files::NamedFile, ServiceError> {
    let id: i32 = req.match_info().query("id").parse()?;
    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let config = manager.config.read().await;
    let storage = config.channel.storage.clone();
    let file_path = req.match_info().query("filename");
    let (path, _, _) = norm_abs_path(&storage, file_path)?;
    let file = actix_files::NamedFile::open(path)?;

    Ok(file
        .use_last_modified(true)
        .set_content_disposition(ContentDisposition {
            disposition: DispositionType::Attachment,
            parameters: vec![],
        }))
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
#[put("/file/{id}/import/")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
async fn import_playlist(
    id: web::Path<i32>,
    payload: Multipart,
    obj: web::Query<ImportObj>,
    controllers: web::Data<RwLock<ChannelController>>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<HttpResponse, ServiceError> {
    let manager = {
        let guard = controllers.read().await;
        guard.get(*id)
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

    Ok(HttpResponse::Ok().body(response))
}
