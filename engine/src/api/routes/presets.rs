use actix_web::{Responder, delete, get, post, put, web};
use actix_web_grants::{authorities::AuthDetails, proc_macro::protect};
use sqlx::{Pool, Sqlite};

use crate::{
    db::{
        handles,
        models::{Role, TextPreset, UserMeta},
    },
    utils::errors::ServiceError,
};

/// #### Text Presets
///
/// Text presets are made for sending text messages to the ffplayout engine, to overlay them as a lower third.
///
/// **Get all Presets**
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/presets/1 -H 'Content-Type: application/json' \
/// -H 'Authorization: Bearer <TOKEN>'
/// ```
#[get("/presets/{id}")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
async fn get_presets(
    pool: web::Data<Pool<Sqlite>>,
    id: web::Path<i32>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    if let Ok(presets) = handles::select_presets(&pool, *id).await {
        return Ok(web::Json(presets));
    }

    Err(ServiceError::InternalServerError)
}

/// **Update Preset**
///
/// ```BASH
/// curl -X PUT http://127.0.0.1:8787/api/presets/1 -H 'Content-Type: application/json' \
/// -d '{ "name": "<PRESET NAME>", "text": "<TEXT>", "x": "<X>", "y": "<Y>", "fontsize": 24, "line_spacing": 4, "fontcolor": "#ffffff", "box": 1, "boxcolor": "#000000", "boxborderw": 4, "alpha": 1.0, "channel_id": 1 }' \
/// -H 'Authorization: Bearer <TOKEN>'
/// ```
#[put("/presets/{channel}/{id}")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&path.0) || role.has_authority(&Role::GlobalAdmin)"
)]
async fn update_preset(
    pool: web::Data<Pool<Sqlite>>,
    path: web::Path<(i32, i32)>,
    data: web::Json<TextPreset>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    let (_, id) = path.into_inner();

    if handles::update_preset(&pool, &id, data.into_inner())
        .await
        .is_ok()
    {
        return Ok("Update Success");
    }

    Err(ServiceError::InternalServerError)
}

/// **Add new Preset**
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/presets/1/ -H 'Content-Type: application/json' \
/// -d '{ "name": "<PRESET NAME>", "text": "TEXT>", "x": "<X>", "y": "<Y>", "fontsize": 24, "line_spacing": 4, "fontcolor": "#ffffff", "box": 1, "boxcolor": "#000000", "boxborderw": 4, "alpha": 1.0, "channel_id": 1 }' \
/// -H 'Authorization: Bearer <TOKEN>'
/// ```
#[post("/presets/{id}/")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
async fn add_preset(
    pool: web::Data<Pool<Sqlite>>,
    id: web::Path<i32>,
    data: web::Json<TextPreset>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    if handles::insert_preset(&pool, data.into_inner())
        .await
        .is_ok()
    {
        return Ok("Add preset Success");
    }

    Err(ServiceError::InternalServerError)
}

/// **Delete Preset**
///
/// ```BASH
/// curl -X DELETE http://127.0.0.1:8787/api/presets/1/1 -H 'Content-Type: application/json' \
/// -H 'Authorization: Bearer <TOKEN>'
/// ```
#[delete("/presets/{channel}/{id}")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&path.0) || role.has_authority(&Role::GlobalAdmin)"
)]
async fn delete_preset(
    pool: web::Data<Pool<Sqlite>>,
    path: web::Path<(i32, i32)>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    let (_, id) = path.into_inner();

    if handles::delete_preset(&pool, &id).await.is_ok() {
        return Ok("Delete preset Success");
    }

    Err(ServiceError::InternalServerError)
}
