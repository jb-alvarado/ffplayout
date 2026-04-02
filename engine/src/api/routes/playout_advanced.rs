use actix_web::{Responder, delete, get, post, put, web};
use actix_web_grants::{authorities::AuthDetails, proc_macro::protect};
use log::*;
use sqlx::{Pool, Sqlite};
use tokio::sync::RwLock;

use crate::{
    AdvancedConfig,
    db::{
        handles,
        models::{Role, UserMeta},
    },
    player::controller::ChannelController,
    utils::{config::get_config, errors::ServiceError},
};

/// #### ffplayout Config
///
/// **Get Advanced Config**
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/playout/advanced/1 -H 'Authorization: Bearer <TOKEN>'
/// ```
///
/// Response is a JSON object
#[get("/playout/advanced/{id}")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
async fn get_advanced_config(
    id: web::Path<i32>,
    controllers: web::Data<RwLock<ChannelController>>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    let manager = {
        let guard = controllers.read().await;
        guard.get(*id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let config = manager.config.read().await.advanced.clone();

    Ok(web::Json(config))
}

/// **Get related Advanced Config**
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/playout/advanced/1/ -H 'Authorization: Bearer <TOKEN>'
/// ```
///
/// Response is a JSON object
#[get("/playout/advanced/{id}/")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
async fn get_related_advanced_config(
    pool: web::Data<Pool<Sqlite>>,
    id: web::Path<i32>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    match handles::select_related_advanced_configuration(&pool, *id).await {
        Ok(configs) => Ok(web::Json(
            configs
                .iter()
                .map(|c| AdvancedConfig::new(c.clone()))
                .collect::<Vec<_>>(),
        )),
        Err(e) => {
            error!("Advanced config: {e}");

            Err(ServiceError::InternalServerError)
        }
    }
}

/// **Delete Advanced Config**
///
/// ```BASH
/// curl -X DELETE http://127.0.0.1:8787/api/playout/advanced/ -H 'Authorization: Bearer <TOKEN>'
/// ```
///
/// Response is a JSON object
#[delete("/playout/advanced/{channel}/{id}")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin"),
    ty = "Role",
    expr = "user.channels.contains(&path.0) || role.has_authority(&Role::GlobalAdmin)"
)]
async fn remove_related_advanced_config(
    pool: web::Data<Pool<Sqlite>>,
    path: web::Path<(i32, i32)>,
    controllers: web::Data<RwLock<ChannelController>>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    let (_, id) = path.into_inner();
    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    if handles::delete_advanced_configuration(&pool, id)
        .await
        .is_ok()
    {
        let new_config = get_config(&pool, id).await?;
        manager.update_config(new_config).await;

        return Ok("Delete advanced configuration Success");
    }

    Err(ServiceError::InternalServerError)
}

/// **Update Advanced Config**
///
/// ```BASH
/// curl -X PUT http://127.0.0.1:8787/api/playout/advanced/1 -H "Content-Type: application/json" \
/// -d { <CONFIG DATA> } -H 'Authorization: Bearer <TOKEN>'
/// ```
#[put("/playout/advanced/{id}")]
#[protect(
    "Role::GlobalAdmin",
    "Role::ChannelAdmin",
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
async fn update_advanced_config(
    pool: web::Data<Pool<Sqlite>>,
    id: web::Path<i32>,
    data: web::Json<AdvancedConfig>,
    controllers: web::Data<RwLock<ChannelController>>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    let manager = {
        let guard = controllers.read().await;
        guard.get(*id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    handles::update_advanced_configuration(&pool, *id, data.into_inner()).await?;
    let new_config = get_config(&pool, *id).await?;

    manager.update_config(new_config).await;

    Ok(web::Json("Update success"))
}

/// **Add Advanced Config**
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/playout/advanced/1 -H "Content-Type: application/json" \
/// -d { <CONFIG DATA> } -H 'Authorization: Bearer <TOKEN>'
/// ```
#[post("/playout/advanced/{id}/")]
#[protect(
    "Role::GlobalAdmin",
    "Role::ChannelAdmin",
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
async fn add_advanced_config(
    pool: web::Data<Pool<Sqlite>>,
    id: web::Path<i32>,
    data: web::Json<AdvancedConfig>,
    controllers: web::Data<RwLock<ChannelController>>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    let manager = {
        let guard = controllers.read().await;
        guard.get(*id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    handles::insert_advanced_configuration(&pool, *id, None, data.into_inner()).await?;
    let new_config = get_config(&pool, *id).await?;

    manager.update_config(new_config).await;

    Ok(web::Json("Update success"))
}
