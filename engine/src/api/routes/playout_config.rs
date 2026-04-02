use std::{path::Path, sync::Arc};

use actix_web::{Responder, get, put, web};
use actix_web_grants::{authorities::AuthDetails, proc_macro::protect};
use sqlx::{Pool, Sqlite};
use tokio::sync::{Mutex, RwLock};

use crate::{
    db::{
        handles,
        models::{Role, UserMeta},
    },
    file::norm_abs_path,
    player::controller::ChannelController,
    utils::{
        config::{PlayoutConfig, get_config},
        errors::ServiceError,
        mail::MailQueue,
    },
};

/// **Get Config**
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/playout/config/1 -H 'Authorization: Bearer <TOKEN>'
/// ```
///
/// Response is a JSON object
#[get("/playout/config/{id}")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
async fn get_playout_config(
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

    let config = manager.config.read().await.clone();

    Ok(web::Json(config))
}

/// **Update Config**
///
/// ```BASH
/// curl -X PUT http://127.0.0.1:8787/api/playout/config/1 -H "Content-Type: application/json" \
/// -d { <CONFIG DATA> } -H 'Authorization: Bearer <TOKEN>'
/// ```
#[allow(clippy::too_many_arguments)]
#[put("/playout/config/{id}")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
async fn update_playout_config(
    pool: web::Data<Pool<Sqlite>>,
    id: web::Path<i32>,
    mut data: web::Json<PlayoutConfig>,
    controllers: web::Data<RwLock<ChannelController>>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
    mail_queues: web::Data<Mutex<Vec<Arc<Mutex<MailQueue>>>>>,
) -> Result<impl Responder, ServiceError> {
    let manager = {
        let guard = controllers.read().await;
        guard.get(*id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;
    let p = manager.channel.lock().await.storage.clone();
    let storage = Path::new(&p);
    let config_id = manager.config.read().await.general.id;

    let (_, _, logo) = norm_abs_path(storage, &data.processing.logo)?;
    let (_, _, filler) = norm_abs_path(storage, &data.storage.filler)?;
    let (_, _, font) = norm_abs_path(storage, &data.text.font)?;

    data.processing.logo = logo;
    data.storage.filler = filler;
    data.text.font = font;

    handles::update_output(&pool, data.output.id, *id, &data.output.output_param).await?;
    handles::update_configuration(&pool, config_id, data.into_inner()).await?;
    let new_config = get_config(&pool, *id).await?;
    let mut queues = mail_queues.lock().await;

    for queue in queues.iter_mut() {
        let mut queue_lock = queue.lock().await;

        if queue_lock.id == *id {
            if queue_lock.config.recipient != new_config.mail.recipient {
                queue_lock.clear_raw();
            }

            queue_lock.update(new_config.mail.clone());
            break;
        }
    }

    manager.update_config(new_config).await;

    Ok(web::Json("Update success"))
}

/// **Get Output**
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/playout/output/1 -H 'Authorization: Bearer <TOKEN>'
/// ```
///
/// Response is a JSON object
#[get("/playout/outputs/{id}")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
async fn get_playout_outputs(
    pool: web::Data<Pool<Sqlite>>,
    id: web::Path<i32>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    if let Ok(outputs) = handles::select_outputs(&pool, *id).await {
        return Ok(web::Json(outputs));
    }

    Err(ServiceError::InternalServerError)
}
