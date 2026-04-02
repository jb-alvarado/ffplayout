use std::{path::Path, sync::Arc};

use axum::{Extension, Json, extract::Path as AxumPath};
use sqlx::{Pool, Sqlite};
use tokio::sync::RwLock;

use crate::{
    db::{
        handles,
        models::{Output, Role},
    },
    file::norm_abs_path,
    player::controller::ChannelController,
    utils::{
        config::{PlayoutConfig, get_config},
        errors::ServiceError,
    },
};

use super::{AuthUser, MailQueues};

/// **Get Config**
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/playout/config/1 -H 'Authorization: Bearer <TOKEN>'
/// ```
///
/// Response is a JSON object
pub async fn get_playout_config(
    AxumPath(id): AxumPath<i32>,
    Extension(controllers): Extension<Arc<RwLock<ChannelController>>>,
    user: AuthUser,
) -> Result<Json<PlayoutConfig>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let config = manager.config.read().await.clone();

    Ok(Json(config))
}

/// **Update Config**
///
/// ```BASH
/// curl -X PUT http://127.0.0.1:8787/api/playout/config/1 -H "Content-Type: application/json" \
/// -d { <CONFIG DATA> } -H 'Authorization: Bearer <TOKEN>'
/// ```
#[allow(clippy::too_many_arguments)]
pub async fn update_playout_config(
    Extension(pool): Extension<Pool<Sqlite>>,
    AxumPath(id): AxumPath<i32>,
    Extension(controllers): Extension<Arc<RwLock<ChannelController>>>,
    Extension(mail_queues): Extension<MailQueues>,
    user: AuthUser,
    Json(mut data): Json<PlayoutConfig>,
) -> Result<Json<&'static str>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
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

    handles::update_output(&pool, data.output.id, id, &data.output.output_param).await?;
    handles::update_configuration(&pool, config_id, data).await?;
    let new_config = get_config(&pool, id).await?;
    let mut queues = mail_queues.lock().await;

    for queue in queues.iter_mut() {
        let mut queue_lock = queue.lock().await;

        if queue_lock.id == id {
            if queue_lock.config.recipient != new_config.mail.recipient {
                queue_lock.clear_raw();
            }

            queue_lock.update(new_config.mail.clone());
            break;
        }
    }

    manager.update_config(new_config).await;

    Ok(Json("Update success"))
}

/// **Get Output**
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/playout/output/1 -H 'Authorization: Bearer <TOKEN>'
/// ```
///
/// Response is a JSON object
pub async fn get_playout_outputs(
    Extension(pool): Extension<Pool<Sqlite>>,
    AxumPath(id): AxumPath<i32>,
    user: AuthUser,
) -> Result<Json<Vec<Output>>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    if let Ok(outputs) = handles::select_outputs(&pool, id).await {
        return Ok(Json(outputs));
    }

    Err(ServiceError::InternalServerError)
}
