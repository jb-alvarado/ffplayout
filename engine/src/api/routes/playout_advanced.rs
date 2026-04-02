use axum::{Extension, Json, extract::Path};
use log::*;
use sqlx::{Pool, Sqlite};
use tokio::sync::RwLock;

use crate::{
    AdvancedConfig,
    db::{handles, models::Role},
    player::controller::ChannelController,
    utils::{config::get_config, errors::ServiceError},
};

use super::AuthUser;

/// #### ffplayout Config
///
/// **Get Advanced Config**
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/playout/advanced/1 -H 'Authorization: Bearer <TOKEN>'
/// ```
///
/// Response is a JSON object
pub async fn get_advanced_config(
    Path(id): Path<i32>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
) -> Result<Json<AdvancedConfig>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let config = manager.config.read().await.advanced.clone();

    Ok(Json(config))
}

/// **Get related Advanced Config**
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/playout/advanced/1/ -H 'Authorization: Bearer <TOKEN>'
/// ```
///
/// Response is a JSON object
pub async fn get_related_advanced_config(
    Extension(pool): Extension<Pool<Sqlite>>,
    Path(id): Path<i32>,
    user: AuthUser,
) -> Result<Json<Vec<AdvancedConfig>>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin])?;
    user.ensure_channel_or_admin(id)?;

    match handles::select_related_advanced_configuration(&pool, id).await {
        Ok(configs) => Ok(Json(
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
pub async fn remove_related_advanced_config(
    Extension(pool): Extension<Pool<Sqlite>>,
    Path((channel, id)): Path<(i32, i32)>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
) -> Result<&'static str, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin])?;
    user.ensure_channel_or_admin(channel)?;

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
pub async fn update_advanced_config(
    Extension(pool): Extension<Pool<Sqlite>>,
    Path(id): Path<i32>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
    Json(data): Json<AdvancedConfig>,
) -> Result<Json<&'static str>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    handles::update_advanced_configuration(&pool, id, data).await?;
    let new_config = get_config(&pool, id).await?;

    manager.update_config(new_config).await;

    Ok(Json("Update success"))
}

/// **Add Advanced Config**
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/api/playout/advanced/1 -H "Content-Type: application/json" \
/// -d { <CONFIG DATA> } -H 'Authorization: Bearer <TOKEN>'
/// ```
pub async fn add_advanced_config(
    Extension(pool): Extension<Pool<Sqlite>>,
    Path(id): Path<i32>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
    Json(data): Json<AdvancedConfig>,
) -> Result<Json<&'static str>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    handles::insert_advanced_configuration(&pool, id, None, data).await?;
    let new_config = get_config(&pool, id).await?;

    manager.update_config(new_config).await;

    Ok(Json("Update success"))
}
