use axum::{Extension, Json, extract::Path};
use tokio::sync::RwLock;

use crate::{
    db::models::Role, player::controller::ChannelController, sse::broadcast::Broadcaster,
    utils::errors::ServiceError,
};

use super::AuthUser;

/// ### System Statistics
///
/// Get statistics about CPU, Ram, Disk, etc. usage.
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/system/1
/// -H 'Content-Type: application/json' -H 'Authorization: Bearer <TOKEN>'
/// ```
pub async fn get_system_stat(
    Path(id): Path<i32>,
    Extension(broadcaster): Extension<std::sync::Arc<Broadcaster>>,
    Extension(controllers): Extension<std::sync::Arc<RwLock<ChannelController>>>,
    user: AuthUser,
) -> Result<Json<crate::utils::system::SystemStat>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    let manager = {
        let guard = controllers.read().await;
        guard.get(id)
    }
    .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

    let config = manager.config.read().await.clone();

    let stat = broadcaster.system.stat(&config).await;

    Ok(Json(stat))
}
