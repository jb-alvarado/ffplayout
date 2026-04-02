use actix_web::{Responder, get, web};
use actix_web_grants::{authorities::AuthDetails, proc_macro::protect};
use tokio::sync::RwLock;

use crate::{
    db::models::{Role, UserMeta},
    player::controller::ChannelController,
    sse::broadcast::Broadcaster,
    utils::errors::ServiceError,
};

/// ### System Statistics
///
/// Get statistics about CPU, Ram, Disk, etc. usage.
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/system/1
/// -H 'Content-Type: application/json' -H 'Authorization: Bearer <TOKEN>'
/// ```
#[get("/system/{id}")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
pub async fn get_system_stat(
    id: web::Path<i32>,
    broadcaster: web::Data<Broadcaster>,
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

    let stat = broadcaster.system.stat(&config).await;

    Ok(web::Json(stat))
}
