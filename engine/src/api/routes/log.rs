use axum::extract::{Path, Query};

use crate::{
    api::routes::LogReq,
    db::models::Role,
    utils::{errors::ServiceError, read_log_file},
};

use super::AuthUser;

/// ### Log file
///
/// **Read Log File**
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/log/1?date=2022-06-20
/// -H 'Content-Type: application/json' -H 'Authorization: Bearer <TOKEN>'
/// ```
pub async fn get_log(
    Path(id): Path<i32>,
    Query(log): Query<LogReq>,
    user: AuthUser,
) -> Result<String, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_channel_or_admin(id)?;

    read_log_file(&id, &log.date, log.timezone, log.download).await
}
