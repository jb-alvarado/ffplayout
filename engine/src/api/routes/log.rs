use actix_web::{Responder, get, web};
use actix_web_grants::{authorities::AuthDetails, proc_macro::protect};

use crate::{
    api::routes::LogReq,
    db::models::{Role, UserMeta},
    utils::{errors::ServiceError, read_log_file},
};

/// ### Log file
///
/// **Read Log File**
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/api/log/1?date=2022-06-20
/// -H 'Content-Type: application/json' -H 'Authorization: Bearer <TOKEN>'
/// ```
#[get("/log/{id}")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "user.channels.contains(&*id) || role.has_authority(&Role::GlobalAdmin)"
)]
pub async fn get_log(
    id: web::Path<i32>,
    log: web::Query<LogReq>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    read_log_file(&id, &log.date, log.timezone, log.download).await
}
