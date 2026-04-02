use actix_web::{Responder, delete, get, post, put, web};
use actix_web_grants::{authorities::AuthDetails, proc_macro::protect};

use argon2::{
    Argon2, PasswordHasher,
    password_hash::{SaltString, rand_core::OsRng},
};
use log::*;
use sqlx::{Pool, Sqlite};

use crate::{
    db::{
        handles,
        models::{Role, User, UserMeta},
    },
    utils::errors::ServiceError,
};

/// From here on all request **must** contain the authorization header:\
/// `"Authorization: Bearer <TOKEN>"`
/// **Get current User**
///
/// ```BASH
/// curl -X GET 'http://127.0.0.1:8787/api/user' -H 'Content-Type: application/json' \
/// -H 'Authorization: Bearer <TOKEN>'
/// ```
#[get("/user")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role"
)]
async fn get_user(
    pool: web::Data<Pool<Sqlite>>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    match handles::select_user(&pool, user.id).await {
        Ok(user) => Ok(web::Json(user)),
        Err(e) => {
            error!("{e}");
            Err(ServiceError::InternalServerError)
        }
    }
}

/// **Get User by ID**
///
/// ```BASH
/// curl -X GET 'http://127.0.0.1:8787/api/user/2' -H 'Content-Type: application/json' \
/// -H 'Authorization: Bearer <TOKEN>'
/// ```
#[get("/user/{id}")]
#[protect("Role::GlobalAdmin", ty = "Role")]
async fn get_by_name(
    pool: web::Data<Pool<Sqlite>>,
    id: web::Path<i32>,
) -> Result<impl Responder, ServiceError> {
    match handles::select_user(&pool, *id).await {
        Ok(user) => Ok(web::Json(user)),
        Err(e) => {
            error!("{e}");
            Err(ServiceError::InternalServerError)
        }
    }
}

// **Get all User**
///
/// ```BASH
/// curl -X GET 'http://127.0.0.1:8787/api/users' -H 'Content-Type: application/json' \
/// -H 'Authorization: Bearer <TOKEN>'
/// ```
#[get("/users")]
#[protect("Role::GlobalAdmin", ty = "Role")]
async fn get_users(pool: web::Data<Pool<Sqlite>>) -> Result<impl Responder, ServiceError> {
    match handles::select_users(&pool).await {
        Ok(users) => Ok(web::Json(users)),
        Err(e) => {
            error!("{e}");
            Err(ServiceError::InternalServerError)
        }
    }
}

/// **Update current User**
///
/// ```BASH
/// curl -X PUT http://127.0.0.1:8787/api/user/1 -H 'Content-Type: application/json' \
/// -d '{"mail": "<MAIL>", "password": "<PASS>"}' -H 'Authorization: Bearer <TOKEN>'
/// ```
#[put("/user/{id}")]
#[protect(
    any("Role::GlobalAdmin", "Role::ChannelAdmin", "Role::User"),
    ty = "Role",
    expr = "*id == user.id || role.has_authority(&Role::GlobalAdmin)"
)]
async fn update_user(
    pool: web::Data<Pool<Sqlite>>,
    id: web::Path<i32>,
    data: web::Json<User>,
    role: AuthDetails<Role>,
    user: web::ReqData<UserMeta>,
) -> Result<impl Responder, ServiceError> {
    let channel_ids = data.channel_ids.clone().unwrap_or_default();
    let mut fields = String::new();

    if let Some(mail) = data.mail.clone() {
        if !fields.is_empty() {
            fields.push_str(", ");
        }

        fields.push_str(&format!("mail = '{mail}'"));
    }

    if !data.password.is_empty() {
        if !fields.is_empty() {
            fields.push_str(", ");
        }

        let password_hash = web::block(move || {
            let salt = SaltString::generate(&mut OsRng);

            Argon2::default()
                .hash_password(data.password.clone().as_bytes(), &salt)
                .map(|p| p.to_string())
        })
        .await?
        .map_err(|e| ServiceError::Conflict(e.to_string()))?;

        fields.push_str(&format!("password = '{password_hash}'"));
    }

    handles::update_user(&pool, *id, fields).await?;

    let related_channels = handles::select_related_channels(&pool, Some(*id)).await?;

    for channel in related_channels {
        if !channel_ids.contains(&channel.id) {
            handles::delete_user_channel(&pool, *id, channel.id).await?;
        }
    }

    handles::insert_user_channel(&pool, *id, channel_ids).await?;

    Ok("Update Success")
}

/// **Add User**
///
/// ```BASH
/// curl -X POST 'http://127.0.0.1:8787/api/user/' -H 'Content-Type: application/json' \
/// -d '{"mail": "<MAIL>", "username": "<USER>", "password": "<PASS>", "role_id": 1, "channel_id": 1}' \
/// -H 'Authorization: Bearer <TOKEN>'
/// ```
#[post("/user/")]
#[protect("Role::GlobalAdmin", ty = "Role")]
async fn add_user(
    pool: web::Data<Pool<Sqlite>>,
    data: web::Json<User>,
) -> Result<impl Responder, ServiceError> {
    match handles::insert_user(&pool, data.into_inner()).await {
        Ok(..) => Ok("Add User Success"),
        Err(e) => {
            error!("{e}");
            Err(ServiceError::InternalServerError)
        }
    }
}

// **Delete User**
///
/// ```BASH
/// curl -X GET 'http://127.0.0.1:8787/api/user/2' -H 'Content-Type: application/json' \
/// -H 'Authorization: Bearer <TOKEN>'
/// ```
#[delete("/user/{id}")]
#[protect("Role::GlobalAdmin", ty = "Role")]
async fn remove_user(
    pool: web::Data<Pool<Sqlite>>,
    id: web::Path<i32>,
) -> Result<impl Responder, ServiceError> {
    match handles::delete_user(&pool, *id).await {
        Ok(_) => return Ok("Delete user success"),
        Err(e) => {
            error!("{e}");
            Err(ServiceError::InternalServerError)
        }
    }
}
