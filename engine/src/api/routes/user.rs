use axum::{Extension, Json, extract::Path};

use argon2::{
    Argon2, PasswordHasher,
    password_hash::{SaltString, rand_core::OsRng},
};
use log::*;
use sqlx::{Pool, Sqlite};
use tokio::task;

use crate::{
    db::{
        handles,
        models::{Role, User},
    },
    utils::errors::ServiceError,
};

use super::AuthUser;

/// From here on all request **must** contain the authorization header:\
/// `"Authorization: Bearer <TOKEN>"`
/// **Get current User**
///
/// ```BASH
/// curl -X GET 'http://127.0.0.1:8787/api/user' -H 'Content-Type: application/json' \
/// -H 'Authorization: Bearer <TOKEN>'
/// ```
pub async fn get_user(
    Extension(pool): Extension<Pool<Sqlite>>,
    user: AuthUser,
) -> Result<Json<User>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;

    match handles::select_user(&pool, user.id).await {
        Ok(user) => Ok(Json(user)),
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
pub async fn get_by_name(
    Extension(pool): Extension<Pool<Sqlite>>,
    Path(id): Path<i32>,
    user: AuthUser,
) -> Result<Json<User>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin])?;

    match handles::select_user(&pool, id).await {
        Ok(user) => Ok(Json(user)),
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
pub async fn get_users(
    Extension(pool): Extension<Pool<Sqlite>>,
    user: AuthUser,
) -> Result<Json<Vec<User>>, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin])?;

    match handles::select_users(&pool).await {
        Ok(users) => Ok(Json(users)),
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
pub async fn update_user(
    Extension(pool): Extension<Pool<Sqlite>>,
    Path(id): Path<i32>,
    user: AuthUser,
    Json(data): Json<User>,
) -> Result<&'static str, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin, Role::ChannelAdmin, Role::User])?;
    user.ensure_self_or_admin(id)?;

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

        let password_hash = task::spawn_blocking(move || {
            let salt = SaltString::generate(&mut OsRng);

            Argon2::default()
                .hash_password(data.password.clone().as_bytes(), &salt)
                .map(|p| p.to_string())
        })
        .await?
        .map_err(|e| ServiceError::Conflict(e.to_string()))?;

        fields.push_str(&format!("password = '{password_hash}'"));
    }

    handles::update_user(&pool, id, fields).await?;

    let related_channels = handles::select_related_channels(&pool, Some(id)).await?;

    for channel in related_channels {
        if !channel_ids.contains(&channel.id) {
            handles::delete_user_channel(&pool, id, channel.id).await?;
        }
    }

    handles::insert_user_channel(&pool, id, channel_ids).await?;

    Ok("Update Success")
}

/// **Add User**
///
/// ```BASH
/// curl -X POST 'http://127.0.0.1:8787/api/user' -H 'Content-Type: application/json' \
/// -d '{"mail": "<MAIL>", "username": "<USER>", "password": "<PASS>", "role_id": 1, "channel_id": 1}' \
/// -H 'Authorization: Bearer <TOKEN>'
/// ```
pub async fn add_user(
    Extension(pool): Extension<Pool<Sqlite>>,
    user: AuthUser,
    Json(data): Json<User>,
) -> Result<&'static str, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin])?;

    match handles::insert_user(&pool, data).await {
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
pub async fn remove_user(
    Extension(pool): Extension<Pool<Sqlite>>,
    Path(id): Path<i32>,
    user: AuthUser,
) -> Result<&'static str, ServiceError> {
    user.ensure_any_role(&[Role::GlobalAdmin])?;

    match handles::delete_user(&pool, id).await {
        Ok(_) => Ok("Delete user success"),
        Err(e) => {
            error!("{e}");
            Err(ServiceError::InternalServerError)
        }
    }
}
