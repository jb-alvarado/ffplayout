use argon2::{Argon2, PasswordVerifier, password_hash::PasswordHash};
use axum::{Json as AxumJson, extract::State, http::StatusCode, response::IntoResponse};
use chrono::{TimeDelta, Utc};
use jsonwebtoken::{self, DecodingKey, EncodingKey, Header, Validation};
use log::*;
use serde::{Deserialize, Serialize};
use tokio::task;

use crate::{
    api::state::AppState,
    db::{
        GLOBAL_SETTINGS, handles,
        models::{Role, User},
    },
    utils::errors::ServiceError,
};

// Token lifetime
const ACCESS_LIFETIME: i64 = 3;
const REFRESH_LIFETIME: i64 = 30;

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
pub struct Claims {
    pub id: i32,
    pub channels: Vec<i32>,
    pub username: String,
    pub role: Role,
    exp: i64,
}

impl Claims {
    pub fn new(user: User, role: Role, lifetime: i64) -> Self {
        Self {
            id: user.id,
            channels: user.channel_ids.unwrap_or_default(),
            username: user.username,
            role,
            exp: (Utc::now() + TimeDelta::try_days(lifetime).unwrap()).timestamp(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Credentials {
    pub username: String,
    pub password: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct TokenRefreshRequest {
    pub refresh: String,
}

/// Create a json web token (JWT)
pub async fn encode_jwt(claims: Claims) -> Result<String, ServiceError> {
    let config = GLOBAL_SETTINGS.get().unwrap();
    let encoding_key = EncodingKey::from_secret(config.secret.clone().unwrap().as_bytes());
    Ok(jsonwebtoken::encode(
        &Header::default(),
        &claims,
        &encoding_key,
    )?)
}

/// Decode a json web token (JWT)
pub async fn decode_jwt(token: &str) -> Result<Claims, ServiceError> {
    let config = GLOBAL_SETTINGS.get().unwrap();
    let decoding_key = DecodingKey::from_secret(config.secret.clone().unwrap().as_bytes());
    jsonwebtoken::decode::<Claims>(token, &decoding_key, &Validation::default())
        .map(|data| data.claims)
        .map_err(|e| ServiceError::Unauthorized(e.to_string()))
}

/// #### User Handling
///
/// **Login**
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/auth/login -H "Content-Type: application/json" \
/// -d '{ "username": "<USER>", "password": "<PASS>" }'
/// ```
/// **Response:**
///
/// ```JSON
/// {
///     "access": "<ACCESS TOKEN>",
///     "refresh": "<REFRESH TOKEN>"
/// }
/// ```
pub async fn login(
    State(state): State<AppState>,
    AxumJson(credentials): AxumJson<Credentials>,
) -> Result<impl IntoResponse, ServiceError> {
    let username = credentials.username.clone();
    let password = credentials.password.clone();

    match handles::select_login(&state.pool, &username).await {
        Ok(mut user) => {
            if user.username.is_empty() {
                return Ok((
                    StatusCode::FORBIDDEN,
                    AxumJson(serde_json::json!({
                        "detail": "Incorrect credentials!",
                    })),
                )
                    .into_response());
            }

            let role = handles::select_role(&state.pool, &user.role_id.unwrap_or_default()).await?;

            let pass_hash = user.password.clone();
            let cred_password = password.clone();

            user.password = String::new();

            let verified_password = task::spawn_blocking(move || {
                let hash = PasswordHash::new(&pass_hash)?;
                Argon2::default().verify_password(cred_password.as_bytes(), &hash)
            })
            .await?;

            if verified_password.is_ok() {
                let access_claims = Claims::new(user.clone(), role.clone(), ACCESS_LIFETIME);
                let access_token = encode_jwt(access_claims).await?;
                let refresh_claims = Claims::new(user, role.clone(), REFRESH_LIFETIME);
                let refresh_token = encode_jwt(refresh_claims).await?;

                info!("user {} login, with role: {role}", username);

                return Ok((
                    StatusCode::OK,
                    AxumJson(serde_json::json!({
                        "access": access_token,
                        "refresh": refresh_token,
                    })),
                )
                    .into_response());
            }

            error!("Wrong password for {username}!");

            Ok((
                StatusCode::FORBIDDEN,
                AxumJson(serde_json::json!({
                    "detail": "Incorrect credentials!",
                })),
            )
                .into_response())
        }
        Err(e) => {
            error!("Login {username} failed! {e}");

            Ok((
                StatusCode::BAD_REQUEST,
                AxumJson(serde_json::json!({
                    "detail": format!("Login {username} failed!"),
                })),
            )
                .into_response())
        }
    }
}

/// **Refresh token**
///
/// ```BASH
/// curl -X POST http://127.0.0.1:8787/auth/refresh -H "Content-Type: application/json" \
/// -d '{ "refresh": "REFRESH TOKEN>" }'
/// ```
/// **Response:**
///
/// ```JSON
/// {
///     "access": "<ACCESS TOKEN>",
/// }
/// ```
pub async fn refresh(
    State(state): State<AppState>,
    AxumJson(data): AxumJson<TokenRefreshRequest>,
) -> Result<impl IntoResponse, ServiceError> {
    let refresh_t = &data.refresh;

    match decode_jwt(refresh_t).await {
        Ok(claims) => {
            let user_id = claims.id;
            let role = claims.role;

            if let Ok(user) = handles::select_user(&state.pool, user_id).await {
                let access_claims = Claims::new(user.clone(), role.clone(), ACCESS_LIFETIME);
                let access_token = encode_jwt(access_claims).await?;

                info!("user {} refresh, with role: {role}", user.username);

                Ok((
                    StatusCode::OK,
                    AxumJson(serde_json::json!({
                        "access": access_token,
                    })),
                ))
            } else {
                Ok((
                    StatusCode::UNAUTHORIZED,
                    AxumJson(serde_json::json!({
                        "detail": "Invalid user in refresh token",
                    })),
                ))
            }
        }
        Err(_) => Ok((
            StatusCode::FORBIDDEN,
            AxumJson(serde_json::json!({
                "detail": "Invalid refresh token",
            })),
        )),
    }
}
