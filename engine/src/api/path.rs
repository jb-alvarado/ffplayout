use axum::{
    Router,
    extract::Request,
    response::{IntoResponse, Response},
    routing::{delete, get, post, put},
};
use sqlx::SqlitePool;

use super::auth;

pub fn routes(pool: SqlitePool) -> Router {
    Router::new()
        .route("/login", post(auth::login))
        .route("/refresh", post(auth::refresh))
        .with_state(pool)
}
