use actix_files;
use actix_web::{
    get,
    http::header::{ContentDisposition, DispositionType},
    web,
};
use path_clean::PathClean;
use tokio::sync::RwLock;

use crate::{
    player::controller::ChannelController,
    utils::{errors::ServiceError, public_path},
};

/// **Get Public**
///
/// Can be used for HLS Playlist and other static files in public folder
///
/// ```BASH
/// curl -X GET http://127.0.0.1:8787/1/live/stream.m3u8
/// ```
#[get("/{id}/{public:live|preview|public}/{file_stem:.*}")]
async fn get_public(
    path: web::Path<(i32, String, String)>,
    controllers: web::Data<RwLock<ChannelController>>,
) -> Result<actix_files::NamedFile, ServiceError> {
    let (id, public, file_stem) = path.into_inner();

    let absolute_path = if file_stem.ends_with(".ts")
        || file_stem.ends_with(".m3u8")
        || file_stem.ends_with(".vtt")
    {
        let manager = {
            let guard = controllers.read().await;
            guard.get(id)
        }
        .ok_or_else(|| ServiceError::BadRequest(format!("Channel {id} not found!")))?;

        let config = manager.config.read().await;
        config.channel.public.join(public)
    } else {
        public_path()
    }
    .clean();

    let path = absolute_path.join(file_stem.as_str());
    let file = actix_files::NamedFile::open(path)?;

    Ok(file
        .use_last_modified(true)
        .set_content_disposition(ContentDisposition {
            disposition: DispositionType::Attachment,
            parameters: vec![],
        }))
}
