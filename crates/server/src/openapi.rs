use utoipa::OpenApi;

#[derive(OpenApi)]
#[openapi(
    info(
        title = "Moe API",
        version = "1.0.0",
        description = "A Bangumi search API built with Axum"
    ),
    tags(
        (name = "search", description = "Bangumi search endpoints")
    ),
    components(schemas(bgmtv::SearchSubjectsResponse, bgmtv::Subject))
)]
pub struct ApiDoc;
