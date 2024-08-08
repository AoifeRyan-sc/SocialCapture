server <- function(input, output, session){

  r <- shiny::reactiveValues(
    permalinks = NULL
  )

  permalinkUploadServer("permalink_upload_panel", r)
  embedPostsServer("embed_post_panel", r)

}
