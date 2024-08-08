permalinkUploadUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
      shiny::textAreaInput(ns("permalink_input"), "Permalink", placeholder = "https://www.tiktok.com/@share_creative/video/7393350296354802977?_r=1&_t=8o9cnJAwka1",
                           # width = "100px",
                           height = "500px"
                           ) ,
    shiny::actionButton(ns("submit_data"), "Submit Links")
  )
}

permalinkUploadServer <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(input$submit_data, {
      req(input$permalink_input)

      perms <- strsplit(input$permalink_input, ",\\s*|\\s+")[[1]] # split on comma or whitespace
      perms <- trimws(perms) # Remove any leading or trailing whitespace
      r$permalinks <- perms
      })
  })
}
