embedPostsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("rendered_posts"))
  )
}

embedPostsServer <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(r$permalinks, {
      req(!is.null(r$permalinks))
      r$embedded_posts <- lapply(r$permalinks, embed_switch)
    })

    output$rendered_posts <- shiny::renderUI({
      req(r$embedded_posts)
      if(all(lapply(r$permalinks, extract_source) %in% c("threads", "twitter", "x", "facebook"))){
        print("one")
        create_div_layout(r$embedded_posts, posts_per_row = 3)
      } else {
        do.call(bslib::layout_column_wrap, c(width = 1/3, c(r$embedded_posts)))
      }
    })
  })
}

