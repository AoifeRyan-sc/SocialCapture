ui <- fluidPage(
  theme = bslib::bs_theme(
    bootswatch = "sandstone",
    heading_font = bslib::font_face(family = "Cinzel-SemiBold",
                                    src = "fonts/Cinzel-SemiBold.ttf"),
    base_font = bslib::font_face(family = "Cinzel-Regular",
                                 src = "fonts/Cinzel-Regular.ttf")
  ),
  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      width = 500,
      permalinkUploadUI("permalink_upload_panel")
      ),
    # "Main contents",
    embedPostsUI("embed_post_panel")
    )
)

