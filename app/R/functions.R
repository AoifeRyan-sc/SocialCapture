# General ----
extract_source <- function(permalink) stringr::str_extract(permalink, "(\\w+)\\.(com|net|co\\.uk)", group = 1)

embed_switch <- function(permalink) {
  source <- extract_source(permalink)

  embed <- switch(
    source,
    "reddit" = create_reddit_embed(permalink),
    "instagram" = create_instagram_embed(permalink),
    "x" = create_tweet_embed(permalink),
    "twitter" = create_tweet_embed(permalink),
    "youtube" = create_youtube_embed(permalink),
    "facebook" = create_facebook_embed(permalink),
    "linkedin" = create_linkedin_embed(permalink),
    "threads" = create_threads_embed(permalink),
    "tiktok" = create_tiktok_embed(permalink)
  )

  return(embed)
}

# tagsinput <- function(tag) {
#   tag$children[[2]] <- tagAppendAttributes(tag$children[[2]],
#                                            `data-role` = "tagsinput")
#   tag
# }

# sets the max height and makes element scrollable
style_embedding <- function(height = 717) {
  # width = 403) {
  start_div <- paste0("<div style='height: ", height, "px; overflow-y: auto;'>")
  # width: ", width, "px;
  end_div <- "</div>"

  return(list(start_div = start_div, end_div = end_div))
}

style_embedding2 <- function(height = 717) {
  # width = 403) {
  start_div <- paste0("<div style='height: ", height, "px; overflow-y: FALSE;'>")
  # width: ", width, "px;
  end_div <- "</div>"

  return(list(start_div = start_div, end_div = end_div))
}


create_div_layout <- function(embedded_links, posts_per_row = 3){

  n_posts <- length(embedded_links)

  div_list <- list()

  container_style <- "display: flex; width: 100%; justify-content: center; align-items: center;"
  spacer_style <- 'width: 80px;'

  for (i in seq(1, n_posts, by = posts_per_row)) { # repeat for relevant number of rows

    row_display <- list(embedded_links[i])

    if (i + 1 <= n_posts) {
      row_display <- append(row_display, list(div(style = spacer_style), embedded_links[i + 1]))
    }

    if (i + 2 <= n_posts) {
      row_display <- append(row_display, list(div(style = spacer_style), embedded_links[i + 2]))
    }

    div_list <- append(div_list, list(div(style = container_style, row_display)))
  }

  print(div_list)

  return(shiny::tagList(div_list))


}


# Reddit ----
create_reddit_embed <- function(permalink){
  if(!grepl("reddit", permalink)) stop("Not a reddit link")

  subreddit <- extract_subreddit(permalink)

  reddit_html <- extract_reddit_html(permalink, subreddit)

  return(reddit_html)
}

extract_subreddit <- function(permalink) stringr::str_extract(permalink, "/r/(\\w+)", group = 1)

extract_reddit_html <- function(url, subreddit) {

  embedding_style <- style_embedding(height = 500)
  html <- paste0(embedding_style$start_div, '<blockquote class="reddit-embed-bq" style="height:400px" data-embed-height="597"><a href="',
                 url, '">Link to post</a><br></a> in<a href="https://www.reddit.com/r/', subreddit, '">',
                 subreddit, '</a></blockquote><script async="" src="https://embed.reddit.com/widgets.js" charset="UTF-8"></script>',
                 embedding_style$end_div)

  return(shiny::HTML(html))
}

# Instagram ----
create_instagram_embed <- function(permalink) {

  embedding_style <- style_embedding(height = 717)

  html <- paste0(
    # embedding_style$start_div,
    '<blockquote class="instagram-media" data-instgrm-permalink="',
                 permalink, '" data-instgrm-version="12" style=" background:#FFF; border:0; border-radius:3px; box-shadow:0 0 1px 0 rgba(0,0,0,0.5),0 1px 10px 0 rgba(0,0,0,0.1); margin: 1px; max-width:540px; min-width:326px; padding:0; width:99.375%; width:-webkit-calc(100% - 2px); width:calc(100% - 2px);">
      <div style="padding:16px;">
      </div>
    </blockquote>
    <script async defer src="//www.instagram.com/embed.js"></script>'
    # embedding_style$end_div
    )

  return(shiny::HTML(html))
}

# Youtube ----
create_youtube_embed <- function(permalink) {

  if (length(grep("short", permalink)) > 0){
    width <- 403
    height <- 717
    permalink_split <- strsplit(permalink, "shorts", fixed = TRUE)[[1]]
  } else{
    width <- 560
    height <- 315
    permalink_split <- strsplit(permalink, "watch?v=", fixed = TRUE)[[1]]
    permalink_split[2] <- paste0("/", permalink_split[2])
  }

  html <- paste0(
    '<iframe width="', width, '" height="', height, '" src="', permalink_split[1], 'embed', permalink_split[2], '" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>'
  )

  return(shiny::HTML(html))
}

# Facebook ----
create_facebook_embed <- function(permalink) {
  width <- 403
  height <- 717
  embedding_style <- style_embedding(height = 717)

  if(length(grep("video", permalink)) > 0){
    class <- "video"
  } else {
    class <- "post"
  }
  print(class)

  html <- paste0(
    # embedding_style$start_div,
    '<div id="fb-root"></div>
<script async defer src="https://connect.facebook.net/en_US/sdk.js#xfbml=1&version=v3.2"></script>',
    '<div class="fb-', class, '" data-href="', permalink, '"  data-width="',
    width,
    # "auto",
    '" data-show-text="true" style="border:none;overflow:hidden;height:717px; scrolling="no" frameborder="0" allowfullscreen="true" allow="autoplay"></div>'
    # embedding_style$end_div
  )

  return(shiny::HTML(html))
}

# Linkedin ----

create_linkedin_embed <- function(permalink){
  split_perm <- strsplit(permalink, "feed", fixed = T)

  width <- "403"
  height <- "717"

  html <- paste0('<div class="embed-container"><iframe src="', split_perm[[1]][1], 'embed/feed', split_perm[[1]][2],
                 '" height="', height, '" width="', width, '" frameborder="0" allowfullscreen="" title="Embedded post"></iframe></div>')

  return(shiny::HTML(html))
}

# Threads ----
extract_threads_id <- function(permalink)  stringr::str_extract(permalink, "(?<=post/).+")

create_threads_embed_working_progress <- function(permalink){
  threads_id <- extract_threads_id(permalink)

  html <- paste0(
    '<blockquote class="text-post-media" data-text-post-permalink="',
    permalink, '" data-text-post-version="0" id="',
    threads_id, '" style=" background:#FFF; border-width: 1px;
    border-style: solid; border-color: #00000026; border-radius: 16px;
    max-width:540px; margin: 1px; min-width:270px; padding:0;
    width:99.375%; width:-webkit-calc(100% - 2px); width:calc(100% - 2px);">
    <a href="https://www.threads.net/@gulfoilinternational/post/C7OZBQsNbvQ"
    style=" background:#FFFFFF; line-height:0; padding:0 0; text-align:center;
    text-decoration:none; width:100%; font-family: -apple-system, BlinkMacSystemFont,
    sans-serif;" target="_blank"> <div style=" padding: 40px; display: flex;
    flex-direction: column; align-items: center;"><div style=" display:block;
    height:32px; width:32px; padding-bottom:20px;"> <svg aria-label="Threads"
    height="32px" role="img" viewBox="0 0 192 192" width="32px"
    xmlns="http://www.w3.org/2000/svg"> <path d="M141.537 88.9883C140.71 88.5919 139.87 88.2104 139.019 87.8451C137.537 60.5382 122.616 44.905 97.5619 44.745C97.4484 44.7443 97.3355 44.7443 97.222 44.7443C82.2364 44.7443 69.7731 51.1409 62.102 62.7807L75.881 72.2328C81.6116 63.5383 90.6052 61.6848 97.2286 61.6848C97.3051 61.6848 97.3819 61.6848 97.4576 61.6855C105.707 61.7381 111.932 64.1366 115.961 68.814C118.893 72.2193 120.854 76.925 121.825 82.8638C114.511 81.6207 106.601 81.2385 98.145 81.7233C74.3247 83.0954 59.0111 96.9879 60.0396 116.292C60.5615 126.084 65.4397 134.508 73.775 140.011C80.8224 144.663 89.899 146.938 99.3323 146.423C111.79 145.74 121.563 140.987 128.381 132.296C133.559 125.696 136.834 117.143 138.28 106.366C144.217 109.949 148.617 114.664 151.047 120.332C155.179 129.967 155.42 145.8 142.501 158.708C131.182 170.016 117.576 174.908 97.0135 175.059C74.2042 174.89 56.9538 167.575 45.7381 153.317C35.2355 139.966 29.8077 120.682 29.6052 96C29.8077 71.3178 35.2355 52.0336 45.7381 38.6827C56.9538 24.4249 74.2039 17.11 97.0132 16.9405C119.988 17.1113 137.539 24.4614 149.184 38.788C154.894 45.8136 159.199 54.6488 162.037 64.9503L178.184 60.6422C174.744 47.9622 169.331 37.0357 161.965 27.974C147.036 9.60668 125.202 0.195148 97.0695 0H96.9569C68.8816 0.19447 47.2921 9.6418 32.7883 28.0793C19.8819 44.4864 13.2244 67.3157 13.0007 95.9325L13 96L13.0007 96.0675C13.2244 124.684 19.8819 147.514 32.7883 163.921C47.2921 182.358 68.8816 191.806 96.9569 192H97.0695C122.03 191.827 139.624 185.292 154.118 170.811C173.081 151.866 172.51 128.119 166.26 113.541C161.776 103.087 153.227 94.5962 141.537 88.9883ZM98.4405 129.507C88.0005 130.095 77.1544 125.409 76.6196 115.372C76.2232 107.93 81.9158 99.626 99.0812 98.6368C101.047 98.5234 102.976 98.468 104.871 98.468C111.106 98.468 116.939 99.0737 122.242 100.233C120.264 124.935 108.662 128.946 98.4405 129.507Z" /></svg></div> <div style=" font-size: 15px; line-height: 21px; color: #999999; font-weight: 400; padding-bottom: 4px; "> Post by @gulfoilinternational</div> <div style=" font-size: 15px; line-height: 21px; color: #000000; font-weight: 600; "> View on Threads</div></div></a></blockquote>
        <script async src="https://www.threads.net/embed.js"></script>'
  )
}
create_threads_embed <- function(permalink){
  width <- 403
  height <- 717

  # embedding_style <- style_embedding_expand(height = 717)
  embedding_style <- style_embedding(height = 717)

  html <- paste0(
    # embedding_style$start_div,
    # '<div class="threads-post">',
    '<iframe src="', permalink, '/embed',
    '" width="', width,
    '" height="',  height,
    '" frameborder="0" style="border:none;overflow:hidden" scrolling="no" frameborder="0" allowTransparency="true" allowFullScreen="true"></iframe>'
    # embedding_style$end_div,
    # embedding_style$end_div
    )

  return(shiny::HTML(html))
}

# TikTok ----
create_tiktok_embed <- function(permalink) {
  tt_oembed_url <- sprintf(
    "https://www.tiktok.com/oembed?url=%s",
    permalink
  )
  res <- httr::GET(tt_oembed_url)
  httr::stop_for_status(res, "Get embedding information from tiktok")
  tt <- httr::content(res)

  expected_items <- c("title", "author_name", "author_url", "html")
  if (!all(expected_items %in% names(tt))) {
    warning(
      "Receiv ed unusual response from TikTok oEmbed API ",
      "for this video:", url,
      immediate. = TRUE
    )
  }

  # if I want to stop autoplay, use this to reduce video height...
  split_embed <- strsplit(tt$html, "325px;")[[1]]
  embed <- paste0(split_embed[1], "325px; height: 580px;", split_embed[2])
  return(shiny::HTML(embed))

  # return(shiny::HTML(tt$html))
}
# Twitter ----

create_tweet_embed <- function(permalink) {
  tweet_id <- extract_tweet_id(permalink)
  tweet_screen_name <- extract_tweet_screen_name(permalink)
  embedding_style <- style_embedding(height = 800)

  tweet_blockquote <- get_tweet_blockquote(screen_name = tweet_screen_name, status_id = tweet_id)

  if (!is.null(tweet_blockquote)) {
    html <- paste0(
      tweet_blockquote,
      '<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
    )

    return(shiny::HTML(html))
  } else {

    return(NULL)
  }
}

extract_tweet_id <- function(permalink) stringr::str_extract(permalink, "status/(\\d+)", group = 1)

extract_tweet_screen_name <- function(permalink){

  if (length(grep("x.com", permalink))){
    stringr::str_extract(permalink, "x.com/(\\w+)", group = 1)
  } else {
    stringr::str_extract(permalink, "twitter.com/(\\w+)", group = 1)
  }
}

# Yoinked from Gadenbuie's Tweet Conf package
get_tweet_blockquote <- function(screen_name, status_id, ..., null_on_error = TRUE, theme = "light") {
  oembed <- list(...)$oembed
  if (!is.null(oembed) && !is.na(oembed)) return(unlist(oembed))
  # oembed_url <- glue::glue("https://publish.twitter.com/oembed?url=https://twitter.com/{screen_name}/status/{status_id}&omit_script=1&dnt=1&theme={theme}")
  oembed_url <- glue::glue("https://publish.twitter.com/oembed?url=https://twitter.com/{screen_name}/status/{status_id}&omit_script=1&dnt=1&theme={theme}")
  bq <- purrr::possibly(httr::GET, list(status_code = 999))(URLencode(oembed_url))
  if (bq$status_code >= 400) {
    if (null_on_error) return(NULL)
    '<blockquote style="font-size: 90%">Sorry, unable to get tweet ¯\\_(ツ)_/¯</blockquote>'
  } else {
    httr::content(bq, "parsed")$html
  }
}
