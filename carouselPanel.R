carouselPanel <- function(..., auto.advance=FALSE){
    n = paste(strsplit(paste(strsplit(as.character(rnorm(1)), "[.]")[[1]], collapse=""), "-")[[1]], collapse="")
    contents = list(...)
    tagList(
        # Import Font Awesome CSS
        singleton(tags$head(tags$link(rel="stylesheet", type="text/css",
                                      href = "shared/font-awesome/css/font-awesome.min.css"))),
        
        # Overwrite the stock white indicators
        singleton(tags$head(tags$style(".carousel-indicators li {
                                       background-color: #DDD;
                                       background-color: rgba(70,70,70,.25);
}
                                       .carousel-indicators .active {
                                       background-color: #999;
                                       }
                                       .carousel-indicators {
                                       bottom: 0px !important;
                                       top: auto;
                                       list-style: none outside none;
                                       margin: 150;
                                       position: absolute;
                                       right: 70px;
                                       z-index: 5;
                                       }
                                       .carousel-control {
                                       opacity: 0.2;
                                       border: none;
                                       }" ))),

        # Set up Javascript to call carousel when document is ready. First if statement uses a 3500 interval to auto advance slides when not hovering
        if( !auto.advance ){
            singleton(tags$head(tags$script("$(document).ready(function(){
                                            $('.carousel').carousel({
                                            interval: 5000
                                            });
        });")))
      }else{
          singleton(tags$head(tags$script("$(document).ready(function(){
                                          $('.carousel').carousel({
                                          interval: 5000
                                          });
                                          });")))
      },
      
      #Set up carousel
      # div(id=paste0("carousel-", n), class="carousel slide", `data-interval`=tolower(as.character(auto.advance)),
      div(id=paste0("carousel-", n), class="carousel slide carousel-fade", `data-interval`=tolower(as.character(auto.advance)),
          # Carousel Inner Div - contains the content to display
          div(class="carousel-inner",
              div(class="item active", contents[[1]], style="padding: 0 0;"), # padding = top, sides in pixels
              mapply(function(elm){
                  list(div(class="item", elm, style="padding: 0 0;"))
              }, contents[2:length(contents)], SIMPLIFY=F, USE.NAMES=F)),
          
          # Carousel controls
          a(class="left carousel-control",
            `data-slide`="prev",
            href=paste0("#carousel-", n),
            style="background: transparent; padding: 100px 0px; color: #000", # color of the background for the left control arrow and padding position
            HTML(paste0("<i class='fa fa-chevron-left'></i>")) ),
          
          a(class="right carousel-control",
            `data-slide`="next",
            href=paste0("#carousel-", n),
            style="background: transparent; padding: 100px 0px; color: #000;",
            HTML(paste0("<i class='fa fa-chevron-right'></i>")) ),
          
          # Generate the carousel indicators
          HTML("<ol class='carousel-indicators'>"),
          tag('li', list(class='active', `data-slide-to`=paste(0),
                         `data-target`=paste0("#carousel-", n))),
          mapply(function(i){
              list(tag('li', list(class='', `data-slide-to`=paste(i),
                                  `data-target`=paste0("#carousel-", n))) )
          }, 1:(length(contents)-1), SIMPLIFY=F, USE.NAMES=F),
          HTML("</ol>")
      )
        )
    }