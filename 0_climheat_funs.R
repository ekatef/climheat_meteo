n_na <- function(x){
    sum(is.na(x))
}

# GHCN data imply multiplication Celsium degrees by 10: 7C -> tavg = 70
drop_unrealistic_tas <- function(x, low_bound = -1000, high_bound = 700){
    x_modif <- ifelse(x < low_bound, NA, x)
    x_res <- ifelse(x_modif > high_bound, NA, x_modif)
    return(x_res)
}

show_stations <- function(st_meta_df, main_txt = "", sub_txt = "",
    annotate = FALSE,
    annot_text_column = "id",
    coord_margin = 5  
){

x_range = range(st_meta_df$longitude) + c(-coord_margin, coord_margin)
y_range = range(st_meta_df$latitude) + c(-coord_margin, coord_margin)  

maps::map(xlim = x_range, ylim = y_range, 
# ‘mar’ A numerical vector of the form ‘c(bottom, left, top, right)’    
    mar = c(4.1, 4.1, 3, 0.1)
    # mar = c(4.1, 4.1, par("mar")[3], 0.1)
)
points(x = st_meta_df$longitude, y = st_meta_df$latitude, 
    pch = 21, bg = "forestgreen", col = "darkgreen", cex = 1)
if ( annotate ){
    text(unlist(st_meta_df[, annot_text_column]),
        x = unlist(st_meta_df[, "longitude"]), 
        y = unlist(st_meta_df[, "latitude"]) + 0.2)
        # x = st_meta_df$longitude, y = st_meta_df$latitude + 0.5)
}
title(main = main_txt, sub = sub_txt, outer = FALSE)    
box()
return(NULL)    

}
