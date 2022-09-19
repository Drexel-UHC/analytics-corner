{
  # 1. Setup -----
  library(janitor)
  library(readxl)
  library(tidyverse)
  library(geofacet)
  library(cowplot)
}


{
  # 2. Data ----
  raw_data = readxl::read_excel(path = "family medical leave preemption dataset 9-19-22.xlsx",
                                sheet = 'Sheet2') %>% 
    select(name = 1, family_leave = 2, preemption = 3, abortion_ban_risk = 4)  %>% 
    drop_na() %>% 
    mutate_all(~as.character(.x))
  
  ## clean state codes
  xwalk_states = geofacet::state_ranks %>% 
    select(name, state) %>% 
    distinct()
  data = raw_data %>% 
    mutate(name = name %>% 
             recode("Washington DC" = "District of Columbia",
                    "New Hampshire*" = "New Hampshire"),) %>% 
    left_join(xwalk_states)
  data
  
}
{
  #3. Prep Plot Data ----
  
  ## Set coordinates
  height = 10 
  padding_x = 1
  width = 8
  rect_xmin = padding_x
  rect_xmax = width + padding_x
  rect_x_mean = mean(c(rect_xmin,rect_xmax))
  rect_ymin = height/3
  rect_ymax = rect_ymin*2
  
  ## Manual colors for data features
  abortion_colors = c("0" = "#e7e8e9",
                      "1" = "#A6A7AA", 
                      "2" = "#757679",
                      "3" = "#000000")
  
  xwalk_colors = c(abortion_colors,
                   "white" = "white",
                   "black" = "black",
                   "0_family_leave" = "white",
                   "1_family_leave" = "#00aeef",
                   "0_preemption" = "white",
                   "1_preemption" = "#f1592a")     
  
  ## Operationalize data for plot
  data_processed = data %>% 
    ## Append coordinates to data for geom_polygon
    mutate(top_triangle_x = list(c(rect_xmin,rect_xmax,rect_x_mean)),
           top_triangle_y = list(c(rect_ymax,rect_ymax,height)),
           bottom_triangle_x = list(c(rect_xmin,rect_xmax,rect_x_mean)),
           bottom_triangle_y = list(c(rect_ymin,rect_ymin,0))
    ) %>% 
    unnest(cols = c(top_triangle_x, top_triangle_y, bottom_triangle_x, bottom_triangle_y)) %>% 
    ## operationalize colors
    mutate(
      state_text = ifelse(as.numeric(abortion_ban_risk)>1, "white","black"),
      family_leave = paste0(family_leave,"_family_leave"),
      preemption = paste0(preemption,"_preemption"))
}


{ # 4. Getfacet map ----
  geom_hex_for_alina = function(gg){
    gg +
      ## add rectangle
      geom_rect(aes(xmin = rect_xmin, xmax = rect_xmax,   
                    ymin = rect_ymin, ymax = rect_ymax, 
                    fill = abortion_ban_risk)) +
      ## Add state abbrv text
      geom_text(aes(label = state, color = state_text),
                x = height/2, y = height/2,
                size = 3)+
      ## Top triangle (family leave)
      geom_polygon(aes(x=top_triangle_x,y=top_triangle_y, fill = family_leave)) + 
      ## Bottom triangle (preemption)
      geom_polygon(aes(x = bottom_triangle_x, y = bottom_triangle_y, fill = preemption)) +   
      ## Manual colors
      scale_fill_manual(values = xwalk_colors) +
      scale_color_manual(name = "state_text", values = xwalk_colors) +
      theme_void()+ 
      theme(
        ## Completely remove facet labels
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        ## Remove legend
        legend.position = 'none'
      )
  }
}

{
  # 5. Legend - State Attitude  ----
  
  ## Legend 1: Rectangle
  
  side = 12
  padding = 0.5 
  rect_width =  (side - 2*padding)/4
  legend_y = 5
  legend_height = 3.25
  legend_mid = legend_y + legend_height/2
  
  dfa = tibble(xmin = padding + 0:3*rect_width,
               xmax = padding + 1:4*rect_width,
               ymin = legend_y,
               ymax = legend_y + legend_height,
               fill = unname(abortion_colors)) %>% 
    mutate(xmid =(xmin +xmax)/2)
  legend_risk = ggplot() + 
    ## Container (10 by 10)
    geom_rect(aes(xmin = 0, xmax = 10, 
                  ymin = 0, ymax = 10),
              fill = "white") + 
    ## Legend for rectangles
    geom_rect(dfa, mapping = 
                aes(xmin = xmin, xmax = xmax, 
                    ymin = ymin, ymax = ymax),
              fill = dfa$fill) + 
    ## Left label
    geom_text(aes(label = "Accessible\n(Protected)", x = xmid),
              x = dfa[1,]$xmid, y = legend_mid,
              size = 2.9) +
    ## Center label
    geom_text(aes(label = "Accessible\n(Not protected)"),
              x = dfa[2,]$xmid, y = legend_mid ,
              size = 2.9) +
    ## Rigth label
    geom_text(aes(label = "Hostile"),
              x = dfa[3,]$xmid, y = legend_mid ,
              size = 3,
              color = 'white') +
    ## Rigth label
    geom_text(aes(label = "Illegal"),
              x = dfa[4,]$xmid, y = legend_mid,
              size = 3,
              color = "white") +
    theme_void()
  
  title_rect <- ggdraw() + 
    draw_label(
      "Rectangle show state\nattitude towards abortion",
      fontface = 'bold',
      x = 0.5,
      hjust = 0.5,
      size = 10
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(6, 0, 0, 0)
    )
  rect_legend = plot_grid(
    title_rect, legend_risk,
    ncol = 1,
    rel_heights = c(2, 8)
  )
  
  rect_legend
}


{
  ##  6. Legend 2: Paid family leave -----
  ## Create funciton to layout for legend
  geom_hex_legend_for_alina = function(gg){
    gg +
      geom_rect(aes(xmin =  0 - 1*height*1.4, xmax = height + 1*height*1.4,   
                    ymin = 0 - 1*height, ymax = height + 1*height),
                fill = "white") 
  }
  
  ## Legend 2: Blue triangle
  title_hex_blue <- ggdraw() + 
    draw_label(
      "Blue Triangles show \n paid family policy",
      fontface = 'bold',
      x = 0.5,
      hjust = 0.5,
      size = 10
    ) +
    theme( plot.margin = margin(5, 0, 0, 0))
  legend_hex_blue = data_processed %>% 
    filter(state == "WA") %>% 
    mutate(state = "") %>% 
    ggplot()  %>% 
    geom_hex_legend_for_alina() %>% 
    geom_hex_for_alina()
  hex_legend_blue = plot_grid(
    title_hex_blue, legend_hex_blue,
    ncol = 1,
    rel_heights = c(2, 8)
  )
  
  hex_legend_blue
}


{
  ## Legend 3: Preemption ---- 
  
  title_hex_red <- ggdraw() + 
    draw_label(
      "Red triangles show state\npreemption of paid\nfamily policies",
      fontface = 'bold',
      x = 0.5,
      hjust = 0.5,
      size = 10
    ) +
    theme(
      plot.margin = margin(5, 0, 0, 0)
    )
  legend_hex_red = data_processed %>% 
    filter(state == "FL") %>% 
    mutate(state = "") %>% 
    ggplot()  %>% 
    geom_hex_legend_for_alina() %>% 
    geom_hex_for_alina()
  hex_legend_red = plot_grid(
    title_hex_red, legend_hex_red,
    ncol = 1,
    rel_heights = c(3, 8)
  )
  hex_legend_red
}
# Layout for Deliverable
 

title =  ggdraw() + 
  draw_label(
    "Abortion, Paid Family Leave and Preemption Policy by U.S. State",
    fontface = 'bold',
    x = 0.5,
    hjust = 0.5,
    size = 14
  ) +
  theme_void()+
  theme( plot.margin = margin(0, 0, 0, 0),
         plot.background = element_rect(fill = "#f2f2f2", colour = 'white' ),
        
         )

legend = plot_grid(rect_legend,hex_legend_blue,hex_legend_red,
                   ncol = 3,
                   rel_widths = c(7, 4, 4))


figure = plot_grid(title,
                   NULL,
                   legend,
                   NULL,
                   plot,
                   ncol = 1,
                   rel_heights = c(0.5,0.2,2,0.2,7)) 
 

ggsave(filename = "output/issue5.jpeg",
       plot = figure,
       height = 8,
       width = 8
       )
 

```

![](output/issue5.jpeg){fig-align="center"}
