# remotes::install_github("dreamRs/fresh")

library(fresh)

create_theme(
    theme = "sandstone",
    bs_vars_navbar(
        height = "50px",
        default_bg = "#dca0b3",  #dca0b3, #78C2AD #8fe3cb #3fc1c9 #fc5185 #39CCCC #dcc9a0 #a0d1dc
        default_color = "#ffffff",
        default_border = "#ffffff",
        default_link_color = "#FFFFFF",
        default_link_active_color = "#ff5542"
    ),
    bs_vars_color(
        gray_base = "#354e5c",
        brand_primary = "#75b8d1",
        brand_success = "#c9d175",
        brand_info = "#758bd1",
        brand_warning = "#d1ab75",
        brand_danger = "#d175b8"
    ),
    bs_vars_state(
        success_text = "#FFF",
        success_bg = "#c9d175",
        success_border = "#c9d175",
        info_text = "#FFF",
        info_bg = "#3f2d54",
        info_border = "#3f2d54",
        danger_text = "#FFF",
        danger_bg = "#d175b8",
        danger_border = "#d175b8"
    ),
    bs_vars_wells(
        bg = "#dcc9a0", #BBB477 #C9A0DC #b3dca0 #a0b3dc
        border = "#ffffff"
    ),
    bs_vars_pills(border_radius = 5,
                  active_link_hover_bg = "#d175b8",
                  active_link_hover_color = "#000000"
                    ),
    # bs_vars_dropdown(bg="#dca0b3",),
    bs_vars_table(
        bg = "#F3969A",
        bg_accent = "lightblue",
        bg_hover = "firebrick",
        bg_active = "#FFFF00"
    ),
    bs_vars_tabs(
    border_color = "#FF0000", # red
    link_hover_border_color = "#FFFF00", # yellow
    active_link_hover_bg = "#FF00FF", # pink
        active_link_hover_color = "#FFF" # white
    ),
    bs_vars_input(bg = "#000000"),
    output_file = "www/mytheme.css"
)
