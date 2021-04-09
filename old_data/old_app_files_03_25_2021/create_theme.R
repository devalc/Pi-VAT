# remotes::install_github("dreamRs/fresh")

library(fresh)

create_theme(
    theme = "darkly",
    bs_vars_navbar(
        height = "50px",
        default_bg = "#17141D",
        default_color = "#17141D",
        default_link_color = "#FFFFFF",
        default_link_active_color = "#FFAE42"
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
        bg = "#17141D",
        border = "#ffffff"
    ),
    # bs_vars_table(
    #     bg = "#FFF",
    #     bg_accent = "lightblue",
    #     bg_hover = "firebrick",
    #     bg_active = "#FFFF00"
    # ),
    # bs_vars_tabs(
        # border_color = "#FF0000", # red
        # link_hover_border_color = "#FFFF00", # yellow
        # active_link_hover_bg = "#FF00FF", # pink
    #     active_link_hover_color = "#FFF" # white
    # ),
    output_file = "www/mytheme.css"
)