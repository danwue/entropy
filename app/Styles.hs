module Styles where

field_width = 500 
grid_spacing= 15
cells=7
tile_size = (field_width-grid_spacing*(cells-1))/cells
tile_border_radius=3

mobile_threshold= field_width + 20