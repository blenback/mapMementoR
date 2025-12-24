# Using built-in Styles

## Built-in Styles

The package includes several built-in map styles. Each style defines
colors for the route, background, streets, highways, and water bodies.

``` r
# List built-in styles
styles <- list(
  Dark = list(
    route_color = "#d1af82",
    bg_color = "#0a0e27",
    street_color = "#1a1f3a",
    highway_color = "#2d3250",
    water_color = "#1a2332"
  ),
  Emerald = list(
    route_color = "#2d4a3e",
    bg_color = "#f5f3ed",
    street_color = "#e8e4d8",
    highway_color = "#d4cfc0",
    water_color = "#d4e3e8"
  ),
  Nautical = list(
    route_color = "#2c3e50",
    bg_color = "#f7f4ed",
    street_color = "#e9e4d8",
    highway_color = "#d5cfc0",
    water_color = "#c8d8e3"
  ),
  Zen = list(
    route_color = "#2d2d2d",
    bg_color = "#fafafa",
    street_color = "#eeeeee",
    highway_color = "#d4d4d4",
    water_color = "#e8e8e8"
  ),
  Ghost = list(
    route_color = "#ffffff",
    bg_color = "#121212",
    street_color = "#1e1e1e",
    highway_color = "#2d2d2d",
    water_color = "#0a0a0a"
  ),
  Obsidian = list(
    route_color = "#ffd60a",
    bg_color = "#000814",
    street_color = "#001d3d",
    highway_color = "#003566",
    water_color = "#000814"
  )
)
knitr::kable(
  do.call(
    rbind,
    lapply(names(styles), function(n) {
      cbind(Style = n, as.data.frame(styles[[n]], stringsAsFactors = FALSE))
    })
  ),
  caption = "Built-in map styles"
)
```

| Style    | route_color | bg_color | street_color | highway_color | water_color |
|:---------|:------------|:---------|:-------------|:--------------|:------------|
| Dark     | \#d1af82    | \#0a0e27 | \#1a1f3a     | \#2d3250      | \#1a2332    |
| Emerald  | \#2d4a3e    | \#f5f3ed | \#e8e4d8     | \#d4cfc0      | \#d4e3e8    |
| Nautical | \#2c3e50    | \#f7f4ed | \#e9e4d8     | \#d5cfc0      | \#c8d8e3    |
| Zen      | \#2d2d2d    | \#fafafa | \#eeeeee     | \#d4d4d4      | \#e8e8e8    |
| Ghost    | \#ffffff    | \#121212 | \#1e1e1e     | \#2d2d2d      | \#0a0a0a    |
| Obsidian | \#ffd60a    | \#000814 | \#001d3d     | \#003566      | \#000814    |

Built-in map styles

## gallery

[![Example race map with Dark
theme](../gallery/Chicago-Alex_Black.png)](../gallery/Chicago-Alex_Black.png "Example race map with Dark theme")

Example race map with Dark theme

[![Example race map with Emerald
theme](../gallery/Edinburgh-Alex_Black.png)](../gallery/Edinburgh-Alex_Black.png "Example race map with Emerald theme")

Example race map with Emerald theme

[![Example race map with Ghost
theme](../gallery/Copenhagen-Alex_Black.png)](../gallery/Copenhagen-Alex_Black.png "Example race map with Ghost theme")

Example race map with Ghost theme

[![Example race map with Zen
theme](../gallery/Great%20North%20Run-Alex_Black.png)](../gallery/Great%20North%20Run-Alex_Black.png "Example race map with Zen theme")

Example race map with Zen theme

[![Example race map with Obsidian
theme](../gallery/Liverpool-Alex_Black.png)](../gallery/Liverpool-Alex_Black.png "Example race map with Obsidian theme")

Example race map with Obsidian theme

[![Example race map with Nautical
theme](../gallery/London-Alex_Black.png)](../gallery/London-Alex_Black.png "Example race map with Nautical theme")

Example race map with Nautical theme
