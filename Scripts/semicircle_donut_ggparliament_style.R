#!/usr/bin/env Rscript
# Semicircle donut with party-logo slice labels
# Saves output to Scripts/semicircle_test.png

fifthGroup <- "Left"  # switch to "Green" to generate the Green version
groupsEP <- c("EVP", "SD", "PfE", "RE", fifthGroup)

source("Scripts/drucker_helper.R")
df <- get_sus_dist(1000, groupsEP, F)

install_if_missing <- function(p) if(!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cran.rstudio.com")
pkgs <- c("tibble", "dplyr", "ggplot2", "ggimage", "scales", "purrr", "ggforce")
invisible(lapply(pkgs, install_if_missing))

library(tibble)
library(dplyr)
library(ggplot2)
library(ggimage)
library(scales)
library(purrr)
library(ggforce)

# Attach logo paths (relative to project root)
df <- df %>% mutate(image = file.path("LaTeX", "Folien", "Bilder", paste0(Fraktion, ".png")))
missing <- df$image[!file.exists(df$image)]
if (length(missing) > 0) message("Warning: missing logos: ", paste(missing, collapse = ", "))

# Build helper blank slice so parties occupy exactly half the circle
total <- sum(df$SuS)
df2 <- bind_rows(df, tibble(Fraktion = "blank", SuS = total, image = NA_character_))

# ensure the blank slice is last (stack order matters)
df2$Fraktion <- factor(df2$Fraktion, levels = c(as.character(df$Fraktion), "blank"))

# compute ymin/ymax/ymid used by polar mapping
df2 <- df2 %>% arrange(Fraktion) %>%
  mutate(ymax = cumsum(SuS), ymin = ymax - SuS, ymid = (ymin + ymax) / 2)

# color and logo mapping: try to read metadata from app.R (party_info)
app_file <- file.path("app.R")
app_meta <- list()
if (file.exists(app_file)) {
  lines <- readLines(app_file, warn = FALSE)
  start <- grep("party_info\\s*<-\\s*list\\(", lines)
  if (length(start) > 0) {
    block <- lines[start:(start + 40)]
    for (ln in block) {
      # capture lines like: evp   = list(name = "EVP",   logo = "EPP.png",   color = "#003399", var = "evp"),
      m <- regexec("^\\s*([a-zA-Z0-9_]+)\\s*=\\s*list\\(.*logo\\s*=\\s*\"([^\"]+)\".*color\\s*=\\s*\"([^\"]+)\"", ln)
      r <- regmatches(ln, m)
      if (length(r) && length(r[[1]]) >= 4) {
        key <- r[[1]][2]
        logo <- r[[1]][3]
        col <- r[[1]][4]
        app_meta[[key]] <- list(logo = logo, color = col)
      }
    }
  }
}

# Fallback palette if no app colors found
party_levels <- as.character(df$Fraktion)
pal <- hue_pal()(length(party_levels))

# map Fraktion names to app keys
map_keys <- c(EVP = "evp", SD = "sd", PfE = "pfe", RE = "renew", Left = "left", Green = "green")

# build fill values and image paths using app_meta when available
fill_vals <- setNames(rep(NA_character_, length(party_levels)), party_levels)
for (p in party_levels) {
  k <- map_keys[[p]]
  if (!is.null(k) && !is.null(app_meta[[k]])) {
    fill_vals[p] <- app_meta[[k]]$color
  } else {
    fill_vals[p] <- pal[which(party_levels == p)]
  }
}
fill_vals["blank"] <- "white"


# Prefer representative JPG logos (rep1_/rep2_/rep3_) then PNG, to match Left/PfE style
images_dir <- file.path("LaTeX", "Folien", "Bilder")
all_imgs <- list.files(images_dir, full.names = TRUE)
find_best_image <- function(fr) {
  # map short/ambiguous keys to clearer search terms to avoid substr-matches
  name_map <- c(RE = "Renew", `PfE` = "PfE", EVP = "EVP", SD = "SD", Left = "Left", Green = "Green")
  fr_use <- if (fr %in% names(name_map)) name_map[[fr]] else fr
  fr_key <- tolower(gsub("[^A-Za-z0-9]", "", fr_use))
  if (length(all_imgs) == 0) return(file.path(images_dir, paste0(fr, ".png")))
  candidates <- purrr::keep(all_imgs, ~{
    nm <- tolower(basename(.x))
    nm_clean <- gsub("[^A-Za-z0-9]", "", nm)
    grepl(fr_key, nm_clean, fixed = TRUE)
  })
  # prefer non-rep images (exclude filenames starting with rep) and prefer png
  candidates <- candidates[!grepl("(^|/)(rep)[0-9_]*", tolower(basename(candidates)))]
  if (length(candidates) == 0) {
    # fallback: accept rep images if they match
    candidates <- purrr::keep(all_imgs, ~{
      nm_clean <- gsub("[^A-Za-z0-9]", "", tolower(basename(.x)))
      grepl(fr_key, nm_clean, fixed = TRUE)
    })
  }
  if (length(candidates) == 0) return(file.path(images_dir, paste0(fr, ".png")))
  # prefer png
  pngs <- candidates[grepl("\\.png$", tolower(candidates))]
  if (length(pngs) > 0) return(pngs[[1]])
  return(candidates[[1]])
}

df <- df %>% rowwise() %>% mutate(
  image = find_best_image(Fraktion)
) %>% ungroup()

# --- Build true semicircle donut by constructing sector polygons ---
# parameters
r_outer <- 1.0
r_inner <- 0.55
npoints <- 120

# reorder so PfE stays on the right side in the final visual order
desired_order <- if (fifthGroup == "Left") {
  c("PfE", "EVP", "RE", "SD", "Left")
} else {
  c("PfE", "EVP", "RE", "Green", "SD")
}
present <- intersect(desired_order, df$Fraktion)
others <- setdiff(df$Fraktion, present)
df <- df %>% slice(match(c(present, others), Fraktion))

# only real parties (exclude any helper blank)
parts <- df
parts$prop <- parts$SuS / sum(parts$SuS)
parts <- parts %>% mutate(
  angle_start = cumsum(lag(prop, default = 0)) * pi,
  angle_end = cumsum(prop) * pi,
  angle_mid = (angle_start + angle_end) / 2
)

# function to create polygon for one sector
sector_poly <- function(a1, a2, r0, r1, n = 100) {
  th_out <- seq(a1, a2, length.out = n)
  x_out <- cos(th_out) * r1
  y_out <- sin(th_out) * r1
  th_in <- seq(a2, a1, length.out = n)
  x_in <- cos(th_in) * r0
  y_in <- sin(th_in) * r0
  data.frame(x = c(x_out, x_in), y = c(y_out, y_in))
}

# build polygons for each party
polys <- purrr::map2_dfr(parts$angle_start, parts$angle_end, ~{
  d <- sector_poly(.x, .y, r_inner, r_outer, n = npoints)
  d$party <- parts$Fraktion[which(parts$angle_start == .x & parts$angle_end == .y)]
  d
})

# compute label positions (place logos outside the outer radius)
label_radius_out <- r_outer + 0.35
labels <- parts %>% mutate(
  lx = cos(angle_mid) * label_radius_out,
  ly = sin(angle_mid) * label_radius_out,
  has_img = file.exists(image)
)

# build plot
library(ggforce)
p_final <- ggplot() +
  geom_polygon(data = polys, aes(x = x, y = y, group = party, fill = party), color = NA, show.legend = FALSE) +
  scale_fill_manual(values = fill_vals) +
  coord_equal(xlim = c(-1.6, 1.6), ylim = c(-0.05, r_outer + 0.6)) +
  theme_void() +
  theme(legend.position = "none")

# subtle center divider for the parliament midpoint
p_final <- p_final +
  geom_segment(
    aes(x = 0, xend = 0, y = 0, yend = r_outer + 0.18),
    inherit.aes = FALSE,
    linetype = "dotted",
    linewidth = 0.45,
    color = "grey35",
    alpha = 0.7
  )

# add logos where available
p_final <- p_final +
  geom_image(data = labels %>% filter(has_img),
             aes(x = lx, y = ly, image = image),
             inherit.aes = FALSE,
             size = 0.16, by = "width", asp = 1.0)

# add text fallback
p_final <- p_final +
  geom_text(data = labels %>% filter(!has_img),
            aes(x = lx, y = ly, label = Fraktion),
            inherit.aes = FALSE, size = 5)

# add connecting segments from slice midpoint to logo (optional)
segments <- parts %>% mutate(
  sx = cos(angle_mid) * r_outer * 0.98,
  sy = sin(angle_mid) * r_outer * 0.98,
  ex = cos(angle_mid) * (r_outer + 0.12),
  ey = sin(angle_mid) * (r_outer + 0.12)
)
## remove connector segments as requested (no lines between logos and pieces)

# trim plot to top half (y >= 0) by setting limits
## already set limits via coord_equal; no ylim trimming

# Save output image to Scripts/
out_file <- file.path("Scripts", paste0("semicircle_", fifthGroup, ".png"))
ggsave(out_file, p_final, width = 8, height = 4, dpi = 150)
message("Saved semicircle test image to: ", normalizePath(out_file))
