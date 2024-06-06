main %>%
  ggplot(aes(edu, wage)) +
  geom_point(alpha = 0.5) +
  geom_point(stat = "summary", fun = "mean", color = "red")

main %>%
  ggplot(aes(edu, logwage)) +
  geom_point(alpha = 0.5) +
  geom_point(stat = "summary", fun = "mean", color = "red")









cormatrix |>
  flextable() |>
  theme_apa() |>
  align(align = "center", part = "header") |>
  align(align = "right", part = "body") |>
  set_table_properties(align = "center", layout = "autofit") #|>
# save_as_docx(path = "cormatrix2.docx")
# save_as_image(x, path = "cormatrix.png", expand = 10, res = 200)