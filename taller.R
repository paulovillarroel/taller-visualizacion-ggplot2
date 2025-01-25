library(tidyverse)

# https://search.r-project.org/CRAN/refmans/dlnm/html/chicagoNMMAPS.html
chicago <- readr::read_csv("https://cedricscherer.com/data/chicago-nmmaps-custom.csv")

chicago <- chicago |> 
  mutate(
    temp_celsius = (temp - 32) * 5/9,
    dewpoint_celsius = (dewpoint - 32) * 5/9
  )

glimpse(data)

# Lo básico ----

(g <- ggplot(chicago, aes(x = date, y = temp_celsius)))

g + geom_point()

g + geom_line()

g + geom_line() + geom_point()

# Paletas de colores http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# Colores RGB https://coolors.co/

g + geom_point(color = "#b388eb", shape = "diamond", size = 2)

g + geom_point(color = "#b388eb", shape = "diamond", size = 2) +
    geom_line(color = "firebrick", linetype = "dotted", lwd = 0.3)

theme_set(theme_bw()) # theme_grey() por defecto

g + geom_point(color = "firebrick")


# Trabajando con los ejes ----

# labs
chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) + 
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°C)")

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "firebrick") +
  xlab("Year") +
  ylab("Temperature (°C)")

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = expression(paste("Temperature (Celsius),"^"(Este es un mensaje!!)")))

# Aumentando la distancia de los ejes
chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 1, size = 15))

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°C)") +
  theme(axis.title.x = element_text(margin = margin(t = 30), size = 15), # t = top
        axis.title.y = element_text(margin = margin(r = 30, t = 10), size = 15)) # r = right

# Cambiar las estéticas de los ejes

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°C)") +
  theme(axis.title = element_text(size = 15, color = "firebrick",
                                  face = "bold")) # bold.italic

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°c)") +
  theme(axis.title.x = element_text(color = "sienna", size = 15),
        axis.title.y = element_text(color = "orangered", size = 15))

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°C)") +
  theme(axis.text = element_text(color = "dodgerblue", size = 12),
        axis.text.x = element_text(face = "italic"))

chicago |>
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°C)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12))

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "firebrick") +
  labs(x = NULL, y = "")

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°C)") +
  ylim(c(-10, 20)) # limitar el rango del eje y

chicago |> 
  ggplot(aes(x = date, y = temp_celsius, group = year(date))) +
  geom_boxplot() +
  labs(x = "Year", y = "Temperature (°C)") +
  scale_y_continuous(limits = c(-10, 20))

chicago |> 
  ggplot(aes(x = date, y = temp_celsius, group = year(date))) +
  geom_boxplot() +
  labs(x = "Year", y = "Temperature (°C)") +
  coord_cartesian(ylim = c(-10, 20))

# Forzar al origen
chicago |> 
  filter(temp_celsius > 10, o3 > 20) |> 
  ggplot(aes(x = temp_celsius, y = o3)) +
  geom_point(color = "darkcyan") +
  labs(x = "Temperature higher than 10°C",
       y = "Ozone higher than 20 ppb") +
  expand_limits(x = 0, y = 0)


# Títulos ----

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°C)") +
  ggtitle("Temperatures in Chicago")

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°C)",
       title = "Temperatures in Chicago",
       subtitle = "Seasonal pattern of daily temperatures from 1997 to 2001",
       caption = "Data: NMMAPS",
       tag = "Fig. 1")

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°C)",
       title = "Temperatures in Chicago") +
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(30, 0, 30, 0), # t b l r
                                  size = 20))

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = NULL,
        title = "Temperatures in Chicago",
        caption = "Data: NMMAPS") +
  theme(plot.title = element_text(hjust = 1, size = 16, face = "bold.italic"),
        plot.caption = element_text(hjust = 0)) # hjust = 0, 0.5, 1

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "firebrick") +
  labs(x = "Year", y = "Temperature (°C)") +
  ggtitle("Temperatures in Chicago\nfrom 1997 to 2001") + # \n
  theme(plot.title = element_text(lineheight = 0.8, size = 16))


# Leyendas ---

chicago |> 
  ggplot(aes(x = date, y = temp_celsius, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°C)")

chicago |> 
  ggplot(aes(x = date, y = temp_celsius, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°C)") +
  theme(legend.position = "none")

chicago |> 
  ggplot(aes(x = date, y = temp_celsius, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°C)") +
  guides(color = "none") # Lo mismo que el anterior

chicago |>
  ggplot(aes(x = date, y = temp_celsius, color = season, shape = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°C)") +
  guides(color = "none")

chicago |> 
  ggplot(aes(x = date, y = temp_celsius, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°C)") +
  theme(legend.position = "bottom") # top, bottom, left, right, none

chicago |> 
  ggplot(aes(x = date, y = temp_celsius, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°C)",
       color = NULL) +
  theme(legend.position = c(0.85, 0.1), # Abajo a la derecha
        legend.background = element_rect(fill = "transparent"))

chicago |> 
  ggplot(aes(x = date, y = temp_celsius, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°C)",
        color = NULL) +
  theme(legend.position = c(0.85, 0.1),
        legend.background = element_rect(fill = "white", color = "black"))

chicago |> 
  ggplot(aes(x = date, y = temp_celsius, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°C)") +
  theme(legend.position = c(0.5, 0.97),
        legend.background = element_rect(fill = "transparent")) +
  guides(color = guide_legend(direction = "horizontal"))

chicago |> 
  ggplot(aes(x = date, y = temp_celsius, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°C)",
       color = "Seasons\nindicated\nby colors:") +
  theme(legend.title = element_text(color = "blue",
                                    size = 14,
                                    face = "bold"))

# Cambiar el orden de la leyenda

chicago$season <- factor(chicago$season, levels = c("Winter", "Spring", "Summer", "Autumn"))

chicago |> 
  ggplot(aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°C)")

chicago |> 
  ggplot(aes(x = date, y = temp, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°C)") +
  scale_color_discrete(
    name = "Seasons:",
    labels = c("Mar—May", "Jun—Aug", "Sep—Nov", "Dec—Feb")
  ) +
  theme(legend.title = element_text(
    color = "blue", size = 14, face = 2 # 1 = plain, 2 = bold, 3 = italic, 4 = bold italic
  ))

  chicago |> 
    ggplot(aes(x = date, y = temp, color = season)) +
    geom_point() +
    labs(x = "Year", y = "Temperature (°C)") +
    scale_color_discrete(
      name = "Seasons:",
      labels = c("Mar—May", "Jun—Aug", "Sep—Nov", "Dec—Feb")
    ) +
    theme(legend.title = element_text(
      color = "blue", size = 14, face = 2
    )) +
    guides(color = guide_legend(override.aes = list(size = 5))) # Ajustar el tamaño de los puntos en la leyenda

chicago |> 
  ggplot(aes(x = date, y = temp_celsius, color = temp_celsius)) +
  geom_point() +
  labs(x = "Year", 
       y = "Temperature (°C)", 
       color = "Temperature (°C)")

chicago |> 
  ggplot(aes(x = date, y = temp_celsius, color = temp_celsius)) +
  geom_point() +
  labs(x = "Year", 
      y = "Temperature (°C)", 
      color = "Temperature (°C)") +
  guides(color = guide_legend())
  
chicago |> 
  ggplot(aes(x = date, y = temp_celsius, color = temp_celsius)) +
  geom_point() +
  labs(x = "Year", 
      y = "Temperature (°C)", 
      color = "Temperature (°C)") +
  guides(color = guide_colorsteps())


# Multi panel plots -----

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "orangered", alpha = 0.3) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "Year", y = "Temperature (°C)") +
  facet_grid(year ~ season)

g <- chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "chartreuse4", alpha = 0.3) +
  labs(x = "Year", y = "Temperature (°C)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

g + facet_wrap(~ year)

g + facet_wrap(~ year, nrow = 1)

g + facet_wrap(~ year, nrow = 2, scales = "free")

# facet_wrap() con 2 variables

g + facet_wrap(year ~ season, nrow = 4, scales = "free_x")

# Destacar un gráfico
chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(aes(color = season == "Summer"), alpha = 0.3) +
  labs(x = "Year", y = "Temperature (°C)") +
  facet_wrap(~ season, nrow = 1) +
  scale_color_manual(values = c("gray40", "firebrick"), guide = "none")


# Colores ----

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "steelblue", size = 2) +
  labs(x = "Year", y = "Temperature (°C)")

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(shape = 21, size = 3, stroke = 1,
             color = "#3cc08f", fill = "#c08f3c") +
  labs(x = "Year", y = "Temperature (°C)")

ga <- chicago |> 
  ggplot(aes(x = date, y = temp_celsius, color = season)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°C)", color = NULL)

ga + scale_color_manual(values = c("dodgerblue4",
                                   "darkolivegreen4",
                                   "darkorchid3",
                                   "goldenrod1"))

# installar paquete RColorBrewer
ga + scale_color_brewer(palette = "Set1")

RColorBrewer::display.brewer.all()


# Variables cuantitativas

gb <- chicago |> 
  ggplot(aes(x = date, y = temp, color = temp_celsius)) +
  geom_point() +
  labs(x = "Year", y = "Temperature (°C)", color = "Temperature (°C):")

gb + scale_color_gradient()

gb + scale_color_gradient(low = "blue", high = "red")

gb + scale_color_gradient2(low = "blue", mid = "grey90", high = "red")

# Paleta Viridis
# https://sjmgarnier.github.io/viridis/articles/intro-to-viridis.html

gb + scale_color_viridis_c() # default
gb + scale_color_viridis_c(option = "inferno")
gb + scale_color_viridis_c(option = "plasma")
gb + scale_color_viridis_c(option = "mako")


# Trabajando con líneas

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point() +
  geom_hline(yintercept = c(0, 25)) + # Agrega una línea horizontal
  labs(x = "Year", y = "Temperature (°C)")

chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point() +
  geom_vline(xintercept = as.Date("2000-01-13")) + # Agrega una línea vertical
  labs(x = "Year", y = "Temperature (°C)")

# Ajustar características de las líneas
chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point() +
  geom_vline(xintercept = as.Date("2000-01-13"), lwd = 1.5, lty = 2, color = "red") + 
  labs(x = "Year", y = "Temperature (°C)")

# Agregar texto
chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point() +
  geom_vline(xintercept = as.Date("2000-01-13"), lwd = 1.5, lty = 2, color = "red") + 
  geom_text(x = as.Date("2000-01-13"), y = 30, label = "Texto ejemplo", hjust = -0.2, color = "red", size = 6) +
  labs(x = "Year", y = "Temperature (°C)")


# Otros gráficos ----

g <- chicago |> 
  ggplot(aes(x = season, y = o3, color = season)) +
  labs(x = "Season", y = "Ozone") +
  scale_color_brewer(palette = "Dark2", guide = "none")

g + geom_boxplot()

g + geom_point()

g + geom_jitter(width = 0.3, alpha = 0.5)

g + geom_violin(fill = "gray80", linewidth = 1, alpha = 0.5)

g + geom_violin(fill = "gray80", linewidth = 1, alpha = 0.5) +
    geom_jitter(alpha = 0.25, width = 0.3) +
    coord_flip()


# Ribbons

# Media movil 30 días
chicago$o3run <- as.numeric(stats::filter(chicago$o3, rep(1/30, 30), sides = 2))

# library(zoo)
chicago$o3run <- zoo::rollmean(chicago$o3, 30, align = "center", na.pad = TRUE)


chicago |> 
  ggplot(aes(x = date, y = o3run)) +
  geom_line(color = "chocolate", lwd = 0.8) +
  labs(x = "Year", y = "Ozone")

chicago |> 
  ggplot(aes(x = date, y = o3run)) +
  geom_ribbon(aes(ymin = 0, ymax = o3run),
               fill = "orange", alpha = 0.4) +
  geom_line(color = "chocolate", lwd = 0.8) +
  labs(x = "Year", y = "Ozone")

# Agregar desviación estándar
chicago$mino3 <- chicago$o3run - sd(chicago$o3run, na.rm = TRUE)
chicago$maxo3 <- chicago$o3run + sd(chicago$o3run, na.rm = TRUE)

chicago |> 
  ggplot(aes(x = date, y = o3run)) +
  geom_ribbon(aes(ymin = mino3, ymax = maxo3), alpha = 0.5,
               fill = "darkseagreen3", color = "transparent") +
  geom_line(color = "aquamarine4", lwd = 0.7) +
  labs(x = "Year", y = "Ozone")

# smooth
chicago |> 
  ggplot(aes(x = date, y = temp_celsius)) +
  geom_point(color = "gray40", alpha = 0.5) +
  stat_smooth() +
  labs(x = "Year", y = "Temperature (°C)")

chicago |> 
  ggplot(aes(x = temp_celsius, y = dewpoint_celsius)) +
  geom_point(color = "gray40", alpha = 0.5) +
  stat_smooth(method = "lm", se = FALSE,
               color = "firebrick", linewidth = 1.3) +
  labs(x = "Temperature (°C)", y = "Dewpoint")


# Gráfico de barras ----

# https://ourworldindata.org/grapher/share-of-invasive-cervical-cancers-caused-by-each-hpv-type
hpv <- read_csv("https://ourworldindata.org/grapher/share-of-invasive-cervical-cancers-caused-by-each-hpv-type.csv?v=1&csvType=full&useColumnShortNames=true")

glimpse(hpv)

hpv <- hpv |>
  pivot_longer(
    cols = -c(Year, Entity, Code),
    names_to = "hpv_type",
    values_to = "share"
  ) |>
  mutate(
    hpv_type = str_replace_all(hpv_type, "_", " ") 
  ) |>
  arrange(desc(share)) |>
  mutate(
    hpv_type = factor(hpv_type, levels = rev(unique(hpv_type)))  # Ajustar los niveles del factor
  )

# Crear el gráfico
hpv |>
  ggplot(aes(x = share, y = hpv_type, label = scales::percent(share / 100, accuracy = 0.1))) +
  geom_col(fill = "steelblue", width = 0.8) +  # Barras horizontales
  geom_text(hjust = -0.2, size = 3.5) +        # Etiquetas de porcentaje
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +  # Expandir el eje Y ligeramente
  labs(
    title = "Distribución de tipos de HPV",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),      # Ajuste de las etiquetas del eje Y
    plot.title = element_text(hjust = 0.5)     # Centrar el título
  )

  
# Ejemplo de datos para gráfico de barras agrupadas (aunque no tiene mucho sentido agregar el género, pero es de ejemplo)
hpv_grouped <- data.frame(
  hpv_type = rep(c("HPV type 16", "HPV type 18", "HPV type 45"), each = 2),
  Gender = rep(c("Male", "Female"), times = 3),
  share = c(40, 50, 10, 15, 5, 7)
)
  
# Barras agrupadas
hpv_grouped |> 
  ggplot(aes(x = hpv_type, y = share, fill = Gender)) +
  geom_col(position = "dodge", width = 0.7) +  # Dodge agrupa las barras
  labs(
    title = "Distribución de tipos de HPV por género",
    x = "Tipo de HPV",
    y = "Porcentaje",
    fill = "Género"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Expandir el eje Y ligeramente
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),    # Centrar título
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotar etiquetas del eje X
  )


# Columnas apiladas
hpv_grouped |> 
  ggplot(aes(x = hpv_type, y = share, fill = Gender)) +
  geom_col(position = "stack", width = 0.7) +  # Apilar las columnas
  labs(
    title = "Distribución de tipos de HPV por género (Apilado)",
    x = "Tipo de HPV",
    y = "Porcentaje",
    fill = "Género"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Expandir el eje Y ligeramente
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),    # Centrar título
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotar etiquetas del eje X
  )


# Columnas al 100%

hpv_grouped |>
  ggplot(aes(x = hpv_type, y = share, fill = Gender)) +
  geom_col(position = "fill") +
  labs(
    title = "Distribución de tipos de HPV por género (100%)",
    x = "Tipo de HPV",
    y = "Porcentaje",
    fill = "Género"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

hpv_total <- hpv_grouped |> 
  group_by(hpv_type) |> 
  summarise(total = sum(share))

hpv_grouped <- hpv_grouped |> 
  left_join(hpv_total, by = "hpv_type") |> 
  mutate(percent = (share / total) * 100)

hpv_grouped |>
  ggplot(aes(x = hpv_type, y = share, fill = Gender)) +
  geom_col(position = "fill") +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_fill(vjust = 0.5), 
            color = "white",
            fontface = "bold",
            size = 5) +
  labs(
    title = "Distribución de tipos de HPV por género (100%)",
    x = "Tipo de HPV",
    y = "Porcentaje",
    fill = "Género"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), labels = scales::percent) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 0.5, size = 15),
    axis.text.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 20, b = 10)) # Agrega separación arriba y abajo
  )

# Cambiar los colores de cada grupo
hpv_grouped |>
  ggplot(aes(x = hpv_type, y = share, fill = Gender)) +
  geom_col(position = "fill") +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_fill(vjust = 0.5),
            color = "white",
            fontface = "bold",
            size = 5
  ) +
  labs(
    title = "Distribución de tipos de HPV por género (100%)",
    x = "Tipo de HPV",
    y = "",
    fill = "Género"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), labels = scales::percent) +
  scale_fill_manual(values = c("Female" = "#FF69B4", "Male" = "#03A9F4")) + # Cambia los colores de cada grupo
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t = 10, b = 30), size = 20, face = "bold"),
    axis.text.x = element_text(hjust = 0.5, size = 15),
    axis.text.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 20, b = 10), size = 10),
    legend.position = "top"
  )


# Gráfico de columnas ----

# Crear un conjunto de datos ejemplo
data <- data.frame(
  category = c("A", "B", "C", "D", "E"),
  value = c(10, 15, 7, 12, 20)
)

ggplot(data, aes(x = category, y = value)) +
  geom_col() +
  labs(title = "Bar Chart",
       subtitle = "Relationship between Category and Value",
       x = "Category",
       y = "Value") +
  theme_classic()

# Reordenar las columnas
data$category <- fct_reorder(data$category, data$value, .desc = TRUE)

levels(data$category)

# Crear el plot con geom_col()
ggplot(data, aes(x = category, y = value)) +
  geom_col() +
  labs(title = "Gráfico de Barras",
       subtitle = "Relación entre Categoría y Valor",
       x = "Categoría",
       y = "Valor") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1))


# Otros ejemplos...

# Gráfico de líneas ----
data <- read_csv("https://raw.githubusercontent.com/github/innovationgraph/main/data/languages.csv")

# Preparar los datos
p <- data |>
  mutate(periodo = paste0(year, " Q", quarter),
         year_quarter = zoo::as.yearqtr(periodo)) |>
  filter(iso2_code == "CL") |>
  group_by(year_quarter) |>
  arrange(desc(num_pushers)) |>
  mutate(ranking = row_number()) |>
  filter(ranking <= 10)

# Crear el gráfico
p |>
  ggplot(aes(year_quarter, ranking, color = language)) +
  geom_line(linewidth = 3, alpha = 0.5) +
  geom_point(size = 3) +
  labs(title = "Ranking of programming languages in Chile (Top 10)",
       x = "Year",
       y = "Ranking",
       color = "Language",
       caption = "Source: Github") +
  scale_y_reverse(breaks = 10:1, labels = 10:1) +
  theme_minimal() +
  theme(plot.title = element_text(margin = margin(b = 20), size = 20),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20))


p |>
  ggplot(aes(year_quarter, ranking, color = language)) +
  geom_line(linewidth = 3, alpha = 0.5) +
  geom_point(size = 3) +
  geom_label(data = p |>  # Agregar la etiqueta a cada línea al final
                group_by(language) |> 
                filter(year_quarter == max(year_quarter)),
                aes(label = language), size = 3, fontface = "bold", nudge_x = 0.2, hjust = 0) +
  labs(title = "Ranking of programming languages in Chile (Top 10)",
        x = "Year",
        y = "Ranking",
        caption = "Source: Github") +
  scale_y_reverse(breaks = 10:1, labels = 10:1) +
  coord_cartesian(xlim = c(min(p$year_quarter), max(p$year_quarter) + 0.6)) + # Agregar margen
  theme_minimal() +
  theme(plot.title = element_text(margin = margin(b = 20), size = 20),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        legend.position = "none")

